use object::{Relocation, RelocationEncoding, RelocationKind, RelocationTarget};
use std::fmt;
use std::sync::Arc;

use super::*;

const R_X86_64_GOTPCREL: u32 = 41;
const R_X86_64_REX_GOTP: u32 = 42;

#[derive(Debug, Clone)]
pub struct LinkRelocation {
    kind: RelocationKind,
    encoding: RelocationEncoding,
    size: u8,
    target: RelocationTarget,
    addend: i64,
    implicit_addend: bool,
}

impl From<Relocation> for LinkRelocation {
    fn from(item: Relocation) -> Self {
        Self {
            kind: item.kind(),
            encoding: item.encoding(),
            size: item.size(),
            target: item.target(),
            addend: item.addend(),
            implicit_addend: item.has_implicit_addend(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct CodeRelocation {
    pub(crate) name: String,
    pub(crate) offset: u64,
    pub(crate) r: LinkRelocation,
}

impl fmt::Display for CodeRelocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Relocation[{}@{:#04x}, kind: {:?}, encoding: {:?}, size: {}, target: {:?}, addend: {}]",
               self.name,
               self.offset,
               self.r.kind,
               self.r.encoding,
               self.r.size,
               self.r.target,
               self.r.addend)
    }
}

impl CodeRelocation {
    pub fn patch(
        &self,
        // pointer to the base of the relocation slice
        patch_base: *mut u8,
        // pointer to address
        addr: *const u8,
    ) {
        println!("{}", self);
        match self.r.kind {
            RelocationKind::Elf(R_X86_64_GOTPCREL) => {
                unsafe {
                    let patch = patch_base.offset(self.offset as isize);

                    // this works
                    let value = addr as isize + self.r.addend as isize - patch as isize;

                    let before = std::ptr::read(patch);
                    (patch as *mut u32).replace(value as u32);
                    println!("patch_base: {:#08x}", patch_base as usize);
                    println!("patch: {:#08x}", patch as usize);
                    println!("value: {:#04x}", value as u32);

                    println!(
                        "rel got {}: patch {:#08x}:{:#08x}=>{:#08x} addend:{:#08x} addr:{:#08x}",
                        &self.name,
                        patch as usize,
                        before,
                        value as u32,
                        self.r.addend,
                        addr as usize,
                    );
                }
            }

            RelocationKind::Elf(R_X86_64_REX_GOTP) => {
                // got entry + addend - reloc_offset(patch)
                // we are computing the offset from the current instruction pointer
                unsafe {
                    let patch = patch_base.offset(self.offset as isize);

                    // this works
                    let value = addr as isize + self.r.addend as isize - patch as isize;

                    // this does not work
                    //let value = patch as isize + rel.r.addend as isize - addr as isize;

                    let before = std::ptr::read(patch);
                    (patch as *mut u32).replace(value as u32);
                    println!("patch_base: {:#08x}", patch_base as usize);
                    println!("patch: {:#08x}", patch as usize);
                    println!("value: {:#04x}", value as u32);

                    println!(
                        "rel got {}: patch {:#08x}:{:#08x}=>{:#08x} addend:{:#08x} addr:{:#08x}",
                        &self.name,
                        patch as usize,
                        before,
                        value as u32,
                        self.r.addend,
                        addr as usize,
                    );
                }
            }

            RelocationKind::Absolute => {
                // S + A
                // S = Address of the symbol
                // A = value of the Addend
                //
                // We get this if we don't compile with -fPIC
                // This doesn't work, and produces an illegal address for some reason
                let name = &self.name;
                unsafe {
                    let adjusted = addr.offset(self.r.addend as isize) as u64;

                    let (before, patch) = match self.r.size {
                        32 => {
                            // patch as 32 bit
                            let patch = patch_base.offset(self.offset as isize) as *mut i32;
                            let before = std::ptr::read(patch);
                            *patch = adjusted as i32;
                            unimplemented!("32 bit absolute relocation does not work");
                            (before as u64, patch as u64)
                        }
                        64 => {
                            // patch as 64 bit
                            let patch = patch_base.offset(self.offset as isize) as *mut u64;
                            let before = std::ptr::read(patch);
                            *patch = adjusted as u64;
                            (before as u64, patch as u64)
                        }
                        _ => unimplemented!(),
                    };

                    println!(
                        "rel absolute {}: patch {:#08x}:{:#08x}=>{:#08x} addend:{:#08x} addr:{:#08x}",
                        name, patch, before, adjusted as usize, self.r.addend, addr as u64
                    );
                }
            }
            RelocationKind::PltRelative => {
                // L + A - P, 32 bit output
                // L = address of the symbols entry within the procedure linkage table
                // A = value of the Addend
                // P = address of the place of the relocation

                let name = &self.name;
                let addend = self.r.addend;

                // complicated pointer arithmetic to update the relocations
                //
                unsafe {
                    let patch = patch_base.offset(self.offset as isize);

                    let symbol_address = addr as isize + addend as isize - patch as isize;

                    // patch as 32 bit
                    let patch = patch as *mut u32;
                    *patch = symbol_address as u32; //patch.replace(symbol_address as u32);

                    println!(
                            "rel {}: patch:{:#08x} patchv:{:#08x} addend:{:#08x} addr:{:#08x} symbol:{:#08x}",
                            name,
                            patch as usize,
                            std::ptr::read(patch),
                            addend,
                            addr as isize,
                            symbol_address as isize,
                            );
                }
            }
            _ => unimplemented!(),
        }
    }
}

pub fn patch_code(
    block: PatchCodeBlock,
    pointers: &PatchSymbolPointers,
) -> (LinkedBlock, LinkedSymbolPointers) {
    println!(
        "patching code {} at base {:#08x}",
        &block.name,
        block.block.as_ptr() as usize
    );

    for r in &block.relocations {
        let patch_base = block.block.as_ptr();
        let addr = *pointers.get(&r.name).unwrap() as *const u8;
        r.patch(patch_base, addr);
    }

    let mut symbols = im::HashMap::new();
    for (name, s) in block.symbols {
        symbols.insert(name, s);
    }

    (
        LinkedBlock(Arc::new(LinkedBlockInner::Code(
            block.block.make_exec_block().unwrap(),
        ))),
        symbols,
    )
}

pub fn patch_data(
    block: PatchDataBlock,
    pointers: &PatchSymbolPointers,
) -> (LinkedBlock, LinkedSymbolPointers) {
    println!(
        "patching data {} at base {:#08x}",
        &block.name,
        block.block.as_ptr() as usize
    );

    for r in &block.relocations {
        let patch_base = block.block.as_ptr();
        let addr = *pointers.get(&r.name).unwrap() as *const u8;
        r.patch(patch_base, addr);
    }

    let mut symbols = im::HashMap::new();
    for (name, s) in block.symbols {
        symbols.insert(name, s);
    }

    (
        LinkedBlock(Arc::new(LinkedBlockInner::DataRW(block.block))),
        symbols,
    )
}
