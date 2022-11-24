use object::{Relocation, RelocationEncoding, RelocationKind, RelocationTarget};
use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

use super::*;

const R_X86_64_GOTPCREL: u32 = 41;
const R_X86_64_REX_GOTP: u32 = 42;

pub enum PatchEffect {
    AddToGot,
    AddToPlt,
    DoNothing,
}

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
    ) -> PatchEffect {
        use PatchEffect::*;

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
                    println!("addr:  {:#08x}", addr as usize);

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
                AddToGot
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
                    println!("addr:  {:#08x}", addr as usize);

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
                AddToGot
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
                DoNothing
            }

            RelocationKind::Relative => {
                unsafe {
                    let patch = patch_base.offset(self.offset as isize);
                    let symbol_address = addr as isize + self.r.addend as isize - patch as isize;

                    // patch as 32 bit
                    let patch = patch as *mut u32;
                    *patch = symbol_address as u32;

                    println!(
                            "rel relative {}: patch:{:#08x} patchv:{:#08x} addend:{:#08x} addr:{:#08x} symbol:{:#08x}",
                            &self.name,
                            patch as usize,
                            std::ptr::read(patch),
                            self.r.addend,
                            addr as isize,
                            symbol_address as isize,
                            );
                }
                DoNothing
            }

            RelocationKind::PltRelative => {
                // L + A - P, 32 bit output
                // L = address of the symbols entry within the procedure linkage table
                // A = value of the Addend
                // P = address of the place of the relocation

                // address should point to the PLT

                // complicated pointer arithmetic to update the relocations
                unsafe {
                    let patch = patch_base.offset(self.offset as isize);

                    let symbol_address = addr as isize + self.r.addend as isize - patch as isize;

                    // patch as 32 bit
                    let patch = patch as *mut u32;
                    *patch = symbol_address as u32;

                    println!(
                            "rel {}: patch:{:#08x} patchv:{:#08x} addend:{:#08x} addr:{:#08x} symbol:{:#08x}",
                            &self.name,
                            patch as usize,
                            std::ptr::read(patch),
                            self.r.addend,
                            addr as isize,
                            symbol_address as isize,
                            );
                }
                AddToPlt
            }
            _ => unimplemented!(),
        }
    }
}

pub fn patch_code(
    block: PatchCodeBlock,
    pointers: PatchSymbolPointers,
    mut got: TableVersion,
    mut plt: TableVersion,
) -> (
    LinkedBlock,
    LinkedSymbolPointers,
    TableVersion,
    TableVersion,
) {
    println!(
        "patching code {} at base {:#08x}",
        &block.name,
        block.block.as_ptr() as usize
    );

    let mut add_to_got = HashMap::new();
    let mut add_to_plt = HashMap::new();

    for r in &block.relocations {
        let patch_base = block.block.as_ptr();
        let addr = *pointers.get(&r.name).unwrap() as *const u8;
        match r.patch(patch_base, addr) {
            PatchEffect::AddToPlt => {
                add_to_plt.insert(&r.name, addr);
            }
            PatchEffect::AddToGot => {
                add_to_got.insert(&r.name, addr);
            }
            _ => (),
        }
    }

    let mut symbols = im::HashMap::new();
    for (name, s) in block.symbols {
        symbols.insert(name, s);
    }

    let mut symbols = im::HashMap::new();
    for (name, ptr) in add_to_got {
        unsafe {
            let buf = std::slice::from_raw_parts(ptr, std::mem::size_of::<*const u8>());
            let mut p = got.create_buffer(buf.len());
            p.copy(buf);
            //symbols.insert(name.clone(), p.as_ptr() as *const ());
            got = got.update(name.clone(), p);
        }
    }

    for (name, ptr) in add_to_plt {
        unsafe {
            let buf = std::slice::from_raw_parts(ptr, std::mem::size_of::<*const u8>());
            let mut p = plt.create_buffer(buf.len());
            p.copy(buf);
            //symbols.insert(name.clone(), p.as_ptr() as *const ());
            plt = plt.update(name.clone(), p);
        }
    }

    (
        LinkedBlock(Arc::new(LinkedBlockInner::Code(
            block.block.make_exec_block().unwrap(),
        ))),
        symbols,
        got,
        plt,
    )
}

pub fn patch_data(
    block: PatchDataBlock,
    pointers: PatchSymbolPointers,
    mut got: TableVersion,
    mut plt: TableVersion,
) -> (
    LinkedBlock,
    LinkedSymbolPointers,
    TableVersion,
    TableVersion,
) {
    println!(
        "patching data {} at base {:#08x}",
        &block.name,
        block.block.as_ptr() as usize
    );

    let mut add_to_got = HashMap::new();
    let mut add_to_plt = HashMap::new();

    for r in &block.relocations {
        let patch_base = block.block.as_ptr();
        let addr = *pointers.get(&r.name).unwrap() as *const u8;
        match r.patch(patch_base, addr) {
            PatchEffect::AddToPlt => {
                add_to_plt.insert(&r.name, addr);
            }
            PatchEffect::AddToGot => {
                add_to_got.insert(&r.name, addr);
            }
            _ => (),
        }
    }

    let mut symbols = im::HashMap::new();
    for (name, s) in block.symbols {
        symbols.insert(name, s);
    }

    for (name, ptr) in add_to_got {
        unsafe {
            let buf = std::slice::from_raw_parts(ptr, std::mem::size_of::<*const u8>());
            let mut p = got.create_buffer(buf.len());
            p.copy(buf);
            //symbols.insert(name.clone(), p.as_ptr() as *const ());
            got = got.update(name.clone(), p);
        }
    }

    for (name, ptr) in add_to_plt {
        unsafe {
            let buf = std::slice::from_raw_parts(ptr, std::mem::size_of::<*const u8>());
            let mut p = plt.create_buffer(buf.len());
            p.copy(buf);
            //symbols.insert(name.clone(), p.as_ptr() as *const ());
            plt = plt.update(name.clone(), p);
        }
    }

    (
        LinkedBlock(Arc::new(LinkedBlockInner::DataRW(block.block))),
        symbols,
        got,
        plt,
    )
}
