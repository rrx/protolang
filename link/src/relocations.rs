use capstone::prelude::*;
use object::{Relocation, RelocationEncoding, RelocationKind, RelocationTarget};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::ffi::CString;
use std::path::Path;
use std::sync::Arc;

use memmap::{Mmap, MmapMut};
use std::fmt;
use std::fs;

use super::*;

#[derive(Debug, Clone)]
pub struct LinkRelocation {
    pub(crate) kind: RelocationKind,
    pub(crate) encoding: RelocationEncoding,
    pub(crate) size: u8,
    pub(crate) target: RelocationTarget,
    pub(crate) addend: i64,
    pub(crate) implicit_addend: bool,
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

#[derive(Debug, Clone)]
pub struct Reloc {
    pub(crate) symbol_name: String,
    pub(crate) r: LinkRelocation,
}

// given a map of pointers, patch the unpatched code page, and return a patched code page
pub fn patch(
    mut code: UnpatchedCodePage,
    pointers: im::HashMap<String, *const ()>,
) -> Result<CodePage, Box<dyn Error>> {
    println!(
        "patching {} at base {:#08x}",
        &code.name,
        code.m.as_ptr() as usize
    );
    for (reloc_offset, rel) in &code.relocations {
        println!("r@{:#04x}: {:?}", &reloc_offset, &rel);
        match rel.r.kind {
            RelocationKind::Elf(42) => {
                // got entry + addend - reloc_offset(patch)
                // we are computing the offset from the current instruction pointer
                unsafe {
                    let patch_base = code.m.as_mut_ptr() as *mut u8;
                    let patch = patch_base.offset(*reloc_offset);

                    // get the entry in the lookup table
                    let addr = *pointers.get(&rel.symbol_name).unwrap();

                    // this works
                    let value = addr as isize + rel.r.addend as isize - patch as isize;

                    // this does not work
                    //let value = patch as isize + rel.r.addend as isize - addr as isize;

                    let before = std::ptr::read(patch);
                    (patch as *mut u32).replace(value as u32);
                    println!("patch_base: {:#08x}", patch_base as usize);
                    println!("patch: {:#08x}", patch as usize);
                    println!("value: {:#04x}", value as u32);

                    println!(
                        "rel got {}: patch {:#08x}:{:#08x}=>{:#08x} addend:{:#08x} addr:{:#08x}",
                        &rel.symbol_name,
                        patch as usize,
                        before,
                        value as u32,
                        rel.r.addend,
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
                let name = &rel.symbol_name;
                println!("look up: {}", name);
                unsafe {
                    let patch_base = code.m.as_mut_ptr() as *mut u8;

                    // address of remote
                    let addr = *pointers.get(name).unwrap();
                    let adjusted = addr as isize + rel.r.addend as isize;

                    let (before, patch) = match rel.r.size {
                        32 => {
                            // patch as 32 bit
                            let patch = patch_base.offset(*reloc_offset as isize) as *mut u32;
                            let before = std::ptr::read(patch);
                            patch.replace(adjusted as u32);
                            (before as u64, patch as u64)
                        }
                        64 => {
                            // patch as 64 bit
                            let patch = patch_base.offset(*reloc_offset as isize) as *mut u64;
                            let before = std::ptr::read(patch);
                            patch.replace(adjusted as u64);
                            (before as u64, patch as u64)
                        }
                        _ => unimplemented!(),
                    };

                    println!(
                        "rel absolute {}: patch {:#08x}:{:#08x}=>{:#08x} addend:{:#08x} addr:{:#08x}",
                        name,
                        patch,
                        before,
                        adjusted as u32,
                        rel.r.addend,
                        addr as u32,
                        );
                }
            }
            RelocationKind::PltRelative => {
                // L + A - P, 32 bit output
                // L = address of the symbols entry within the procedure linkage table
                // A = value of the Addend
                // P = address of the place of the relocation

                let name = &rel.symbol_name;
                println!("look up: {}", name);
                let symbol_addr = *pointers.get(name).unwrap();
                let addend = rel.r.addend;

                // complicated pointer arithmetic to update the relocations
                //
                unsafe {
                    let patch_base = code.m.as_mut_ptr() as *mut u8;
                    let patch = patch_base.offset(*reloc_offset as isize);

                    let symbol_address = symbol_addr as isize + addend as isize - patch as isize;

                    // patch as 32 bit
                    let patch = patch as *mut u32;
                    patch.replace(symbol_address as u32);

                    println!(
                        "rel {}: patch:{:#08x} patchv:{:#08x} addend:{:#08x} addr:{:#08x} symbol:{:#08x}",
                        name,
                        patch as usize,
                        std::ptr::read(patch),
                        addend,
                        symbol_addr as isize,
                        symbol_address as isize,
                        );
                }
            }
            _ => unimplemented!(),
        }
    }

    Ok(Arc::new(CodePageInner {
        kind: code.kind,
        m: code.m.make_exec()?,
        code_size: code.code_size,
        got_size: code.got_size,
        symbols: code.symbols.clone(),
        relocations: code.relocations.clone(),
        name: code.name.clone(),
    }))
}
