use crate::*;
use std::collections::HashMap;
use std::error::Error;
use std::ptr::NonNull;

#[derive(Clone)]
pub struct LinkVersion {
    linked: im::HashMap<String, LinkedBlock>,
    got: TableVersion,
    plt: TableVersion,
    pointers: LinkedSymbolPointers,
    libraries: SharedLibraryRepo,
}

impl LinkVersion {
    pub fn debug(&self) {
        log::debug!("Debug:");
        for (k, v) in &self.linked {
            log::debug!("link: {:?}", (&k, &v));
        }
        eprint_process_maps();
    }

    pub fn lookup(&self, symbol: &str) -> Option<RelocationPointer> {
        match self.pointers.get(symbol) {
            //Some(RelocationPointer::Got(ptr)) => unsafe {
            //Some(*(*ptr as *const usize) as *const ())
            //},
            Some(ptr) => Some(ptr.clone()),
            None => self.libraries.search_dynamic(symbol),
        }
    }

    pub fn invoke<P, T>(&self, name: &str, args: P) -> Result<T, Box<dyn Error>> {
        // call the main function

        // make sure we dereference the pointer!
        let ptr = self.lookup(name).ok_or(LinkError::SymbolNotFound)?;
        unsafe {
            type MyFunc<P, T> = unsafe extern "cdecl" fn(P) -> T;
            log::debug!("invoking {} @ {:#08x}", name, ptr.as_ptr() as usize);
            let f: MyFunc<P, T> = std::mem::transmute(ptr.as_ptr());
            let ret = f(args);
            Ok(ret)
        }
    }
}

pub fn build_version(link: &mut Link) -> Result<LinkVersion, Box<dyn Error>> {
    // create data and code patch blocks
    let mut blocks = vec![];
    for (_name, unlinked) in &link.unlinked {
        let name = format!("{}.data", &unlinked.name);
        if let Some(block) = unlinked.create_data(&name, &mut link.mem)? {
            //block.disassemble();
            blocks.push((name, block));
        }
        let name = format!("{}.code", &unlinked.name);
        if let Some(block) = unlinked.create_code(&name, &mut link.mem)? {
            //block.disassemble();
            blocks.push((name, block));
        }
    }

    // generate a list of symbols and their pointers
    let mut pointers = im::HashMap::new();
    for (_block_name, block) in &blocks {
        match block {
            PatchBlock::Code(PatchCodeBlock {
                symbols, externs, ..
            }) => {
                for symbol in externs {
                    if let Some(ptr) = link.libraries.search_dynamic(symbol) {
                        // data pointers should already have a got in the shared library
                        unsafe {
                            let p = ptr.as_ptr() as *const usize;
                            let v = *p as *const usize;
                            log::debug!(
                                "Searching Shared {:#08x}:{:#08x}:{}",
                                p as usize,
                                v as usize,
                                symbol
                            );

                            pointers.insert(symbol.clone(), ptr);
                        }
                    }
                }

                for (symbol, ptr) in symbols {
                    //let p =
                    //RelocationPointer::Direct(NonNull::new(ptr.clone() as *mut u8).unwrap());
                    pointers.insert(symbol.clone(), *ptr);
                }
            }
            PatchBlock::Data(PatchDataBlock {
                symbols, internal, ..
            }) => {
                for (symbol, ptr) in internal {
                    //let p =
                    //RelocationPointer::Direct(NonNull::new(ptr.clone() as *mut u8).unwrap());
                    pointers.insert(symbol.clone(), *ptr);
                }

                for (symbol, ptr) in symbols {
                    //let p =
                    //RelocationPointer::Direct(NonNull::new(ptr.clone() as *mut u8).unwrap());
                    pointers.insert(symbol.clone(), *ptr);
                }
            }
        }
    }

    let mut add_to_got = HashMap::new();
    let mut add_to_plt = HashMap::new();
    let mut add_direct = HashMap::new();

    // generate a list of GOT/PLT entries to create from the relocation list
    for (_block_name, block) in &blocks {
        use PatchEffect::*;
        match block {
            PatchBlock::Code(PatchCodeBlock { relocations, .. }) => {
                for r in relocations {
                    let direct = pointers
                        .get(&r.name)
                        .expect(&format!("symbol missing {}", &r.name));
                    match r.effect() {
                        AddToGot => add_to_got.insert(r.name.clone(), *direct),
                        AddToPlt => add_to_plt.insert(r.name.clone(), *direct),
                        DoNothing => add_direct.insert(r.name.clone(), *direct),
                    };
                }
            }
            PatchBlock::Data(PatchDataBlock {
                symbols,
                relocations,
                ..
            }) => {
                for (symbol, ptr) in symbols {
                    //let p = RelocationPointer::Direct(ptr.clone());
                    //let p =
                    //RelocationPointer::Direct(NonNull::new(ptr.clone() as *mut u8).unwrap());
                    add_to_got.insert(symbol.clone(), *ptr);
                }

                // add all data objects to the GOT
                for r in relocations {
                    let direct = pointers
                        .get(&r.name)
                        .expect(&format!("symbol missing {}", &r.name));
                    add_to_got.insert(r.name.clone(), *direct);
                }
            }
        }
    }

    // patch source is used for relocations
    let mut patch_source = im::HashMap::new();

    // create a direct entries for patch
    for (name, direct) in add_direct {
        patch_source.insert(
            name.clone(),
            RelocationPointer::Direct(NonNull::new(direct.as_ptr() as *mut u8).unwrap()),
        );
    }

    // get tables
    let mut got = link.got.clone();
    let mut plt = link.plt.clone();

    // create a GOT entry, and add it to the mapping for patch
    for (name, direct) in add_to_got {
        // cast pointer to usize
        let v = direct.as_ptr() as usize;
        // write usize to buffer
        let buf = v.to_ne_bytes();
        let mut p = got.create_buffer(buf.len());
        p.copy(buf.as_slice());
        patch_source.insert(
            name.clone(),
            RelocationPointer::Got(NonNull::new(p.as_ptr() as *mut u8).unwrap()),
        );
        got = got.update(name, p);
    }

    // create a PLT entry
    // TODO, this is not complete
    for (name, direct) in add_to_plt {
        // cast pointer to usize
        let _v = direct.as_ptr() as usize;
        // write usize to buffer
        let mut buf = [0u8; 5];
        buf[0] = 0xe9;
        let mut p = got.create_buffer(buf.len());
        p.copy(buf.as_slice());

        // save as direct for now
        patch_source.insert(
            name.clone(),
            RelocationPointer::Direct(NonNull::new(direct.as_ptr() as *mut u8).unwrap()),
        );
    }

    if log::log_enabled!(log::Level::Debug) {
        log::debug!("patch source");
        for (k, p) in &patch_source {
            let v = p.as_ptr() as *const usize;
            log::debug!("p: {}:{:#08x}:{}", p, v as usize, k);
        }

        log::debug!("patch pointers");
        for (k, p) in &pointers {
            let v = p.as_ptr() as *const usize;
            log::debug!("p: {}:{:#08x}:{}", p, v as usize, k);
        }
    }

    // patch everything
    let mut linked = im::HashMap::new();
    for (block_name, block) in blocks {
        let patched_block = block.patch(patch_source.clone(), got.clone(), plt.clone());
        //patched_block.disassemble();
        linked.insert(block_name.clone(), patched_block);
    }

    got.debug();
    plt.debug();
    log::debug!("Symbols");
    for (k, v) in &patch_source {
        unsafe {
            log::debug!(
                "p: {}:{:#08x}:{}",
                v,
                *(v.as_ptr() as *const usize) as usize,
                k
            );
        }
    }

    Ok(LinkVersion {
        linked,
        pointers,
        libraries: link.libraries.clone(),
        got,
        plt,
    })
}
