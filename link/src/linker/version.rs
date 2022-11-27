use crate::*;
use std::collections::HashMap;
use std::error::Error;

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

    pub fn compare(&self, symbol: &str) {
        let v1 = self.pointers.get(symbol);
        let v2 = self.libraries.search_dynamic(symbol);
        let v3 = self.lookup(symbol);
        log::debug!("1:{}: {:?}", symbol, v1);
        log::debug!("2:{}: {:?}", symbol, v2);
        log::debug!("3:{}: {:?}", symbol, v3);
    }

    pub fn lookup(&self, symbol: &str) -> Option<*const ()> {
        match self.pointers.get(symbol) {
            Some(RelocationPointer::Got(ptr)) => unsafe {
                Some(*(*ptr as *const usize) as *const ())
            },
            Some(ptr) => Some(ptr.as_ptr()),
            None => self.libraries.search_dynamic(symbol),
        }
    }

    pub fn invoke<P, T>(&self, name: &str, args: P) -> Result<T, Box<dyn Error>> {
        // call the main function

        // make sure we dereference the pointer!
        let ptr = self.lookup(name).ok_or(LinkError::SymbolNotFound)? as *const ();
        unsafe {
            type MyFunc<P, T> = unsafe extern "cdecl" fn(P) -> T;
            log::debug!("invoking {} @ {:#08x}", name, ptr as usize);
            let f: MyFunc<P, T> = std::mem::transmute(ptr);
            let ret = f(args);
            Ok(ret)
        }
    }
}

pub fn build_version(link: &mut Link) -> Result<LinkVersion, Box<dyn Error>> {
    // get tables
    let mut got = link.got.clone();
    let mut plt = link.plt.clone();

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
    let mut patch_pointers = im::HashMap::new();

    let mut add_to_got = HashMap::new();
    let mut add_to_plt = HashMap::new();

    for (_block_name, block) in &blocks {
        match block {
            PatchBlock::Code(PatchCodeBlock {
                symbols, externs, ..
            }) => {
                for symbol in externs {
                    if let Some(ptr) = link.libraries.search_dynamic(symbol) {
                        // data pointers should already have a got in the shared library
                        unsafe {
                            let ptr = ptr as *const usize;
                            let v = *ptr as *const usize;
                            log::debug!(
                                "Searching Shared {:#08x}:{:#08x}:{}",
                                ptr as usize,
                                v as usize,
                                symbol
                            );

                            // dereferencing the pointer doesn't work
                            //patch_pointers.insert(symbol.clone(), RelocationPointer::Direct(v as *const ()));
                            patch_pointers.insert(
                                symbol.clone(),
                                RelocationPointer::Direct(ptr as *const ()),
                            );
                        }
                    }
                }

                for (symbol, ptr) in symbols {
                    let p = RelocationPointer::Direct(ptr.clone());
                    patch_pointers.insert(symbol.clone(), p);
                }
            }

            PatchBlock::Data(PatchDataBlock {
                symbols, internal, ..
            }) => {
                for (symbol, ptr) in internal {
                    let p = RelocationPointer::Direct(ptr.clone());
                    patch_pointers.insert(symbol.clone(), p);
                }

                for (symbol, ptr) in symbols {
                    let p = RelocationPointer::Direct(ptr.clone());
                    patch_pointers.insert(symbol.clone(), p);
                    add_to_got.insert(symbol.clone(), p);
                }
            }
        }
    }

    for (_block_name, block) in &blocks {
        // for each relocation, create an associated got/plt entry and update pointers
        //
        use PatchEffect::*;
        match block {
            PatchBlock::Code(PatchCodeBlock { relocations, .. })
            | PatchBlock::Data(PatchDataBlock { relocations, .. }) => {
                for r in relocations {
                    match r.effect() {
                        AddToGot => {
                            let direct = patch_pointers
                                .get(&r.name)
                                .expect(&format!("symbol missing {}", &r.name));
                            add_to_got.insert(r.name.clone(), *direct);
                        }
                        AddToPlt => {
                            let direct = patch_pointers
                                .get(&r.name)
                                .expect(&format!("symbol missing {}", &r.name));
                            add_to_plt.insert(r.name.clone(), *direct);
                        }
                        DoNothing => (),
                    }
                }
            }
        }
    }

    // patch source is used for relocations
    let mut patch_source = patch_pointers.clone();

    for (name, direct) in add_to_got {
        // cast pointer to usize
        let v = direct.as_ptr() as usize;
        // write usize to buffer
        let buf = v.to_ne_bytes();
        let mut p = got.create_buffer(buf.len());
        p.copy(buf.as_slice());
        patch_source.insert(
            name.clone(),
            RelocationPointer::Got(p.as_ptr() as *const ()),
        );
        got = got.update(name, p);
    }

    for (name, direct) in add_to_plt {
        // cast pointer to usize
        let v = direct.as_ptr() as usize;
        // write usize to buffer
        let mut buf = [0u8; 5];
        buf[0] = 0xe9;
        let mut p = got.create_buffer(buf.len());
        p.copy(buf.as_slice());
    }

    log::debug!("patch source");
    for (k, p) in &patch_source {
        unsafe {
            let v = p.as_ptr() as *const usize;
            log::debug!("p: {}:{:#08x}:{}", p, v as usize, k);
        }
    }

    log::debug!("patch pointers");
    for (k, p) in &patch_pointers {
        unsafe {
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
        pointers: patch_source,
        libraries: link.libraries.clone(),
        got,
        plt,
    })
}
