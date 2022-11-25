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
        eprintln!("Debug:");
        for (k, v) in &self.linked {
            eprintln!("link: {:?}", (&k, &v));
        }

        eprintln!("Process Maps:");
        for map in proc_maps::get_process_maps(std::process::id() as proc_maps::Pid).unwrap() {
            eprintln!(
                "Map: {:#08x}+{:x}, {}, {:?}",
                map.start(),
                map.size(),
                map.flags,
                map.filename()
            );
        }
    }

    pub fn compare(&self, symbol: &str) {
        let v1 = self.pointers.get(symbol);
        let v2 = self.libraries.search_dynamic(symbol);
        let v3 = self.lookup(symbol);
        eprintln!("1:{}: {:?}", symbol, v1);
        eprintln!("2:{}: {:?}", symbol, v2);
        eprintln!("3:{}: {:?}", symbol, v3);
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
            println!("invoking {} @ {:#08x}", name, ptr as usize);
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
            block.disassemble();
            blocks.push((name, block));
        }
        let name = format!("{}.code", &unlinked.name);
        if let Some(block) = unlinked.create_code(&name, &mut link.mem)? {
            block.disassemble();
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
                            eprintln!(
                                "Searching Shared {:#08x}:{:#08x}:{}",
                                ptr as usize, v as usize, symbol
                            );
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
            PatchBlock::Data(PatchDataBlock { symbols, .. }) => {
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
                            let direct = patch_pointers.get(&r.name).unwrap();
                            add_to_got.insert(r.name.clone(), *direct);
                            /*
                            unsafe {
                                let buf = std::slice::from_raw_parts(direct.as_ptr() as *const u8, std::mem::size_of::<*const u8>());
                                let mut p = got.create_buffer(buf.len());
                                p.copy(buf);
                                all_pointers.insert(r.name.clone(), RelocationPointer::Got(p.as_ptr() as *const ()));
                                got = got.update(r.name.clone(), p);
                            }
                            */
                        }
                        AddToPlt => {
                            let direct = patch_pointers.get(&r.name).unwrap();
                            add_to_plt.insert(r.name.clone(), *direct);
                            //add_to_plt.insert(r.name.clone());
                            /*
                            let direct = all_pointers.get(&r.name).unwrap();
                            unsafe {
                                let buf = std::slice::from_raw_parts(direct.as_ptr() as *const u8, std::mem::size_of::<*const u8>());
                                let mut p = plt.create_buffer(buf.len());
                                p.copy(buf);
                                all_pointers.insert(r.name.clone(), RelocationPointer::Got(p.as_ptr() as *const ()));
                                plt = plt.update(r.name.clone(), p);
                            }
                            */
                        }
                        DoNothing => (),
                    }
                }
            }
        }
    }

    let mut all_pointers = patch_pointers.clone();

    for (name, direct) in add_to_got {
        // cast pointer to usize
        let v = direct.as_ptr() as usize;
        // write usize to buffer
        let buf = v.to_ne_bytes();
        //let buf = std::slice::from_raw_parts(direct.as_ptr() as *const u8, std::mem::size_of::<*const u8>());
        let mut p = got.create_buffer(buf.len());
        p.copy(buf.as_slice());
        all_pointers.insert(
            name.clone(),
            RelocationPointer::Got(p.as_ptr() as *const ()), //direct
        );
        got = got.update(name, p);
    }

    for (name, direct) in add_to_plt {
        // cast pointer to usize
        let v = direct.as_ptr() as usize;
        // write usize to buffer
        //let size = 1 + std::mem::size_of::<u32>();
        let mut buf = [0u8; 5];
        buf[0] = 0xe9;
        //let mut buf = Vec::with_capacity(size);
        //buf.insert
        //buf.as_mut_slice()[0..1].copy_from_slice(&[0xe9].as_slice());
        //buf.as_mut_slice()[1..5].copy_from_slice(&v.to_ne_bytes());
        let mut p = got.create_buffer(buf.len());
        p.copy(buf.as_slice());

        //unsafe {
        //let buf = std::slice::from_raw_parts(direct.as_ptr() as *const u8, std::mem::size_of::<*const u8>());
        //let mut p = plt.create_buffer(buf.len());
        //p.copy(buf);
        //all_pointers.insert(name.clone(), RelocationPointer::Plt(p.as_ptr() as *const ()));
        //plt = plt.update(name, p);
        //}
    }

    for (_block_name, block) in &blocks {
        //
        //
        // look up all of the unknowns in the shared libraries, if they exist
        /*
        if let PatchBlock::Code(PatchCodeBlock { unknowns, .. }) = block {
            for symbol in unknowns {
                if let Some(ptr) = link.libraries.search_dynamic(symbol) {
                    // data pointers should already have a got in the shared library
                    eprintln!("Searching Shared {:#08x}: {}", ptr as usize, symbol);
                    all_pointers.insert(symbol.clone(), ptr.clone());
                }
            }
        }
        */

        // add all of the symbols
        match block {
            PatchBlock::Code(PatchCodeBlock {
                symbols, externs, ..
            }) => {
                for symbol in externs {
                    //if let Some(ptr) = link.libraries.search_dynamic(symbol) {
                    // data pointers should already have a got in the shared library
                    //eprintln!("Searching Shared {:#08x}: {}", ptr as usize, symbol);
                    //all_pointers.insert(symbol.clone(), ptr.clone());
                    /*
                    unsafe {
                        let buf = std::slice::from_raw_parts(ptr as *const u8, std::mem::size_of::<*const u8>());
                        let mut p = got.create_buffer(buf.len());
                        p.copy(buf);
                        all_pointers.insert(symbol.clone(), p.as_ptr() as *const ());
                        got = got.update(symbol.clone(), p);
                    }
                    */
                    //}
                }

                for (symbol, ptr) in symbols {
                    //all_pointers.insert(symbol.clone(), ptr.clone());
                }
            }
            PatchBlock::Data(PatchDataBlock { symbols, .. }) => {
                for (symbol, ptr) in symbols {
                    //all_pointers.insert(symbol.clone(), ptr.clone());
                    //
                    /*/
                    unsafe {
                        let buf = std::slice::from_raw_parts(*ptr as *const u8, std::mem::size_of::<*const u8>());
                        let mut p = got.create_buffer(buf.len());
                        p.copy(buf);
                        all_pointers.insert(symbol.clone(), p.as_ptr() as *const ());
                        got = got.update(symbol.clone(), p);
                    }
                    */
                }
            }
        }
    }

    //let mut all_linked_pointers = im::HashMap::new();
    //all_linked_pointers = all_linked_pointers.union(all_pointers.clone());

    eprintln!("all pointers");
    for (k, p) in &all_pointers {
        unsafe {
            let v = p.as_ptr() as *const usize;
            eprintln!("p: {}:{:#08x}:{}", p, v as usize, k);
        }
    }

    eprintln!("patch pointers");
    for (k, p) in &patch_pointers {
        unsafe {
            let v = p.as_ptr() as *const usize;
            eprintln!("p: {}:{:#08x}:{}", p, v as usize, k);
        }
    }

    // patch everything
    let mut linked = im::HashMap::new();
    for (block_name, block) in blocks {
        let patched_block = //, linked_pointers) = //, new_got, new_plt) =
            block.patch(all_pointers.clone(), got.clone(), plt.clone()); //, got.clone(), plt.clone());
                                                                         //got = new_got;
                                                                         //plt = new_plt;
        patched_block.disassemble();
        linked.insert(block_name.clone(), patched_block);
        //all_linked_pointers = all_linked_pointers.union(linked_pointers);
    }

    got.debug();
    plt.debug();
    eprintln!("Symbols");
    //for (k, v) in &all_linked_pointers {
    //eprintln!("{:#08x}: {}", v.as_ptr() as usize, k);
    //}
    for (k, v) in &all_pointers {
        //let ptr = v.as_ptr() as *const usize;
        unsafe {
            eprintln!(
                "p: {}:{:#08x}:{}",
                v,
                *(v.as_ptr() as *const usize) as usize,
                k
            );
        }
    }

    Ok(LinkVersion {
        linked,
        pointers: all_pointers,
        libraries: link.libraries.clone(),
        got,
        plt,
    })
}
