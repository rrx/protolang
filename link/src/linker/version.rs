use crate::*;
use std::error::Error;
use std::ffi::CString;
use std::sync::Arc;

#[derive(Clone)]
pub struct LinkVersion {
    linked: im::HashMap<String, LinkedBlock>,
    got: TableVersion,
    plt: TableVersion,
    pointers: LinkedSymbolPointers,
    libraries: SharedLibraryRepo,
}

impl LinkVersion {
    /*
    pub fn new() -> Self {
        Self {
            linked: im::HashMap::new(),
            pointers: im::HashMap::new(),
            libraries: SharedLibraryRepo::default(),
        }
    }
    */

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

    pub fn lookup(&self, symbol: &str) -> Option<*const ()> {
        match self.pointers.get(symbol) {
            Some(ptr) => Some(*ptr),
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

pub fn build_version(
    blocks: Vec<(String, PatchBlock)>,
    link: &Link,
) -> Result<LinkVersion, Box<dyn Error>> {
    let mut got = link.got.clone();
    let mut plt = link.plt.clone();

    // generate a list of symbols and their pointers
    let mut all_pointers = im::HashMap::new();

    for (_block_name, block) in &blocks {
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
                symbols, unknowns, ..
            }) => {
                for symbol in unknowns {
                    if let Some(ptr) = link.libraries.search_dynamic(symbol) {
                        // data pointers should already have a got in the shared library
                        eprintln!("Searching Shared {:#08x}: {}", ptr as usize, symbol);
                        all_pointers.insert(symbol.clone(), ptr.clone());
                        /*
                        unsafe {
                            let buf = std::slice::from_raw_parts(ptr as *const u8, std::mem::size_of::<*const u8>());
                            let mut p = got.create_buffer(buf.len());
                            p.copy(buf);
                            all_pointers.insert(symbol.clone(), p.as_ptr() as *const ());
                            got = got.update(symbol.clone(), p);
                        }
                        */
                    }
                }

                for (symbol, ptr) in symbols {
                    all_pointers.insert(symbol.clone(), ptr.clone());
                }
            }
            PatchBlock::Data(PatchDataBlock { symbols, .. }) => {
                for (symbol, ptr) in symbols {
                    all_pointers.insert(symbol.clone(), ptr.clone());
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

    // patch everything
    let mut all_linked_pointers = im::HashMap::new();
    let mut linked = im::HashMap::new();
    all_linked_pointers = all_linked_pointers.union(all_pointers.clone());

    for (k, v) in &all_pointers {
        let ptr = *v as *const usize;
        unsafe {
            eprintln!("p: {:#08x}:{:#08x}:{}", ptr as usize, *ptr as usize, k);
        }
    }

    for (block_name, block) in blocks {
        let (patched_block, linked_pointers, new_got, new_plt) =
            block.patch(all_pointers.clone(), got.clone(), plt.clone());
        got = new_got;
        plt = new_plt;
        patched_block.disassemble();
        linked.insert(block_name.clone(), patched_block);
        all_linked_pointers = all_linked_pointers.union(linked_pointers);
    }

    got.debug();
    plt.debug();
    eprintln!("Symbols");
    for (k, v) in &all_linked_pointers {
        eprintln!("{:#08x}: {}", *v as usize, k);
    }

    Ok(LinkVersion {
        linked,
        pointers: all_linked_pointers,
        libraries: link.libraries.clone(),
        got,
        plt,
    })
}
