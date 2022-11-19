use crate::*;
use std::error::Error;

#[derive(Clone)]
pub struct LinkVersion {
    linked: im::HashMap<String, LinkedBlock>,
    pointers: LinkedSymbolPointers,
}

impl LinkVersion {
    pub fn new() -> Self {
        Self {
            linked: im::HashMap::new(),
            pointers: im::HashMap::new(),
        }
    }

    pub fn debug(&self) {
        for (k, v) in &self.linked {
            println!("link: {:?}", (&k, &v));
        }
    }

    pub fn invoke<P, T>(&self, name: &str, args: P) -> Result<T, Box<dyn Error>> {
        // call the main function

        // make sure we dereference the pointer!
        let ptr = *self.pointers.get(name).ok_or(LinkError::SymbolNotFound)? as *const ();
        unsafe {
            type MyFunc<P, T> = unsafe extern "cdecl" fn(P) -> T;
            println!("invoking {} @ {:#08x}", name, ptr as usize);
            let f: MyFunc<P, T> = std::mem::transmute(ptr);

            for map in proc_maps::get_process_maps(std::process::id() as proc_maps::Pid).unwrap() {
                println!(
                    "Map: {:#08x}+{:x}, {}, {:?}",
                    map.start(),
                    map.size(),
                    map.flags,
                    map.filename()
                );
            }

            let ret = f(args);
            Ok(ret)
        }
    }
}

pub fn build_version(
    mut blocks: Vec<(String, PatchBlock)>,
    link: &Link,
) -> Result<LinkVersion, Box<dyn Error>> {
    // generate a list of symbols and their pointers
    let mut all_pointers = im::HashMap::new();

    for (block_name, block) in &blocks {
        // look up all of the unknowns in the shared libraries, if they exist
        if let PatchBlock::Code(PatchCodeBlock { unknowns, .. }) = block {
            for symbol in unknowns {
                if let Ok(Some(ptr)) = link.search_dynamic(symbol) {
                    all_pointers.insert(symbol.clone(), ptr.clone());
                }
            }
        }
        match block {
            PatchBlock::Code(PatchCodeBlock { symbols, .. })
            | PatchBlock::Data(PatchDataBlock { symbols, .. }) => {
                for (symbol, ptr) in symbols {
                    all_pointers.insert(symbol.clone(), ptr.clone());
                }
            }
        }
    }

    // patch everything
    let mut all_linked_pointers = im::HashMap::new();
    let mut linked = im::HashMap::new();
    for (block_name, mut block) in blocks {
        let (patched_block, linked_pointers) = block.patch(all_pointers.clone());
        patched_block.disassemble();
        linked.insert(block_name.clone(), patched_block);
        all_linked_pointers = all_linked_pointers.union(linked_pointers);
    }

    Ok(LinkVersion {
        linked,
        pointers: all_linked_pointers,
    })
}
