use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::ffi::CString;
use std::path::Path;
use std::sync::Arc;

use std::fs;

use super::*;
use crate::*;

pub struct Link {
    unlinked: HashMap<String, UnlinkedCodeSegment>,
    pub(crate) libraries: SharedLibraryRepo,
    mem: BlockFactory,
}

impl Link {
    pub fn new() -> Self {
        Self {
            unlinked: HashMap::new(),
            libraries: SharedLibraryRepo::default(),
            mem: BlockFactory::create(20).unwrap(),
        }
    }

    pub fn get_mem_ptr(&self) -> (*const u8, usize) {
        self.mem.get_mem_ptr()
    }

    pub fn remove(&mut self, name: &str) {
        self.unlinked.remove(&name.to_string());
    }

    pub fn add_library(&mut self, name: &str, path: &Path) -> Result<(), Box<dyn Error>> {
        unsafe {
            let lib = libloading::Library::new(path)?;
            // we need to parse the header files to know what all of the symbols mean
            // but we may as well just use bindgen?
            //let _: libloading::Symbol<unsafe extern fn() -> u32> = lib.get(b"gzopen")?;
            self.libraries.add(name, lib);
            eprintln!("Loaded library: {}", &path.to_string_lossy());
        }
        Ok(())
    }

    fn add_segments(&mut self, segments: Vec<UnlinkedCodeSegmentInner>) {
        for s in segments {
            self.unlinked.insert(s.name.clone(), Arc::new(s));
        }
    }

    pub fn add_obj_file(&mut self, name: &str, path: &Path) -> Result<(), Box<dyn Error>> {
        let buf = fs::read(path)?;
        self.add_obj_buf(name, buf.as_slice())
    }

    pub fn add_obj_buf(&mut self, name: &str, buf: &[u8]) -> Result<(), Box<dyn Error>> {
        let segments = UnlinkedCodeSegmentInner::create_segments(name, buf)?;
        self.add_segments(segments);
        Ok(())
    }

    pub fn link(&mut self) -> Result<LinkVersion, Box<dyn Error>> {
        let mut pointers = im::HashMap::new();
        let mut duplicates = HashSet::new();

        // get all of the symbols and the name that provides it
        for (link_name, unlinked) in &self.unlinked {
            println!("Linking: {}", link_name);
            for (symbol_name, code_symbol) in &unlinked.symbols {
                if code_symbol.def == CodeSymbolDefinition::Defined {
                    //println!("\tSymbol: {}", &symbol_name);
                    if pointers.contains_key(symbol_name) {
                        println!("\tDuplicate symbol: {}", &symbol_name);
                        duplicates.insert(symbol_name);
                    } else {
                        pointers.insert(symbol_name.clone(), code_symbol.address);
                    }
                }
            }
        }

        let mut relocations = HashSet::new();
        let mut missing = HashSet::new();
        for (_name, unlinked) in &self.unlinked {
            let mut children = HashSet::new();
            //println!("checking: {}", name);
            // ensure all relocations map somewhere
            for r in &unlinked.relocations {
                //println!("\tReloc: {}", &symbol_name);
                if pointers.contains_key(&r.name)
                    || self.libraries.search_dynamic(&r.name).is_some()
                {
                    children.insert(r.name.clone());
                    relocations.insert(r.name.clone());
                } else {
                    println!("\tSymbol {} missing", &r.name);
                    missing.insert(r.name.clone());
                }
            }
        }

        if missing.len() == 0 && duplicates.len() == 0 {
            let mut blocks = vec![];
            for (_name, unlinked) in &self.unlinked {
                let name = format!("{}.data", &unlinked.name);
                if let Some(block) = unlinked.create_data(&name, &mut self.mem)? {
                    block.disassemble();
                    blocks.push((name, block));
                }
                let name = format!("{}.code", &unlinked.name);
                if let Some(block) = unlinked.create_code(&name, &mut self.mem)? {
                    block.disassemble();
                    blocks.push((name, block));
                }
            }
            build_version(blocks, &self)
        } else {
            Err(LinkError::MissingSymbol.into())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn linker_segfault() {
        let mut b = Link::new();
        b.add_library("test", Path::new("libsigsegv.so"));
        b.add_obj_file("test", Path::new("../tmp/segfault.o"))
            .unwrap();
        let version = b.link().unwrap();
        //let ret: i64 = version.invoke("handlers_init", ()).unwrap();
        //let ret: i64 = version.invoke("segfault_me", ()).unwrap();
        //println!("ret: {:#08x}", ret);
        //assert_eq!(13, ret);
    }

    #[test]
    fn linker_global_long() {
        let mut b = Link::new();
        b.add_obj_file("test", Path::new("../tmp/live.o")).unwrap();
        let collection = b.link().unwrap();

        let ret: i64 = collection.invoke("call_live", (3,)).unwrap();
        println!("ret: {:#08x}", ret);
        assert_eq!(17, ret);

        let ret: i64 = collection.invoke("simple_function", ()).unwrap();
        println!("ret: {:#08x}", ret);
        assert_eq!(1, ret);

        let ret: i64 = collection.invoke("func2", (2,)).unwrap();
        println!("ret: {:#08x}", ret);
        assert_eq!(3, ret);
    }

    #[test]
    fn linker_shared() {
        let mut b = Link::new();
        b.add_library("gz", Path::new("../tmp/libz.so")).unwrap();
        b.add_obj_file("test", Path::new("../tmp/link_shared.o"))
            .unwrap();
        let collection = b.link().unwrap();
        let _ret: std::ffi::c_void = collection.invoke("call_z", ()).unwrap();
        //println!("ret: {:#08x}", ret);
        //assert_eq!(3, ret);
    }

    #[test]
    fn linker_livelink() {
        let mut b = Link::new();

        // unable to link, missing symbol
        b.add_obj_file("test1", Path::new("../tmp/testfunction.o"))
            .unwrap();
        assert_eq!(false, b.link().is_ok());

        // provide missing symbol
        b.add_obj_file("asdf", Path::new("../tmp/asdf.o")).unwrap();
        assert_eq!(true, b.link().is_ok());

        // links fine
        b.add_obj_file("simple", Path::new("../tmp/simplefunction.o"))
            .unwrap();
        assert_eq!(true, b.link().is_ok());

        let collection = b.link().unwrap();
        let ret: i64 = collection.invoke("func", ()).unwrap();
        println!("ret: {}", ret);
        assert_eq!(10001, ret);

        let ret: i64 = collection.invoke("simple", ()).unwrap();
        println!("ret: {}", ret);
        assert_eq!(10012, ret);

        let ret: i64 = collection.invoke("call_external", ()).unwrap();
        println!("ret: {}", ret);
        assert_eq!(4, ret);

        let ret: i64 = collection.invoke("asdf", (2,)).unwrap();
        println!("ret: {}", ret);
        assert_eq!(3, ret);
    }
}
