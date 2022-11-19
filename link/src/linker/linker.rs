use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::ffi::CString;
use std::path::Path;
use std::sync::Arc;

use std::fs;

use super::*;
use crate::*;

pub struct Link {
    pages: HashMap<String, UnlinkedCodeSegment>,
    libraries: HashMap<String, libloading::Library>,
    mem: BlockFactory,
}

impl Link {
    pub fn new() -> Self {
        Self {
            pages: HashMap::new(),
            libraries: HashMap::new(),
            mem: BlockFactory::create(10, 10).unwrap(),
        }
    }

    pub fn remove(&mut self, name: &str) {
        self.pages.remove(&name.to_string());
    }

    pub fn add_library(&mut self, name: &str, path: &Path) -> Result<(), Box<dyn Error>> {
        unsafe {
            let lib = libloading::Library::new(path)?;
            // we need to parse the header files to know what all of the symbols mean
            // but we may as well just use bindgen?
            //let _: libloading::Symbol<unsafe extern fn() -> u32> = lib.get(b"gzopen")?;
            self.libraries.insert(name.to_string(), lib);
            eprintln!("Loaded library: {}", &path.to_string_lossy());
        }
        Ok(())
    }

    fn add_segements(&mut self, segments: Vec<UnlinkedCodeSegmentInner>) {
        for s in segments {
            self.pages.insert(s.name.clone(), Arc::new(s));
        }
    }

    pub fn add_obj_file(&mut self, name: &str, path: &Path) -> Result<(), Box<dyn Error>> {
        let buf = fs::read(path)?;
        self.add_obj_buf(name, buf.as_slice())
    }

    pub fn add_obj_buf(&mut self, name: &str, buf: &[u8]) -> Result<(), Box<dyn Error>> {
        let segments = UnlinkedCodeSegmentInner::create_segments(name, buf)?;
        self.add_segements(segments);
        Ok(())
    }

    pub fn link(&mut self) -> Result<LinkVersion, Box<dyn Error>> {
        let mut pointers = im::HashMap::new();
        let mut duplicates = HashSet::new();

        // get all of the symbols and the name that provides it
        for (_name, unlinked) in &self.pages {
            //println!("linking: {}", name);
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
        for (_name, unlinked) in &self.pages {
            let mut children = HashSet::new();
            //println!("checking: {}", name);
            // ensure all relocations map somewhere
            for (symbol_name, _r) in &unlinked.relocations {
                //println!("\tReloc: {}", &symbol_name);
                if pointers.contains_key(symbol_name) || self.search_dynamic(symbol_name)?.is_some()
                {
                    children.insert(symbol_name.clone());
                    relocations.insert(symbol_name.clone());
                } else {
                    println!("\tSymbol {} missing", symbol_name);
                    missing.insert(symbol_name);
                }
            }
        }

        if missing.len() == 0 && duplicates.len() == 0 {
            let mut blocks = vec![];
            for (_name, unlinked) in &self.pages {
                let name = format!("{}_data", &unlinked.name);
                if let Some(block) = unlinked.create_data_mem(&name, &mut self.mem)? {
                    block.disassemble();
                    blocks.push((name, block));
                }
                let name = format!("{}_code", &unlinked.name);
                if let Some(block) = unlinked.create_code_mem(&name, &mut self.mem)? {
                    block.disassemble();
                    blocks.push((name, block));
                }
            }
            build_version(blocks, &self)
        } else {
            Err(LinkError::MissingSymbol.into())
        }
    }

    // search the dynamic libraries to see if the symbol exists
    pub fn search_dynamic(&self, symbol: &str) -> Result<Option<*const ()>, Box<dyn Error>> {
        for (_name, lib) in &self.libraries {
            let cstr = CString::new(symbol)?;
            unsafe {
                let result: Result<libloading::Symbol<unsafe fn()>, libloading::Error> =
                    lib.get(cstr.as_bytes());
                if let Ok(f) = result {
                    return Ok(Some(f.into_raw().into_raw() as *const ()));
                }
            }
        }
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn linker_global_long() {
        let mut b = Link::new();
        b.add_obj_file("test", Path::new("/home/rrx/code/protolang/tmp/live.o"))
            .unwrap();
        let collection = b.link().unwrap();

        let ret: i64 = collection.invoke("call_live", (2,)).unwrap();
        println!("ret: {:#08x}", ret);
        assert_eq!(13, ret);

        let ret: i64 = collection.invoke("simple", ()).unwrap();
        println!("ret: {:#08x}", ret);
        assert_eq!(1, ret);

        let ret: i64 = collection.invoke("func2", (2,)).unwrap();
        println!("ret: {:#08x}", ret);
        assert_eq!(3, ret);
    }

    #[test]
    fn linker_shared() {
        let mut b = Link::new();
        b.add_library("gz", Path::new("/home/rrx/code/protolang/tmp/libz.so"))
            .unwrap();
        b.add_obj_file(
            "test",
            Path::new("/home/rrx/code/protolang/tmp/link_shared.o"),
        )
        .unwrap();
        let collection = b.link().unwrap();
        let ret: std::ffi::c_void = collection.invoke("call_z", ()).unwrap();
        //println!("ret: {:#08x}", ret);
        //assert_eq!(3, ret);
    }

    #[test]
    fn linker_livelink() {
        let mut b = Link::new();

        // unable to link, missing symbol
        b.add_obj_file(
            "test1",
            Path::new("/home/rrx/code/protolang/tmp/testfunction.o"),
        )
        .unwrap();
        assert_eq!(false, b.link().is_ok());

        // provide missing symbol
        b.add_obj_file("asdf", Path::new("/home/rrx/code/protolang/tmp/asdf.o"))
            .unwrap();
        assert_eq!(true, b.link().is_ok());

        // links fine
        b.add_obj_file(
            "simple",
            Path::new("/home/rrx/code/protolang/tmp/simplefunction.o"),
        )
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
