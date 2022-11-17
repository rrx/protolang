use std::sync::Arc;
use std::error::Error;
use std::path::Path;
use std::collections::{HashMap, HashSet};
use std::ffi::CString;

use std::fs;

use super::*;

pub struct LinkBuilder {
    collection: LinkCollection,
    pages: HashMap<String, UnlinkedCode>,
    libraries: HashMap<String, libloading::Library>
}

impl LinkBuilder {
    pub fn new() -> Self {
        Self {
            collection: LinkCollection::new(), pages: HashMap::new(),
            libraries: HashMap::new()
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

    pub fn add(&mut self, name: &str, path: &Path) -> Result<(), Box<dyn Error>> {
        let buf = fs::read(path)?;
        let unlinked = Arc::new(UnlinkedCodeInner::create(name, buf.as_slice())?);
        self.pages.insert(name.to_string(), unlinked);
        Ok(())
    }

    pub fn add_buf(&mut self, name: &str, buf: &[u8]) -> Result<(), Box<dyn Error>> {
        let unlinked = Arc::new(UnlinkedCodeInner::create(name, buf)?);
        self.pages.insert(name.to_string(), unlinked);
        Ok(())
    }

    pub fn link(&mut self) -> Result<LinkCollection, Box<dyn Error>> {
        let mut symbols = HashMap::new();
        let mut duplicates = HashSet::new();

        // get all of the symbols and the name that provides it
        for (name, unlinked) in &self.pages {
            println!("linking: {}", name);
            for symbol in &unlinked.symbols {
                println!("\tSymbol: {}", &symbol);
                if symbols.contains_key(symbol) {
                    println!("\tDuplicate symbol: {}", &symbol);
                    duplicates.insert(symbol);
                } else {
                    symbols.insert(symbol.clone(), name);
                }
            }
        }

        let mut relocations = HashSet::new();
        let mut missing = HashSet::new();
        for (name, unlinked) in &self.pages {
            let mut children = HashSet::new();
            println!("checking: {}", name);
            // ensure all relocations map somewhere
            for symbol in &unlinked.relocations {
                println!("\tReloc: {}", &symbol);
                if symbols.contains_key(symbol) || self.search_dynamic(symbol)?.is_some() {
                    children.insert(symbol.clone());
                    relocations.insert(symbol.clone());
                } else {
                    println!("\tSymbol {} missing", symbol);
                    missing.insert(symbol);
                }
            }
        }

        if missing.len() == 0 && duplicates.len() == 0 {
            let mut unpatched = vec![];
            for (name, unlinked) in &self.pages {
                let name = format!("{}_data", &unlinked.name);
                unpatched.push(unlinked.create_data(&name)?);
                let name = format!("{}_code", &unlinked.name);
                unpatched.push(unlinked.create_unpatched(&name)?);
            }
            LinkCollection::build(unpatched)
        } else {
            Err(LinkError::MissingSymbol.into())
        }

    }


    // search the dynamic libraries to see if the symbol exists
    fn search_dynamic(&self, symbol: &str) -> Result<Option<*const()>, Box<dyn Error>> {
        for (_name, lib) in &self.libraries {
            let cstr = CString::new(symbol)?;
            unsafe {
                let result: Result<libloading::Symbol<unsafe fn ()>, libloading::Error> = lib.get(cstr.as_bytes());
                if let Ok(f) = result {
                    return Ok(Some(f.into_raw().into_raw() as *const()));
                }
            }
        }
        Ok(None)
    }


    /*
    pub fn link2(&mut self) -> Result<(), Box<dyn Error>> {
        let visited = im::HashSet::new();
        for (name, page) in &self.pages {
            let unpatched = page.create_unpatched("")?;
            self.collection.add_unpatched(unpatched, visited.clone())?;
        }
        Ok(())
    }
    */

}


