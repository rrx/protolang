use super::*;
use memmap::MmapMut;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::sync::Arc;

pub fn page_align(n: usize) -> usize {
    // hardwired for now, but we can get this from the target we are running at at runtime
    let p = 4096;
    return (n + (p - 1)) & !(p - 1);
}

#[derive(Clone)]
pub struct CodePointer {
    // we need to hold a reference here so we don't deallocate the code page
    #[allow(dead_code)]
    code: CodePage,
    ptr: *const (),
}

pub type UnpatchedSet = HashMap<String, UnpatchedCodePage>;

#[derive(Clone)]
pub struct LinkCollection {
    code_pages: im::HashMap<String, CodePage>, // map names to CodePages
    symbols: im::HashMap<String, CodePointer>, // map Symbols to Pointers
}
impl LinkCollection {
    pub fn new() -> Self {
        Self {
            code_pages: im::HashMap::new(),
            symbols: im::HashMap::new(),
        }
    }

    pub fn build(unpatches: Vec<UnpatchedCodePage>) -> Result<Self, Box<dyn Error>> {
        // generate a list of symbols and their pointers
        let mut pointers = im::HashMap::new();
        for unpatch in &unpatches {
            for (symbol, ptr) in &unpatch.symbols {
                pointers.insert(symbol.clone(), *ptr);
            }
        }

        let mut code_pages = im::HashMap::new();
        let mut symbols = im::HashMap::new();
        for unpatch in unpatches.into_iter() {
            unpatch.disassemble();
            let code = patch(unpatch, pointers.clone())?;
            for (symbol, ptr) in &code.symbols {
                symbols.insert(
                    symbol.clone(),
                    CodePointer {
                        code: code.clone(),
                        ptr: *ptr,
                    },
                );
            }
            code.disassemble();
            code_pages.insert(code.name.clone(), code);
        }
        Ok(Self {
            code_pages,
            symbols,
        })
    }

    pub fn add_unpatched(
        &mut self,
        unpatched: UnpatchedCodePage,
        mut visited: im::HashSet<String>,
    ) -> Result<Self, Box<dyn Error>> {
        // prevent cycles
        if visited.contains(&unpatched.name) {
            return Ok(self.clone());
        }

        let maybe_old_code = self.code_pages.get(&unpatched.name);
        let old_symbols: HashSet<String> = match maybe_old_code {
            Some(code) => HashSet::from_iter(code.symbols.keys().cloned()),
            None => HashSet::new(),
        };
        let old_calling_codes = match maybe_old_code {
            Some(code) => code.children(),
            None => vec![],
        };
        let new_symbols: HashSet<String> = HashSet::from_iter(unpatched.symbols.keys().cloned());
        // get a list of symbols in the old page, that are not in the new page, so we can remove
        // them
        let to_remove: Vec<String> = old_symbols.difference(&new_symbols).cloned().collect();
        for symbol in to_remove {
            self.symbols.remove(&symbol);
        }

        visited.insert(unpatched.name.clone());

        /*
        let code = self.patch(unpatched)?;

        for (symbol, ptr) in &code.symbols {
            self.symbols.insert(symbol.clone(), CodePointer {
                code: code.clone(), ptr: *ptr
            });
        }



        // recursively visit the children and recreate anything that points to this page
        for child in code.children() {
            // create an unpatched clone
            let unpatched = child.clone_unpatched()?;
            self.add_unpatched(unpatched, visited.clone())?;
        }
        */

        Ok(self.clone())
    }

    pub fn invoke<P, T>(&self, name: &str, args: P) -> Result<T, Box<dyn Error>> {
        // call the main function

        for (k, v) in &self.code_pages {
            println!("{:?}", (&k, &v));
        }

        unsafe {
            let ptr = self.symbols.get(name).ok_or(LinkError::SymbolNotFound)?.ptr as *const ();
            type MyFunc<P, T> = unsafe extern "cdecl" fn(P) -> T;
            let v: MyFunc<P, T> = std::mem::transmute(ptr);
            println!("invoking {} @ {:#08x}", name, ptr as usize);
            let ret = v(args);
            Ok(ret)
        }
    }
}

pub type CodePage = Arc<CodePageInner>;

#[derive(Debug)]
pub struct CodePageInner {
    pub(crate) kind: CodePageKind,
    pub(crate) m: memmap::Mmap,
    pub(crate) symbols: im::HashMap<String, *const ()>,
    pub(crate) relocations: im::HashMap<isize, Reloc>,
    pub(crate) code_size: usize,
    pub(crate) got_size: usize,
    pub(crate) name: String,
}
impl CodePageInner {
    /// return a list of nodes that call into this page
    pub fn children(&self) -> Vec<CodePage> {
        vec![]
    }

    pub fn clone_unpatched(&self) -> Result<UnpatchedCodePage, Box<dyn Error>> {
        let mut m = MmapMut::map_anon(self.m.len())?;
        Ok(UnpatchedCodePage {
            kind: self.kind,
            name: self.name.clone(),
            m,
            code_size: self.code_size,
            got_size: self.got_size,
            symbols: self.symbols.clone(),
            relocations: self.relocations.clone(),
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CodePageKind {
    Code,
    Data,
}

#[derive(Debug)]
pub struct UnpatchedCodePage {
    pub(crate) kind: CodePageKind,
    pub(crate) name: String,
    pub(crate) m: memmap::MmapMut,
    pub(crate) code_size: usize,
    pub(crate) got_size: usize,
    pub(crate) symbols: im::HashMap<String, *const ()>,
    pub(crate) relocations: im::HashMap<isize, Reloc>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn global_long() {
        let mut b = LinkBuilder::new();
        //b.add_library("gz", Path::new("/home/rrx/code/protolang/tmp/libz.so")).unwrap();
        b.add("test", Path::new("/home/rrx/code/protolang/tmp/live.o"))
            .unwrap();
        let collection = b.link().unwrap();
        let ret: i64 = collection.invoke("func2", (2,)).unwrap();
        println!("ret: {:#08x}", ret);
        assert_eq!(3, ret);

        let ret: i64 = collection.invoke("call_live", (2,)).unwrap();
        println!("ret: {:#08x}", ret);
        assert_eq!(10, ret);
    }

    #[test]
    fn link_shared() {
        let mut b = LinkBuilder::new();
        b.add_library("gz", Path::new("/home/rrx/code/protolang/tmp/libz.so"))
            .unwrap();
        b.add(
            "test",
            Path::new("/home/rrx/code/protolang/tmp/link_shared.o"),
        )
        .unwrap();
        let collection = b.link().unwrap();
        //let _: std::ffi::c_void = collection.invoke("call_z", ()).unwrap();
        //println!("ret: {:#08x}", ret);
        //assert_eq!(3, ret);
    }

    #[test]
    fn livelink() {
        let mut b = LinkBuilder::new();

        // unable to link, missing symbol
        b.add(
            "test1",
            Path::new("/home/rrx/code/protolang/tmp/testfunction.o"),
        )
        .unwrap();
        assert_eq!(false, b.link().is_ok());

        // provide missing symbol
        b.add("asdf", Path::new("/home/rrx/code/protolang/tmp/asdf.o"))
            .unwrap();
        assert_eq!(true, b.link().is_ok());

        // links fine
        b.add(
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
