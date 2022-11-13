use std::sync::Arc;
use std::error::Error;
use object::{
    Object, ObjectSection, ObjectSymbol, ObjectSymbolTable, RelocationKind, RelocationTarget,
    Relocation, RelocationEncoding, SymbolScope,
    Symbol
};
use capstone::prelude::*;
use std::path::Path;
use std::collections::{HashMap, HashSet};
use std::borrow::Borrow;

use memmap::{Mmap, MmapMut};
use std::fs;
use std::fmt;

#[derive(Debug)]
pub enum LinkError {
    NotFound,
    MissingSymbol
}
impl std::error::Error for LinkError {}
impl fmt::Display for LinkError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LinkError: {:?}", &self)
    }
}


fn page_align(n: usize) -> usize {
    // hardwired for now, but we can get this from the target we are running at at runtime
    let p = 4096;
    return (n + (p - 1)) & !(p - 1);
}

#[derive(Clone)]
pub struct CodePointer {
    // we need to hold a reference here so we don't deallocate the code page
    #[allow(dead_code)]
    code: CodePage,
    ptr: *const()
}

pub type UnpatchedSet = HashMap<String, UnpatchedCodePage>;

pub struct LinkBuilder {
    collection: LinkCollection,
    pages: HashMap<String, UnlinkedCode>,
}

impl LinkBuilder {
    pub fn new() -> Self {
        Self {
            collection: LinkCollection::new(), pages: HashMap::new()
        }
    }

    pub fn remove(&mut self, name: &str) {
        self.pages.remove(&name.to_string());
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
            println!("loading: {}", name);
            for symbol in &unlinked.symbols {
                println!("Symbol: {}", &symbol);
                if symbols.contains_key(symbol) {
                    println!("Duplicate symbol: {}", &symbol);
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
            for symbol in &unlinked.relocations {
                println!("Reloc: {}", &symbol);
                if symbols.contains_key(symbol) {
                    children.insert(symbol.clone());
                    relocations.insert(symbol.clone());
                } else {
                    println!("Symbol {} missing", symbol);
                    missing.insert(symbol);
                }
            }
        }

        if missing.len() == 0 && duplicates.len() == 0 {
            let mut unpatched = vec![];
            for (name, unlinked) in &self.pages {
                unpatched.push(unlinked.create_unpatched()?);
            }
            LinkCollection::build(unpatched)
        } else {
            Err(LinkError::MissingSymbol.into())
        }

    }

    pub fn link2(&mut self) -> Result<(), Box<dyn Error>> {
        let visited = im::HashSet::new();
        for (name, page) in &self.pages {
            let unpatched = page.create_unpatched()?;
            self.collection.add_unpatched(unpatched, visited.clone())?;
        }
        Ok(())
    }

}

// given a map of pointers, patch the unpatched code page, and return a patched code page
pub fn patch(mut code: UnpatchedCodePage, pointers: im::HashMap<String, *const ()>) -> Result<CodePage, Box<dyn Error>> {
    println!("patching {} at base {:#08x}", &code.name, code.m.as_ptr() as usize);
    for (reloc_offset, rel) in &code.relocations {
        println!("r@{:#04x}: {:?}", &reloc_offset, &rel);
        match rel.r.kind {
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

                    let symbol_address =
                        symbol_addr as isize + addend as isize - patch as isize;

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
        m: code.m.make_exec()?,
        code_size: code.code_size,
        symbols: code.symbols.clone(),
        relocations: code.relocations.clone(),
        name: code.name.clone()
    }))
}


#[derive(Clone)]
pub struct LinkCollection {
    code_pages: im::HashMap<String, CodePage>, // map names to CodePages
    symbols: im::HashMap<String, CodePointer>, // map Symbols to Pointers
}
impl LinkCollection {
    pub fn new() -> Self {
        Self { code_pages: im::HashMap::new(), symbols: im::HashMap::new() }
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
                symbols.insert(symbol.clone(), CodePointer { code: code.clone(), ptr: *ptr });
            }
            code.disassemble();
            code_pages.insert(code.name.clone(), code);
        }
        Ok(Self { code_pages, symbols })
    }


    pub fn add_unpatched(&mut self, unpatched: UnpatchedCodePage, mut visited: im::HashSet<String>) -> Result<Self, Box<dyn Error>> {
        // prevent cycles
        if visited.contains(&unpatched.name) {
            return Ok(self.clone());
        }

        let maybe_old_code = self.code_pages.get(&unpatched.name);
        let old_symbols: HashSet<String> = match maybe_old_code {
            Some(code) => HashSet::from_iter(code.symbols.keys().cloned()),
            None => HashSet::new()
        };
        let old_calling_codes = match maybe_old_code {
            Some(code) => code.children(),
            None => vec![]
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
        unsafe {
            let ptr = self.symbols.get(name).unwrap().ptr as *const();
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
    m: memmap::Mmap,
    symbols: im::HashMap<String, *const ()>,
    relocations: im::HashMap<isize, Reloc>,
    //address: HashMap<*const , String>,
    code_size: usize,
    name: String
}
impl CodePageInner {
    /// return a list of nodes that call into this page
    pub fn children(&self) -> Vec<CodePage> {
        vec![]
    }

    pub fn clone_unpatched(&self) -> Result<UnpatchedCodePage, Box<dyn Error>> {
        let mut m = MmapMut::map_anon(self.m.len())?;
        Ok(UnpatchedCodePage {
            name: self.name.clone(),
            m,
            code_size: self.code_size,
            symbols: self.symbols.clone(),
            relocations: self.relocations.clone()
        })
    }

    pub fn disassemble(&self) {
        println!("code: {}, {:?}", &self.name, &self);
        let buf = &self.m.split_at(self.code_size).0;
        //let buf = &self.m.as_ref()[0..self.code_size];
        let mut pointers = im::HashMap::new();
        let base = self.m.as_ptr() as usize;
        for (name, ptr) in &self.symbols {
            pointers.insert(*ptr as usize - base, name.clone());
        }
        disassemble(buf, pointers);
    }
}

#[derive(Debug, Clone)]
pub struct LinkRelocation {
    kind: RelocationKind,
    encoding: RelocationEncoding,
    size: u8,
    target: RelocationTarget,
    addend: i64,
    implicit_addend: bool,
}

impl From<Relocation> for LinkRelocation {
    fn from(item: Relocation) -> Self {
        Self { kind: item.kind(), encoding: item.encoding(), size: item.size(), target: item.target(), addend: item.addend(), implicit_addend: item.has_implicit_addend() }
    }
}


#[derive(Debug, Clone)]
pub struct Reloc {
    symbol_name: String,
    r: LinkRelocation
}

#[derive(Debug)]
pub struct UnpatchedCodePage {
    name: String,
    m: memmap::MmapMut,
    code_size: usize,
    symbols: im::HashMap<String, *const ()>,
    relocations: im::HashMap<isize, Reloc>
}
impl UnpatchedCodePage {
    pub fn disassemble(&self) {
        println!("code: {}, {:?}", &self.name, &self);
        let buf = &self.m.as_ref()[0..self.code_size];
        let mut pointers = im::HashMap::new();
        let base = self.m.as_ptr() as usize;
        for (name, ptr) in &self.symbols {
            pointers.insert(*ptr as usize - base, name.clone());
        }
        disassemble(buf, pointers);
    }
}

pub type UnlinkedCode = Arc<UnlinkedCodeInner>;
pub struct UnlinkedCodeInner {
    name: String,
    bytes: Vec<u8>,
    symbols: im::HashSet<String>,
    relocations: im::HashSet<String>
}

impl UnlinkedCodeInner {
    pub fn create(name: &str, buf: &[u8]) -> Result<Self, Box<dyn Error>> {
        let obj_file = object::File::parse(buf)?;
        let mut symbols = im::HashSet::new();
        let mut relocations = im::HashSet::new();

        if let Some(symbol_table) = obj_file.symbol_table() {
            for s in symbol_table.symbols() {
                // only track dynamic symbols for now
                let name = s.name()?.to_string();
                println!("Found symbol[{:?}, {}]: {:#04x} {:?}", s.index(), name, s.address(), s);
                if s.scope() == SymbolScope::Dynamic {
                    //unsafe {
                    //let offset = self.m.as_ptr().offset(s.address() as isize) as *const ();
                    symbols.insert(name);
                    //}
                }
            }

            for section in obj_file.sections() {
                for (reloc_offset, r) in section.relocations() {
                    let symbol = if let RelocationTarget::Symbol(symbol_index) = r.target() {
                        symbol_table.symbol_by_index(symbol_index)?
                    } else {
                        unimplemented!()
                    };
                    let name = symbol.name()?.to_string();
                    println!("Found relocation[{}]: {:#04x} {:?}", name, reloc_offset, r);

                    match symbol.scope() {
                        SymbolScope::Dynamic | SymbolScope::Linkage | SymbolScope::Unknown => {
                            relocations.insert(name);
                        }
                        SymbolScope::Compilation => (),
                    }
                }
            }

        }
        Ok(Self {
            name: name.to_string(),
            bytes: buf.to_vec(),
            symbols,
            relocations
        })
    }

    pub fn create_unpatched(&self) -> Result<UnpatchedCodePage, Box<dyn Error>> {
        let obj_file = object::File::parse(self.bytes.as_slice())?;

        println!("create unpatched: {}", self.name);
        // read all sections
        let mut section_data = vec![];
        let mut size = 0;
        let mut symbols = im::HashMap::new();
        let mut relocations = im::HashMap::new();

        if let Some(symbol_table) = obj_file.symbol_table() {
            for section in obj_file.sections() {
                let section_name = section.name()?.to_string();
                let data = section.uncompressed_data()?;

                for (reloc_offset, r) in section.relocations() {
                    let symbol = if let RelocationTarget::Symbol(symbol_index) = r.target() {
                        obj_file
                            .symbol_table()
                            .unwrap()
                            .symbol_by_index(symbol_index)?
                    } else {
                        unimplemented!()
                    };
                    match symbol.scope() {
                        SymbolScope::Dynamic | SymbolScope::Linkage | SymbolScope::Unknown => {
                            relocations.insert(reloc_offset as isize, Reloc {
                                symbol_name: symbol.name()?.to_string(),
                                r: r.into()
                            });
                        }
                        SymbolScope::Compilation => (),
                    }

                }

                if section_name == ".text" {
                    size += data.len();
                    section_data.push((section_name, data));
                }

                for s in symbol_table.symbols() {
                    // only track dynamic symbols for now
                    if s.scope() == SymbolScope::Dynamic {
                        let name = s.name()?.to_string();
                        //unsafe {
                        //let offset = self.m.as_ptr().offset(s.address() as isize) as *const ();
                        symbols.insert(name, s.address());
                        //}
                    }
                }
            }

        }



        let page_aligned_size = page_align(size); // round up to page alignment

        // allocate page aligned memory and copy the functions over
        let mut mmap = MmapMut::map_anon(page_aligned_size)?;

        // only copy the first part, the remainder is uninitialized
        let mut start_index = 0;
        let buf = mmap.as_mut();

        // copy section data over
        for (name, data) in section_data {
            let start_end = start_index + data.len();
            println!("copy: {:?}", (name, start_index, start_end, &data));
            buf[start_index..start_end].copy_from_slice(&data);
            start_index = start_end;
        }

        let mut symbols_with_offsets = im::HashMap::new();

        for (name, offset) in symbols {
            unsafe {
                let ptr = mmap.as_ptr().offset(offset as isize) as *const (); 
                println!("symbol: {:?}", (&name, offset, ptr));
                symbols_with_offsets.insert(name, ptr);
            }
        }

        Ok(UnpatchedCodePage {
            name: self.name.clone(),
            symbols: symbols_with_offsets,
            relocations,
            m: mmap,
            code_size: size
        })

    }

}


pub fn disassemble(buf: &[u8], pointers: im::HashMap<usize, String>) {
    // disassemble the code we are generating
    let cs = capstone::Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode64)
        .build()
        .unwrap();
    let insts = cs
        .disasm_all(&buf, 0)
        .expect("disassemble");
    let mut last_name = None;
    for instr in insts.as_ref() {
        let addr = instr.address() as usize;
        if let Some(v) = pointers.get(&addr) {
            let display_symbol = if let Some(name) = last_name {
                if name != v {
                    last_name = Some(v);
                    Some(v)
                } else {
                    None
                }
            } else {
                Some(v)
            };

            if let Some(_) = display_symbol {
                println!("fn {}: {:#06x}", v, &addr);
            }
            last_name = Some(v);
        }

        println!(
            "  {:#06x} {}\t\t{}",
            &addr,
            instr.mnemonic().expect("no mnmemonic found"),
            instr.op_str().expect("no op_str found")
            );
    }
}



#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn livelink() {
        let mut b = LinkBuilder::new();
        b.add("test1", Path::new("/home/rrx/code/protolang/tmp/testfunction.o"));
        assert_eq!(false, b.link().is_ok());
        b.add("test2", Path::new("/home/rrx/code/protolang/tmp/simplefunction.o"));
        assert_eq!(false, b.link().is_ok());
        b.remove("test1");
        let collection = b.link().unwrap();
        //assert_eq!(true, b.can_link());
        //assert_eq!(true, b.can_link());
        //b.link().unwrap();
        let ret: i64 = collection.invoke("func", ()).unwrap();
        println!("ret: {}", ret);
        assert_eq!(10001, ret);

    }
}

