use std::sync::Arc;
use std::error::Error;
use object::{
    Object, ObjectSection, ObjectSymbol, ObjectSymbolTable, RelocationKind, RelocationTarget,
    Relocation, RelocationEncoding, SymbolScope,
    Symbol,
    SymbolSection,
    SectionKind,
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
    MissingSymbol,
    SymbolNotFound
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
            let func: libloading::Symbol<unsafe extern fn() -> u32> = lib.get(b"gzopen")?;
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
            for symbol in &unlinked.relocations {
                println!("\tReloc: {}", &symbol);
                if symbols.contains_key(symbol) {
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

    pub fn link2(&mut self) -> Result<(), Box<dyn Error>> {
        let visited = im::HashSet::new();
        for (name, page) in &self.pages {
            let unpatched = page.create_unpatched("")?;
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
            RelocationKind::Elf(42) => {
                // got entry + addend - reloc_offset(patch)
                // we are computing the offset from the current instruction pointer
                unsafe {
                    let patch_base = code.m.as_mut_ptr() as *mut u8;
                    let patch = patch_base.offset(*reloc_offset);

                    // get the entry in the lookup table
                    let addr = *pointers.get(&rel.symbol_name).unwrap();

                    // this works
                    let value = addr as isize + rel.r.addend as isize - patch as isize;

                    // this does not work
                    //let value = patch as isize + rel.r.addend as isize - addr as isize;

                    let before = std::ptr::read(patch);
                    (patch as *mut u32).replace(value as u32);
                    println!("patch_base: {:#08x}", patch_base as usize);
                    println!("patch: {:#08x}", patch as usize);
                    println!("value: {:#04x}", value as u32);

                    println!(
                        "rel got {}: patch {:#08x}:{:#08x}=>{:#08x} addend:{:#08x} addr:{:#08x}",
                        &rel.symbol_name,
                        patch as usize,
                        before,
                        value as u32,
                        rel.r.addend,
                        addr as usize,
                        );
                }

            }

            RelocationKind::Absolute => {
                // S + A
                // S = Address of the symbol
                // A = value of the Addend
                //
                // We get this if we don't compile with -fPIC
                // This doesn't work, and produces an illegal address for some reason
                let name = &rel.symbol_name;
                println!("look up: {}", name);
                unsafe {
                    let patch_base = code.m.as_mut_ptr() as *mut u8;


                    // address of remote
                    let addr = *pointers.get(name).unwrap();
                    let adjusted = addr as isize + rel.r.addend as isize;
                    
                    let (before, patch) = match rel.r.size {
                        32 => {
                            // patch as 32 bit
                            let patch = patch_base.offset(*reloc_offset as isize) as *mut u32;
                            let before = std::ptr::read(patch);
                            patch.replace(adjusted as u32);
                            (before as u64, patch as u64)
                        }
                        64 => {
                            // patch as 64 bit
                            let patch = patch_base.offset(*reloc_offset as isize) as *mut u64;
                            let before = std::ptr::read(patch);
                            patch.replace(adjusted as u64);
                            (before as u64, patch as u64)
                        }
                        _ => unimplemented!()
                    };

                    println!(
                        "rel absolute {}: patch {:#08x}:{:#08x}=>{:#08x} addend:{:#08x} addr:{:#08x}",
                        name,
                        patch,
                        before,
                        adjusted as u32,
                        rel.r.addend,
                        addr as u32,
                        );
                }
            }
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
        kind: code.kind,
        m: code.m.make_exec()?,
        code_size: code.code_size,
        got_size: code.got_size,
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
        Self {
            code_pages: im::HashMap::new(),
            symbols: im::HashMap::new() }
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

        for (k, v) in &self.code_pages {
            println!("{:?}", (&k, &v));
        }

        unsafe {
            let ptr = self.symbols.get(name).ok_or(LinkError::SymbolNotFound)?.ptr as *const();
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
    kind: CodePageKind,
    m: memmap::Mmap,
    symbols: im::HashMap<String, *const ()>,
    relocations: im::HashMap<isize, Reloc>,
    code_size: usize,
    got_size: usize,
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
            kind: self.kind,
            name: self.name.clone(),
            m,
            code_size: self.code_size,
            got_size: self.got_size,
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
        println!("start: {:#08x}", &base);
        disassemble(self.kind, buf, pointers);
        match self.kind {
            CodePageKind::Data => {
            }
            _ => ()
        }

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

#[derive(Debug, Clone, Copy)]
pub enum CodePageKind {
    Code,
    Data
}

#[derive(Debug)]
pub struct UnpatchedCodePage {
    kind: CodePageKind,
    name: String,
    m: memmap::MmapMut,
    code_size: usize,
    got_size: usize,
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
        disassemble(self.kind, buf, pointers);
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
            for section in obj_file.sections() {
                let section_name = section.name()?.to_string();
                println!("Found section[{:?}, {}]: {:?}", section.index(), section_name, section);

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

            for s in symbol_table.symbols() {
                // only track dynamic symbols for now
                let name = s.name()?.to_string();
                //println!("symbol: {:?}", &s);
                let maybe_section = match s.section() {
                    SymbolSection::Section(section_index) => {
                        Some(obj_file.section_by_index(s.section_index().unwrap())?)
                    }
                    _ => None
                };

                let section_name = maybe_section.map(|section| section.name().map_or("".to_string(), |n| n.to_string()).to_string());
                println!("Found symbol[{:?}, {:20}]: {:#04x} {:?}, {:?}", s.index().0, name, s.address(), s, section_name);
                if s.scope() == SymbolScope::Dynamic {
                    symbols.insert(name);
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

    pub fn create_data(&self, code_page_name: &str) -> Result<UnpatchedCodePage, Box<dyn Error>> {
        let obj_file = object::File::parse(self.bytes.as_slice())?;
        let mut size = 0;
        let mut symbols = im::HashMap::new();
        let mut section_data = vec![];
        let mut got_size = 0;

        if let Some(symbol_table) = obj_file.symbol_table() {
            let mut section_ids = HashSet::new();

            for section in obj_file.sections() {
                let data = section.uncompressed_data()?;
                let section_name = section.name()?.to_string();
                match section.kind() {
                    SectionKind::UninitializedData => {
                        println!("xx sec: {:?}", (&section, &data));
                        section_data.push((section_name, section.index(), section.size() as usize, None));
                        section_ids.insert(section.index());
                    }
                    SectionKind::Data => {
                        println!("xx sec data: {:?}", (&section, &data));
                        section_data.push((section_name, section.index(), data.len(), Some(data)));
                        section_ids.insert(section.index());
                    }
                    _ => ()
                }
            }

            for s in symbol_table.symbols() {
                // only track dynamic symbols for now
                if let Some(section_index) = s.section_index() {
                    if s.scope() == SymbolScope::Dynamic && section_ids.contains(&section_index) {
                        let name = s.name()?.to_string();
                        println!("add: {:?}", (&name, s.address()));
                        symbols.insert(name, (s.section_index().unwrap(), s.address()));

                        // space for 64bit pointer
                        got_size += 8;
                    }
                }
            }
        }

        let size: usize = section_data.iter().fold(0, |acc, (_, _, size, _)| acc+size);

        let page_aligned_size = page_align(size + 1); // round up to page alignment

        // allocate page aligned memory and copy the functions over
        println!("allocating {} bytes on {}", page_aligned_size, &code_page_name);
        let mut mmap = MmapMut::map_anon(page_aligned_size)?;

        // only copy the first part, the remainder is uninitialized
        let buf = mmap.as_mut();

        // copy section data over
        let mut section_base = HashMap::new();
        let mut start_index = 0;
        for (name, section_index, size, data) in section_data {
            section_base.insert(section_index, start_index); 
            let start_end = start_index + size;
            println!("copy: {:?}", (name, start_index, start_end, &data));
            if let Some(data) = data {
                buf[start_index..start_end].copy_from_slice(&data);
            }
            start_index = start_end;
        }
        println!("buf: {:?}", (&buf[0..size]));

        let mut symbols_with_offsets = im::HashMap::new();

        for (i, (name, (section_index, offset))) in symbols.iter().enumerate() {
            let base = *section_base.get(&section_index).unwrap();
            unsafe {
                let page_base = mmap.as_ptr() as *const u8; 
                let value_ptr = page_base.offset(base as isize + *offset as isize); 
                // got pointer follows the data section
                let got_ptr = page_base.offset((size+i*8) as isize) as *mut u64; 

                // set the got_ptr to be the value ptr
                *got_ptr = value_ptr as u64;
                //(got_ptr as *mut u32).replace(value_ptr as u32);

                println!("symbol: {:?}", (&name, offset, value_ptr, got_ptr));
                symbols_with_offsets.insert(name.clone(), got_ptr as *const ());
            }
        }

        Ok(UnpatchedCodePage {
            kind: CodePageKind::Data,
            name: code_page_name.to_string(),
            symbols: symbols_with_offsets,
            relocations: im::HashMap::new(),
            m: mmap,
            code_size: size,
            got_size
        })

    }

    pub fn create_unpatched(&self, code_page_name: &str) -> Result<UnpatchedCodePage, Box<dyn Error>> {
        let obj_file = object::File::parse(self.bytes.as_slice())?;

        println!("create unpatched: {}", self.name);
        // read all sections
        let mut section_data = vec![];
        let mut size = 0;
        let mut symbols = im::HashMap::new();
        let mut relocations = im::HashMap::new();

        let mut got_size = 0;

        if let Some(symbol_table) = obj_file.symbol_table() {
            let mut section_ids = HashSet::new();
            for section in obj_file.sections() {
                let data = section.uncompressed_data()?;
                let section_name = section.name()?.to_string();
                if section.kind() == SectionKind::Text {
                    size += data.len();
                    section_data.push((section_name, data));
                    section_ids.insert(section.index());
                }

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
            }

            for s in symbol_table.symbols() {
                // only track dynamic symbols for now
                if let Some(section_index) = s.section_index() {
                    if s.scope() == SymbolScope::Dynamic && section_ids.contains(&section_index) {
                        let name = s.name()?.to_string();
                        symbols.insert(name, s.address());
                    }
                }
            }
        }

        let page_aligned_size = page_align(size); // round up to page alignment

        // allocate page aligned memory and copy the functions over
        println!("allocating: {}", page_aligned_size);
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
            kind: CodePageKind::Code,
            name: code_page_name.to_string(),
            symbols: symbols_with_offsets,
            relocations,
            m: mmap,
            code_size: size,
            got_size
        })

    }

}


pub fn disassemble(kind: CodePageKind, buf: &[u8], pointers: im::HashMap<usize, String>) {
    match kind {
        CodePageKind::Data => disassemble_data(buf, pointers),
        CodePageKind::Code => disassemble_code(buf, pointers),
    }
}

pub fn disassemble_data(buf: &[u8], pointers: im::HashMap<usize, String>) {
    println!("pointers: {:?}", pointers);
    for (ptr, name) in pointers {
        println!("buf: {:?}", (ptr, name));
    }
    println!("buf: {:?}", buf);
}

pub fn disassemble_code(buf: &[u8], pointers: im::HashMap<usize, String>) {

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
    fn global_long() {
        let mut b = LinkBuilder::new();
        b.add("test", Path::new("/home/rrx/code/protolang/tmp/live.o")).unwrap();
        let collection = b.link().unwrap();
        let ret: i64 = collection.invoke("func2", (2,)).unwrap();
        println!("ret: {:#08x}", ret);
        assert_eq!(3, ret);

        let ret: i64 = collection.invoke("call_live", (2,)).unwrap();
        println!("ret: {:#08x}", ret);
        assert_eq!(10, ret);
    }

    #[test]
    fn livelink() {
        let mut b = LinkBuilder::new();

        // unable to link, missing symbol
        b.add("test1", Path::new("/home/rrx/code/protolang/tmp/testfunction.o")).unwrap();
        assert_eq!(false, b.link().is_ok());
        
        // provide missing symbol
        b.add("asdf", Path::new("/home/rrx/code/protolang/tmp/asdf.o")).unwrap();
        assert_eq!(true, b.link().is_ok());

        // links fine
        b.add("simple", Path::new("/home/rrx/code/protolang/tmp/simplefunction.o")).unwrap();
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

