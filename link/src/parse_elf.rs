use std::sync::Arc;
use std::error::Error;
use object::{
    Object, ObjectSection, ObjectSymbol, ObjectSymbolTable, RelocationKind, RelocationTarget,
    Relocation, RelocationEncoding, SymbolScope,
    Symbol,
    SymbolKind,
    SymbolSection,
    SectionKind,
};
use std::collections::{HashMap, HashSet};

use memmap::{Mmap, MmapMut};

use super::*;

pub struct CodeSymbol {
    name: String
}

pub type UnlinkedCode = Arc<UnlinkedCodeInner>;
pub struct UnlinkedCodeInner {
    pub(crate) name: String,
    pub(crate) bytes: Vec<u8>,
    pub(crate) symbols: im::HashSet<String>,// CodeSymbol>,
    pub(crate) relocations: im::HashSet<String>
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
                if let Some(section_index) = s.section_index() {
                    // only track dynamic symbols for now
                    if  s.scope() == SymbolScope::Dynamic && section_ids.contains(&section_index) {
                        let name = s.name()?.to_string();
                        println!("add: {:?}", (&name, s.address()));
                        symbols.insert(name, (s.section_index().unwrap(), s.address()));

                        // space for 64bit pointer
                        got_size += 8;
                    }
                } else if s.kind() == SymbolKind::Unknown {
                    let name = s.name()?.to_string();
                    println!("add unknown: {:?}", (&name, s.address()));
                    //symbols.insert(name, (s.section_index().unwrap(), s.address()));
                    // space for 64bit pointer
                    //got_size += 8;
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


