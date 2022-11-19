use object::{
    Object, ObjectSection, ObjectSymbol, ObjectSymbolTable, RelocationTarget, SectionKind,
    SymbolKind, SymbolScope, SymbolSection,
};
use std::error::Error;
use std::sync::Arc;

use std::collections::{HashMap};

use super::*;

pub type UnlinkedCodeSegment = Arc<UnlinkedCodeSegmentInner>;

pub struct UnlinkedCodeSegmentInner {
    pub(crate) name: String,
    pub(crate) bytes: Vec<u8>,
    pub(crate) symbols: im::HashMap<String, CodeSymbol>,
    pub(crate) unknowns: im::HashSet<String>,
    pub(crate) relocations: im::HashMap<String, CodeRelocation>,
}

impl UnlinkedCodeSegmentInner {
    pub fn create_segments(link_name: &str, buf: &[u8]) -> Result<Vec<Self>, Box<dyn Error>> {
        let obj_file = object::File::parse(buf)?;
        let mut symbols = HashMap::new();
        let mut segments = vec![]; //im::HashMap::new(); //<SegmentId, Segment>;
        let mut unknowns = im::HashSet::new();

        if let Some(symbol_table) = obj_file.symbol_table() {
            for s in symbol_table.symbols() {
                // only track dynamic symbols for now
                let name = s.name()?.to_string();
                //eprintln!("symbol: {:?}", &s);
                let maybe_section = match s.section() {
                    SymbolSection::Section(section_index) => {
                        Some(obj_file.section_by_index(section_index)?)
                    }
                    _ => None,
                };

                let section_name = maybe_section.as_ref().map(|section| {
                    section
                        .name()
                        .map_or("".to_string(), |n| n.to_string())
                        .to_string()
                });

                eprintln!(
                    "Symbol[{}, {:20}, address: {:#04x}, size: {}, kind: {:?}, scope: {:?}, weak: {}, section: {:?}]",
                    s.index().0,
                    &name,
                    s.size(),
                    s.address(),
                    s.kind(),
                    s.scope(),
                    s.is_weak(),
                    section_name,
                );

                let maybe_code_symbol = match &maybe_section {
                    Some(section) => {
                        match (s.scope(), section.kind()) {
                            (SymbolScope::Dynamic, SectionKind::UninitializedData) => {
                                Some(CodeSymbol {
                                    name,
                                    address: s.address(),
                                    size: s.size(),
                                    kind: CodeSymbolKind::Data,
                                    def: CodeSymbolDefinition::Defined,
                                })
                            }
                            (SymbolScope::Dynamic, SectionKind::Data) => Some(CodeSymbol {
                                name,
                                address: s.address(),
                                size: s.size(),
                                kind: CodeSymbolKind::Data,
                                def: CodeSymbolDefinition::Defined,
                            }),
                            (SymbolScope::Dynamic, SectionKind::Text) => Some(CodeSymbol {
                                name,
                                address: s.address(),
                                size: s.size(),
                                kind: CodeSymbolKind::Text,
                                def: CodeSymbolDefinition::Defined,
                            }),

                            // skip these
                            (SymbolScope::Compilation, _) => None,
                            _ => unimplemented!(),
                        }
                    }

                    None => match s.kind() {
                        SymbolKind::Unknown => {
                            // external references
                            unknowns.insert(name.clone());
                            Some(CodeSymbol {
                                name,
                                address: s.address(),
                                size: s.size(),
                                kind: CodeSymbolKind::Text,
                                def: CodeSymbolDefinition::Extern,
                            })
                        }

                        // skip these
                        SymbolKind::Null => None,
                        // we might want to capture the file info later
                        SymbolKind::File => None,
                        _ => unimplemented!(),
                    },
                };

                if let Some(code_symbol) = maybe_code_symbol {
                    symbols.insert(code_symbol.name.clone(), (maybe_section, code_symbol));
                }
            }

            for section in obj_file.sections() {
                let section_name = section.name()?.to_string();
                let section_index = section.index().0;
                eprintln!(
                    "Section[{:?}, {}, address: {}, size: {}, align: {}, kind: {:?}]",
                    section_index,
                    section_name,
                    section.address(),
                    section.size(),
                    section.align(),
                    section.kind()
                );
                let mut relocations = im::HashMap::new();
                let mut section_symbols = im::HashMap::new();

                for (symbol_name, (maybe_section, code_symbol)) in &symbols {
                    match maybe_section {
                        Some(symbol_section) => {
                            if symbol_section.index() == section.index() {
                                println!("Symbol[{}] = {:?}", &symbol_name, &code_symbol);
                                section_symbols.insert(symbol_name.clone(), code_symbol.clone());
                            }
                        }
                        None => (),
                    }
                }


                for (reloc_offset, r) in section.relocations() {
                    let symbol = if let RelocationTarget::Symbol(symbol_index) = r.target() {
                        symbol_table.symbol_by_index(symbol_index)?
                    } else {
                        unimplemented!()
                    };
                    let name = symbol.name()?.to_string();

                    match symbol.scope() {
                        SymbolScope::Dynamic | SymbolScope::Unknown => {
                            // | SymbolScope::Linkage | SymbolScope::Unknown => {
                            relocations.insert(
                                name.clone(),
                                CodeRelocation {
                                    name,
                                    offset: reloc_offset,
                                    r: r.into(),
                                },
                            );
                        }

                         //do nothing here
                        //SymbolScope::Unknown => {
                            //unknowns.insert(name);
                        //}
                        SymbolScope::Compilation => (),

                        _ => unimplemented!("{:?}", symbol),
                    }
                    for (_, r) in &relocations {
                        eprintln!("{}", r);
                    }
                }

                let name = format!("{}_{}", link_name, section_index);
                let data = section.uncompressed_data()?;

                // for bss, we have empty data, so we pass in a zero initialized buffer
                // to be consistent
                let bytes = if section.size() as usize > data.len() {
                    let mut data = Vec::new();
                    data.resize(section.size() as usize, 0);
                    data
                } else {
                    data.to_vec()
                };

                segments.push(UnlinkedCodeSegmentInner {
                    name,
                    bytes,
                    unknowns: unknowns.clone(),
                    symbols: section_symbols,
                    relocations,
                });
            }
        }

        Ok(segments)
    }

    pub fn create_data_mem(
        &self,
        code_page_name: &str,
        b: &mut BlockFactory,
    ) -> Result<Option<PatchBlock>, Box<dyn Error>> {
        // get a list of data symbols
        let symbols = self.symbols.iter().filter(|(_, s)| {
            s.kind == CodeSymbolKind::Data
        }).collect::<Vec<_>>();

        if symbols.len() > 0 {
            eprintln!("create data: {}, {:?}", &code_page_name, (&symbols, self.bytes.len()));

            // allocate enough space for the actual data, and a lookup table as well
            let size = self.bytes.len() + symbols.len() * std::mem::size_of::<usize>();
            if let Some(block) = b.alloc_data(size) {
                // to create the data section, we need to copy the data, but we also need
                // to create pointers to the data

                // copy the data over, and the symbols have the offsets
                // append the got entries after the data
                block.as_mut_slice()[0..self.bytes.len()].copy_from_slice(&self.bytes);
                let mut pointers = im::HashMap::new();
                unsafe {

                    let mut entry_counter = 0;
                    let got_base = block.as_ptr().offset(self.bytes.len() as isize) as *mut u64;
                    for (name, s) in &symbols {
                        let value_ptr = block.as_ptr().offset(s.address as isize) as *const ();
                        let got_ptr = got_base.offset(entry_counter); 
                        *got_ptr = value_ptr as u64;
                        entry_counter += 1;
                        pointers.insert(s.name.clone(), got_ptr as *const ());
                        println!("got: {}, {:#08x}, {:#08x}", &name, value_ptr as usize, got_ptr as usize);
                    }
                }

                Ok(Some(PatchBlock::Data(PatchDataBlock {
                    name: code_page_name.to_string(),
                    block,
                    symbols: pointers,
                    relocations: self.relocations.clone(),
                })))
            } else {
                // oom?
                unimplemented!()
            }
        } else {
            Ok(None)
        }
    }

    pub fn create_code_mem(
        &self,
        code_page_name: &str,
        b: &mut BlockFactory,
    ) -> Result<Option<PatchBlock>, Box<dyn Error>> {
        // get a list of data symbols
        let symbols = self.symbols.iter().filter(|(_, s)| {
            s.kind == CodeSymbolKind::Text
        }).collect::<Vec<_>>();

        if symbols.len() > 0 {
            let size = self.bytes.len();
            if let Some(block) = b.alloc_code(size) {
                block.as_mut_slice()[0..size].copy_from_slice(&self.bytes);
                let mut pointers = im::HashMap::new();
                for (_, s) in &symbols {
                    unsafe {
                        let ptr = block.as_ptr().offset(s.address as isize) as *const ();
                        pointers.insert(s.name.clone(), ptr);
                    }
                }

                Ok(Some(PatchBlock::Code(PatchCodeBlock {
                    name: code_page_name.to_string(),
                    block,
                    unknowns: self.unknowns.clone(),
                    symbols: pointers,
                    relocations: self.relocations.clone(),
                })))
            } else {
                // oom?
                unimplemented!()
            }
        } else {
            Ok(None)
        }
    }
}
