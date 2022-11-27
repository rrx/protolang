use object::{
    Object, ObjectSection, ObjectSymbol, ObjectSymbolTable, RelocationTarget, SectionKind,
    SymbolKind, SymbolScope, SymbolSection,
};
use std::error::Error;
use std::fmt;
use std::sync::Arc;

use std::collections::{HashMap, HashSet};

use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum CodeSymbolDefinition {
    Extern,
    Defined,
}

#[derive(Clone, Debug, PartialEq)]
pub enum CodeSymbolKind {
    Text,
    Data,
    Section,
}

#[derive(Clone, Debug)]
pub struct DataSection {
    name: String,
    size: u64,
    symbols: Vec<CodeSymbol>,
}

#[derive(Clone, Debug)]
pub struct CodeSymbol {
    name: String,
    size: u64,
    pub(crate) address: u64,
    kind: CodeSymbolKind,
    pub(crate) def: CodeSymbolDefinition,
}

impl fmt::Display for CodeSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Symbol addr: {:6}, size: {:6}, kind: {:?}, def: {:?}: {}",
            self.address, self.size, self.kind, self.def, self.name
        )
    }
}

pub struct GotEntry {}

pub struct PltEntry {}

pub type UnlinkedCodeSegment = Arc<UnlinkedCodeSegmentInner>;

pub struct UnlinkedCodeSegmentInner {
    pub(crate) name: String,
    pub(crate) section_name: String,
    pub(crate) bytes: Vec<u8>,
    pub(crate) defined: im::HashMap<String, CodeSymbol>,
    pub(crate) internal: im::HashMap<String, CodeSymbol>,
    pub(crate) externs: HashSet<String>,
    pub(crate) relocations: Vec<CodeRelocation>,
    //pub(crate) got_entries: Vec<GotEntry>,
    //pub(crate) plt_entries: Vec<PltEntry>,
    pub(crate) data_section: Option<DataSection>,
}

impl UnlinkedCodeSegmentInner {
    pub fn read_archive(archive_name: &str, buf: &[u8]) -> Result<Vec<Self>, Box<dyn Error>> {
        eprintln!("Archive: {}", archive_name);
        let archive = object::read::archive::ArchiveFile::parse(buf)?;
        eprintln!(
            "Archive: {}, size: {}, kind: {:?}",
            archive_name,
            buf.len(),
            archive.kind()
        );
        let mut segments = vec![];
        for result in archive.members() {
            let m = result?;
            let name = std::str::from_utf8(&m.name())?;
            let (offset, size) = m.file_range();
            let obj_buf = &buf[offset as usize..(offset + size) as usize];
            eprintln!("Member: {}, {:?}", &name, &m);
            segments.extend(Self::create_segments(name, obj_buf)?);
        }
        Ok(segments)
    }

    pub fn create_segments(link_name: &str, buf: &[u8]) -> Result<Vec<Self>, Box<dyn Error>> {
        eprintln!("Segment: {}, size: {}", link_name, buf.len());
        let obj_file = object::File::parse(buf)?;
        let mut symbols = HashMap::new();
        let mut symbols_by_id = HashMap::new();
        let mut segments = vec![];
        let mut externs = HashSet::new();
        let mut internal = im::HashMap::new();

        if let Some(symbol_table) = obj_file.symbol_table() {
            for s in symbol_table.symbols() {
                // only track dynamic symbols for now
                let name = s.name()?.to_string();
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
                    " Symbol[{}, {:20}, address: {:#04x}, size: {}, kind: {:?}, scope: {:?}, weak: {}, section: {:?}]",
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
                        let section_start = section.address();
                        let address = s.address() - section_start;
                        match (s.scope(), s.kind()) {
                            (SymbolScope::Dynamic | SymbolScope::Linkage, SymbolKind::Text) => {
                                Some(CodeSymbol {
                                    name,
                                    address,
                                    size: s.size(),
                                    kind: CodeSymbolKind::Text,
                                    def: CodeSymbolDefinition::Defined,
                                })
                            }

                            (SymbolScope::Dynamic, SymbolKind::Unknown) => {
                                let kind = match section.kind() {
                                    SectionKind::Text => CodeSymbolKind::Text,
                                    SectionKind::Data => CodeSymbolKind::Data,
                                    SectionKind::ReadOnlyData => CodeSymbolKind::Data,
                                    // XXX:
                                    _ => continue,
                                };

                                Some(CodeSymbol {
                                    name,
                                    address,
                                    size: s.size(),
                                    kind,
                                    def: CodeSymbolDefinition::Defined,
                                })
                            }

                            (
                                SymbolScope::Dynamic | SymbolScope::Linkage,
                                SymbolKind::Data | SymbolKind::Tls,
                            ) => Some(CodeSymbol {
                                name,
                                address,
                                size: s.size(),
                                kind: CodeSymbolKind::Data,
                                def: CodeSymbolDefinition::Defined,
                            }),

                            (SymbolScope::Compilation, SymbolKind::Data) => Some(CodeSymbol {
                                name,
                                address,
                                size: s.size(),
                                kind: CodeSymbolKind::Data,
                                def: CodeSymbolDefinition::Defined,
                            }),

                            (SymbolScope::Compilation, SymbolKind::Section) => {
                                let name = section.name()?.to_string();
                                let code_symbol = CodeSymbol {
                                    name: name.clone(),
                                    address,
                                    size: s.size(),
                                    kind: CodeSymbolKind::Section,
                                    def: CodeSymbolDefinition::Defined,
                                };
                                internal.insert(name, code_symbol.clone());
                                Some(code_symbol)
                            }

                            _ => unimplemented!(
                                "Symbol Scope: {:?}, Kind: {:?}",
                                s.scope(),
                                s.kind()
                            ),
                        }
                    }

                    None => match s.kind() {
                        SymbolKind::Unknown | SymbolKind::Tls => {
                            // external references
                            externs.insert(name.clone());
                            None
                            /*
                            Some(CodeSymbol {
                                name,
                                address: s.address(),
                                size: s.size(),
                                kind: CodeSymbolKind::Text,
                                def: CodeSymbolDefinition::Extern,
                            })
                            */
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
                symbols_by_id.insert(s.index().clone(), s);
            }

            /*
            for section in obj_file.sections() {
                let section_name = section.name()?.to_string();
                internal.insert(section_name.clone(), CodeSymbol {
                    name: section_name.clone(),
                    size: section.size(),
                    address: section.address(),
                    kind: CodeSymbolKind::Section,
                    def: CodeSymbolDefinition::Defined
                });
            }
            */

            for section in obj_file.sections() {
                let section_name = section.name()?.to_string();
                let section_index = section.index().0;
                eprintln!(
                    " Section[{:?}, {}, address: {}, size: {}, align: {}, kind: {:?}, relocs: {}]",
                    section_index,
                    section_name,
                    section.address(),
                    section.size(),
                    section.align(),
                    section.kind(),
                    section.relocations().count()
                );
                let mut defined = im::HashMap::new();

                let mut section_symbols = vec![];

                for (symbol_name, (maybe_section, code_symbol)) in &symbols {
                    match maybe_section {
                        Some(symbol_section) => {
                            if symbol_section.index() == section.index() {
                                section_symbols.push(code_symbol.clone());
                                if code_symbol.kind == CodeSymbolKind::Section {
                                    //eprintln!("  Internal Symbol[{}] = {:?}", &symbol_name, &code_symbol);
                                    //internal.insert(symbol_name.clone(), code_symbol.clone());
                                } else {
                                    eprintln!(
                                        "  Defined  Symbol[{}] = {:?}",
                                        &symbol_name, &code_symbol
                                    );
                                    defined.insert(symbol_name.clone(), code_symbol.clone());
                                }
                            }
                        }
                        None => (),
                    }
                }

                let data_section = match section.kind() {
                    SectionKind::Data | SectionKind::ReadOnlyData => Some(DataSection {
                        name: section_name.clone(),
                        size: section.size(),
                        symbols: section_symbols,
                    }),
                    _ => None,
                };

                let mut relocations = vec![];
                for (reloc_offset, r) in section.relocations() {
                    //eprintln!(" R:{:?}", (&reloc_offset,&r));
                    let symbol = if let RelocationTarget::Symbol(symbol_index) = r.target() {
                        symbol_table.symbol_by_index(symbol_index)?
                    } else {
                        // relocation must be associated with a symbol
                        unimplemented!()
                    };
                    let name = symbol.name()?.to_string();

                    match (symbol.kind(), symbol.scope()) {
                        (_, SymbolScope::Dynamic | SymbolScope::Unknown | SymbolScope::Linkage) => {
                            // | SymbolScope::Linkage | SymbolScope::Unknown => {
                            relocations.push(CodeRelocation {
                                name,
                                offset: reloc_offset,
                                r: r.into(),
                            });
                        }

                        //do nothing here
                        //SymbolScope::Unknown => {
                        //unknowns.insert(name);
                        //}
                        (SymbolKind::Data, SymbolScope::Compilation) => {
                            relocations.push(CodeRelocation {
                                name,
                                offset: reloc_offset,
                                r: r.into(),
                            });
                        }
                        (SymbolKind::Section, SymbolScope::Compilation) => {
                            // if the relocation references a section, then look up the section
                            // name
                            let section_index = symbol.section().index().unwrap();
                            let section = obj_file.section_by_index(section_index)?;
                            let name = section.name()?.to_string();
                            relocations.push(CodeRelocation {
                                name,
                                offset: reloc_offset,
                                r: r.into(),
                            });
                        }
                        _ => unimplemented!("{:?}", symbol),
                    }
                }
                for r in &relocations {
                    eprintln!("  {}", r);
                }

                let name = format!("{}{}", link_name, section_name);
                let data = section.uncompressed_data()?;
                //eprintln!(" data: {}, size: {}", name, data.len());

                // for bss, we have empty data, so we pass in a zero initialized buffer
                // to be consistent
                let bytes = if section.size() as usize > data.len() {
                    let mut data = Vec::new();
                    data.resize(section.size() as usize, 0);
                    data
                } else {
                    data.to_vec()
                };

                //let got_entries = vec![];
                //let plt_entries = vec![];

                segments.push(UnlinkedCodeSegmentInner {
                    name,
                    section_name,
                    bytes,
                    externs: externs.clone(),
                    defined,
                    internal: internal.clone(),
                    relocations,
                    //got_entries,
                    //plt_entries,
                    data_section,
                });
            }
        }

        Ok(segments)
    }

    pub fn create_data(
        &self,
        code_page_name: &str,
        b: &mut BlockFactory,
    ) -> Result<Option<PatchBlock>, Box<dyn Error>> {
        // get a list of data symbols
        let symbols = self
            .defined
            .iter()
            .filter(|(_, s)| s.kind == CodeSymbolKind::Data || s.kind == CodeSymbolKind::Section)
            .collect::<Vec<_>>();

        /*
        if let Some(data_section) = &self.data_section {
            let size = data_section.size as usize;
            let block = b.alloc_block(size).unwrap();
            eprintln!(
                "XData Block: {}, size: {}",
                &code_page_name,
                self.bytes.len()
                );
            block.as_mut_slice()[0..self.bytes.len()].copy_from_slice(&self.bytes);
            let mut pointers = HashMap::new();
            unsafe {
                for s in &data_section.symbols {
                    let value_ptr = block.as_ptr().offset(s.address as isize) as *const ();
                    eprintln!(" Data Symbol: {}:{}:{:#08x}", s.name, s.address, value_ptr as usize);
                    pointers.insert(s.name.clone(), value_ptr as *const ());
                }
            }
            Ok(Some(PatchBlock::Data(PatchDataBlock {
                name: code_page_name.to_string(),
                block: WritableDataBlock::new(block),
                symbols: pointers,
                relocations: self.relocations.clone(),
            })))
        } else
            */
        if symbols.len() > 0 {
            // allocate enough space for the actual data, and a lookup table as well
            let size = self.bytes.len() + symbols.len() * std::mem::size_of::<usize>();

            if let Some(block) = b.alloc_block(size) {
                eprintln!(
                    "Data Block: {}, size: {}",
                    &code_page_name,
                    self.bytes.len()
                );
                for (_symbol_name, symbol) in &symbols {
                    eprintln!(" Data Symbol: {}", symbol);
                }
                // to create the data section, we need to copy the data, but we also need
                // to create pointers to the data

                // copy the data over, and the symbols have the offsets
                // append the got entries after the data
                block.as_mut_slice()[0..self.bytes.len()].copy_from_slice(&self.bytes);
                let mut pointers = HashMap::new();
                let mut internal = HashMap::new();

                internal.insert(self.section_name.clone(), block.as_ptr() as *const ());
                pointers.insert(self.section_name.clone(), block.as_ptr() as *const ());

                unsafe {
                    for (_name, s) in &symbols {
                        let value_ptr = block.as_ptr().offset(s.address as isize) as *const ();
                        pointers.insert(s.name.clone(), value_ptr as *const ());
                    }

                    for (_name, s) in &self.internal {
                        let value_ptr = block.as_ptr().offset(s.address as isize) as *const ();
                        internal.insert(s.name.clone(), value_ptr as *const ());
                    }
                }

                Ok(Some(PatchBlock::Data(PatchDataBlock {
                    name: code_page_name.to_string(),
                    block: WritableDataBlock::new(block),
                    symbols: pointers,
                    internal,
                    relocations: self.relocations.clone(),
                })))
            } else {
                // oom?
                unimplemented!()
            }
        } else {
            eprintln!(
                "no symbols in {}, size:{}, {:?}",
                code_page_name,
                self.bytes.len(),
                &self.relocations.len()
            );
            Ok(None)
        }
    }

    pub fn create_code(
        &self,
        code_page_name: &str,
        b: &mut BlockFactory,
    ) -> Result<Option<PatchBlock>, Box<dyn Error>> {
        // get a list of data symbols
        let symbols = self
            .defined
            .iter()
            .filter(|(_, s)| s.kind == CodeSymbolKind::Text)
            .collect::<Vec<_>>();

        if symbols.len() > 0 {
            let size = self.bytes.len();
            if let Some(block) = b.alloc_block(size) {
                eprintln!(
                    "Code Block: {}, size: {}",
                    &code_page_name,
                    self.bytes.len()
                );
                for (_symbol_name, symbol) in &symbols {
                    eprintln!(" Code Symbol: {}", symbol);
                }

                for r in &self.relocations {
                    eprintln!(" Code Relocation: {}", r);
                }

                // copy code into the block
                block.as_mut_slice()[0..size].copy_from_slice(&self.bytes);

                // for each symbol, add a reference to it's full address
                let mut pointers = HashMap::new();
                let mut internal = HashMap::new();

                unsafe {
                    for (_, s) in &symbols {
                        let ptr = block.as_ptr().offset(s.address as isize) as *const ();
                        pointers.insert(s.name.clone(), ptr);
                    }

                    for (_name, s) in &self.internal {
                        let value_ptr = block.as_ptr().offset(s.address as isize) as *const ();
                        internal.insert(s.name.clone(), value_ptr as *const ());
                    }
                }

                Ok(Some(PatchBlock::Code(PatchCodeBlock {
                    name: code_page_name.to_string(),
                    block: WritableCodeBlock::new(block),
                    externs: self.externs.clone(),
                    symbols: pointers,
                    internal,
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