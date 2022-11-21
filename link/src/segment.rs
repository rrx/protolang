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

pub type UnlinkedCodeSegment = Arc<UnlinkedCodeSegmentInner>;

pub struct UnlinkedCodeSegmentInner {
    pub(crate) name: String,
    pub(crate) bytes: Vec<u8>,
    pub(crate) symbols: im::HashMap<String, CodeSymbol>,
    pub(crate) unknowns: HashSet<String>,
    pub(crate) relocations: Vec<CodeRelocation>,
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
        let mut segments = vec![];
        let mut unknowns = HashSet::new();

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
                        match (s.scope(), s.kind()) {
                            (SymbolScope::Dynamic | SymbolScope::Linkage, SymbolKind::Text) => {
                                Some(CodeSymbol {
                                    name,
                                    address: s.address(),
                                    size: s.size(),
                                    kind: CodeSymbolKind::Text,
                                    def: CodeSymbolDefinition::Defined,
                                })
                            }

                            (SymbolScope::Dynamic, SymbolKind::Unknown) => {
                                let kind = match section.kind() {
                                    SectionKind::Text => CodeSymbolKind::Text,
                                    SectionKind::Data => CodeSymbolKind::Data,
                                    _ => unimplemented!()
                                };

                                Some(CodeSymbol {
                                    name,
                                    address: s.address(),
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
                                address: s.address(),
                                size: s.size(),
                                kind: CodeSymbolKind::Data,
                                def: CodeSymbolDefinition::Defined,
                            }),

                            // skip these
                            (SymbolScope::Compilation, _) => None,
                            _ => unimplemented!(
                                "Symbol Scope: {:?}, Kind: {:?}",
                                s.scope(),
                                s.kind()
                            ),
                        }

                        /*
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
                                    (SymbolScope::Dynamic, SectionKind::UninitializedTls) => {
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
                                    (SymbolScope::Dynamic, SectionKind::Tls) => Some(CodeSymbol {
                                        name,
                                        address: s.address(),
                                        size: s.size(),
                                        kind: CodeSymbolKind::Data,
                                        def: CodeSymbolDefinition::Defined,
                                    }),
                                    (SymbolScope::Dynamic, SectionKind::ReadOnlyString) => Some(CodeSymbol {
                                        name,
                                        address: s.address(),
                                        size: s.size(),
                                        kind: CodeSymbolKind::Data,
                                        def: CodeSymbolDefinition::Defined,
                                    }),
                                    (SymbolScope::Dynamic, SectionKind::ReadOnlyData) => Some(CodeSymbol {
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

                                    (SymbolScope::Linkage, SectionKind::Text) => Some(CodeSymbol {
                                        name,
                                        address: s.address(),
                                        size: s.size(),
                                        kind: CodeSymbolKind::Text,
                                        def: CodeSymbolDefinition::Defined,
                                    }),

                                    (SymbolScope::Linkage, SectionKind::Data) => Some(CodeSymbol {
                                        name,
                                        address: s.address(),
                                        size: s.size(),
                                        kind: CodeSymbolKind::Data,
                                        def: CodeSymbolDefinition::Defined,
                                    }),

                                    // skip these
                                    (SymbolScope::Compilation, _) => None,
                                    //(SymbolScope::Linkage, _) => None,
                                    _ => unimplemented!("Symbol Scope: {:?}, Section Kind: {:?}", s.scope(), section.kind() ),
                                }
                        */
                    }

                    None => match s.kind() {
                        SymbolKind::Unknown | SymbolKind::Tls => {
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
                    " Section[{:?}, {}, address: {}, size: {}, align: {}, kind: {:?}]",
                    section_index,
                    section_name,
                    section.address(),
                    section.size(),
                    section.align(),
                    section.kind()
                );
                let mut section_symbols = im::HashMap::new();

                for (symbol_name, (maybe_section, code_symbol)) in &symbols {
                    match maybe_section {
                        Some(symbol_section) => {
                            if symbol_section.index() == section.index() {
                                eprintln!(" Symbol[{}] = {:?}", &symbol_name, &code_symbol);
                                section_symbols.insert(symbol_name.clone(), code_symbol.clone());
                            }
                        }
                        None => (),
                    }
                }

                let mut relocations = vec![];
                for (reloc_offset, r) in section.relocations() {
                    let symbol = if let RelocationTarget::Symbol(symbol_index) = r.target() {
                        symbol_table.symbol_by_index(symbol_index)?
                    } else {
                        unimplemented!()
                    };
                    let name = symbol.name()?.to_string();

                    match symbol.scope() {
                        SymbolScope::Dynamic | SymbolScope::Unknown | SymbolScope::Linkage => {
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
                        SymbolScope::Compilation => (),

                        _ => unimplemented!("{:?}", symbol),
                    }

                    for r in &relocations {
                        eprintln!(" {}", r);
                    }
                }

                let name = format!("{}{}", link_name, section_name);
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

    pub fn create_data(
        &self,
        code_page_name: &str,
        b: &mut BlockFactory,
    ) -> Result<Option<PatchBlock>, Box<dyn Error>> {
        // get a list of data symbols
        let symbols = self
            .symbols
            .iter()
            .filter(|(_, s)| s.kind == CodeSymbolKind::Data)
            .collect::<Vec<_>>();

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
                    eprintln!(" Symbol: {}", symbol);
                }
                // to create the data section, we need to copy the data, but we also need
                // to create pointers to the data

                // copy the data over, and the symbols have the offsets
                // append the got entries after the data
                block.as_mut_slice()[0..self.bytes.len()].copy_from_slice(&self.bytes);
                let mut pointers = HashMap::new();
                unsafe {
                    let mut entry_counter = 0;
                    let got_base = block.as_ptr().offset(self.bytes.len() as isize) as *mut u64;
                    for (name, s) in &symbols {
                        let value_ptr = block.as_ptr().offset(s.address as isize) as *const ();
                        let got_ptr = got_base.offset(entry_counter);
                        *got_ptr = value_ptr as u64;
                        entry_counter += 1;
                        pointers.insert(s.name.clone(), got_ptr as *const ());
                        eprintln!(
                            " GOT: {}, Value: {:#08x}, Entry: {:#08x}",
                            &name, value_ptr as usize, got_ptr as usize
                        );
                    }
                }

                Ok(Some(PatchBlock::Data(PatchDataBlock {
                    name: code_page_name.to_string(),
                    block: WritableDataBlock::new(block),
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

    pub fn create_code(
        &self,
        code_page_name: &str,
        b: &mut BlockFactory,
    ) -> Result<Option<PatchBlock>, Box<dyn Error>> {
        // get a list of data symbols
        let symbols = self
            .symbols
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
                    eprintln!(" Symbol: {}", symbol);
                }

                for r in &self.relocations {
                    eprintln!(" Relocation: {}", r);
                }

                block.as_mut_slice()[0..size].copy_from_slice(&self.bytes);
                let mut pointers = HashMap::new();
                for (_, s) in &symbols {
                    unsafe {
                        let ptr = block.as_ptr().offset(s.address as isize) as *const ();
                        pointers.insert(s.name.clone(), ptr);
                    }
                }

                Ok(Some(PatchBlock::Code(PatchCodeBlock {
                    name: code_page_name.to_string(),
                    block: WritableCodeBlock::new(block),
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
