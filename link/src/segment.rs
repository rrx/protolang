use memmap::MmapMut;
use object::{
    Object, ObjectSection, ObjectSymbol, ObjectSymbolTable, RelocationTarget, SectionKind,
    SymbolKind, SymbolScope, SymbolSection,
};
use std::error::Error;
use std::fmt;
use std::sync::Arc;

use std::collections::{HashMap, HashSet};

use super::*;

pub type UnlinkedCodeSegment = Arc<UnlinkedCodeSegmentInner>;

pub struct UnlinkedCodeSegmentInner {
    pub(crate) name: String,
    pub(crate) bytes: Vec<u8>,
    pub(crate) symbols: im::HashMap<String, CodeSymbol>,
    pub(crate) relocations: im::HashMap<String, CodeRelocation>,
}

impl UnlinkedCodeSegmentInner {
    pub fn create_segments(link_name: &str, buf: &[u8]) -> Result<Vec<Self>, Box<dyn Error>> {
        let obj_file = object::File::parse(buf)?;
        let mut symbols = HashMap::new();
        let mut segments = vec![]; //im::HashMap::new(); //<SegmentId, Segment>;

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
                        SymbolScope::Dynamic => {
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

                        SymbolScope::Compilation => (),

                        _ => unimplemented!(),
                    }
                    for (_, r) in &relocations {
                        eprintln!("{}", r);
                    }
                }

                let name = format!("{}_{}", link_name, section_index);
                let data = section.uncompressed_data()?;
                segments.push(UnlinkedCodeSegmentInner {
                    name,
                    bytes: data.to_vec(),
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
        let mut got_size = 0;
        let mut size = 0;
        let size = self.bytes.len();

        //for (name, code_symbol) in &self.symbols {
        //if code_symbol.kind == CodeSymbolKind::Data {
        //size += code_symbol.size as usize;
        //}
        //}

        if size > 0 {
            if let Some(block) = b.alloc_data(size) {
                let mut pointers = im::HashMap::new();
                for (name, code_symbol) in &self.symbols {
                    if code_symbol.kind == CodeSymbolKind::Data {
                        unsafe {
                            pointers.insert(
                                name.clone(),
                                block.as_ptr().offset(code_symbol.address as isize) as *const (),
                            );
                        }
                        //size += code_symbol.size as usize;
                    }
                }
                if pointers.len() > 0 {
                    Ok(Some(PatchBlock::Data(PatchDataBlock {
                        name: code_page_name.to_string(),
                        block,
                        symbols: pointers,
                        relocations: self.relocations.clone(),
                    })))
                } else {
                    Ok(None)
                }
            } else {
                unreachable!()
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
        let mut got_size = 0;
        let mut size = self.bytes.len();
        if size > 0 {
            if let Some(block) = b.alloc_code(size) {
                let mut pointers = im::HashMap::new();
                for (name, code_symbol) in &self.symbols {
                    if code_symbol.kind == CodeSymbolKind::Text {
                        unsafe {
                            pointers.insert(
                                name.clone(),
                                block.as_ptr().offset(code_symbol.address as isize) as *const (),
                            );
                        }
                    }
                }
                if pointers.len() > 0 {
                    Ok(Some(PatchBlock::Code(PatchCodeBlock {
                        name: code_page_name.to_string(),
                        block,
                        symbols: pointers,
                        relocations: self.relocations.clone(),
                    })))
                } else {
                    Ok(None)
                }
            } else {
                unreachable!()
            }
        } else {
            Ok(None)
        }
    }
}
