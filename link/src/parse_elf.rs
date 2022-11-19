use object::{
    Object, ObjectSection, ObjectSymbol, ObjectSymbolTable, RelocationTarget, SectionKind,
    SymbolKind, SymbolScope, SymbolSection,
};
use std::error::Error;
use std::sync::Arc;

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
    pub(crate) name: String,
    pub(crate) size: u64,
    pub(crate) address: u64,
    //pub(crate) ptr: *const (),
    pub(crate) kind: CodeSymbolKind,
    pub(crate) def: CodeSymbolDefinition,
}

pub type UnlinkedCode = Arc<UnlinkedCodeInner>;
pub struct UnlinkedCodeInner {
    pub(crate) name: String,
    pub(crate) bytes: Vec<u8>,
    pub(crate) symbols: im::HashMap<String, CodeSymbol>,
    pub(crate) relocations: im::HashMap<String, CodeRelocation>,
}

impl UnlinkedCodeInner {
    pub fn create(name: &str, buf: &[u8]) -> Result<Self, Box<dyn Error>> {
        let obj_file = object::File::parse(buf)?;
        let mut symbols = im::HashMap::new();
        let mut relocations = im::HashMap::new();

        if let Some(symbol_table) = obj_file.symbol_table() {
            for section in obj_file.sections() {
                let section_name = section.name()?.to_string();
                eprintln!(
                    "Section[{:?}, {}, address: {}, size: {}, align: {}, kind: {:?}]",
                    section.index().0,
                    section_name,
                    section.address(),
                    section.size(),
                    section.align(),
                    section.kind()
                );

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
                }
            }

            for (_, r) in &relocations {
                eprintln!("{}", r);
            }

            for s in symbol_table.symbols() {
                // only track dynamic symbols for now
                let name = s.name()?.to_string();
                //eprintln!("symbol: {:?}", &s);
                let maybe_section = match s.section() {
                    SymbolSection::Section(section_index) => {
                        Some(obj_file.section_by_index(s.section_index().unwrap())?)
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
                    symbols.insert(code_symbol.name.clone(), code_symbol);
                }
            }
        }

        for (symbol_name, code_symbol) in &symbols {
            println!("Symbol[{}] = {:?}", symbol_name, code_symbol);
        }

        Ok(Self {
            name: name.to_string(),
            bytes: buf.to_vec(),
            symbols,
            relocations,
        })
    }
}
