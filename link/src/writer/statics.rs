use super::*;
use object::write::elf::Sym;
use object::write::elf::{SectionIndex, SymbolIndex, Writer};
use object::write::StringId;

use std::collections::HashMap;

struct StaticStringIndex {
    index: usize,
    string_id: StringId,
}

struct StaticSymbolIndex {
    index: usize,
    string_id: StringId,
    symbol_index: SymbolIndex,
    section_index: Option<SectionIndex>,
    st_type: u8,
    pointer: ResolvePointer,
}

pub struct Statics {
    // ordered list
    strings: Vec<String>,
    // hash of index
    string_hash: HashMap<String, StaticStringIndex>,

    // ordered list
    symbols: Vec<String>,
    symbol_hash: HashMap<String, StaticSymbolIndex>,
}

impl Statics {
    pub fn new() -> Self {
        Self {
            strings: vec![],
            string_hash: HashMap::new(),
            symbols: vec![],
            symbol_hash: HashMap::new(),
        }
    }

    pub fn string_add(&mut self, name: &str, w: &mut Writer) -> StringId {
        if let Some(index) = self.string_hash.get(name) {
            index.string_id
        } else {
            let name = name.to_string();
            let cloned_name = name.clone();
            unsafe {
                let buf = extend_lifetime(name.as_bytes());
                // save the string
                let index = self.strings.len();
                self.strings.push(name);
                let string_id = w.add_string(buf);
                let string_index = StaticStringIndex { index, string_id };
                self.string_hash.insert(cloned_name, string_index);
                string_id
            }
        }
    }

    pub fn string_get(&self, name: &str) -> StringId {
        self.string_hash
            .get(name)
            .expect(&format!("String not found: {}", name))
            .string_id
    }

    pub fn symbol_count(&self) -> usize {
        self.symbols.len()
    }

    pub fn gen_symbols(&self, data: &Data) -> Vec<Sym> {
        let mut symbols = vec![];
        for name in self.symbols.iter() {
            let track = self.symbol_hash.get(name).unwrap();
            let st_value = track.pointer.resolve(data).unwrap();
            let s = Sym {
                name: Some(track.string_id),
                section: track.section_index,
                st_info: 0,
                st_other: 0,
                st_shndx: 0,
                st_value,
                st_size: 0,
            };
            symbols.push(s);
        }
        symbols
    }

    //pub fn symbol_add2(&mut self, symbol: ReadSymbol, data: &Data, w: &mut Writer) -> SymbolIndex {
    //self.symbol_add(&symbol.name, symbol.section.section_index(data), w)
    //}

    pub fn symbol_add(
        &mut self,
        name: &str,
        section_index: Option<SectionIndex>,
        w: &mut Writer,
    ) -> SymbolIndex {
        if let Some(index) = self.symbol_hash.get(name) {
            index.symbol_index
        } else {
            let string_id = self.string_add(name, w);
            let symbol_index = SymbolIndex(w.reserve_symbol_index(section_index).0);

            let st_type = elf::STT_OBJECT;
            let pointer = ResolvePointer::Resolved(0);

            /*
            let st_type = match kind {
                GotKind::GOT(_) => elf::STT_OBJECT,
                GotKind::GOTPLT => elf::STT_FUNC,
            };

            let pointer = match kind {
                GotKind::GOT(_) => ResolvePointer::Got(self.got_index),
                GotKind::GOTPLT => ResolvePointer::Got(self.gotplt_index),
            };
            */

            let index = self.symbols.len();
            self.symbols.push(name.to_string());

            let track = StaticSymbolIndex {
                index,
                string_id,
                symbol_index,
                section_index,
                st_type,
                pointer,
            };

            self.symbol_hash.insert(name.to_string(), track);

            symbol_index
        }
    }

    pub fn symbol_get(&self, name: &str) -> SymbolIndex {
        self.symbol_hash
            .get(name)
            .expect(&format!("Pointer not found: {}", name))
            .symbol_index
    }
}
