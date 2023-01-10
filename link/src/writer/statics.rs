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
    symbol_index: Option<SymbolIndex>,
    symbol: ReadSymbol,
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
            let s = track.symbol.get_symbol(data);
            symbols.push(s);
        }
        symbols
    }

    pub fn symbol_add(
        &mut self,
        symbol: &ReadSymbol,
        section_index: Option<SectionIndex>,
        w: &mut Writer,
    ) {
        if let Some(track) = self.symbol_hash.get(&symbol.name) {
        } else {
            let string_id = self.string_add(&symbol.name, w);
            let symbol_index = Some(w.reserve_symbol_index(section_index));
            let index = self.symbols.len();
            self.symbols.push(symbol.name.to_string());

            let track = StaticSymbolIndex {
                index,
                string_id,
                symbol_index,
                symbol: symbol.clone(),
            };

            self.symbol_hash.insert(symbol.name.to_string(), track);
        }
    }

    /*
    pub fn symbol_get2(&self, name: &str) -> SymbolIndex {
        self.symbol_hash
            .get(name)
            .expect(&format!("Pointer not found: {}", name))
            .symbol_index
    }
    */
}
