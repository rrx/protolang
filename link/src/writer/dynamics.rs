use super::*;
use object::write::elf::Sym;
use object::write::elf::{SymbolIndex, Writer};
use object::write::StringId;
use std::collections::HashMap;

struct TrackStringIndex {
    index: usize,
    string_id: StringId,
}

enum GotIndex {
    GOT(usize),
    GOTPLT(usize),
}

struct TrackSymbolIndex {
    index: usize,
    string_id: StringId,
    symbol_index: SymbolIndex,
    symbol: ReadSymbol,
    //st_type: u8,
    got_index: GotIndex,
    pointer: ResolvePointer,
}

pub struct Dynamics {
    // ordered list
    strings: Vec<String>,
    // hash of index
    string_hash: HashMap<String, TrackStringIndex>,

    // ordered list
    symbols: Vec<String>,
    symbol_hash: HashMap<String, TrackSymbolIndex>,

    r_got: Vec<(bool, String, bool)>,
    r_gotplt: Vec<(bool, String)>,

    // plt entries
    plt: Vec<(String, ResolvePointer, ResolvePointer)>,

    got_index: usize,
    gotplt_index: usize,
    plt_index: usize,
}

impl Dynamics {
    pub fn new() -> Self {
        Self {
            strings: vec![],
            string_hash: HashMap::new(),
            symbols: vec![],
            symbol_hash: HashMap::new(),
            r_got: vec![],
            r_gotplt: vec![],
            plt: vec![],
            got_index: 0,
            gotplt_index: 3,
            plt_index: 0,
        }
    }

    pub fn relocations(&self, kind: GotKind) -> Vec<(bool, String)> {
        match kind {
            GotKind::GOT(_) => self.r_got.iter().cloned().map(|(b, s, _)| (b, s)).collect(),
            GotKind::GOTPLT => self.r_gotplt.iter().cloned().collect(),
        }
    }

    pub fn plt_objects(&self) -> Vec<(String, ResolvePointer, ResolvePointer)> {
        self.plt.clone()
        /*
        self.r_gotplt
            .iter()
            .map(|(_, name)| {
                let track = self.symbol_hash.get(name).unwrap();
                (name, track.symbol.pointer.clone())
            })
            .collect()
            */
    }

    pub fn string_get(&self, name: &str) -> StringId {
        self.string_hash
            .get(name)
            .expect(&format!("String not found: {}", name))
            .string_id
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
                let string_id = w.add_dynamic_string(buf);
                let string_index = TrackStringIndex { index, string_id };
                self.string_hash.insert(cloned_name, string_index);
                string_id
            }
        }
    }

    pub fn symbol_count(&self) -> usize {
        self.symbols.len()
    }

    pub fn relocation_add(&mut self, symbol: &ReadSymbol, kind: GotKind) {}

    pub fn symbol_add(
        &mut self,
        symbol: &ReadSymbol,
        kind: GotKind,
        r: &CodeRelocation,
        w: &mut Writer,
    ) -> SymbolIndex {
        let name = &symbol.name;

        if let Some(track) = self.symbol_hash.get(name) {
            if let GotIndex::GOT(index) = track.got_index {
                if symbol.kind == object::SymbolKind::Text
                    && symbol.source == SymbolSource::Dynamic
                    && r.r.kind() == object::RelocationKind::PltRelative
                {
                    let pointer = ResolvePointer::Got(index);
                    let plt_index = self.plt.len();
                    self.plt
                        .push((symbol.name.clone(), pointer, ResolvePointer::Plt(plt_index)));
                }
            }
            track.symbol_index
        } else {
            let symbol = symbol.clone();
            let string_id = self.string_add(name, w);
            let symbol_index = SymbolIndex(w.reserve_dynamic_symbol_index().0);
            eprintln!("sym: {:?}", symbol);
            eprintln!("r: {}", r);

            let pointer = match kind {
                GotKind::GOT(_) => ResolvePointer::Got(self.got_index),
                GotKind::GOTPLT => ResolvePointer::GotPlt(self.gotplt_index),
            };

            let got_index = match kind {
                GotKind::GOT(_) => {
                    let index = GotIndex::GOT(self.got_index);
                    self.got_index += 1;
                    index
                }
                GotKind::GOTPLT => {
                    let index = GotIndex::GOTPLT(self.gotplt_index);
                    self.gotplt_index += 1;
                    index
                }
            };

            match kind {
                GotKind::GOT(relative) => self.r_got.push((relative, name.to_string(), false)),
                GotKind::GOTPLT => self.r_gotplt.push((false, name.to_string())),
            }

            let index = self.symbols.len();
            self.symbols.push(name.to_string());

            let track = TrackSymbolIndex {
                index,
                string_id,
                symbol_index,
                //st_type,
                got_index,
                pointer,
                symbol,
            };

            self.symbol_hash.insert(name.to_string(), track);

            symbol_index
        }
    }

    pub fn symbol_get(&self, name: &str, data: &Data) -> Option<(SymbolIndex, Sym)> {
        self.symbol_hash.get(name).map(|track| {
            let sym = track.symbol.get_dynamic_symbol(data);
            (track.symbol_index, sym)
        })
    }

    pub fn symbols_write(&self, data: &Data, w: &mut Writer) {
        w.write_null_dynamic_symbol();
        for name in self.symbols.iter() {
            let (_symbol_index, sym) = self.symbol_get(name, data).unwrap();
            w.write_dynamic_symbol(&sym);
        }
    }

    pub fn symbols(&self) -> Vec<(String, ResolvePointer)> {
        let mut out = vec![];
        for name in self.symbols.iter() {
            let track = self.symbol_hash.get(name).unwrap();
            out.push((name.to_string(), track.pointer.clone()));
        }
        out
    }
}
