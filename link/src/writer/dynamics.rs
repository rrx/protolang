use super::*;
use object::write::elf::Sym;
use object::write::elf::{SectionIndex, SymbolIndex, Writer};
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
    st_type: u8,
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

    r_got: Vec<(bool, String, i64)>,
    r_gotplt: Vec<(bool, String, i64)>,

    got_index: usize,
    gotplt_index: usize,
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
            got_index: 0,
            gotplt_index: 3,
        }
    }

    pub fn relocations(&self, kind: GotKind) -> Vec<(bool, String, i64)> {
        match kind {
            GotKind::GOT(_) => self.r_got.iter().cloned().collect(),
            GotKind::GOTPLT => self.r_gotplt.iter().cloned().collect(),
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

    pub fn relocation_add(
        &mut self,
        name: &str,
        kind: GotKind,
        addend: i64,
        w: &mut Writer,
    ) -> SymbolIndex {
        if let Some(index) = self.symbol_hash.get(name) {
            index.symbol_index
        } else {
            let string_id = self.string_add(name, w);
            let symbol_index = SymbolIndex(w.reserve_dynamic_symbol_index().0);

            let st_type = match kind {
                GotKind::GOT(_) => elf::STT_OBJECT,
                GotKind::GOTPLT => elf::STT_FUNC,
            };

            let pointer = match kind {
                GotKind::GOT(_) => ResolvePointer::Got(self.got_index),
                GotKind::GOTPLT => ResolvePointer::Got(self.gotplt_index),
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
                GotKind::GOT(relative) => self.r_got.push((relative, name.to_string(), addend)),
                GotKind::GOTPLT => self.r_gotplt.push((false, name.to_string(), addend)),
            }

            let index = self.symbols.len();
            self.symbols.push(name.to_string());

            let track = TrackSymbolIndex {
                index,
                string_id,
                symbol_index,
                st_type,
                got_index,
                pointer,
            };

            self.symbol_hash.insert(name.to_string(), track);

            symbol_index
        }
    }

    pub fn symbol_get(&self, name: &str) -> Option<(SymbolIndex, Sym)> {
        self.symbol_hash.get(name).map(|track| {
            let stb = elf::STB_GLOBAL;
            let st_info = (stb << 4) + (track.st_type & 0x0f);
            let st_other = elf::STV_DEFAULT;
            (
                track.symbol_index,
                Sym {
                    name: Some(track.string_id),
                    section: None,
                    st_info,
                    st_other,
                    st_shndx: 0,
                    st_value: 0,
                    st_size: 0,
                },
            )
        })
    }

    pub fn symbols_write(&self, w: &mut Writer) {
        w.write_null_dynamic_symbol();
        for name in self.symbols.iter() {
            let (_symbol_index, sym) = self.symbol_get(name).unwrap();
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
