use super::*;
use object::write::elf::Sym;
use object::write::elf::{SymbolIndex, Writer};
use object::write::StringId;
use std::collections::HashMap;

struct TrackStringIndex {
    index: usize,
    string_id: StringId,
}

#[derive(Debug)]
pub enum GotPltAssign {
    Got,           // object
    GotWithPltGot, // function
    GotPltWithPlt, // function
    None,
}

struct TrackSymbolIndex {
    index: usize,
    string_id: StringId,
    symbol_index: SymbolIndex,
    symbol: ReadSymbol,
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

    r_got: Vec<(bool, String, ResolvePointer)>,
    r_gotplt: Vec<(bool, String, ResolvePointer)>,

    // plt entries
    plt: Vec<(String, ResolvePointer)>,
    pub plt_hash: HashMap<String, ResolvePointer>,
    pltgot: Vec<(String, ResolvePointer)>,
    pub pltgot_hash: HashMap<String, ResolvePointer>,

    got_index: usize,
    gotplt_index: usize,
    plt_index: usize,
    pltgot_index: usize,
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
            plt_hash: HashMap::new(),

            pltgot: vec![],
            pltgot_hash: HashMap::new(),
            got_index: 0,
            gotplt_index: 3,
            plt_index: 0,
            pltgot_index: 0,
        }
    }

    pub fn relocations(&self, kind: GotSectionKind) -> Vec<(bool, String, ResolvePointer)> {
        match kind {
            GotSectionKind::GOT => self.r_got.iter().cloned().collect(),
            GotSectionKind::GOTPLT => self.r_gotplt.iter().cloned().collect(),
        }
    }

    pub fn lookup(&self, r: &CodeRelocation) -> Option<ResolvePointer> {
        if r.is_got() {
            if let Some(track) = self.symbol_hash.get(&r.name) {
                Some(track.pointer.clone())
            } else {
                None
            }
        } else if r.is_plt() {
            if let Some(addr) = self.plt_hash.get(&r.name) {
                Some(addr.clone())
            } else if let Some(addr) = self.pltgot_hash.get(&r.name) {
                Some(addr.clone())
            } else if let Some(track) = self.symbol_hash.get(&r.name) {
                Some(track.pointer.clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn pltgot_objects(&self) -> Vec<(String, ResolvePointer)> {
        self.pltgot.clone()
    }

    pub fn plt_objects(&self) -> Vec<(String, ResolvePointer)> {
        self.plt.clone()
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

    pub fn relocation_add(
        &mut self,
        symbol: &ReadSymbol,
        //kind: GotKind,
        relative: bool,
        assign: GotPltAssign,
        r: &CodeRelocation,
        w: &mut Writer,
    ) -> SymbolIndex {
        let name = &symbol.name;

        if let Some(track) = self.symbol_hash.get(name) {
            track.symbol_index
        } else {
            eprintln!("sym: {:?}", symbol);
            eprintln!("r: {:?}, {}", assign, r);

            let pointer = match assign {
                GotPltAssign::Got => {
                    /*
                    if r.is_plt() {
                        let pointer = ResolvePointer::PltGot(self.pltgot_index);
                        self.pltgot.push((symbol.name.clone(), pointer.clone()));
                        self.pltgot_hash.insert(name.to_string(), pointer);
                        self.pltgot_index += 1;
                    }
                    */

                    let pointer = ResolvePointer::Got(self.got_index);
                    self.r_got
                        .push((relative, name.to_string(), pointer.clone()));

                    self.got_index += 1;

                    pointer
                }

                GotPltAssign::GotWithPltGot => {
                    let pointer = ResolvePointer::PltGot(self.pltgot_index);
                    self.pltgot.push((symbol.name.clone(), pointer.clone()));
                    self.pltgot_hash.insert(name.to_string(), pointer);
                    self.pltgot_index += 1;

                    let pointer = ResolvePointer::Got(self.got_index);
                    self.r_got
                        .push((relative, name.to_string(), pointer.clone()));

                    self.got_index += 1;
                    pointer
                }

                GotPltAssign::GotPltWithPlt => {
                    let pointer = ResolvePointer::Plt(self.plt_index);
                    self.plt.push((symbol.name.clone(), pointer.clone()));
                    self.plt_hash.insert(name.to_string(), pointer.clone());
                    self.plt_index += 1;

                    self.r_gotplt
                        .push((false, name.to_string(), pointer.clone()));
                    self.gotplt_index += 1;

                    pointer
                }
                _ => unreachable!(),
            };

            self._symbol_add(symbol.clone(), pointer, w)
        }
    }

    fn _symbol_add(
        &mut self,
        symbol: ReadSymbol,
        pointer: ResolvePointer,
        w: &mut Writer,
    ) -> SymbolIndex {
        let index = self.symbols.len();
        self.symbols.push(symbol.name.clone());

        let string_id = self.string_add(&symbol.name, w);
        let symbol_index = SymbolIndex(w.reserve_dynamic_symbol_index().0);
        let symbol = symbol.clone();
        let name = symbol.name.clone();
        let track = TrackSymbolIndex {
            index,
            string_id,
            symbol_index,
            pointer,
            symbol,
        };

        self.symbol_hash.insert(name.clone(), track);
        symbol_index
    }

    pub fn symbol_lookup(&self, name: &str) -> Option<ResolvePointer> {
        self.symbol_hash
            .get(name)
            .map(|track| track.pointer.clone())
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
