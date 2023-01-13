use super::*;
use object::write::elf::Sym;
use object::write::elf::{SymbolIndex, Writer};
use object::write::StringId;
use object::RelocationKind;
use std::collections::HashMap;

struct TrackStringIndex {
    index: usize,
    string_id: StringId,
}

enum GotIndex {
    GOT(usize),
    GOTPLT(usize),
}

#[derive(Debug)]
pub enum GotPltAssign {
    GOT,        // object
    GOT_PLTGOT, // function
    GOTPLT_PLT, // function
    None,
}

struct TrackSymbolIndex {
    index: usize,
    string_id: StringId,
    symbol_index: SymbolIndex,
    symbol: ReadSymbol,
    //got_index: GotIndex,
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
    plt_hash: HashMap<String, ResolvePointer>,
    pltgot: Vec<(String, ResolvePointer)>,
    pltgot_hash: HashMap<String, ResolvePointer>,

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

    pub fn relocations(&self, kind: GotKind) -> Vec<(bool, String, ResolvePointer)> {
        match kind {
            GotKind::GOT(_) => self.r_got.iter().cloned().collect(),
            GotKind::GOTPLT => self.r_gotplt.iter().cloned().collect(),
        }
    }

    pub fn lookup(&self, r: &CodeRelocation) -> Option<ResolvePointer> {
        eprintln!("lookup: {}, {:?}", &r.name, r.r.kind());
        if r.is_plt() {
            //.r.kind() == object::RelocationKind::PltRelative {
            self.pltgot_hash.get(&r.name).cloned()
        } else {
            self.symbol_hash
                .get(&r.name)
                .map(|track| track.pointer.clone())
        }

        /*
        match r.effect() {
            PatchEffect::AddToPlt => {
                self.pltgot_hash.get(&r.name).cloned()
            }
            PatchEffect::AddToGot => {
                self.symbol_hash.get(&r.name).map(|track| track.pointer.clone())
            }
            _ => None
        }
        */
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

    //pub fn update(&self, data: &Data) {
    //}

    pub fn symbol_add(
        &mut self,
        symbol: &ReadSymbol,
        //kind: GotKind,
        relative: bool,
        assign: GotPltAssign,
        r: &CodeRelocation,
        w: &mut Writer,
    ) -> SymbolIndex {
        let name = &symbol.name;

        let track = if let Some(track) = self.symbol_hash.get(name) {
            track
        } else {
            let symbol = symbol.clone();
            let string_id = self.string_add(name, w);
            let symbol_index = SymbolIndex(w.reserve_dynamic_symbol_index().0);
            eprintln!("sym: {:?}", symbol);
            eprintln!("r: {:?}, {}", assign, r);

            /*
            let pointer = match assign {
                GotPltAssign::GOT | GotPltAssign::GOT_PLTGOT => ResolvePointer::Got(self.got_index),
                GotPltAssign::GOTPLT_PLT => ResolvePointer::GotPlt(self.gotplt_index),
                _ => unreachable!()
            };
            */

            let pointer = match assign {
                GotPltAssign::GOT => {
                    let pointer = ResolvePointer::Got(self.got_index);
                    //let index = GotIndex::GOT(self.got_index);

                    if r.is_plt() {
                        //.r.kind() == RelocationKind::PltRelative {
                        self.pltgot.push((
                            symbol.name.clone(),
                            ResolvePointer::PltGot(self.pltgot_index),
                        ));
                        self.pltgot_index += 1;
                    }

                    //self.pltgot
                    //.push((symbol.name.clone(), pointer.clone(), pointer.clone()));//ResolvePointer::Got(self.got_index)));
                    self.r_got
                        .push((relative, name.to_string(), pointer.clone()));

                    self.got_index += 1;

                    pointer
                }

                GotPltAssign::GOT_PLTGOT => {
                    let pointer = ResolvePointer::Got(self.got_index);
                    //let index = GotIndex::GOT(self.got_index);

                    if r.is_plt() {
                        //r.kind() == RelocationKind::PltRelative {
                        self.pltgot.push((
                            symbol.name.clone(),
                            ResolvePointer::PltGot(self.pltgot_index),
                        ));
                        self.pltgot_index += 1;
                    }

                    self.got_index += 1;
                    pointer
                }

                GotPltAssign::GOTPLT_PLT => {
                    let pointer = ResolvePointer::Plt(self.plt_index);
                    //let index = GotIndex::GOTPLT(self.gotplt_index);

                    //self.plt
                    //.push((symbol.name.clone(), pointer.clone(), pointer.clone()));//ResolvePointer::Plt(self.plt_index)));
                    //self.plt_index += 1;

                    self.plt.push((symbol.name.clone(), pointer.clone()));
                    self.plt_index += 1;

                    self.r_gotplt
                        .push((false, name.to_string(), pointer.clone()));
                    self.gotplt_index += 1;

                    //self.plt
                    //.push((symbol.name.clone(), pointer.clone(), ResolvePointer::Plt(self.got_index)));
                    //.push((symbol.name.clone(), pointer.clone(), ResolvePointer::Plt(0)));//self.gotplt_index)));//self.got_index)));

                    pointer
                }
                _ => unreachable!(),
            };

            //match assign {
            //GotPltAssign::GOT | GotPltAssign::GOT_PLTGOT => self.r_got.push((relative, name.to_string(), false)),
            //GotPltAssign::GOTPLT_PLT => self.r_gotplt.push((false, name.to_string())),
            //_ => unreachable!()
            //}

            let index = self.symbols.len();
            self.symbols.push(name.to_string());

            let track = TrackSymbolIndex {
                index,
                string_id,
                symbol_index,
                pointer,
                symbol,
            };

            self.symbol_hash.insert(name.to_string(), track);
            self.symbol_hash.get(name).unwrap()
        };

        /*
        if symbol.kind == object::SymbolKind::Text
            && symbol.source == SymbolSource::Dynamic
            && r.r.kind() == object::RelocationKind::PltRelative
        {
            match track.got_index {
                GotIndex::GOT(index) => {
                    let pointer = ResolvePointer::Got(index);
                    let plt_index = self.plt.len();
                    self.plt
                        .push((symbol.name.clone(), pointer, ResolvePointer::Plt(plt_index)));
                }
                GotIndex::GOTPLT(index) => {
                    let pointer = ResolvePointer::GotPlt(index);
                    let plt_index = self.plt.len();
                    self.plt
                        //.push((symbol.name.clone(), pointer.clone(), pointer));
                        .push((symbol.name.clone(), pointer, ResolvePointer::Plt(plt_index)));
                }
            }
        }
        */
        track.symbol_index
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
