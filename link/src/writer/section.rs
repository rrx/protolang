use std::error::Error;

use object::elf;
use object::read::elf::FileHeader;
use object::write::elf::{SectionIndex, Writer};
use object::write::StringId;
use object::Endianness;
use std::collections::HashMap;

use super::*;

#[derive(Debug)]
pub struct ProgSymbol {
    pub name_id: Option<StringId>,
    pub is_start: bool,
    pub s: CodeSymbol,
}

pub struct ProgSection {
    pub name: Option<String>,
    pub name_id: Option<StringId>,
    pub index: Option<SectionIndex>,
    pub kind: AllocSegment,
    pub size: u64,
    pub symbols: HashMap<String, ProgSymbol>,
    pub relocations: Vec<CodeRelocation>,
    pub bytes: Vec<u8>,
}
impl ProgSection {
    pub fn new(kind: AllocSegment, name: Option<String>) -> Self {
        Self {
            name,
            name_id: None,
            index: None,
            kind,
            size: 0,
            symbols: HashMap::new(),
            relocations: vec![],
            bytes: vec![],
        }
    }

    pub fn append<'a>(&mut self, unlinked: &'a UnlinkedCodeSegment, w: &mut Writer<'a>) {
        //let base = self.size as u64;
        self.bytes.extend(unlinked.bytes.clone());
        //self.size += unlinked.bytes.len();
        for r in &unlinked.relocations {
            let mut r = r.clone();
            r.offset += self.size as u64;
            self.relocations.push(r.clone());
        }

        for (name, symbol) in unlinked.defined.iter() {
            let name_id = Some(w.add_string(name.as_bytes()));
            let mut symbol = symbol.clone();
            symbol.address += self.size as u64;
            let is_start = name == "_start";
            let ps = ProgSymbol {
                name_id,
                is_start,
                s: symbol,
            };
            self.symbols.insert(name.clone(), ps);
        }
        self.size += unlinked.bytes.len() as u64;
    }
}
