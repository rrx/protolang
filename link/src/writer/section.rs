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
    pub index: Option<SectionIndex>,
    pub kind: AllocSegment,
    pub addr: u64,
    pub symbols: HashMap<String, ProgSymbol>,
    pub relocations: Vec<CodeRelocation>,
}
impl ProgSection {
    pub fn new(kind: AllocSegment) -> Self {
        Self {
            index: None,
            kind,
            addr: 0,
            symbols: HashMap::new(),
            relocations: vec![],
        }
    }
}

/*
pub struct Sections {
    pub ro: ProgSection,
    pub rw: ProgSection,
    pub rx: ProgSection,
}
impl Default for Sections {
    fn default() -> Self {
        Self {
            ro: ProgSection::new(AllocSegment::RO),
            rw: ProgSection::new(AllocSegment::RW),
            rx: ProgSection::new(AllocSegment::RX),
        }
    }
}
*/
