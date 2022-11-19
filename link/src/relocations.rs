use object::{Relocation, RelocationEncoding, RelocationKind, RelocationTarget};

use super::*;

#[derive(Debug, Clone)]
pub struct LinkRelocation {
    pub(crate) kind: RelocationKind,
    pub(crate) encoding: RelocationEncoding,
    pub(crate) size: u8,
    pub(crate) target: RelocationTarget,
    pub(crate) addend: i64,
    pub(crate) implicit_addend: bool,
}

impl From<Relocation> for LinkRelocation {
    fn from(item: Relocation) -> Self {
        Self {
            kind: item.kind(),
            encoding: item.encoding(),
            size: item.size(),
            target: item.target(),
            addend: item.addend(),
            implicit_addend: item.has_implicit_addend(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Reloc {
    pub(crate) symbol_name: String,
    pub(crate) r: LinkRelocation,
}

