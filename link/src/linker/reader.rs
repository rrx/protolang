// read elf file
use object::elf::FileHeader64;
use object::read::elf;
use object::read::elf::ProgramHeader;
use object::write::elf::{SectionIndex, Writer};
use object::write::StringId;
use object::{
    Object, ObjectKind, ObjectSection, ObjectSymbol, RelocationTarget, SectionKind, SymbolIndex,
    SymbolKind,
};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::path::Path;

use super::*;
use crate::disassemble::*;
use crate::writer::*;
use crate::*;

pub type SymbolMap = HashMap<String, ReadSymbol>;

#[derive(Debug)]
pub struct Reader {
    // blocks
    blocks: Vec<ReadBlock>,

    // link block
    block: ReadBlock,

    got: HashSet<String>,
    plt: HashSet<String>,
}

impl Reader {
    pub fn new() -> Self {
        Self {
            blocks: vec![],
            block: ReadBlock::new("exe"),
            got: HashSet::new(),
            plt: HashSet::new(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ReadSectionKind {
    RX,
    RO,
    RW,
    Bss,
    Undefined,
    Other,
}

impl ReadSectionKind {
    pub fn block(&self) -> Box<dyn ElfBlock> {
        Box::new(BlockSectionX::new(self.clone()))
    }

    pub fn section_index(&self, data: &Data) -> Option<SectionIndex> {
        match self {
            ReadSectionKind::RX => Some(data.section_index_get(".text")),
            ReadSectionKind::RW => Some(data.section_index_get(".data")),
            ReadSectionKind::RO => Some(data.section_index_get(".rodata")),
            ReadSectionKind::Bss => Some(data.section_index_get(".bss")),
            _ => None,
        }
    }
}

impl ReadSectionKind {
    pub fn new_section_kind(kind: SectionKind) -> Self {
        match kind {
            SectionKind::Text => ReadSectionKind::RX,
            SectionKind::Data => ReadSectionKind::RW,
            SectionKind::ReadOnlyData => ReadSectionKind::RO,
            SectionKind::ReadOnlyString => ReadSectionKind::RO,
            SectionKind::UninitializedData => ReadSectionKind::Bss,
            SectionKind::Metadata => ReadSectionKind::Other,
            SectionKind::OtherString => ReadSectionKind::Other,
            SectionKind::Other => ReadSectionKind::Other,
            SectionKind::Note => ReadSectionKind::Other,
            SectionKind::UninitializedTls => ReadSectionKind::Other,
            SectionKind::Tls => ReadSectionKind::Other,
            SectionKind::Elf(_) => ReadSectionKind::Other,
            _ => unimplemented!("{:?}", kind),
        }
    }

    pub fn section_name(&self) -> &'static str {
        match self {
            Self::RX => ".text",
            Self::RO => ".rodata",
            Self::RW => ".data",
            Self::Bss => ".bss",
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct ReadSection {
    kind: ReadSectionKind,
    relocations: Vec<LinkRelocation>,
    bytes: Vec<u8>,
    bss: usize,
}
impl ReadSection {
    pub fn new(kind: ReadSectionKind) -> Self {
        Self {
            kind,
            relocations: vec![],
            bytes: vec![],
            bss: 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolSource {
    Dynamic,
    Static,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolBind {
    Local,
    Global,
    Weak,
}

#[derive(Debug, Clone)]
pub enum SymbolLookupTable {
    GOT,
    PLT,
    None,
}

#[derive(Debug, Clone)]
pub struct ReadSymbol {
    pub(crate) name: String,
    pub(crate) name_id: Option<StringId>,
    pub(crate) dyn_name_id: Option<StringId>,
    pub(crate) section: ReadSectionKind,
    source: SymbolSource,
    kind: SymbolKind,
    bind: SymbolBind,
    pub(crate) address: u64,
    pub(crate) size: u64,
    lookup: SymbolLookupTable,
}

#[derive(Debug)]
pub struct ReadBlock {
    name: String,
    // dynamic libraries referenced
    libs: HashSet<String>,
    local_index: usize,
    pub(crate) locals: SymbolMap,
    pub(crate) exports: SymbolMap,
    pub(crate) dynamic: SymbolMap,
    pub(crate) unknown: SymbolMap,
    pub ro: BlockSection,
    pub rw: BlockSection,
    pub rx: BlockSection,
    pub got: BlockSection,
    pub gotplt: BlockSection,
    pub bss: BssSection,
    pub unresolved: HashSet<String>,
}

impl ReadBlock {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            ro: BlockSection::new(AllocSegment::RO, ".rodata"),
            rw: BlockSection::new(AllocSegment::RW, ".data"),
            rx: BlockSection::new(AllocSegment::RX, ".text"),
            got: BlockSection::new(AllocSegment::RW, ".got"),
            gotplt: BlockSection::new(AllocSegment::RW, ".got.plt"),
            bss: BssSection::new(AllocSegment::RW, ".bss"),
            libs: HashSet::new(),
            local_index: 0,
            exports: SymbolMap::new(),
            locals: SymbolMap::new(),
            dynamic: SymbolMap::new(),
            unknown: SymbolMap::new(),
            unresolved: HashSet::new(),
        }
    }

    pub fn reserve_symbols(&mut self, data: &mut Data, w: &mut Writer) {}

    pub fn reserve_strings(&mut self, data: &mut Data, w: &mut Writer) {
        let iter = self
            .ro
            .section
            .relocations
            .iter()
            .chain(self.rw.section.relocations.iter())
            .chain(self.rx.section.relocations.iter())
            .chain(self.bss.relocations.iter());

        // add the relocations to the sets
        // we only want to add a relocation to either got or gotplt
        // if it's being added to got, then only add it to got
        // with entries in the got and gotplt, we then apply relocations
        // to point to the appropriate got and gotplt entries
        let mut got = HashSet::new();
        let mut gotplt = HashSet::new();
        for r in iter.clone() {
            match r.effect() {
                PatchEffect::AddToGot => {
                    got.insert(&r.name);
                }
                PatchEffect::AddToPlt => {
                    gotplt.insert(&r.name);
                }
                _ => (),
            }
        }

        for r in iter {
            if let Some(s) = self.lookup(&r.name) {
                // we don't know the section yet, we just know which kind
                let mut p = ProgSymbol::new_object(&r.name, Some(SectionIndex(0)));
                let def = match s.bind {
                    SymbolBind::Local => CodeSymbolDefinition::Local,
                    SymbolBind::Global => CodeSymbolDefinition::Defined,
                    SymbolBind::Weak => CodeSymbolDefinition::Defined,
                };
                p.s.def = def;

                if s.source == SymbolSource::Dynamic {
                    //eprintln!("p {:?}", &s);
                    let mut r = r.clone();
                    p.name_id = r.name_id;

                    eprintln!("reloc {}", &r);
                    if got.contains(&r.name) {
                        let symbol_index = data.dyn_relocation(&r.name, GotKind::GOT, w);
                        let sym = data.dyn_symbols.get(&r.name).unwrap();
                        r.name_id = sym.sym.name;
                        r.r.target = RelocationTarget::Symbol(sym.symbol_index);

                        let index = data.got_index(&r.name);
                        //data.relocations_got.push(r.clone());
                        //self.got.section.relocations.push(r.clone());
                        data.lookup.insert(r.name.clone(), p.clone());
                    } else if gotplt.contains(&r.name) {
                        let symbol_index = data.dyn_relocation(&r.name, GotKind::GOTPLT, w);
                        let sym = data.dyn_symbols.get(&r.name).unwrap();
                        r.name_id = sym.sym.name;
                        r.r.target = RelocationTarget::Symbol(sym.symbol_index);

                        let index = data.gotplt_index(&r.name);
                        //self.gotplt.section.relocations.push(r.clone());
                        data.lookup.insert(r.name.clone(), p.clone());
                    }
                } else if p.s.def != CodeSymbolDefinition::Local {
                    p.name_id = Some(data.string(&r.name, w));
                    data.lookup.insert(r.name.clone(), p.clone());
                }
            } else {
                unreachable!()
            }
        }

        /*
         // Write relocations
         // we need the offset is the address + offset of GOT or GOTPLT entry
         // we need the symbol index
         // We map the type depending got , or gotplt
         // addend is 0
            let r_type = match self.kind {
                GotKind::GOT => elf::R_X86_64_GLOB_DAT,
                GotKind::GOTPLT => elf::R_X86_64_JUMP_SLOT,
            };
        for name in got.iter() {
            &object::write::elf::Rel {
                r_offset: r_offset as u64,
                r_sym,
                r_type,
                r_addend,
            },
            */

        /*
        for r in data.relocations_got.iter() {
            eprintln!("r got: {}", r);
        }
        for r in data.relocations_gotplt.iter() {
            eprintln!("r gotplt: {}", r);
        }
        */
    }

    /*
    pub fn base(&self, section: ReadSectionKind) -> usize {
        match section {
            ReadSectionKind::RX => self.rx.base,
            ReadSectionKind::RW => self.rw.base,
            ReadSectionKind::RO => self.ro.base,
            ReadSectionKind::Bss => self.bss.section.base,
            _ => unreachable!(),
        }
    }

    pub fn file_offset(&self, section: ReadSectionKind) -> usize {
        match section {
            ReadSectionKind::RX => self.rx.file_offset,
            ReadSectionKind::RW => self.rw.file_offset,
            ReadSectionKind::RO => self.ro.file_offset,
            ReadSectionKind::Bss => self.bss.file_offset,
            _ => unreachable!(),
        }
    }
    */

    /*
        pub fn update_symbols(&mut self, data: &mut Data, w: &mut Writer) {
            let mut got = vec![];
            let mut plt = vec![];
            // write symbols
            for (name, s) in self
                .exports
                .iter_mut()
                //.chain(self.locals.iter_mut())
                //.chain(self.dynamic.iter_mut())
                .chain(self.unknown.iter_mut())
            {
                let section_index = s.section.section_index(data);
                if section_index.is_none() {
                    continue;
                }

                let base = self.base(s.section);
                let file_offset = self.file_offset(s.section);

                //let mut s = s.clone();
                let addr = base + file_offset + s.address as usize;
                s.address = addr as u64;
                data.pointer_set(name.clone(), addr as u64);
                assert!(s.name_id.is_some());
                let p = ProgSymbol {
                    name_id: s.name_id,
                    section_index,
                    base,
                    s: CodeSymbol {
                        name: s.name.clone(),
                        size: s.size,
                        address: addr as u64,
                        kind: CodeSymbolKind::Data,
                        def: CodeSymbolDefinition::Defined,
                        st_info: 0,
                        st_other: 0,
                    },
                };
                data.symbol_set(name.clone(), p);
            }

            for r in self
                .rx
                .relocations
                .iter()
                .chain(self.ro.relocations.iter())
                .chain(self.rw.relocations.iter())
                .chain(self.bss.relocations.iter())
            {
                if let Some(s) = self.lookup(&r.name) {
                    let section_index = match s.section {
                        ReadSectionKind::RX => data.section_index_get(".text"),
                        ReadSectionKind::RW => data.section_index_get(".data"),
                        ReadSectionKind::RO => data.section_index_get(".rodata"),
                        ReadSectionKind::Bss => data.section_index_get(".bss"),
                        ReadSectionKind::Other => continue,
                        _ => unreachable!("{:?}", s),
                    };
                    let base = match s.section {
                        ReadSectionKind::RX => self.rx.base,
                        ReadSectionKind::RW => self.rw.base,
                        ReadSectionKind::RO => self.ro.base,
                        ReadSectionKind::Bss => self.bss.base,
                        _ => unreachable!(),
                    };
                    let file_offset = match s.section {
                        ReadSectionKind::RX => self.rx.file_offset,
                        ReadSectionKind::RW => self.rw.file_offset,
                        ReadSectionKind::RO => self.ro.file_offset,
                        ReadSectionKind::Bss => self.bss.file_offset,
                        _ => unreachable!(),
                    };
                    let mut s = s.clone();
                    let addr = base + file_offset + s.address as usize;
                    s.address = addr as u64;
                    let p = ProgSymbol {
                        name_id: s.name_id,
                        section_index: Some(section_index),
                        base,
                        s: CodeSymbol {
                            name: s.name.clone(),
                            size: s.size,
                            address: addr as u64,
                            kind: CodeSymbolKind::Data,
                            def: CodeSymbolDefinition::Defined,
                            st_info: 0,
                            st_other: 0,
                        },
                    };
                    let s = p.get_symbol();
                    plt.push(r.clone());
                    let s = p.get_symbol();
                    got.push(r.clone());
                }
                //let p = data.symbol_get(&r.name);
            }

            for r in plt.iter() {
                data.sections.unapplied_plt.push(r.clone());
            }
            for r in got.iter() {
                data.sections.unapplied_got.push(r.clone());
            }
        }
    */

    pub fn write<Elf: object::read::elf::FileHeader<Endian = object::Endianness>>(
        self,
        path: &Path,
    ) -> Result<(), Box<dyn Error>> {
        let mut out_data = Vec::new();
        let mut data = crate::writer::Data::new(self.libs.iter().cloned().collect());
        let endian = object::Endianness::Little;
        let mut writer = object::write::elf::Writer::new(endian, data.is_64, &mut out_data);
        data.block = Some(self);
        write_file_main::<Elf>(&mut data, &mut writer)?;
        std::fs::write(path, out_data)?;
        Ok(())
    }

    /*
    pub fn load<'a>(&self, w: &mut Writer<'a>) -> ProgSections {
        let mut out = ProgSections::new();
        if self.rx.bytes.len() > 0 {
            let name = ".text";
            let name_id = Some(w.add_section_name(name.as_bytes()));
            let mut section =
                ProgSection::new(AllocSegment::RX, Some(name.to_string()), name_id, 0);
            section.add_bytes(self.rx.bytes.as_slice());

            use crate::segment::*;
            for (name, symbol) in self.locals.iter().chain(self.exports.iter()) {
                //let name_id = Some(w.add_string(name.as_bytes()));
                let name_id = None;
                let mut symbol = symbol.clone();
                //symbol.address += self.base as u64 + self.addr as u64 + self.data_count as u64;
                let ps = ProgSymbol {
                    name_id,
                    section_index: None,
                    base: 0,
                    s: CodeSymbol {
                        name: name.clone(),
                        size: symbol.size,
                        address: 0,
                        kind: CodeSymbolKind::Text,
                        def: CodeSymbolDefinition::Defined,
                        st_info: 0,
                        st_other: 0,
                    },
                };
                eprintln!("symbol extern: {}, {:#0x}", &name, &ps.s.address);
                section.symbols.insert(name.clone(), ps);
            }

            section.relocations = self.rx.relocations.clone();
            out.add(section);
            //data.sections.add(section);
        }

        if self.ro.bytes.len() > 0 {
            let name = ".rodata".to_string();
            let name_id = Some(w.add_section_name(".rodata".as_bytes()));
            let mut section = ProgSection::new(AllocSegment::RO, Some(name), name_id, 0);
            section.add_bytes(self.ro.bytes.as_slice());
            out.add(section);
            //data.sections.add(section);
        }

        if self.rw.bytes.len() > 0 {
            let name = ".data".to_string();
            let name_id = Some(w.add_section_name(".data".as_bytes()));
            let mut section = ProgSection::new(AllocSegment::RW, Some(name), name_id, 0);
            section.add_bytes(self.rw.bytes.as_slice());
            out.add(section);
            //data.sections.add(section);
        }
        out
    }
    */

    pub fn insert_local(&mut self, s: ReadSymbol) {
        self.locals.insert(s.name.clone(), s);
    }
    pub fn insert_export(&mut self, s: ReadSymbol) {
        self.exports.insert(s.name.clone(), s);
    }
    pub fn insert_dynamic(&mut self, s: ReadSymbol) {
        self.dynamic.insert(s.name.clone(), s);
    }
    pub fn insert_unknown(&mut self, s: ReadSymbol) {
        self.unknown.insert(s.name.clone(), s);
    }

    fn relocate_symbol(&self, s: &mut ReadSymbol) {
        use ReadSectionKind::*;
        let addr = match s.section {
            RX => s.address + self.rx.section.bytes.len() as u64,
            RO => s.address + self.ro.section.bytes.len() as u64,
            RW => s.address + self.rw.section.bytes.len() as u64,
            Bss => s.address + self.bss.section.size as u64,
            _ => 0,
        };
        s.address = addr;
    }

    pub fn merge(renames: &HashMap<String, String>, src: &BlockSection, dst: &mut BlockSection) {
        let base_offset = dst.section.size;
        dst.section.size += src.section.size;
        for r in src.section.relocations.iter() {
            let mut r = r.clone();
            r.offset += base_offset as u64;
            if let Some(name) = renames.get(&r.name) {
                r.name = name.clone();
            }
            dst.section.relocations.push(r);
        }
    }

    pub fn add_block(&mut self, block: ReadBlock) {
        let mut renames = HashMap::new();

        // rename local symbols so they are globally unique
        for (name, mut s) in block.locals.into_iter() {
            self.relocate_symbol(&mut s);
            let unique = format!(".u.{}{}", self.local_index, name);
            s.name = unique.clone();
            self.local_index += 1;
            self.insert_local(s);
            renames.insert(name, unique);
        }

        // exports
        for (_name, mut s) in block.exports.into_iter() {
            self.relocate_symbol(&mut s);
            //eprintln!("E: {:?}", &s);
            self.insert_export(s);
        }

        // update BSS
        let base_offset = self.bss.section.size;
        self.bss.section.size += block.bss.section.size;
        for mut r in block.bss.relocations.into_iter() {
            r.offset += base_offset as u64;
            if let Some(name) = renames.get(&r.name) {
                r.name = name.clone();
            }
            self.bss.relocations.push(r);
        }

        // update RX
        Self::merge(&renames, &block.rx, &mut self.rx);
        /*
        let base_offset = self.rx.section.bytes.len();
        self.rx.section.bytes.extend(block.rx.section.bytes);
        for mut r in block.rx.relocations.into_iter() {
            r.offset += base_offset as u64;
            if let Some(name) = renames.get(&r.name) {
                r.name = name.clone();
            }
            self.rx.relocations.push(r);
        }
        */

        // update RO
        Self::merge(&renames, &block.ro, &mut self.ro);
        /*
        let base_offset = self.ro.section.bytes.len();
        self.ro.section.bytes.extend(block.ro.section.bytes);
        for mut r in block.ro.section.relocations.into_iter() {
            r.offset += base_offset as u64;
            if let Some(name) = renames.get(&r.name) {
                r.name = name.clone();
            }
            self.ro.section.relocations.push(r);
        }
        */

        // update RW
        Self::merge(&renames, &block.rw, &mut self.rw);
        /*
        let base_offset = self.rw.section.bytes.len();
        self.rw.section.bytes.extend(block.rw.section.bytes);
        for mut r in block.rw.section.relocations.into_iter() {
            r.offset += base_offset as u64;
            if let Some(name) = renames.get(&r.name) {
                r.name = name.clone();
            }
            self.rw.section.relocations.push(r);
        }
        */
    }

    pub fn from_section<'a, 'b, A: elf::FileHeader, B: object::ReadRef<'a>>(
        &mut self,
        b: &elf::ElfFile<'a, A, B>,
        section: &elf::ElfSection<'a, 'b, A, B>,
    ) -> Result<(), Box<dyn Error>> {
        let kind = ReadSectionKind::new_section_kind(section.kind());
        match kind {
            ReadSectionKind::Bss => {
                self.bss.from_section(b, section)?;
                /*
                let base_offset = self.bss.section.size;
                self.bss.section.extend_size(section.size() as usize);
                for (offset, r) in section.relocations() {
                    let r = code_relocation(b, r.into(), base_offset + offset as usize)?;
                    self.bss.relocations.push(r);
                }
                */
            }
            ReadSectionKind::RX => {
                self.rx.from_section(b, section);
                /*
                let data = section.uncompressed_data()?;
                let base_offset = self.rx.section.size;
                self.rx.section.extend_bytes(&data);
                for (offset, r) in section.relocations() {
                    let r = code_relocation(b, r.into(), base_offset + offset as usize)?;
                    self.rx.relocations.push(r);
                }
                */
            }
            ReadSectionKind::RO => {
                self.rx.from_section(b, section);
                /*
                let data = section.uncompressed_data()?;
                let base_offset = self.ro.section.size;
                self.ro.section.extend_bytes(&data);
                for (offset, r) in section.relocations() {
                    let r = code_relocation(b, r.into(), base_offset + offset as usize)?;
                    self.ro.relocations.push(r);
                }
                */
            }
            ReadSectionKind::RW => {
                self.rw.from_section(b, section);
                /*
                let data = section.uncompressed_data()?;
                let base_offset = self.rw.section.size;
                self.rw.section.extend_bytes(&data);
                for (offset, r) in section.relocations() {
                    let r = code_relocation(b, r.into(), base_offset + offset as usize)?;
                    self.rw.relocations.push(r);
                }
                */
            }
            _ => unimplemented!(),
        }
        Ok(())
    }

    pub fn lookup_static(&self, name: &str) -> Option<ReadSymbol> {
        if let Some(symbol) = self.locals.get(name) {
            Some(symbol.clone())
        } else if let Some(symbol) = self.exports.get(name) {
            Some(symbol.clone())
        } else {
            None
        }
    }

    pub fn lookup_dynamic(&self, name: &str) -> Option<ReadSymbol> {
        if let Some(symbol) = self.dynamic.get(name) {
            Some(symbol.clone())
        } else {
            None
        }
    }

    pub fn lookup(&self, name: &str) -> Option<ReadSymbol> {
        if let Some(symbol) = self.lookup_static(name) {
            Some(symbol.clone())
        } else if let Some(symbol) = self.lookup_dynamic(name) {
            Some(symbol.clone())
        } else {
            None
        }
    }

    pub fn complete(&self, data: &Data) {
        //eprintln!("Block: {}", &self.name);

        let mut dsymbols = HashMap::new();
        let mut rx_symbols = vec![];
        let mut rw_symbols = vec![];
        let mut ro_symbols = vec![];
        let mut bss_symbols = vec![];
        let mut other_symbols = vec![];

        for (name, s) in self.locals.iter().chain(self.exports.iter()) {
            dsymbols.insert(name, Symbol::new(0, s.address, &s.name));
            let sym = Symbol::new(0, s.address, &s.name);
            match s.section {
                ReadSectionKind::RX => rx_symbols.push(sym),
                ReadSectionKind::RW => rw_symbols.push(sym),
                ReadSectionKind::RO => ro_symbols.push(sym),
                ReadSectionKind::Bss => bss_symbols.push(sym),
                _ => other_symbols.push(sym),
            }
        }

        /*
        for r in self
            .rx
            .relocations
            .iter()
            .chain(self.ro.relocations.iter())
            .chain(self.rw.relocations.iter())
            .chain(self.bss.relocations.iter())
        {
            if let Some(s) = self.lookup(&r.name) {
                let addr = match s.section {
                    ReadSectionKind::RX => self.rx.addr,
                    ReadSectionKind::RW => self.rw.addr,
                    ReadSectionKind::RO => self.ro.addr,
                    ReadSectionKind::Bss => self.bss.addr,
                    _ => unreachable!(),
                };
                //eprintln!(" R: {}, addr, {:#0x}, {:?}", r, addr, s);
            }
        }
        */
    }

    pub fn dump(&self) {
        eprintln!("Block: {}", &self.name);

        let mut dsymbols = HashMap::new();
        let mut rx_symbols = vec![];
        let mut rw_symbols = vec![];
        let mut ro_symbols = vec![];
        let mut bss_symbols = vec![];
        let mut other_symbols = vec![];

        for (name, s) in self.locals.iter().chain(self.exports.iter()) {
            dsymbols.insert(name, Symbol::new(0, s.address, &s.name));
            let sym = Symbol::new(0, s.address, &s.name);
            match s.section {
                ReadSectionKind::RX => rx_symbols.push(sym),
                ReadSectionKind::RW => rw_symbols.push(sym),
                ReadSectionKind::RO => ro_symbols.push(sym),
                ReadSectionKind::Bss => bss_symbols.push(sym),
                _ => other_symbols.push(sym),
            }
        }
        eprintln!("RX, size: {:#0x}", self.rx.section.bytes.len());
        for local in rx_symbols.iter() {
            eprintln!(" S: {:?}", local);
        }
        for r in self.rx.section.relocations.iter() {
            eprintln!(" R: {}, {:?}", r, self.lookup(&r.name));
        }
        disassemble_code_with_symbols(
            self.rx.section.bytes.as_slice(),
            &rx_symbols,
            &self.rx.section.relocations,
        );

        eprintln!("RO, size: {:#0x}", self.ro.section.bytes.len());
        for local in ro_symbols.iter() {
            eprintln!(" S: {:?}", local);
        }
        for r in self.ro.section.relocations.iter() {
            eprintln!(" R: {}, {:?}", r, self.lookup(&r.name));
        }
        print_bytes(self.ro.section.bytes.as_slice(), 0);

        eprintln!("RW, size: {:#0x}", self.rw.section.bytes.len());
        for local in rw_symbols.iter() {
            eprintln!(" S: {:?}", local);
        }
        for r in self.rw.section.relocations.iter() {
            eprintln!(" R: {}, {:?}", r, self.lookup(&r.name));
        }
        print_bytes(self.rw.section.bytes.as_slice(), 0);

        eprintln!("Bss, size: {:#0x}", self.bss.section.size);
        for local in bss_symbols.iter() {
            eprintln!(" S: {:?}", local);
        }
        for r in self.bss.relocations.iter() {
            eprintln!(" R: {}, {:?}", r, self.lookup(&r.name));
        }

        if other_symbols.len() > 0 {
            eprintln!("Other");
            for local in other_symbols.iter() {
                eprintln!(" S: {:?}", local);
            }
        }

        if self.unresolved.len() > 0 {
            eprintln!("Unresolved: {}", self.unresolved.len());
            for s in self.unresolved.iter() {
                eprintln!(" {}", s);
            }
        }

        //assert!(notfound.len() == 0);
    }
}

impl Reader {
    pub fn add(&mut self, path: &std::path::Path) -> Result<(), Box<dyn Error>> {
        let buf = std::fs::read(path)?;
        self.elf_read(path.to_str().unwrap(), &buf)?;
        Ok(())
    }

    pub fn merge_export(&mut self, s: ReadSymbol) {
        // if we have two strong symbols, favor the first
        // if we have two weak symbols, favor the first
        // if we already have a weak symbol, and a strong one comes next, override
        // if we have a strong, and a weak follows, we ignore the weak

        // if it's defined, and undefined follows, it's defined
        // if its undefined, and defined follows, it's defined
        if let Some(existing) = self.block.lookup(&s.name) {
            use SymbolBind::*;
            match (&existing.bind, &s.bind) {
                (Weak, Weak) => (),

                // this might indicate a duplicate symbol
                (Global, Global) => (),

                // weak override
                (Weak, Global) => {
                    self.block.insert_export(s.clone());
                }

                // drop weak if we alredy have a global
                (Global, Weak) => (),
                (Local, _) => unreachable!(),
                (_, Local) => unreachable!(),
            }

            match (&existing.section, &s.section) {
                (ReadSectionKind::Undefined, _) => {
                    self.block.insert_unknown(s);
                }
                _ => (),
            }
        } else {
            self.block.insert_export(s);
        }
    }

    fn elf_read(&mut self, name: &str, buf: &[u8]) -> Result<(), Box<dyn Error>> {
        let b: elf::ElfFile<'_, FileHeader64<object::Endianness>> =
            object::read::elf::ElfFile::parse(buf)?;
        match b.kind() {
            ObjectKind::Relocatable => {
                let block = self.relocatable(name.to_string(), &b)?;
                self.blocks.push(block);
            }
            ObjectKind::Dynamic => {
                self.dynamic(&b)?;
                self.block.libs.insert(name.to_string());
            }
            _ => unimplemented!("{:?}", b.kind()),
        }
        Ok(())
    }

    fn dynamic<'a, 'b, A: elf::FileHeader, B: object::ReadRef<'a>>(
        &mut self,
        b: &elf::ElfFile<'a, A, B>,
    ) -> Result<(), Box<dyn Error>> {
        if let Some(dr) = b.dynamic_relocations() {
            for (_offset, r) in dr {
                //eprintln!("dr: {:#08x}, {:?}", offset, r);
            }
        }
        for symbol in b.dynamic_symbols() {
            let mut s = read_symbol(&b, &symbol)?;
            s.source = SymbolSource::Dynamic;
            //eprintln!("s: {:#08x}, {:?}", 0, &s);
            if s.kind != SymbolKind::Unknown {
                self.block.insert_dynamic(s);
                //self.insert_symbol(s);
            }
            //out.push(s);
        }
        Ok(())
    }

    fn relocatable<'a, 'b, A: elf::FileHeader, B: object::ReadRef<'a>>(
        &mut self,
        name: String,
        b: &elf::ElfFile<'a, A, B>,
    ) -> Result<ReadBlock, Box<dyn Error>> {
        let mut block = ReadBlock::new(&name);

        for symbol in b.symbols() {
            // skip the null symbol
            if symbol.kind() == SymbolKind::Null {
                continue;
            }
            if symbol.kind() == SymbolKind::File {
                continue;
            }

            let s = read_symbol(&b, &symbol)?;
            //eprintln!("Read: {:?}", &s);

            if s.bind == SymbolBind::Local {
                // can't be local and unknown
                if symbol.kind() == SymbolKind::Unknown {
                    unreachable!();
                }
                block.insert_local(s);
            } else if s.section == ReadSectionKind::Undefined {
                //block.insert_unknown(s);
            } else {
                self.merge_export(s.clone());
                block.insert_export(s);
            }
        }

        for section in b.sections() {
            let kind = ReadSectionKind::new_section_kind(section.kind());
            // skip other kinds
            if kind == ReadSectionKind::Other {
                continue;
            }

            block.from_section(&b, &section)?;
            //block.add_block(ReadBlock::from_section(&b, &section)?);
        }

        Ok(block)
    }

    pub fn build(mut self) -> ReadBlock {
        self.block.name = "exe".to_string();
        for b in self.blocks.into_iter() {
            self.block.add_block(b);
        }

        // make sure everything resolves
        let iter = self
            .block
            .rx
            .section
            .relocations
            .iter()
            .chain(self.block.ro.section.relocations.iter())
            .chain(self.block.rw.section.relocations.iter())
            .chain(self.block.bss.relocations.iter());

        for r in iter {
            if let Some(symbol) = self.block.lookup(&r.name) {
                //eprintln!(" R: {:?}", (r, symbol));
            } else {
                self.block.unresolved.insert(r.name.clone());
            }
        }

        self.block
    }
}

pub fn code_relocation<'a, 'b, A: elf::FileHeader, B: object::ReadRef<'a>>(
    b: &elf::ElfFile<'a, A, B>,
    r: LinkRelocation,
    offset: usize,
) -> Result<CodeRelocation, Box<dyn Error>> {
    let name = match r.target {
        RelocationTarget::Section(index) => {
            let section = b.section_by_index(index)?;
            section.name()?.to_string()
        }
        RelocationTarget::Symbol(index) => {
            let symbol = b.symbol_by_index(index)?;
            let name = if symbol.kind() == SymbolKind::Section {
                let section = b.section_by_index(symbol.section_index().unwrap())?;
                section.name()?.to_string()
            } else {
                symbol.name()?.to_string()
            };
            name
        }
        _ => unreachable!(),
    };
    Ok(CodeRelocation {
        name,
        name_id: None,
        offset: offset as u64,
        r,
    })
}

fn read_symbol<'a, 'b, A: elf::FileHeader, B: object::ReadRef<'a>>(
    b: &elf::ElfFile<'a, A, B>,
    symbol: &elf::ElfSymbol<'a, 'b, A, B>,
) -> Result<ReadSymbol, Box<dyn Error>> {
    let section_kind;
    let name = if symbol.kind() == SymbolKind::Section {
        let section = b.section_by_index(symbol.section_index().unwrap())?;
        section_kind = ReadSectionKind::new_section_kind(section.kind());
        section.name()?.to_string()
    } else {
        if let Some(section_index) = symbol.section_index() {
            let section = b.section_by_index(section_index)?;
            section_kind = ReadSectionKind::new_section_kind(section.kind());
        } else {
            section_kind = ReadSectionKind::Undefined;
        }
        symbol.name()?.to_string()
    };

    let address = symbol.address();
    let size = symbol.size();

    let bind = if symbol.is_local() {
        SymbolBind::Local
    } else if symbol.is_global() {
        SymbolBind::Global
    } else if symbol.is_weak() {
        SymbolBind::Weak
    } else {
        unreachable!()
    };

    Ok(ReadSymbol {
        name,
        name_id: None,
        dyn_name_id: None,
        section: section_kind,
        kind: symbol.kind(),
        bind,
        address,
        size,
        source: SymbolSource::Static,
        lookup: SymbolLookupTable::None,
    })
}

pub fn elf_read2(buf: &[u8]) -> Result<(), Box<dyn Error>> {
    let b: elf::ElfFile<'_, FileHeader64<object::Endianness>> =
        object::read::elf::ElfFile::parse(buf)?;

    let endian = b.endian();

    let h = b.raw_header();
    eprintln!("{:?}", h);
    eprintln!("e_entry: {:#0x}", h.e_entry.get(endian));
    eprintln!("e_phoff: {:#0x}", h.e_phoff.get(endian));
    eprintln!("e_phnum: {:#0x}", h.e_phnum.get(endian));
    for seg in b.raw_segments() {
        eprintln!("Segment");
        eprintln!("  p_type:   {:#0x}", seg.p_type(endian));
        eprintln!("  p_flags {:#0x}", seg.p_flags(endian));
        eprintln!("  p_offset {:#0x}", seg.p_offset(endian));
        eprintln!("  p_vaddr {:#0x}", seg.p_vaddr(endian));
        eprintln!("  p_paddr {:#0x}", seg.p_paddr(endian));
        eprintln!("  p_filesz: {:#0x}", seg.p_filesz(endian));
        eprintln!("  p_memsz:  {:#0x}", seg.p_memsz(endian));
        eprintln!("  p_align {:#0x}", seg.p_align(endian));
        let _offset = seg.p_offset(endian) as usize;
        let _size = seg.p_filesz(endian) as usize;
    }

    for section in b.sections() {
        let name = section.name()?;
        let section_addr = section.address();
        eprintln!("Section: {}", name);
        eprintln!("  kind:   {:?}", section.kind());
        eprintln!("  addr:   {:#0x}", section_addr);

        let mut symbols = vec![];
        let mut relocations = vec![];

        for (r_offset, r) in section.relocations() {
            relocations.push(CodeRelocation {
                name: "".to_string(),
                name_id: None,
                offset: r_offset,
                r: r.into(),
            });
        }

        for symbol in b.symbols() {
            if let Some(index) = symbol.section_index() {
                if index == section.index() {
                    if section_addr <= symbol.address() {
                        let addr = symbol.address() - section_addr;
                        let name = symbol.name()?;
                        symbols.push(Symbol::new(section_addr, addr, name));
                    }
                }
            }
        }

        let buf = section.data()?;
        if name == ".got" {
            for (offset, r) in b.dynamic_relocations().unwrap() {
                relocations.push(CodeRelocation {
                    name: "".to_string(),
                    name_id: None,
                    offset: offset - section_addr,
                    r: r.into(),
                });
            }
            disassemble_code_with_symbols(buf, &symbols, &relocations);
        } else if name == ".text" {
            disassemble_code_with_symbols(buf, &symbols, &relocations);
        }
    }

    /*
    for seg in b.segments() {
    eprintln!("Segment: {:?}", seg.name()?);
    eprintln!("  flags: {:?}", seg.flags());
    eprintln!("  addr:  {:#0x}", seg.address());
    eprintln!("  size:  {:#0x}", seg.size());
    eprintln!("  align:  {:#0x}", seg.align());
    }
    */
    //.program_headers()?;
    //b.raw_header().e_ident;
    //let obj_file = object::File::parse(buf)?;
    //obj_file.format()
    //let mut symbols = HashMap::new();
    //let mut symbols_by_id = HashMap::new();
    //let mut segments = vec![];
    //let mut externs = HashSet::new();
    //let mut internal = im::HashMap::new();
    Ok(())
}
