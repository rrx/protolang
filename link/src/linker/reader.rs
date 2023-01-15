// read elf file
use object::elf::FileHeader64;
use object::read::elf;
use object::read::elf::ProgramHeader;
use object::write::elf::{SectionIndex, Writer};
use object::write::StringId;
use object::{
    Object, ObjectKind, ObjectSection, ObjectSymbol, RelocationTarget, SectionKind, SymbolKind,
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
    ROStrings,
    ROData,
    RW,
    Bss,
    Undefined,
    Other,
}

impl ReadSectionKind {
    pub fn block(&self) -> Box<dyn ElfBlock> {
        Box::new(BlockSectionX::new(self.clone()))
    }

    pub fn alloc(&self) -> Option<AllocSegment> {
        match self {
            ReadSectionKind::RX => Some(AllocSegment::RX),
            ReadSectionKind::RW => Some(AllocSegment::RW),
            ReadSectionKind::ROStrings => Some(AllocSegment::RO),
            ReadSectionKind::ROData => Some(AllocSegment::RO),
            ReadSectionKind::Bss => Some(AllocSegment::RW),
            _ => None,
        }
    }

    pub fn section_index(&self, data: &Data) -> Option<SectionIndex> {
        use ReadSectionKind::*;
        match self {
            RX | RW | ROStrings | ROData | Bss => {
                data.section_index.get(self.section_name()).cloned()
            }
            _ => None,
        }
    }
}

impl ReadSectionKind {
    pub fn new_section_kind(kind: SectionKind) -> Self {
        match kind {
            SectionKind::Text => ReadSectionKind::RX,
            SectionKind::Data => ReadSectionKind::RW,
            SectionKind::ReadOnlyData => ReadSectionKind::ROData,
            SectionKind::ReadOnlyString => ReadSectionKind::ROData,
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
            Self::ROData => ".rodata",
            Self::ROStrings => ".strtab",
            Self::RW => ".data",
            Self::Bss => ".bss",
            _ => unreachable!("Unhandled section: {:?}", self),
        }
    }

    pub fn pointer(&self, address: u64) -> ResolvePointer {
        match self {
            Self::RX | Self::ROStrings | Self::ROData | Self::RW | Self::Bss => {
                ResolvePointer::Section(self.section_name().to_string(), address)
            }
            _ => ResolvePointer::Resolved(address),
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
impl SymbolBind {
    pub fn bind(&self) -> u8 {
        use object::elf;
        match self {
            Self::Local => elf::STB_LOCAL,
            Self::Global => elf::STB_GLOBAL,
            Self::Weak => elf::STB_WEAK,
        }
    }
}

/*
#[derive(Debug, Clone)]
pub enum SymbolLookupTable {
    GOT,
    PLT,
    None,
}
*/

#[derive(Debug, Clone)]
pub struct ReadSymbol {
    pub(crate) name: String,
    pub(crate) name_id: Option<StringId>,
    pub(crate) dyn_name_id: Option<StringId>,
    pub(crate) section: ReadSectionKind,
    pub(crate) source: SymbolSource,
    pub(crate) kind: SymbolKind,
    pub(crate) bind: SymbolBind,
    pub(crate) pointer: ResolvePointer,
    pub(crate) size: u64,
    //lookup: SymbolLookupTable,
}

impl ReadSymbol {
    pub fn from_pointer(name: String, pointer: ResolvePointer) -> Self {
        ReadSymbol {
            name,
            name_id: None,
            dyn_name_id: None,
            section: ReadSectionKind::Undefined,
            source: SymbolSource::Static,
            kind: SymbolKind::Unknown,
            bind: SymbolBind::Local,
            pointer,
            size: 0,
            //lookup: SymbolLookupTable::None,
        }
    }

    pub fn is_static(&self) -> bool {
        self.source == SymbolSource::Static
    }

    pub fn relocate(&mut self, base: u64) {
        self.pointer = self.pointer.clone().relocate(base);
    }

    pub fn get_dynamic_symbol(&self, data: &Data) -> object::write::elf::Sym {
        let name = Some(data.dynamics.string_get(&self.name));
        self._get_symbol(name, 0, 0, None)
    }

    pub fn get_static_symbol(&self, data: &Data) -> object::write::elf::Sym {
        let name = Some(data.statics.string_get(&self.name));
        let st_value = {
            if let Some(addr) = self.pointer.resolve(data) {
                addr
            } else {
                0
            }
        };
        let section = self.section.section_index(data);
        self._get_symbol(name, st_value, self.size, section)
    }

    pub fn _get_symbol(
        &self,
        string_id: Option<StringId>,
        st_value: u64,
        st_size: u64,
        section: Option<SectionIndex>,
    ) -> object::write::elf::Sym {
        use object::elf;

        let stt = match self.kind {
            SymbolKind::Data => elf::STT_OBJECT,
            SymbolKind::Text => elf::STT_FUNC,
            _ => elf::STT_NOTYPE,
        };

        let stb = self.bind.bind();

        let st_info = (stb << 4) + (stt & 0x0f);
        let st_other = elf::STV_DEFAULT;

        object::write::elf::Sym {
            name: string_id,
            section,
            st_info,
            st_other,
            st_shndx: 0,
            st_value,
            st_size,
        }
    }
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

    pub fn build_strings(&mut self, data: &mut Data, w: &mut Writer) {
        // These need to be declared
        let locals = vec![LocalSymbol::new(
            "_DYNAMIC".into(),
            ".dynamic".into(),
            ResolvePointer::Section(".dynamic".to_string(), 0),
            Some(w.add_string("_DYNAMIC".as_bytes())),
            None,
        )];

        for local in locals.iter() {
            data.pointers
                .insert(local.symbol.clone(), local.pointer.clone());
            let symbol = ReadSymbol::from_pointer(local.symbol.clone(), local.pointer.clone());
            self.insert_local(symbol);
        }

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
                    got.insert(r.name.clone());
                }
                PatchEffect::AddToPlt => {
                    gotplt.insert(r.name.clone());
                }
                _ => (),
            }
        }

        for r in iter {
            if let Some(s) = self.lookup(&r.name) {
                // we don't know the section yet, we just know which kind
                let def = match s.bind {
                    SymbolBind::Local => CodeSymbolDefinition::Local,
                    SymbolBind::Global => CodeSymbolDefinition::Defined,
                    SymbolBind::Weak => CodeSymbolDefinition::Defined,
                };

                let assign = match s.kind {
                    SymbolKind::Text => {
                        if s.is_static() {
                            GotPltAssign::Got
                        } else if got.contains(&r.name) {
                            if r.is_plt() {
                                GotPltAssign::GotWithPltGot
                            } else {
                                GotPltAssign::Got
                            }
                        } else if gotplt.contains(&r.name) {
                            GotPltAssign::GotPltWithPlt
                        } else {
                            GotPltAssign::None
                        }
                    }
                    SymbolKind::Data => GotPltAssign::Got,
                    //_ => unimplemented!("{:?}, {}", s, r)
                    _ => GotPltAssign::None,
                };

                if s.source == SymbolSource::Dynamic {
                    eprintln!("reloc {}", &r);
                    data.statics.symbol_add(&s, None, w);
                    data.dynamics.relocation_add(&s, assign, r, w);
                } else if def != CodeSymbolDefinition::Local {
                    eprintln!("reloc2 {}", &r);
                    if assign == GotPltAssign::None {
                    } else {
                        data.dynamics.relocation_add(&s, assign, r, w);
                    }
                } else {
                    eprintln!("reloc3 {}", &r);
                }
            } else {
                unreachable!("Unable to find symbol for relocation: {}", &r.name)
            }
        }

        for (name, pointer) in data.dynamics.symbols() {
            data.pointers.insert(name, pointer);
        }

        eprintln!("plt: {:?}", data.dynamics.plt_hash);
        eprintln!("pltgot: {:?}", data.dynamics.pltgot_hash);

        for (name, symbol) in self.locals.iter() {
            match symbol.section {
                ReadSectionKind::RX
                | ReadSectionKind::ROStrings
                | ReadSectionKind::ROData
                | ReadSectionKind::RW
                | ReadSectionKind::Bss => {
                    data.pointers
                        .insert(name.to_string(), symbol.pointer.clone());
                }
                _ => (),
            }
        }

        for (name, symbol) in self.exports.iter() {
            // allocate string for the symbol table
            //eprintln!("y: {:?}", symbol);
            let _string_id = data.statics.string_add(name, w);
            data.pointers
                .insert(name.to_string(), symbol.pointer.clone());
        }
    }

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

    fn relocate_symbol(&self, mut s: ReadSymbol) -> ReadSymbol {
        let base = match s.section {
            ReadSectionKind::RX => self.rx.section.bytes.len() as u64,
            ReadSectionKind::ROData => self.ro.section.bytes.len() as u64,
            ReadSectionKind::RW => self.rw.section.bytes.len() as u64,
            ReadSectionKind::Bss => self.bss.section.size as u64,
            _ => 0,
        };
        s.relocate(base);
        s
    }

    pub fn merge(renames: &HashMap<String, String>, src: &BlockSection, dst: &mut BlockSection) {
        let base_offset = dst.section.size;
        dst.section.size += src.section.size;
        dst.section.bytes.extend(src.section.bytes.iter());
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
        for (name, s) in block.locals.into_iter() {
            let mut s = self.relocate_symbol(s);
            let unique = format!(".u.{}{}", self.local_index, name);
            s.name = unique.clone();
            self.local_index += 1;
            self.insert_local(s);
            renames.insert(name, unique);
        }

        // exports
        for (_name, s) in block.exports.into_iter() {
            let s = self.relocate_symbol(s);
            //eprintln!("E: {:?}", (&block.name, name, &s));
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

        // update Bss
        //Self::merge(&renames, &block.bss, &mut self.bss);

        // update RX
        Self::merge(&renames, &block.rx, &mut self.rx);
        // update RO
        Self::merge(&renames, &block.ro, &mut self.ro);
        // update RW
        Self::merge(&renames, &block.rw, &mut self.rw);

        //eprintln!("B: {:?}", (&self.rx));
    }

    pub fn from_section<'a, 'b, A: elf::FileHeader, B: object::ReadRef<'a>>(
        &mut self,
        b: &elf::ElfFile<'a, A, B>,
        section: &elf::ElfSection<'a, 'b, A, B>,
    ) -> Result<(), Box<dyn Error>> {
        let kind = ReadSectionKind::new_section_kind(section.kind());
        let base = match kind {
            ReadSectionKind::Bss => self.bss.section.size,
            ReadSectionKind::RX => self.rx.section.size,
            ReadSectionKind::ROData => self.ro.section.size,
            ReadSectionKind::RW => self.rw.section.size,
            _ => unimplemented!(),
        } as u64;

        for symbol in b.symbols() {
            // skip the null symbol
            if symbol.kind() == object::SymbolKind::Null {
                continue;
            }
            if symbol.kind() == object::SymbolKind::File {
                continue;
            }

            if symbol.section_index() == Some(section.index()) {
                let s = read_symbol(&b, base, &symbol)?;
                eprintln!("Read: {:?}", &s);

                if s.bind == SymbolBind::Local {
                    // can't be local and unknown
                    //if symbol.kind() == SymbolKind::Unknown {
                    //unreachable!("{:?}", s);
                    //}
                    self.insert_local(s.clone());
                } else if s.section == ReadSectionKind::Undefined {
                    //block.insert_unknown(s);
                } else {
                    self.insert_export(s.clone());
                }
            }
        }

        match kind {
            ReadSectionKind::Bss => {
                self.bss.from_section(b, section)?;
            }
            ReadSectionKind::RX => {
                self.rx.from_section(b, section)?;
            }
            ReadSectionKind::ROData => {
                self.ro.from_section(b, section)?;
            }
            ReadSectionKind::RW => {
                self.rw.from_section(b, section)?;
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

    pub fn dump(&self) {
        eprintln!("Block: {}", &self.name);

        //let mut dsymbols = HashMap::new();
        let mut rx_symbols = vec![];
        let mut rw_symbols = vec![];
        let mut ro_symbols = vec![];
        let mut strings_symbols = vec![];
        let mut bss_symbols = vec![];
        let mut other_symbols = vec![];

        for (_name, sym) in self.locals.iter().chain(self.exports.iter()) {
            match sym.section {
                ReadSectionKind::RX => rx_symbols.push(sym),
                ReadSectionKind::RW => rw_symbols.push(sym),
                ReadSectionKind::ROData => ro_symbols.push(sym),
                ReadSectionKind::ROStrings => strings_symbols.push(sym),
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

        let symbols = rx_symbols
            .into_iter()
            .map(|s| {
                if let ResolvePointer::Section(_name, address) = &s.pointer {
                    Symbol::new(0, *address, &s.name)
                } else {
                    unreachable!()
                }
            })
            .collect();
        disassemble_code_with_symbols(
            self.rx.section.bytes.as_slice(),
            &symbols,
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

        eprintln!("Strings");
        for local in strings_symbols.iter() {
            eprintln!(" S: {:?}", local);
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
        for symbol in b.dynamic_symbols() {
            let mut s = read_symbol(&b, 0, &symbol)?;
            s.pointer = ResolvePointer::Resolved(0);
            s.source = SymbolSource::Dynamic;
            s.size = 0;
            //eprintln!("s: {:#08x}, {:?}", 0, &s);
            if s.kind != SymbolKind::Unknown {
                self.block.insert_dynamic(s);
            }
        }
        Ok(())
    }

    fn relocatable<'a, 'b, A: elf::FileHeader, B: object::ReadRef<'a>>(
        &mut self,
        name: String,
        b: &elf::ElfFile<'a, A, B>,
    ) -> Result<ReadBlock, Box<dyn Error>> {
        let mut block = ReadBlock::new(&name);

        eprintln!("relocatable: {}", &name);
        /*
        for symbol in b.symbols() {
            // skip the null symbol
            if symbol.kind() == SymbolKind::Null {
                continue;
            }
            if symbol.kind() == SymbolKind::File {
                continue;
            }

            let s = read_symbol(&b, &symbol)?;
            eprintln!("Read: {:?}", &s);

            if s.bind == SymbolBind::Local {
                // can't be local and unknown
                //if symbol.kind() == SymbolKind::Unknown {
                //unreachable!("{:?}", s);
                //}
                block.insert_local(s);
            } else if s.section == ReadSectionKind::Undefined {
                //block.insert_unknown(s);
            } else {
                //let s = block.relocate_symbol(s);
                self.merge_export(s.clone());
                block.insert_export(s);
            }
        }
        */

        for section in b.sections() {
            let kind = ReadSectionKind::new_section_kind(section.kind());
            // skip other kinds
            if kind == ReadSectionKind::Other {
                continue;
            }
            /*
            let mut symbols = vec![];
            for symbol in b.symbols() {
                // skip the null symbol
                if symbol.kind() == SymbolKind::Null {
                    continue;
                }
                if symbol.kind() == SymbolKind::File {
                    continue;
                }

                if symbol.section_index() == Some(section.index()) {
                    let s = read_symbol(&b, &symbol)?;
                    eprintln!("Read: {:?}", &s);

                    if s.bind == SymbolBind::Local {
                        // can't be local and unknown
                        //if symbol.kind() == SymbolKind::Unknown {
                        //unreachable!("{:?}", s);
                        //}
                        block.insert_local(s.clone());
                    } else if s.section == ReadSectionKind::Undefined {
                        //block.insert_unknown(s);
                    } else {
                        //let s = block.relocate_symbol(s);
                        //self.merge_export(s.clone());
                        block.insert_export(s.clone());
                    }
                    symbols.push(s);
                }
            }
            */

            block.from_section(&b, &section)?;
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
            if let Some(_symbol) = self.block.lookup(&r.name) {
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

pub fn read_symbol<'a, 'b, A: elf::FileHeader, B: object::ReadRef<'a>>(
    b: &elf::ElfFile<'a, A, B>,
    base: u64,
    symbol: &elf::ElfSymbol<'a, 'b, A, B>,
) -> Result<ReadSymbol, Box<dyn Error>> {
    let section_kind;

    let address = base + symbol.address();
    let size = symbol.size();

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

    let pointer = section_kind.pointer(address);

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
        pointer,
        size,
        source: SymbolSource::Static,
        //lookup: SymbolLookupTable::None,
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
