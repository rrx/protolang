// read elf file
use object::elf::FileHeader64;
use object::read::elf;
use object::read::elf::ProgramHeader;
use object::{
    Object, ObjectKind, ObjectSection, ObjectSymbol, RelocationTarget, SectionKind, SymbolKind,
};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::path::Path;

use crate::disassemble::*;
use crate::relocations::*;

pub type SymbolMap = HashMap<String, ReadSymbol>;

#[derive(Debug, Default)]
pub struct Reader {
    // blocks
    blocks: Vec<ReadBlock>,

    // link block
    block: ReadBlock,

    got: HashSet<String>,
    plt: HashSet<String>,
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
    name: String,
    section: ReadSectionKind,
    source: SymbolSource,
    kind: SymbolKind,
    bind: SymbolBind,
    address: u64,
    size: u64,
    lookup: SymbolLookupTable,
}

#[derive(Debug, Default)]
pub struct ReadBlock {
    name: String,
    // dynamic libraries referenced
    libs: HashSet<String>,
    local_index: usize,
    locals: SymbolMap,
    exports: SymbolMap,
    dynamic: SymbolMap,
    unknown: SymbolMap,
    ro: Vec<u8>,
    ro_relocations: Vec<CodeRelocation>,
    rw: Vec<u8>,
    rw_relocations: Vec<CodeRelocation>,
    rx: Vec<u8>,
    rx_relocations: Vec<CodeRelocation>,
    bss: usize,
    bss_relocations: Vec<CodeRelocation>,
}

impl ReadBlock {
    pub fn write(&mut self, path: &Path) -> Result<(), Box<dyn Error>> {
        use object::elf;
        use object::Endianness;
        let data = crate::writer::Data::new(self.libs.iter().cloned().collect());
        let out_data =
            crate::writer::write_file_block::<elf::FileHeader64<Endianness>>(self, data)?;
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

    fn relocate_symbol(&self, s: &mut ReadSymbol) {
        use ReadSectionKind::*;
        let addr = match s.section {
            RX => s.address + self.rx.len() as u64,
            RO => s.address + self.ro.len() as u64,
            RW => s.address + self.rw.len() as u64,
            Bss => s.address + self.bss as u64,
            _ => 0,
        };
        s.address = addr;
    }

    pub fn add_block(&mut self, block: ReadBlock) {
        let mut renames = HashMap::new();
        for (name, mut s) in block.locals.into_iter() {
            self.relocate_symbol(&mut s);
            let unique = format!(".u.{}{}", self.local_index, name);
            s.name = unique.clone();
            self.local_index += 1;
            self.insert_local(s);
            renames.insert(name, unique);
        }

        for (_name, mut s) in block.exports.into_iter() {
            self.relocate_symbol(&mut s);
            eprintln!("E: {:?}", &s);
            self.insert_export(s);
        }

        let base_offset = self.bss;
        self.bss += block.bss;
        for mut r in block.bss_relocations.into_iter() {
            r.offset += base_offset as u64;
            if let Some(name) = renames.get(&r.name) {
                r.name = name.clone();
            }
            self.bss_relocations.push(r);
        }

        let base_offset = self.rx.len();
        self.rx.extend(block.rx);
        for mut r in block.rx_relocations.into_iter() {
            r.offset += base_offset as u64;
            if let Some(name) = renames.get(&r.name) {
                r.name = name.clone();
            }
            self.rx_relocations.push(r);
        }

        let base_offset = self.ro.len();
        self.ro.extend(block.ro);
        for mut r in block.ro_relocations.into_iter() {
            r.offset += base_offset as u64;
            if let Some(name) = renames.get(&r.name) {
                r.name = name.clone();
            }
            self.ro_relocations.push(r);
        }

        let base_offset = self.rw.len();
        self.rw.extend(block.rw);
        for mut r in block.rw_relocations.into_iter() {
            r.offset += base_offset as u64;
            if let Some(name) = renames.get(&r.name) {
                r.name = name.clone();
            }
            self.rw_relocations.push(r);
        }
    }

    pub fn new(name: &str) -> Self {
        let mut block = Self::default();
        block.name = name.to_string();
        block
    }

    pub fn from_section<'a, 'b, A: elf::FileHeader, B: object::ReadRef<'a>>(
        &mut self,
        b: &elf::ElfFile<'a, A, B>,
        section: &elf::ElfSection<'a, 'b, A, B>,
    ) -> Result<(), Box<dyn Error>> {
        let kind = ReadSectionKind::new_section_kind(section.kind());

        for symbol in b.symbols() {
            // skip the null symbol
            if symbol.kind() == SymbolKind::Null {
                continue;
            }
            if symbol.kind() == SymbolKind::File {
                continue;
            }

            let s = read_symbol(&b, &symbol)?;

            if s.section == kind {
                eprintln!("S: {:?}", &s);

                if s.kind == SymbolKind::Unknown {
                    self.insert_unknown(s);
                } else {
                    match s.bind {
                        SymbolBind::Local => self.insert_local(s),
                        _ => self.insert_export(s),
                    }
                }
            }
        }

        //let mut s = ReadSection::new(kind);
        match kind {
            ReadSectionKind::Bss => {
                let base_offset = self.bss;
                self.bss += section.size() as usize;
                for (offset, r) in section.relocations() {
                    let r = code_relocation(b, r.into(), base_offset + offset as usize)?;
                    self.bss_relocations.push(r);
                }
            }
            ReadSectionKind::RX => {
                let data = section.uncompressed_data()?;
                let base_offset = self.rx.len();
                self.rx.extend(data.iter());
                for (offset, r) in section.relocations() {
                    let r = code_relocation(b, r.into(), base_offset + offset as usize)?;
                    self.rx_relocations.push(r);
                }
            }
            ReadSectionKind::RO => {
                let data = section.uncompressed_data()?;
                let base_offset = self.ro.len();
                self.ro.extend(data.iter());
                for (offset, r) in section.relocations() {
                    let r = code_relocation(b, r.into(), base_offset + offset as usize)?;
                    self.ro_relocations.push(r);
                }
            }
            ReadSectionKind::RW => {
                let data = section.uncompressed_data()?;
                let base_offset = self.rw.len();
                self.rw.extend(data.iter());
                for (offset, r) in section.relocations() {
                    let r = code_relocation(b, r.into(), base_offset + offset as usize)?;
                    self.rw_relocations.push(r);
                }
            }
            _ => unimplemented!(),
        }

        /*
        if kind == ReadSectionKind::Bss {
            s.bss = section.size() as usize;
        } else {
            let data = section.uncompressed_data()?;
            s.bytes.extend(data.iter());
        }
        */
        Ok(())
    }

    pub fn lookup(&self, name: &str) -> Option<ReadSymbol> {
        if let Some(symbol) = self.locals.get(name) {
            Some(symbol.clone())
        } else if let Some(symbol) = self.exports.get(name) {
            Some(symbol.clone())
        } else if let Some(symbol) = self.dynamic.get(name) {
            Some(symbol.clone())
        } else {
            None
        }
    }

    pub fn dump(&self) {
        eprintln!("Block: {}", &self.name);

        let mut dsymbols = HashMap::new();
        let mut rx_symbols = vec![];
        let mut rw_symbols = vec![];
        let mut ro_symbols = vec![];
        let mut bss_symbols = vec![];
        for (name, s) in self.locals.iter().chain(self.exports.iter()) {
            eprintln!(" S: {:?}", s);
            dsymbols.insert(name, Symbol::new(0, s.address, &s.name));
            let sym = Symbol::new(0, s.address, &s.name);
            match s.section {
                ReadSectionKind::RX => rx_symbols.push(sym),
                ReadSectionKind::RW => rw_symbols.push(sym),
                ReadSectionKind::RO => ro_symbols.push(sym),
                ReadSectionKind::Bss => bss_symbols.push(sym),
                _ => (),
            }
        }
        eprintln!("RX");

        /*
        for r in self.rx_relocations.iter() {
            if let Some(symbol) = dsymbols.get(&r.name) {
                eprintln!(" R: {:?}", (r, symbol));
            } else if let Some(symbol) = self.locals.get(&r.name) {
                eprintln!(" R Local: {:?}", (r, symbol));
            } else if let Some(symbol) = self.exports.get(&r.name) {
                eprintln!(" R export: {:?}", (r, symbol));
            } else {
                //notfound.insert(&r.name);
                //unreachable!(" Not found: {}", r.name);
            }
        }
        */
        disassemble_code_with_symbols(self.rx.as_slice(), &rx_symbols, &self.rx_relocations);

        eprintln!("RO, size: {:#0x}", self.ro.len());
        for local in ro_symbols.iter() {
            eprintln!(" S: {:?}", local);
        }
        for r in self.ro_relocations.iter() {
            eprintln!(" R: {}", r);
        }
        print_bytes(self.ro.as_slice(), 0);

        eprintln!("RW, size: {:#0x}", self.rw.len());
        for local in rw_symbols.iter() {
            eprintln!(" S: {:?}", local);
        }
        print_bytes(self.rw.as_slice(), 0);

        eprintln!("Bss, size: {:#0x}", self.bss);
        for local in bss_symbols.iter() {
            eprintln!(" S: {:?}", local);
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
        let mut block = ReadBlock::default();
        block.name = name;

        for symbol in b.symbols() {
            // skip the null symbol
            if symbol.kind() == SymbolKind::Null {
                continue;
            }
            if symbol.kind() == SymbolKind::File {
                continue;
            }

            let s = read_symbol(&b, &symbol)?;

            if s.bind == SymbolBind::Local {
                // can't be local and unknown
                if symbol.kind() == SymbolKind::Unknown {
                    unreachable!();
                }
                block.insert_local(s);
            } else if s.section == ReadSectionKind::Undefined {
                block.insert_unknown(s);
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
        let mut notfound = HashSet::new();
        let iter = self
            .block
            .rx_relocations
            .iter()
            .chain(self.block.ro_relocations.iter())
            .chain(self.block.rw_relocations.iter())
            .chain(self.block.bss_relocations.iter());

        for r in iter {
            if let Some(symbol) = self.block.lookup(&r.name) {
                //eprintln!(" R: {:?}", (r, symbol));
            } else {
                notfound.insert(&r.name);
            }
        }

        for s in notfound {
            eprintln!("NotFound: {}", s);
        }

        self.block
    }
}

fn print_bytes(buf: &[u8], base: usize) {
    let N = 16;
    let chunks = buf.chunks(N).collect::<Vec<_>>();
    let mut offset = base;
    for c in chunks.iter() {
        let numbers = c
            .iter()
            .map(|b| format!("{:02x}", *b))
            .collect::<Vec<_>>()
            .join(" ");
        let x = c
            .iter()
            .map(|b| {
                if b.is_ascii_alphanumeric() {
                    *b
                } else {
                    '.' as u8
                }
            })
            .collect::<Vec<_>>();
        let x = String::from_utf8(x).unwrap();
        eprintln!(" {:#08x}: {}  {}", offset, numbers, x);
        offset += N;
    }
}

fn code_relocation<'a, 'b, A: elf::FileHeader, B: object::ReadRef<'a>>(
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
