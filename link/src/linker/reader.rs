// read elf file
use object::elf::FileHeader64;
use object::read::elf;
use object::read::elf::ProgramHeader;
use object::{Object, ObjectSection, ObjectSymbol, SymbolKind, RelocationTarget,
ObjectKind};
use std::error::Error;
use std::collections::{HashSet, HashMap};

use crate::disassemble::*;
use crate::relocations::*;

#[derive(Debug, Default)]
pub struct Reader {
    symbols: HashMap<String, ReadSymbol>,
    unknowns: HashMap<String, ReadSymbol>,
    got: HashSet<String>,
    plt: HashSet<String>,
}

impl Reader {
    pub fn add(&mut self, path: &std::path::Path) -> Result<(), Box<dyn Error>> {
        let buf = std::fs::read(path)?;
        self.elf_read(&buf)?;
        Ok(())
    }

    pub fn insert_unknown(&mut self, s: ReadSymbol) {
        self.unknowns.insert(s.name.clone(), s);
    }
    pub fn insert_symbol(&mut self, s: ReadSymbol) {
        // don't insert Unknowns
        //if s.kind == SymbolKind::Unknown {
            //self.unknowns.insert(s.name.clone(), s);
            //return;
        //}

        // if we have two strong symbols, favor the first
        // if we have two weak symbols, favor the first
        // if we already have a weak symbol, and a strong one comes next, override
        // if we have a strong, and a weak follows, we ignore the weak
        if let Some(existing) = self.symbols.get(&s.name) {
            use SymbolBind::*;
            match (&existing.bind, &s.bind) {
                (Weak, Weak) => (),
                (Global, Global) => (),
                (Weak, Global) => {
                    self.symbols.insert(s.name.clone(), s);
                }
                (Global, Weak) => (),
                (_, _) => (),
            }
        } else {
            self.symbols.insert(s.name.clone(), s);
        }
    }

    fn elf_read(&mut self, buf: &[u8]) -> Result<(), Box<dyn Error>> {
        let b: elf::ElfFile<'_, FileHeader64<object::Endianness>> =
            object::read::elf::ElfFile::parse(buf)?;
        match b.kind() {
            ObjectKind::Relocatable => self.relocatable(&b),
            ObjectKind::Dynamic => self.dynamic(&b),
            _ => unimplemented!()
        }
    }

    fn dynamic<'a, 'b, A: elf::FileHeader, B: object::ReadRef<'a>>(&mut self, b: &elf::ElfFile<'a, A, B>) -> Result<(), Box<dyn Error>> {
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
                self.insert_symbol(s);
            }
            //out.push(s);
        }
        Ok(())
    }

    fn relocatable<'a, 'b, A: elf::FileHeader, B: object::ReadRef<'a>>(&mut self, b: &elf::ElfFile<'a, A, B>) -> Result<(), Box<dyn Error>> {
        //let lib = libloading::Library::new("libc.so")?;

        //let mut got = HashSet::new();
        //let mut plt = HashSet::new();
        for section in b.sections() {
            for (offset, r) in section.relocations() {
                let r: LinkRelocation = r.into();
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
                    _ => unreachable!()
                };

                match r.effect() {
                    PatchEffect::AddToPlt => {
                        self.plt.insert(name.clone());
                    }
                    PatchEffect::AddToGot => {
                        self.got.insert(name.clone());
                    }
                    PatchEffect::DoNothing => ()
                }

                eprintln!("r: {}, {:#08x}, {:?}, {:?}", &name, offset, r, r.effect());
            }
        }

        for symbol in b.symbols() {
            // skip the null symbol
            if symbol.kind() == SymbolKind::Null {
                continue;
            }
            if symbol.kind() == SymbolKind::File {
                continue;
            }


            let mut s = read_symbol(&b, &symbol)?;

            if symbol.is_undefined() {
                //let s = lib.get(name.as_bytes())?;
                //let func: libloading::Symbol<unsafe extern fn() -> u32> = lib.get(name.as_bytes())?;
            }

            // we only need one lookup, and we default to GOT
            let lookup = if self.got.contains(&s.name) {
                SymbolLookupTable::GOT
            } else if self.plt.contains(&s.name) {
                SymbolLookupTable::PLT
            } else {
                SymbolLookupTable::None
            };
            s.lookup = lookup;

            if s.kind == SymbolKind::Unknown {
                self.insert_unknown(s);
            } else {
                self.insert_symbol(s);
            }
        }
        Ok(())
    }


    pub fn dump(&mut self) {
        let mut symbols = self.symbols.values().filter(|s| {
            s.source == SymbolSource::Static
        }).cloned().collect::<Vec<_>>();

        for s in symbols.iter_mut() {
            //if s.kind == SymbolKind::Unknown {
            //} else {
            eprintln!("D: {:?}", s);
            //}
        }

        for s in self.unknowns.values() {
            if let Some(shared) = self.symbols.get(&s.name) {
                eprintln!("U: Found {:?}, {:?}", &s, &shared);
                //s.kind = shared.kind;
            } else {
                eprintln!("U: Not Found {:?}", s);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolSource {
    Dynamic,
    Static,
}

#[derive(Debug, Clone)]
pub enum SymbolBind {
    Local,
    Global,
    Weak
}

#[derive(Debug, Clone)]
pub enum SymbolLookupTable {
    GOT,
    PLT,
    None
}


#[derive(Debug, Clone)]
pub struct ReadSymbol {
    name: String,
    source: SymbolSource,
    kind: SymbolKind,
    bind: SymbolBind,
    address: u64,
    size: u64,
    lookup: SymbolLookupTable,
}

fn read_symbol<'a, 'b, A: elf::FileHeader, B: object::ReadRef<'a>>(b: &elf::ElfFile<'a, A, B>, symbol: &elf::ElfSymbol<'a, 'b, A, B>) -> Result<ReadSymbol, Box<dyn Error>> {
    let name = if symbol.kind() == SymbolKind::Section {
        let section = b.section_by_index(symbol.section_index().unwrap())?;
        section.name()?.to_string()
    } else {
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
        kind: symbol.kind(),
        bind,
        address,
        size,
        source: SymbolSource::Static,
        lookup: SymbolLookupTable::None
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
