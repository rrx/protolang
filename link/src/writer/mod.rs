use std::error::Error;

use object::elf;
use object::read::elf::FileHeader;
use object::write::elf::Sym;
use object::write::elf::{SectionIndex, SymbolIndex, Writer};
use object::write::StringId;
use object::{Architecture, Endianness};
use std::collections::HashMap;
use std::mem;

use super::*;

mod blocks;
mod section;
mod segments;

pub use blocks::*;
pub use section::*;
pub use segments::*;

#[derive(Debug, Clone)]
pub struct ProgramHeaderEntry {
    p_type: u32,
    p_flags: u32,
    p_offset: u64,
    p_vaddr: u64,
    p_paddr: u64,
    p_filesz: u64,
    p_memsz: u64,
    p_align: u64,
}

/*
enum SectionKind {
    Interp,
}

#[derive(Debug)]
struct Section {
    name: Option<object::write::StringId>,
    sh_name: u32, // offset of name in the .shstrtab section
    sh_type: u32, // section header type
    sh_flags: usize,
    sh_addr: usize,
    sh_link: u32,
    sh_info: u32,
    sh_entsize: u32,
    sh_offset: usize,
    sh_addralign: usize,
    data: Vec<u8>,
}

impl Section {
    fn is_alloc(&self) -> bool {
        self.sh_flags & elf::SHF_ALLOC as usize != 0
    }
}
struct Symbol {
    in_sym: usize,
    name: Option<object::write::StringId>,
    section: Option<object::write::elf::SectionIndex>,
}

struct DynamicSymbol {
    in_sym: usize,
    name: Option<object::write::StringId>,
    section: Option<object::write::elf::SectionIndex>,
    hash: Option<u32>,
    gnu_hash: Option<u32>,
}

*/

struct Library {
    name: String,
    string_id: Option<StringId>,
}

struct Dynamic {
    tag: u32,
    // Ignored if `string` is set.
    val: u64,
    string: Option<object::write::StringId>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum AllocSegment {
    Interp,
    RO,
    RW,
    RX,
}

impl AllocSegment {
    pub fn section_header_flags(&self) -> u32 {
        match self {
            AllocSegment::RO | AllocSegment::Interp => elf::SHF_ALLOC,
            AllocSegment::RW => elf::SHF_ALLOC | elf::SHF_WRITE,
            AllocSegment::RX => elf::SHF_ALLOC | elf::SHF_EXECINSTR,
        }
    }
    pub fn program_header_flags(&self) -> u32 {
        match self {
            AllocSegment::RO | AllocSegment::Interp => elf::PF_R,
            AllocSegment::RW => elf::PF_R | elf::PF_W,
            AllocSegment::RX => elf::PF_R | elf::PF_X,
        }
    }
    pub fn align(&self) -> usize {
        0x10
    }
}

pub enum SymbolPointer {
    RX(usize),
    RO(usize),
    RW(usize),
    Bss(usize),
    Got(usize),
    GotPlt(usize),
}

pub struct Data {
    arch: Architecture,
    interp: String,
    pub is_64: bool,
    //pub sections: ProgSections,

    //lib_names: Vec<String>,
    libs: Vec<Library>,
    page_size: u32,
    base: usize,
    addr: HashMap<String, u64>,
    section_index: HashMap<String, SectionIndex>,
    size_dynstr: usize,
    addr_dynsym: u64,
    size_dynsym: usize,
    addr_reladyn: u64,
    size_reladyn: usize,
    addr_hash: u64,
    pointers: HashMap<String, u64>,
    pub block: Option<ReadBlock>,
    add_section_headers: bool,
    add_symbols: bool,
    debug: bool,

    // store strings that we reference their bytes
    // because Data has a long lifetime, we extend
    // the strings bytes to static
    //strings: Vec<String>,
    //unapplied_got: Vec<(Sym, CodeRelocation)>,
    //unapplied_plt: Vec<(Sym, CodeRelocation)>,
    dyn_symbols: HashMap<String, Sym>,
    symbols: HashMap<String, ProgSymbol>,
    pub lookup: HashMap<String, ProgSymbol>,
    locals: Vec<LocalSymbol>,
    dynamic: Vec<LocalSymbol>,
    pub relocations_got: Vec<CodeRelocation>,
    pub relocations_gotplt: Vec<CodeRelocation>,

    pub strings: HashMap<String, (String, StringId)>,
    pub dyn_strings: HashMap<String, (String, StringId)>,

    // index of symbols in got/gotplt
    pub got_index: HashMap<String, usize>,
    pub gotplt_index: HashMap<String, usize>,
}

impl Data {
    pub fn new(lib_names: Vec<String>) -> Self {
        let libs = lib_names
            .iter()
            .map(|name| Library {
                name: name.clone(),
                string_id: None,
            })
            .collect();
        Self {
            arch: Architecture::X86_64,
            is_64: true,
            interp: "/lib64/ld-linux-x86-64.so.2".to_string(),
            //ph: vec![],
            //lib_names,
            block: None,
            //sections: ProgSections::new(),
            libs,
            base: 0x80000,
            page_size: 0x1000,
            addr: HashMap::new(),
            section_index: HashMap::new(),
            size_dynstr: 0,
            addr_dynsym: 0,
            size_dynsym: 0,
            addr_reladyn: 0,
            size_reladyn: 0,
            addr_hash: 0,
            pointers: HashMap::new(),

            add_section_headers: true,
            add_symbols: true,
            debug: true,
            //strings: vec![],
            //unapplied_got: vec![],
            //unapplied_plt: vec![],

            // Tables
            dyn_symbols: HashMap::new(),
            symbols: HashMap::new(),
            lookup: HashMap::new(),
            locals: vec![],
            dynamic: vec![],
            relocations_got: vec![],
            relocations_gotplt: vec![],
            strings: HashMap::new(),
            dyn_strings: HashMap::new(),
            got_index: HashMap::new(),
            gotplt_index: HashMap::new(),
        }
    }

    pub fn is_64(&self) -> bool {
        use object::AddressSize;
        match self.arch.address_size().unwrap() {
            AddressSize::U8 | AddressSize::U16 | AddressSize::U32 => false,
            AddressSize::U64 => true,
            _ => unimplemented!(),
        }
    }

    fn is_dynamic(&self) -> bool {
        self.libs.len() > 0
    }

    pub fn got_index(&mut self, name: &str) -> usize {
        if let Some(index) = self.got_index.get(name) {
            *index
        } else {
            let index = self.got_index.len();
            self.got_index.insert(name.to_string(), index);
            index
        }
    }

    pub fn gotplt_index(&mut self, name: &str) -> usize {
        if let Some(index) = self.gotplt_index.get(name) {
            *index
        } else {
            let index = self.gotplt_index.len();
            self.gotplt_index.insert(name.to_string(), index);
            index
        }
    }

    pub fn string(&mut self, name: &str, w: &mut Writer) -> StringId {
        if let Some(s) = self.strings.get(name) {
            s.1
        } else {
            let name = name.to_string();
            unsafe {
                let buf = extend_lifetime(name.as_bytes());
                let string_id = w.add_string(buf);
                //eprintln!("reserve str: {}, {:?}", &name, string_id);
                self.strings.insert(name.clone(), (name, string_id));
                string_id
            }
        }
    }

    pub fn dyn_string(&mut self, name: &str, w: &mut Writer) -> StringId {
        if let Some(s) = self.dyn_strings.get(name) {
            s.1
        } else {
            let name = name.to_string();
            unsafe {
                let buf = extend_lifetime(name.as_bytes());
                let string_id = w.add_dynamic_string(buf);
                //eprintln!("reserve dyn str: {}, {:?}", &name, string_id);
                self.dyn_strings.insert(name.clone(), (name, string_id));
                string_id
            }
        }
    }

    pub fn pointer_set(&mut self, name: String, p: u64) {
        self.pointers.insert(name, p);
    }

    pub fn pointer_get(&self, name: &str) -> u64 {
        *self
            .pointers
            .get(name)
            .expect(&format!("Pointer not found: {}", name))
    }

    pub fn symbol_set(&mut self, name: String, s: ProgSymbol) {
        self.symbols.insert(name, s);
    }

    pub fn symbol_get<'a>(&'a self, name: &str) -> &'a ProgSymbol {
        self.symbols
            .get(name)
            .expect(&format!("Pointer not found: {}", name))
    }

    pub fn addr_get(&self, name: &str) -> u64 {
        *self
            .addr
            .get(name)
            .expect(&format!("Address not found: {}", name))
    }

    pub fn addr_set(&mut self, name: &str, value: u64) {
        self.addr.insert(name.to_string(), value);
    }

    pub fn section_index_get(&self, name: &str) -> SectionIndex {
        *self
            .section_index
            .get(name)
            .expect(&format!("Section Index not found: {}", name))
    }

    pub fn section_index_set(&mut self, name: &str, section_index: SectionIndex) {
        self.section_index.insert(name.to_string(), section_index);
    }

    fn gen_dynamic(&self) -> Vec<Dynamic> {
        let mut out = vec![];
        for lib in self.libs.iter() {
            out.push(Dynamic {
                tag: elf::DT_NEEDED,
                val: 0,
                string: lib.string_id,
            });
        }
        out.push(Dynamic {
            tag: elf::DT_HASH,
            val: self.addr_hash,
            string: None,
        });
        out.push(Dynamic {
            tag: elf::DT_STRTAB,
            val: self.addr_get(".dynstr"),
            string: None,
        });
        out.push(Dynamic {
            tag: elf::DT_SYMTAB,
            val: self.addr_dynsym,
            string: None,
        });
        out.push(Dynamic {
            tag: elf::DT_RELA,
            val: self.addr_reladyn,
            string: None,
        });
        out.push(Dynamic {
            tag: elf::DT_RELASZ,
            val: self.size_reladyn as u64,
            string: None,
        });
        out.push(Dynamic {
            tag: elf::DT_RELAENT,
            val: self.rel_size(true) as u64,
            string: None,
        });
        out.push(Dynamic {
            tag: elf::DT_STRSZ,
            val: self.size_dynstr as u64,
            string: None,
        });
        out.push(Dynamic {
            tag: elf::DT_SYMENT,
            val: self.symbol_size() as u64,
            string: None,
        });
        out.push(Dynamic {
            tag: elf::DT_DEBUG,
            val: 0,
            string: None,
        });

        out.push(Dynamic {
            tag: elf::DT_NULL,
            val: 0,
            string: None,
        });
        out
    }

    pub fn symbol_size(&self) -> usize {
        if self.is_64 {
            mem::size_of::<elf::Sym64<Endianness>>()
        } else {
            mem::size_of::<elf::Sym32<Endianness>>()
        }
    }

    pub fn rel_size(&self, is_rela: bool) -> usize {
        if self.is_64 {
            if is_rela {
                mem::size_of::<elf::Rela64<Endianness>>()
            } else {
                mem::size_of::<elf::Rel64<Endianness>>()
            }
        } else {
            if is_rela {
                mem::size_of::<elf::Rela32<Endianness>>()
            } else {
                mem::size_of::<elf::Rel32<Endianness>>()
            }
        }
    }
}

/*
pub struct ProgSections {
    sections: Vec<ProgSection>,
    pub unapplied_got: Vec<CodeRelocation>,
    pub unapplied_plt: Vec<CodeRelocation>,
    //dyn_symbols: HashMap<String, Sym>,
}
impl ProgSections {
    pub fn new() -> Self {
        Self {
            sections: vec![],
            unapplied_got: vec![],
            unapplied_plt: vec![],
            //dyn_symbols: HashMap::new(),
        }
    }
    pub fn add(&mut self, section: ProgSection) {
        self.sections.push(section);
    }

    pub fn extern_symbol_pointers(&self) -> HashMap<String, ProgSymbol> {
        let mut pointers = HashMap::new();
        for section in &self.sections {
            for (name, s) in &section.externs {
                pointers.insert(name.clone(), s.clone());
            }
        }
        pointers
    }

    pub fn symbol_pointers(&self) -> HashMap<String, ProgSymbol> {
        let mut pointers = HashMap::new();
        //for (name, s) in &self.dyn_symbols {
        //pointers.insert(name.clone(), s.clone());
        //}
        for section in &self.sections {
            for (name, s) in &section.symbols {
                pointers.insert(name.clone(), s.clone());
            }
        }
        pointers
    }
}
*/

pub unsafe fn extend_lifetime<'b>(r: &'b [u8]) -> &'static [u8] {
    std::mem::transmute::<&'b [u8], &'static [u8]>(r)
}

/*
pub fn unapplied_relocations<'a>(data: &mut Data, w: &mut Writer) {
    let symbols = data.sections.symbol_pointers();
    let externs = data.sections.extern_symbol_pointers();
    for section in data.sections.sections.iter() {
        for (symbol, mut r) in section
            .unapplied_relocations(&symbols, &externs)
            .into_iter()
        {
            let buf = r.name.as_bytes();
            unsafe {
                r.name_id = Some(w.add_dynamic_string(extend_lifetime(buf)));
            }
            let sym = Sym {
                name: r.name_id,
                section: None,
                st_info: symbol.s.st_info,
                st_other: symbol.s.st_other,
                st_shndx: 0,
                st_value: 0,
                st_size: 0,
            };
            eprintln!("s: {:?}, {}", &symbol, r);
            data.dyn_symbols.insert(r.name.clone(), sym.clone());
            match r.effect() {
                PatchEffect::AddToGot => {
                    eprintln!("unapp data: {:?}", &sym);
                    data.sections.unapplied_got.push(r);
                }
                PatchEffect::AddToPlt => {
                    eprintln!("unapp text: {:?}", &sym);
                    data.sections.unapplied_plt.push(r);
                }
                PatchEffect::DoNothing => (), //_ => unreachable!(),
            }
        }
    }

    for u in data.sections.unapplied_plt.iter() {
        eprintln!("R-PLT: {:?}", u);
    }

    for u in data.sections.unapplied_got.iter() {
        eprintln!("R-GOT: {:?}", u);
    }
}
*/

/*
pub fn load_sections<'a>(data: &mut Data, link: &'a Link, w: &mut Writer<'a>) {
    let mut ro = vec![];
    let mut rw = vec![];
    let mut rx = vec![];

    for (_name, unlinked) in link.unlinked.iter() {
        use object::SectionKind as K;
        match unlinked.kind {
            K::Data | K::UninitializedData => {
                rw.push(unlinked);
            }
            K::ReadOnlyString | K::ReadOnlyData => {
                eprintln!("X:{:?}", (&unlinked.name, &unlinked.kind));
                ro.push(unlinked);
            }
            K::Text => {
                rx.push(unlinked);
            }

            // ignore for now
            K::Metadata => (),
            K::Other => (),
            K::Note => (),
            // OtherString is usually comments, we can drop these
            K::OtherString => (),
            K::Elf(_x) => {
                // ignore
                //unimplemented!("Elf({:#x})", x);
            }
            _ => unimplemented!("Unlinked kind: {:?}", unlinked.kind),
        }
    }

    if rx.len() > 0 {
        let name = ".text".to_string();
        let name_id = Some(w.add_section_name(".text".as_bytes()));
        let mut section = ProgSection::new(AllocSegment::RX, Some(name), name_id, 0);
        for u in rx.into_iter() {
            section.append(&u, w);
        }
        data.sections.add(section);
    }

    if ro.len() > 0 {
        let name = ".rodata".to_string();
        let name_id = Some(w.add_section_name(".rodata".as_bytes()));
        let mut section = ProgSection::new(AllocSegment::RO, Some(name), name_id, 0);
        for u in ro.into_iter() {
            section.append(&u, w);
        }
        data.sections.add(section);
    }

    if rw.len() > 0 {
        let name = ".data".to_string();
        let name_id = Some(w.add_section_name(".data".as_bytes()));
        let mut section = ProgSection::new(AllocSegment::RW, Some(name), name_id, 0);
        for u in rw.into_iter() {
            section.append(&u, w);
        }
        data.sections.add(section);
    }
}
*/

/*
fn update_symbols(locals: &Vec<LocalSymbol>, data: &mut Data, _tracker: &mut SegmentTracker) {
    for local in locals.iter() {
        let addr = data.addr_get(&local.section) + local.offset as u64;
        // Add symbol
        let st_info = (elf::STB_LOCAL << 4) + (elf::STT_OBJECT & 0x0f);
        let st_other = elf::STV_DEFAULT;
        //let st_shndx = 0;
        //let st_value = addr;
        let st_size = 0;

        let section_index = data.section_index.get(&local.section).cloned();

        let p = ProgSymbol {
            name_id: local.string_id,
            section_index,
            base: 0,
            s: CodeSymbol {
                name: local.symbol.clone(),
                size: st_size,
                address: addr,
                kind: CodeSymbolKind::Data,
                def: CodeSymbolDefinition::Defined,
                st_info,
                st_other,
            },
        };

        data.symbols.insert(local.symbol.clone(), p.clone());
        data.lookup.insert(local.symbol.clone(), p);
    }
}
*/

/*
pub fn write_file<Elf: FileHeader<Endian = Endianness>>(
    link: &Link,
    mut data: Data,
) -> std::result::Result<Vec<u8>, Box<dyn Error>> {
    let mut out_data = Vec::new();
    let endian = Endianness::Little;
    let mut writer = object::write::elf::Writer::new(endian, data.is_64, &mut out_data);

    load_sections(&mut data, link, &mut writer);
    unapplied_relocations(&mut data, &mut writer);
    write_file_main::<Elf>(&mut data, &mut writer)?;
    Ok(out_data)
}
*/

pub fn write_file_main<Elf: object::read::elf::FileHeader<Endian = Endianness>>(
    data: &mut Data,
    w: &mut Writer,
) -> std::result::Result<(), Box<dyn Error>> {
    // add libraries if they are configured
    for mut lib in data.libs.iter_mut() {
        unsafe {
            let buf = extend_lifetime(lib.name.as_bytes());
            lib.string_id = Some(w.add_dynamic_string(buf));
        }
    }

    //let (mut blocks, maybe_block) = BlocksBuilder::new().build(data, w);
    let mut block = data.block.take().unwrap();
    let mut blocks = BlocksBuilder::new().build(data, w, &mut block);
    blocks.build(data, w, &mut block);
    Ok(())
}

/// align size
pub fn size_align(n: usize, align: usize) -> usize {
    return (n + (align - 1)) & !(align - 1);
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;
    use test_log::test;

    #[test]
    fn write_empty_main() {
        let mut b = Link::new();
        b.add_obj_file("test", Path::new("../tmp/empty_main.o"))
            .unwrap();
        //b.write(Path::new("../tmp/out.exe")).unwrap();
    }
}
