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

use blocks::*;
use section::*;
use segments::*;

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
    string_id: StringId,
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

pub struct Data {
    arch: Architecture,
    interp: String,
    is_64: bool,
    //ph: Vec<ProgramHeaderEntry>,
    sections: ProgSections,
    lib_names: Vec<String>,
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
    symbols: HashMap<String, ProgSymbol>,

    add_section_headers: bool,
    add_symbols: bool,
    debug: bool,
}

impl Data {
    pub fn new(lib_names: Vec<String>) -> Self {
        Self {
            arch: Architecture::X86_64,
            is_64: true,
            interp: "/lib64/ld-linux-x86-64.so.2".to_string(),
            //ph: vec![],
            lib_names,
            sections: ProgSections::new(),
            libs: vec![],
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
            symbols: HashMap::new(),

            add_section_headers: true,
            add_symbols: true,
            debug: true,
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

    fn add_library(&mut self, string_id: StringId) {
        self.libs.push(Library { string_id });
    }

    fn add_dynstr(&mut self, string_id: StringId) {
        self.libs.push(Library { string_id });
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
        *self.section_index.get(name).unwrap()
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
                string: Some(lib.string_id),
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

    /*
    pub fn debug(&mut self, _link: &Link) {
        let mut out_data = Vec::new();
        let endian = Endianness::Little;
        let _writer = object::write::elf::Writer::new(endian, self.is_64, &mut out_data);

        // load bytes and relocations
        //self.segments.load(link, &mut writer);
        //self.segments.load(link);
        //self.segments.read_unlinked(link, &mut writer);

        //self.segments.rx.debug();
        //
    }
    */

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

pub struct ProgSections {
    sections: Vec<ProgSection>,
    unapplied_got: Vec<(Sym, CodeRelocation)>,
    unapplied_plt: Vec<(Sym, CodeRelocation)>,
    dynsymbols: Vec<SymbolIndex>,
}
impl ProgSections {
    pub fn new() -> Self {
        Self {
            sections: vec![],
            unapplied_got: vec![],
            unapplied_plt: vec![],
            dynsymbols: vec![],
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
        for section in &self.sections {
            for (name, s) in &section.symbols {
                pointers.insert(name.clone(), s.clone());
            }
        }
        pointers
    }
}

unsafe fn extend_lifetime<'b>(r: &'b [u8]) -> &'static [u8] {
    std::mem::transmute::<&'b [u8], &'static [u8]>(r)
}

pub fn unapplied_relocations<'a>(sections: &mut ProgSections, w: &mut Writer) {
    let symbols = sections.symbol_pointers();
    let externs = sections.extern_symbol_pointers();
    for section in sections.sections.iter() {
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
            match r.effect() {
                PatchEffect::AddToGot => {
                    eprintln!("unapp data: {:?}", &sym);
                    sections.unapplied_got.push((sym, r));
                }
                PatchEffect::AddToPlt => {
                    eprintln!("unapp text: {:?}", &sym);
                    sections.unapplied_plt.push((sym, r));
                }
                PatchEffect::DoNothing => (), //_ => unreachable!(),
            }
        }
    }

    for (sym, u) in sections.unapplied_plt.iter() {
        eprintln!("R-PLT: {:?}", (sym, u));
    }

    for (sym, u) in sections.unapplied_got.iter() {
        eprintln!("R-GOT: {:?}", (sym, u));
    }
}

pub fn load_blocks<'a>(blocks: &mut Blocks, data: &mut Data) {
    for section in data.sections.sections.drain(..) {
        let buf = section.bytes.clone();
        if section.relocations.len() > 0 {
            //blocks.add_block(Box::new(RelocationSection::new(section.kind, &section)));
        }

        let block = BufferSection::new(
            AllocSegment::RX,
            section.name.clone(),
            section.name_id,
            buf,
            section,
        );
        blocks.add_block(Box::new(block));
    }
}

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

    unapplied_relocations(&mut data.sections, w);
}

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

        data.symbols.insert(local.symbol.clone(), p);
    }
}

pub fn write_file<Elf: FileHeader<Endian = Endianness>>(
    link: &Link,
    mut data: Data,
) -> std::result::Result<Vec<u8>, Box<dyn Error>> {
    let mut out_data = Vec::new();
    let endian = Endianness::Little;
    let mut writer = object::write::elf::Writer::new(endian, data.is_64, &mut out_data);

    // add libraries if they are configured
    let lib_names = data.lib_names.clone();
    for lib_name in lib_names.iter() {
        let string_id = writer.add_dynamic_string(lib_name.as_bytes());
        data.add_library(string_id);
    }

    load_sections(&mut data, link, &mut writer);

    let locals = vec![
        LocalSymbol::new(
            "_DYNAMIC".into(),
            ".dynamic".into(),
            0,
            Some(writer.add_string("_DYNAMIC".as_bytes())),
        ),
        LocalSymbol::new(
            "_GLOBAL_OFFSET_TABLE_".into(),
            ".got.plt".into(),
            0,
            Some(writer.add_string("_GLOBAL_OFFSET_TABLE_".as_bytes())),
        ),
        /*
        LocalSymbol::new(
            "ASDF".into(),
            ".got.plt".into(),
            0,
            Some(writer.add_string("ASDF".as_bytes())),
        ),
        */
    ];

    // configure blocks
    // these are used to correctly order the reservation of space
    // and to write things out in the correct order
    let mut blocks = Blocks::new();
    blocks.add_block(Box::new(HeaderComponent::default()));

    if data.is_dynamic() {
        // BufferSection doesn't implement the program header, we really need
        // the dedicated interp section code to make that work
        // interp is an exception
        blocks.add_block(Box::new(InterpSection::new(&data)));
    }

    blocks.add_block(Box::new(HashSection::default()));

    if writer.dynstr_needed() {
        blocks.add_block(Box::new(DynStrSection::default()));
    }
    if data.is_dynamic() {
        blocks.add_block(Box::new(RelaDynSection::new(GotKind::GOT)));
        blocks.add_block(Box::new(RelaDynSection::new(GotKind::GOTPLT)));
        blocks.add_block(Box::new(DynSymSection::default()));
    }

    blocks.add_block(Box::new(PltSection::new()));
    load_blocks(&mut blocks, &mut data);

    if data.is_dynamic() {
        blocks.add_block(Box::new(DynamicSection::default()));
        //blocks.add_block(Box::new(GotPltSection::default()));
        blocks.add_block(Box::new(GotSection::new(GotKind::GOT)));
        blocks.add_block(Box::new(GotSection::new(GotKind::GOTPLT)));
    }

    if data.add_symbols {
        blocks.add_block(Box::new(SymTabSection::default()));
    }

    if data.add_symbols && writer.strtab_needed() {
        blocks.add_block(Box::new(StrTabSection::default()));
    }

    // shstrtab needs to be allocated last, once all headers are reserved
    if data.add_symbols {
        blocks.add_block(Box::new(ShStrTabSection::default()));
    }

    let mut tracker = SegmentTracker::new(data.base);

    // RESERVE
    // section headers are optional
    if data.add_section_headers {
        blocks.reserve_section_index(&mut data, &mut writer);
    }

    // what are these for? reserving symbols for locals
    for _ in locals.iter() {
        writer.reserve_symbol_index(data.section_index.get(&".dynamic".to_string()).cloned());
    }

    blocks.reserve(&mut tracker, &mut data, &mut writer);

    if data.add_section_headers {
        writer.reserve_section_headers();
    }

    for (k, v) in data.pointers.iter() {
        eprintln!("P: {}, {:#0x}", k, v);
    }

    // UPDATE
    tracker.ph = blocks.program_headers(&tracker);
    blocks.update(&mut data);

    update_symbols(&locals, &mut data, &mut tracker);

    // WRITE
    blocks.write(&data, &mut tracker, &mut writer);

    // SECTION HEADERS
    if data.add_section_headers {
        blocks.write_section_headers(&data, &tracker, &mut writer);
    }

    //data.debug(link);
    Ok(out_data)
}

/// align size
fn size_align(n: usize, align: usize) -> usize {
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
        b.write(Path::new("../tmp/out.exe")).unwrap();
    }
}
