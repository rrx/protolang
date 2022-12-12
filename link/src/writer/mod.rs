use std::error::Error;

use object::elf;
use object::read::elf::FileHeader;
use object::write::elf::{SectionIndex, Writer};
use object::write::StringId;
use object::Endianness;
use std::collections::HashMap;

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
    interp: String,
    is_64: bool,
    ph: Vec<ProgramHeaderEntry>,
    lib_names: Vec<String>,
    libs: Vec<Library>,
    page_size: u32,
    base: usize,
    //segments: Segments,
    //tracker: SegmentTracker,
    addr_dynamic: u64,
    addr_dynstr: u64,
    addr_dynsym: u64,
    //addr_interp: u64,
    index_strtab: Option<SectionIndex>,
    index_symtab: Option<SectionIndex>,
    index_dynstr: Option<SectionIndex>,
    index_dynsym: Option<SectionIndex>,
    add_section_headers: bool,
    add_symbols: bool,
    debug: bool,
}
impl Data {
    pub fn new(lib_names: Vec<String>) -> Self {
        Self {
            is_64: true,
            interp: "/lib64/ld-linux-x86-64.so.2".to_string(),
            ph: vec![],
            lib_names,
            libs: vec![],
            base: 0x80000,
            page_size: 0x1000,
            //segments: Segments::default(),
            //tracker: SegmentTracker::new(0x80000),
            addr_dynamic: 0,
            addr_dynstr: 0,
            addr_dynsym: 0,
            //addr_interp: 0,
            index_strtab: None,
            index_symtab: None,
            index_dynstr: None,
            index_dynsym: None,
            add_section_headers: true,
            add_symbols: true,
            debug: true,
        }
    }

    fn is_dynamic(&self) -> bool {
        self.libs.len() > 0
    }

    fn add_library(&mut self, string_id: StringId) {
        self.libs.push(Library { string_id });
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
            tag: elf::DT_DEBUG,
            val: 0,
            string: None,
        });

        out.push(Dynamic {
            tag: elf::DT_SYMTAB,
            val: self.addr_dynsym,
            string: None,
        });
        out.push(Dynamic {
            tag: elf::DT_STRTAB,
            val: self.addr_dynstr,
            string: None,
        });

        out.push(Dynamic {
            tag: elf::DT_NULL,
            val: 0,
            string: None,
        });
        out
    }

    pub fn debug(&mut self, _link: &Link) {
        let mut out_data = Vec::new();
        let endian = Endianness::Little;
        let mut writer = object::write::elf::Writer::new(endian, self.is_64, &mut out_data);

        // add libraries if they are configured
        let lib_names = self.lib_names.clone();
        for lib_name in lib_names.iter() {
            let string_id = writer.add_dynamic_string(lib_name.as_bytes());
            self.add_library(string_id);
        }

        // load bytes and relocations
        //self.segments.load(link, &mut writer);
        //self.segments.load(link);
        //self.segments.read_unlinked(link, &mut writer);

        //self.segments.rx.debug();
    }
}

pub fn load<'a>(
    blocks: &mut Blocks,
    //data: &mut Data,
    link: &'a Link,
    w: &mut Writer<'a>,
) {
    let mut ro = vec![];
    let mut rw = vec![];
    let mut rx = vec![];

    for (_name, unlinked) in link.unlinked.iter() {
        use object::SectionKind as K;
        match unlinked.kind {
            K::Data | K::UninitializedData => {
                rw.push(unlinked);
            }

            // OtherString is usually comments, we can drop these
            K::OtherString => (),
            K::ReadOnlyString | K::ReadOnlyData => {
                eprintln!("X:{:?}", (&unlinked.name, &unlinked.kind));
                ro.push(unlinked);
                // XXX: this can mess things up
                // it adds things to RO before the text begins and gets confused
            }
            K::Text => {
                rx.push(unlinked);
            }

            // ignore for now
            K::Metadata => (),
            K::Other => (),
            K::Note => (),
            K::Elf(_x) => {
                // ignore
                //unimplemented!("Elf({:#x})", x);
            }
            _ => unimplemented!("Unlinked kind: {:?}", unlinked.kind),
        }
    }

    if rx.len() > 0 {
        //let name_id = Some(w.add_section_name(".text".as_bytes()));
        let name = ".text".to_string();
        let name_id = Some(w.add_section_name(".text".as_bytes()));
        let rel_name_id = Some(w.add_section_name(".rela.text".as_bytes()));
        let mut section = ProgSection::new(AllocSegment::RX, name_id, rel_name_id, 0);
        for u in rx.clone() {
            section.append(&u, w);
        }
        //eprintln!("s: {:?}", &section.name);
        //blocks.push(Box::new(section));
        let buf = section.bytes.clone();
        let block = BufferSection::new(AllocSegment::RX, name_id, buf, section);
        //for u in rx {
        //block.unlinked.push(u.clone());
        //}
        //block.unlinked.extend(rx);
        blocks.add_block(Box::new(block));
    }

    if ro.len() > 0 {
        let name_id = Some(w.add_section_name(".rodata".as_bytes()));
        let rel_name_id = Some(w.add_section_name(".rela.rodata".as_bytes()));
        let mut section = ProgSection::new(AllocSegment::RO, name_id, rel_name_id, 0);
        for u in ro {
            section.append(&u, w);
        }
        //eprintln!("s: {:?}", &section.name);
        //blocks.push(Box::new(section));
    }

    if rw.len() > 0 {
        //let name = ".data".to_string();
        let name_id = Some(w.add_section_name(".data".as_bytes()));
        let rel_name_id = Some(w.add_section_name(".rela.data".as_bytes()));
        let mut section = ProgSection::new(AllocSegment::RW, name_id, rel_name_id, 0);
        for u in rw.clone() {
            section.append(&u, w);
        }
        //eprintln!("s: {:?}", &section.name);
        //blocks.push(Box::new(section));
        let buf = section.bytes.clone();
        let name_id = Some(w.add_section_name(".data".as_bytes()));
        let block = BufferSection::new(AllocSegment::RW, name_id, buf, section);
        //for u in rw {
        //block.unlinked.push(u.clone());
        //}
        blocks.add_block(Box::new(block));
        //blocks.push(Box::new(BufferSection::new(AllocSegment::RW, name_id, buf)));
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

    // configure blocks
    // these are used to correctly order the reservation of space
    // and to write things out in the correct order
    let mut blocks = Blocks::new();
    blocks.add_block(Box::new(HeaderComponent::default()));

    if data.is_dynamic() {
        if false {
            let name_id = Some(writer.add_section_name(".interp".as_bytes()));
            let mut section = ProgSection::new(AllocSegment::RO, name_id, None, 0);
            let interp = data.interp.as_bytes().to_vec();
            let cstr = std::ffi::CString::new(interp).unwrap();
            section.add_bytes(cstr.as_bytes_with_nul());
            blocks.add_block(Box::new(section));
        } else {
            blocks.add_block(Box::new(InterpSection::new(&data)));
        }
    }

    // relocations go here
    if RelocationSection::has_rx_relocs(&data) {
        blocks.add_block(Box::new(RelocationSection::new(AllocSegment::RX)));
    }
    if RelocationSection::has_rw_relocs(&data) {
        blocks.add_block(Box::new(RelocationSection::new(AllocSegment::RW)));
    }

    load(&mut blocks, link, &mut writer);

    if writer.dynstr_needed() {
        blocks.add_block(Box::new(DynStrSection::default()));
    }

    if data.is_dynamic() {
        blocks.add_block(Box::new(DynSymSection::default()));
    }

    if data.is_dynamic() {
        blocks.add_block(Box::new(DynamicSection::default()));
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

    blocks.reserve(&mut tracker, &mut data, &mut writer);

    if data.add_section_headers {
        writer.reserve_section_headers();
    }

    // UPDATE
    tracker.update(&mut data, &mut blocks);
    blocks.update(&mut data);

    // WRITE
    blocks.write(&data, &mut tracker, &mut writer);

    // SECTION HEADERS
    if data.add_section_headers {
        blocks.write_section_headers(&data, &tracker, &mut writer);
    }

    data.debug(link);
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
