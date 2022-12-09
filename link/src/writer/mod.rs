use std::error::Error;

use object::elf;
use object::read::elf::FileHeader;
use object::write::elf::{SectionIndex, Writer};
use object::write::StringId;
use object::Endianness;
use std::collections::HashMap;

use super::*;
//use crate::*;

mod section;
mod segments;
mod blocks;

use section::*;
use segments::*;
use blocks::*;

enum SectionKind {
    Interp,
}

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

struct Library {
    string_id: StringId,
}

struct Dynamic {
    tag: u32,
    // Ignored if `string` is set.
    val: u64,
    string: Option<object::write::StringId>,
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


#[derive(PartialEq)]
pub enum AllocSegment {
    Interp,
    RO,
    RW,
    RX,
}

impl AllocSegment {
    pub fn flags(&self) -> u32 {
        match self {
            AllocSegment::RO | AllocSegment::Interp => elf::SHF_ALLOC,
            AllocSegment::RW => elf::SHF_ALLOC | elf::SHF_WRITE,
            AllocSegment::RX => elf::SHF_ALLOC | elf::SHF_EXECINSTR,
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
    segments: Segments,
    addr_dynamic: u64,
    addr_dynstr: u64,
    addr_dynsym: u64,
    addr_interp: u64,
    index_strtab: Option<SectionIndex>,
    index_symtab: Option<SectionIndex>,
    index_dynstr: Option<SectionIndex>,
    index_dynsym: Option<SectionIndex>,
    add_section_headers: bool,
    add_symbols: bool,
}
impl Data {
    pub fn new(lib_names: Vec<String>) -> Self {
        Self {
            is_64: true,
            interp: "/lib64/ld-linux-x86-64.so.2".to_string(),
            ph: vec![],
            lib_names,
            libs: vec![],
            page_size: 0x1000,
            segments: Segments::default(),
            addr_dynamic: 0,
            addr_dynstr: 0,
            addr_dynsym: 0,
            addr_interp: 0,
            index_strtab: None,
            index_symtab: None,
            index_dynstr: None,
            index_dynsym: None,
            add_section_headers: true,
            add_symbols: true,
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

    // load bytes and relocations
    data.segments.load(link);
    data.segments.read_unlinked(link, &mut writer);

    // configure blocks
    // these are used to correctly order the reservation of space
    // and to write things out in the correct order
    let mut blocks: Vec<Box<dyn ElfBlock>> = vec![];
    blocks.push(Box::new(HeaderComponent::default()));

    if data.is_dynamic() {
        let name_id = writer.add_section_name(".interp".as_bytes());
        let buf = data.interp.as_bytes().to_vec();
        blocks.push(Box::new(BufferSection::new(
            AllocSegment::Interp,
            Some(name_id),
            buf.to_vec(),
        )));
    }

    if writer.dynstr_needed() {
        blocks.push(Box::new(DynStrSection::default()));
    }

    if data.is_dynamic() {
        blocks.push(Box::new(DynSymSection::default()));
    }

    // .text
    // Add .text section to the text load segment
    let buf = data.segments.rx.bytes.clone();
    let name_id = Some(writer.add_section_name(".text".as_bytes()));
    blocks.push(Box::new(BufferSection::new(AllocSegment::RX, name_id, buf)));

    // .data
    let buf = data.segments.rw.bytes.clone();
    let name_id = Some(writer.add_section_name(".data".as_bytes()));
    blocks.push(Box::new(BufferSection::new(AllocSegment::RW, name_id, buf)));

    if data.is_dynamic() {
        blocks.push(Box::new(DynamicSection::default()));
    }

    if data.add_symbols {
        blocks.push(Box::new(SymTabSection::default()));
    }

    if data.add_symbols && writer.strtab_needed() {
        blocks.push(Box::new(StrTabSection::default()));
    }

    // relocations go here
    blocks.push(Box::new(RelocationSection::default()));

    // shstrtab needs to be allocated last, once all headers are reserved
    if data.add_symbols {
        blocks.push(Box::new(ShStrTabSection::default()));
    }

    // RESERVE

    // get a list of program headers
    // we really only need to know the number of headers, so we can correctly
    // set the values in the file header
    let mut ph = vec![];
    for b in blocks.iter() {
        ph.extend(b.program_header(&data));
    }

    // section headers are optional
    if data.add_section_headers {
        for b in blocks.iter_mut() {
            b.reserve_section_index(&mut data, &mut writer);
        }
    }

    for b in blocks.iter_mut() {
        b.reserve(&mut data, &ph, &mut writer);
    }

    if data.add_section_headers {
        writer.reserve_section_headers();
    }

    // UPDATE

    data.segments.update(0x80000, 0x1000);

    for b in blocks.iter_mut() {
        b.update(&mut data);
    }

    // WRITE

    // get a list of program headers
    // we now have the values so they will be correctly written
    let mut ph = vec![];
    for b in blocks.iter() {
        ph.extend(b.program_header(&data));
    }

    for b in blocks.iter_mut() {
        b.write(&data, &ph, &mut writer);
    }

    // SECTION HEADERS
    if data.add_section_headers {
        for b in blocks.iter() {
            b.write_section_header(&data, &mut writer);
        }
    }
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
