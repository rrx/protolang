use std::error::Error;

use object::elf;
use object::read::elf::{Dyn, FileHeader, ProgramHeader, Rel, Rela, SectionHeader, Sym};
use object::write::elf::{SectionIndex, Writer};
use object::write::{Result, StringId};
use object::Endianness;
//use core::slice::SlicePattern;

use super::*;
use crate::*;

enum SectionKind {
    Interp,
}

struct ProgramHeaderEntry {
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

trait ElfComponent {
    fn reserve(&mut self, data: &mut Data, w: &mut Writer) {}
    fn update(&mut self, data: &mut Data) {}
    fn write(&self, data: &Data, w: &mut Writer) {}
    fn write_section_header(&self, data: &Data, w: &mut Writer) {}
}

struct HeaderComponent {}

impl ElfComponent for HeaderComponent {
    fn reserve(&mut self, data: &mut Data, w: &mut Writer) {
        if w.reserved_len() > 0 {
            panic!("Must start with file header");
        }

        let null_section_index = w.reserve_null_section_index();

        // Start reserving file ranges.
        w.reserve_file_header();
        data.size_fh = w.reserved_len();

        let ph_count = data.get_header_count();

        let before = w.reserved_len();
        w.reserve_program_headers(ph_count as u32);
        let after = w.reserved_len();
        data.size_ph = after - before;

        // add headers to the ro_size
        data.ro.add_data(data.size_fh, 0x1);
        data.ro.add_data(data.size_ph, 0x1);
    }

    fn update(&mut self, data: &mut Data) {}

    fn write(&self, data: &Data, w: &mut Writer) {
        w.write_file_header(&object::write::elf::FileHeader {
            os_abi: 0x00,             // SysV
            abi_version: 0,           // ignored on linux
            e_type: elf::ET_EXEC,     // ET_EXEC - Executable file
            e_machine: 0x3E,          // AMD x86-64
            e_entry: data.addr_start, // e_entry, normally points to _start
            e_flags: 0,               // e_flags
        })
        .unwrap();

        w.write_align_program_headers();

        for ph in data.gen_ph().iter() {
            w.write_program_header(&object::write::elf::ProgramHeader {
                p_type: ph.p_type,
                p_flags: ph.p_flags,
                p_offset: ph.p_offset,
                p_vaddr: ph.p_vaddr,
                p_paddr: ph.p_paddr,
                p_filesz: ph.p_filesz,
                p_memsz: ph.p_memsz,
                p_align: ph.p_align,
            });
        }
    }
    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        w.write_null_section_header();
    }
}

/*
struct SegmentSection {
    index: Option<SectionIndex>,
    align: usize,
    name_id: Option<StringId>,
    alloc: AllocSegment,
}

impl SegmentSection {
    fn new(alloc: AllocSegment, align: usize) -> Self {
        Self {
            index: None,
            alloc,
            align,
            name_id: None,
        }
    }
    pub fn name_section(mut self, name_id: StringId) -> Self {
        self.name_id = Some(name_id);
        self
    }
}

impl ElfComponent for SegmentSection {
    fn reserve(&mut self, data: &mut Data, w: &mut Writer) {
        let blocks = match self.alloc {
            AllocSegment::RO => &data.ro.blocks,
            AllocSegment::RW => &data.rw.blocks,
            AllocSegment::RX => &data.rx.blocks,
        };

        //for c in components.iter_mut() {
        //c.reserve(data, w);
        //}
        //for b in blocks {
        //let index = w.reserve_section_index();
        //w.reserve(b.buf.len(), 1);
        //}
    }

    fn update(&self, data: &mut Data) {}

    fn write(&self, data: &Data, w: &mut Writer) {
        let blocks = match self.alloc {
            AllocSegment::RO => &data.ro.blocks,
            AllocSegment::RW => &data.rw.blocks,
            AllocSegment::RX => &data.rx.blocks,
        };
        let components = match self.alloc {
            AllocSegment::RO => &data.ro.components,
            AllocSegment::RW => &data.rw.components,
            AllocSegment::RX => &data.rx.components,
        };
        for c in components {
            c.write(data, w);
        }
        for b in blocks {
            w.write(b.buf.as_slice());
        }
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        let blocks = match self.alloc {
            AllocSegment::RO => &data.ro.blocks,
            AllocSegment::RW => &data.rw.blocks,
            AllocSegment::RX => &data.rx.blocks,
        };
        let components = match self.alloc {
            AllocSegment::RO => &data.ro.components,
            AllocSegment::RW => &data.rw.components,
            AllocSegment::RX => &data.rx.components,
        };

        let addr = match self.alloc {
            AllocSegment::RO => data.ro.addr,
            AllocSegment::RW => data.rw.addr,
            AllocSegment::RX => data.rx.addr,
        };
        let size = match self.alloc {
            AllocSegment::RO => data.ro.size,
            AllocSegment::RW => data.rw.size,
            AllocSegment::RX => data.rx.size,
        };
        let segment_offset = match self.alloc {
            AllocSegment::RO => data.ro.offset,
            AllocSegment::RW => data.rw.offset,
            AllocSegment::RX => data.rx.offset,
        };
        let sh_flags = self.alloc.flags();

        let mut offset = 0;
        for c in components {
            c.write_section_header(data, w);
        }

        for b in blocks.iter() {
            let size = b.buf.len();
            if let Some(name_id) = b.name_id {
                w.write_section_header(&object::write::elf::SectionHeader {
                    name: Some(name_id),
                    sh_type: elf::SHT_PROGBITS,
                    sh_flags: sh_flags as u64,
                    sh_addr: addr,
                    sh_offset: segment_offset + offset,
                    sh_info: 0,
                    sh_link: 0,
                    sh_entsize: 0,
                    sh_addralign: self.align as u64,
                    sh_size: size as u64,
                });
                offset += size as u64;
            }
        }
    }
}

*/

struct DynamicSection {
    index: Option<SectionIndex>,
    start: usize,
    align: usize,
}
impl Default for DynamicSection {
    fn default() -> Self {
        Self {
            index: None,
            start: 0,
            align: 0x10,
        }
    }
}

impl ElfComponent for DynamicSection {
    fn reserve(&mut self, data: &mut Data, w: &mut Writer) {
        let dynamic = data.gen_dynamic();
        let name = Some(w.add_section_name(".dynamic".as_bytes()));
        let dynamic_index = w.reserve_dynamic_section_index();
        let before = w.reserved_len();
        self.start = size_align(before, self.align);
        //self.start = data.rw.offset as usize;
        w.reserve_until(self.start);
        w.reserve_dynamic(dynamic.len());
        let after = w.reserved_len();
        let size = after - before;
        // allocate space in the rw segment
        data.rw.add_data(size, self.align);
        data.size_dynamic = size;
    }

    fn update(&mut self, data: &mut Data) {
        data.addr_dynamic = data.rw.addr;
    }

    fn write(&self, data: &Data, w: &mut Writer) {
        let dynamic = data.gen_dynamic();
        // write dynamic
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align);
        w.pad_until(aligned_pos);
        w.write_align_dynamic();
        for d in dynamic.iter() {
            if let Some(string) = d.string {
                w.write_dynamic_string(d.tag, string);
            } else {
                w.write_dynamic(d.tag, d.val);
            }
        }
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        w.write_dynamic_section_header(data.addr_dynamic);
    }
}

#[derive(PartialEq)]
enum AllocSegment {
    Interp,
    RO,
    RW,
    RX,
}

impl AllocSegment {
    fn flags(&self) -> u32 {
        match self {
            AllocSegment::RO | AllocSegment::Interp => elf::SHF_ALLOC,
            AllocSegment::RW => elf::SHF_ALLOC | elf::SHF_WRITE,
            AllocSegment::RX => elf::SHF_ALLOC | elf::SHF_EXECINSTR,
        }
    }
}

/*
struct AllocateSection {
    data: Vec<u8>,
    name_id: Option<StringId>,
    align: usize,
    page_align: usize,
    addr: u64,
    offset: u64,
    size: usize,
    alloc: AllocSegment,
}

impl AllocateSection {
    pub fn new(data: Vec<u8>, align: usize, page_align: usize, alloc: AllocSegment) -> Self {
        Self {
            data,
            name_id: None,
            align,
            page_align,
            addr: 0,
            offset: 0,
            size: 0,
            alloc,
        }
    }

    pub fn name_section(mut self, name_id: StringId) -> Self {
        self.name_id = Some(name_id);
        self
    }
}

impl ElfComponent for AllocateSection {
    fn reserve(&mut self, data: &mut Data, w: &mut Writer) {
        let index = w.reserve_section_index();

        let pos = w.reserved_len();
        let align_pos = size_align(pos, self.align);
        w.reserve_until(align_pos);

        let start = w.reserved_len();
        self.offset = start as u64;
        w.reserve(self.data.len(), self.align as usize) as u64;
        let end = w.reserved_len() as u64;
        self.size = self.data.len();
        eprintln!(
            "reserveA: pos: {:#0x}, start: {:#0x}, end: {:#0x}, size: {:#0x}",
            pos, start, self.offset, self.size
        );
        match self.alloc {
            AllocSegment::RO => data.ro.add_data(self.size, self.align),
            AllocSegment::RW => data.rx.add_data(self.size, self.align),
            AllocSegment::RX => data.rx.add_data(self.size, self.align),
        };
    }

    fn write(&self, data: &Data, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align);
        w.pad_until(aligned_pos);
        let addr = w.len();
        w.write(self.data.as_slice());
        let end = w.len();
        let size = end - addr;
        eprintln!(
            "writeA: pos: {:#0x}, addr: {:#0x}, offset: {:#0x}, size: {:#0x}",
            pos, addr, self.offset, size
        );
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        let addr = match self.alloc {
            AllocSegment::RO => data.ro.addr, // + self.offset,
            AllocSegment::RW => data.rw.addr, // + self.offset,
            AllocSegment::RX => data.rx.addr, // + self.offset,
        };
        let sh_flags = self.alloc.flags();
        w.write_section_header(&object::write::elf::SectionHeader {
            name: self.name_id,
            sh_type: elf::SHT_PROGBITS,
            sh_flags: sh_flags as u64,
            sh_addr: addr + self.offset,
            sh_offset: self.offset,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign: self.align as u64,
            sh_size: self.data.len() as u64,
        });
    }
}
*/

struct SymTabSection {
    index: Option<SectionIndex>,
    align: usize,
}
impl Default for SymTabSection {
    fn default() -> Self {
        Self {
            index: None,
            align: 0x10,
        }
    }
}

impl ElfComponent for SymTabSection {
    fn reserve(&mut self, data: &mut Data, w: &mut Writer) {
        //w.reserve_symbol_index(data.index_text);
        //w.reserve_symbol_index(data.index_data);
        w.reserve_symtab_section_index();
        if w.symtab_shndx_needed() {
            w.reserve_symtab_shndx_section_index();
        }

        // reserve the symbols in the various sections
        for sym in &data.sections.ro.symbols {
            w.reserve_symbol_index(data.index_data);
        }
        for sym in &data.sections.rx.symbols {
            w.reserve_symbol_index(data.index_text);
        }
        for sym in &data.sections.rw.symbols {
            w.reserve_symbol_index(data.index_data);
        }

        w.reserve_symtab();

        if w.symtab_shndx_needed() {
            w.reserve_symtab_shndx();
        }
    }

    fn update(&mut self, data: &mut Data) {
        for sym in &data.sections.rx.symbols {
            if sym.is_start {
                data.addr_start = data.rx.addr + sym.s.address;
            }
        }
    }

    fn write(&self, data: &Data, w: &mut Writer) {
        // write symbols
        w.write_null_symbol();

        // write symbols out
        for sym in &data.sections.ro.symbols {
            let st_info = (elf::STB_GLOBAL << 4) + elf::STT_FUNC;
            let st_other = elf::STV_DEFAULT;
            let st_shndx = elf::SHN_ABS;
            let st_size = sym.s.size;
            let addr = sym.s.address + data.ro.base;
            eprintln!("write sym: {:?}, {:#0x}", &sym, addr);
            w.write_symbol(&object::write::elf::Sym {
                name: sym.name_id,
                section: data.index_data, //None,//sym.section,
                st_info,                  //in_sym.st_info(),
                st_other,                 //in_sym.st_other(),
                st_shndx,                 //in_sym.st_shndx(endian),
                st_value: addr,           //in_sym.st_value(endian).into(),
                st_size,                  //in_sym.st_size(endian).into(),
            });
        }
        for sym in &data.sections.rx.symbols {
            let addr = sym.s.address + data.rx.addr;
            eprintln!("write sym: {:?}, {:#0x}", &sym, addr);
            let st_info = (elf::STB_GLOBAL << 4) + elf::STT_FUNC;
            let st_other = elf::STV_DEFAULT;
            let st_shndx = elf::SHN_ABS;
            let st_size = 1;
            w.write_symbol(&object::write::elf::Sym {
                name: sym.name_id,
                section: data.index_text, //None,//sym.section,
                st_info,                  //in_sym.st_info(),
                st_other,                 //in_sym.st_other(),
                st_shndx,                 //in_sym.st_shndx(endian),
                st_value: addr,           //in_sym.st_value(endian).into(),
                st_size,                  //in_sym.st_size(endian).into(),
            });
        }

        for sym in &data.sections.rw.symbols {
            let addr = sym.s.address + data.rw.base;
            eprintln!("write sym: {:?}, {:#0x}", &sym, addr);
            let st_info = (elf::STB_GLOBAL << 4) + elf::STT_FUNC;
            let st_other = elf::STV_DEFAULT;
            let st_shndx = elf::SHN_ABS;
            let st_size = 1;
            w.write_symbol(&object::write::elf::Sym {
                name: sym.name_id,
                section: data.index_data, //sym.section,
                st_info,                  //in_sym.st_info(),
                st_other,                 //in_sym.st_other(),
                st_shndx,                 //in_sym.st_shndx(endian),
                st_value: addr,           //in_sym.st_value(endian).into(),
                st_size,                  //in_sym.st_size(endian).into(),
            });
        }
        if w.symtab_shndx_needed() {
            w.write_symtab_shndx();
        }
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        w.write_symtab_section_header(0);
        if w.symtab_shndx_needed() {
            w.write_symtab_shndx_section_header();
        }
    }
}

struct DynSymSection {
    index: Option<SectionIndex>,
    align: usize,
}
impl Default for DynSymSection {
    fn default() -> Self {
        Self {
            index: None,
            align: 0x10,
        }
    }
}

impl ElfComponent for DynSymSection {
    fn reserve(&mut self, data: &mut Data, w: &mut Writer) {
        w.reserve_null_dynamic_symbol_index();

        data.index_dynsym = Some(w.reserve_dynsym_section_index());

        let pos = w.reserved_len();
        let align_pos = size_align(pos, self.align);
        w.reserve_until(align_pos);

        w.reserve_dynsym();
        let after = w.reserved_len();
        data.addr_dynsym = data.ro.addr + align_pos as u64;
    }

    fn write(&self, data: &Data, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align);
        w.pad_until(aligned_pos);
        w.write_null_dynamic_symbol();
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        w.write_dynsym_section_header(data.addr_dynsym, 1);
    }
}

#[derive(Default)]
struct DynStrSection {
    index: Option<SectionIndex>,
}
impl ElfComponent for DynStrSection {
    fn reserve(&mut self, data: &mut Data, w: &mut Writer) {
        data.index_dynstr = Some(w.reserve_dynstr_section_index());
        let before = w.reserved_len();
        w.reserve_dynstr();
        let after = w.reserved_len();
        data.addr_dynstr = data.rw.addr;
    }

    fn write(&self, data: &Data, w: &mut Writer) {
        w.write_dynstr();
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        w.write_dynstr_section_header(data.addr_dynstr);
    }
}

#[derive(Default)]
struct ShStrTabSection {
    index: Option<SectionIndex>,
}
impl ElfComponent for ShStrTabSection {
    fn reserve(&mut self, data: &mut Data, w: &mut Writer) {
        //let name = Some(w.add_section_name(".shstrtab".as_bytes()));
        let shstrtab_index = w.reserve_shstrtab_section_index();
        let before = w.reserved_len();
        w.reserve_shstrtab();
        let after = w.reserved_len();
    }

    fn write(&self, data: &Data, w: &mut Writer) {
        w.write_shstrtab();
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        w.write_shstrtab_section_header();
    }
}

#[derive(Default)]
struct StrTabSection {
    index: Option<SectionIndex>,
}
impl ElfComponent for StrTabSection {
    fn reserve(&mut self, data: &mut Data, w: &mut Writer) {
        //let name = Some(w.add_section_name(".strtab".as_bytes()));
        data.index_strtab = Some(w.reserve_strtab_section_index());
        w.reserve_strtab();
    }

    fn write(&self, data: &Data, w: &mut Writer) {
        w.write_strtab();
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        w.write_strtab_section_header();
    }
}

struct BufferSection {
    alloc: AllocSegment,
    name_id: Option<StringId>,
    addr: usize,
    offset: usize,
    size: usize,
    base: usize,
    buf: Vec<u8>,
}

impl BufferSection {
    fn new(alloc: AllocSegment, name_id: Option<StringId>, buf: Vec<u8>) -> Self {
        Self {
            alloc,
            name_id,
            addr: 0,
            offset: 0,
            size: 0,
            base: 0,
            buf,
        }
    }
    fn align(&self) -> usize {
        0x10
    }
}

impl ElfComponent for BufferSection {
    fn reserve(&mut self, data: &mut Data, w: &mut Writer) {
        let index = w.reserve_section_index();
        match self.alloc {
            AllocSegment::RW => {
                data.index_data = Some(index);
            }
            AllocSegment::RX => {
                data.index_text = Some(index);
            }
            _ => (), //AllocSegment::RO => (),
        }

        let pos = w.reserved_len();
        let align_pos = size_align(pos, self.align());
        w.reserve_until(align_pos);
        let start = w.reserved_len();
        let end = w.reserve(self.buf.len(), self.align());
        self.size = self.buf.len();
        self.offset = start;
        match self.alloc {
            AllocSegment::RO | AllocSegment::Interp => data.ro.add_data(self.size, self.align()),
            AllocSegment::RW => data.rw.add_data(self.size, self.align()),
            AllocSegment::RX => data.rx.add_data(self.size, self.align()),
        };

        if self.alloc == AllocSegment::Interp {
            data.addr_interp = start as u64;
        }

        log::debug!(
            "reserve: pos: {:#0x}, start: {:#0x}, end: {:#0x}, size: {:#0x}",
            pos,
            start,
            end,
            self.buf.len()
        );
    }

    fn update(&mut self, data: &mut Data) {
        let segment = match self.alloc {
            AllocSegment::RO | AllocSegment::Interp => &data.ro,
            AllocSegment::RW => &data.rw,
            AllocSegment::RX => &data.rx,
        };
        match self.alloc {
            AllocSegment::RW => {
                data.addr_text = self.offset as u64;
            }
            //AllocSegment::RX => {}
            //AllocSegment::RO => (),
            _ => (),
        }

        self.base = segment.base as usize;
        self.addr = segment.base as usize + self.offset;
    }

    fn write(&self, data: &Data, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align());
        w.pad_until(aligned_pos);
        log::debug!(
            "write at: pos: {:#0x}, addr: {:#0x}, offset: {:#0x}, size: {:#0x}",
            pos,
            self.addr,
            self.offset,
            self.buf.len()
        );
        w.write(self.buf.as_slice());
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        if let Some(name_id) = self.name_id {
            w.write_section_header(&object::write::elf::SectionHeader {
                name: Some(name_id),
                sh_type: elf::SHT_PROGBITS,
                sh_flags: self.alloc.flags() as u64,
                sh_addr: self.addr as u64,
                sh_offset: self.offset as u64,
                sh_info: 0,
                sh_link: 0,
                sh_entsize: 0,
                sh_addralign: self.align() as u64,
                sh_size: self.size as u64,
            });
        }
    }
}

struct Segment {
    base: u64,
    addr: u64,
    offset: u64,
    size: usize,
    align: u32,
    alloc: AllocSegment,
    blocks: Vec<BufferSection>,
    components: Vec<Box<dyn ElfComponent>>,
}

impl Segment {
    fn new_ro() -> Self {
        Self {
            base: 0,
            addr: 0,
            offset: 0,
            size: 0,
            alloc: AllocSegment::RO,
            align: 0x1000,
            blocks: vec![],
            components: vec![],
        }
    }

    fn new_rw() -> Self {
        Self {
            base: 0,
            addr: 0,
            offset: 0,
            size: 0,
            alloc: AllocSegment::RW,
            align: 0x1000,
            blocks: vec![],
            components: vec![],
        }
    }

    fn new_rx() -> Self {
        Self {
            base: 0,
            addr: 0,
            offset: 0,
            size: 0,
            alloc: AllocSegment::RX,
            align: 0x1000,
            blocks: vec![],
            components: vec![],
        }
    }

    fn add_data(&mut self, size: usize, align: usize) {
        self.size = size_align(self.size, align) + size;
    }
}

#[derive(Debug)]
struct ProgSymbol {
    name_id: Option<StringId>,
    is_start: bool,
    s: CodeSymbol,
}

#[derive(Default)]
struct ProgSection {
    symbols: Vec<ProgSymbol>,
}

#[derive(Default)]
struct Sections {
    ro: ProgSection,
    rw: ProgSection,
    rx: ProgSection,
}

struct Data {
    interp: Option<String>,
    is_64: bool,
    code_segments: Vec<UnlinkedCodeSegment>,
    data_segments: Vec<UnlinkedCodeSegment>,
    ph: Vec<ProgramHeaderEntry>,
    libs: Vec<Library>,
    page_size: u32,
    ro: Segment,
    rw: Segment,
    rx: Segment,
    addr_dynamic: u64,
    addr_dynstr: u64,
    addr_dynsym: u64,
    addr_text: u64,
    addr_start: u64,
    addr_interp: u64,
    index_data: Option<SectionIndex>,
    index_text: Option<SectionIndex>,
    index_strtab: Option<SectionIndex>,
    index_dynstr: Option<SectionIndex>,
    index_dynsym: Option<SectionIndex>,
    size_fh: usize,
    size_ph: usize,
    size_dynamic: usize,
    sections: Sections,
}
impl Data {
    fn read_unlinked<'a>(link: &'a Link, w: &mut Writer<'a>, s: &mut Sections) {
        let mut ro_size = 0;
        let mut rw_size = 0;
        let mut rx_size = 0;

        for (_name, unlinked) in link.unlinked.iter() {
            use object::SectionKind as K;
            let is_start = false;
            match unlinked.kind {
                K::Data | K::UninitializedData => {
                    //w.reserve_symbol_index(None);
                    for (name, symbol) in unlinked.defined.iter() {
                        let name_id = Some(w.add_string(name.as_bytes()));

                        let mut symbol = symbol.clone();
                        symbol.address += rw_size as u64;

                        let ps = ProgSymbol {
                            name_id,
                            is_start,
                            s: symbol,
                        };

                        s.rw.symbols.push(ps);
                    }
                    rw_size += unlinked.bytes.len();
                }
                K::OtherString | K::ReadOnlyString | K::ReadOnlyData => {
                    for (name, symbol) in unlinked.defined.iter() {
                        let name_id = Some(w.add_string(name.as_bytes()));
                        let mut symbol = symbol.clone();
                        symbol.address += ro_size as u64;
                        let ps = ProgSymbol {
                            name_id,
                            is_start,
                            s: symbol,
                        };
                        s.ro.symbols.push(ps);
                    }
                    ro_size += unlinked.bytes.len();
                }
                K::Text => {
                    //w.reserve_symbol_index(None);
                    for (name, symbol) in unlinked.defined.iter() {
                        let name_id = Some(w.add_string(name.as_bytes()));
                        let mut symbol = symbol.clone();
                        symbol.address += rx_size as u64;

                        let is_start = name == "_start";
                        let ps = ProgSymbol {
                            name_id,
                            is_start,
                            s: symbol,
                        };
                        s.rx.symbols.push(ps);
                    }
                    rx_size += unlinked.bytes.len();
                }

                // ignore for now
                K::Metadata => (),
                K::Other => (),
                K::Elf(_x) => {
                    // ignore
                    //unimplemented!("Elf({:#x})", x);
                }
                _ => unimplemented!("Unlinked kind: {:?}", unlinked.kind),
            }
        }
    }

    fn new(link: &Link) -> Self {
        let mut code_segments = vec![];
        let mut data_segments = vec![];

        let mut ro_size = 0;
        let mut rw_size = 0;
        let mut rx_size = 0;

        for (_name, unlinked) in link.unlinked.iter() {
            use object::SectionKind as K;
            match unlinked.kind {
                K::Data | K::UninitializedData => {
                    rw_size += unlinked.bytes.len();
                    data_segments.push(unlinked.clone());
                }
                K::OtherString | K::ReadOnlyString | K::ReadOnlyData => {
                    ro_size += unlinked.bytes.len();
                    data_segments.push(unlinked.clone());
                }
                K::Text => {
                    rx_size += unlinked.bytes.len();
                    code_segments.push(unlinked.clone());
                }

                // ignore for now
                K::Metadata => (),
                K::Other => (),
                K::Elf(_x) => {
                    // ignore
                    //unimplemented!("Elf({:#x})", x);
                }
                _ => unimplemented!("Unlinked kind: {:?}", unlinked.kind),
            }
        }

        let mut ro = Segment::new_ro();
        //ro.size = 0;//ro_size;
        let mut rw = Segment::new_rw();
        //rw.size = 0;//rw_size;
        //rx.add_data(rx_size, 0x10);// = 0;//rx_size;
        let mut rx = Segment::new_rx();
        //rx.add_data(rx_size, 0x10); // = 0;//rx_size;

        //let sections = Self::read_unlinked(link, );

        Self {
            is_64: true,
            interp: None,
            //interp: Some("/lib/ld-linux-x86-64.so.2".to_string()),
            code_segments,
            data_segments,
            ph: vec![],
            libs: vec![],
            page_size: 0x1000,
            ro,
            rx,
            rw,
            addr_dynamic: 0,
            addr_dynstr: 0,
            addr_dynsym: 0,
            addr_text: 0,
            addr_start: 0,
            addr_interp: 0,
            index_strtab: None,
            index_dynstr: None,
            index_dynsym: None,
            index_data: None,
            index_text: None,
            size_fh: 0,
            size_ph: 0,
            size_dynamic: 0,
            sections: Sections::default(),
        }
    }

    fn add_library(&mut self, w: &mut Writer, string_id: StringId) {
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

        if false {
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
        }
        out.push(Dynamic {
            tag: elf::DT_NULL,
            val: 0,
            string: None,
        });
        out
    }

    fn get_header_count(&self) -> usize {
        //let dynamic = self.gen_dynamic();
        // calculate the number of headers
        let mut ph_count = 1; // the header itself
        if let Some(_) = self.interp {
            ph_count += 1;
        }

        // we always have 3 segemnts ro, rw, rx, they might be empty
        // ro is never empty, because it includes the headers
        // rx might be empty if there's no code
        // rw has symbols and tables usually
        ph_count += 3;
        if self.libs.len() > 0 {
            ph_count += 1; // dynamic
        }
        ph_count
    }

    fn update_segments(&mut self) {
        let align = 0x10;
        let base = 0x80000;
        let mut offset = 0;
        let ro_size_elf_aligned = size_align(self.ro.size as usize, align);
        self.ro.base = base;
        self.ro.addr = base + offset as u64;
        self.ro.offset = offset as u64;
        self.ro.align = align as u32;
        log::debug!(
            "RO {:#0x}, {:#0x}, size: {:#0x}",
            base,
            offset,
            self.ro.size
        );
        offset += ro_size_elf_aligned;

        let base = size_align(
            base as usize + offset, //self.ro.size as usize,
            self.page_size as usize,
        ) as u64;
        let rx_size_elf_aligned = size_align(self.rx.size as usize, align);
        self.rx.base = base;
        self.rx.addr = base + offset as u64;
        self.rx.offset = offset as u64;
        self.rx.align = align as u32;
        log::debug!(
            "RX {:#0x}, {:#0x}, size: {:#0x} {}",
            base,
            offset,
            self.rx.size,
            rx_size_elf_aligned
        );
        offset += rx_size_elf_aligned;

        let base = size_align(
            base as usize + rx_size_elf_aligned, //self.rx.size as usize,
            self.page_size as usize,
        ) as u64;
        let rw_size_elf_aligned = size_align(self.rw.size as usize, align);
        self.rw.base = base;
        self.rw.addr = base + offset as u64;
        self.rw.offset = offset as u64;
        self.rw.align = align as u32;
        log::debug!(
            "RW {:#0x}, {:#0x}, size: {:#0x}",
            base,
            offset,
            self.rw.size
        );
        //offset += rw_size_elf_aligned;
    }

    fn gen_ph(&self) -> Vec<ProgramHeaderEntry> {
        //let dynamic = self.gen_dynamic();
        let mut ph = vec![];

        let offset = self.size_fh as u64;
        ph.push(ProgramHeaderEntry {
            p_type: elf::PT_PHDR,
            p_flags: elf::PF_R,
            p_offset: self.size_fh as u64, // calculate later
            p_vaddr: self.ro.addr as u64 + offset,
            p_paddr: 0, //self.ro.addr as u64 + offset,
            p_filesz: self.size_ph as u64,
            p_memsz: self.size_ph as u64,
            p_align: 8,
        });
        let mut offset = offset + self.size_ph as u64;

        if let Some(interp) = &self.interp {
            //offset = size_align(offset as usize, 0x10) as u64;
            //
            let cstr = std::ffi::CString::new(interp.as_bytes().to_vec()).unwrap();
            let cstr_size = cstr.as_bytes_with_nul().len();
            ph.push(ProgramHeaderEntry {
                p_type: elf::PT_INTERP,
                p_flags: elf::PF_R,
                p_offset: self.addr_interp,
                p_vaddr: self.addr_interp,
                p_paddr: 0, //self.addr_interp,
                p_filesz: cstr_size as u64,
                p_memsz: cstr_size as u64,
                p_align: 0x10,
            });
            //offset += interp.as_bytes().len() as u64;
        }

        // load segments
        // program LOAD (R)
        let addr = self.ro.addr; // + self.ro.offset as u64;
        ph.push(ProgramHeaderEntry {
            p_type: elf::PT_LOAD,
            p_flags: elf::PF_R,
            p_offset: self.ro.offset, // read section starts at 0 offset to include headers
            p_vaddr: addr,
            p_paddr: 0, //addr,
            p_filesz: self.ro.size as u64,
            p_memsz: self.ro.size as u64,
            p_align: self.page_size as u64,
        });

        // program LOAD (RX)
        let addr = self.rx.addr; // + self.rx.offset as u64;
        ph.push(ProgramHeaderEntry {
            p_type: elf::PT_LOAD,
            p_flags: elf::PF_R | elf::PF_X,
            p_offset: self.rx.offset,
            p_vaddr: addr,
            p_paddr: 0, //addr,
            p_filesz: self.rx.size as u64,
            p_memsz: self.rx.size as u64,
            p_align: self.page_size as u64,
        });
        log::debug!("{:#0x}, {:#0x}", self.rx.offset, addr);

        // program LOAD (RW)
        let addr = self.rw.addr; // + self.rw.offset as u64;
        ph.push(ProgramHeaderEntry {
            p_type: elf::PT_LOAD,
            p_flags: elf::PF_R | elf::PF_W,
            p_offset: self.rw.offset as u64,
            p_vaddr: addr,
            p_paddr: 0, //addr,
            p_filesz: self.rw.size as u64,
            p_memsz: self.rw.size as u64,
            p_align: self.page_size as u64,
        });

        if self.size_dynamic > 0 {
            //program DYNAMIC
            ph.push(ProgramHeaderEntry {
                p_type: elf::PT_DYNAMIC,
                p_flags: elf::PF_R | elf::PF_W,
                p_offset: self.rw.offset as u64,
                p_vaddr: addr,
                p_paddr: 0, //addr,
                p_filesz: self.size_dynamic as u64,
                p_memsz: self.size_dynamic as u64,
                p_align: 0x8,
            });
        }
        ph
    }

    fn reserve_sections(&mut self, w: &mut Writer, components: &mut Vec<Box<dyn ElfComponent>>) {
        components.iter_mut().for_each(|c| {
            c.reserve(self, w);
        });

        w.reserve_section_headers();
    }
}

pub fn write_file<Elf: FileHeader<Endian = Endianness>>(
    link: &Link,
) -> std::result::Result<Vec<u8>, Box<dyn Error>> {
    let mut out_data = Vec::new();
    let endian = Endianness::Little;
    let mut data = Data::new(link);
    let is_class_64 = data.is_64;
    let mut writer = object::write::elf::Writer::new(endian, is_class_64, &mut out_data);

    Data::read_unlinked(link, &mut writer, &mut data.sections);

    let mut blocks: Vec<Box<dyn ElfComponent>> = vec![];
    blocks.push(Box::new(HeaderComponent {}));

    let page_align = 0x1000;
    if data.interp.is_some() {
        let name_id = writer.add_section_name(".interp".as_bytes());
        let buf = data.interp.clone().unwrap().as_bytes().to_vec();
        blocks.push(Box::new(BufferSection::new(
            AllocSegment::Interp,
            Some(name_id),
            buf.to_vec(),
        )));
    }

    // .text
    // Add .text section to the text load segment
    let mut buf = vec![];
    for segment in data.code_segments.iter() {
        buf.extend(segment.bytes.clone());
    }
    let name_id = Some(writer.add_section_name(".text".as_bytes()));
    blocks.push(Box::new(BufferSection::new(
        AllocSegment::RX,
        name_id,
        buf.to_vec(),
    )));

    // .data
    let mut buf = vec![];
    for segment in data.data_segments.iter() {
        buf.extend(segment.bytes.clone());
    }
    let name_id = Some(writer.add_section_name(".data".as_bytes()));
    blocks.push(Box::new(BufferSection::new(
        AllocSegment::RW,
        name_id,
        buf.to_vec(),
    )));

    if data.libs.len() > 0 {
        blocks.push(Box::new(DynamicSection::default()));
    }

    if writer.dynstr_needed() {
        blocks.push(Box::new(DynStrSection::default()));
    }

    if data.libs.len() > 0 {
        blocks.push(Box::new(DynSymSection::default()));
    }

    blocks.push(Box::new(SymTabSection::default()));

    if writer.strtab_needed() {
        blocks.push(Box::new(StrTabSection::default()));
    }

    // relocations go here

    // shstrtab needs to be allocated last, once all headers are reserved
    blocks.push(Box::new(ShStrTabSection::default()));

    if false {
        let string_id = writer.add_dynamic_string("libc.so.6".as_bytes());
        data.add_library(&mut writer, string_id);
    }

    // RESERVE
    for b in blocks.iter_mut() {
        b.reserve(&mut data, &mut writer);
    }

    writer.reserve_section_headers();

    // UPDATE
    data.update_segments();
    for b in blocks.iter_mut() {
        b.update(&mut data);
    }

    // WRITE
    for b in blocks.iter_mut() {
        b.write(&data, &mut writer);
    }

    // SECTION HEADERS
    for b in blocks.iter() {
        b.write_section_header(&data, &mut writer);
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
