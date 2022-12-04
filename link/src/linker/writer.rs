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
    fn update(&self, data: &mut Data) {}
    fn write(&self, data: &Data, w: &mut Writer) {}
    fn write_section_header(&self, data: &Data, w: &mut Writer) {}
}
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
        let sh_flags = match self.alloc {
            AllocSegment::RO => elf::SHF_ALLOC,
            AllocSegment::RW => elf::SHF_ALLOC | elf::SHF_WRITE,
            AllocSegment::RX => elf::SHF_ALLOC | elf::SHF_EXECINSTR,
        };

        let mut offset = 0;
        for c in components {
            c.write_section_header(data, w);
        }

        for b in blocks.iter() {
            let size = b.buf.len();
            w.write_section_header(&object::write::elf::SectionHeader {
                name: Some(b.name_id),
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
        // allocate space in the rw segment
        data.rw.size += after - before;
        data.size_dynamic = after - before;
        //data.addr_dynamic = data.rw.addr;// + self.start as u64;
    }

    fn update(&self, data: &mut Data) {
        data.addr_dynamic = data.rw.addr; // + self.start as u64;
    }

    fn write(&self, data: &Data, w: &mut Writer) {
        let dynamic = data.gen_dynamic();
        // write dynamic
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align);
        //w.pad_until(self.start);//aligned_pos);
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

enum AllocSegment {
    RO,
    RW,
    RX,
}

struct AllocateSection<'a> {
    data: Vec<u8>,
    name_id: Option<StringId>,
    align: usize,
    page_align: usize,
    addr: u64,
    offset: u64,
    alloc: AllocSegment,
    _p: std::marker::PhantomData<&'a u8>,
}

impl<'a> AllocateSection<'a> {
    pub fn new(data: Vec<u8>, align: usize, page_align: usize, alloc: AllocSegment) -> Self {
        Self {
            data,
            name_id: None,
            align,
            page_align,
            addr: 0,
            offset: 0,
            alloc,
            _p: std::marker::PhantomData::default(),
        }
    }

    pub fn name_section(mut self, name_id: StringId) -> Self {
        self.name_id = Some(name_id);
        self
    }
}

impl<'a> ElfComponent for AllocateSection<'a> {
    fn reserve(&mut self, data: &mut Data, w: &mut Writer) {
        let index = w.reserve_section_index();

        let pos = w.reserved_len();
        let align_pos = size_align(pos, self.align);
        w.reserve_until(align_pos);

        self.offset = w.reserve(self.data.len(), self.align as usize) as u64;
        let size = (self.offset - pos as u64) as usize;
        match self.alloc {
            AllocSegment::RO => data.ro.size += size,
            AllocSegment::RW => data.rw.size += size,
            AllocSegment::RX => data.rx.size += size,
        };
    }

    fn write(&self, data: &Data, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align);
        w.pad_until(aligned_pos);
        //w.write_align(self.align as usize);
        w.write(self.data.as_slice());
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        let addr = match self.alloc {
            AllocSegment::RO => data.ro.addr, // + self.offset,
            AllocSegment::RW => data.rw.addr, // + self.offset,
            AllocSegment::RX => data.rx.addr, // + self.offset,
        };
        let sh_flags = match self.alloc {
            AllocSegment::RO => elf::SHF_ALLOC,
            AllocSegment::RW => elf::SHF_ALLOC | elf::SHF_WRITE,
            AllocSegment::RX => elf::SHF_ALLOC | elf::SHF_EXECINSTR,
        };
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
        //w.reserve_dynamic_symbol_index();
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
        let name = Some(w.add_section_name(".shstrtab".as_bytes()));
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
        let name = Some(w.add_section_name(".strtab".as_bytes()));
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
    name_id: StringId,
    addr: usize,
    offset: usize,
    size: usize,
    //align: usize,
    buf: Vec<u8>,
}

impl BufferSection {
    fn flags(&self) -> u32 {
        match self.alloc {
            AllocSegment::RO => elf::SHF_ALLOC,
            AllocSegment::RW => elf::SHF_ALLOC | elf::SHF_WRITE,
            AllocSegment::RX => elf::SHF_ALLOC | elf::SHF_EXECINSTR,
        }
    }

    fn align(&self) -> usize {
        0x10
        /*
        match self.alloc {
            AllocSegment::RO => data.ro.align,
            AllocSegment::RW => data.rw.align,
            AllocSegment::RX => data.rx.align,
        }
        */
    }

    fn reserve(&self, w: &mut Writer) {
        let index = w.reserve_section_index();
        let pos = w.reserved_len();
        let align_pos = size_align(pos, self.align());
        w.reserve_until(align_pos);
        w.reserve(self.buf.len(), self.align());
    }

    fn update(&mut self, data: &mut Data) {
        let segment = match self.alloc {
            AllocSegment::RO => &data.ro,
            AllocSegment::RW => &data.rw,
            AllocSegment::RX => &data.rx,
        };
        self.addr = segment.base as usize;
        self.offset = segment.offset as usize;
        self.size = self.buf.len();//segment.size as usize;
    }

    fn write(&mut self, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align());
        //w.write_align(aligned_pos - pos);
        //w.pad_until(self.offset);//aligned_pos);
        w.pad_until(aligned_pos);
        self.addr = self.addr as usize + w.len();
        self.offset = w.len();
        eprintln!("write at: {:#0x}, size: {:#0x}", w.len(), self.buf.len());
        w.write(self.buf.as_slice());
    }

    fn write_section_header(&self, w: &mut Writer) {
        w.write_section_header(&object::write::elf::SectionHeader {
            name: Some(self.name_id),
            sh_type: elf::SHT_PROGBITS,
            sh_flags: self.flags() as u64,
            sh_addr: self.addr as u64,
            sh_offset: self.offset as u64, // + offset,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign: self.align() as u64,
            sh_size: self.size as u64,
        });
    }
}

struct Segment {
    base: u64,
    addr: u64,
    offset: u64,
    size: usize,
    align: u32,
    flags: u32,
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
            flags: elf::SHF_ALLOC,
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
            flags: elf::SHF_ALLOC | elf::SHF_WRITE,
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
            flags: elf::SHF_ALLOC | elf::SHF_EXECINSTR,
            align: 0x1000,
            blocks: vec![],
            components: vec![],
        }
    }

    fn reserve(&mut self, data: &mut Data, w: &mut Writer) {
        for c in self.components.iter_mut() {
            c.reserve(data, w);
        }
        for b in self.blocks.iter() {
            let index = w.reserve_section_index();
            w.reserve(b.buf.len(), 1);
        }
    }

    fn update(&self, data: &mut Data) {}

    fn write(&self, data: &Data, w: &mut Writer) {
        for c in self.components.iter() {
            c.write(data, w);
        }
        for b in self.blocks.iter() {
            w.write(b.buf.as_slice());
        }
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        let mut offset = 0;
        for c in self.components.iter() {
            c.write_section_header(data, w);
        }

        for b in self.blocks.iter() {
            let size = b.buf.len();
            w.write_section_header(&object::write::elf::SectionHeader {
                name: Some(b.name_id),
                sh_type: elf::SHT_PROGBITS,
                sh_flags: self.flags as u64,
                sh_addr: self.addr,
                sh_offset: self.offset + offset,
                sh_info: 0,
                sh_link: 0,
                sh_entsize: 0,
                sh_addralign: self.align as u64,
                sh_size: self.size as u64,
            });
            offset += size as u64;
        }
    }
}

struct Data {
    interp: Option<String>,
    is_64: bool,
    code_segments: Vec<UnlinkedCodeSegment>,
    data_segments: Vec<UnlinkedCodeSegment>,
    ph: Vec<ProgramHeaderEntry>,
    libs: Vec<Library>,
    //dynamic: Vec<Dynamic>,
    sections: Vec<Section>,
    page_size: u32,
    ro: Segment,
    rw: Segment,
    rx: Segment,
    addr_dynamic: u64,
    addr_dynstr: u64,
    addr_dynsym: u64,
    addr_text: u64,
    index_strtab: Option<SectionIndex>,
    index_dynstr: Option<SectionIndex>,
    index_dynsym: Option<SectionIndex>,
    size_fh: usize,
    size_ph: usize,
    size_dynamic: usize,
}
impl Data {
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
        ro.size = ro_size;
        let mut rw = Segment::new_rw();
        rw.size = rw_size;
        let mut rx = Segment::new_rx();
        rx.size = rx_size;

        Self {
            is_64: true,
            interp: Some("/lib/ld-linux-x86-64.so.2".to_string()),
            code_segments,
            data_segments,
            ph: vec![],
            libs: vec![],
            //dynamic: vec![],
            sections: vec![],
            page_size: 0x1000,
            ro,
            rx,
            rw,
            addr_dynamic: 0,
            addr_dynstr: 0,
            addr_dynsym: 0,
            addr_text: 0,
            index_strtab: None,
            index_dynstr: None,
            index_dynsym: None,
            size_fh: 0,
            size_ph: 0,
            size_dynamic: 0,
        }
    }

    //fn reserve(&mut self, w: &mut Writer) {
    //self.ro.reserve(self, w);
    //}

    /*
     fn reserve(&mut self, w: &mut Writer, alloc: AllocSegment) {
         let s = match alloc {
             AllocSegment::RO => &mut self.ro,
             AllocSegment::RW => &mut self.rw,
             AllocSegment::RX => &mut self.rx,
         };
         s.reserve(self, w);
     }
    */

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
            tag: elf::DT_NULL,
            val: 0,
            string: None,
        });
        out
    }

    fn get_header_count(&self) -> usize {
        let dynamic = self.gen_dynamic();
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
        if dynamic.len() > 0 {
            ph_count += 1; // dynamic
        }
        ph_count
    }

    fn reserve_header(&mut self, w: &mut Writer) {
        if w.reserved_len() > 0 {
            panic!("Must start with file header");
        }

        // Start reserving file ranges.
        w.reserve_file_header();
        self.size_fh = w.reserved_len();

        let ph_count = self.get_header_count();

        let before = w.reserved_len();
        w.reserve_program_headers(ph_count as u32);
        let after = w.reserved_len();
        self.size_ph = after - before;

        // add headers to the ro_size
        self.ro.size += self.size_fh + self.size_ph;

        // add interp size to data section
        if self.interp.is_some() {
            self.ro.size += self.interp.as_ref().unwrap().len();
        }
    }

    fn update_segments(&mut self) {
        let align = 0x10;
        let base = 0;
        let mut offset = 0;
        let ro_size_elf_aligned = size_align(self.ro.size as usize, align);
        self.ro.base = base;
        self.ro.addr = base + offset as u64;
        self.ro.offset = offset as u64;
        self.ro.align = align as u32;
        eprintln!("{:#0x}, {:#0x}", base, offset);
        offset += ro_size_elf_aligned;

        let base = size_align(
            base as usize + self.ro.size as usize,
            self.page_size as usize,
        ) as u64;
        let rx_size_elf_aligned = size_align(self.rx.size as usize, align);
        self.rx.base = base;
        self.rx.addr = base + offset as u64;
        self.rx.offset = offset as u64;
        self.rx.align = align as u32;
        eprintln!("{:#0x}, {:#0x}", base, offset);
        offset += rx_size_elf_aligned;

        let base = size_align(
            base as usize + self.rx.size as usize,
            self.page_size as usize,
        ) as u64;
        let rw_size_elf_aligned = size_align(self.rw.size as usize, align);
        self.rw.base = base;
        self.rw.addr = base + offset as u64;
        self.rw.offset = offset as u64;
        self.rw.align = align as u32;
        eprintln!("{:#0x}, {:#0x}", base, offset);
        offset += rw_size_elf_aligned;
    }

    fn gen_ph(&self) -> Vec<ProgramHeaderEntry> {
        let dynamic = self.gen_dynamic();
        let mut ph = vec![];

        let offset = self.size_fh as u64;
        ph.push(ProgramHeaderEntry {
            p_type: elf::PT_PHDR,
            p_flags: elf::PF_R,
            p_offset: self.size_fh as u64, // calculate later
            p_vaddr: self.ro.addr as u64 + offset,
            p_paddr: self.ro.addr as u64 + offset,
            p_filesz: self.size_ph as u64,
            p_memsz: self.size_ph as u64,
            p_align: 8,
        });

        if let Some(interp) = &self.interp {
            let offset = self.size_fh as u64 + self.size_ph as u64;
            ph.push(ProgramHeaderEntry {
                p_type: elf::PT_INTERP,
                p_flags: elf::PF_R,
                p_offset: offset,
                p_vaddr: self.ro.addr + offset,
                p_paddr: self.ro.addr + offset,
                p_filesz: interp.as_bytes().len() as u64,
                p_memsz: interp.as_bytes().len() as u64,
                p_align: 1,
            });
        }

        // load segments
        // program LOAD (R)
        let addr = self.ro.addr; // + self.ro.offset as u64;
        ph.push(ProgramHeaderEntry {
            p_type: elf::PT_LOAD,
            p_flags: elf::PF_R,
            p_offset: self.ro.offset, // read section starts at 0 offset to include headers
            p_vaddr: addr,
            p_paddr: addr,
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
            p_paddr: addr,
            p_filesz: self.rx.size as u64,
            p_memsz: self.rx.size as u64,
            p_align: self.page_size as u64,
        });
        eprintln!("{:#0x}, {:#0x}", self.rx.offset, addr);

        // program LOAD (RW)
        let addr = self.rw.addr; // + self.rw.offset as u64;
        ph.push(ProgramHeaderEntry {
            p_type: elf::PT_LOAD,
            p_flags: elf::PF_R | elf::PF_W,
            p_offset: self.rw.offset as u64,
            p_vaddr: addr,
            p_paddr: addr,
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
                p_paddr: addr,
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

    fn write_sections(
        &self,
        w: &mut Writer,
        components: &Vec<Box<dyn ElfComponent>>,
    ) -> Result<()> {
        Ok(())
    }

    fn write_header(&self, w: &mut Writer) -> Result<()> {
        w.write_file_header(&object::write::elf::FileHeader {
            os_abi: 0x00,            // SysV
            abi_version: 0,          // ignored on linux
            e_type: elf::ET_EXEC,    // ET_EXEC - Executable file
            e_machine: 0x3E,         // AMD x86-64
            e_entry: self.addr_text, // e_entry, normally points to _start
            e_flags: 0,              // e_flags
        })?;

        w.write_align_program_headers();

        for ph in self.gen_ph().iter() {
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
        Ok(())
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

    let mut ro_components: Vec<Box<dyn ElfComponent>> = vec![];
    //let mut rw_components = vec![];
    //let mut rx_components = vec![];
    let mut blocks = vec![];
    //let mut rw_blocks = vec![];

    let page_align = 0x1000;
    let mut components: Vec<Box<dyn ElfComponent>> = vec![];
    if data.interp.is_some() {
        let name_id = writer.add_section_name(".interp".as_bytes());
        //let interp = data.interp.take().unwrap_or(String::default()).into_bytes();
        let buf = data.interp.clone().unwrap().as_bytes().to_vec();
        let s = AllocateSection::new(
            data.interp.clone().unwrap().as_bytes().to_vec(),
            0x10,
            page_align,
            AllocSegment::RO,
        )
        .name_section(name_id);
        //components.push(Box::new(s));
        //data.ro.blocks.push(BufferSection { name_id, buf: buf.to_vec() });
        ro_components.push(Box::new(s)); //BufferSection { name_id, buf: buf.to_vec() });
                                         //let s = SegmentSection::new(AllocSegment::RO, 0x20);//.name_section(name_id);
                                         //components.push(Box::new(s));
    }

    // .text
    let mut buf = vec![];
    for segment in data.code_segments.iter() {
        buf.extend(segment.bytes.clone());
    }
    let name_id = writer.add_section_name(".text".as_bytes());
    //data.rx.blocks.push(BufferSection { name_id, buf: buf.to_vec() });
    blocks.push(BufferSection {
        alloc: AllocSegment::RX,
        name_id, addr: 0, offset: 0, size: 0,
        //align: 0x10,
        buf: buf.to_vec(),
    });
    //let s = AllocateSection::new(buf, 0x20, page_align, AllocSegment::RX).name_section(name_id);
    //data.rx.components.push(Box::new(s));
    //rx_components.push(Box::new(s));
    //let s = SegmentSection::new(AllocSegment::RX, 0x20);//.name_section(name_id);
    //components.push(Box::new(s));

    // .data
    let mut buf = vec![];
    for segment in data.data_segments.iter() {
        buf.extend(segment.bytes.clone());
        //data.rw.blocks.push(segment.bytes.clone());
    }
    let name_id = writer.add_section_name(".data".as_bytes());
    //data.rw.blocks.push(BufferSection { name_id, buf: buf.to_vec() });
    blocks.push(BufferSection {
        alloc: AllocSegment::RW,
        name_id, addr: 0, offset: 0, size: 0,
        //align: 0x10,
        buf: buf.to_vec(),
    });
    //let s = AllocateSection::new(buf, 0x20, page_align, AllocSegment::RW).name_section(name_id);
    //let s = SegmentSection::new(AllocSegment::RW, 0x20);//.name_section(name_id);
    //components.push(Box::new(s));

    components.push(Box::new(DynamicSection::default()));

    components.push(Box::new(DynStrSection::default()));
    components.push(Box::new(DynSymSection::default()));
    components.push(Box::new(StrTabSection::default()));

    // shstrtab needs to be allocated last, once all headers are reserved
    components.push(Box::new(ShStrTabSection::default()));

    let string_id = writer.add_dynamic_string("libc.so.6".as_bytes());
    data.add_library(&mut writer, string_id);

    // RESERVE

    //data.reserve_dynamic(&mut writer);
    data.reserve_header(&mut writer);
    let null_section_index = writer.reserve_null_section_index();

    for c in ro_components.iter_mut() {
        c.reserve(&mut data, &mut writer);
    }

    for b in blocks.iter() {
        b.reserve(&mut writer);
    }

    //data.rx.reserve(&mut data, &mut writer);
    //for c in rx_components.iter() {
    //c.reserve(&mut data, &mut writer);
    //}
    //for c in rw_components.iter() {
    //c.reserve(&mut data, &mut writer);
    //}

    //data.reserve(&mut writer);
    //data.reserve_sections(&mut writer, &mut components);
    components.iter_mut().for_each(|c| {
        c.reserve(&mut data, &mut writer);
    });

    writer.reserve_section_headers();

    data.update_segments();

    // UPDATE
    for c in ro_components.iter_mut() {
        c.update(&mut data);
    }
    for c in components.iter() {
        c.update(&mut data);
    }


    for b in blocks.iter_mut() {
        b.update(&mut data);
    }

    // WRITE
    data.write_header(&mut writer)?;

    for c in ro_components.iter_mut() {
        c.write(&data, &mut writer);
    }

    for b in blocks.iter_mut() {
        b.write(&mut writer);
    }

    for c in components.iter() {
        c.write(&data, &mut writer);
    }

    // write symbols
    writer.write_null_symbol();

    // write section headers
    writer.write_null_section_header();
    for c in ro_components.iter_mut() {
        c.write_section_header(&data, &mut writer);
    }

    //let mut offset = 0;
    for b in blocks.iter() {
        //b.write_section_header(&mut writer);
        //let size = b.buf.len();
        b.write_section_header(&mut writer);
        /*
        writer.write_section_header(&object::write::elf::SectionHeader {
            name: Some(b.name_id),
            sh_type: elf::SHT_PROGBITS,
            sh_flags: data.rx.flags as u64,
            sh_addr: data.rx.addr,
            sh_offset: data.rx.offset, // + offset,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign: data.rx.align as u64,
            sh_size: data.rx.size as u64,
        });
        */
        //offset += size as u64;
    }

    //for b in rw_blocks.iter() {
        //b.write_section_header(&mut writer);
        //let size = b.buf.len();
        //b.write_section_header(&mut writer);
        /*
        writer.write_section_header(&object::write::elf::SectionHeader {
            name: Some(b.name_id),
            sh_type: elf::SHT_PROGBITS,
            sh_flags: data.rw.flags as u64,
            sh_addr: data.rw.addr,
            sh_offset: data.rw.offset, // + offset,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign: data.rw.align as u64,
            sh_size: data.rw.size as u64,
        });
        */
        //offset += size as u64;
    //}

    for c in components.iter() {
        c.write_section_header(&data, &mut writer);
    }

    //data.write_sections(&mut writer, &components)?;

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
