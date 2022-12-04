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
    fn write(&self, data: &Data, w: &mut Writer) {}
    fn write_section_header(&self, data: &Data, w: &mut Writer) {}
}

#[derive(Default)]
struct DynamicSection {
    index: Option<SectionIndex>,
}

impl ElfComponent for DynamicSection {
    fn reserve(&mut self, data: &mut Data, w: &mut Writer) {
        let name = Some(w.add_section_name(".dynamic".as_bytes()));
        let dynamic_index = w.reserve_dynamic_section_index();
        let before = w.reserved_len();
        let align_offset = size_align(before, 0x10);
        w.reserve_until(align_offset);
        w.reserve_dynamic(data.dynamic.len());
        let after = w.reserved_len();
        data.addr_dynamic = data.rw.addr + align_offset as u64;

        // allocate space in the rw segment
        data.rw.size += (after - before) as u64;
    }

    fn write(&self, data: &Data, w: &mut Writer) {
        // write dynamic
        let pos = w.len();
        let aligned_pos = size_align(pos, 0x10);
        w.pad_until(aligned_pos);
        w.write_align_dynamic();
        for d in data.dynamic.iter() {
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
    //name: &'a str,
    //name_bytes: Vec<u8>,
    data: Vec<u8>,
    name_id: Option<StringId>,
    align: usize,
    addr: u64,
    alloc: AllocSegment,
    _p: std::marker::PhantomData<&'a u8>,
}

impl<'a> AllocateSection<'a> {
    pub fn new(data: Vec<u8>, align: usize, alloc: AllocSegment) -> Self {
        //let name_bytes = name.as_bytes().to_vec();
        //let name_id = Some(w.add_section_name(name.as_bytes()));
        Self {
            data, //: data.to_vec(),
            //name,
            //name_bytes,
            name_id: None,
            align,
            addr: 0,
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
        //self.name_id = Some(w.add_section_name(self.name_bytes.as_slice()));
        //self.name_id = Some(w.add_section_name(self.name.as_bytes()));
        let index = w.reserve_section_index();
        self.addr = w.reserve(self.data.len(), self.align as usize) as u64;
    }

    fn write(&self, data: &Data, w: &mut Writer) {
        w.write_align(self.align as usize);
        w.write(self.data.as_slice());
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        w.write_section_header(&object::write::elf::SectionHeader {
            name: self.name_id,
            sh_type: elf::SHT_PROGBITS,
            sh_flags: elf::SHF_ALLOC as u64,
            sh_addr: data.ro.addr + self.addr as u64,
            sh_offset: self.addr,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign: self.align as u64,
            sh_size: self.data.len() as u64,
        });
    }
}

#[derive(Default)]
struct DynSymSection {
    index: Option<SectionIndex>,
}

impl ElfComponent for DynSymSection {
    fn reserve(&mut self, data: &mut Data, w: &mut Writer) {
        //w.reserve_dynamic_symbol_index();
        w.reserve_null_dynamic_symbol_index();

        data.index_dynsym = Some(w.reserve_dynsym_section_index());
        let before = w.reserved_len();
        w.reserve_dynsym();
        let after = w.reserved_len();
        data.addr_dynsym = data.ro.addr;
    }

    fn write(&self, data: &Data, w: &mut Writer) {
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

struct Segment {
    addr: u64,
    size: u64,
    align: u32,
}
impl Default for Segment {
    fn default() -> Self {
        Self {
            addr: 0,
            size: 0,
            align: 0x1000,
        }
    }
}

struct Data {
    interp: Option<String>,
    is_64: bool,
    code_segments: Vec<UnlinkedCodeSegment>,
    data_segments: Vec<UnlinkedCodeSegment>,
    ph: Vec<ProgramHeaderEntry>,
    dynamic: Vec<Dynamic>,
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

        let mut ro = Segment::default();
        ro.size = ro_size as u64;
        let mut rw = Segment::default();
        rw.size = rw_size as u64;
        let mut rx = Segment::default();
        rx.size = rx_size as u64;

        Self {
            is_64: true,
            interp: Some("/lib/ld-linux-x86-64.so.2".to_string()),
            code_segments,
            data_segments,
            ph: vec![],
            dynamic: vec![],
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
        }
    }

    fn reserve_dynamic(&mut self, w: &mut Writer) {
        let string = Some(w.add_dynamic_string("libc.so.6".as_bytes()));
        self.dynamic.push(Dynamic {
            tag: elf::DT_NEEDED,
            val: 0,
            string,
        });
        self.dynamic.push(Dynamic {
            tag: elf::DT_DEBUG,
            val: 0,
            string: None,
        });
        self.dynamic.push(Dynamic {
            tag: elf::DT_SYMTAB,
            val: 0,
            string: None,
        });
        self.dynamic.push(Dynamic {
            tag: elf::DT_NULL,
            val: 0,
            string: None,
        });
    }

    fn reserve_header(&mut self, w: &mut Writer) {
        // Start reserving file ranges.
        w.reserve_file_header();
        let offset_fh = w.reserved_len();

        // calculate the number of headers
        let mut ph_count = 1; // the header itself
        if let Some(_) = self.interp {
            ph_count += 1;
        }

        let rw = self.rw.size > 0 || self.dynamic.len() > 0;
        if rw {
            ph_count += 1;
        }
        let rx = self.rx.size > 0;
        if rx {
            ph_count += 1;
        }
        let ro = true;
        if ro {
            ph_count += 1;
        }
        ph_count += 1; // dynamic

        let before = w.reserved_len();
        w.reserve_program_headers(ph_count as u32);
        let after = w.reserved_len();
        let ph_size = after - before;

        self.ph.push(ProgramHeaderEntry {
            p_type: elf::PT_PHDR,
            p_flags: elf::PF_R,
            p_offset: offset_fh as u64, // calculate later
            p_vaddr: self.ro.addr as u64 + offset_fh as u64,
            p_paddr: self.ro.addr as u64 + offset_fh as u64,
            p_filesz: ph_size as u64,
            p_memsz: ph_size as u64,
            p_align: 8,
        });
        let mut offset = offset_fh + ph_size;

        // add headers to the ro_size
        self.ro.size += offset as u64;

        // load segments
        // program LOAD (R)
        let ro_size_page_aligned = size_align(self.ro.size as usize, self.page_size as usize);
        if ro {
            let ro_size_elf_aligned = size_align(self.ro.size as usize, 16);
            self.ph.push(ProgramHeaderEntry {
                p_type: elf::PT_LOAD,
                p_flags: elf::PF_R,
                p_offset: 0, // read section starts at 0 offset to include headers
                p_vaddr: self.ro.addr as u64,
                p_paddr: self.ro.addr as u64,
                p_filesz: self.ro.size,
                p_memsz: self.ro.size,
                p_align: self.page_size as u64,
            });
            offset += ro_size_elf_aligned;
        }

        self.rx.addr = self.ro.addr + ro_size_page_aligned as u64;
        let rx_size_page_aligned = size_align(self.rx.size as usize, self.page_size as usize);
        if rx {
            // program LOAD (RX)
            //let rx_offset = offset;
            let rx_size_elf_aligned = size_align(self.rx.size as usize, 16);
            self.ph.push(ProgramHeaderEntry {
                p_type: elf::PT_LOAD,
                p_flags: elf::PF_R | elf::PF_X,
                p_offset: offset as u64,
                p_vaddr: (self.rx.addr + offset as u64),
                p_paddr: (self.rx.addr + offset as u64),
                p_filesz: self.rx.size,
                p_memsz: self.rx.size,
                p_align: self.page_size as u64,
            });
            offset += rx_size_elf_aligned;
        }

        self.rw.addr = self.rx.addr + rx_size_page_aligned as u64;
        let rw_size_page_aligned = size_align(self.rw.size as usize, self.page_size as usize);
        if rw {
            // program LOAD (RW)
            //let rw_offset = offset;
            let rw_size_elf_aligned = size_align(self.rw.size as usize, 16);
            self.ph.push(ProgramHeaderEntry {
                p_type: elf::PT_LOAD,
                p_flags: elf::PF_R | elf::PF_W,
                p_offset: offset as u64,
                p_vaddr: self.rw.addr + offset as u64,
                p_paddr: self.rw.addr + offset as u64,
                p_filesz: self.rw.size,
                p_memsz: self.rw.size,
                p_align: self.page_size as u64,
            });
            offset += self.rw.size as usize; //_elf_aligned;
        }

        if let Some(interp) = &self.interp {
            self.ph.push(ProgramHeaderEntry {
                p_type: elf::PT_INTERP,
                p_flags: elf::PF_R,
                p_offset: offset as u64,
                p_vaddr: self.ro.addr + offset as u64,
                p_paddr: self.ro.addr + offset as u64,
                p_filesz: interp.as_bytes().len() as u64,
                p_memsz: interp.as_bytes().len() as u64,
                p_align: 1,
            });
            offset += interp.len();
        }

        //let before = writer.reserved_len();
        //writer.reserve_dynamic(out_dynamic.len());
        //let after = writer.reserved_len();
        //let dynamic_size = after - before;
        if self.dynamic.len() > 0 {
            let dynamic_size = 0;
            //program DYNAMIC
            self.ph.push(ProgramHeaderEntry {
                p_type: elf::PT_DYNAMIC,
                p_flags: elf::PF_R | elf::PF_W,
                p_offset: offset as u64,
                p_vaddr: self.rw.addr + offset as u64,
                p_paddr: self.rw.addr + offset as u64,
                p_filesz: dynamic_size,
                p_memsz: dynamic_size,
                p_align: 0x8,
            });
        }
    }

    fn reserve_sections(&mut self, w: &mut Writer, components: &mut Vec<Box<dyn ElfComponent>>) {
        let null_section_index = w.reserve_null_section_index();
        //self.reserve_section_interp(w);
        self.reserve_section_data(w);
        self.reserve_section_code(w);

        components.iter_mut().for_each(|c| {
            c.reserve(self, w);
        });

        w.reserve_section_headers();
    }

    /*
    fn reserve_section_interp(&mut self, w: &mut Writer) {
        if let Some(interp) = &self.interp {
            let name = Some(w.add_section_name(".interp".as_bytes()));
            let index = w.reserve_section_index();
            let align = 0x10;
            let start = w.reserve(interp.len(), align);
            self.sections.push(Section {
                name,
                sh_type: elf::SHT_PROGBITS,
                sh_flags: elf::SHF_ALLOC as usize,
                sh_name: 0,
                sh_addr: (self.ro.addr + start as u64) as usize,
                sh_offset: start as usize,
                sh_info: 0,
                sh_link: 0,
                sh_entsize: 0,
                sh_addralign: align,
                data: interp.as_bytes().to_vec(),
            });
        }
    }
    */

    fn reserve_section_data(&mut self, w: &mut Writer) {
        if self.data_segments.len() > 0 {
            let name = Some(w.add_section_name(".data".as_bytes()));
            let data_index = w.reserve_section_index();

            let mut data = vec![];
            for segment in self.data_segments.iter() {
                data.extend(segment.bytes.clone());
            }

            //offset += data.len() as u64;
            let align = 0x1;
            let start = w.reserve(data.len(), align);

            self.sections.push(Section {
                name,
                sh_type: elf::SHT_PROGBITS,
                sh_flags: elf::SHF_ALLOC as usize,
                sh_name: 0,
                sh_addr: (self.rw.addr + start as u64) as usize,
                sh_offset: start, //data_offset as usize,
                sh_info: 0,
                sh_link: 0,
                sh_entsize: 0,
                sh_addralign: 1,
                data,
            });
        }
    }

    fn reserve_section_code(&mut self, w: &mut Writer) {
        if self.code_segments.len() > 0 {
            let name = Some(w.add_section_name(".text".as_bytes()));
            let text_index = w.reserve_section_index();
            let mut data = vec![];
            for segment in self.code_segments.iter() {
                data.extend(segment.bytes.clone());
            }
            let align = 0x20;
            let start = w.reserve(data.len(), align);

            self.addr_text = self.rx.addr + start as u64;
            self.sections.push(Section {
                name,
                sh_type: elf::SHT_PROGBITS,
                sh_flags: (elf::SHF_ALLOC | elf::SHF_EXECINSTR) as usize,
                sh_name: 0,
                sh_addr: (self.rx.addr as u64 + start as u64) as usize,
                sh_offset: start,
                sh_info: 0,
                sh_link: 0,
                sh_entsize: 0,
                sh_addralign: align,
                data,
            });
        }
    }

    fn write_sections(
        &self,
        w: &mut Writer,
        components: &Vec<Box<dyn ElfComponent>>,
    ) -> Result<()> {
        for section in self.sections.iter() {
            //w.pad_until(section.sh_offset as usize);
            match (section.sh_type, section.is_alloc()) {
                (
                    elf::SHT_PROGBITS | elf::SHT_NOTE | elf::SHT_INIT_ARRAY | elf::SHT_FINI_ARRAY,
                    true,
                ) => {
                    //eprintln!("write: {:?}", section);
                    w.write_align(section.sh_addralign as usize);
                    w.write(section.data.as_slice());
                }
                //(elf::SHT_DYNAMIC, true) => {
                //}
                //elf::SHT_DYNSYM => {
                //writer.write_null_dynamic_symbol();
                //}
                //elf::SHT_STRTAB => {
                //}
                _ => (),
            }
        }

        for c in components {
            c.write(self, w);
        }

        // write symbols
        w.write_null_symbol();

        // write section headers
        w.write_null_section_header();

        for section in self.sections.iter() {
            match section.sh_type {
                //elf::SHT_NULL => (),
                elf::SHT_PROGBITS | elf::SHT_NOTE => {
                    w.write_section_header(&object::write::elf::SectionHeader {
                        name: section.name,
                        sh_type: section.sh_type,
                        sh_flags: section.sh_flags as u64,
                        sh_addr: section.sh_addr as u64,
                        sh_offset: section.sh_offset as u64,
                        sh_size: section.data.len() as u64,
                        sh_link: section.sh_link,
                        sh_info: section.sh_info,
                        sh_addralign: section.sh_addralign as u64,
                        sh_entsize: section.sh_entsize as u64,
                    });
                }
                elf::SHT_STRTAB => (),
                elf::SHT_DYNAMIC => {}
                _ => unimplemented!(),
            }
        }

        for c in components {
            c.write_section_header(self, w);
        }

        Ok(())
    }

    fn write_header(&self, w: &mut Writer) -> Result<()> {
        w.write_file_header(&object::write::elf::FileHeader {
            os_abi: 0x00,            // SysV
            abi_version: 0,          // ignored on linux
            e_type: 0x02,            // ET_EXEC - Executable file
            e_machine: 0x3E,         // AMD x86-64
            e_entry: self.addr_text, // e_entry, normally points to _start
            e_flags: 0,              // e_flags
        })?;

        w.write_align_program_headers();
        for ph in self.ph.iter() {
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

    let mut components: Vec<Box<dyn ElfComponent>> = vec![];
    let name_id = writer.add_section_name(".interp".as_bytes());
    if data.interp.is_some() {
        let interp = data.interp.take().unwrap_or(String::default()).into_bytes();
        let s = AllocateSection::new(interp.clone(), 0x10, AllocSegment::RO).name_section(name_id);
        components.push(Box::new(s));
    }
    components.push(Box::new(DynStrSection::default()));
    components.push(Box::new(DynSymSection::default()));
    components.push(Box::new(StrTabSection::default()));
    components.push(Box::new(DynamicSection::default()));

    // shstrtab needs to be allocated last, once all headers are reserved
    components.push(Box::new(ShStrTabSection::default()));

    data.reserve_dynamic(&mut writer);
    data.reserve_header(&mut writer);
    data.reserve_sections(&mut writer, &mut components);
    data.write_header(&mut writer)?;
    data.write_sections(&mut writer, &components)?;
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
