use std::error::Error;

use object::elf;
use object::read::elf::{Dyn, FileHeader, ProgramHeader, Rel, Rela, SectionHeader, Sym};
use object::write::elf::{Writer, SectionIndex};
use object::write::Result;
use object::Endianness;

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

struct Data {
    interp: Option<String>,
    is_64: bool,
    code_segments: Vec<UnlinkedCodeSegment>,
    data_segments: Vec<UnlinkedCodeSegment>,
    ph: Vec<ProgramHeaderEntry>,
    dynamic: Vec<Dynamic>,
    sections: Vec<Section>,
    page_size: u32,
    ro_size: u64,
    rw_size: u64,
    rx_size: u64,
    ro_addr_start: u64,
    rx_addr_start: u64,
    rw_addr_start: u64,
    addr_dynamic: u64,
    addr_dynstr: u64,
    index_strtab: Option<SectionIndex>,
    //index_dynstr: Option<SectionIndex>,
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
                K::Data
                | K::UninitializedData => {
                    rw_size += unlinked.bytes.len();
                    data_segments.push(unlinked.clone());
                }
                K::OtherString
                | K::ReadOnlyString
                | K::ReadOnlyData => {
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

        Self {
            is_64: true,
            interp: Some("/lib/ld-linux-x86-64.so.2".to_string()),
            code_segments,
            data_segments,
            ph: vec![],
            dynamic: vec![],
            sections: vec![],
            page_size: 0x1000,
            ro_size: ro_size as u64,
            rw_size: rw_size as u64,
            rx_size: rx_size as u64,
            ro_addr_start: 0x200000,
            rx_addr_start: 0x200000,
            rw_addr_start: 0x200000,
            addr_dynamic: 0,
            addr_dynstr: 0,
            index_strtab: None,
            //index_dynstr: None,
        }
    }

    fn reserve_dynamic(&mut self, w: &mut Writer) {
        let string = Some(w.add_dynamic_string("libc.so.6".as_bytes()));
        self.dynamic.push(Dynamic { tag: elf::DT_NEEDED, val: 0, string });
        self.dynamic.push(Dynamic { tag: elf::DT_DEBUG, val: 0, string: None });
        self.dynamic.push(Dynamic { tag: elf::DT_SYMTAB, val: 0, string: None });
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
        if self.code_segments.len() > 0 {
            ph_count += 1;
        }
        if self.data_segments.len() > 0 {
            ph_count += 2; // ro, rw
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
            p_vaddr: self.ro_addr_start as u64 + offset_fh as u64,
            p_paddr: self.ro_addr_start as u64 + offset_fh as u64,
            p_filesz: ph_size as u64,
            p_memsz: ph_size as u64,
            p_align: 8,
        });
        let mut offset = offset_fh + ph_size;

        if let Some(interp) = &self.interp {
            self.ph.push(ProgramHeaderEntry {
                p_type: elf::PT_INTERP,
                p_flags: elf::PF_R,
                p_offset: offset as u64,
                p_vaddr: self.ro_addr_start + offset as u64,
                p_paddr: self.ro_addr_start + offset as u64,
                p_filesz: interp.len() as u64,
                p_memsz: interp.len() as u64,
                p_align: 1,
            });
            offset += interp.len();
        }

        // add headers to the ro_size
        self.ro_size += offset as u64;

        // load segments
        // program LOAD (R)
        let ro_size_page_aligned = size_align(self.ro_size as usize, self.page_size as usize);
        if self.data_segments.len() > 0 {
            let ro_size_elf_aligned = size_align(self.ro_size as usize, 16);
            self.ph.push(ProgramHeaderEntry {
                p_type: elf::PT_LOAD,
                p_flags: elf::PF_R,
                p_offset: 0, // read section starts at 0 offset to include headers
                p_vaddr: self.ro_addr_start as u64,
                p_paddr: self.ro_addr_start as u64,
                p_filesz: self.ro_size,
                p_memsz: self.ro_size,
                p_align: self.page_size as u64,
            });
            offset += ro_size_elf_aligned;
        }

        self.rx_addr_start = self.ro_addr_start + ro_size_page_aligned as u64;
        let rx_size_page_aligned = size_align(self.rx_size as usize, self.page_size as usize);
        if self.code_segments.len() > 0 {
            // program LOAD (RX)
            //let rx_offset = offset;
            let rx_size_elf_aligned = size_align(self.rx_size as usize, 16);
            self.ph.push(ProgramHeaderEntry {
                p_type: elf::PT_LOAD,
                p_flags: elf::PF_R | elf::PF_X,
                p_offset: offset as u64,
                p_vaddr: (self.rx_addr_start + offset as u64),
                p_paddr: (self.rx_addr_start + offset as u64),
                p_filesz: self.rx_size,
                p_memsz: self.rx_size,
                p_align: self.page_size as u64,
            });
            offset += rx_size_elf_aligned;
        }

        self.rw_addr_start = self.rx_addr_start + rx_size_page_aligned as u64;
        let rw_size_page_aligned = size_align(self.rw_size as usize, self.page_size as usize);
        if self.data_segments.len() > 0 {
            // program LOAD (RW)
            //let rw_offset = offset;
            let rw_size_elf_aligned = size_align(self.rw_size as usize, 16);
            self.ph.push(ProgramHeaderEntry {
                p_type: elf::PT_LOAD,
                p_flags: elf::PF_R | elf::PF_W,
                p_offset: offset as u64,
                p_vaddr: self.rw_addr_start + offset as u64,
                p_paddr: self.rw_addr_start + offset as u64,
                p_filesz: self.rw_size,
                p_memsz: self.rw_size,
                p_align: self.page_size as u64,
            });
            offset += self.rw_size as usize;//_elf_aligned;
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
                p_vaddr: self.rw_addr_start + offset as u64,
                p_paddr: self.rw_addr_start + offset as u64,
                p_filesz: dynamic_size,
                p_memsz: dynamic_size,
                p_align: 0x8,
            });
        }
    }

    fn reserve_sections(&mut self, w: &mut Writer) {
        let null_section_index = w.reserve_null_section_index();
        self.reserve_section_interp(w);
        self.reserve_section_data(w);
        self.reserve_section_code(w);
        self.reserve_section_dynstr(w);
        self.reserve_section_strtab(w);
        self.reserve_section_dynamic(w);

        // shstrtab needs to be allocated last, once all headers are reserved
        self.reserve_section_shstrtab(w);
        w.reserve_section_headers();
    }

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
                sh_addr: (self.ro_addr_start + start as u64) as usize,
                sh_offset: start as usize,
                sh_info: 0,
                sh_link: 0,
                sh_entsize: 0,
                sh_addralign: align,
                data: interp.as_bytes().to_vec(),
            });
        }
    }

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
                sh_addr: (self.rw_addr_start + start as u64) as usize,
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
            //out_sections_index.push(text_index);

            //let text_offset = offset;
            let mut data = vec![];
            for segment in self.code_segments.iter() {
                data.extend(segment.bytes.clone());
            }
            //offset += data.len() as u64;
            let align = 0x20;
            let start = w.reserve(data.len(), align);

            //offset = start as u64 + data.len() as u64;
            self.sections.push(Section {
                name,
                sh_type: elf::SHT_PROGBITS,
                sh_flags: (elf::SHF_ALLOC | elf::SHF_EXECINSTR) as usize,
                sh_name: 0,
                sh_addr: (self.rx_addr_start as u64 + start as u64) as usize,
                sh_offset: start, //offset as usize,
                sh_info: 0,
                sh_link: 0,
                sh_entsize: 0,
                sh_addralign: align,
                data,
            });
        }
    }

    fn reserve_section_dynstr(&mut self, w: &mut Writer) {
        //let name = Some(w.add_section_name(".dynstr".as_bytes()));
        let index_dynstr = Some(w.reserve_dynstr_section_index());
        //out_sections_index.push(shstrtab_index);
        let before = w.reserved_len();
        w.reserve_dynstr();
        let after = w.reserved_len();
        self.addr_dynstr = self.rw_addr_start; // + offset;
    }

    fn reserve_section_strtab(&mut self, w: &mut Writer) {
        let name = Some(w.add_section_name(".strtab".as_bytes()));
        self.index_strtab = Some(w.reserve_strtab_section_index());
        //out_sections_index.push(shstrtab_index);
        let before = w.reserved_len();
        w.reserve_strtab();
        let after = w.reserved_len();
        /*
        self.sections.push(Section {
            name,
            sh_type: elf::SHT_STRTAB,
            sh_flags: 0,
            sh_name: 0, //shstrtab_offset as u32, // set offset to name later when it's allocated
            sh_addr: (self.ro_addr_start as usize), // + offset) as usize,
            sh_offset: 0, //offset as usize,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign: 1,
            data: vec![],
        });
        //offset += (after - before) as u64;
        */
    }

    fn reserve_section_shstrtab(&mut self, w: &mut Writer) {
        let name = Some(w.add_section_name(".shstrtab".as_bytes()));
        let shstrtab_index = w.reserve_shstrtab_section_index();
        //out_sections_index.push(shstrtab_index);
        let before = w.reserved_len();
        w.reserve_shstrtab();
        let after = w.reserved_len();
        /*
        self.sections.push(Section {
            name,
            sh_type: elf::SHT_STRTAB,
            sh_flags: 0,
            sh_name: 0, //shstrtab_offset as u32, // set offset to name later when it's allocated
            sh_addr: (self.ro_addr_start as usize), // + offset) as usize,
            sh_offset: 0, //offset as usize,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign: 1,
            data: vec![],
        });
        //offset += (after - before) as u64;
        */
    }

    fn reserve_section_dynamic(&mut self, w: &mut Writer) {
        if self.dynamic.len() > 0 {
            let name = Some(w.add_section_name(".dynamic".as_bytes()));
            let dynamic_index = w.reserve_dynamic_section_index();
            //out_sections_index.push(dynamic_index);
            self.addr_dynamic = self.rw_addr_start; // + offset;
            //let before = w.reserved_len();
            w.reserve_dynamic(self.dynamic.len());
            //let after = w.reserved_len();
            /*
            self.sections.push(Section {
                name,
                sh_type: elf::SHT_DYNAMIC,
                sh_flags: (elf::SHF_ALLOC | elf::SHF_WRITE) as usize,
                sh_name: 0, // set offset to name later when it's allocated
                sh_addr: self.addr_dynamic as usize,
                sh_offset: 0,//before as usize,
                sh_info: 0,
                sh_link: w.dynstr_index().0, // reference dynstr
                sh_entsize: 0x10,
                sh_addralign: 8,
                data: vec![],
            });
            */

            //offset += dynamic_size as u64;
        }
    }

    fn write_sections(&self, w: &mut Writer) -> Result<()> {
        for section in self.sections.iter() {
            //w.pad_until(section.sh_offset as usize);
            match (section.sh_type, section.is_alloc()) {
                (
                    elf::SHT_PROGBITS | elf::SHT_NOTE | elf::SHT_INIT_ARRAY | elf::SHT_FINI_ARRAY,
                    true,
                ) => {
                    eprintln!("write: {:?}", section);
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
        w.write_dynstr();
        w.write_strtab();

        // write dynamic
        //eprintln!("write: {:?}", section);
        w.write_align_dynamic();
        for d in self.dynamic.iter() {
            if let Some(string) = d.string {
                w.write_dynamic_string(d.tag, string);
            } else {
                w.write_dynamic(d.tag, d.val);
            }
        }

        w.write_shstrtab();

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
                elf::SHT_DYNAMIC => {
                }
                _ => unimplemented!(),
            }
        }
        w.write_dynstr_section_header(self.addr_dynstr);
        w.write_strtab_section_header();
        w.write_dynamic_section_header(self.addr_dynamic);
        w.write_shstrtab_section_header();

        Ok(())
    }

    fn write_header(&self, w: &mut Writer) -> Result<()> {
        w.write_file_header(&object::write::elf::FileHeader {
            os_abi: 0x00,    // SysV
            abi_version: 0,  // ignored on linux
            e_type: 0x02,    // ET_EXEC - Executable file
            e_machine: 0x3E, // AMD x86-64
            e_entry: 0,      // e_entry
            e_flags: 0,      // e_flags
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
    data.reserve_dynamic(&mut writer);
    data.reserve_header(&mut writer);
    data.reserve_sections(&mut writer);
    data.write_header(&mut writer)?;
    data.write_sections(&mut writer)?;
    Ok(out_data)
}

pub fn write_file2<Elf: FileHeader<Endian = Endianness>>(
    link: &Link,
) -> std::result::Result<Vec<u8>, Box<dyn Error>> {
    let mut out_data = Vec::new();
    let endian = Endianness::Little;
    let mut data = Data::new(link);
    let is_class_64 = data.is_64;
    let mut writer = object::write::elf::Writer::new(endian, is_class_64, &mut out_data);

    // Find metadata sections, and assign section indices.
    //let mut in_dynamic = None;
    //let mut in_hash = None;
    //let mut in_gnu_hash = None;
    //let mut in_versym = None;
    //let mut in_verdef = None;
    //let mut in_verneed = None;
    //let mut out_ph = Vec::new();
    let mut out_sections = Vec::new();
    let mut out_sections_index = Vec::new();
    //let mut alloc_sections = Vec::new();

    // get a list of sections
    // add the section

    // Start reserving file ranges.
    //writer.reserve_file_header();
    //let offset_fh = writer.reserved_len();

    // null section is first
    //let null_section_index = writer.reserve_null_section_index();

    // virtual address start
    let ro_addr_start = 0x200000;
    let rx_addr_start = 0x201000;
    let rw_addr_start = 0x202000;

    let mut shstrtab_offset = 0;

    // reserve program headers

    let mut offset = 0; //offset_fh;
    let offset_fh = 0;
    let mut out_dynamic: Vec<u8> = Vec::new();

    data.reserve_header(&mut writer);

    /*
    // load segments
    //
    // program LOAD (R)
    let ro_size = 0x210;
    out_ph.push(ProgramHeaderEntry {
        p_type: elf::PT_LOAD,
        p_flags: elf::PF_R,
        p_offset: 0, // read section starts at 0 offset to include headers
        p_vaddr: ro_addr_start as u64,
        p_paddr: ro_addr_start as u64,
        p_filesz: ro_size,
        p_memsz: ro_size,
        p_align: 0x1000,
    });
    //offset += ro_size;

    // program LOAD (RX)
    let rx_offset = offset;
    let rx_size = 0x210;
    out_ph.push(ProgramHeaderEntry {
        p_type: elf::PT_LOAD,
        p_flags: elf::PF_R | elf::PF_X,
        p_offset: 0,//offset,
        p_vaddr: (rx_addr_start + offset) as u64,
        p_paddr: (rx_addr_start + offset) as u64,
        p_filesz: rx_size,
        p_memsz: rx_size,
        p_align: 0x1000,
    });
    //offset += rx_size;

    // program LOAD (RW)
    let rw_offset = rx_offset + 0x1000;
    let rw_size = 0x210;
    out_ph.push(ProgramHeaderEntry {
        p_type: elf::PT_LOAD,
        p_flags: elf::PF_R | elf::PF_W,
        p_offset: 0,//rw_offset,
        p_vaddr: rw_addr_start as u64,
        p_paddr: rw_addr_start as u64,
        p_filesz: rw_size,
        p_memsz: rw_size,
        p_align: 0x1000,
    });
    //offset += rx_size;

    let mut headers_count = 1 + out_ph.len();
    let before = writer.reserved_len();
    writer.reserve_program_headers(headers_count as u32);
    let after = writer.reserved_len();
    let ph_size = (after - before) as u64;


    out_ph.insert(0, ProgramHeaderEntry {
        p_type: elf::PT_PHDR,
        p_flags: elf::PF_R,
        p_offset: offset_fh as u64, // calculate later
        p_vaddr: ro_addr_start + offset_fh as u64,
        p_paddr: ro_addr_start + offset_fh as u64,
        p_filesz: ph_size as u64,
        p_memsz: ph_size as u64,
        p_align: 8,
    });

    let mut offset = offset_fh;
    for ph in out_ph.iter() {
        offset += ph.p_filesz as usize;
    }


    //let mut offset = offset_fh as u64;

    //offset += ph_size;

    // Assign dynamic strings.

    if false {
        let string = Some(writer.add_dynamic_string("libc.so.6".as_bytes()));
        out_dynamic.push(Dynamic { tag: elf::DT_NEEDED, val: 0, string });
        out_dynamic.push(Dynamic { tag: elf::DT_NULL, val: 0, string: None });
        //let before = writer.reserved_len();
        //writer.reserve_dynamic(out_dynamic.len());
        //let after = writer.reserved_len();
        //let dynamic_size = after - before;
        let dynamic_size = 0;
        //program DYNAMIC
        out_ph.push(ProgramHeaderEntry {
            p_type: elf::PT_DYNAMIC,
            p_flags: elf::PF_R | elf::PF_W,
            p_offset: 0,//rw_offset,
            p_vaddr: rw_addr_start as u64,
            p_paddr: rw_addr_start as u64,
            p_filesz: dynamic_size as u64,
            p_memsz: dynamic_size as u64,
            p_align: 0x8,
        });

        //offset += dynamic_size as u64;
    }
    */

    let offset = 0;
    let mut hash_addr = 0;
    let mut gnu_hash_addr = 0;
    let mut versym_addr = 0;
    let mut verdef_addr = 0;
    let mut verneed_addr = 0;
    let mut dynamic_addr = 0;
    let mut dynsym_addr = 0;
    let mut dynstr_addr = 0;

    /*
    if false {
        if let Some(interp) = &data.interp {
            let name = Some(writer.add_section_name(".interp".as_bytes()));
            let index = writer.reserve_section_index();
            let align = 1;
            let interp_offset = writer.reserve(interp.len(), align);
            out_sections.push(Section {
                name,
                sh_type: elf::SHT_PROGBITS,
                sh_flags: elf::SHF_ALLOC as usize,
                sh_name: shstrtab_offset as u32, // set offset to name later when it's allocated
                sh_addr: (ro_addr_start + offset) as usize,
                sh_offset: interp_offset as usize,
                sh_info: 0,
                sh_link: 0,
                sh_entsize: 0,
                sh_addralign: align,
                data: interp.as_bytes().to_vec(),
            });
            out_sections_index.push(index);
        }
    }
    */

    /*
    if false && data_sections.len() > 0 {
        let name = Some(writer.add_section_name(".data".as_bytes()));
        let data_index = writer.reserve_section_index();
        out_sections_index.push(data_index);

        let data_offset = offset;
        let mut data = vec![];
        for section in data_sections.iter() {
            data.extend(section.bytes.clone());
        }
        //offset += data.len() as u64;
        let align = 0x10;
        let start = writer.reserve(data.len(), align);

        out_sections.push(Section {
            name,
            sh_type: elf::SHT_PROGBITS,
            sh_flags: elf::SHF_ALLOC as usize,
            sh_name: 0,
            sh_addr: (rw_addr_start + start) as usize,
            sh_offset: data_offset as usize,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign: 1,
            data,
        });
    }

    if false && code_sections.len() > 0 {
        let name = Some(writer.add_section_name(".text".as_bytes()));
        let text_index = writer.reserve_section_index();
        out_sections_index.push(text_index);

        let text_offset = offset;
        let mut data = vec![];
        for section in code_sections.iter() {
            data.extend(section.bytes.clone());
        }
        //offset += data.len() as u64;
        let align = 0x10;
        let start = writer.reserve(data.len(), align);

        //offset = start as u64 + data.len() as u64;
        out_sections.push(Section {
            name,
            sh_type: elf::SHT_PROGBITS,
            sh_flags: (elf::SHF_ALLOC | elf::SHF_EXECINSTR) as usize,
            sh_name: 0,
            sh_addr: (rx_addr_start as u64 + start as u64) as usize,
            sh_offset: offset as usize,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign: align,
            data,
        });
    }
    */

    if false {
        let name = Some(writer.add_section_name(".dynamic".as_bytes()));
        let dynamic_index = writer.reserve_dynamic_section_index();
        out_sections_index.push(dynamic_index);
        dynamic_addr = rx_addr_start; // + offset;
        let before = writer.reserved_len();
        writer.reserve_dynamic(out_dynamic.len());
        let after = writer.reserved_len();
        out_sections.push(Section {
            name,
            sh_type: elf::SHT_DYNAMIC,
            sh_flags: (elf::SHF_ALLOC | elf::SHF_WRITE) as usize,
            sh_name: 0, // set offset to name later when it's allocated
            sh_addr: dynamic_addr as usize,
            sh_offset: offset as usize,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0x10,
            sh_addralign: 8,
            data: vec![],
        });
        //offset += (after - before) as u64;
    }

    if false {
        let name = Some(writer.add_section_name(".dynstr".as_bytes()));
        let dynstr_index = writer.reserve_dynstr_section_index();
        out_sections_index.push(dynstr_index);
        dynstr_addr = ro_addr_start + offset;
        let before = writer.reserved_len();
        writer.reserve_dynstr();
        let after = writer.reserved_len();
        out_sections.push(Section {
            name,
            sh_type: elf::SHT_STRTAB,
            sh_flags: elf::SHF_ALLOC as usize,
            sh_name: shstrtab_offset as u32, // set offset to name later when it's allocated
            sh_addr: dynstr_addr as usize,
            sh_offset: offset as usize,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign: 1,
            data: vec![],
        });
        //offset += (after - before) as u64;
    }

    if false {
        let name = Some(writer.add_section_name(".shstrtab".as_bytes()));
        let shstrtab_index = writer.reserve_shstrtab_section_index();
        out_sections_index.push(shstrtab_index);
        let before = writer.reserved_len();
        writer.reserve_shstrtab();
        let after = writer.reserved_len();
        out_sections.push(Section {
            name,
            sh_type: elf::SHT_STRTAB,
            sh_flags: 0,
            sh_name: shstrtab_offset as u32, // set offset to name later when it's allocated
            sh_addr: (ro_addr_start + offset) as usize,
            sh_offset: offset as usize,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign: 1,
            data: vec![],
        });
        //offset += (after - before) as u64;
    }

    if false {
        let name = Some(writer.add_section_name(".strtab".as_bytes()));
        let strtab_index = writer.reserve_strtab_section_index();
        out_sections_index.push(strtab_index);
        let before = writer.reserved_len();
        writer.reserve_strtab();
        let after = writer.reserved_len();
        out_sections.push(Section {
            name,
            sh_type: elf::SHT_STRTAB,
            sh_flags: 0,
            sh_name: shstrtab_offset as u32, // set offset to name later when it's allocated
            sh_addr: (ro_addr_start + offset) as usize,
            sh_offset: offset as usize,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign: 1,
            data: vec![],
        });
        //offset += (after - before) as u64;
    }

    //let dynamic_symbol_index = writer.reserve_dynamic_symbol_index();

    // Assign dynamic symbol indices.
    //let mut out_dynsyms = Vec::new();
    //let mut out_dynsyms_index = vec![];
    //for out_dynsym in out_dynsyms.iter_mut() {

    // allocate and adjust offsets
    for (i, section) in out_sections.iter_mut().enumerate() {
        match section.sh_type {
            elf::SHT_PROGBITS | elf::SHT_NOTE => {
                //section.sh_offset = writer.reserve(section.data.len(), section.sh_addralign);
            }
            elf::SHT_STRTAB => {}
            elf::SHT_DYNAMIC => {}
            _ => unimplemented!(),
        }
    }

    //writer.reserve_symtab();
    //writer.reserve_symtab_shndx();
    //writer.reserve_strtab();

    //writer.reserve_shstrtab();
    writer.reserve_section_headers();

    // WRITING

    data.write_header(&mut writer)?;
    /*
    writer.write_file_header(&object::write::elf::FileHeader {
        os_abi: 0x00,    // SysV
        abi_version: 0,  // ignored on linux
        e_type: 0x02,    // ET_EXEC - Executable file
        e_machine: 0x3E, // AMD x86-64
        e_entry: 0,      // e_entry
        e_flags: 0,      // e_flags
    })?;

    writer.write_align_program_headers();
    for ph in out_ph.iter() {
        writer.write_program_header(&object::write::elf::ProgramHeader {
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
    */

    for section in out_sections.iter() {
        //writer.pad_until(section.sh_offset as usize);
        match section.sh_type {
            elf::SHT_PROGBITS | elf::SHT_NOTE | elf::SHT_INIT_ARRAY | elf::SHT_FINI_ARRAY => {
                //writer.write_align(section.sh_addralign as usize);
                //writer.write(section.data.as_slice());
            }
            elf::SHT_DYNAMIC => {
                for d in out_dynamic.iter() {
                    //if let Some(string) = d.string {
                    //writer.write_dynamic_string(d.tag, string);
                    //} else {
                    //writer.write_dynamic(d.tag, d.val);
                    //}
                }
            }
            //elf::SHT_DYNSYM => {
            //writer.write_null_dynamic_symbol();
            //}
            elf::SHT_STRTAB => {}
            _ => (),
        }
    }

    writer.write_null_symbol();
    // write symbols

    //writer.write_symtab_shndx();
    //writer.write_shstrtab();
    //writer.write_strtab();

    // Write Section headers
    writer.write_null_section_header();

    // write the section headers
    for section in out_sections.iter() {
        match section.sh_type {
            elf::SHT_NULL => (),
            elf::SHT_PROGBITS | elf::SHT_NOTE => {
                writer.write_section_header(&object::write::elf::SectionHeader {
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
            elf::SHT_DYNAMIC => {
                //writer.write_dynamic_section_header(dynamic_addr);
            }
            _ => unimplemented!(),
        }
    }

    //writer.write_strtab_section_header();
    //writer.write_shstrtab_section_header();
    //writer.write_dynstr_section_header(dynstr_addr);

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
