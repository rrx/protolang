use std::error::Error;

use object::elf;
use object::read::elf::{Dyn, FileHeader, ProgramHeader, Rel, Rela, SectionHeader, Sym};
use object::Endianness;

use super::*;
use crate::*;

enum SectionKind {
    Interp
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
    data: Vec<u8>
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
}
impl Data {
    fn default() -> Self {
        Self {
            is_64: true,
            interp: Some("/lib/ld-linux-x86-64.so.2".to_string())
        }
    }
}

pub fn write_file<Elf: FileHeader<Endian = Endianness>>(
    link: &Link,
) -> Result<Vec<u8>, Box<dyn Error>> {
    let mut out_data = Vec::new();
    let endian = Endianness::Little;
    let data = Data::default();
    let is_class_64 = data.is_64;
    let mut writer = object::write::elf::Writer::new(endian, is_class_64, &mut out_data);

    // Find metadata sections, and assign section indices.
    //let mut in_dynamic = None;
    //let mut in_hash = None;
    //let mut in_gnu_hash = None;
    //let mut in_versym = None;
    //let mut in_verdef = None;
    //let mut in_verneed = None;
    let mut out_ph = Vec::new();
    let mut out_sections = Vec::new();
    let mut out_sections_index = Vec::new();
    //let mut alloc_sections = Vec::new();

    // get a list of sections
    // add the section

    // Start reserving file ranges.
    writer.reserve_file_header();
    let offset_fh = writer.reserved_len();

    // null section is first
    let null_section_index = writer.reserve_null_section_index();


    // virtual address start
    let ro_addr_start = 0x200000;
    let rx_addr_start = 0x201000;
    let rw_addr_start = 0x202000;

    let mut shstrtab_offset = 0;


    // reserve program headers
    let mut headers_count = 1+3; // program header + load segments
    if let Some(_) = data.interp {
        headers_count += 1;  //interp
    }
    let before = writer.reserved_len();
    writer.reserve_program_headers(headers_count);
    let after = writer.reserved_len();
    let ph_size = (after - before) as u64;

    let mut offset = offset_fh as u64;

    out_ph.push(ProgramHeaderEntry {
        p_type: elf::PT_PHDR,
        p_flags: elf::PF_R,
        p_offset: offset, // calculate later
        p_vaddr: ro_addr_start + offset,
        p_paddr: ro_addr_start + offset,
        p_filesz: ph_size as u64,
        p_memsz: ph_size as u64,
        p_align: 8,
    });

    offset += ph_size;



    if let Some(interp) = data.interp {
        out_ph.push(ProgramHeaderEntry {
            p_type: elf::PT_INTERP,
            p_flags: elf::PF_R,
            p_offset: offset, // calculate later
            p_vaddr: ro_addr_start + offset,
            p_paddr: ro_addr_start + offset,
            p_filesz: interp.len() as u64,
            p_memsz: interp.len() as u64,
            p_align: 1,
        });

        let name = Some(writer.add_section_name(".interp".as_bytes()));
        let index = writer.reserve_section_index();
        out_sections.push(Section {
            name,
            sh_type: elf::SHT_PROGBITS,
            sh_flags: elf::SHF_ALLOC as usize,
            sh_name: shstrtab_offset as u32, // set offset to name later when it's allocated
            sh_addr: (ro_addr_start + offset) as usize,
            sh_offset: offset as usize,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign: 1,
            data: interp.as_bytes().to_vec(),
        });
        out_sections_index.push(index);
        offset += interp.len() as u64;
    }

    // load segments
    //
    // program LOAD (R)
    out_ph.push(ProgramHeaderEntry {
        p_type: elf::PT_LOAD,
        p_flags: elf::PF_R,
        p_offset: 0, // read section starts at 0 offset to include headers
        p_vaddr: ro_addr_start as u64,
        p_paddr: ro_addr_start as u64,
        p_filesz: 0x1000,
        p_memsz: 0x1000,
        p_align: 0x1000,
    });
    offset += 0x1000;

    // program LOAD (RX)
    out_ph.push(ProgramHeaderEntry {
        p_type: elf::PT_LOAD,
        p_flags: elf::PF_R | elf::PF_X,
        p_offset: offset,
        p_vaddr: rx_addr_start as u64,
        p_paddr: rx_addr_start as u64,
        p_filesz: 0x1000,
        p_memsz: 0x1000,
        p_align: 0x1000,
    });
    offset += 0x1000;

    // program LOAD (RW)
    out_ph.push(ProgramHeaderEntry {
        p_type: elf::PT_LOAD,
        p_flags: elf::PF_R | elf::PF_W,
        p_offset: offset,
        p_vaddr: rw_addr_start as u64,
        p_paddr: rw_addr_start as u64,
        p_filesz: 0x1000,
        p_memsz: 0x1000,
        p_align: 0x1000,
    });
    offset += 0x1000;

    let mut out_dynamic = Vec::new();

    //let string = Some(writer.add_dynamic_string("libc.so.6".as_bytes()));
    //out_dynamic.push(Dynamic { tag: elf::DT_NEEDED, val: 0, string });
    out_dynamic.push(Dynamic { tag: elf::DT_NULL, val: 0, string: None });

    // Assign dynamic strings.

    if false {
        let before = writer.reserved_len();
        writer.reserve_dynamic(out_dynamic.len());
        let after = writer.reserved_len();
        let dynamic_size = 0;//after - before;
                             //program DYNAMIC
        out_ph.push(ProgramHeaderEntry {
            p_type: elf::PT_DYNAMIC,
            p_flags: elf::PF_R | elf::PF_W,
            p_offset: offset,
            p_vaddr: rx_addr_start as u64,
            p_paddr: rx_addr_start as u64,
            p_filesz: dynamic_size as u64,
            p_memsz: dynamic_size as u64,
            p_align: 0x8,
        });

        offset += dynamic_size as u64;
    }

    let mut hash_addr = 0;
    let mut gnu_hash_addr = 0;
    let mut versym_addr = 0;
    let mut verdef_addr = 0;
    let mut verneed_addr = 0;
    let mut dynamic_addr = 0;
    let mut dynsym_addr = 0;
    let mut dynstr_addr = 0;

    let mut code_sections = vec![];
    let mut data_sections = vec![];

    for (_name, unlinked) in link.unlinked.iter() {
        use object::SectionKind as K;
        match unlinked.kind {
            K::Data | K::UninitializedData | K::OtherString | K::ReadOnlyString | K::ReadOnlyData => {
                //let name = Some(writer.add_section_name(unlinked.section_name.as_bytes()));
                //let index = writer.reserve_section_index();
                //out_sections_index.push(index);
                data_sections.push(unlinked);
            }
            K::Text => {
                //let name = Some(writer.add_section_name(unlinked.section_name.as_bytes()));
                //let index = writer.reserve_section_index();
                //out_sections_index.push(index);
                code_sections.push(unlinked);
            }

            // ignore for now
            K::Metadata => (),
            K::Other => (),
            K::Elf(x) => {
                // ignore
                //unimplemented!("Elf({:#x})", x);
            }
            _ => unimplemented!("Unlinked kind: {:?}", unlinked.kind)
        }

        for (name, symbol) in unlinked.defined.iter() {
            match symbol.kind {
                CodeSymbolKind::Data => {
                }
                _ => ()
            }
        }

    }

    if data_sections.len() > 0 {
        let name = Some(writer.add_section_name(".data".as_bytes()));
        let data_index = writer.reserve_section_index();
        out_sections_index.push(data_index);

        let data_offset = offset;
        let mut data = vec![];
        for section in data_sections.iter() {
            data.extend(section.bytes.clone());
        }
        offset += data.len() as u64;

        out_sections.push(Section {
            name,
            sh_type: elf::SHT_PROGBITS,
            sh_flags: elf::SHF_ALLOC as usize,
            sh_name: 0,//shstrtab_offset as u32, // set offset to name later when it's allocated
            sh_addr: (rw_addr_start + data_offset) as usize,
            sh_offset: data_offset as usize,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign: 1,
            data,
        });
    }

    if code_sections.len() > 0 {
        let name = Some(writer.add_section_name(".text".as_bytes()));
        let text_index = writer.reserve_section_index();
        out_sections_index.push(text_index);

        let text_offset = offset;
        let mut data = vec![];
        for section in code_sections.iter() {
            data.extend(section.bytes.clone());
        }
        offset += data.len() as u64;

        out_sections.push(Section {
            name,
            sh_type: elf::SHT_PROGBITS,
            sh_flags: (elf::SHF_ALLOC | elf::SHF_EXECINSTR) as usize,
            sh_name: 0,//shstrtab_offset as u32, // set offset to name later when it's allocated
            sh_addr: (rx_addr_start + text_offset) as usize,
            sh_offset: offset as usize,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign: 1,
            data,
        });
    }

    if false {
        let name = Some(writer.add_section_name(".dynamic".as_bytes()));
        let dynamic_index = writer.reserve_dynamic_section_index();
        out_sections_index.push(dynamic_index);
        dynamic_addr = rx_addr_start + offset;
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
        offset += (after - before) as u64;
    }

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
    offset += (after - before) as u64;

    let name = Some(writer.add_section_name(".shstrtab".as_bytes()));
    let shstrtab_index = writer.reserve_shstrtab_section_index();
    out_sections_index.push(shstrtab_index);
    let before = writer.reserved_len();
    writer.reserve_dynstr();
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
    offset += (after - before) as u64;

    let name = Some(writer.add_section_name(".strtab".as_bytes()));
    let strtab_index = writer.reserve_strtab_section_index();
    out_sections_index.push(strtab_index);
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
    offset += (after - before) as u64;

    //let dynamic_symbol_index = writer.reserve_dynamic_symbol_index();

    // Assign dynamic symbol indices.
    //let mut out_dynsyms = Vec::new();
    //let mut out_dynsyms_index = vec![];
    //for out_dynsym in out_dynsyms.iter_mut() {

    // allocate and adjust offsets
    for (i, section) in out_sections.iter_mut().enumerate() {
        match section.sh_type {
            elf::SHT_PROGBITS | elf::SHT_NOTE => {
                section.sh_offset = writer.reserve(section.data.len(), section.sh_addralign);
            }
            elf::SHT_STRTAB => {
            }
            elf::SHT_DYNAMIC => {
            }
            _ => unimplemented!()
        }
    }


    writer.reserve_symtab();
    writer.reserve_symtab_shndx();
    writer.reserve_strtab();

    writer.reserve_shstrtab();
    writer.reserve_section_headers();

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

    for section in out_sections.iter() {
        //writer.pad_until(section.sh_offset as usize);
        match section.sh_type {
            elf::SHT_PROGBITS | elf::SHT_NOTE | elf::SHT_INIT_ARRAY | elf::SHT_FINI_ARRAY => {
                writer.write_align(section.sh_addralign as usize);
                writer.write(section.data.as_slice());
            }
            elf::SHT_DYNAMIC => {
                for d in out_dynamic.iter() {
                    if let Some(string) = d.string {
                        writer.write_dynamic_string(d.tag, string);
                    } else {
                        writer.write_dynamic(d.tag, d.val);
                    }
                }
            }
            //elf::SHT_DYNSYM => {
                //writer.write_null_dynamic_symbol();
            //}
            elf::SHT_STRTAB => {
            }
            _ => ()
        }
    }






    writer.write_null_symbol();
    // write symbols

    writer.write_symtab_shndx();
    writer.write_strtab();
    writer.write_shstrtab();


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
                writer.write_dynamic_section_header(dynamic_addr);
            }
            _ => unimplemented!()
        }
    }

    writer.write_strtab_section_header();
    writer.write_shstrtab_section_header();
    writer.write_dynstr_section_header(dynstr_addr);

    Ok(out_data)
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
