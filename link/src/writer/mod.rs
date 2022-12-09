use std::error::Error;

use object::elf;
use object::read::elf::FileHeader;
use object::write::elf::{SectionIndex, Writer};
use object::write::StringId;
use object::Endianness;
use std::collections::HashMap;

use super::*;
use crate::*;

mod section;
use section::*;

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
    fn program_header(&self, data: &Data) -> Vec<ProgramHeaderEntry> {
        vec![]
    }
    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {}
    fn reserve(&mut self, data: &mut Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {}
    fn update(&mut self, data: &mut Data) {}
    fn write(&self, data: &Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {}
    fn write_section_header(&self, data: &Data, w: &mut Writer) {}
}

struct HeaderComponent {}

impl ElfComponent for HeaderComponent {
    fn program_header(&self, data: &Data) -> Vec<ProgramHeaderEntry> {
        vec![
            // program header
            ProgramHeaderEntry {
                p_type: elf::PT_PHDR,
                p_flags: elf::PF_R,
                p_offset: data.size_fh as u64,
                p_vaddr: data.ro.addr + data.size_fh as u64,
                p_paddr: 0,
                p_filesz: data.size_ph as u64,
                p_memsz: data.size_ph as u64,
                p_align: 8,
            },
            // add a load section for the file and program header, so it's covered
            ProgramHeaderEntry {
                p_type: elf::PT_LOAD,
                p_flags: elf::PF_R,
                p_offset: data.ro.offset,
                p_vaddr: data.ro.addr,
                p_paddr: 0,
                p_filesz: data.ro.size as u64,
                p_memsz: data.ro.size as u64,
                p_align: data.page_size as u64,
            },
        ]
    }

    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        let null_section_index = w.reserve_null_section_index();
    }

    fn reserve(&mut self, data: &mut Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        if w.reserved_len() > 0 {
            panic!("Must start with file header");
        }

        // Start reserving file ranges.
        w.reserve_file_header();
        data.size_fh = w.reserved_len();

        // we only need to know the number of program headers
        // we don't need the actual headers
        let ph_count = ph.len();

        let before = w.reserved_len();
        w.reserve_program_headers(ph_count as u32);
        let after = w.reserved_len();
        data.size_ph = after - before;

        // add headers to the ro_size
        data.ro.add_data(data.size_fh, 0x1);
        data.ro.add_data(data.size_ph, 0x1);
    }

    fn update(&mut self, data: &mut Data) {}

    fn write(&self, data: &Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
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

        for ph in ph.iter() {
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

struct DynamicSection {
    index: Option<SectionIndex>,
    start: usize,
    align: usize,
    size: usize,
}
impl Default for DynamicSection {
    fn default() -> Self {
        Self {
            index: None,
            start: 0,
            size: 0,
            align: 0x10,
        }
    }
}

impl ElfComponent for DynamicSection {
    fn program_header(&self, data: &Data) -> Vec<ProgramHeaderEntry> {
        //program DYNAMIC
        let size = self.size as u64;
        let addr = data.rw.addr;
        vec![ProgramHeaderEntry {
            p_type: elf::PT_DYNAMIC,
            p_flags: elf::PF_R | elf::PF_W,
            p_offset: data.rw.offset as u64,
            p_vaddr: addr,
            p_paddr: 0,
            p_filesz: size,
            p_memsz: size,
            p_align: self.align as u64,
        }]
    }
    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        let dynamic_index = w.reserve_dynamic_section_index();
    }

    fn reserve(&mut self, data: &mut Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        let dynamic = data.gen_dynamic();
        let before = w.reserved_len();
        self.start = size_align(before, self.align);
        w.reserve_until(self.start);
        w.reserve_dynamic(dynamic.len());
        let after = w.reserved_len();
        self.size = after - before;
        // allocate space in the rw segment
        data.rw.size += after - self.start;
        data.rw.add_data(self.size, self.align);
        //data.size_dynamic = size;
    }

    fn update(&mut self, data: &mut Data) {
        data.addr_dynamic = data.rw.addr;
    }

    fn write(&self, data: &Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        let dynamic = data.gen_dynamic();
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align);
        eprintln!("reserve: {:#0x}, {:#0x}", &pos, aligned_pos);
        w.pad_until(aligned_pos);
        w.write_align_dynamic();
        let pos2 = w.len();
        eprintln!("reserve: {:#0x}, {:#0x}, {:#0x}", &pos, aligned_pos, pos2);
        for d in dynamic.iter() {
            if let Some(string) = d.string {
                w.write_dynamic_string(d.tag, string);
            } else {
                w.write_dynamic(d.tag, d.val);
            }
        }
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        if data.add_section_headers {
            w.write_dynamic_section_header(data.addr_dynamic);
        }
    }
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

struct RelocationSection {
    index: Option<SectionIndex>,
    align: usize,
    data_name: Option<StringId>,
    text_name: Option<StringId>,
    data_offset: usize,
    text_offset: usize,
    data_index: SectionIndex,
    text_index: SectionIndex,
}
impl Default for RelocationSection {
    fn default() -> Self {
        Self {
            index: None,
            data_name: None,
            text_name: None,
            align: 0x10,
            data_offset: 0,
            text_offset: 0,
            data_index: SectionIndex::default(),
            text_index: SectionIndex::default(),
        }
    }
}
impl ElfComponent for RelocationSection {
    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        self.text_name = Some(w.add_section_name(".rela.text".as_bytes()));
        self.text_index = w.reserve_section_index();
        self.data_name = Some(w.add_section_name(".rela.data".as_bytes()));
        self.data_index = w.reserve_section_index();
    }

    fn reserve(&mut self, data: &mut Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        self.text_offset = w.reserve_relocations(data.sections.rx.relocations.len(), true);
        self.data_offset = w.reserve_relocations(data.sections.rw.relocations.len(), true);
    }

    fn update(&mut self, data: &mut Data) {}

    fn write(&self, data: &Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        w.write_align_relocation();
        for rel in data.sections.rx.relocations.iter() {
            let r_offset = rel.offset;
            let r_addend = rel.r.addend;
            let r_sym = 0;
            let r_type = 0;
            w.write_relocation(
                true,
                &object::write::elf::Rel {
                    r_offset,
                    r_sym,
                    r_type,
                    r_addend,
                },
            );
        }
        for rel in data.sections.rw.relocations.iter() {
            let r_offset = rel.offset;
            let r_addend = rel.r.addend;
            let r_sym = 0;
            let r_type = 0;
            w.write_relocation(
                true,
                &object::write::elf::Rel {
                    r_offset,
                    r_sym,
                    r_type,
                    r_addend,
                },
            );
        }
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        w.write_relocation_section_header(
            self.text_name.unwrap(),
            data.index_text.unwrap(),
            data.index_symtab.unwrap(),
            self.text_offset,
            data.sections.rx.relocations.len(),
            true,
        );
        w.write_relocation_section_header(
            self.data_name.unwrap(),
            data.index_data.unwrap(),
            data.index_symtab.unwrap(),
            self.data_offset,
            data.sections.rw.relocations.len(),
            true,
        );
    }
}

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
    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        //w.reserve_symbol_index(data.index_text);
        //w.reserve_symbol_index(data.index_data);
        data.index_symtab = Some(w.reserve_symtab_section_index());
        if w.symtab_shndx_needed() {
            w.reserve_symtab_shndx_section_index();
        }
    }

    fn reserve(&mut self, data: &mut Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
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

    fn update(&mut self, data: &mut Data) {}

    fn write(&self, data: &Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        // write symbols
        w.write_null_symbol();

        // write symbols out
        for (_, sym) in &data.sections.ro.symbols {
            let st_info = (elf::STB_GLOBAL << 4) + elf::STT_FUNC;
            let st_info = sym.s.st_info;
            let st_other = sym.s.st_other; //elf::STV_DEFAULT;
            let st_shndx = elf::SHN_ABS;
            let st_size = sym.s.size;
            let addr = sym.s.address + data.ro.base;
            //eprintln!("write sym: {:?}, {:#0x}", &sym, addr);
            w.write_symbol(&object::write::elf::Sym {
                name: sym.name_id,
                section: data.index_data,
                st_info,
                st_other,
                st_shndx,
                st_value: addr,
                st_size,
            });
        }
        for (_, sym) in &data.sections.rx.symbols {
            let addr = sym.s.address + data.rx.addr;
            //eprintln!("write sym: {:?}, {:#0x}", &sym, addr);
            let st_info = (elf::STB_GLOBAL << 4) + elf::STT_FUNC;
            let st_other = elf::STV_DEFAULT;
            let st_shndx = elf::SHN_ABS;
            let st_size = 1;
            w.write_symbol(&object::write::elf::Sym {
                name: sym.name_id,
                section: data.index_text,
                st_info: sym.s.st_info,
                st_other: sym.s.st_other,
                st_shndx,
                st_value: addr,
                st_size,
            });
        }

        for (_, sym) in &data.sections.rw.symbols {
            let addr = sym.s.address + data.rw.base;
            //eprintln!("write sym: {:?}, {:#0x}", &sym, addr);
            let st_info = (elf::STB_GLOBAL << 4) + elf::STT_FUNC;
            let st_other = elf::STV_DEFAULT;
            let st_shndx = elf::SHN_ABS;
            let st_size = 1;
            w.write_symbol(&object::write::elf::Sym {
                name: sym.name_id,
                section: data.index_data,
                st_info: sym.s.st_info,
                st_other: sym.s.st_other,
                st_shndx,
                st_value: addr,
                st_size,
            });
        }
        if w.symtab_shndx_needed() {
            w.write_symtab_shndx();
        }
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        if data.add_section_headers {
            // one greater than the symbol table index of the last
            // local symbol (binding STB_LOCAL)
            w.write_symtab_section_header(1);
            if w.symtab_shndx_needed() {
                w.write_symtab_shndx_section_header();
            }
        }
    }
}

struct DynSymSection {
    index: Option<SectionIndex>,
    align: usize,
    start: usize,
}
impl Default for DynSymSection {
    fn default() -> Self {
        Self {
            index: None,
            align: 0x10,
            start: 0,
        }
    }
}

impl ElfComponent for DynSymSection {
    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        w.reserve_null_dynamic_symbol_index();

        data.index_dynsym = Some(w.reserve_dynsym_section_index());
    }

    fn reserve(&mut self, data: &mut Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        let pos = w.reserved_len();
        let align_pos = size_align(pos, self.align);
        w.reserve_until(align_pos);

        self.start = w.reserved_len();
        w.reserve_dynsym();
        let after = w.reserved_len();
        data.ro.size += after - pos;
    }

    fn update(&mut self, data: &mut Data) {
        data.addr_dynsym = data.ro.addr + self.start as u64;
    }

    fn write(&self, data: &Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align);
        w.pad_until(aligned_pos);
        w.write_null_dynamic_symbol();
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        if data.add_section_headers {
            w.write_dynsym_section_header(data.addr_dynsym, 1);
        }
    }
}

struct DynStrSection {
    index: Option<SectionIndex>,
    align: usize,
    start: usize,
}
impl Default for DynStrSection {
    fn default() -> Self {
        Self {
            index: None,
            align: 0x10,
            start: 0,
        }
    }
}
impl ElfComponent for DynStrSection {
    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        data.index_dynstr = Some(w.reserve_dynstr_section_index());
    }

    fn reserve(&mut self, data: &mut Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        let pos = w.reserved_len();
        let align_pos = size_align(pos, self.align);
        w.reserve_until(align_pos);
        self.start = w.reserved_len();
        w.reserve_dynstr();
        let after = w.reserved_len();
        data.ro.size += after - pos;
    }

    fn update(&mut self, data: &mut Data) {
        data.addr_dynstr = data.ro.addr + self.start as u64;
    }

    fn write(&self, data: &Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align);
        w.pad_until(aligned_pos);
        w.write_dynstr();
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        if data.add_section_headers {
            w.write_dynstr_section_header(data.addr_dynstr);
        }
    }
}

#[derive(Default)]
struct ShStrTabSection {
    index: Option<SectionIndex>,
}
impl ElfComponent for ShStrTabSection {
    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        let shstrtab_index = w.reserve_shstrtab_section_index();
    }

    fn reserve(&mut self, data: &mut Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        let before = w.reserved_len();
        w.reserve_shstrtab();
        let after = w.reserved_len();
    }

    fn write(&self, data: &Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        w.write_shstrtab();
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        if data.add_section_headers {
            w.write_shstrtab_section_header();
        }
    }
}

#[derive(Default)]
struct StrTabSection {
    index: Option<SectionIndex>,
}
impl ElfComponent for StrTabSection {
    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        data.index_strtab = Some(w.reserve_strtab_section_index());
    }

    fn reserve(&mut self, data: &mut Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        w.reserve_strtab();
    }

    fn write(&self, data: &Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        w.write_strtab();
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        if data.add_section_headers {
            w.write_strtab_section_header();
        }
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
}

impl ElfComponent for BufferSection {
    fn program_header(&self, data: &Data) -> Vec<ProgramHeaderEntry> {
        match self.alloc {
            AllocSegment::Interp => {
                let interp = data.interp.as_bytes().to_vec();
                let cstr = std::ffi::CString::new(interp).unwrap();
                let cstr_size = cstr.as_bytes_with_nul().len();
                vec![ProgramHeaderEntry {
                    p_type: elf::PT_INTERP,
                    p_flags: elf::PF_R,
                    p_offset: data.addr_interp,
                    p_vaddr: data.addr_interp,
                    p_paddr: 0,
                    p_filesz: cstr_size as u64,
                    p_memsz: cstr_size as u64,
                    p_align: self.alloc.align() as u64,
                }]
            }

            AllocSegment::RO => {
                // load segments
                // program LOAD (R)
                let addr = data.ro.addr;
                vec![ProgramHeaderEntry {
                    p_type: elf::PT_LOAD,
                    p_flags: elf::PF_R,
                    p_offset: data.ro.offset,
                    p_vaddr: addr,
                    p_paddr: 0,
                    p_filesz: data.ro.size as u64,
                    p_memsz: data.ro.size as u64,
                    p_align: data.page_size as u64,
                }]
            }

            AllocSegment::RX => {
                // program LOAD (RX)
                let addr = data.rx.addr;
                log::debug!("{:#0x}, {:#0x}", data.rx.offset, addr);
                vec![ProgramHeaderEntry {
                    p_type: elf::PT_LOAD,
                    p_flags: elf::PF_R | elf::PF_X,
                    p_offset: data.rx.offset,
                    p_vaddr: addr,
                    p_paddr: 0,
                    p_filesz: data.rx.size as u64,
                    p_memsz: data.rx.size as u64,
                    p_align: data.page_size as u64,
                }]
            }

            AllocSegment::RW => {
                // program LOAD (RW)
                let addr = data.rw.addr;
                vec![ProgramHeaderEntry {
                    p_type: elf::PT_LOAD,
                    p_flags: elf::PF_R | elf::PF_W,
                    p_offset: data.rw.offset as u64,
                    p_vaddr: addr,
                    p_paddr: 0,
                    p_filesz: data.rw.size as u64,
                    p_memsz: data.rw.size as u64,
                    p_align: data.page_size as u64,
                }]
            }
        }
    }

    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
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
    }

    fn reserve(&mut self, data: &mut Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        let align = self.alloc.align();
        let pos = w.reserved_len();
        let align_pos = size_align(pos, align);
        w.reserve_until(align_pos);
        let start = w.reserved_len();
        let end = w.reserve(self.buf.len(), align);
        self.size = self.buf.len();
        self.offset = start;
        match self.alloc {
            AllocSegment::RO | AllocSegment::Interp => data.ro.add_data(self.size, align),
            AllocSegment::RW => data.rw.add_data(self.size, align),
            AllocSegment::RX => data.rx.add_data(self.size, align),
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
        // set the address correctly on the pointers, now that we have assigned addresses
        // to the load segments
        let mut pointers = HashMap::new();
        for (name, s) in data.sections.rx.symbols.iter() {
            let addr = data.rx.base + s.s.address;
            pointers.insert(name, addr);
        }
        for (name, s) in data.sections.rw.symbols.iter() {
            let addr = data.rw.base + s.s.address;
            pointers.insert(name, addr);
        }

        let segment = match self.alloc {
            AllocSegment::RO | AllocSegment::Interp => &data.ro,
            AllocSegment::RW => &data.rw,
            AllocSegment::RX => &data.rx,
        };
        match self.alloc {
            AllocSegment::RW => {
                data.addr_rw = self.offset as u64;
                let patch_base = self.buf.as_ptr();
                let v_base = data.rw.base;
                for r in data.data_relocs.iter() {
                    let addr = *pointers.get(&r.name).unwrap();
                    log::debug!(
                        "R-RW: vbase: {:#0x}, addr: {:#0x}, {}",
                        v_base,
                        addr as usize,
                        &r.name
                    );
                    r.patch(patch_base as *mut u8, v_base as *mut u8, addr as *const u8);
                }
            }
            AllocSegment::RX => {
                data.addr_rx = self.offset as u64;
                let patch_base = self.buf.as_ptr();
                let v_base = data.rx.base;
                for r in data.text_relocs.iter() {
                    let addr = *pointers.get(&r.name).unwrap();
                    log::debug!(
                        "R-RX: vbase: {:#0x}, addr: {:#0x}, {}",
                        v_base,
                        addr as usize,
                        &r.name
                    );
                    r.patch(patch_base as *mut u8, v_base as *mut u8, addr as *const u8);
                }
            }
            //AllocSegment::RO => (),
            _ => (),
        }

        self.base = segment.base as usize;
        self.addr = segment.base as usize + self.offset;
    }

    fn write(&self, data: &Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.alloc.align());
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
        if data.add_section_headers {
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
                    sh_addralign: self.alloc.align() as u64,
                    sh_size: self.size as u64,
                });
            }
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

pub struct Data {
    interp: String,
    is_64: bool,
    //code_segments: Vec<UnlinkedCodeSegment>,
    //data_segments: Vec<UnlinkedCodeSegment>,
    data_buf: Vec<u8>,
    text_buf: Vec<u8>,
    data_relocs: Vec<CodeRelocation>,
    text_relocs: Vec<CodeRelocation>,
    ph: Vec<ProgramHeaderEntry>,
    lib_names: Vec<String>,
    libs: Vec<Library>,
    page_size: u32,
    components: Vec<Box<dyn ElfComponent>>,
    ro: Segment,
    rw: Segment,
    rx: Segment,
    addr_dynamic: u64,
    addr_dynstr: u64,
    addr_dynsym: u64,
    addr_rx: u64,
    addr_rw: u64,
    addr_start: u64,
    addr_interp: u64,
    index_data: Option<SectionIndex>,
    index_text: Option<SectionIndex>,
    index_strtab: Option<SectionIndex>,
    index_symtab: Option<SectionIndex>,
    index_dynstr: Option<SectionIndex>,
    index_dynsym: Option<SectionIndex>,
    size_fh: usize,
    size_ph: usize,
    size_dynamic: usize,
    sections: Sections,
    add_section_headers: bool,
    add_symbols: bool,
}
impl Data {
    fn read_unlinked<'a>(link: &'a Link, w: &mut Writer<'a>, s: &mut Sections) {
        let mut ro_size = 0;
        let mut rw_size = 0;
        let mut rx_size = 0;

        // get symbols and relocations
        for (_name, unlinked) in link.unlinked.iter() {
            use object::SectionKind as K;
            let is_start = false;
            match unlinked.kind {
                K::Data | K::UninitializedData => {
                    s.rw.relocations.extend(unlinked.relocations.clone());
                    for (name, symbol) in unlinked.defined.iter() {
                        let name_id = Some(w.add_string(name.as_bytes()));

                        let mut symbol = symbol.clone();
                        symbol.address += rw_size as u64;

                        let ps = ProgSymbol {
                            name_id,
                            is_start,
                            s: symbol,
                        };

                        s.rw.symbols.insert(name.clone(), ps);
                    }
                    rw_size += unlinked.bytes.len();
                }
                K::OtherString | K::ReadOnlyString | K::ReadOnlyData => {
                    s.ro.relocations.extend(unlinked.relocations.clone());
                    for (name, symbol) in unlinked.defined.iter() {
                        let name_id = Some(w.add_string(name.as_bytes()));
                        let mut symbol = symbol.clone();
                        symbol.address += ro_size as u64;
                        let ps = ProgSymbol {
                            name_id,
                            is_start,
                            s: symbol,
                        };
                        s.ro.symbols.insert(name.clone(), ps);
                    }
                    ro_size += unlinked.bytes.len();
                }
                K::Text => {
                    s.rx.relocations.extend(unlinked.relocations.clone());
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
                        s.rx.symbols.insert(name.clone(), ps);
                    }
                    rx_size += unlinked.bytes.len();
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
    }

    pub fn new(link: &Link, lib_names: Vec<String>) -> Self {
        let mut ro_size = 0;
        let mut rw_size = 0;
        let mut rx_size = 0;
        let mut data_buf = vec![];
        let mut text_buf = vec![];

        let mut data_relocs = vec![];
        let mut text_relocs = vec![];
        for (_name, unlinked) in link.unlinked.iter() {
            use object::SectionKind as K;
            match unlinked.kind {
                K::Data | K::UninitializedData => {
                    rw_size += unlinked.bytes.len();
                    data_buf.extend(unlinked.bytes.clone());
                    data_relocs.extend(unlinked.relocations.clone());
                }
                K::OtherString | K::ReadOnlyString | K::ReadOnlyData => {
                    ro_size += unlinked.bytes.len();
                    data_buf.extend(unlinked.bytes.clone());
                    data_relocs.extend(unlinked.relocations.clone());
                }
                K::Text => {
                    rx_size += unlinked.bytes.len();
                    text_buf.extend(unlinked.bytes.clone());
                    text_relocs.extend(unlinked.relocations.clone());
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

        let ro = Segment::new_ro();
        let rw = Segment::new_rw();
        let rx = Segment::new_rx();

        Self {
            is_64: true,
            interp: "/lib64/ld-linux-x86-64.so.2".to_string(),
            data_buf,
            text_buf,
            text_relocs,
            data_relocs,
            ph: vec![],
            components: vec![],
            lib_names,
            libs: vec![],
            page_size: 0x1000,
            ro,
            rx,
            rw,
            addr_dynamic: 0,
            addr_dynstr: 0,
            addr_dynsym: 0,
            addr_rx: 0,
            addr_rw: 0,
            addr_start: 0,
            addr_interp: 0,
            index_strtab: None,
            index_symtab: None,
            index_dynstr: None,
            index_dynsym: None,
            index_data: None,
            index_text: None,
            size_fh: 0,
            size_ph: 0,
            size_dynamic: 0,
            sections: Sections::default(),
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

    fn update_segments(&mut self) {
        let align = AllocSegment::RO.align();
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

        let align = AllocSegment::RX.align();
        let base = size_align(base as usize + offset, self.page_size as usize) as u64;
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

        let align = AllocSegment::RW.align();
        let base = size_align(base as usize + rx_size_elf_aligned, self.page_size as usize) as u64;
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

        // set entry point
        for (_name, sym) in &self.sections.rx.symbols {
            if sym.is_start {
                self.addr_start = self.rx.addr + sym.s.address;
            }
        }
    }
}

pub fn write_file<Elf: FileHeader<Endian = Endianness>>(
    link: &Link,
    mut data: Data,
) -> std::result::Result<Vec<u8>, Box<dyn Error>> {
    let mut out_data = Vec::new();
    let endian = Endianness::Little;
    let mut writer = object::write::elf::Writer::new(endian, data.is_64, &mut out_data);

    let lib_names = data.lib_names.clone();
    for lib_name in lib_names.iter() {
        let string_id = writer.add_dynamic_string(lib_name.as_bytes());
        data.add_library(string_id);
    }

    Data::read_unlinked(link, &mut writer, &mut data.sections);

    let mut blocks: Vec<Box<dyn ElfComponent>> = vec![];
    blocks.push(Box::new(HeaderComponent {}));

    let page_align = 0x1000;
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
    let buf = data.text_buf.clone();
    let name_id = Some(writer.add_section_name(".text".as_bytes()));
    blocks.push(Box::new(BufferSection::new(AllocSegment::RX, name_id, buf)));

    // .data
    let buf = data.data_buf.clone();
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
    data.update_segments();

    for b in blocks.iter_mut() {
        b.update(&mut data);
    }

    // get a list of program headers
    // we now have the values so they will be correctly written
    let mut ph = vec![];
    for b in blocks.iter() {
        ph.extend(b.program_header(&data));
    }

    // WRITE
    for b in blocks.iter_mut() {
        b.write(&data, &ph, &mut writer);
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
