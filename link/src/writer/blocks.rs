use super::*;

pub trait ElfBlock {
    fn program_header(&self, data: &Data) -> Vec<ProgramHeaderEntry> {
        vec![]
    }
    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {}
    fn reserve(&mut self, data: &mut Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {}
    fn update(&mut self, data: &mut Data) {}
    fn write(&self, data: &Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {}
    fn write_section_header(&self, data: &Data, w: &mut Writer) {}
}

pub struct HeaderComponent {
    size_fh: usize,
    size_ph: usize,
}

impl Default for HeaderComponent {
    fn default() -> Self {
        Self {
            size_fh: 0,
            size_ph: 0,
        }
    }
}

impl ElfBlock for HeaderComponent {
    fn program_header(&self, data: &Data) -> Vec<ProgramHeaderEntry> {
        vec![
            // program header
            ProgramHeaderEntry {
                p_type: elf::PT_PHDR,
                p_flags: elf::PF_R,
                p_offset: self.size_fh as u64,
                p_vaddr: data.segments.ro.addr + self.size_fh as u64,
                p_paddr: 0,
                p_filesz: self.size_ph as u64,
                p_memsz: self.size_ph as u64,
                p_align: 8,
            },
            // add a load section for the file and program header, so it's covered
            ProgramHeaderEntry {
                p_type: elf::PT_LOAD,
                p_flags: elf::PF_R,
                p_offset: data.segments.ro.offset,
                p_vaddr: data.segments.ro.addr,
                p_paddr: 0,
                p_filesz: data.segments.ro.size() as u64,
                p_memsz: data.segments.ro.size() as u64,
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
        self.size_fh = w.reserved_len();

        // we only need to know the number of program headers
        // we don't need the actual headers
        let ph_count = ph.len();

        let before = w.reserved_len();
        w.reserve_program_headers(ph_count as u32);
        let after = w.reserved_len();
        self.size_ph = after - before;

        // add headers to the ro_size
        data.segments.ro.add_data(self.size_fh, 0x1);
        data.segments.ro.add_data(self.size_ph, 0x1);
    }

    fn update(&mut self, data: &mut Data) {}

    fn write(&self, data: &Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        w.write_file_header(&object::write::elf::FileHeader {
            os_abi: 0x00,                      // SysV
            abi_version: 0,                    // ignored on linux
            e_type: elf::ET_EXEC,              // ET_EXEC - Executable file
            e_machine: 0x3E,                   // AMD x86-64
            e_entry: data.segments.addr_start, // e_entry, normally points to _start
            e_flags: 0,                        // e_flags
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

pub struct DynamicSection {
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

impl ElfBlock for DynamicSection {
    fn program_header(&self, data: &Data) -> Vec<ProgramHeaderEntry> {
        //program DYNAMIC
        let size = self.size as u64;
        let addr = data.segments.rw.addr;
        vec![ProgramHeaderEntry {
            p_type: elf::PT_DYNAMIC,
            p_flags: elf::PF_R | elf::PF_W,
            p_offset: data.segments.rw.offset as u64,
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
        //data.segments.rw.size += after - self.start;
        data.segments.rw.add_data(self.size, self.align);
    }

    fn update(&mut self, data: &mut Data) {
        data.addr_dynamic = data.segments.rw.addr;
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
        w.write_dynamic_section_header(data.addr_dynamic);
    }
}

pub struct RelocationSection {
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
impl ElfBlock for RelocationSection {
    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        self.text_name = Some(w.add_section_name(".rela.text".as_bytes()));
        self.text_index = w.reserve_section_index();
        self.data_name = Some(w.add_section_name(".rela.data".as_bytes()));
        self.data_index = w.reserve_section_index();
    }

    fn reserve(&mut self, data: &mut Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        self.text_offset = w.reserve_relocations(data.segments.rx.section.relocations.len(), true);
        self.data_offset = w.reserve_relocations(data.segments.rw.section.relocations.len(), true);
    }

    fn update(&mut self, data: &mut Data) {}

    fn write(&self, data: &Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        w.write_align_relocation();
        for rel in data.segments.rx.relocations.iter() {
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
        for rel in data.segments.rw.relocations.iter() {
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
            data.segments.rx.section.index.unwrap(),
            data.index_symtab.unwrap(),
            self.text_offset,
            data.segments.rx.relocations.len(),
            true,
        );
        w.write_relocation_section_header(
            self.data_name.unwrap(),
            data.segments.rw.section.index.unwrap(),
            data.index_symtab.unwrap(),
            self.data_offset,
            data.segments.rw.relocations.len(),
            true,
        );
    }
}

pub struct SymTabSection {
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

impl ElfBlock for SymTabSection {
    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        data.index_symtab = Some(w.reserve_symtab_section_index());
        if w.symtab_shndx_needed() {
            w.reserve_symtab_shndx_section_index();
        }
    }

    fn reserve(&mut self, data: &mut Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        // reserve the symbols in the various sections
        for sym in &data.segments.ro.section.symbols {
            w.reserve_symbol_index(data.segments.ro.section.index);
        }
        for sym in &data.segments.rx.section.symbols {
            w.reserve_symbol_index(data.segments.rx.section.index);
        }
        for sym in &data.segments.rw.section.symbols {
            w.reserve_symbol_index(data.segments.rw.section.index);
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
        for (_, sym) in &data.segments.ro.section.symbols {
            let st_info = (elf::STB_GLOBAL << 4) + elf::STT_FUNC;
            let st_info = sym.s.st_info;
            let st_other = sym.s.st_other; //elf::STV_DEFAULT;
            let st_shndx = elf::SHN_ABS;
            let st_size = sym.s.size;
            let addr = sym.s.address + data.segments.ro.base;
            //eprintln!("write sym: {:?}, {:#0x}", &sym, addr);
            w.write_symbol(&object::write::elf::Sym {
                name: sym.name_id,
                section: data.segments.rw.section.index,
                st_info,
                st_other,
                st_shndx,
                st_value: addr,
                st_size,
            });
        }
        for (_, sym) in &data.segments.rx.section.symbols {
            let addr = sym.s.address + data.segments.rx.addr;
            //eprintln!("write sym: {:?}, {:#0x}", &sym, addr);
            let st_info = (elf::STB_GLOBAL << 4) + elf::STT_FUNC;
            let st_other = elf::STV_DEFAULT;
            let st_shndx = elf::SHN_ABS;
            let st_size = 1;
            w.write_symbol(&object::write::elf::Sym {
                name: sym.name_id,
                section: data.segments.rx.section.index,
                st_info: sym.s.st_info,
                st_other: sym.s.st_other,
                st_shndx,
                st_value: addr,
                st_size,
            });
        }

        for (_, sym) in &data.segments.rw.section.symbols {
            let addr = sym.s.address + data.segments.rw.base;
            //eprintln!("write sym: {:?}, {:#0x}", &sym, addr);
            let st_info = (elf::STB_GLOBAL << 4) + elf::STT_FUNC;
            let st_other = elf::STV_DEFAULT;
            let st_shndx = elf::SHN_ABS;
            let st_size = 1;
            w.write_symbol(&object::write::elf::Sym {
                name: sym.name_id,
                section: data.segments.rw.section.index,
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
        // one greater than the symbol table index of the last
        // local symbol (binding STB_LOCAL)
        w.write_symtab_section_header(1);
        if w.symtab_shndx_needed() {
            w.write_symtab_shndx_section_header();
        }
    }
}

pub struct DynSymSection {
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

impl ElfBlock for DynSymSection {
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
        data.segments.ro.size += after - pos;
        //data.segments.ro.add_data(after - pos, align_pos);
    }

    fn update(&mut self, data: &mut Data) {
        data.addr_dynsym = data.segments.ro.addr + self.start as u64;
    }

    fn write(&self, data: &Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align);
        w.pad_until(aligned_pos);
        w.write_null_dynamic_symbol();
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        w.write_dynsym_section_header(data.addr_dynsym, 1);
    }
}

pub struct DynStrSection {
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
impl ElfBlock for DynStrSection {
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
        data.segments.ro.size += after - pos;
        //data.segments.ro.add_data(after - pos, align_pos);
    }

    fn update(&mut self, data: &mut Data) {
        data.addr_dynstr = data.segments.ro.addr + self.start as u64;
    }

    fn write(&self, data: &Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align);
        w.pad_until(aligned_pos);
        w.write_dynstr();
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        w.write_dynstr_section_header(data.addr_dynstr);
    }
}

#[derive(Default)]
pub struct ShStrTabSection {
    index: Option<SectionIndex>,
}
impl ElfBlock for ShStrTabSection {
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
        w.write_shstrtab_section_header();
    }
}

#[derive(Default)]
pub struct StrTabSection {
    index: Option<SectionIndex>,
}
impl ElfBlock for StrTabSection {
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
        w.write_strtab_section_header();
    }
}

pub struct BufferSection {
    alloc: AllocSegment,
    name_id: Option<StringId>,
    addr: usize,
    offset: usize,
    size: usize,
    base: usize,
    buf: Vec<u8>,
}

impl BufferSection {
    pub fn new(alloc: AllocSegment, name_id: Option<StringId>, buf: Vec<u8>) -> Self {
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

impl ElfBlock for BufferSection {
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
                let addr = data.segments.ro.addr;
                vec![ProgramHeaderEntry {
                    p_type: elf::PT_LOAD,
                    p_flags: elf::PF_R,
                    p_offset: data.segments.ro.offset,
                    p_vaddr: addr,
                    p_paddr: 0,
                    p_filesz: data.segments.ro.size() as u64,
                    p_memsz: data.segments.ro.size() as u64,
                    p_align: data.page_size as u64,
                }]
            }

            AllocSegment::RX => {
                // program LOAD (RX)
                let addr = data.segments.rx.addr;
                //log::debug!("rx: {:#0x}, {:#0x}", data.segments.rx.offset, addr);
                vec![ProgramHeaderEntry {
                    p_type: elf::PT_LOAD,
                    p_flags: elf::PF_R | elf::PF_X,
                    p_offset: data.segments.rx.offset,
                    p_vaddr: addr,
                    p_paddr: 0,
                    p_filesz: data.segments.rx.size() as u64,
                    p_memsz: data.segments.rx.size() as u64,
                    p_align: data.page_size as u64,
                }]
            }

            AllocSegment::RW => {
                // program LOAD (RW)
                let addr = data.segments.rw.addr;
                vec![ProgramHeaderEntry {
                    p_type: elf::PT_LOAD,
                    p_flags: elf::PF_R | elf::PF_W,
                    p_offset: data.segments.rw.offset as u64,
                    p_vaddr: addr,
                    p_paddr: 0,
                    p_filesz: data.segments.rw.size() as u64,
                    p_memsz: data.segments.rw.size() as u64,
                    p_align: data.page_size as u64,
                }]
            }
        }
    }

    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        let index = w.reserve_section_index();
        match self.alloc {
            AllocSegment::RW => {
                data.segments.rw.section.index = Some(index);
            }
            AllocSegment::RX => {
                data.segments.rx.section.index = Some(index);
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
            AllocSegment::RO | AllocSegment::Interp => data.segments.ro.add_data(self.size, align),
            AllocSegment::RW => data.segments.rw.add_data(self.size, align),
            AllocSegment::RX => data.segments.rx.add_data(self.size, align),
        };

        if self.alloc == AllocSegment::Interp {
            data.addr_interp = start as u64;
        }

        //log::debug!(
        //"reserve: pos: {:#0x}, start: {:#0x}, end: {:#0x}, size: {:#0x}",
        //pos,
        //start,
        //end,
        //self.buf.len()
        //);
    }

    fn update(&mut self, data: &mut Data) {
        let segment = match self.alloc {
            AllocSegment::RO | AllocSegment::Interp => &data.segments.ro,
            AllocSegment::RW => &data.segments.rw,
            AllocSegment::RX => &data.segments.rx,
        };

        self.offset = segment.offset as usize;
        self.base = segment.base as usize;
        self.addr = segment.base as usize + self.offset;

        // set the address correctly on the pointers, now that we have assigned addresses
        // to the load segments
        let mut pointers = HashMap::new();
        for (name, s) in &data.segments.rx.section.symbols {
            let addr = data.segments.rx.base + s.s.address;
            pointers.insert(name, addr);
        }
        for (name, s) in &data.segments.rw.section.symbols {
            let addr = data.segments.rw.base + s.s.address;
            pointers.insert(name, addr);
        }

        match self.alloc {
            AllocSegment::RW => {
                //data.segments.rw.addr = self.offset as u64;
                let patch_base = self.buf.as_ptr();
                let v_base = data.segments.rw.base;
                for r in data.segments.rw.relocations.iter() {
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
                //data.segments.rx.addr = self.offset as u64;
                let patch_base = self.buf.as_ptr();
                let v_base = data.segments.rx.base;
                for r in data.segments.rx.relocations.iter() {
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
    }

    fn write(&self, data: &Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.alloc.align());
        w.pad_until(aligned_pos);
        //log::debug!(
        //"write at: pos: {:#0x}, addr: {:#0x}, offset: {:#0x}, size: {:#0x}",
        //pos,
        //self.addr,
        //self.offset,
        //self.buf.len()
        //);
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
                sh_addralign: self.alloc.align() as u64,
                sh_size: self.size as u64,
            });
        }
    }
}
