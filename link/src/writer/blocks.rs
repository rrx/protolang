use super::*;
use object::write::elf::Sym;

pub trait ElfBlock {
    fn alloc(&self) -> Option<AllocSegment> {
        None
    }
    fn program_header(&self) -> Vec<ProgramHeaderEntry> {
        vec![]
    }
    fn reserve_strings<'a>(&self, _: &'a mut Vec<LocalSymbol>, _: &mut Writer<'a>) {}
    fn reserve_section_index(&mut self, _: &mut Data, _: &mut Writer) {}
    fn reserve(&mut self, _: &mut Data, _: &mut SegmentTracker, _: &mut Writer) {}
    fn update_tracker(&mut self, _: &Data, _: &mut SegmentTracker) {}
    fn update(&mut self, _: &mut Data) {}
    fn write(&self, _: &Data, _: &mut SegmentTracker, _: &mut Writer) {}
    fn write_section_header(&self, _: &Data, _: &SegmentTracker, _: &mut Writer) {}
}

pub struct HeaderComponent {
    size_fh: usize,
    size_ph: usize,
    base: usize,
}

impl Default for HeaderComponent {
    fn default() -> Self {
        Self {
            size_fh: 0,
            size_ph: 0,
            base: 0,
        }
    }
}

impl ElfBlock for HeaderComponent {
    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RO)
    }

    fn program_header(&self) -> Vec<ProgramHeaderEntry> {
        vec![
            // program header
            ProgramHeaderEntry {
                p_type: elf::PT_PHDR,
                p_flags: elf::PF_R,
                p_offset: self.size_fh as u64,
                p_vaddr: self.base as u64 + self.size_fh as u64,
                p_paddr: 0,
                p_filesz: self.size_ph as u64,
                p_memsz: self.size_ph as u64,
                p_align: 8,
            },
        ]
    }

    fn reserve_section_index(&mut self, _data: &mut Data, w: &mut Writer) {
        let _null_section_index = w.reserve_null_section_index();
    }

    fn reserve(&mut self, _data: &mut Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        if w.reserved_len() > 0 {
            panic!("Must start with file header");
        }

        // Start reserving file ranges.
        w.reserve_file_header();
        self.size_fh = w.reserved_len();

        let ph_count = tracker.ph.len();

        let before = w.reserved_len();
        w.reserve_program_headers(ph_count as u32);
        let after = w.reserved_len();
        self.size_ph = after - before;

        tracker.add_data(self.alloc().unwrap(), self.size_fh, 0);
        self.base = tracker.add_data(self.alloc().unwrap(), self.size_ph, self.size_fh);
    }

    fn write(&self, data: &Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        let mut e_entry = 0;
        if let Some(start) = data.pointers.get("_start") {
            e_entry = *start;
        }

        w.write_file_header(&object::write::elf::FileHeader {
            os_abi: 0x00,         // SysV
            abi_version: 0,       // ignored on linux
            e_type: elf::ET_EXEC, // ET_EXEC - Executable file
            e_machine: 0x3E,      // AMD x86-64
            e_entry,              // e_entry, normally points to _start
            e_flags: 0,           // e_flags
        })
        .unwrap();

        w.write_align_program_headers();

        for ph in tracker.ph.iter() {
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

    fn write_section_header(&self, _data: &Data, _tracker: &SegmentTracker, w: &mut Writer) {
        w.write_null_section_header();
    }
}

pub struct DynamicSection {
    index: Option<SectionIndex>,
    align: usize,
    size: usize,
    address: u64,
    file_offset: usize,
    base: usize,
}
impl Default for DynamicSection {
    fn default() -> Self {
        Self {
            index: None,
            size: 0,
            align: 0x10,
            address: 0,
            file_offset: 0,
            base: 0,
        }
    }
}

impl ElfBlock for DynamicSection {
    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RW)
    }

    fn program_header(&self) -> Vec<ProgramHeaderEntry> {
        //program DYNAMIC
        vec![ProgramHeaderEntry {
            p_type: elf::PT_DYNAMIC,
            p_flags: elf::PF_R | elf::PF_W,
            p_offset: self.file_offset as u64,
            p_vaddr: self.address,
            p_paddr: 0,
            p_filesz: self.size as u64,
            p_memsz: self.size as u64,
            p_align: self.align as u64,
        }]
    }

    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        let index = w.reserve_dynamic_section_index();
        data.section_index.insert(".dynamic".to_string(), index);
        //data.index_dynamic = Some(index);
        self.index = Some(index);
    }

    fn reserve(&mut self, data: &mut Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        let dynamic = data.gen_dynamic();
        let before = w.reserved_len();
        self.file_offset = size_align(before, self.align);
        w.reserve_until(self.file_offset);
        w.reserve_dynamic(dynamic.len());
        let after = w.reserved_len();
        self.size = after - self.file_offset;

        self.base = tracker.add_data(self.alloc().unwrap(), after - before, self.file_offset);
        self.address = self.base as u64 + self.file_offset as u64;
        data.addr.insert(".dynamic".to_string(), self.address);
    }

    fn write(&self, data: &Data, _tracker: &mut SegmentTracker, w: &mut Writer) {
        let dynamic = data.gen_dynamic();
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align);
        //eprintln!("reserve: {:#0x}, {:#0x}", &pos, aligned_pos);
        w.pad_until(aligned_pos);
        w.write_align_dynamic();
        //let pos2 = w.len();
        //eprintln!("reserve: {:#0x}, {:#0x}, {:#0x}", &pos, aligned_pos, pos2);

        // write out dynamic symbols
        for d in dynamic.iter() {
            if let Some(string) = d.string {
                w.write_dynamic_string(d.tag, string);
            } else {
                w.write_dynamic(d.tag, d.val);
            }
        }
    }

    fn write_section_header(&self, _data: &Data, _tracker: &SegmentTracker, w: &mut Writer) {
        w.write_dynamic_section_header(self.address);
    }
}

pub struct RelaDynSection {
    kind: GotKind,
    index: SectionIndex,
    align: usize,
    name_id: Option<StringId>,
    file_offset: usize,
    base: usize,
    size: usize,
    relocation_names: HashMap<String, StringId>,
}

impl RelaDynSection {
    pub fn new(kind: GotKind) -> Self {
        Self {
            kind,
            index: SectionIndex::default(),
            name_id: None,
            align: 0x10,
            file_offset: 0,
            base: 0,
            size: 0,
            relocation_names: HashMap::default(),
        }
    }
}

impl ElfBlock for RelaDynSection {
    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RO)
    }

    fn reserve_section_index(&mut self, _data: &mut Data, w: &mut Writer) {
        let name = self.kind.rel_section_name();
        self.name_id = Some(w.add_section_name(name.as_bytes()));
        self.index = w.reserve_section_index();
    }

    fn reserve(&mut self, data: &mut Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        let unapplied = self.kind.unapplied(data);
        let before = w.reserved_len();
        self.file_offset = size_align(before, self.align);
        w.reserve_until(self.file_offset);

        w.reserve_relocations(unapplied.len(), true);

        let after = w.reserved_len();

        self.size = after - self.file_offset;
        self.base = tracker.add_data(self.alloc().unwrap(), after - before, before);
        data.addr_reladyn = self.base as u64 + self.file_offset as u64;
    }

    fn write(&self, data: &Data, _tracker: &mut SegmentTracker, w: &mut Writer) {
        let unapplied = self.kind.unapplied(data);
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align);
        w.pad_until(aligned_pos);

        let got_addr = data.addr_get(".got");
        // we are writing a relocation for the GOT entries
        let start = 3;
        for (index, (sym, rel)) in unapplied.iter().enumerate() {
            eprintln!("unapplied: {:?}", (sym, rel));
            let r_offset = got_addr as usize + (index + start) * std::mem::size_of::<usize>();
            let r_addend = 0;
            // we needed to fork object in order to access .0
            let r_sym = sym.name.unwrap().0 as u32;
            let r_type = elf::R_X86_64_GLOB_DAT;
            w.write_relocation(
                true,
                &object::write::elf::Rel {
                    r_offset: r_offset as u64,
                    r_sym,
                    r_type,
                    r_addend,
                },
            );
        }
    }

    fn write_section_header(&self, data: &Data, _tracker: &SegmentTracker, w: &mut Writer) {
        let unapplied = self.kind.unapplied(data);
        w.write_relocation_section_header(
            self.name_id.unwrap(),
            SectionIndex::default(),
            *data.section_index.get(&".dynsym".to_string()).unwrap(),
            self.file_offset,
            unapplied.len(),
            true,
        );
    }
}

pub struct RelocationSection {
    alloc: AllocSegment,
    index: SectionIndex,
    target_section_index: SectionIndex,
    align: usize,
    name_id: Option<StringId>,
    file_offset: usize,
    relocations: Vec<CodeRelocation>,
}

impl RelocationSection {
    pub fn new(alloc: AllocSegment, section: &ProgSection) -> Self {
        let relocations = section.relocations.clone();
        Self {
            alloc,
            index: SectionIndex::default(),
            target_section_index: SectionIndex::default(),
            name_id: None,
            align: 0x10,
            file_offset: 0,
            relocations,
        }
    }
}

impl ElfBlock for RelocationSection {
    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RO)
    }
    fn reserve_section_index(&mut self, _data: &mut Data, w: &mut Writer) {
        match self.alloc {
            AllocSegment::RX => {
                self.name_id = Some(w.add_section_name(".rela.text".as_bytes()));
                self.index = w.reserve_section_index();
            }
            AllocSegment::RO => {
                self.name_id = Some(w.add_section_name(".rela.data".as_bytes()));
                self.index = w.reserve_section_index();
            }
            _ => (),
        }
    }

    fn reserve(&mut self, _data: &mut Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        let before = w.reserved_len();
        self.file_offset = w.reserve_relocations(self.relocations.len(), true);
        let after = w.reserved_len();
        tracker.add_data(self.alloc().unwrap(), after - before, before);
    }

    fn update(&mut self, data: &mut Data) {
        match self.alloc {
            AllocSegment::RX => {
                self.target_section_index = data.section_index_get(".text");
            }
            AllocSegment::RO => {
                self.target_section_index = data.section_index_get(".data");
            }
            _ => (),
        }
    }

    fn write(&self, data: &Data, _tracker: &mut SegmentTracker, w: &mut Writer) {
        w.write_align_relocation();

        for rel in self.relocations.iter() {
            let r_offset = rel.offset;
            let r_addend = rel.r.addend;
            rel.r.target;
            let r_sym = self.target_section_index.0;
            let r_type = crate::linker::relocations::r_type(data.arch, &rel.r).unwrap();
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

    fn write_section_header(&self, data: &Data, _tracker: &SegmentTracker, w: &mut Writer) {
        w.write_relocation_section_header(
            self.name_id.unwrap(),
            // section the relocations apply to (.text)
            *data.section_index.get(".text").unwrap(),
            // .symtab section
            *data.section_index.get(".symtab").unwrap(),
            self.file_offset,
            self.relocations.len(),
            true,
        );
    }
}

#[derive(Default)]
pub struct StrTabSection {
    index: Option<SectionIndex>,
}
impl ElfBlock for StrTabSection {
    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        let index = w.reserve_strtab_section_index();
        data.section_index.insert(".strtab".to_string(), index);
    }

    fn reserve(&mut self, _data: &mut Data, _tracker: &mut SegmentTracker, w: &mut Writer) {
        w.reserve_strtab();
    }

    fn write(&self, _data: &Data, _tracker: &mut SegmentTracker, w: &mut Writer) {
        w.write_strtab();
    }

    fn write_section_header(&self, _data: &Data, _tracker: &SegmentTracker, w: &mut Writer) {
        w.write_strtab_section_header();
    }
}

pub struct SymTabSection {
    index: Option<SectionIndex>,
    align: usize,
    symbols: Vec<Sym>,
}
impl Default for SymTabSection {
    fn default() -> Self {
        Self {
            index: None,
            align: 0x10,
            symbols: vec![],
        }
    }
}

impl ElfBlock for SymTabSection {
    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        let index = w.reserve_symtab_section_index();
        data.section_index.insert(".symtab".to_string(), index);
        //data.index_symtab = Some(index);
        if w.symtab_shndx_needed() {
            w.reserve_symtab_shndx_section_index();
        }
    }

    fn reserve(&mut self, data: &mut Data, _tracker: &mut SegmentTracker, w: &mut Writer) {
        // reserve the symbols in the various sections
        for s in data.symbols.values() {
            w.reserve_symbol_index(s.section_index);
        }
        w.reserve_symtab();

        if w.symtab_shndx_needed() {
            w.reserve_symtab_shndx();
        }
    }

    fn write(&self, data: &Data, _tracker: &mut SegmentTracker, w: &mut Writer) {
        // write symbols
        w.write_null_symbol();

        let mut symbols = vec![];
        for s in data.symbols.values() {
            symbols.push(s.get_symbol());
        }

        // write them, locals first
        let mut num_locals = 0;
        symbols
            .iter()
            .filter(|s| s.st_info >> 4 == elf::STB_LOCAL)
            .for_each(|s| {
                w.write_symbol(s);
                num_locals += 1;
            });

        symbols
            .iter()
            .filter(|s| s.st_info >> 4 != elf::STB_LOCAL)
            .for_each(|s| {
                w.write_symbol(s);
            });

        if w.symtab_shndx_needed() {
            w.write_symtab_shndx();
        }
    }

    fn write_section_header(&self, data: &Data, _tracker: &SegmentTracker, w: &mut Writer) {
        let mut symbols = vec![];
        for s in data.symbols.values() {
            symbols.push(s.get_symbol());
        }

        let mut num_locals = 0;

        symbols
            .iter()
            .filter(|s| s.st_info >> 4 == elf::STB_LOCAL)
            .for_each(|_s| {
                num_locals += 1;
            });

        // one greater than the symbol table index of the last
        // local symbol (binding STB_LOCAL)
        w.write_symtab_section_header(num_locals as u32 + 1);
        if w.symtab_shndx_needed() {
            w.write_symtab_shndx_section_header();
        }
    }
}

pub struct DynSymSection {
    index: Option<SectionIndex>,
    align: usize,
    start: usize,
    base: usize,
    size: usize,
}
impl Default for DynSymSection {
    fn default() -> Self {
        Self {
            index: None,
            align: 0x10,
            start: 0,
            base: 0,
            size: 0,
        }
    }
}

impl ElfBlock for DynSymSection {
    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RO)
    }
    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        let index = w.reserve_dynsym_section_index();
        data.section_index.insert(".dynsym".to_string(), index);

        w.reserve_null_dynamic_symbol_index();
        for _ in data.sections.unapplied_data.iter() {
            let _symbol_id = Some(w.reserve_dynamic_symbol_index());
        }
        for _ in data.sections.unapplied_text.iter() {
            let _symbol_id = Some(w.reserve_dynamic_symbol_index());
        }
    }

    fn reserve(&mut self, data: &mut Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        let pos = w.reserved_len();
        let align_pos = size_align(pos, self.align);
        w.reserve_until(align_pos);

        self.start = w.reserved_len();
        w.reserve_dynsym();
        let after = w.reserved_len();
        self.base = tracker.add_data(self.alloc().unwrap(), after - pos, self.start);
        self.size = after - self.start;
        data.addr_dynsym = self.base as u64 + self.start as u64;
        data.size_dynsym = self.size;
    }

    fn write(&self, data: &Data, _tracker: &mut SegmentTracker, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align);
        w.pad_until(aligned_pos);

        w.write_null_dynamic_symbol();
        for (sym, _r) in data.sections.unapplied_data.iter() {
            w.write_dynamic_symbol(sym);
        }
        for (sym, _r) in data.sections.unapplied_text.iter() {
            w.write_dynamic_symbol(sym);
        }
    }

    fn write_section_header(&self, data: &Data, _tracker: &SegmentTracker, w: &mut Writer) {
        let len = data.sections.unapplied_data.len() + data.sections.unapplied_text.len();
        w.write_dynsym_section_header(data.addr_dynsym, len as u32 + 1);
    }
}

pub struct DynStrSection {
    index: Option<SectionIndex>,
    align: usize,
    start: usize,
    base: usize,
    size: usize,
}
impl Default for DynStrSection {
    fn default() -> Self {
        Self {
            index: None,
            align: 0x10,
            start: 0,
            base: 0,
            size: 0,
        }
    }
}
impl ElfBlock for DynStrSection {
    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RO)
    }
    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        let index = w.reserve_dynstr_section_index();
        data.section_index.insert(".dynstr".to_string(), index);
    }

    fn reserve(&mut self, data: &mut Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        let pos = w.reserved_len();
        let align_pos = size_align(pos, self.align);
        w.reserve_until(align_pos);
        self.start = w.reserved_len();
        w.reserve_dynstr();
        let after = w.reserved_len();
        self.base = tracker.add_data(self.alloc().unwrap(), after - pos, self.start);
        self.size = after - self.start;
        data.addr_set(".dynstr", self.base as u64 + self.start as u64);
        data.size_dynstr = self.size;
    }

    fn write(&self, _data: &Data, _tracker: &mut SegmentTracker, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align);
        w.pad_until(aligned_pos);
        w.write_dynstr();
    }

    fn write_section_header(&self, data: &Data, _tracker: &SegmentTracker, w: &mut Writer) {
        w.write_dynstr_section_header(data.addr_get(".dynstr"));
    }
}

#[derive(Default)]
pub struct ShStrTabSection {
    index: Option<SectionIndex>,
}
impl ElfBlock for ShStrTabSection {
    fn reserve_section_index(&mut self, _data: &mut Data, w: &mut Writer) {
        let _shstrtab_index = w.reserve_shstrtab_section_index();
    }

    fn reserve(&mut self, _data: &mut Data, _tracker: &mut SegmentTracker, w: &mut Writer) {
        w.reserve_shstrtab();
    }

    fn write(&self, _data: &Data, _tracker: &mut SegmentTracker, w: &mut Writer) {
        w.write_shstrtab();
    }

    fn write_section_header(&self, _data: &Data, _tracker: &SegmentTracker, w: &mut Writer) {
        w.write_shstrtab_section_header();
    }
}

#[derive(Default)]
pub struct HashSection {
    index: Option<SectionIndex>,
    base: usize,
    offset: usize,
    addr: usize,
    bucket_count: u32,
    chain_count: u32,
}
impl ElfBlock for HashSection {
    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RO)
    }

    fn reserve_section_index(&mut self, _data: &mut Data, w: &mut Writer) {
        self.index = Some(w.reserve_hash_section_index());
    }

    fn reserve(&mut self, data: &mut Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        let align = self.alloc().unwrap().align();
        let pos = w.reserved_len();
        let align_pos = size_align(pos, align);
        w.reserve_until(align_pos);
        self.offset = w.reserved_len();

        w.reserve_hash(self.bucket_count, self.chain_count);

        let after = w.reserved_len();
        let delta = after - pos;
        self.base = tracker.add_data(self.alloc().unwrap(), delta, self.offset);
        self.addr = self.base + self.offset;
        data.addr_hash = self.addr as u64;
    }

    fn write(&self, _data: &Data, _tracker: &mut SegmentTracker, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.alloc().unwrap().align());
        w.pad_until(aligned_pos);
        w.write_hash(self.bucket_count, self.chain_count, |x| Some(x));
    }

    fn write_section_header(&self, _data: &Data, _tracker: &SegmentTracker, w: &mut Writer) {
        w.write_hash_section_header(self.addr as u64);
    }
}

pub enum GotKind {
    GOT,
    GOTPLT,
}

impl GotKind {
    pub fn section_name(&self) -> &'static str {
        match self {
            Self::GOT => ".got",
            Self::GOTPLT => ".got.plt",
        }
    }

    pub fn rel_section_name(&self) -> &'static str {
        match self {
            Self::GOT => ".rela.dyn",
            Self::GOTPLT => ".rela.plt",
        }
    }

    pub fn start_index(&self) -> usize {
        match self {
            Self::GOT => 0,
            Self::GOTPLT => 3,
        }
    }

    pub fn unapplied<'a>(&self, data: &'a Data) -> &'a Vec<(Sym, CodeRelocation)> {
        match self {
            GotKind::GOT => &data.sections.unapplied_data,
            GotKind::GOTPLT => &data.sections.unapplied_text,
        }
    }
}

pub struct GotSection {
    kind: GotKind,
    name_id: Option<StringId>,
    index: Option<SectionIndex>,
    bytes: Vec<u8>,
    base: usize,
    file_offset: usize,
    addr: usize,
    size: usize,
}
impl GotSection {
    pub fn new(kind: GotKind) -> Self {
        Self {
            kind,
            name_id: None,
            index: None,
            bytes: vec![],
            base: 0,
            file_offset: 0,
            addr: 0,
            size: 0,
        }
    }
}

impl ElfBlock for GotSection {
    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RW)
    }

    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        let name = self.kind.section_name();
        self.name_id = Some(w.add_section_name(name.as_bytes()));
        let index = w.reserve_section_index();
        data.section_index.insert(name.to_string(), index);
        self.index = Some(index);
    }

    fn reserve(&mut self, data: &mut Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        // each entry in unapplied will be a GOT entry
        //let unapplied = self.kind.unapplied(data);
        let unapplied = match self.kind {
            GotKind::GOT => &data.sections.unapplied_data,
            GotKind::GOTPLT => &data.sections.unapplied_text,
        };
        let name = self.kind.section_name();

        let len = unapplied.len() + self.kind.start_index();
        let size = len * std::mem::size_of::<usize>();
        self.bytes.resize(size, 0);

        let align = self.alloc().unwrap().align();

        let pos = w.reserved_len();
        let align_pos = size_align(pos, align);
        w.reserve_until(align_pos);
        self.file_offset = w.reserved_len();
        w.reserve(self.bytes.len(), align);
        let after = w.reserved_len();
        self.size = after - self.file_offset;
        let delta = after - pos;
        self.base = tracker.add_data(self.alloc().unwrap(), delta, self.file_offset);

        self.addr = self.base + self.file_offset;

        // add got pointers to the pointers table, so we can do relocations
        for (index, (_, r)) in unapplied.iter().enumerate() {
            let name = &r.name;
            let addr = self.addr + (index + self.kind.start_index()) * std::mem::size_of::<usize>();
            eprintln!("adding: {}, {:#0x}", &name, addr as u64);
            data.pointers.insert(name.clone(), addr as u64);
        }

        // update section pointers
        data.addr.insert(name.to_string(), self.addr as u64);
    }

    fn write(&self, _data: &Data, _tracker: &mut SegmentTracker, w: &mut Writer) {
        //let align = self.alloc().unwrap().align();
        let pos = w.len();
        let aligned_pos = size_align(pos, self.alloc().unwrap().align());
        w.pad_until(aligned_pos);
        w.write(self.bytes.as_slice());
    }

    fn write_section_header(&self, _data: &Data, _tracker: &SegmentTracker, w: &mut Writer) {
        let sh_flags = self.alloc().unwrap().section_header_flags() as u64;
        let sh_addralign = self.alloc().unwrap().align() as u64;
        w.write_section_header(&object::write::elf::SectionHeader {
            name: self.name_id,
            sh_type: elf::SHT_PROGBITS,
            sh_flags,
            sh_addr: self.addr as u64,
            sh_offset: self.file_offset as u64,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign,
            sh_size: self.size as u64,
        });
    }
}

pub struct PltSection {
    name_id: Option<StringId>,
    index: Option<SectionIndex>,
    bytes: Vec<u8>,
    base: usize,
    file_offset: usize,
    addr: usize,
    size: usize,
}
impl PltSection {
    pub fn new() -> Self {
        Self {
            name_id: None,
            index: None,
            bytes: vec![],
            base: 0,
            file_offset: 0,
            addr: 0,
            size: 0,
        }
    }
}

impl PltSection {
    pub fn get_name() -> &'static str {
        ".plt"
    }
}

impl ElfBlock for PltSection {
    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RX)
    }

    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        let name = Self::get_name();
        self.name_id = Some(w.add_section_name(name.as_bytes()));
        let index = w.reserve_section_index();
        data.section_index.insert(name.to_string(), index);
        self.index = Some(index);
    }

    fn reserve(&mut self, data: &mut Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        let unapplied = &data.sections.unapplied_text;

        // length + 1, to account for the stub.  Each entry is 0x10 in size
        self.size = (1 + unapplied.len()) * 0x10;
        self.bytes.resize(self.size, 0);
        let align = self.alloc().unwrap().align();

        let pos = w.reserved_len();
        let align_pos = size_align(pos, align);
        w.reserve_until(align_pos);
        self.file_offset = w.reserved_len();
        w.reserve(self.bytes.len(), align);
        let after = w.reserved_len();
        let delta = after - pos;
        self.base = tracker.add_data(self.alloc().unwrap(), delta, self.file_offset);
        self.addr = self.base + self.file_offset;
        // update section pointers
        data.addr
            .insert(Self::get_name().to_string(), self.addr as u64);
    }

    fn write(&self, data: &Data, _tracker: &mut SegmentTracker, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.alloc().unwrap().align());
        w.pad_until(aligned_pos);

        let got_addr = *data.addr.get(".got.plt").unwrap() as isize;
        let vbase = self.addr as isize;

        let mut stub: Vec<u8> = vec![
            // 0x401020: push   0x2fe2(%rip)        # 404008 <_GLOBAL_OFFSET_TABLE_+0x8>
            // got+8 - rip // (0x404000+0x8) - (0x401020 + 0x06)
            0xff, 0x35, 0xe2, 0x2f, 0x00, 0x00,
            // 0x401026: jump to GOT[2]
            // jmp    *0x2fe4(%rip)        # 404010 <_GLOBAL_OFFSET_TABLE_+0x10>
            0xff, 0x25, 0xe4, 0x2f, 0x00, 0x00,
            // 40102c:       0f 1f 40 00             nopl   0x0(%rax)
            0x0f, 0x1f, 0x40, 0x00,
        ];

        unsafe {
            let patch = (stub.as_mut_ptr().offset(2)) as *mut i32;
            let got1 = got_addr + 0x8 - (vbase + 0x06);
            *patch = got1 as i32;

            let patch = (stub.as_mut_ptr().offset(2 + 6)) as *mut i32;
            let got2 = got_addr + 0x10 - (vbase + 0x0c);
            *patch = got2 as i32;
        }

        let unapplied = &data.sections.unapplied_text;

        for (i, _) in unapplied.iter().enumerate() {
            let mut slot: Vec<u8> = vec![
                // # 404018 <puts@GLIBC_2.2.5>, .got.plot 4th entry, GOT[3], jump there
                // # got.plt[3] = 0x401036, initial value,
                // which points to the second instruction (push) in this plt entry
                // # the dynamic linker will update GOT[3] with the actual address, so this lookup only happens once
                // 401030:       ff 25 e2 2f 00 00       jmp    *0x2fe2(%rip)        # 404018 <puts@GLIBC_2.2.5>
                0xff, 0x25, 0xe2, 0x2f, 0x00, 0x00,
                // # push plt index onto the stack
                // # this is a reference to the entry in the relocation table defined by DT_JMPREL (.rela.plt)
                // # that reloc will have type R_X86_64_JUMP_SLOT
                // # the reloc will have an offset that points to GOT[3], 0x404018 = BASE + 3*0x08
                // 401036:       68 00 00 00 00          push   $0x0
                0x68, 0x00, 0x00, 0x00, 0x00,
                // # jump to stub, which is (i+2)*0x10 relative to rip
                // 40103b:       e9 e0 ff ff ff          jmp    401020 <_init+0x20>,
                0xe9, 0xe0, 0xff, 0xff, 0xff,
            ];
            stub.extend(slot);

            unsafe {
                let offset = (i as isize + 1) * 0x10;
                let patch = (stub.as_mut_ptr().offset(offset + 2)) as *mut i32;
                let rip = vbase + offset + 6;
                let addr = got_addr + (3 + i as isize) * 0x08 - rip;
                *patch = addr as i32;

                let patch = (stub.as_mut_ptr().offset(offset + 7)) as *mut i32;
                *patch = i as i32;

                // next instruction
                let rip = vbase + offset + 0x10;
                let addr = self.addr as isize - rip;
                eprintln!("got: {}, {:#0x}, {:#0x}", i, rip, addr);
                let patch = (stub.as_mut_ptr().offset(offset + 0x0c)) as *mut i32;
                *patch = addr as i32;
            }
        }

        //let mut bytes = self.bytes.clone();
        //bytes.as_mut_slice().copy_from_slice(stub.as_slice());

        // write stub
        // for each entry, write a slot
        w.write(stub.as_slice());
    }

    fn write_section_header(&self, _data: &Data, _tracker: &SegmentTracker, w: &mut Writer) {
        let sh_flags = self.alloc().unwrap().section_header_flags() as u64;
        let sh_addralign = self.alloc().unwrap().align() as u64;
        w.write_section_header(&object::write::elf::SectionHeader {
            name: self.name_id,
            sh_type: elf::SHT_PROGBITS,
            sh_flags,
            sh_addr: self.addr as u64,
            sh_offset: self.file_offset as u64,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign,
            sh_size: self.size as u64,
        });
    }
}

/*
#[derive(Default)]
pub struct GotPltSection {
    got_name_id: Option<StringId>,
    plt_name_id: Option<StringId>,
    got_index: Option<SectionIndex>,
    plt_index: Option<SectionIndex>,
    got: Vec<u8>,
    plt: Vec<u8>,
    base: usize,
    got_offset: usize,
    plt_offset: usize,
    got_addr: usize,
    plt_addr: usize,
    got_size: usize,
    plt_size: usize,
    start_index: usize,
}
impl ElfBlock for GotPltSection {
    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RW)
    }

    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        self.start_index = 3;
        self.got_name_id = Some(w.add_section_name(".got".as_bytes()));
        self.plt_name_id = Some(w.add_section_name(".got.plt".as_bytes()));
        let got_index = w.reserve_section_index();
        let plt_index = w.reserve_section_index();
        data.section_index.insert(".got".to_string(), got_index);
        data.section_index.insert(".got.plt".to_string(), plt_index);
        self.got_index = Some(got_index);
        self.plt_index = Some(plt_index);
    }

    fn reserve(&mut self, data: &mut Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        // each entry in unapplied will be a GOT entry
        let len = data.sections.unapplied.len() + self.start_index;
        let got_size = len * std::mem::size_of::<usize>();
        self.got.resize(got_size, 0);
        let plt_size = len * std::mem::size_of::<usize>() * 2;
        self.plt.resize(plt_size, 0);

        let align = self.alloc().unwrap().align();

        let pos1 = w.reserved_len();
        let align_pos = size_align(pos1, align);
        w.reserve_until(align_pos);
        self.got_offset = w.reserved_len();
        w.reserve(self.got.len(), align);
        self.got_size = w.reserved_len() - self.got_offset;

        let pos2 = w.reserved_len();
        let align_pos = size_align(pos2, align);
        w.reserve_until(align_pos);
        self.plt_offset = w.reserved_len();
        w.reserve(self.plt.len(), align);
        self.plt_size = w.reserved_len() - self.plt_offset;

        let after = w.reserved_len();
        let delta = after - pos1;
        self.base = tracker.add_data(self.alloc().unwrap(), delta, self.got_offset);

        self.got_addr = self.base + self.got_offset;
        self.plt_addr = self.base + self.plt_offset;

        // add got pointers to the pointers table, so we can do relocations
        for (index, (_, r)) in data.sections.unapplied.iter().enumerate() {
            let name = &r.name;
            let addr = self.got_addr + (index + self.start_index) * std::mem::size_of::<usize>();
            data.pointers.insert(name.clone(), addr as u64);
        }

        // update section pointers
        data.addr.insert(".got".to_string(), self.got_addr as u64);
        data.addr
            .insert(".got.plt".to_string(), self.plt_addr as u64);
    }

    fn write(&self, _data: &Data, _tracker: &mut SegmentTracker, w: &mut Writer) {
        let align = self.alloc().unwrap().align();
        let pos = w.len();
        let aligned_pos = size_align(pos, self.alloc().unwrap().align());
        w.pad_until(aligned_pos);
        w.write(self.got.as_slice());
        w.write_align(align);
        w.write(self.plt.as_slice());
    }

    fn write_section_header(&self, _data: &Data, _tracker: &SegmentTracker, w: &mut Writer) {
        let sh_flags = self.alloc().unwrap().section_header_flags() as u64;
        let sh_addralign = self.alloc().unwrap().align() as u64;
        w.write_section_header(&object::write::elf::SectionHeader {
            name: self.got_name_id,
            sh_type: elf::SHT_PROGBITS,
            sh_flags,
            sh_addr: self.got_addr as u64,
            sh_offset: self.got_offset as u64,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign,
            sh_size: self.got_size as u64,
        });
        w.write_section_header(&object::write::elf::SectionHeader {
            name: self.plt_name_id,
            sh_type: elf::SHT_PROGBITS,
            sh_flags,
            sh_addr: self.plt_addr as u64,
            sh_offset: self.plt_offset as u64,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign,
            sh_size: self.plt_size as u64,
        });
    }
}
*/

use std::ffi::CString;
pub struct InterpSection {
    alloc: AllocSegment,
    name_id: Option<StringId>,
    cstr: CString,
    offset: usize,
    addr: usize,
    base: usize,
}

impl InterpSection {
    pub fn new(data: &Data) -> Self {
        let interp = data.interp.as_bytes().to_vec();
        let cstr = std::ffi::CString::new(interp).unwrap();
        Self {
            alloc: AllocSegment::RO,
            cstr,
            name_id: None,
            offset: 0,
            addr: 0,
            base: 0,
        }
    }

    pub fn as_slice(&self) -> &[u8] {
        self.cstr.as_bytes_with_nul()
    }
}

impl ElfBlock for InterpSection {
    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RO)
    }
    fn program_header(&self) -> Vec<ProgramHeaderEntry> {
        let buf = self.as_slice();
        vec![ProgramHeaderEntry {
            p_type: elf::PT_INTERP,
            p_flags: self.alloc().unwrap().program_header_flags(),
            p_offset: self.offset as u64,
            p_vaddr: self.addr as u64,
            p_paddr: 0,
            p_filesz: buf.len() as u64,
            p_memsz: buf.len() as u64,
            p_align: self.alloc.align() as u64,
        }]
    }

    fn reserve_section_index(&mut self, _data: &mut Data, w: &mut Writer) {
        self.name_id = Some(w.add_section_name(".interp".as_bytes()));
        let _index = w.reserve_section_index();
    }

    fn reserve(&mut self, _data: &mut Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        let align = self.alloc.align();
        let pos = w.reserved_len();
        let align_pos = size_align(pos, align);
        w.reserve_until(align_pos);
        self.offset = w.reserved_len();

        let buf = self.as_slice();
        w.reserve(buf.len(), align);
        let after = w.reserved_len();
        let delta = after - pos;
        self.base = tracker.add_data(self.alloc().unwrap(), delta, self.offset);
        self.addr = self.base + self.offset;
    }

    fn write(&self, _data: &Data, _tracker: &mut SegmentTracker, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.alloc.align());
        w.pad_until(aligned_pos);
        w.write(self.as_slice());
    }

    fn write_section_header(&self, _data: &Data, _tracker: &SegmentTracker, w: &mut Writer) {
        if let Some(name_id) = self.name_id {
            w.write_section_header(&object::write::elf::SectionHeader {
                name: Some(name_id),
                sh_type: elf::SHT_PROGBITS,
                sh_flags: self.alloc.section_header_flags() as u64,
                sh_addr: self.base as u64 + self.offset as u64,
                sh_offset: self.offset as u64,
                sh_info: 0,
                sh_link: 0,
                sh_entsize: 0,
                sh_addralign: self.alloc.align() as u64,
                sh_size: self.as_slice().len() as u64,
            });
        }
    }
}

pub struct BufferSection {
    alloc: AllocSegment,
    name: Option<String>,
    name_id: Option<StringId>,
    addr: usize,
    offset: usize,
    size: usize,
    base: usize,
    buf: Vec<u8>,
    pub unlinked: Vec<UnlinkedCodeSegment>,
    //relocations: Vec<CodeRelocation>,
    section: ProgSection,
    pointers: HashMap<String, u64>,
    symbols: HashMap<String, ProgSymbol>,
}

impl BufferSection {
    pub fn new(
        alloc: AllocSegment,
        name: Option<String>,
        name_id: Option<StringId>,
        buf: Vec<u8>,
        section: ProgSection,
    ) -> Self {
        Self {
            alloc,
            name,
            name_id,
            addr: 0,
            offset: 0,
            size: 0,
            base: 0,
            buf,
            unlinked: vec![],
            //relocations: vec![],
            section,
            pointers: HashMap::new(),
            symbols: HashMap::new(),
        }
    }

    pub fn apply_relocations(&self, v_base: usize, pointers: &HashMap<String, u64>) {
        let patch_base = self.buf.as_ptr();
        for r in self.section.relocations.iter() {
            if let Some(addr) = pointers.get(&r.name) {
                log::debug!(
                    "R-{:?}: vbase: {:#0x}, addr: {:#0x}, {}",
                    self.alloc().unwrap(),
                    v_base,
                    *addr as usize,
                    &r.name
                );
                r.patch(patch_base as *mut u8, v_base as *mut u8, *addr as *const u8);
            } else {
                unreachable!("Unable to locate symbol: {}, {}", &r.name, &r);
            }
        }

        let mut symbols = vec![];
        for (name, p) in self.pointers.iter() {
            symbols.push(Symbol::new(self.addr as u64, *p - self.addr as u64, name));
        }
        disassemble_code_with_symbols(self.buf.as_slice(), &symbols, &self.section.relocations);
    }
}

impl ElfBlock for BufferSection {
    fn alloc(&self) -> Option<AllocSegment> {
        Some(self.alloc)
    }

    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        let index = w.reserve_section_index();
        self.section.index = Some(index);
        data.section_index_set(&self.name.as_ref().unwrap(), index);
    }

    fn reserve(&mut self, data: &mut Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        let align = self.alloc.align();
        let pos = w.reserved_len();
        let align_pos = size_align(pos, align);
        w.reserve_until(align_pos);
        let start = w.reserved_len();

        w.reserve(self.buf.len(), align);
        let after = w.reserved_len();
        self.size = self.buf.len();
        self.offset = start;
        let _delta = after - pos;

        self.symbols = self.section.symbols.clone();
        let section_index = self.section.index.clone();

        // add section to the tracker, so we have a base address
        self.base = tracker.add_section(self.alloc, &self.section, start);

        // write symbols
        for (name, s) in &self.symbols {
            let mut s = s.clone();
            s.section_index = section_index;
            s.base = self.base;
            let addr = self.base + self.offset + s.s.address as usize;
            s.s.address = addr as u64;
            data.pointers.insert(name.clone(), addr as u64);
            self.pointers.insert(name.clone(), addr as u64);
            data.symbols.insert(name.clone(), s.clone());
        }

        let name = self.name.as_ref().unwrap();
        data.addr_set(&name, self.base as u64 + self.offset as u64);
        self.addr = self.base as usize + self.offset;

        data.addr
            .insert(self.name.clone().unwrap(), self.addr as u64);
    }

    fn update(&mut self, data: &mut Data) {
        self.apply_relocations(self.addr, &data.pointers);
    }

    fn write(&self, _data: &Data, _tracker: &mut SegmentTracker, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.alloc.align());
        w.pad_until(aligned_pos);
        w.write(self.buf.as_slice());
    }

    fn write_section_header(&self, _data: &Data, _tracker: &SegmentTracker, w: &mut Writer) {
        if let Some(name_id) = self.name_id {
            w.write_section_header(&object::write::elf::SectionHeader {
                name: Some(name_id),
                sh_type: elf::SHT_PROGBITS,
                sh_flags: self.alloc.section_header_flags() as u64,
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

pub struct LocalSymbol {
    pub(crate) section: String,
    pub(crate) symbol: String,
    pub(crate) offset: usize,
    pub(crate) string_id: Option<StringId>,
}
impl LocalSymbol {
    pub fn new(
        symbol: String,
        section: String,
        offset: usize,
        string_id: Option<StringId>,
    ) -> Self {
        Self {
            section,
            symbol,
            offset,
            string_id,
        }
    }
}

/*
#[derive(Default)]
pub struct SymbolWriter {
    //locals: Vec<LocalSymbol>,
}

impl SymbolWriter {
    pub fn new() -> Self {
        /*
        let locals = vec![
            LocalSymbol::new("_DYNAMIC".into(), ".dynamic".into(), 0),
            LocalSymbol::new("_GLOBAL_OFFSET_TABLE_".into(), ".got.plt".into(), 0),
        ];
        Self { locals }
        */
        Self {}
    }
}

impl ElfBlock for SymbolWriter {
    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RO)
    }

    fn reserve_strings<'a>(&self, symbols: &'a mut Vec<LocalSymbol>, w: &mut Writer<'a>) {
        for local in symbols.iter_mut() {
            local.string_id = Some(w.add_string(local.symbol.as_bytes()));
        }
    }

    //fn reserve(&mut self, _data: &mut Data, tracker: &mut SegmentTracker, w: &mut Writer) {
    //}

    /*
    fn update_tracker(&mut self, data: &Data, tracker: &mut SegmentTracker) {
        for local in self.locals.iter() {
            let addr = data.get_addr(&local.section).unwrap() + local.offset as u64;
            // Add symbols
            //w.reserve_symbol_index(self.index);
            let st_info = (elf::STB_LOCAL << 4) + (elf::STT_OBJECT & 0x0f);
            let st_other = elf::STV_DEFAULT;
            let st_shndx = 0;
            let st_value = addr; //(self.base + self.offset) as u64;
            let st_size = 0;
            tracker.symbols.push(object::write::elf::Sym {
                name: local.string_id,
                section: data.index_dynamic,
                st_info,
                st_other,
                st_shndx,
                st_value,
                st_size,
            });
        }
    }
    */

    //fn write(&self, _data: &Data, _tracker: &mut SegmentTracker, w: &mut Writer) {
    //}

    //fn write_section_header(&self, _data: &Data, _tracker: &SegmentTracker, w: &mut Writer) {
    //}
}


*/
