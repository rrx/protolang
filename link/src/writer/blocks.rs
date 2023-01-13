use super::*;
use object::write::elf::Sym;

pub trait ElfBlock {
    fn name(&self) -> String;
    fn alloc(&self) -> Option<AllocSegment> {
        None
    }
    fn program_header(&self, _block: &ReadBlock) -> Vec<ProgramHeaderEntry> {
        vec![]
    }
    fn reserve_section_index(&mut self, _: &mut Data, _block: &mut ReadBlock, _: &mut Writer) {}
    fn reserve_symbols(&mut self, _data: &mut Data, _block: &ReadBlock, _w: &mut Writer) {}
    fn reserve(&mut self, _: &mut Data, _: &mut SegmentTracker, _: &mut ReadBlock, _: &mut Writer) {
    }
    fn update_tracker(&mut self, _: &Data, _: &mut SegmentTracker) {}
    fn update(&mut self, _: &mut Data) {}
    fn write(&self, _: &Data, _: &mut SegmentTracker, _: &mut ReadBlock, _: &mut Writer) {}
    fn write_section_header(&self, _: &Data, _: &SegmentTracker, _: &ReadBlock, _: &mut Writer) {}
}

pub struct FileHeader {
    size: usize,
    offsets: SectionOffset,
}

impl Default for FileHeader {
    fn default() -> Self {
        Self {
            size: 0,
            offsets: SectionOffset::new(0x01),
        }
    }
}

impl ElfBlock for FileHeader {
    fn name(&self) -> String {
        return "fh".to_string();
    }

    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RO)
    }

    fn reserve_section_index(&mut self, _data: &mut Data, _block: &mut ReadBlock, w: &mut Writer) {
        let _null_section_index = w.reserve_null_section_index();
    }

    fn reserve(
        &mut self,
        _data: &mut Data,
        tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        if w.reserved_len() > 0 {
            panic!("Must start with file header");
        }

        // Start reserving file ranges.
        w.reserve_file_header();
        self.size = w.reserved_len();
        let alloc = self.alloc().unwrap();
        tracker.add_offsets(alloc, &mut self.offsets, self.size, w);
    }

    fn write(
        &self,
        data: &Data,
        _tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        //let mut e_entry = 0;
        let start = data.pointer_get("_start");
        let e_entry = start;

        w.write_file_header(&object::write::elf::FileHeader {
            os_abi: 0x00,         // SysV
            abi_version: 0,       // ignored on linux
            e_type: elf::ET_EXEC, // ET_EXEC - Executable file
            e_machine: 0x3E,      // AMD x86-64
            e_entry,              // e_entry, normally points to _start
            e_flags: 0,           // e_flags
        })
        .unwrap();
    }

    fn write_section_header(
        &self,
        _data: &Data,
        _tracker: &SegmentTracker,
        _block: &ReadBlock,
        w: &mut Writer,
    ) {
        w.write_null_section_header();
    }
}

pub struct ProgramHeader {
    size: usize,
    base: usize,
    offsets: SectionOffset,
    ph_count: usize,
}

impl Default for ProgramHeader {
    fn default() -> Self {
        Self {
            size: 0,
            ph_count: 0,
            base: 0,
            offsets: SectionOffset::new(0x01),
        }
    }
}

impl ElfBlock for ProgramHeader {
    fn name(&self) -> String {
        return "ph".to_string();
    }

    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RO)
    }

    fn program_header(&self, _block: &ReadBlock) -> Vec<ProgramHeaderEntry> {
        vec![
            // program header
            ProgramHeaderEntry {
                p_type: elf::PT_PHDR,
                p_flags: elf::PF_R,
                p_offset: self.offsets.file_offset,
                p_vaddr: self.offsets.address,
                p_paddr: 0,
                p_filesz: self.size as u64,
                p_memsz: self.size as u64,
                p_align: 8,
            },
        ]
    }

    fn reserve(
        &mut self,
        _data: &mut Data,
        tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        self.ph_count = tracker.ph.len();
        let before = w.reserved_len();
        w.reserve_program_headers(self.ph_count as u32);
        let after = w.reserved_len();
        self.size = after - before;

        let alloc = self.alloc().unwrap();
        tracker.add_offsets(alloc, &mut self.offsets, self.size, w);
    }

    fn write(
        &self,
        _data: &Data,
        tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        w.write_align_program_headers();

        for ph in tracker.ph.iter() {
            //eprintln!("ph: {:?}", ph);
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
        assert_eq!(tracker.ph.len(), self.ph_count);
    }
}

use std::ffi::CString;
pub struct InterpSection {
    alloc: AllocSegment,
    name_id: Option<StringId>,
    cstr: CString,
    offsets: SectionOffset,
}

impl InterpSection {
    pub fn new(data: &Data) -> Self {
        let interp = data.interp.as_bytes().to_vec();
        let cstr = std::ffi::CString::new(interp).unwrap();
        Self {
            alloc: AllocSegment::RO,
            cstr,
            name_id: None,
            offsets: SectionOffset::new(0x01),
        }
    }

    pub fn as_slice(&self) -> &[u8] {
        self.cstr.as_bytes_with_nul()
    }
}

impl ElfBlock for InterpSection {
    fn name(&self) -> String {
        return "interp".to_string();
    }
    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RO)
    }
    fn program_header(&self, _block: &ReadBlock) -> Vec<ProgramHeaderEntry> {
        let size = self.as_slice().len() as u64;
        vec![ProgramHeaderEntry {
            p_type: elf::PT_INTERP,
            p_flags: self.alloc().unwrap().program_header_flags(),
            p_offset: self.offsets.file_offset,
            p_vaddr: self.offsets.address,
            p_paddr: 0,
            p_filesz: size,
            p_memsz: size,
            p_align: self.offsets.align as u64,
        }]
    }

    fn reserve_section_index(&mut self, _data: &mut Data, _block: &mut ReadBlock, w: &mut Writer) {
        self.name_id = Some(w.add_section_name(".interp".as_bytes()));
        let _index = w.reserve_section_index();
    }

    fn reserve(
        &mut self,
        _data: &mut Data,
        tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        let align = self.offsets.align as usize;
        let pos = w.reserved_len();
        let align_pos = size_align(pos, align);
        w.reserve_until(align_pos);
        let size = self.as_slice().len();
        w.reserve(size, align);
        tracker.add_offsets(self.alloc().unwrap(), &mut self.offsets, size, w);
    }

    fn write(
        &self,
        _data: &Data,
        _tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.offsets.align as usize);
        w.pad_until(aligned_pos);
        w.write(self.as_slice());
    }

    fn write_section_header(
        &self,
        _data: &Data,
        _tracker: &SegmentTracker,
        _block: &ReadBlock,
        w: &mut Writer,
    ) {
        if let Some(name_id) = self.name_id {
            w.write_section_header(&object::write::elf::SectionHeader {
                name: Some(name_id),
                sh_type: elf::SHT_PROGBITS,
                sh_flags: self.alloc.section_header_flags() as u64,
                sh_addr: self.offsets.address,
                sh_offset: self.offsets.file_offset,
                sh_info: 0,
                sh_link: 0,
                sh_entsize: 0,
                sh_addralign: self.offsets.align,
                sh_size: self.as_slice().len() as u64,
            });
        }
    }
}

pub struct DynamicSection {
    index: Option<SectionIndex>,
    offsets: SectionOffset,
}
impl Default for DynamicSection {
    fn default() -> Self {
        Self {
            index: None,
            offsets: SectionOffset::new(0x08),
        }
    }
}

impl ElfBlock for DynamicSection {
    fn name(&self) -> String {
        return "dynamic".to_string();
    }
    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RW)
    }

    fn program_header(&self, _block: &ReadBlock) -> Vec<ProgramHeaderEntry> {
        //program DYNAMIC
        vec![ProgramHeaderEntry {
            p_type: elf::PT_DYNAMIC,
            p_flags: elf::PF_R | elf::PF_W,
            p_offset: self.offsets.file_offset as u64,
            p_vaddr: self.offsets.address,
            p_paddr: 0,
            p_filesz: self.offsets.size as u64,
            p_memsz: self.offsets.size as u64,
            p_align: self.offsets.align as u64,
        }]
    }

    fn reserve_section_index(&mut self, data: &mut Data, _block: &mut ReadBlock, w: &mut Writer) {
        let index = w.reserve_dynamic_section_index();
        data.section_index.insert(".dynamic".to_string(), index);
        self.index = Some(index);
    }

    fn reserve(
        &mut self,
        data: &mut Data,
        tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        let dynamic = data.gen_dynamic();
        let before = w.reserved_len();
        let file_offset = size_align(before, self.offsets.align as usize);
        w.reserve_until(file_offset);
        w.reserve_dynamic(dynamic.len());
        let after = w.reserved_len();

        tracker.add_offsets(
            self.alloc().unwrap(),
            &mut self.offsets,
            after - file_offset,
            w,
        );
        data.addr.insert(
            AddressKey::Section(".dynamic".to_string()),
            self.offsets.address,
        );
        data.addr.insert(
            AddressKey::SectionIndex(self.index.unwrap()),
            self.offsets.address,
        );
    }

    fn write(
        &self,
        data: &Data,
        _tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        let dynamic = data.gen_dynamic();
        let pos = w.len();
        let aligned_pos = size_align(pos, self.offsets.align as usize);
        assert_eq!(aligned_pos, self.offsets.file_offset as usize);
        w.pad_until(aligned_pos);
        w.write_align_dynamic();

        // write out dynamic symbols
        for d in dynamic.iter() {
            if let Some(string) = d.string {
                w.write_dynamic_string(d.tag, string);
            } else {
                w.write_dynamic(d.tag, d.val);
            }
        }
    }

    fn write_section_header(
        &self,
        _data: &Data,
        _tracker: &SegmentTracker,
        _block: &ReadBlock,
        w: &mut Writer,
    ) {
        w.write_dynamic_section_header(self.offsets.address);
    }
}

pub struct RelaDynSection {
    kind: GotKind,
    index: SectionIndex,
    name_id: Option<StringId>,
    count: usize,
    relocation_names: HashMap<String, StringId>,
    offsets: SectionOffset,
}

impl RelaDynSection {
    pub fn new(kind: GotKind) -> Self {
        Self {
            kind,
            index: SectionIndex::default(),
            name_id: None,
            count: 0,
            relocation_names: HashMap::default(),
            offsets: SectionOffset::new(0x08),
        }
    }
}

impl ElfBlock for RelaDynSection {
    fn name(&self) -> String {
        return format!("reladyn:{:?}", self.kind);
    }
    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RO)
    }

    fn reserve_section_index(&mut self, _data: &mut Data, _block: &mut ReadBlock, w: &mut Writer) {
        let name = self.kind.rel_section_name();
        self.name_id = Some(w.add_section_name(name.as_bytes()));
        self.index = w.reserve_section_index();
    }

    fn reserve(
        &mut self,
        data: &mut Data,
        tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        let unapplied = data.dynamics.relocations(self.kind);

        self.count = unapplied.len();
        let before = w.reserved_len();
        let file_offset = size_align(before, self.offsets.align as usize);
        w.reserve_until(file_offset);

        w.reserve_relocations(unapplied.len(), true);

        let after = w.reserved_len();

        tracker.add_offsets(
            self.alloc().unwrap(),
            &mut self.offsets,
            after - file_offset,
            w,
        );
        match self.kind {
            GotKind::GOT(_) => {
                data.addr_set(".rela.dyn", self.offsets.address);
                data.size_reladyn = w.rel_size(true) * unapplied.len();
            }
            GotKind::GOTPLT => {
                data.addr_set(".rela.plt", self.offsets.address);
                data.size_relaplt = w.rel_size(true) * unapplied.len();
            }
        }
    }

    fn write(
        &self,
        data: &Data,
        _tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        let unapplied = data.dynamics.relocations(self.kind);
        let pos = w.len();
        let aligned_pos = size_align(pos, self.offsets.align as usize);
        assert_eq!(self.count, unapplied.len());
        assert_eq!(aligned_pos, self.offsets.file_offset as usize);
        w.pad_until(aligned_pos);

        // we are writing a relocation for the GOT entries
        for (index, (relative, name, pointer)) in unapplied.iter().enumerate() {
            eprintln!("unapplied: {}, {}", name, relative);
            let (symbol_index, _sym) = data.dynamics.symbol_get(name, data).unwrap();

            let mut r_addend = 0;
            let r_sym;
            if *relative {
                r_sym = 0;
                if let Some(p) = data.statics.symbol_get(name) {
                    if let Some(addr) = p.resolve(data) {
                        r_addend = addr as i64;
                    }
                }
            } else {
                // we needed to fork object in order to access .0
                r_sym = symbol_index.0;
                r_addend = 0;
            }

            let r_type = match self.kind {
                GotKind::GOT(_) => {
                    if *relative {
                        elf::R_X86_64_RELATIVE
                    } else {
                        elf::R_X86_64_GLOB_DAT
                    }
                }
                GotKind::GOTPLT => elf::R_X86_64_JUMP_SLOT,
            };

            let r_offset = match self.kind {
                GotKind::GOT(_) => {
                    let got_addr = data.addr_get(".got");
                    got_addr as usize + index * std::mem::size_of::<usize>()
                }
                GotKind::GOTPLT => {
                    let start = 3;
                    let plt_addr = data.addr_get(".got.plt");
                    plt_addr as usize + (index + start) * std::mem::size_of::<usize>()
                }
            };

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

    fn write_section_header(
        &self,
        data: &Data,
        _tracker: &SegmentTracker,
        _block: &ReadBlock,
        w: &mut Writer,
    ) {
        let unapplied = data.dynamics.relocations(self.kind);

        let sh_addralign = self.offsets.align;
        let sh_info = SectionIndex::default().0;
        let sh_link = data.section_index.get(&".dynsym".to_string()).unwrap().0;
        let sh_entsize = w.rel_size(true) as u64;

        w.write_section_header(&object::write::elf::SectionHeader {
            name: self.name_id,
            sh_type: elf::SHT_RELA,
            sh_flags: elf::SHF_INFO_LINK.into(),
            sh_addr: self.offsets.address,
            sh_offset: self.offsets.file_offset,
            sh_info,
            sh_link,
            sh_entsize,
            sh_addralign,
            sh_size: sh_entsize * unapplied.len() as u64,
        });
    }
}

/*
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
    fn name(&self) -> String {
        return "reloc".to_string();
    }
    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RO)
    }
    fn reserve_section_index(&mut self, _data: &mut Data, _block: &mut ReadBlock, w: &mut Writer) {
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

    fn reserve(
        &mut self,
        _data: &mut Data,
        tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        let before = w.reserved_len();
        self.file_offset = w.reserve_relocations(self.relocations.len(), true);
        let after = w.reserved_len();
        tracker.add_data(self.alloc().unwrap(), 1, after - before, before);
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

    fn write(
        &self,
        data: &Data,
        _tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
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

    fn write_section_header(
        &self,
        data: &Data,
        _tracker: &SegmentTracker,
        _block: &ReadBlock,
        w: &mut Writer,
    ) {
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
*/

//#[derive(Default)]
pub struct StrTabSection {
    index: Option<SectionIndex>,
    file_offset: usize,
    align: usize,
}
impl StrTabSection {
    pub fn new() -> Self {
        Self {
            index: None,
            file_offset: 0,
            align: 1,
        }
    }
}

impl ElfBlock for StrTabSection {
    fn name(&self) -> String {
        return "strtab".to_string();
    }
    fn reserve_section_index(&mut self, data: &mut Data, _block: &mut ReadBlock, w: &mut Writer) {
        let index = w.reserve_strtab_section_index();
        data.section_index.insert(".strtab".to_string(), index);
    }

    fn reserve(
        &mut self,
        _data: &mut Data,
        _tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        let pos = w.reserved_len();
        let align_pos = size_align(pos, self.align);
        w.reserve_until(align_pos);
        self.file_offset = w.reserved_len();
        assert!(w.strtab_needed());

        w.reserve_strtab();
    }

    fn write(
        &self,
        _data: &Data,
        _tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align);
        w.pad_until(aligned_pos);
        w.write_strtab();
        assert_eq!(aligned_pos, self.file_offset);
    }

    fn write_section_header(
        &self,
        _data: &Data,
        _tracker: &SegmentTracker,
        _block: &ReadBlock,
        w: &mut Writer,
    ) {
        w.write_strtab_section_header();
    }
}

pub struct SymTabSection {
    index: Option<SectionIndex>,
    symbols: Vec<Sym>,
    count: usize,
    offsets: SectionOffset,
}
impl Default for SymTabSection {
    fn default() -> Self {
        Self {
            index: None,
            symbols: vec![],
            count: 0,
            offsets: SectionOffset::new(0x10),
        }
    }
}

impl ElfBlock for SymTabSection {
    fn name(&self) -> String {
        return "symtab".to_string();
    }

    fn reserve_section_index(&mut self, data: &mut Data, _block: &mut ReadBlock, w: &mut Writer) {
        let index = w.reserve_symtab_section_index();
        data.section_index.insert(".symtab".to_string(), index);

        if w.symtab_shndx_needed() {
            w.reserve_symtab_shndx_section_index();
        }
    }

    fn reserve_symbols(&mut self, data: &mut Data, block: &ReadBlock, w: &mut Writer) {
        {
            let name = "data_start";
            let pointer = ResolvePointer::Section(".data".to_string(), 0);
            let mut symbol = ReadSymbol::from_pointer(name.to_string(), pointer);
            symbol.bind = SymbolBind::Weak;
            let section_index = data.section_index_get(".data");
            data.statics.symbol_add(&symbol, Some(section_index), w);
        }

        {
            let name = "__data_start";
            let pointer = ResolvePointer::Section(".data".to_string(), 0);
            let mut symbol = ReadSymbol::from_pointer(name.to_string(), pointer);
            symbol.bind = SymbolBind::Global;
            let section_index = data.section_index_get(".data");
            data.statics.symbol_add(&symbol, Some(section_index), w);
        }

        {
            let name = "__bss_start";
            let pointer = ResolvePointer::Section(".bss".to_string(), 0);
            let mut symbol = ReadSymbol::from_pointer(name.to_string(), pointer);
            symbol.bind = SymbolBind::Global;
            let section_index = data.section_index_get(".bss");
            data.statics.symbol_add(&symbol, Some(section_index), w);
        }

        {
            let name = "__rodata_start";
            let pointer = ResolvePointer::Section(".rodata".to_string(), 0);
            let mut symbol = ReadSymbol::from_pointer(name.to_string(), pointer);
            symbol.bind = SymbolBind::Global;
            let section_index = data.section_index_get(".rodata");
            data.statics.symbol_add(&symbol, Some(section_index), w);
        }

        {
            let name = "_GLOBAL_OFFSET_TABLE_";
            let pointer = ResolvePointer::Section(".got.plt".to_string(), 0);
            let mut symbol = ReadSymbol::from_pointer(name.to_string(), pointer);
            symbol.bind = SymbolBind::Local;
            symbol.kind = object::SymbolKind::Data;
            let section_index = data.section_index_get(".got.plt");
            data.statics.symbol_add(&symbol, Some(section_index), w);
        }

        {
            let name = "_DYNAMIC";
            let pointer = ResolvePointer::Section(".dynamic".to_string(), 0);
            let mut symbol = ReadSymbol::from_pointer(name.to_string(), pointer);
            symbol.bind = SymbolBind::Local;
            symbol.kind = object::SymbolKind::Data;
            let section_index = data.section_index_get(".dynamic");
            data.statics.symbol_add(&symbol, Some(section_index), w);
        }

        for local in data.locals.iter() {
            let symbol = ReadSymbol::from_pointer(local.symbol.clone(), local.pointer.clone());
            let section_index = symbol.section.section_index(data);
            data.statics.symbol_add(&symbol, section_index, w);
        }

        for (_, symbol) in block.exports.iter() {
            let section_index = symbol.section.section_index(data);
            data.statics.symbol_add(symbol, section_index, w);
        }

        self.count = data.statics.symbol_count();
    }

    fn reserve(
        &mut self,
        data: &mut Data,
        _tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        let symbols = data.statics.gen_symbols(data);
        assert_eq!(symbols.len(), self.count);
        assert_eq!(symbols.len() + 1, w.symbol_count() as usize);

        // reserve the symbols in the various sections
        let pos = w.reserved_len();
        let align_pos = size_align(pos, self.offsets.align as usize);
        w.reserve_until(align_pos);
        self.offsets.file_offset = w.reserved_len() as u64;

        w.reserve_symtab();

        if w.symtab_shndx_needed() {
            w.reserve_symtab_shndx();
        }
    }

    fn write(
        &self,
        data: &Data,
        _tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.offsets.align as usize);
        w.pad_until(aligned_pos);
        assert_eq!(aligned_pos, self.offsets.file_offset as usize);
        assert_eq!(self.count + 1, w.symbol_count() as usize);

        data.statics.symbols_write(data, w);

        if w.symtab_shndx_needed() {
            w.write_symtab_shndx();
        }
    }

    fn write_section_header(
        &self,
        data: &Data,
        _tracker: &SegmentTracker,
        _block: &ReadBlock,
        w: &mut Writer,
    ) {
        let symbols = data.statics.gen_symbols(data);
        assert_eq!(symbols.len(), self.count);

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
    offsets: SectionOffset,
    count: u32,
}
impl Default for DynSymSection {
    fn default() -> Self {
        Self {
            index: None,
            offsets: SectionOffset::new(0x08),
            count: 0,
        }
    }
}

impl ElfBlock for DynSymSection {
    fn name(&self) -> String {
        return "dynsym".to_string();
    }

    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RO)
    }

    fn reserve_section_index(&mut self, data: &mut Data, _block: &mut ReadBlock, w: &mut Writer) {
        let index = w.reserve_dynsym_section_index();
        data.section_index.insert(".dynsym".to_string(), index);
    }

    fn reserve(
        &mut self,
        data: &mut Data,
        tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        self.count = w.dynamic_symbol_count();
        let pos = w.reserved_len();
        let align_pos = size_align(pos, self.offsets.align as usize);
        w.reserve_until(align_pos);

        let start = w.reserved_len();
        w.reserve_dynsym();
        let after = w.reserved_len();
        tracker.add_offsets(self.alloc().unwrap(), &mut self.offsets, after - start, w);
        data.addr_dynsym = self.offsets.address;
        data.size_dynsym = self.offsets.size as usize;
    }

    fn write(
        &self,
        data: &Data,
        _tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        assert_eq!(self.count, w.dynamic_symbol_count());
        assert_eq!(self.count as usize, data.dynamics.symbol_count() + 1);
        let pos = w.len();
        let aligned_pos = size_align(pos, self.offsets.align as usize);
        w.pad_until(aligned_pos);

        data.dynamics.symbols_write(data, w);
    }

    fn write_section_header(
        &self,
        data: &Data,
        _tracker: &SegmentTracker,
        _block: &ReadBlock,
        w: &mut Writer,
    ) {
        let got = data.dynamics.relocations(GotKind::GOT(true));
        let plt = data.dynamics.relocations(GotKind::GOTPLT);

        let len = got.len() + plt.len() + data.dynamic.len();
        w.write_dynsym_section_header(data.addr_dynsym, len as u32 + 1);
    }
}

pub struct DynStrSection {
    index: Option<SectionIndex>,
    offsets: SectionOffset,
}
impl Default for DynStrSection {
    fn default() -> Self {
        Self {
            index: None,
            offsets: SectionOffset::new(0x01),
        }
    }
}
impl ElfBlock for DynStrSection {
    fn name(&self) -> String {
        return "dynstr".to_string();
    }

    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RO)
    }

    fn reserve_section_index(&mut self, data: &mut Data, _block: &mut ReadBlock, w: &mut Writer) {
        let index = w.reserve_dynstr_section_index();
        data.section_index.insert(".dynstr".to_string(), index);
    }

    fn reserve(
        &mut self,
        data: &mut Data,
        tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        let pos = w.reserved_len();
        let align_pos = size_align(pos, self.offsets.align as usize);
        w.reserve_until(align_pos);
        let start = w.reserved_len();
        w.reserve_dynstr();
        let after = w.reserved_len();
        tracker.add_offsets(self.alloc().unwrap(), &mut self.offsets, after - start, w);
        data.addr_set(".dynstr", self.offsets.address);
        data.size_dynstr = self.offsets.size as usize;
    }

    fn write(
        &self,
        _data: &Data,
        _tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.offsets.align as usize);
        w.pad_until(aligned_pos);
        w.write_dynstr();
    }

    fn write_section_header(
        &self,
        data: &Data,
        _tracker: &SegmentTracker,
        _block: &ReadBlock,
        w: &mut Writer,
    ) {
        w.write_dynstr_section_header(data.addr_get(".dynstr"));
    }
}

#[derive(Default)]
pub struct ShStrTabSection {
    index: Option<SectionIndex>,
    file_offset: usize,
}
impl ElfBlock for ShStrTabSection {
    fn name(&self) -> String {
        return "shstrtab".to_string();
    }
    fn reserve_section_index(&mut self, _data: &mut Data, _block: &mut ReadBlock, w: &mut Writer) {
        let _shstrtab_index = w.reserve_shstrtab_section_index();
    }

    fn reserve(
        &mut self,
        _data: &mut Data,
        _tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        self.file_offset = w.reserved_len();
        w.reserve_shstrtab();
    }

    fn write(
        &self,
        _data: &Data,
        _tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        assert_eq!(w.len(), self.file_offset);
        w.write_shstrtab();
    }

    fn write_section_header(
        &self,
        _data: &Data,
        _tracker: &SegmentTracker,
        _block: &ReadBlock,
        w: &mut Writer,
    ) {
        w.write_shstrtab_section_header();
    }
}

pub struct HashSection {
    index: Option<SectionIndex>,
    bucket_count: u32,
    chain_count: u32,
    offsets: SectionOffset,
}

impl HashSection {
    pub fn new() -> Self {
        Self {
            index: None,
            bucket_count: 0,
            chain_count: 0,
            offsets: SectionOffset::new(0x08),
        }
    }
}

impl ElfBlock for HashSection {
    fn name(&self) -> String {
        return "hash".to_string();
    }
    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RO)
    }

    fn reserve_section_index(&mut self, _data: &mut Data, _block: &mut ReadBlock, w: &mut Writer) {
        self.index = Some(w.reserve_hash_section_index());
    }

    fn reserve(
        &mut self,
        data: &mut Data,
        tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        let pos = w.reserved_len();
        let aligned_pos = size_align(pos, self.offsets.align as usize);
        w.reserve_until(aligned_pos);
        let file_offset = w.reserved_len();

        w.reserve_hash(self.bucket_count, self.chain_count);

        let after = w.reserved_len();
        tracker.add_offsets(
            self.alloc().unwrap(),
            &mut self.offsets,
            after - file_offset,
            w,
        );
        data.addr_hash = self.offsets.address;
    }

    fn write(
        &self,
        _data: &Data,
        _tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.offsets.align as usize);
        w.pad_until(aligned_pos);
        w.write_hash(self.bucket_count, self.chain_count, |x| Some(x));
    }

    fn write_section_header(
        &self,
        _data: &Data,
        _tracker: &SegmentTracker,
        _block: &ReadBlock,
        w: &mut Writer,
    ) {
        w.write_hash_section_header(self.offsets.address);
    }
}

#[derive(Debug, Clone, Copy)]
pub enum GotKind {
    GOT(bool),
    GOTPLT,
}

impl GotKind {
    pub fn section_name(&self) -> &'static str {
        match self {
            Self::GOT(_) => ".got",
            Self::GOTPLT => ".got.plt",
        }
    }

    pub fn rel_section_name(&self) -> &'static str {
        match self {
            Self::GOT(_) => ".rela.dyn",
            Self::GOTPLT => ".rela.plt",
        }
    }

    pub fn start_index(&self) -> usize {
        match self {
            Self::GOT(_) => 0,
            Self::GOTPLT => 3,
        }
    }
}

pub struct GotSection {
    kind: GotKind,
    name_id: Option<StringId>,
    index: Option<SectionIndex>,
    bytes: Vec<u8>,
    offsets: SectionOffset,
}
impl GotSection {
    pub fn new(kind: GotKind) -> Self {
        Self {
            kind,
            name_id: None,
            index: None,
            bytes: vec![],
            offsets: SectionOffset::new(0x08),
        }
    }
}

impl ElfBlock for GotSection {
    fn name(&self) -> String {
        return format!("gotsection-{:?}", self.kind);
    }
    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RW)
    }

    fn reserve_section_index(&mut self, data: &mut Data, _block: &mut ReadBlock, w: &mut Writer) {
        let name = self.kind.section_name();
        self.name_id = Some(w.add_section_name(name.as_bytes()));
        let index = w.reserve_section_index();
        data.section_index.insert(name.to_string(), index);
        self.index = Some(index);
    }

    fn reserve(
        &mut self,
        data: &mut Data,
        tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        // each entry in unapplied will be a GOT entry
        let unapplied = data.dynamics.relocations(self.kind);
        let name = self.kind.section_name();

        let len = unapplied.len() + self.kind.start_index();
        let size = len * std::mem::size_of::<usize>();
        self.bytes.resize(size, 0);

        let align = self.offsets.align as usize;

        let pos = w.reserved_len();
        let align_pos = size_align(pos, align);
        w.reserve_until(align_pos);
        let file_offset = w.reserved_len();
        w.reserve(self.bytes.len(), align);
        let after = w.reserved_len();
        tracker.add_offsets(
            self.alloc().unwrap(),
            &mut self.offsets,
            after - file_offset,
            w,
        );

        /*
        // add got pointers to the pointers table, so we can do relocations
        for (index, (relative, name, pointer)) in unapplied.iter().enumerate() {
            let addr = self.offsets.address as usize
                + (index + self.kind.start_index()) * std::mem::size_of::<usize>();
            //eprintln!("adding: {}, {:#0x}", &name, addr as u64);
            match self.kind {
                GotKind::GOT(_) => {
                    data.pointers
                        .insert(name.clone(), ResolvePointer::Got(index));
                }
                GotKind::GOTPLT => {
                    data.pointers
                        .insert(name.clone(), ResolvePointer::GotPlt(index));
                }
            }
        }
        */

        // update section pointers
        data.addr_set(name, self.offsets.address);
    }

    fn write(
        &self,
        data: &Data,
        _tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        let align = self.offsets.align as usize;
        let pos = w.len();
        let aligned_pos = size_align(pos, align);
        w.pad_until(aligned_pos);

        let unapplied = data.dynamics.relocations(self.kind);
        eprintln!("{}: {:?}", self.name(), unapplied);

        match self.kind {
            GotKind::GOT(_) => {
                // just empty
                let mut bytes: Vec<u8> = vec![];
                bytes.resize(self.offsets.size as usize, 0);
                w.write(bytes.as_slice());
            }
            GotKind::GOTPLT => {
                // populate with predefined values
                let mut values: Vec<u64> = vec![data.addr_get(".dynamic"), 0, 0];
                let len = unapplied.len();
                let plt_addr = data.addr_get(".plt") + 0x16;
                for i in 0..len {
                    values.push(plt_addr + i as u64 * 0x10);
                }
                let mut bytes: Vec<u8> = vec![];
                for v in values {
                    bytes.extend(v.to_le_bytes().as_slice());
                }
                w.write(bytes.as_slice());
            }
        }
    }

    fn write_section_header(
        &self,
        _data: &Data,
        _tracker: &SegmentTracker,
        _block: &ReadBlock,
        w: &mut Writer,
    ) {
        let sh_flags = self.alloc().unwrap().section_header_flags() as u64;
        let sh_addralign = self.offsets.align;
        w.write_section_header(&object::write::elf::SectionHeader {
            name: self.name_id,
            sh_type: elf::SHT_PROGBITS,
            sh_flags,
            sh_addr: self.offsets.address,
            sh_offset: self.offsets.file_offset,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign,
            sh_size: self.offsets.size,
        });
    }
}

pub struct PltSection {
    name_id: Option<StringId>,
    index: Option<SectionIndex>,
    bytes: Vec<u8>,
    offsets: SectionOffset,
}

impl PltSection {
    pub fn new() -> Self {
        Self {
            name_id: None,
            index: None,
            bytes: vec![],
            offsets: SectionOffset::new(0x10),
        }
    }
}

impl PltSection {
    pub fn get_name() -> &'static str {
        ".plt"
    }
}

impl ElfBlock for PltSection {
    fn name(&self) -> String {
        return "plt".to_string();
    }
    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RX)
    }

    fn reserve_section_index(&mut self, data: &mut Data, _block: &mut ReadBlock, w: &mut Writer) {
        let name = Self::get_name();
        self.name_id = Some(w.add_section_name(name.as_bytes()));
        let index = w.reserve_section_index();
        data.section_index.insert(name.to_string(), index);
        self.index = Some(index);
    }

    fn reserve(
        &mut self,
        data: &mut Data,
        tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        //let unapplied = GotKind::GOTPLT.unapplied_data(data);
        //let unapplied = data.dynamics.relocations(GotKind::GOTPLT);
        let plt = data.dynamics.plt_objects();

        // length + 1, to account for the stub.  Each entry is 0x10 in size
        let size = (1 + plt.len()) * 0x10;
        self.bytes.resize(size, 0);
        let align = self.offsets.align as usize;

        let pos = w.reserved_len();
        let align_pos = size_align(pos, align);
        w.reserve_until(align_pos);
        let file_offset = w.reserved_len();
        w.reserve(self.bytes.len(), align);
        let after = w.reserved_len();
        assert_eq!(size, after - file_offset);
        tracker.add_offsets(
            self.alloc().unwrap(),
            &mut self.offsets,
            after - file_offset,
            w,
        );

        // update section pointers
        data.addr.insert(
            AddressKey::Section(Self::get_name().to_string()),
            self.offsets.address,
        );
        data.addr.insert(
            AddressKey::SectionIndex(self.index.unwrap()),
            self.offsets.address,
        );
    }

    fn write(
        &self,
        data: &Data,
        _tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        let align = self.offsets.align as usize;
        let pos = w.len();
        let aligned_pos = size_align(pos, align);
        w.pad_until(aligned_pos);

        let got_addr = data.addr_get_by_name(".got.plt").unwrap() as isize;
        let vbase = self.offsets.address as isize;

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

        //let unapplied = GotKind::GOTPLT.unapplied_data(data);
        //let unapplied = data.dynamics.relocations(GotKind::GOTPLT);
        let plt = data.dynamics.plt_objects();
        eprintln!("plt: {:?}", plt);

        for (i, _) in plt.iter().enumerate() {
            let slot: Vec<u8> = vec![
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
                let addr = self.offsets.address as isize - rip;
                //eprintln!("got: {}, {:#0x}, {:#0x}", i, rip, addr);
                let patch = (stub.as_mut_ptr().offset(offset + 0x0c)) as *mut i32;
                *patch = addr as i32;
            }
        }

        // write stub
        w.write(stub.as_slice());
    }

    fn write_section_header(
        &self,
        _data: &Data,
        _tracker: &SegmentTracker,
        _block: &ReadBlock,
        w: &mut Writer,
    ) {
        let sh_flags = self.alloc().unwrap().section_header_flags() as u64;
        let sh_addralign = self.offsets.align;
        w.write_section_header(&object::write::elf::SectionHeader {
            name: self.name_id,
            sh_type: elf::SHT_PROGBITS,
            sh_flags,
            sh_addr: self.offsets.address,
            sh_offset: self.offsets.file_offset,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign,
            sh_size: self.offsets.size,
        });
    }
}

pub struct PltGotSection {
    name_id: Option<StringId>,
    index: Option<SectionIndex>,
    offsets: SectionOffset,
    entry_size: usize,
}

impl PltGotSection {
    pub fn new() -> Self {
        Self {
            name_id: None,
            index: None,
            offsets: SectionOffset::new(0x10),
            entry_size: 0x08,
        }
    }
}

impl PltGotSection {
    pub fn get_name() -> &'static str {
        ".plt.got"
    }
}

impl ElfBlock for PltGotSection {
    fn name(&self) -> String {
        return "plt.got".to_string();
    }
    fn alloc(&self) -> Option<AllocSegment> {
        Some(AllocSegment::RX)
    }

    fn reserve_section_index(&mut self, data: &mut Data, _block: &mut ReadBlock, w: &mut Writer) {
        let name = Self::get_name();
        self.name_id = Some(w.add_section_name(name.as_bytes()));
        let index = w.reserve_section_index();
        data.section_index.insert(name.to_string(), index);
        self.index = Some(index);
    }

    fn reserve(
        &mut self,
        data: &mut Data,
        tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        let pltgot = data.dynamics.pltgot_objects();

        let size = (pltgot.len()) * self.entry_size;
        let align = self.offsets.align as usize;

        let pos = w.reserved_len();
        let align_pos = size_align(pos, align);
        w.reserve_until(align_pos);
        let file_offset = w.reserved_len();
        w.reserve(size, align);
        let after = w.reserved_len();
        assert_eq!(size, after - file_offset);
        tracker.add_offsets(
            self.alloc().unwrap(),
            &mut self.offsets,
            after - file_offset,
            w,
        );

        // update section pointers
        data.addr.insert(
            AddressKey::Section(Self::get_name().to_string()),
            self.offsets.address,
        );
        data.addr.insert(
            AddressKey::SectionIndex(self.index.unwrap()),
            self.offsets.address,
        );

        //let pltgot = data.dynamics.plt_objects();
        //for (name, p, p2) in pltgot.into_iter() {
        //data.pointers_plt.insert(name.to_string(), p2.clone());
        //eprintln!("p: {:?}", (name, p2));
        //}
    }

    fn write(
        &self,
        data: &Data,
        _tracker: &mut SegmentTracker,
        _block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        let align = self.offsets.align as usize;
        let pos = w.len();
        let aligned_pos = size_align(pos, align);
        w.pad_until(aligned_pos);

        let vbase = self.offsets.address as isize;

        let pltgot = data.dynamics.pltgot_objects();
        eprintln!("pltgot: {:?}", pltgot);

        for (i, (name, p)) in pltgot.iter().enumerate() {
            let mut slot: Vec<u8> = vec![0xff, 0x25, 0x00, 0x00, 0x00, 0x00, 0x66, 0x90];
            let slot_size = slot.len();
            assert_eq!(slot_size, self.entry_size);

            //1050:       ff 25 82 2f 00 00       jmp    *0x2f82(%rip)        # 3fd8 <fprintf@GLIBC_2.2.5>
            //1056:       66 90                   xchg   %ax,%ax

            let gotplt_addr = p.resolve(data).unwrap();
            unsafe {
                let offset = (i as isize) * slot_size as isize;
                let patch = (slot.as_mut_ptr().offset(offset + 2)) as *mut i32;
                let rip = vbase + offset + 6;
                let addr = gotplt_addr as isize - rip;
                *patch = addr as i32;
            }
            w.write(slot.as_slice());
        }
    }

    fn write_section_header(
        &self,
        _data: &Data,
        _tracker: &SegmentTracker,
        _block: &ReadBlock,
        w: &mut Writer,
    ) {
        let sh_flags = self.alloc().unwrap().section_header_flags() as u64;
        let sh_addralign = self.offsets.align;
        w.write_section_header(&object::write::elf::SectionHeader {
            name: self.name_id,
            sh_type: elf::SHT_PROGBITS,
            sh_flags,
            sh_addr: self.offsets.address,
            sh_offset: self.offsets.file_offset,
            sh_info: 0,
            sh_link: 0,
            sh_entsize: 0,
            sh_addralign,
            sh_size: self.offsets.size,
        });
    }
}

pub struct BlockSectionX {
    pub kind: ReadSectionKind,
}
impl BlockSectionX {
    pub fn new(kind: ReadSectionKind) -> Self {
        Self { kind }
    }
}
impl ElfBlock for BlockSectionX {
    fn name(&self) -> String {
        return format!("blockx:{:?}", self.kind);
    }

    fn reserve_section_index(&mut self, data: &mut Data, block: &mut ReadBlock, w: &mut Writer) {
        match self.kind {
            ReadSectionKind::RX => block.rx.block_reserve_section_index(data, w),
            ReadSectionKind::ROData => block.ro.block_reserve_section_index(data, w),
            ReadSectionKind::RW => block.rw.block_reserve_section_index(data, w),
            ReadSectionKind::Bss => block.bss.block_reserve_section_index(data, w),
            _ => unreachable!(),
        }
    }

    fn reserve(
        &mut self,
        data: &mut Data,
        tracker: &mut SegmentTracker,
        block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        match self.kind {
            ReadSectionKind::RX => block.rx.block_reserve(data, tracker, w),
            ReadSectionKind::ROData => block.ro.block_reserve(data, tracker, w),
            ReadSectionKind::RW => block.rw.block_reserve(data, tracker, w),
            ReadSectionKind::Bss => block.bss.block_reserve(data, tracker, w),
            _ => unreachable!(),
        }
    }

    fn write(&self, data: &Data, _: &mut SegmentTracker, block: &mut ReadBlock, w: &mut Writer) {
        match self.kind {
            ReadSectionKind::RX => block.rx.block_write(data, w),
            ReadSectionKind::ROData => block.ro.block_write(data, w),
            ReadSectionKind::RW => block.rw.block_write(data, w),
            ReadSectionKind::Bss => block.bss.block_write(data, w),
            _ => unreachable!(),
        }
    }

    fn write_section_header(
        &self,
        data: &Data,
        _: &SegmentTracker,
        block: &ReadBlock,
        w: &mut Writer,
    ) {
        match self.kind {
            ReadSectionKind::RX => block.rx.block_write_section_header(data, w),
            ReadSectionKind::ROData => block.ro.block_write_section_header(data, w),
            ReadSectionKind::RW => block.rw.block_write_section_header(data, w),
            ReadSectionKind::Bss => block.bss.block_write_section_header(data, w),
            _ => unreachable!(),
        }
    }
}

pub struct LocalSymbol {
    pub(crate) section: String,
    pub(crate) symbol: String,
    pub(crate) pointer: ResolvePointer,
    pub(crate) string_id: Option<StringId>,
    pub(crate) dyn_string_id: Option<StringId>,
}

impl LocalSymbol {
    pub fn new(
        symbol: String,
        section: String,
        pointer: ResolvePointer,
        string_id: Option<StringId>,
        dyn_string_id: Option<StringId>,
    ) -> Self {
        Self {
            section,
            symbol,
            pointer,
            string_id,
            dyn_string_id,
        }
    }
}
