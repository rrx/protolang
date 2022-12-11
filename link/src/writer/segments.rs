use super::*;
pub struct SegmentTracker {
    segments: Vec<Segment>,
    base: usize,
    page_size: usize,
    pointers: HashMap<String, u64>,
    //blocks: Vec<Box<dyn ElfBlock>>
    pub addr_start: u64,
}

impl SegmentTracker {
    pub fn new(base: usize) -> Self {
        Self {
            segments: vec![],
            base,
            page_size: 0x1000,
            pointers: HashMap::new(),
            addr_start: 0,
        }
    }

    pub fn current(&self) -> &Segment {
        self.segments.last().unwrap()
    }

    pub fn current_mut(&mut self) -> &mut Segment {
        self.segments.last_mut().unwrap()
    }

    /*
    pub fn add_unlinked(&mut self, alloc: AllocSegment, unlinked: UnlinkedCodeSegment, file_offset: usize) -> usize {
        let size = unlinked.bytes.len();
        let base = self.add_data(alloc, size, file_offset);
        //self.current().add_section(section)
        base
    }
    */

    pub fn add_section(
        &mut self,
        alloc: AllocSegment,
        section: ProgSection,
        file_offset: usize,
    ) -> usize {
        let size = section.size();
        let base = self.add_data(alloc, size, file_offset);
        self.current_mut().add_section(section);
        base
    }

    // add non-section data
    pub fn add_data(&mut self, alloc: AllocSegment, size: usize, file_offset: usize) -> usize {
        let mut current_size = 0;
        let mut current_alloc = alloc;
        if let Some(c) = self.segments.last() {
            current_size = c.size();
            current_alloc = c.alloc;
        }

        if self.segments.len() == 0 || alloc != current_alloc {
            self.base = size_align(self.base + current_size, self.page_size);
            let mut segment = Segment::new(alloc);
            segment.base = self.base as u64;
            segment.offset = file_offset as u64;
            self.segments.push(segment);
            eprintln!("new add: base: {:?}, {:#0x}", alloc, self.base);
        }
        self.current_mut().add_data(size, alloc.align());
        self.base
    }

    pub fn symbol_pointers(&self) -> HashMap<String, u64> {
        let mut pointers = HashMap::new();
        for seg in &self.segments {
            for (name, addr) in seg.symbol_pointers() {
                pointers.insert(name.clone(), addr);
            }
        }
        pointers
    }

    pub fn reserve_symbols(&self, w: &mut Writer) {
        for s in self.segments.iter() {
            s.reserve_symbols(w);
        }
    }

    pub fn write_symbols(&self, w: &mut Writer) {
        for s in self.segments.iter() {
            s.write_symbols(w);
        }
    }

    pub fn load_segment_count(&self) -> usize {
        self.program_headers().len()
    }

    pub fn program_headers(&self) -> Vec<ProgramHeaderEntry> {
        let mut out = vec![];
        for s in self.segments.iter() {
            if let Some(ph) = s.program_header() {
                out.push(ph);
            }
        }
        out
    }

    pub fn update(&mut self) {
        let pointers = self.symbol_pointers();
        if let Some(start) = pointers.get("_start") {
            self.addr_start = *start;
        }
    }

    pub fn apply_relocations(&self) {
        for seg in &self.segments {
            seg.apply_relocations();
        }
    }

    pub fn reserve_relocations(&mut self, w: &mut Writer) {
        for seg in self.segments.iter_mut() {
            //seg.section.reserve_relocations(w);
            //let offset = w.reserve_relocations(seg.section.relocations.len(), true);
            for section in seg.sections.iter_mut() {
                section.reserve_relocations(w);
                //let offset = w.reserve_relocations(section.relocations.len(), true);
            }
        }
    }

    pub fn write_relocations(&self, w: &mut Writer) {
        for seg in self.segments.iter() {
            //seg.section.write_relocations(w);
            for section in seg.sections.iter() {
                section.write_relocations(w);
            }
        }
    }

    pub fn write_relocation_section_headers(&self, w: &mut Writer, index_symtab: SectionIndex) {
        for seg in &self.segments {
            //seg.section.write_relocation_section_headers(w, index_symtab);
            for section in &seg.sections {
                section.write_relocation_section_headers(w, index_symtab);
            }
        }
    }

    //pub fn add_block(&mut self, block: Box<dyn ElfBlock>) {
    //self.blocks.push(block);
    //}
}

pub fn update_blocks(tracker: &mut SegmentTracker, blocks: &mut Vec<Box<dyn ElfBlock>>) {
    for block in blocks {}
}

pub struct Segments {
    pub ro: Segment,
    pub rx: Segment,
    pub rw: Segment,
    //pub addr_start: u64,
    file_offset: u64,
}
impl Default for Segments {
    fn default() -> Self {
        Self {
            ro: Segment::new(AllocSegment::RO),
            rw: Segment::new(AllocSegment::RW),
            rx: Segment::new(AllocSegment::RX),
            //addr_start: 0,
            file_offset: 0,
        }
    }
}

impl Segments {
    /*
    fn add_file_offset(&mut self, offset: u64) {
        self.file_offset += offset;
    }

    pub fn file_offset(&self) -> u64 {
        self.file_offset
    }

    pub fn symbol_pointers2(&self) -> HashMap<String, u64> {
        let mut pointers = HashMap::new();
        for (name, s) in &self.rx.section.symbols {
            let addr = self.rx.base + s.s.address;
            pointers.insert(name.clone(), addr);
        }
        for (name, s) in &self.rw.section.symbols {
            let addr = self.rw.base + s.s.address;
            pointers.insert(name.clone(), addr);
        }
        pointers
    }
    */

    /*
    pub fn reserve_symbols(&self, w: &mut Writer) {
        self.ro.reserve_symbols(w);
        self.rw.reserve_symbols(w);
        self.rx.reserve_symbols(w);
    }

    pub fn write_symbols(&self, w: &mut Writer) {
        self.ro.write_symbols(w);
        self.rx.write_symbols(w);
        self.rw.write_symbols(w);
    }

    pub fn load2<'a>(&mut self, link: &'a Link, w: &mut Writer<'a>) {
        for (_name, unlinked) in link.unlinked.iter() {
            use object::SectionKind as K;
            match unlinked.kind {
                K::Data | K::UninitializedData => {
                    self.rw.section.append(unlinked, w);
                }

                // OtherString is usually comments, we can drop these
                K::OtherString => (),
                K::ReadOnlyString | K::ReadOnlyData => {
                    eprintln!("X:{:?}", (&unlinked.name, &unlinked.kind));
                    self.ro.section.append(unlinked, w);
                    // XXX: this can mess things up
                    // it adds things to RO before the text begins and gets confused
                }
                K::Text => {
                    self.rx.section.append(unlinked, w);
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

    pub fn apply_relocations(&self) {
        self.ro.apply_relocations();
        self.rw.apply_relocations();
        self.rx.apply_relocations();
    }

    /// update the segments
    pub fn update(&mut self, base: usize, page_size: usize) {
        /*
        let align = AllocSegment::RO.align();
        let mut offset = 0;
        let ro_size_elf_aligned = size_align(self.ro.size() as usize, align);
        //self.ro.align = align as u32;
        log::debug!(
            "RO base:{:#0x}, offset:{:#0x}, size:{:#0x}, fo:{:#0x}",
            base,
            offset,
            self.ro.size(),
            self.file_offset()
        );
        offset += ro_size_elf_aligned;
        self.add_file_offset(self.ro.size() as u64);

        let align = AllocSegment::RX.align();
        let base = size_align(base as usize + offset, page_size) as u64;
        let rx_size_elf_aligned = size_align(self.rx.size() as usize, align);
        //self.rx.base = base;
        //self.rx.addr = base + offset as u64;
        //self.rx.offset = offset as u64;
        //self.rx.align = align as u32;
        log::debug!(
            "RX base:{:#0x}, offset:{:#0x}, size:{:#0x} {}, fo:{:#0x}",
            base,
            offset,
            self.rx.size(),
            rx_size_elf_aligned,
            self.file_offset()
        );
        offset += rx_size_elf_aligned;
        self.add_file_offset(self.rx.size() as u64);

        let align = AllocSegment::RW.align();
        let base = size_align(base as usize + rx_size_elf_aligned, page_size) as u64;
        let _rw_size_elf_aligned = size_align(self.rw.size() as usize, align);
        //self.rw.base = base;
        //self.rw.addr = base + offset as u64;
        //self.rw.offset = offset as u64;
        //self.rw.align = align as u32;
        log::debug!(
            "RW base:{:#0x}, offset:{:#0x}, size:{:#0x}, fo:{:#0x}",
            base,
            offset,
            self.rw.size(),
            self.file_offset()
        );
        //self.add_file_offset(self.rw.size() as u64);
        */

        // set entry point
        /*
        let pointers = self.symbol_pointers();
        for (name, p) in &pointers {
            eprintln!("P: {}, {:#0x}", name, p);
        }

        if let Some(start) = pointers.get("_start") {
            self.addr_start = *start;
        }
        */
        //for (_name, sym) in &self.rx.section.symbols {
            //if sym.is_start {
                //self.addr_start = self.rx.addr + sym.s.address;
            //}
        //}
    }
    */
}

pub struct Segment {
    pub base: u64,
    pub addr: u64,
    pub offset: u64,
    segment_size: usize,
    pub align: u32,
    pub alloc: AllocSegment,
    //pub section: ProgSection,
    pub sections: Vec<ProgSection>,
}

impl Segment {
    pub fn new(alloc: AllocSegment) -> Self {
        Self {
            base: 0,
            addr: 0,
            offset: 0,
            segment_size: 0,
            alloc,
            align: 0x1000,
            //section: ProgSection::new(alloc, None, 0),
            sections: vec![],
        }
    }

    pub fn add_section(&mut self, section: ProgSection) {
        let start = size_align(self.segment_size, section.alloc().unwrap().align());
        self.segment_size = start + section.size();
        self.sections.push(section);
    }

    pub fn size(&self) -> usize {
        self.segment_size
    }

    pub fn add_data(&mut self, size: usize, align: usize) {
        // set size to match the offset size
        let before = self.segment_size;
        let delta = size_align(size, align);
        eprintln!("x/{:#0x}/{:#0x}", size, delta);
        self.segment_size += delta;
        eprintln!(
            "add_data/{:?}/{:#0x}, {:#0x}+{:#0x}={:#0x}/{:#0x}",
            self.alloc, size, before, delta, self.segment_size, align
        );
    }

    pub fn debug(&self) {
        eprintln!(
            "Segment: {:?} base:{:#0x}, offset:{:#0x}, size:{:#0x} ({})",
            self.alloc,
            self.base,
            self.offset,
            self.size(),
            self.size(),
        );
        for section in &self.sections {
            eprintln!(
                "Section: {:?}, size:{:#0x} ({})",
                section.name,
                section.bytes.len(),
                section.bytes.len()
            );
            for (name, s) in &section.symbols {
                eprintln!(" S:{} {:?}", name, s);
            }
        }

        //self.disassemble_code();
    }

    pub fn disassemble_code(&self) {
        //self.section.disassemble_code();
        for s in self.sections.iter() {
            s.disassemble_code();
        }
    }

    pub fn reserve_symbols(&self, w: &mut Writer) {
        //self.section.reserve_symbols(w);
        for s in &self.sections {
            s.reserve_symbols(w);
        }
    }

    pub fn write_symbols(&self, w: &mut Writer) {
        //self.section.write_symbols(self.base, w);
        for s in self.sections.iter() {
            s.write_symbols(self.base + self.offset, w);
        }
    }

    pub fn apply_relocations(&self) {
        let pointers = self.symbol_pointers();
        //self.section.apply_relocations(self.base as usize, &pointers);
        for section in &self.sections {
            section.apply_relocations(self.base as usize, &pointers);
        }
    }

    pub fn symbol_pointers(&self) -> HashMap<String, u64> {
        let mut pointers = HashMap::new();
        for section in &self.sections {
            for (name, s) in &section.symbols {
                let addr = self.base + self.offset + s.s.address;
                pointers.insert(name.clone(), addr);
            }
        }
        pointers
    }

    pub fn program_header(&self) -> Option<ProgramHeaderEntry> {
        // add a load section for the file and program header, so it's covered
        Some(ProgramHeaderEntry {
            p_type: elf::PT_LOAD,
            p_flags: self.alloc.program_header_flags(),
            p_offset: self.offset,
            p_vaddr: self.base + self.offset,
            p_paddr: 0,
            p_filesz: self.size() as u64,
            p_memsz: self.size() as u64,
            p_align: self.align as u64,
        })
    }
}
