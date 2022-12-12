use super::*;
use object::write::elf::Sym;

pub struct Blocks {
    pub blocks: Vec<Box<dyn ElfBlock>>,
}

impl Blocks {
    pub fn new() -> Self {
        Self { blocks: vec![] }
    }

    pub fn add_block(&mut self, block: Box<dyn ElfBlock>) {
        self.blocks.push(block);
    }

    pub fn reserve(&mut self, tracker: &mut SegmentTracker, data: &mut Data, w: &mut Writer) {
        // build a list of sections that are loaded
        // this is a hack to get tracker to build a correct list of program headers
        // without having to go through the blocks and do reservations
        let mut temp_tracker = SegmentTracker::new(2);
        for b in self.blocks.iter() {
            if let Some(alloc) = b.alloc() {
                temp_tracker.add_data(alloc, 1, 0);
            }
        }
        // get a list of program headers
        // we really only need to know the number of headers, so we can correctly
        // set the values in the file header

        self.generate_program_headers(&mut temp_tracker);
        // hack
        tracker.ph = temp_tracker.ph.clone();

        for p in temp_tracker.ph.iter() {
            eprintln!("P: {:?}", p);
        }

        for b in self.blocks.iter_mut() {
            b.reserve(data, tracker, w);
        }
    }

    pub fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        for b in self.blocks.iter_mut() {
            b.reserve_section_index(data, w);
        }
    }

    pub fn update(&mut self, data: &mut Data) {
        for b in self.blocks.iter_mut() {
            b.update(data);
        }
    }

    pub fn write(&self, data: &Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        // generate the program headers, so they have up to date fields
        //self.generate_program_headers(tracker);
        //for p in tracker.ph.iter() {
        //eprintln!("P: {:?}", p);
        //}
        for b in self.blocks.iter() {
            b.write(&data, tracker, w);
        }
    }

    pub fn write_section_headers(&self, data: &Data, tracker: &SegmentTracker, w: &mut Writer) {
        for b in self.blocks.iter() {
            b.write_section_header(&data, &tracker, w);
        }
    }

    pub fn program_headers(&self, tracker: &SegmentTracker) -> Vec<ProgramHeaderEntry> {
        let mut ph = vec![];
        for b in self.blocks.iter() {
            ph.extend(b.program_header());
        }
        ph.extend(tracker.program_headers());
        ph
    }

    pub fn generate_program_headers(&self, tracker: &mut SegmentTracker) {
        let ph = self.program_headers(tracker);
        tracker.ph = ph;
    }
}

pub struct SegmentTracker {
    segments: Vec<Segment>,
    base: usize,
    page_size: usize,
    pub ph: Vec<ProgramHeaderEntry>,
    pub addr_start: u64,
    pub symbols: Vec<object::write::elf::Sym>,
}

impl SegmentTracker {
    pub fn new(base: usize) -> Self {
        Self {
            segments: vec![],
            base,
            page_size: 0x1000,
            addr_start: 0,
            ph: vec![],
            symbols: vec![],
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

        for s in self.symbols.iter() {
            //let name_id = Some(w.add_string("_DYNAMIC_".as_bytes()));
            w.reserve_symbol_index(s.section);
            //w.reserve_symbol_index(s.section);
        }
    }

    pub fn get_symbols(&self) -> Vec<Sym> {
        self.segments
            .iter()
            .map(|seg| seg.get_symbols())
            .flatten()
            .chain(self.symbols.iter().cloned())
            .collect()
    }

    pub fn write_symbols(&self, w: &mut Writer) -> usize {
        let symbols = self.get_symbols();
        // sort them, locals first

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
        num_locals
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

    pub fn update(&mut self, _data: &mut Data, blocks: &mut Blocks) {
        let pointers = self.symbol_pointers();
        if let Some(start) = pointers.get("_start") {
            self.addr_start = *start;
        }
        self.ph = blocks.program_headers(self);
        for p in self.ph.iter() {
            eprintln!("P: {:?}", p);
        }

        self.apply_relocations();
    }

    fn apply_relocations(&self) {
        for seg in &self.segments {
            seg.apply_relocations();
        }
    }

    pub fn reserve_relocations(&mut self, w: &mut Writer) {
        for seg in self.segments.iter_mut() {
            for section in seg.sections.iter_mut() {
                section.reserve_relocations(w);
            }
        }
    }

    pub fn write_relocations(&self, w: &mut Writer) {
        for seg in self.segments.iter() {
            for section in seg.sections.iter() {
                section.write_relocations(w);
            }
        }
    }

    pub fn write_relocation_section_headers(&self, w: &mut Writer, index_symtab: SectionIndex) {
        for seg in &self.segments {
            for section in &seg.sections {
                section.write_relocation_section_headers(w, index_symtab);
            }
        }
    }
}

pub struct Segment {
    pub base: u64,
    pub addr: u64,
    pub offset: u64,
    segment_size: usize,
    pub align: u32,
    pub alloc: AllocSegment,
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
                section.name_id,
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
        for s in self.sections.iter() {
            s.disassemble_code();
        }
    }

    pub fn reserve_symbols(&self, w: &mut Writer) {
        for s in &self.sections {
            s.reserve_symbols(w);
        }
    }

    pub fn get_symbols(&self) -> Vec<Sym> {
        self.sections
            .iter()
            .map(|s| s.get_symbols(self.base + self.offset))
            .flatten()
            .collect()
    }

    pub fn write_symbols(&self, w: &mut Writer) {
        for s in self.sections.iter() {
            s.write_symbols(self.base + self.offset, w);
        }
    }

    pub fn apply_relocations(&self) {
        let pointers = self.symbol_pointers();
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
