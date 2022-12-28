use super::*;

pub struct BlocksBuilder {}

impl BlocksBuilder {
    pub fn new() -> Self {
        Self {}
    }

    /*
    fn load_buffer_sections<'a>(&mut self, data: &mut Data) -> Vec<Box<dyn ElfBlock>> {
        let mut blocks: Vec<Box<dyn ElfBlock>> = vec![];
        for section in data.sections.sections.drain(..) {
            let buf = section.bytes.clone();
            if section.relocations.len() > 0 {
                //blocks.add_block(Box::new(RelocationSection::new(section.kind, &section)));
            }

            let block = BufferSection::new(
                AllocSegment::RX,
                section.name.clone(),
                section.name_id,
                buf,
                section,
            );
            blocks.push(Box::new(block));
        }
        blocks
    }
    */

    pub fn build(mut self, data: &mut Data, w: &mut Writer, block: &mut ReadBlock) -> Blocks {
        //, Option<ReadBlock>) {
        // hack to get generate_ph to work
        //let x = Some(w.add_string("_DYNAMIC".as_bytes()));
        //let x = Some(w.add_dynamic_string("_DYNAMIC".as_bytes()));

        //self.reserve_strings(data, w, block);
        block.reserve_strings(data, w);

        let mut blocks: Vec<Box<dyn ElfBlock>> = vec![];
        blocks.push(Box::new(HeaderComponent::default()));
        if data.is_dynamic() {
            // BufferSection doesn't implement the program header, we really need
            // the dedicated interp section code to make that work
            // interp is an exception
            blocks.push(Box::new(InterpSection::new(&data)));
        }

        blocks.push(Box::new(HashSection::default()));

        if w.dynstr_needed() {
            blocks.push(Box::new(DynStrSection::default()));
        }

        if data.is_dynamic() {
            blocks.push(Box::new(RelaDynSection::new(GotKind::GOT)));
            blocks.push(Box::new(RelaDynSection::new(GotKind::GOTPLT)));
            blocks.push(Box::new(DynSymSection::default()));
        }

        //let maybe_block = data.block.take();
        //if let Some(block) = maybe_block.as_ref() {
        //blocks.push(Box::new(block.ro.clone()));
        //blocks.push(Box::new(block.rx.clone()));
        //blocks.push(Box::new(block.rw.clone()));
        blocks.push(ReadSectionKind::RO.block());
        blocks.push(ReadSectionKind::RX.block());
        blocks.push(Box::new(PltSection::new()));
        blocks.push(ReadSectionKind::RW.block());
        //data.block = Some(block);
        //} else {
        /*
        blocks.push(Box::new(PltSection::new()));
        blocks.extend(self.load_buffer_sections(data));
        //load_buffer_sections(&mut blocks, &mut data);
        */
        //}

        if data.is_dynamic() {
            blocks.push(Box::new(DynamicSection::default()));
            //blocks.add_block(Box::new(GotPltSection::default()));
            //blocks.push(Box::new(block.got.clone()));
            blocks.push(Box::new(GotSection::new(GotKind::GOT)));
            blocks.push(Box::new(GotSection::new(GotKind::GOTPLT)));
            //blocks.push(Box::new(block.gotplt.clone()));
        }

        // bss is the last alloc block
        //if let Some(block) = maybe_block.as_ref() {
        //blocks.push(Box::new(block.bss.clone()));
        //}
        blocks.push(ReadSectionKind::Bss.block());

        if data.add_symbols {
            blocks.push(Box::new(SymTabSection::default()));
        }

        assert!(w.strtab_needed());
        if data.add_symbols && w.strtab_needed() {
            blocks.push(Box::new(StrTabSection::new()));
        }

        // shstrtab needs to be allocated last, once all headers are reserved
        if data.add_symbols {
            blocks.push(Box::new(ShStrTabSection::default()));
        }

        Blocks { blocks } //, maybe_block)
    }
}

pub struct Blocks {
    pub blocks: Vec<Box<dyn ElfBlock>>,
}

impl Blocks {
    /*
        pub fn reserve_strings(&mut self, data: &mut Data, w: &mut Writer, block: &mut ReadBlock) {
            // Reserve Strings
            //if let Some(block) = maybe_block {
                block.reserve_strings(data, w);
            //} else {
                /*
                for (name, p) in data.sections.symbol_pointers().iter() {
                    data.lookup.insert(name.clone(), p.clone());
                }
                for (name, p) in data.sections.extern_symbol_pointers().iter() {
                    data.lookup.insert(name.clone(), p.clone());
                }
                */
            //}
        }
    */

    pub fn build(&mut self, data: &mut Data, w: &mut Writer, block: &mut ReadBlock) {
        let mut tracker = SegmentTracker::new(data.base);
        tracker.ph = self.generate_ph(block);

        // RESERVE SECTION HEADERS
        // section headers are optional
        if data.add_section_headers {
            self.reserve_section_index(data, block, w);
        }

        // RESERVE SYMBOLS
        //self.reserve_symbols(data, w);
        // what are these for? reserving symbols for locals
        // set up sections
        data.locals = vec![
            LocalSymbol::new(
                "_DYNAMIC".into(),
                ".dynamic".into(),
                0,
                Some(w.add_string("_DYNAMIC".as_bytes())),
                None, //Some(w.add_dynamic_string("_DYNAMIC".as_bytes())),
            ),
            LocalSymbol::new(
                "_GLOBAL_OFFSET_TABLE_".into(),
                ".got.plt".into(),
                0,
                Some(w.add_string("_GLOBAL_OFFSET_TABLE_".as_bytes())),
                None, //Some(w.add_dynamic_string("_GLOBAL_OFFSET_TABLE_".as_bytes())),
            ),
            LocalSymbol::new(
                "ASDF".into(),
                ".got.plt".into(),
                0,
                Some(w.add_string("ASDF".as_bytes())),
                None, //Some(w.add_dynamic_string("ASDF".as_bytes())),
            ),
        ];
        //data.locals = locals;

        // requires dynamic addr
        //update_symbols(&locals, data, &mut tracker);

        //if let Some(block) = maybe_block.as_mut() {
        //for (name, s) in block.locals.iter() {
        //let p = ProgSymbol::new_object(name, None);
        //data.lookup.insert(name.clone(), p);
        //}

        for (name, s) in block.exports.iter() {
            //eprintln!("x: {:?}", s);
            //let p = ProgSymbol::new_object(name, None);
            //data.lookup.insert(name.clone(), p);
            unsafe {
                let buf = extend_lifetime(s.name.as_bytes());
                let name_id = Some(w.add_string(buf));
                data.locals.push(LocalSymbol::new(
                    name.clone(),
                    s.section.section_name().to_string(),
                    0,
                    name_id,
                    None,
                ));

                let name_id = Some(w.add_dynamic_string(buf));
                data.dynamic.push(LocalSymbol::new(
                    name.clone(),
                    s.section.section_name().to_string(),
                    0,
                    name_id,
                    None,
                ));
            }
        }

        //block.update_symbols(data, w);
        //}

        /*
        let dynamic_section_index = data
            .section_index
            .get(&".dynamic".to_string())
            .cloned()
            .unwrap();
        //for _ in locals.iter() {
            //w.reserve_symbol_index(Some(dynamic_section_index));
        //}
        */

        // setup symbols
        for b in self.blocks.iter_mut() {
            b.reserve_symbols(data, w);
        }

        //if let Some(block) = maybe_block.as_mut() {
        block.reserve_symbols(data, w);
        //}

        // RESERVE

        // finalize the layout
        self.reserve(&mut tracker, data, block, w);

        // once we have the layout, we can assign the symbols
        //if let Some(block) = maybe_block.as_mut() {
        //block.dump();
        block.complete(&data);
        //}

        if data.add_section_headers {
            w.reserve_section_headers();
        }

        //for (k, v) in data.pointers.iter() {
        //eprintln!("P: {}, {:#0x}", k, v);
        //}

        // UPDATE
        tracker.ph = self.program_headers(&tracker);
        self.update(data);

        // WRITE
        self.write(&data, &mut tracker, block, w);

        // SECTION HEADERS
        if data.add_section_headers {
            self.write_section_headers(&data, &tracker, block, w);
        }
    }

    /*
    fn reserve_symbols(&mut self, data: &mut Data, w: &mut Writer) {
        for s in data.symbols.values() {
            //w.reserve_symbol_index(s.section_index);
        }
    }
    */

    /// generate a temporary list of program headers
    pub fn generate_ph(&mut self, block: &mut ReadBlock) -> Vec<ProgramHeaderEntry> {
        // build a list of sections that are loaded
        // this is a hack to get tracker to build a correct list of program headers
        // without having to go through the blocks and do reservations
        let mut temp_tracker = SegmentTracker::new(0);
        let mut d = Data::new(vec![]);
        let mut out_data = Vec::new();
        let endian = Endianness::Little;
        let mut temp_w = object::write::elf::Writer::new(endian, d.is_64, &mut out_data);
        temp_w.add_string("asdf".as_bytes());
        temp_w.add_dynamic_string("asdf".as_bytes());

        for b in self.blocks.iter_mut() {
            let pos = temp_w.reserved_len();
            b.reserve(&mut d, &mut temp_tracker, block, &mut temp_w);
            let after = temp_w.reserved_len();
        }
        // get a list of program headers
        // we really only need to know the number of headers, so we can correctly
        // set the values in the file header
        self.generate_program_headers(&mut temp_tracker);
        temp_tracker.ph
    }

    pub fn reserve(
        &mut self,
        tracker: &mut SegmentTracker,
        data: &mut Data,
        block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        for b in self.blocks.iter_mut() {
            let pos = w.reserved_len();
            b.reserve(data, tracker, block, w);
            let after = w.reserved_len();
            log::debug!(
                "reserve: {}, {:#0x}, {:#0x},  {:?}",
                b.name(),
                pos,
                after,
                b.alloc()
            );
        }
    }

    pub fn reserve_section_index(
        &mut self,
        data: &mut Data,
        block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        for b in self.blocks.iter_mut() {
            b.reserve_section_index(data, block, w);
        }
    }

    pub fn update(&mut self, data: &mut Data) {
        for b in self.blocks.iter_mut() {
            b.update(data);
        }
    }

    pub fn write(
        &self,
        data: &Data,
        tracker: &mut SegmentTracker,
        block: &mut ReadBlock,
        w: &mut Writer,
    ) {
        // generate the program headers, so they have up to date fields
        //self.generate_program_headers(tracker);
        //for p in tracker.ph.iter() {
        //eprintln!("P: {:?}", p);
        //}
        for b in self.blocks.iter() {
            let pos = w.len();
            b.write(&data, tracker, block, w);
            let after = w.len();
            log::debug!(
                "write: {}, {:?}, pos: {:#0x}, after: {:#0x}, base: {:#0x}",
                b.name(),
                b.alloc(),
                pos,
                after,
                tracker.current().base
            );
        }
    }

    pub fn write_section_headers(
        &self,
        data: &Data,
        tracker: &SegmentTracker,
        block: &ReadBlock,
        w: &mut Writer,
    ) {
        for b in self.blocks.iter() {
            b.write_section_header(&data, &tracker, block, w);
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
}

impl SegmentTracker {
    pub fn new(base: usize) -> Self {
        Self {
            segments: vec![],
            base,
            page_size: 0x1000,
            ph: vec![],
        }
    }

    pub fn current(&self) -> &Segment {
        self.segments.last().unwrap()
    }

    pub fn current_mut(&mut self) -> &mut Segment {
        self.segments.last_mut().unwrap()
    }

    pub fn add_section(
        &mut self,
        alloc: AllocSegment,
        section: &ProgSection,
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
            //eprintln!("new seg: {:?}, offset: {:#0x}", alloc, file_offset);
            self.segments.push(segment);
        }
        self.current_mut().add_data(size, alloc.align());
        self.base
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
}

pub struct Segment {
    pub base: u64,
    pub addr: u64,
    pub offset: u64,
    segment_size: usize,
    pub align: u32,
    pub alloc: AllocSegment,
    //pub sections: Vec<ProgSection>,
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
            //sections: vec![],
        }
    }

    pub fn add_section(&mut self, section: &ProgSection) {
        let start = size_align(self.segment_size, section.kind.align());
        self.segment_size = start + section.size();
        //self.sections.push(section);
    }

    pub fn size(&self) -> usize {
        self.segment_size
    }

    pub fn add_data(&mut self, size: usize, align: usize) {
        // set size to match the offset size
        let _before = self.segment_size;
        let delta = size_align(size, align);
        //eprintln!("x/{:#0x}/{:#0x}", size, delta);
        self.segment_size += delta;
        //eprintln!(
        //"add_data/{:?}/{:#0x}, {:#0x}+{:#0x}={:#0x}/{:#0x}",
        //self.alloc, size, before, delta, self.segment_size, align
        //);
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
