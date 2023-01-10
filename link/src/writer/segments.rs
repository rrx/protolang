use super::*;

pub struct BlocksBuilder {}

impl BlocksBuilder {
    pub fn new() -> Self {
        Self {}
    }

    pub fn build(self, data: &mut Data, w: &mut Writer, block: &mut ReadBlock) -> Blocks {
        // Setup locals
        data.locals = vec![
            LocalSymbol::new(
                "_DYNAMIC".into(),
                ".dynamic".into(),
                ResolvePointer::Section(".dynamic".to_string(), 0),
                Some(w.add_string("_DYNAMIC".as_bytes())),
                None, //Some(w.add_dynamic_string("_DYNAMIC".as_bytes())),
            ),
            LocalSymbol::new(
                "_GLOBAL_OFFSET_TABLE_".into(),
                ".got.plt".into(),
                ResolvePointer::Section(".got.plt".to_string(), 0),
                Some(w.add_string("_GLOBAL_OFFSET_TABLE_".as_bytes())),
                None, //Some(w.add_dynamic_string("_GLOBAL_OFFSET_TABLE_".as_bytes())),
            ),
            LocalSymbol::new(
                "__bss_start".into(),
                ".bss".into(),
                ResolvePointer::Section(".bss".to_string(), 0),
                Some(w.add_string("__bss_start".as_bytes())),
                None,
            ),
            LocalSymbol::new(
                "__rodata_start".into(),
                ".rodata".into(),
                ResolvePointer::Section(".rodata".to_string(), 0),
                Some(w.add_string("__rodata_start".as_bytes())),
                None,
            ),
        ];

        for local in data.locals.iter() {
            //data.statics.symbol_add(
            data.pointers
                .insert(local.symbol.clone(), local.pointer.clone());
            let symbol = ReadSymbol::from_pointer(local.symbol.clone(), local.pointer.clone());
            //data.statics.symbol_add(&symbol.name, symbol.section.section_index(data), w);
            block.insert_local(symbol);
        }

        block.build_strings(data, w);

        let mut blocks: Vec<Box<dyn ElfBlock>> = vec![];

        blocks.push(Box::new(FileHeader::default()));
        blocks.push(Box::new(ProgramHeader::default()));

        if data.is_dynamic() {
            // BufferSection doesn't implement the program header, we really need
            // the dedicated interp section code to make that work
            // interp is an exception
            blocks.push(Box::new(InterpSection::new(&data)));
        }

        blocks.push(Box::new(HashSection::new()));

        if data.is_dynamic() {
            blocks.push(Box::new(DynSymSection::default()));
            if w.dynstr_needed() {
                blocks.push(Box::new(DynStrSection::default()));
            }
            blocks.push(Box::new(RelaDynSection::new(GotKind::GOT(false))));
            blocks.push(Box::new(RelaDynSection::new(GotKind::GOTPLT)));
        }

        blocks.push(ReadSectionKind::ROData.block());
        blocks.push(ReadSectionKind::RX.block());
        blocks.push(Box::new(PltSection::new()));
        blocks.push(ReadSectionKind::RW.block());

        if data.is_dynamic() {
            blocks.push(Box::new(DynamicSection::default()));
            blocks.push(Box::new(GotSection::new(GotKind::GOT(false))));
            blocks.push(Box::new(GotSection::new(GotKind::GOTPLT)));
        }

        // bss is the last alloc block
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

        Blocks { blocks }
    }
}

pub struct Blocks {
    pub blocks: Vec<Box<dyn ElfBlock>>,
}

impl Blocks {
    pub fn build(&mut self, data: &mut Data, w: &mut Writer, block: &mut ReadBlock) {
        let mut tracker = SegmentTracker::new(data.base as u64);
        tracker.ph = self.generate_ph(block);

        // RESERVE SECTION HEADERS
        // section headers are optional
        if data.add_section_headers {
            self.reserve_section_index(data, block, w);
        }

        // RESERVE SYMBOLS
        for local in data.locals.iter() {
            let symbol = ReadSymbol::from_pointer(local.symbol.clone(), local.pointer.clone());
            data.statics
                .symbol_add(&symbol.name, symbol.section.section_index(data), w);
        }

        for (_, symbol) in block.exports.iter() {
            data.statics
                .symbol_add(&symbol.name, symbol.section.section_index(data), w);
        }

        for b in self.blocks.iter_mut() {
            b.reserve_symbols(data, block, w);
        }

        //block.reserve_symbols(data, w);

        // RESERVE

        // finalize the layout
        self.reserve(&mut tracker, data, block, w);

        // once we have the layout, we can assign the symbols
        block.complete(&data);

        if data.add_section_headers {
            w.reserve_section_headers();
        }

        // UPDATE
        tracker.ph = self.program_headers(&tracker, block);
        self.update(data);

        // WRITE
        for b in self.blocks.iter() {
            let pos = w.len();
            //eprintln!("write: {}", b.name());
            b.write(&data, &mut tracker, block, w);
            let after = w.len();
            eprintln!(
                "write: {}, {:?}, pos: {:#0x}, after: {:#0x}, base: {:#0x}",
                b.name(),
                b.alloc(),
                pos,
                after,
                tracker.current().base
            );
        }

        // SECTION HEADERS
        if data.add_section_headers {
            self.write_section_headers(&data, &tracker, block, w);
        }
    }

    /// generate a temporary list of program headers
    pub fn generate_ph(&mut self, block: &mut ReadBlock) -> Vec<ProgramHeaderEntry> {
        // build a list of sections that are loaded
        // this is a hack to get tracker to build a correct list of program headers
        // without having to go through the blocks and do reservations
        let mut temp_tracker = SegmentTracker::new(0);
        let mut d = Data::new(vec![]);
        d.addr_set(".got.plt", 0);
        d.pointer_set("_start".to_string(), 0);
        d.pointer_set("__data_start".to_string(), 0);
        let mut out_data = Vec::new();
        let endian = Endianness::Little;
        let mut temp_w = object::write::elf::Writer::new(endian, d.is_64, &mut out_data);
        temp_w.add_string("asdf".as_bytes());
        temp_w.add_dynamic_string("asdf".as_bytes());

        block.build_strings(&mut d, &mut temp_w);
        for b in self.blocks.iter_mut() {
            b.reserve_section_index(&mut d, block, &mut temp_w);
        }

        for b in self.blocks.iter_mut() {
            b.reserve_symbols(&mut d, block, &mut temp_w);
        }

        for b in self.blocks.iter_mut() {
            //eprintln!("reserve: {}", b.name());
            b.reserve(&mut d, &mut temp_tracker, block, &mut temp_w);
        }
        // get a list of program headers
        // we really only need to know the number of headers, so we can correctly
        // set the values in the file header
        self.generate_program_headers(&mut temp_tracker, block);
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
            eprintln!(
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

    pub fn program_headers(
        &self,
        tracker: &SegmentTracker,
        block: &ReadBlock,
    ) -> Vec<ProgramHeaderEntry> {
        let mut ph = vec![];
        for b in self.blocks.iter() {
            ph.extend(b.program_header(block));
        }
        ph.extend(tracker.program_headers());

        // hack to get dynamic to be the last program header
        // may not be necessary
        ph.iter()
            .filter(|p| p.p_type != elf::PT_DYNAMIC)
            .cloned()
            .chain(ph.iter().filter(|p| p.p_type == elf::PT_DYNAMIC).cloned())
            .collect()
    }

    pub fn generate_program_headers(&self, tracker: &mut SegmentTracker, block: &ReadBlock) {
        let ph = self.program_headers(tracker, block);
        tracker.ph = ph;
    }
}

#[derive(Debug)]
pub struct SectionOffset {
    pub base: u64,
    pub address: u64,
    pub file_offset: u64,
    pub align: u64,
    pub size: u64,
}

impl SectionOffset {
    pub fn new(align: u64) -> Self {
        Self {
            address: 0,
            base: 0,
            file_offset: 0,
            align,
            size: 0,
        }
    }
}

pub struct SegmentTracker {
    segments: Vec<Segment>,
    // track the current segment base
    start_base: u64,
    page_size: usize,
    pub ph: Vec<ProgramHeaderEntry>,
}

impl SegmentTracker {
    pub fn new(start_base: u64) -> Self {
        Self {
            segments: vec![],
            start_base,
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

    // add non-section data
    pub fn add_offsets(
        &mut self,
        alloc: AllocSegment,
        offsets: &mut SectionOffset,
        size: usize,
        w: &Writer,
    ) {
        let current_size;
        let current_file_offset;
        let current_alloc;
        let mut base;

        // get current segment, or defaults
        if let Some(c) = self.segments.last() {
            current_size = c.size() as u64;
            current_file_offset = c.file_offset;
            current_alloc = c.alloc;
            base = c.base;
        } else {
            current_alloc = alloc;
            current_file_offset = 0;
            current_size = 0;
            base = self.start_base;
        }

        // if we are initializing the first segment, or the segment has changed
        // we start a new segment
        if self.segments.len() == 0 || alloc != current_alloc {
            // calculate the base for the segment, based on page size, and the size of the previous
            // segment
            base = size_align((base + current_size) as usize, self.page_size) as u64;

            // align the new file offset
            let file_offset = size_align(
                current_file_offset as usize + current_size as usize,
                offsets.align as usize,
            ) as u64;

            // new segment
            let segment = Segment::new(alloc, base, file_offset as u64);

            log::debug!(
                "new seg: {:?}, offset: {:#0x}, last_offset: {:#0x}, last_size: {:#0x}, size: {:#0x}, align: {:#0x}, base: {:#0x}",
                alloc,
                file_offset,
                current_file_offset,
                current_size,
                size,
                offsets.align,
                base,
            );
            //eprintln!("seg: {:?}", segment);
            self.segments.push(segment);

            if file_offset < (current_file_offset + current_size) {
                eprintln!(
                    "fail: {:?}, file_offset: {:#0x}: current offset: {:#0x}, current size: {:#0x}",
                    alloc, file_offset, current_file_offset, current_size
                );
            }
            assert!(file_offset >= (current_file_offset + current_size));
        }

        self.current_mut().add_offsets(offsets, size, w);
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

#[derive(Debug)]
pub struct Segment {
    // segment base
    pub base: u64,

    // address of the segment
    //pub addr: u64,

    // track the size of teh segment
    segment_size: usize,

    // segment alignment (0x1000)
    pub align: u32,

    pub alloc: AllocSegment,

    // file offset for the segment
    pub file_offset: u64,

    // keep track of the last section file offset for this segment
    pub adjusted_file_offset: u64,
}

impl Segment {
    pub fn new(alloc: AllocSegment, base: u64, file_offset: u64) -> Self {
        Self {
            base,
            //addr: 0,
            file_offset,
            adjusted_file_offset: file_offset,
            segment_size: 0,
            alloc,
            align: 0x1000,
        }
    }

    pub fn size(&self) -> usize {
        self.segment_size
    }

    pub fn add_offsets(&mut self, offsets: &mut SectionOffset, size: usize, w: &Writer) {
        let aligned = size_align(self.segment_size, offsets.align as usize);
        self.segment_size = aligned + size;
        self.adjusted_file_offset = self.file_offset + aligned as u64;

        //eprintln!("add: {:#0x}, {:?}", size, self);
        //eprintln!(
        //"x: {:#0x}, {:#0x}",
        //self.adjusted_file_offset as usize + size,
        //w.reserved_len()
        //);

        assert_eq!(self.adjusted_file_offset as usize + size, w.reserved_len());

        offsets.base = self.base;
        offsets.size = size as u64;
        offsets.address = self.base + self.adjusted_file_offset;
        offsets.file_offset = self.adjusted_file_offset;

        //eprintln!("add: {:#0x}, {:?}", size, self);
    }

    pub fn program_header(&self) -> Option<ProgramHeaderEntry> {
        // add a load section for the file and program header, so it's covered
        let size = self.size() as u64;
        Some(ProgramHeaderEntry {
            p_type: elf::PT_LOAD,
            p_flags: self.alloc.program_header_flags(),
            p_offset: self.file_offset,
            p_vaddr: self.base + self.file_offset,
            p_paddr: 0,
            p_filesz: size,
            p_memsz: size,
            p_align: self.align as u64,
        })
    }
}
