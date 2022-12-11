use super::*;
use capstone::prelude::*;

pub struct SegmentTracker {
    segments: Vec<Segment>,
    base: usize,
    page_size: usize,
    //blocks: Vec<Box<dyn ElfBlock>>
}

impl SegmentTracker {
    pub fn new(base: usize) -> Self {
        Self {
            segments: vec![],
            base,
            page_size: 0x1000,
        }
    }

    pub fn current(&self) -> &Segment {
        self.segments.last().unwrap()
    }

    pub fn current_mut(&mut self) -> &mut Segment {
        self.segments.last_mut().unwrap()
    }

    // add non-section data
    pub fn add_data(&mut self, alloc: AllocSegment, size: usize) {
        if self.segments.len() == 0 {
            self.segments.push(Segment::new(alloc));
        }

        let c = self.current();
        if alloc != c.alloc {
            let base = size_align(self.base + c.size(), self.page_size) as u64;
            let mut segment = Segment::new(alloc);
            segment.base = base;
            self.segments.push(segment);
        }
        self.current_mut().add_data(size, alloc.align());
    }

    pub fn write_symbols(&self, w: &mut Writer) {
        for s in self.segments.iter() {
            s.write_symbols(w);
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
    pub addr_start: u64,
    file_offset: u64,
}
impl Default for Segments {
    fn default() -> Self {
        Self {
            ro: Segment::new(AllocSegment::RO),
            rw: Segment::new(AllocSegment::RW),
            rx: Segment::new(AllocSegment::RX),
            addr_start: 0,
            file_offset: 0,
        }
    }
}

impl Segments {
    fn add_file_offset(&mut self, offset: u64) {
        self.file_offset += offset;
    }

    pub fn file_offset(&self) -> u64 {
        self.file_offset
    }

    pub fn write_symbols(&self, w: &mut Writer) {
        self.ro.write_symbols(w);
        self.rx.write_symbols(w);
        self.rw.write_symbols(w);
    }

    pub fn load<'a>(&mut self, link: &'a Link, w: &mut Writer<'a>) {
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

    /// update the segments
    pub fn update(&mut self, base: usize, page_size: usize) {
        let align = AllocSegment::RO.align();
        let mut offset = 0;
        let ro_size_elf_aligned = size_align(self.ro.size() as usize, align);
        self.ro.base = base as u64;
        self.ro.addr = base as u64 + offset as u64;
        self.ro.offset = offset as u64;
        self.ro.align = align as u32;
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
        self.rx.base = base;
        self.rx.addr = base + offset as u64;
        self.rx.offset = offset as u64;
        self.rx.align = align as u32;
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
        self.rw.base = base;
        self.rw.addr = base + offset as u64;
        self.rw.offset = offset as u64;
        self.rw.align = align as u32;
        log::debug!(
            "RW base:{:#0x}, offset:{:#0x}, size:{:#0x}, fo:{:#0x}",
            base,
            offset,
            self.rw.size(),
            self.file_offset()
        );
        self.add_file_offset(self.rw.size() as u64);

        // set entry point
        for (_name, sym) in &self.rx.section.symbols {
            if sym.is_start {
                self.addr_start = self.rx.addr + sym.s.address;
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
    pub section: ProgSection,
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
            section: ProgSection::new(alloc, None, 1, 0),
            sections: vec![],
        }
    }

    pub fn add_section(&mut self, section: ProgSection) {
        let start = size_align(self.segment_size, section.align);
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
        eprintln!(
            "Section: {:?}, size:{:#0x} ({})",
            self.section.name,
            self.section.bytes.len(),
            self.section.bytes.len()
        );

        for (name, s) in &self.section.symbols {
            eprintln!(" S:{} {:?}", name, s);
        }
        //self.disassemble_code();
    }

    pub fn disassemble_code(&self) {
        self.section.disassemble_code();
        for s in self.sections.iter() {
            s.disassemble_code();
        }
    }

    pub fn write_symbols(&self, w: &mut Writer) {
        self.section.write_symbols(self.base, w);
        for s in self.sections.iter() {
            s.write_symbols(self.base, w);
        }
    }
}
