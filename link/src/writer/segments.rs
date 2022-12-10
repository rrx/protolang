use super::*;
use capstone::prelude::*;

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
            ro: Segment::new_ro(),
            rw: Segment::new_rw(),
            rx: Segment::new_rx(),
            addr_start: 0,
            file_offset: 0,
        }
    }
}

impl Segments {
    pub fn add_file_offset(&mut self, offset: u64) {
        self.file_offset += offset;
    }

    pub fn file_offset(&self) -> u64 {
        self.file_offset
    }

    pub fn load<'a>(&mut self, link: &'a Link, w: &mut Writer<'a>) {
        for (_name, unlinked) in link.unlinked.iter() {
            use object::SectionKind as K;
            match unlinked.kind {
                K::Data | K::UninitializedData => {
                    self.rw.add_unlinked(unlinked, w);
                }

                // OtherString is usually comments, we can drop these
                K::OtherString => (),
                K::ReadOnlyString | K::ReadOnlyData => {
                    eprintln!("X:{:?}", (&unlinked.name, &unlinked.kind));
                    self.ro.add_unlinked(unlinked, w);
                    // XXX: this can mess things up
                    // it adds things to RO before the text begins and gets confused
                }
                K::Text => {
                    self.rx.add_unlinked(unlinked, w);
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

    /*
        pub fn load(&mut self, link: &Link) {
            let mut ro_size = 0;
            let mut rw_size = 0;
            let mut rx_size = 0;

            let mut data_relocs = vec![];
            let mut text_relocs = vec![];
            for (_name, unlinked) in link.unlinked.iter() {
                use object::SectionKind as K;
                match unlinked.kind {
                    K::Data | K::UninitializedData => {
                        rw_size += unlinked.bytes.len();
                        self.rw.bytes.extend(unlinked.bytes.clone());
                        data_relocs.extend(unlinked.relocations.clone());
                    }
                    K::OtherString | K::ReadOnlyString | K::ReadOnlyData => {
                        ro_size += unlinked.bytes.len();
                        self.ro.bytes.extend(unlinked.bytes.clone());
                        data_relocs.extend(unlinked.relocations.clone());
                    }
                    K::Text => {
                        rx_size += unlinked.bytes.len();
                        self.rx.bytes.extend(unlinked.bytes.clone());
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
        }

        pub fn read_unlinked<'a>(&mut self, link: &'a Link, w: &mut Writer<'a>) {
            let mut ro_size = 0;
            let mut rw_size = 0;
            let mut rx_size = 0;

            // get symbols and relocations
            for (_name, unlinked) in link.unlinked.iter() {
                use object::SectionKind as K;
                let is_start = false;
                match unlinked.kind {
                    K::Data | K::UninitializedData => {
                        self.rw.relocations.extend(unlinked.relocations.clone());
                        for (name, symbol) in unlinked.defined.iter() {
                            let name_id = Some(w.add_string(name.as_bytes()));

                            let mut symbol = symbol.clone();
                            symbol.address += rw_size as u64;

                            let ps = ProgSymbol {
                                name_id,
                                is_start,
                                s: symbol,
                            };

                            self.rw.section.symbols.insert(name.clone(), ps);
                        }
                        rw_size += unlinked.bytes.len();
                    }
                    K::OtherString | K::ReadOnlyString | K::ReadOnlyData => {
                        self.ro.relocations.extend(unlinked.relocations.clone());
                        for (name, symbol) in unlinked.defined.iter() {
                            let name_id = Some(w.add_string(name.as_bytes()));
                            let mut symbol = symbol.clone();
                            symbol.address += ro_size as u64;
                            let ps = ProgSymbol {
                                name_id,
                                is_start,
                                s: symbol,
                            };
                            self.ro.section.symbols.insert(name.clone(), ps);
                        }
                        ro_size += unlinked.bytes.len();
                    }
                    K::Text => {
                        self.rx.relocations.extend(unlinked.relocations.clone());
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
                            self.rx.section.symbols.insert(name.clone(), ps);
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
    */

    /// update the segments
    pub fn update(&mut self, base: usize, page_size: usize) {
        let align = AllocSegment::RO.align();
        let mut offset = 0;
        let ro_size_elf_aligned = size_align(self.ro.size as usize, align);
        self.ro.base = base as u64;
        self.ro.addr = base as u64 + offset as u64;
        self.ro.offset = offset as u64;
        self.ro.align = align as u32;
        log::debug!(
            "RO base:{:#0x}, offset:{:#0x}, size:{:#0x}, fo:{:#0x}",
            base,
            offset,
            self.ro.size,
            self.file_offset()
        );
        offset += ro_size_elf_aligned;
        self.add_file_offset(self.ro.size as u64);

        let align = AllocSegment::RX.align();
        let base = size_align(base as usize + offset, page_size) as u64;
        let rx_size_elf_aligned = size_align(self.rx.size as usize, align);
        self.rx.base = base;
        self.rx.addr = base + offset as u64;
        self.rx.offset = offset as u64;
        self.rx.align = align as u32;
        log::debug!(
            "RX base:{:#0x}, offset:{:#0x}, size:{:#0x} {}, fo:{:#0x}",
            base,
            offset,
            self.rx.size,
            rx_size_elf_aligned,
            self.file_offset()
        );
        offset += rx_size_elf_aligned;
        self.add_file_offset(self.rx.size as u64);

        let align = AllocSegment::RW.align();
        let base = size_align(base as usize + rx_size_elf_aligned, page_size) as u64;
        let rw_size_elf_aligned = size_align(self.rw.size as usize, align);
        self.rw.base = base;
        self.rw.addr = base + offset as u64;
        self.rw.offset = offset as u64;
        self.rw.align = align as u32;
        log::debug!(
            "RW base:{:#0x}, offset:{:#0x}, size:{:#0x}, fo:{:#0x}",
            base,
            offset,
            self.rw.size,
            self.file_offset()
        );
        self.add_file_offset(self.rw.size as u64);

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
    size: usize,
    segment_size: usize,
    pub align: u32,
    pub alloc: AllocSegment,
    pub bytes: Vec<u8>,
    pub relocations: Vec<CodeRelocation>,
    pub section: ProgSection,
}

impl Segment {
    pub fn new_ro() -> Self {
        Self {
            base: 0,
            addr: 0,
            offset: 0,
            size: 0,
            segment_size: 0,
            alloc: AllocSegment::RO,
            align: 0x1000,
            bytes: vec![],
            relocations: vec![],
            section: ProgSection::new(AllocSegment::RO),
        }
    }

    pub fn new_rw() -> Self {
        Self {
            base: 0,
            addr: 0,
            offset: 0,
            size: 0,
            segment_size: 0,
            alloc: AllocSegment::RW,
            align: 0x1000,
            bytes: vec![],
            relocations: vec![],
            section: ProgSection::new(AllocSegment::RW),
        }
    }

    pub fn new_rx() -> Self {
        Self {
            base: 0,
            addr: 0,
            offset: 0,
            size: 0,
            segment_size: 0,
            alloc: AllocSegment::RX,
            align: 0x1000,
            bytes: vec![],
            relocations: vec![],
            section: ProgSection::new(AllocSegment::RX),
        }
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn add_data(&mut self, size: usize, align: usize) {
        // set size to match the offset size
        let before = self.size;
        let delta = size_align(size, align);
        eprintln!("x/{:#0x}/{:#0x}", size, delta);
        self.size += delta;
        eprintln!("add_data/{:?}/{:#0x}, {:#0x}+{:#0x}={:#0x}/{:#0x}", self.alloc, size, before, delta, self.size, align);
    }

    pub fn add_unlinked<'a>(&mut self, unlinked: &'a UnlinkedCodeSegment, w: &mut Writer<'a>) {
        //let base = self.size as u64;
        self.bytes.extend(unlinked.bytes.clone());
        //self.size += unlinked.bytes.len();
        for r in &unlinked.relocations {
            let mut r = r.clone();
            r.offset += self.segment_size as u64;
            self.relocations.push(r.clone());
        }

        for (name, symbol) in unlinked.defined.iter() {
            let name_id = Some(w.add_string(name.as_bytes()));
            let mut symbol = symbol.clone();
            symbol.address += self.segment_size as u64;
            let is_start = name == "_start";
            let ps = ProgSymbol {
                name_id,
                is_start,
                s: symbol,
            };
            self.section.symbols.insert(name.clone(), ps);
        }
        self.segment_size += unlinked.bytes.len();
    }

    pub fn debug(&self) {
        eprintln!(
            "Segment: {:?} base:{:#0x}, offset:{:#0x}, size:{:#0x} ({})",
            self.alloc, self.base, self.offset, self.size, self.size,
        );

        for (name, s) in &self.section.symbols {
            eprintln!(" S:{} {:?}", name, s);
        }
        //self.disassemble_code();
    }

    pub fn disassemble_code(&self) {
        let buf = &self.bytes.as_slice()[0..self.size];
        let cs = capstone::Capstone::new()
            .x86()
            .mode(arch::x86::ArchMode::Mode64)
            .syntax(arch::x86::ArchSyntax::Att)
            .detail(true)
            .build()
            .unwrap();
        let insts = cs.disasm_all(&buf, 0).expect("disassemble");
        for instr in insts.as_ref() {
            let addr = instr.address() as usize;
            eprintln!(
                "  {:#06x} {}\t\t{}",
                &addr,
                instr.mnemonic().expect("no mnmemonic found"),
                instr.op_str().expect("no op_str found")
            );
        }
    }
}
