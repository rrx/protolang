use super::*;

pub struct Segments {
    pub ro: Segment,
    pub rx: Segment,
    pub rw: Segment,
}
impl Default for Segments {
    fn default() -> Self {
        Self {
            ro: Segment::new_ro(),
            rw: Segment::new_rw(),
            rx: Segment::new_rx(),
        }
    }
}

impl Segments {
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
}

pub struct Segment {
    pub base: u64,
    pub addr: u64,
    pub offset: u64,
    pub size: usize,
    pub align: u32,
    pub alloc: AllocSegment,
    pub bytes: Vec<u8>,
    pub relocations: Vec<CodeRelocation>,
    pub section: ProgSection,
    //pub blocks: Vec<BufferSection>,
    //pub components: Vec<Box<dyn ElfComponent>>,
}

impl Segment {
    pub fn new_ro() -> Self {
        Self {
            base: 0,
            addr: 0,
            offset: 0,
            size: 0,
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
            alloc: AllocSegment::RW,
            align: 0x1000,
            bytes: vec![],
            relocations: vec![],
            section: ProgSection::new(AllocSegment::RW),
            //blocks: vec![],
            //components: vec![],
        }
    }

    pub fn new_rx() -> Self {
        Self {
            base: 0,
            addr: 0,
            offset: 0,
            size: 0,
            alloc: AllocSegment::RX,
            align: 0x1000,
            bytes: vec![],
            relocations: vec![],
            section: ProgSection::new(AllocSegment::RX),
            //blocks: vec![],
            //components: vec![],
        }
    }

    pub fn add_data(&mut self, size: usize, align: usize) {
        self.size = size_align(self.size, align) + size;
    }
}
