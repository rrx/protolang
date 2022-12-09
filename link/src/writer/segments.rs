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

pub struct Segment {
    pub base: u64,
    pub addr: u64,
    pub offset: u64,
    pub size: usize,
    pub align: u32,
    pub alloc: AllocSegment,
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
            //blocks: vec![],
            //components: vec![],
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
            //blocks: vec![],
            //components: vec![],
        }
    }

    pub fn add_data(&mut self, size: usize, align: usize) {
        self.size = size_align(self.size, align) + size;
    }
}
