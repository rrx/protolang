use crate::relocations::*;
use crate::writer::*;
use object::elf;
use object::write::elf::{SectionIndex, Writer};
use object::write::StringId;

#[derive(Debug, Clone)]
pub struct BlockSection {
    pub(crate) alloc: AllocSegment,
    name: &'static str,
    name_id: Option<StringId>,
    pub(crate) file_offset: usize,
    pub(crate) base: usize,
    pub(crate) addr: usize,
    pub(crate) section_index: Option<SectionIndex>,
    pub(crate) bytes: Vec<u8>,
    pub(crate) relocations: Vec<CodeRelocation>,
}

impl BlockSection {
    pub fn new(alloc: AllocSegment, name: &'static str) -> Self {
        Self {
            alloc,
            name,
            file_offset: 0,
            base: 0,
            addr: 0,
            section_index: None,
            name_id: None,
            bytes: vec![],
            relocations: vec![],
        }
    }

    pub fn align(&self) -> usize {
        self.alloc.align()
    }

    pub fn block_reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        self.name_id = Some(w.add_section_name(self.name.as_bytes()));
        //self.name = Some(name.to_string());
        let index = w.reserve_section_index();
        self.section_index = Some(index);
        data.section_index_set(&self.name, index);
    }

    pub fn block_reserve(&mut self, data: &mut Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        let pos = w.reserved_len();
        let align_pos = size_align(pos, self.align());
        w.reserve_until(align_pos);
        self.file_offset = w.reserved_len();

        w.reserve(self.bytes.len(), self.align());
        let after = w.reserved_len();
        let delta = after - pos;

        self.base = tracker.add_data(self.alloc, delta, self.file_offset);
        self.addr = self.base + self.file_offset;
        //let name = self.name.as_ref().unwrap();
        data.addr_set(&self.name, self.addr as u64);
        eprintln!(
            "FO: {}, {:?}, {:#0x}, base: {:#0x}, addr: {:#0x}, delta: {:#0x}, size: {:#0x}",
            self.name,
            self.alloc,
            self.file_offset,
            self.base,
            self.addr,
            delta,
            self.bytes.len()
        );

        /*
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

        */
    }

    pub fn block_write(&self, _data: &Data, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align());
        //panic!("ASdfa");
        eprintln!("AF: {:#0x}", aligned_pos);
        w.pad_until(aligned_pos);
        w.write(self.bytes.as_slice());
    }

    pub fn block_write_section_header(&self, data: &Data, w: &mut Writer) {
        if let Some(name_id) = self.name_id {
            w.write_section_header(&object::write::elf::SectionHeader {
                name: Some(name_id),
                sh_type: elf::SHT_PROGBITS,
                sh_flags: self.alloc.section_header_flags() as u64,
                sh_addr: self.addr as u64,
                sh_offset: self.file_offset as u64,
                sh_info: 0,
                sh_link: 0,
                sh_entsize: 0,
                sh_addralign: self.alloc.align() as u64,
                sh_size: self.bytes.len() as u64,
            });
        }
    }
}

#[derive(Debug, Clone)]
pub struct BssSection {
    pub(crate) alloc: AllocSegment,
    name: &'static str,
    name_id: Option<StringId>,
    pub(crate) file_offset: usize,
    pub(crate) base: usize,
    pub(crate) addr: usize,
    pub(crate) section_index: Option<SectionIndex>,
    pub(crate) size: usize,
    pub(crate) relocations: Vec<CodeRelocation>,
}

impl BssSection {
    pub fn new(alloc: AllocSegment, name: &'static str) -> Self {
        Self {
            alloc,
            name,
            file_offset: 0,
            base: 0,
            addr: 0,
            section_index: None,
            name_id: None,
            size: 0,
            relocations: vec![],
        }
    }

    pub fn align(&self) -> usize {
        self.alloc.align()
    }

    pub fn block_reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        self.name_id = Some(w.add_section_name(self.name.as_bytes()));
        let index = w.reserve_section_index();
        self.section_index = Some(index);
        data.section_index_set(&self.name, index);
    }

    pub fn block_reserve(&mut self, data: &mut Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        let pos = w.reserved_len();
        let align_pos = size_align(pos, self.align());
        w.reserve_until(align_pos);
        self.file_offset = w.reserved_len();
        let delta = self.file_offset - pos;
        self.base = tracker.add_data(self.alloc, delta, self.file_offset);
        self.addr = self.base + self.file_offset;
        data.addr_set(&self.name, self.addr as u64);
    }

    pub fn block_write(&self, _data: &Data, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align());
        w.pad_until(aligned_pos);
    }

    pub fn block_write_section_header(&self, _data: &Data, w: &mut Writer) {
        if let Some(name_id) = self.name_id {
            w.write_section_header(&object::write::elf::SectionHeader {
                name: Some(name_id),
                sh_type: elf::SHT_PROGBITS,
                sh_flags: self.alloc.section_header_flags() as u64,
                sh_addr: self.addr as u64,
                sh_offset: self.file_offset as u64,
                sh_info: 0,
                sh_link: 0,
                sh_entsize: 0,
                sh_addralign: self.alloc.align() as u64,
                sh_size: self.size as u64,
            });
        }
    }
}
