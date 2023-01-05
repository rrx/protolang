use crate::relocations::*;
use crate::writer::*;
use crate::*;
//use object::elf;
use object::elf::FileHeader64;
use object::read::elf;
use object::write::elf::{SectionIndex, Writer};
use object::write::StringId;
use object::ObjectSection;
use std::error::Error;

#[derive(Debug, Clone, Default)]
pub enum BlockSectionState {
    #[default]
    Start,
    Located,
}

#[derive(Debug)]
pub struct BlockSection {
    pub(crate) section: GeneralSection,
}

impl BlockSection {
    pub fn new(alloc: AllocSegment, name: &'static str) -> Self {
        Self {
            section: GeneralSection::new(alloc, name),
        }
    }

    pub fn align(&self) -> usize {
        self.section.alloc.align()
    }

    pub fn from_section<'a, 'b, A: elf::FileHeader, B: object::ReadRef<'a>>(
        &mut self,
        b: &elf::ElfFile<'a, A, B>,
        section: &elf::ElfSection<'a, 'b, A, B>,
    ) -> Result<(), Box<dyn Error>> {
        let data = section.uncompressed_data()?;
        let base_offset = self.section.size;
        self.section.extend_bytes(&data);
        for (offset, r) in section.relocations() {
            let r = code_relocation(b, r.into(), base_offset + offset as usize)?;
            self.section.relocations.push(r);
        }
        Ok(())
    }

    pub fn block_reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        self.section.block_reserve_section_index(data, w);
    }

    pub fn block_reserve(&mut self, data: &mut Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        self.section.block_reserve(data, tracker, w);
        /*
        let pos = w.reserved_len();
        let align_pos = size_align(pos, self.align());
        w.reserve_until(align_pos);
        self.file_offset = w.reserved_len();

        w.reserve(self.bytes.len(), self.align());
        let after = w.reserved_len();
        let delta = after - pos;

        self.base = tracker.add_data(self.alloc, delta, self.file_offset);
        self.addr = self.base + self.file_offset;
        data.addr_set(&self.name, self.addr as u64);

        log::debug!(
            "FO: {:#0x}, {}, {:?}, base: {:#0x}, addr: {:#0x}, delta: {:#0x}, size: {:#0x}",
            self.file_offset,
            self.name,
            self.alloc,
            self.base,
            self.addr,
            delta,
            self.bytes.len()
        );

        self.state = BlockSectionState::Located;
        */
    }

    pub fn block_write(&self, data: &Data, w: &mut Writer) {
        self.section.block_write(data, w);
        /*
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align());
        log::debug!(
            "AF: {:?}, {:#0x}, {:#0x}",
            self.alloc,
            aligned_pos,
            self.file_offset
        );
        assert_eq!(aligned_pos, self.file_offset);
        w.pad_until(aligned_pos);
        w.write(self.bytes.as_slice());
        */
    }

    pub fn block_write_section_header(&self, data: &Data, w: &mut Writer) {
        self.section.block_write_section_header(w);
        /*
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
        */
    }
}

#[derive(Debug)]
pub struct GeneralSection {
    pub(crate) alloc: AllocSegment,
    state: BlockSectionState,
    pub(crate) name: &'static str,
    name_id: Option<StringId>,
    pub(crate) file_offset: usize,
    pub(crate) base: usize,
    pub(crate) addr: usize,
    pub(crate) section_index: Option<SectionIndex>,
    pub(crate) size: usize,
    pub(crate) bytes: Vec<u8>,
    pub(crate) relocations: Vec<CodeRelocation>,
}

impl GeneralSection {
    pub fn new(alloc: AllocSegment, name: &'static str) -> Self {
        Self {
            state: BlockSectionState::default(),
            alloc,
            name,
            name_id: None,
            file_offset: 0,
            base: 0,
            addr: 0,
            section_index: None,
            size: 0,
            bytes: vec![],
            relocations: vec![],
        }
    }

    pub fn align(&self) -> usize {
        self.alloc.align()
    }

    pub fn extend_bytes(&mut self, bytes: &[u8]) {
        self.bytes.extend(bytes.iter());
        self.size += bytes.len();
    }

    pub fn extend_size(&mut self, size: usize) {
        self.size += size;
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

        w.reserve(self.bytes.len(), self.align());
        let after = w.reserved_len();
        let delta = after - pos;

        self.base = tracker.add_data(self.alloc, delta, self.file_offset);
        self.addr = self.base + self.file_offset;
        data.addr_set(&self.name, self.addr as u64);
        self.state = BlockSectionState::Located;

        log::debug!(
            "FO: {:#0x}, {}, {:?}, base: {:#0x}, addr: {:#0x}, size: {:#0x}",
            self.file_offset,
            self.name,
            self.alloc,
            self.base,
            self.addr,
            self.size
        );
    }

    pub fn block_write(&self, _data: &Data, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align());
        log::debug!(
            "AF: {:?}, {:#0x}, {:#0x}",
            self.alloc,
            aligned_pos,
            self.file_offset
        );
        assert_eq!(aligned_pos, self.file_offset);
        w.pad_until(aligned_pos);
        w.write(self.bytes.as_slice());
    }

    pub fn block_write_section_header(&self, w: &mut Writer) {
        if let Some(name_id) = self.name_id {
            w.write_section_header(&object::write::elf::SectionHeader {
                name: Some(name_id),
                sh_type: object::elf::SHT_PROGBITS,
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

#[derive(Debug)]
pub struct BssSection {
    pub(crate) relocations: Vec<CodeRelocation>,
    pub(crate) section: GeneralSection,
}

impl BssSection {
    pub fn new(alloc: AllocSegment, name: &'static str) -> Self {
        Self {
            section: GeneralSection::new(alloc, name),
            relocations: vec![],
        }
    }

    pub fn from_section<'a, 'b, A: elf::FileHeader, B: object::ReadRef<'a>>(
        &mut self,
        b: &elf::ElfFile<'a, A, B>,
        section: &elf::ElfSection<'a, 'b, A, B>,
    ) -> Result<(), Box<dyn Error>> {
        let data = section.uncompressed_data()?;
        let base_offset = self.section.size;
        self.section.extend_bytes(&data);
        for (offset, r) in section.relocations() {
            let r = code_relocation(b, r.into(), base_offset + offset as usize)?;
            self.relocations.push(r);
        }
        Ok(())
    }

    pub fn block_reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        self.section.block_reserve_section_index(data, w);
    }

    pub fn block_reserve(&mut self, data: &mut Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        self.section.block_reserve(data, tracker, w);

        /*
        for r in self.relocations.iter() {
            data.relocations_got.push(r.clone());
            data.relocations_gotplt.push(r.clone());
        }
        */
    }

    pub fn block_write(&self, data: &Data, w: &mut Writer) {
        self.section.block_write(data, w);
    }

    pub fn block_write_section_header(&self, _data: &Data, w: &mut Writer) {
        self.section.block_write_section_header(w);
    }
}
