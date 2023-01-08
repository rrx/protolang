use crate::writer::*;
use crate::*;
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
    }

    pub fn block_write(&self, data: &Data, w: &mut Writer) {
        self.section.block_write(data, w);
    }

    pub fn block_write_section_header(&self, _data: &Data, w: &mut Writer) {
        self.section.block_write_section_header(w);
    }
}

#[derive(Debug)]
pub struct GeneralSection {
    pub(crate) alloc: AllocSegment,
    state: BlockSectionState,
    pub(crate) name: &'static str,
    name_id: Option<StringId>,
    pub(crate) section_index: Option<SectionIndex>,
    pub(crate) size: usize,
    pub(crate) bytes: Vec<u8>,
    pub(crate) relocations: Vec<CodeRelocation>,
    pub(crate) offsets: SectionOffset,
}

impl GeneralSection {
    pub fn new(alloc: AllocSegment, name: &'static str) -> Self {
        Self {
            state: BlockSectionState::default(),
            alloc,
            name,
            name_id: None,
            section_index: None,
            size: 0,
            bytes: vec![],
            relocations: vec![],
            offsets: SectionOffset::new(0x10),
        }
    }

    pub fn extend_bytes(&mut self, bytes: &[u8]) {
        self.bytes.extend(bytes.iter());
        self.size += bytes.len();
    }

    pub fn extend_size(&mut self, size: usize) {
        self.size += size;
    }

    pub fn apply_relocations(&self, data: &Data) {
        let patch_base = self.bytes.as_ptr();
        for r in self.relocations.iter() {
            if let Some(resolve_addr) = data.pointers.get(&r.name) {
                if let Some(addr) = resolve_addr.resolve(data) {
                    eprintln!(
                        "R-{:?}: vbase: {:#0x}, addr: {:#0x}, {}",
                        self.alloc, self.offsets.address, addr as usize, &r.name
                    );
                    r.patch(
                        patch_base as *mut u8,
                        self.offsets.address as *mut u8,
                        addr as *const u8,
                    );
                } else {
                    unreachable!("Unable to resolve symbol: {}, {:?}", &r.name, &resolve_addr);
                }
            } else {
                unreachable!("Unable to locate symbol: {}, {}", &r.name, &r);
            }
        }
        self.disassemble(data);
    }

    pub fn block_reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        self.name_id = Some(w.add_section_name(self.name.as_bytes()));
        let index = w.reserve_section_index();
        self.section_index = Some(index);
        //eprintln!("section index set: {}, {:?}", self.name, index);
        data.section_index_set(&self.name, index);
    }

    pub fn block_reserve(&mut self, data: &mut Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        //let align = self.align();
        let align = self.offsets.align as usize;
        let pos = w.reserved_len();
        let align_pos = size_align(pos, align);
        w.reserve_until(align_pos);
        let file_offset = w.reserved_len();

        w.reserve(self.bytes.len(), 1); //align);
        let after = w.reserved_len();
        //let delta = after - pos;

        eprintln!("align: {:#0x}, fileoffset: {:#0x}", align, file_offset);
        tracker.add_offsets(self.alloc, &mut self.offsets, after - file_offset, w);
        data.addr_set(&self.name, self.offsets.address);
        self.state = BlockSectionState::Located;

        eprintln!(
            "FO: {:#0x}, {}, {:?}, base: {:#0x}, addr: {:#0x}, size: {:#0x}, align: {:#0x}",
            self.offsets.file_offset,
            self.name,
            self.alloc,
            self.offsets.base,
            self.offsets.address,
            self.offsets.size,
            align,
        );
    }

    pub fn block_write(&self, data: &Data, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.offsets.align as usize);
        eprintln!(
            "AF: {:?}, {:#0x}, {:#0x}",
            self.alloc,
            aligned_pos,
            self.offsets.file_offset
        );
        assert_eq!(aligned_pos, self.offsets.file_offset as usize);
        w.pad_until(aligned_pos);

        self.apply_relocations(data);

        w.write(self.bytes.as_slice());
        /*
        for r in self.relocations.iter() {
            eprintln!("r: {}", r);
            let p = data.pointers.get(&r.name).unwrap();
            eprintln!("s: {:#0x}", p.resolve(data).unwrap());
            //if let Some(p) = data.pointers.get(&r.name) {
            //eprintln!("s: {:?}", p.resolve(data));
            //}
        }
        */
    }

    pub fn block_write_section_header(&self, w: &mut Writer) {
        if let Some(name_id) = self.name_id {
            w.write_section_header(&object::write::elf::SectionHeader {
                name: Some(name_id),
                sh_type: object::elf::SHT_PROGBITS,
                sh_flags: self.alloc.section_header_flags() as u64,
                sh_addr: self.offsets.address, //addr as u64,
                sh_offset: self.offsets.file_offset,
                sh_info: 0,
                sh_link: 0,
                sh_entsize: 0,
                sh_addralign: self.offsets.align, //alloc.align() as u64,
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
    }

    pub fn block_write(&self, data: &Data, w: &mut Writer) {
        self.section.block_write(data, w);
    }

    pub fn block_write_section_header(&self, _data: &Data, w: &mut Writer) {
        self.section.block_write_section_header(w);
    }
}
