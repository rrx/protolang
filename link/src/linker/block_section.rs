use crate::writer::*;
use crate::*;
use object::read::elf;
use object::write::elf::{SectionIndex, Writer};
use object::write::StringId;
use object::ObjectSection;
use std::error::Error;
//use object::ObjectSymbol;

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

    //pub fn align(&self) -> usize {
    //self.section.alloc.align()
    //}

    pub fn from_section<'a, 'b, A: elf::FileHeader, B: object::ReadRef<'a>>(
        &mut self,
        b: &elf::ElfFile<'a, A, B>,
        section: &elf::ElfSection<'a, 'b, A, B>,
    ) -> Result<(), Box<dyn Error>> {
        let data = section.uncompressed_data()?;
        let base_offset = self.section.size;
        log::debug!("name: {}", section.name()?);
        //eprintln!("before: {:#0x}", self.section.bytes.len());
        self.section.extend_bytes(&data);

        //eprintln!("after: {:#0x}", self.section.bytes.len());
        for (offset, r) in section.relocations() {
            let r = code_relocation(b, r.into(), base_offset + offset as usize)?;
            self.section.relocations.push(r);
        }
        Ok(())
    }

    pub fn block_reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        self.section.block_reserve_section_index(data, w);
    }

    pub fn block_reserve(&mut self, data: &mut Data, w: &mut Writer) {
        self.section.block_reserve(data, w);
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
    state: BlockSectionState,
    pub(crate) name: &'static str,
    name_id: Option<StringId>,
    pub(crate) section_index: Option<SectionIndex>,
    pub(crate) size: usize,
    pub(crate) bytes: Vec<u8>,
    pub(crate) relocations: Vec<CodeRelocation>,
    pub(crate) offsets: SectionOffset,
}

fn resolve_r(data: &Data, r: &CodeRelocation) -> Option<u64> {
    //eprintln!("resolve: {}, kind: {:?}", &r.name, r.r.kind());

    // check if it's in the plt or got, and look it up in dynamics
    //if r.is_plt() || r.is_got() {
    if let Some(resolve_addr) = data.dynamics.lookup(r) {
        return resolve_addr.resolve(data);
    }
    //}

    // otherwise, just look up the symbol
    if let Some(resolve_addr) = data.pointers.get(&r.name) {
        resolve_addr.resolve(data)
    } else {
        None
    }
}

impl GeneralSection {
    pub fn new(alloc: AllocSegment, name: &'static str) -> Self {
        Self {
            state: BlockSectionState::default(),
            name,
            name_id: None,
            section_index: None,
            size: 0,
            bytes: vec![],
            relocations: vec![],
            offsets: SectionOffset::new(alloc, 0x10),
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
        //eprintln!("symbols: {:?}", data.dynamics.symbols());
        //eprintln!("plt: {:?}", data.dynamics.plt_hash);
        //eprintln!("pltgot: {:?}", data.dynamics.pltgot_hash);
        for r in self.relocations.iter() {
            if let Some(addr) = resolve_r(data, r) {
                log::info!(
                    target: "relocations",
                    "R-{:?}: vbase: {:#0x}, addr: {:#0x}, {}",
                    self.offsets.alloc, self.offsets.address, addr as usize, &r.name
                );
                r.patch(
                    patch_base as *mut u8,
                    self.offsets.address as *mut u8,
                    addr as *const u8,
                );
            } else {
                unreachable!("Unable to locate symbol: {}, {}", &r.name, &r);
            }
        }

        if data.debug_enabled(&DebugFlag::Disassemble) {
            self.disassemble(data);
        }
    }

    pub fn block_reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        self.name_id = Some(w.add_section_name(self.name.as_bytes()));
        let index = w.reserve_section_index();
        self.section_index = Some(index);
        //eprintln!("section index set: {}, {:?}", self.name, index);
        data.section_index_set(&self.name, index);
    }

    pub fn block_reserve(&mut self, data: &mut Data, w: &mut Writer) {
        let align = self.offsets.align as usize;
        let pos = w.reserved_len();
        let align_pos = size_align(pos, align);
        w.reserve_until(align_pos);
        let file_offset = w.reserved_len();

        w.reserve(self.bytes.len(), 1);
        let after = w.reserved_len();

        log::debug!("align: {:#0x}, fileoffset: {:#0x}", align, file_offset);
        data.segments.add_offsets(
            self.offsets.alloc,
            &mut self.offsets,
            after - file_offset,
            w,
        );
        data.addr_set(&self.name, self.offsets.address);
        self.state = BlockSectionState::Located;

        log::debug!(
            "FO: {:#0x}, {}, {:?}, base: {:#0x}, addr: {:#0x}, size: {:#0x}, align: {:#0x}",
            self.offsets.file_offset,
            self.name,
            self.offsets.alloc,
            self.offsets.base,
            self.offsets.address,
            self.offsets.size,
            align,
        );
    }

    pub fn block_write(&self, data: &Data, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.offsets.align as usize);
        log::debug!(
            "AF: {:?}, {:#0x}, {:#0x}",
            self.offsets.alloc,
            aligned_pos,
            self.offsets.file_offset
        );
        assert_eq!(aligned_pos, self.offsets.file_offset as usize);
        w.pad_until(aligned_pos);

        self.apply_relocations(data);

        w.write(self.bytes.as_slice());
    }

    pub fn block_write_section_header(&self, w: &mut Writer) {
        if let Some(name_id) = self.name_id {
            w.write_section_header(&object::write::elf::SectionHeader {
                name: Some(name_id),
                sh_type: object::elf::SHT_PROGBITS,
                sh_flags: self.offsets.alloc.section_header_flags() as u64,
                sh_addr: self.offsets.address,
                sh_offset: self.offsets.file_offset,
                sh_info: 0,
                sh_link: 0,
                sh_entsize: 0,
                sh_addralign: self.offsets.align,
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

    pub fn block_reserve(&mut self, data: &mut Data, w: &mut Writer) {
        self.section.block_reserve(data, w);
    }

    pub fn block_write(&self, data: &Data, w: &mut Writer) {
        self.section.block_write(data, w);
    }

    pub fn block_write_section_header(&self, _data: &Data, w: &mut Writer) {
        self.section.block_write_section_header(w);
    }
}
