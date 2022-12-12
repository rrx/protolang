use std::error::Error;

use object::elf;
use object::read::elf::FileHeader;
use object::write::elf::{SectionIndex, Writer};
use object::write::StringId;
use object::Endianness;
use std::collections::HashMap;

use super::*;

#[derive(Debug)]
pub struct ProgSymbol {
    pub name_id: Option<StringId>,
    pub is_start: bool,
    pub s: CodeSymbol,
}

pub struct ProgSectionBuilder {}

pub struct ProgSection {
    pub name: Option<String>,
    pub rel_name: Option<String>,
    pub rel_name_bytes: Vec<u8>,
    pub name_id: Option<StringId>,
    pub rel_name_id: Option<StringId>,
    pub index: Option<SectionIndex>,
    pub rel_index: Option<SectionIndex>,
    pub kind: AllocSegment,
    pub base: usize,
    pub addr: usize,
    pub data_count: usize,
    pub file_offset: usize,     // file offset
    pub rel_file_offset: usize, // file offset
    pub mem_size: u64,          // might be different than file size
    pub symbols: HashMap<String, ProgSymbol>,
    pub relocations: Vec<CodeRelocation>,
    pub bytes: Vec<u8>,
    //pub align: usize,
}

impl ProgSection {
    pub fn new(kind: AllocSegment, name: Option<String>, mem_size: u64) -> Self {
        let mut rel_name = None;
        let mut rel_name_bytes = vec![];
        if let Some(n) = &name {
            let formatted = format!(".rela{}", n.clone());
            rel_name_bytes = formatted.as_bytes().to_vec();
            rel_name = Some(formatted);
        }
        Self {
            name,
            rel_name,
            rel_name_bytes,
            name_id: None,
            rel_name_id: None,
            index: None,
            rel_index: None,
            kind,
            addr: 0,
            base: 0,
            file_offset: 0,
            rel_file_offset: 0,
            mem_size,
            data_count: 0,
            symbols: HashMap::new(),
            relocations: vec![],
            bytes: vec![],
        }
    }

    pub fn size(&self) -> usize {
        if self.bytes.len() == 0 {
            self.mem_size as usize
        } else {
            self.bytes.len()
        }
    }

    pub fn update_segment_base(&mut self, base: usize) {
        self.addr += base;
    }

    pub fn symbol_pointers(&self) -> HashMap<String, u64> {
        let mut out = HashMap::new();
        for (name, s) in &self.symbols {
            let addr = self.base + s.s.address as usize;
            out.insert(name.clone(), addr as u64);
        }
        out
    }

    pub fn append<'a>(&mut self, unlinked: &'a UnlinkedCodeSegment, w: &mut Writer<'a>) {
        self.bytes.extend(unlinked.bytes.clone());
        for r in &unlinked.relocations {
            let mut r = r.clone();
            r.offset += self.data_count as u64;
            self.relocations.push(r.clone());
        }

        for (name, symbol) in unlinked.defined.iter() {
            let name_id = Some(w.add_string(name.as_bytes()));
            let mut symbol = symbol.clone();
            symbol.address += self.base as u64 + self.addr as u64 + self.data_count as u64;
            let is_start = name == "_start";
            let ps = ProgSymbol {
                name_id,
                is_start,
                s: symbol,
            };
            eprintln!("symbol: {}, {:#0x}", &name, &ps.s.address);
            self.symbols.insert(name.clone(), ps);
        }
        self.data_count += unlinked.bytes.len();
    }

    pub fn disassemble_code(&self) {
        let buf = &self.bytes.as_slice()[0..self.size()];
        use capstone::prelude::*;
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

    pub fn reserve_symbols(&self, w: &mut Writer) {
        for (_, sym) in &self.symbols {
            w.reserve_symbol_index(self.index);
        }
    }

    //
    pub fn write_symbols(&self, base: u64, w: &mut Writer) {
        // write symbols out
        for (name, sym) in &self.symbols {
            let st_shndx = elf::SHN_ABS;
            let st_size = sym.s.size;
            let addr = self.base as u64 + sym.s.address + base;
            //eprintln!("write sym: {:?}, {:#0x}", &sym, addr);
            eprintln!("write symbol: {}, {:#0x}", &name, &addr);
            w.write_symbol(&object::write::elf::Sym {
                name: sym.name_id,
                section: self.index,
                st_info: sym.s.st_info,
                st_other: sym.s.st_other,
                st_shndx,
                st_value: addr,
                st_size,
            });
        }
    }

    pub fn apply_relocations(&self, v_base: usize, pointers: &HashMap<String, u64>) {
        let patch_base = self.bytes.as_ptr();
        for r in self.relocations.iter() {
            let addr = *pointers.get(&r.name).unwrap();
            log::debug!(
                "R-{:?}: vbase: {:#0x}, addr: {:#0x}, {}",
                self.alloc(),
                v_base,
                addr as usize,
                &r.name
            );
            r.patch(patch_base as *mut u8, v_base as *mut u8, addr as *const u8);
        }
    }

    pub fn reserve_relocations<'a>(&mut self, w: &mut Writer<'a>) {
        //let label = format!(".rela{}", self.name.clone().unwrap());
        //let buf = label.as_bytes();
        //let buf = self.rel_name.unwrap().as_bytes();
        //self.rel_name_id = Some(w.add_section_name(self.rel_name_bytes.as_slice()));
        self.rel_index = Some(w.reserve_section_index());
        self.rel_file_offset = w.reserve_relocations(self.relocations.len(), true);
    }

    pub fn write_relocations(&self, w: &mut Writer) {
        for rel in self.relocations.iter() {
            let r_offset = rel.offset;
            let r_addend = rel.r.addend;
            let r_sym = 0;
            let r_type = 0;
            w.write_relocation(
                true,
                &object::write::elf::Rel {
                    r_offset,
                    r_sym,
                    r_type,
                    r_addend,
                },
            );
        }
    }

    pub fn write_relocation_section_headers(&self, w: &mut Writer, index_symtab: SectionIndex) {
        if false {
            w.write_relocation_section_header(
                self.rel_name_id.unwrap(),
                self.rel_index.unwrap(),
                index_symtab,
                self.rel_file_offset,
                self.relocations.len(),
                true,
            );
        }
    }
}

impl ElfBlock for ProgSection {
    fn alloc(&self) -> Option<AllocSegment> {
        Some(self.kind)
    }
}
/*

    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        self.index = Some(w.reserve_section_index());
        // reserve the symbols in the various sections
        //for sym in &self.symbols {
        //w.reserve_symbol_index(self.index);
        //}
    }

    fn reserve(&mut self, data: &mut Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        let pos = w.reserved_len();
        let align_pos = size_align(pos, self.kind.align());
        w.reserve_until(align_pos);
        let start = w.reserved_len();

        w.reserve(self.bytes.len(), self.kind.align());
        let after = w.reserved_len();
        self.file_offset = start;

        // this will be updated later when segments are aligned
        let delta = after - pos;

        self.base = tracker
            .add_data(self.alloc().unwrap(), delta, self.file_offset);
        self.addr = self.base + self.file_offset;
        eprintln!(
            "reserve: {:?}, {:#0x}/{:#0x}/{:#0x}",
            self.alloc(),
            self.addr,
            self.file_offset,
            self.bytes.len()
        );
    }

    fn update(&mut self, data: &mut Data) {}

    fn write(&self, data: &Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.kind.align());
        w.pad_until(aligned_pos);
        let before = w.len();
        w.write(self.bytes.as_slice());
        let after = w.len();
        eprintln!(
            "write: {:?}, {:#0x}/{:#0x}/{:#0x}",
            self.alloc(),
            before,
            after,
            self.bytes.len()
        );
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        if let Some(name_id) = self.name_id {
            w.write_section_header(&object::write::elf::SectionHeader {
                name: Some(name_id),
                sh_type: elf::SHT_PROGBITS,
                sh_flags: self.kind.section_header_flags() as u64,
                sh_addr: self.addr as u64,
                sh_offset: self.file_offset as u64,
                sh_info: 0,
                sh_link: 0,
                sh_entsize: 0,
                sh_addralign: self.kind.align() as u64,
                sh_size: self.size() as u64,
            });
        }
    }
}
*/
