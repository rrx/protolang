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
    pub name_id: Option<StringId>,
    pub index: Option<SectionIndex>,
    pub kind: AllocSegment,
    pub addr: usize,
    pub data_count: usize,
    pub file_offset: usize, // file offset
    pub mem_size: u64,      // might be different than file size
    pub symbols: HashMap<String, ProgSymbol>,
    pub relocations: Vec<CodeRelocation>,
    pub bytes: Vec<u8>,
    pub align: usize,
}

impl ProgSection {
    pub fn new(kind: AllocSegment, name: Option<String>, align: usize, mem_size: u64) -> Self {
        Self {
            name,
            name_id: None,
            index: None,
            kind,
            addr: 0,
            file_offset: 0,
            mem_size,
            data_count: 0,
            symbols: HashMap::new(),
            relocations: vec![],
            bytes: vec![],
            align,
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
            symbol.address += self.data_count as u64;
            let is_start = name == "_start";
            let ps = ProgSymbol {
                name_id,
                is_start,
                s: symbol,
            };
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
    //
    pub fn write_symbols(&self, base: u64, w: &mut Writer) {
        // write symbols out
        for (_, sym) in &self.symbols {
            let st_shndx = elf::SHN_ABS;
            let st_size = sym.s.size;
            let addr = sym.s.address + base;
            //eprintln!("write sym: {:?}, {:#0x}", &sym, addr);
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
}

impl ElfBlock for ProgSection {
    fn reserve_section_index(&mut self, data: &mut Data, w: &mut Writer) {
        self.index = Some(w.reserve_section_index());

        // reserve the symbols in the various sections
        for sym in &self.symbols {
            w.reserve_symbol_index(self.index);
        }
    }

    fn reserve(&mut self, data: &mut Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        let pos = w.reserved_len();
        let align_pos = size_align(pos, self.align);
        w.reserve_until(align_pos);
        let start = w.reserved_len();

        w.reserve(self.bytes.len(), self.align);
        let after = w.reserved_len();
        //self.size = self.bytes.len();
        self.file_offset = start;

        // this will be updated later when segments are aligned
        self.addr = start;

        let delta = after - pos;

        if self.kind == AllocSegment::Interp {
            data.addr_interp = start as u64;
        }
    }

    fn update(&mut self, data: &mut Data) {}

    fn write(&self, data: &Data, ph: &Vec<ProgramHeaderEntry>, w: &mut Writer) {
        let pos = w.len();
        let aligned_pos = size_align(pos, self.align);
        w.pad_until(aligned_pos);
        w.write(self.bytes.as_slice());
    }

    fn write_section_header(&self, data: &Data, w: &mut Writer) {
        if let Some(name_id) = self.name_id {
            w.write_section_header(&object::write::elf::SectionHeader {
                name: Some(name_id),
                sh_type: elf::SHT_PROGBITS,
                sh_flags: self.kind.flags() as u64,
                sh_addr: self.addr as u64,
                sh_offset: self.file_offset as u64,
                sh_info: 0,
                sh_link: 0,
                sh_entsize: 0,
                sh_addralign: self.align as u64,
                sh_size: self.size() as u64,
            });
        }
    }
}
