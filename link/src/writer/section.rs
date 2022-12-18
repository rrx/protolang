use object::elf;
use object::write::elf::{SectionIndex, Sym, Writer};
use object::write::StringId;
use std::collections::{HashMap, HashSet};

use super::*;

#[derive(Debug, Clone)]
pub struct ProgSymbol {
    pub name_id: Option<StringId>,
    pub s: CodeSymbol,
}

impl ProgSymbol {
    pub fn new_object(name: &str, index: SectionIndex) -> Self {
        Self {
            name_id: None,
            s: CodeSymbol {
                name: name.to_string(),
                size: 0,
                address: 0,
                kind: CodeSymbolKind::Data,
                def: CodeSymbolDefinition::Defined,
                st_info: 0,
                st_other: 0,
            },
        }
    }

    pub fn reserve(&mut self, w: &mut Writer) {
        //self.name_id = Some(w.add_string(self.s.name.as_bytes()));//"_DYNAMIC_".as_bytes()));
        //w.reserve_symbol_index(data.index_dynamic);
    }

    pub fn get_symbol(&self, base: usize, index: Option<SectionIndex>) -> object::write::elf::Sym {
        let st_shndx = elf::SHN_ABS;
        let st_size = self.s.size;
        let addr = base as u64 + self.s.address;
        //eprintln!("write sym: {:?}, {:#0x}", &sym, addr);
        eprintln!("write symbol: {}, {:#0x}", &self.s.name, &addr);
        object::write::elf::Sym {
            name: self.name_id,
            section: index,
            st_info: self.s.st_info,
            st_other: self.s.st_other,
            st_shndx,
            st_value: addr,
            st_size,
        }
    }

    pub fn write_symbol(&self, base: usize, index: Option<SectionIndex>, w: &mut Writer) {
        let sym = self.get_symbol(base, index);
        w.write_symbol(&sym);
        /*
        let st_shndx = elf::SHN_ABS;
        let st_size = self.s.size;
        let addr = base as u64 + self.s.address;
        //eprintln!("write sym: {:?}, {:#0x}", &sym, addr);
        eprintln!("write symbol: {}, {:#0x}", &self.s.name, &addr);
        w.write_symbol(&object::write::elf::Sym {
            name: self.name_id,
            section: index,
            st_info: self.s.st_info,
            st_other: self.s.st_other,
            st_shndx,
            st_value: addr,
            st_size,
        });
        */
    }
}

pub struct ProgSectionBuilder {}

pub struct ProgSection {
    pub name: Option<String>,
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
    pub mem_size: usize,        // might be different than file size
    pub symbols: HashMap<String, ProgSymbol>,
    pub externs: HashMap<String, ProgSymbol>,
    pub relocations: Vec<CodeRelocation>,
    pub bytes: Vec<u8>,
}

impl ProgSection {
    pub fn new(
        kind: AllocSegment,
        name: Option<String>,
        name_id: Option<StringId>,
        rel_name_id: Option<StringId>,
        mem_size: usize,
    ) -> Self {
        Self {
            name,
            name_id,
            rel_name_id,
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
            externs: HashMap::new(),
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

    pub fn add_bytes(&mut self, bytes: &[u8]) {
        self.file_offset += bytes.len();
        self.mem_size += bytes.len();
        self.bytes.extend(bytes.to_vec());
    }

    pub fn append<'a>(&mut self, unlinked: &'a UnlinkedCodeSegment, w: &mut Writer<'a>) {
        self.bytes.extend(unlinked.bytes.clone());
        for r in &unlinked.relocations {
            let mut r = r.clone();
            eprintln!("relocation before: {}", &r);
            r.offset += self.data_count as u64;
            eprintln!("relocation after: {}", &r);
            self.relocations.push(r.clone());
        }

        for (name, symbol) in unlinked.externs.iter() {
            let name_id = Some(w.add_string(name.as_bytes()));
            let mut symbol = symbol.clone();
            symbol.address += self.base as u64 + self.addr as u64 + self.data_count as u64;
            let ps = ProgSymbol { name_id, s: symbol };
            eprintln!("symbol extern: {}, {:#0x}", &name, &ps.s.address);
            self.externs.insert(name.clone(), ps);
        }

        for (name, symbol) in unlinked.defined.iter() {
            let name_id = Some(w.add_string(name.as_bytes()));
            let mut symbol = symbol.clone();
            symbol.address += self.base as u64 + self.addr as u64 + self.data_count as u64;
            let ps = ProgSymbol { name_id, s: symbol };
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
        for (_, _) in &self.symbols {
            w.reserve_symbol_index(self.index);
        }
    }

    //
    pub fn get_symbols(&self, base: u64) -> Vec<Sym> {
        self.symbols
            .iter()
            .map(|(_, s)| s.get_symbol(self.base + base as usize, self.index))
            .collect()
    }

    pub fn write_symbols(&self, base: u64, w: &mut Writer) {
        // write symbols out
        for (name, sym) in &self.symbols {
            sym.write_symbol(self.base + base as usize, self.index, w);

            /*
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
            */
        }
    }

    pub fn unapplied_relocations(
        &self,
        symbols: &HashMap<String, ProgSymbol>,
        externs: &HashMap<String, ProgSymbol>,
    ) -> Vec<(ProgSymbol, CodeRelocation)> {
        let mut unapplied = vec![];
        for r in self.relocations.iter() {
            if let Some(symbol) = externs.get(&r.name) {
                if !symbols.contains_key(&r.name) && externs.contains_key(&r.name) {
                    unapplied.push((symbol.clone(), r.clone()));
                }
            }
        }
        unapplied
    }

    pub fn apply_relocations(
        &self,
        v_base: usize,
        pointers: &HashMap<String, u64>,
    ) -> Vec<CodeRelocation> {
        let patch_base = self.bytes.as_ptr();
        let mut unapplied = vec![];
        for r in self.relocations.iter() {
            if let Some(addr) = pointers.get(&r.name) {
                log::debug!(
                    "R-{:?}: vbase: {:#0x}, addr: {:#0x}, {}",
                    self.alloc().unwrap(),
                    v_base,
                    *addr as usize,
                    &r.name
                );
                r.patch(patch_base as *mut u8, v_base as *mut u8, *addr as *const u8);
            } else {
                //eprintln!("unapplied: {}", &r);
                //unapplied.push(r.clone());
                //unreachable!();
                unreachable!("Unable to locate symbol: {}, {}", &r.name, &r);
            }
        }
        disassemble_code(self.bytes.as_slice(), im::HashMap::new());
        unapplied
    }

    pub fn reserve_relocations(&mut self, w: &mut Writer) {
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

    fn reserve_section_index(&mut self, _data: &mut Data, w: &mut Writer) {
        self.index = Some(w.reserve_section_index());
    }

    fn reserve(&mut self, _data: &mut Data, tracker: &mut SegmentTracker, w: &mut Writer) {
        let pos = w.reserved_len();
        let align_pos = size_align(pos, self.kind.align());
        w.reserve_until(align_pos);
        let start = w.reserved_len();

        w.reserve(self.bytes.len(), self.kind.align());
        let after = w.reserved_len();
        self.file_offset = start;

        // this will be updated later when segments are aligned
        let delta = after - pos;

        self.base = tracker.add_data(self.alloc().unwrap(), delta, self.file_offset);
        self.addr = self.base + self.file_offset;
        eprintln!(
            "reserve: {:?}, {:#0x}/{:#0x}/{:#0x}",
            self.alloc(),
            self.addr,
            self.file_offset,
            self.bytes.len()
        );
    }

    fn update(&mut self, _data: &mut Data) {}

    fn write(&self, _data: &Data, _tracker: &mut SegmentTracker, w: &mut Writer) {
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

    fn write_section_header(&self, _data: &Data, _tracker: &SegmentTracker, w: &mut Writer) {
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
