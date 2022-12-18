// read elf file
use object::elf::FileHeader64;
use object::read::elf;
use object::read::elf::ProgramHeader;
use object::{Object, ObjectSection, ObjectSymbol, Relocation, SectionKind};
use std::collections::HashMap;
use std::error::Error;

use binary_heap_plus::*;
use capstone::prelude::*;

struct Symbol<'a> {
    section_addr: u64,
    addr: u64,
    name: &'a str,
}
impl<'a> Symbol<'a> {
    fn new(section_addr: u64, addr: u64, name: &'a str) -> Self {
        Self {
            section_addr,
            addr,
            name,
        }
    }
}

#[derive(Debug)]
struct Reloc {
    offset: u64,
    r: Relocation,
}

fn disassemble_code(buf: &[u8], symbols: Vec<Symbol>, relocations: Vec<Reloc>) {
    let mut heap = BinaryHeap::from_vec_cmp(symbols, |a: &Symbol, b: &Symbol| b.addr.cmp(&a.addr));
    let mut r_heap =
        BinaryHeap::from_vec_cmp(relocations, |a: &Reloc, b: &Reloc| b.offset.cmp(&a.offset));
    //let mut dr_heap = BinaryHeap::from_vec_cmp(dynamic_relocations, |a: &Reloc, b: &Reloc| b.offset.cmp(&a.offset));
    let cs = capstone::Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode64)
        .syntax(arch::x86::ArchSyntax::Att)
        .detail(true)
        .build()
        .unwrap();
    let insts = cs.disasm_all(&buf, 0).expect("disassemble");

    //while r_heap.len() > 0 {
    //let r = r_heap.pop();
    //eprintln!("R: {:?}", r);
    //}

    //while dr_heap.len() > 0 {
    //let r = dr_heap.pop();
    //eprintln!("DR: {:?}", r);
    //}

    for instr in insts.as_ref() {
        let addr = instr.address();

        if heap.len() > 0 {
            let next_symbol_addr = heap.peek().unwrap().addr;

            if next_symbol_addr <= addr {
                let symbol = heap.pop().unwrap();
                eprintln!(
                    "{}: {:#0x} {:#0x}",
                    symbol.name,
                    symbol.addr,
                    symbol.section_addr + symbol.addr
                );
            }
        }

        eprintln!(
            "  {:#06x} {}\t\t{}",
            instr.address(),
            instr.mnemonic().expect("no mnmemonic found"),
            instr.op_str().expect("no op_str found")
        );

        if r_heap.len() > 0 {
            let next_reloc_addr = r_heap.peek().unwrap().offset;
            if next_reloc_addr <= addr {
                let r = r_heap.pop().unwrap();
                eprintln!("    Relocation: {:#0x} {:?}", r.offset, r.r);
            }
        }
    }
}

pub fn elf_read(buf: &[u8]) -> Result<(), Box<dyn Error>> {
    let b: elf::ElfFile<'_, FileHeader64<object::Endianness>> =
        object::read::elf::ElfFile::parse(buf)?;

    let endian = b.endian();

    let h = b.raw_header();
    eprintln!("{:?}", h);
    eprintln!("e_entry: {:#0x}", h.e_entry.get(endian));
    eprintln!("e_phoff: {:#0x}", h.e_phoff.get(endian));
    eprintln!("e_phnum: {:#0x}", h.e_phnum.get(endian));
    for seg in b.raw_segments() {
        eprintln!("Segment");
        eprintln!("  p_type:   {:#0x}", seg.p_type(endian));
        eprintln!("  p_flags {:#0x}", seg.p_flags(endian));
        eprintln!("  p_offset {:#0x}", seg.p_offset(endian));
        eprintln!("  p_vaddr {:#0x}", seg.p_vaddr(endian));
        eprintln!("  p_paddr {:#0x}", seg.p_paddr(endian));
        eprintln!("  p_filesz: {:#0x}", seg.p_filesz(endian));
        eprintln!("  p_memsz:  {:#0x}", seg.p_memsz(endian));
        eprintln!("  p_align {:#0x}", seg.p_align(endian));
        let offset = seg.p_offset(endian) as usize;
        let size = seg.p_filesz(endian) as usize;
        //let data = &b.data()[offset..offset + size];
        //disassemble_code(data);
    }

    for section in b.sections() {
        let name = section.name()?;
        let section_addr = section.address();
        eprintln!("Section: {}", name);
        eprintln!("  kind:   {:?}", section.kind());
        eprintln!("  addr:   {:#0x}", section_addr);

        let mut symbols = vec![];
        let mut relocations = vec![];

        for (r_offset, r) in section.relocations() {
            relocations.push(Reloc {
                offset: r_offset,
                r,
            });
        }

        for symbol in b.symbols() {
            if let Some(index) = symbol.section_index() {
                if index == section.index() {
                    if section_addr <= symbol.address() {
                        let addr = symbol.address() - section_addr;
                        let name = symbol.name()?;
                        symbols.push(Symbol::new(section_addr, addr, name));
                    }
                }
            }
        }

        let buf = section.data()?;
        if name == ".got" {
            for (offset, r) in b.dynamic_relocations().unwrap() {
                //eprintln!("DR: {:#0x} {:?}", offset, r);
                relocations.push(Reloc {
                    offset: offset - section_addr,
                    r,
                });
            }
            disassemble_code(buf, symbols, relocations);
        } else {
            disassemble_code(buf, symbols, relocations);
        }
    }

    /*
    for seg in b.segments() {
        eprintln!("Segment: {:?}", seg.name()?);
        eprintln!("  flags: {:?}", seg.flags());
        eprintln!("  addr:  {:#0x}", seg.address());
        eprintln!("  size:  {:#0x}", seg.size());
        eprintln!("  align:  {:#0x}", seg.align());
    }
    */
    //.program_headers()?;
    //b.raw_header().e_ident;
    //let obj_file = object::File::parse(buf)?;
    //obj_file.format()
    //let mut symbols = HashMap::new();
    //let mut symbols_by_id = HashMap::new();
    //let mut segments = vec![];
    //let mut externs = HashSet::new();
    //let mut internal = im::HashMap::new();
    Ok(())
}
