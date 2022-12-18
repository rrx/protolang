use super::*;
use binary_heap_plus::*;
use capstone::prelude::*;
use object::Relocation;
use std::collections::HashMap;

impl PatchBlock {
    pub fn disassemble(&self) {
        match self.kind {
            PatchBlockKind::Code => {
                let base = self.block.as_ptr() as u64;
                eprintln!(
                    "Code Block Disassemble: Base: {:#08x}, Name: {}",
                    base, &self.name
                );
                let mut pointers = HashMap::new();
                for (name, ptr) in &self.symbols {
                    eprintln!(" {:#08x}: {}", ptr.as_ptr() as usize, name);
                    pointers.insert(name.clone(), ptr.as_ptr() as u64 - base);
                }
                for r in &self.relocations {
                    eprintln!(" {}", &r);
                }
                let size = self.block.size;
                let buf = &self.block.as_slice()[0..size];
                disassemble_code(buf, &pointers);
            }

            PatchBlockKind::Data | PatchBlockKind::DataRO => {
                let _base = self.block.as_ptr() as usize;
                eprint_bytes(&self.block.as_slice()[0..self.block.size]);
                let mut pointers = im::HashMap::new();
                let base = self.block.as_ptr() as usize;
                eprintln!(
                    "Data Block Disassemble: Base: {:#08x}, Name: {}",
                    base, &self.name
                );
                eprintln!("data_rw@{:#08x}+{:#x}", base, self.block.size);
                for (name, ptr_ref) in &self.symbols {
                    let ptr = ptr_ref.as_ptr();
                    pointers.insert(ptr as usize - base, name.clone());
                    unsafe {
                        let value = std::ptr::read(ptr as *const u64);
                        eprintln!(
                            " {:#08x}, Offset: {:#08x}, Value: {:#08x} {}",
                            ptr as usize,
                            ptr as usize - base,
                            value,
                            &name
                        )
                    }
                }
                let size = self.block.size;
                let buf = &self.block.as_slice()[0..size];
                println!(" buf: {:?}", buf);
            }
        }
    }
}

fn eprint_bytes(buf: &[u8]) {
    let x = String::from_utf8(
        buf.iter()
            .flat_map(|b| std::ascii::escape_default(*b))
            .collect::<Vec<u8>>(),
    )
    .unwrap();
    eprintln!("{}", x);
}

pub fn disassemble_buf(buf: &[u8]) {
    let pointers = HashMap::new();
    disassemble_code(buf, &pointers);
}

pub fn disassemble_code(buf: &[u8], pointers: &HashMap<String, u64>) {
    let mut hash = HashMap::new();
    for (name, p) in pointers.iter() {
        hash.insert(p, name);
    }

    // disassemble the code we are generating
    let cs = capstone::Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode64)
        .syntax(arch::x86::ArchSyntax::Att)
        .detail(true)
        .build()
        .unwrap();
    let insts = cs.disasm_all(&buf, 0).expect("disassemble");
    let mut last_name = None;
    for instr in insts.as_ref() {
        let addr = instr.address();
        if let Some(v) = hash.get(&addr) {
            let display_symbol = if let Some(name) = last_name {
                if name != v {
                    //last_name = Some(v);
                    Some(v)
                } else {
                    None
                }
            } else {
                Some(v)
            };

            if let Some(_) = display_symbol {
                println!("fn {}: {:#06x}", v, &addr);
            }
            last_name = Some(v);
        }

        unsafe {
            println!(
                "  {:#08x} {:#06x} {}\t\t{}",
                buf.as_ptr().offset(addr as isize) as usize,
                &addr,
                instr.mnemonic().expect("no mnmemonic found"),
                instr.op_str().expect("no op_str found")
            );
        }
    }
}

#[derive(Debug, Clone)]
pub struct Symbol<'a> {
    section_addr: u64,
    addr: u64,
    name: &'a str,
}
impl<'a> Symbol<'a> {
    pub fn new(section_addr: u64, addr: u64, name: &'a str) -> Self {
        Self {
            section_addr,
            addr,
            name,
        }
    }
}

#[derive(Debug)]
pub struct Reloc {
    offset: u64,
    r: Relocation,
}
impl Reloc {
    pub fn new(offset: u64, r: Relocation) -> Self {
        Self { offset, r }
    }
}

pub fn disassemble_code_with_symbols(
    buf: &[u8],
    symbols: &Vec<Symbol>,
    relocations: &Vec<CodeRelocation>,
) {
    let mut heap = BinaryHeap::from_vec_cmp(symbols.clone(), |a: &Symbol, b: &Symbol| {
        b.addr.cmp(&a.addr)
    });
    let mut r_heap = BinaryHeap::from_vec_cmp(
        relocations.clone(),
        |a: &CodeRelocation, b: &CodeRelocation| b.offset.cmp(&a.offset),
    );
    let cs = capstone::Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode64)
        .syntax(arch::x86::ArchSyntax::Att)
        .detail(true)
        .build()
        .unwrap();
    let insts = cs.disasm_all(&buf, 0).expect("disassemble");

    for instr in insts.as_ref() {
        let addr = instr.address();

        while heap.len() > 0 {
            let next_symbol_addr = heap.peek().unwrap().addr;

            if next_symbol_addr <= addr {
                let symbol = heap.pop().unwrap();
                eprintln!(
                    "{}: {:#0x} {:#0x}",
                    symbol.name,
                    symbol.addr,
                    symbol.section_addr + symbol.addr
                );
            } else {
                break;
            }
        }

        while r_heap.len() > 0 {
            let next_reloc_addr = r_heap.peek().unwrap().offset;
            if next_reloc_addr <= addr {
                let r = r_heap.pop().unwrap();
                eprintln!("    {}", r);
                //Relocation: {:#0x} {:?}", r.offset, r.r);
            } else {
                break;
            }
        }

        eprintln!(
            "  {:#06x} {}\t\t{}",
            instr.address(),
            instr.mnemonic().expect("no mnmemonic found"),
            instr.op_str().expect("no op_str found")
        );
    }
}
