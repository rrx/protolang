use super::*;
use capstone::prelude::*;

impl PatchBlock {
    pub fn disassemble(&self) {
        match self.kind {
            PatchBlockKind::Code => {
                let base = self.block.as_ptr() as usize;
                eprintln!(
                    "Code Block Disassemble: Base: {:#08x}, Name: {}",
                    base, &self.name
                );
                let mut pointers = im::HashMap::new();
                for (name, ptr) in &self.symbols {
                    eprintln!(" {:#08x}: {}", ptr.as_ptr() as usize, name);
                    pointers.insert(ptr.as_ptr() as usize - base, name.clone());
                }
                for r in &self.relocations {
                    eprintln!(" {}", &r);
                }
                let size = self.block.size;
                let buf = &self.block.as_slice()[0..size];
                disassemble_code(buf, pointers);
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
    let pointers = im::HashMap::new();
    disassemble_code(buf, pointers);
}

pub fn disassemble_code(buf: &[u8], pointers: im::HashMap<usize, String>) {
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
        let addr = instr.address() as usize;
        if let Some(v) = pointers.get(&addr) {
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
