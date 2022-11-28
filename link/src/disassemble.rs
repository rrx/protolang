use super::*;
use capstone::prelude::*;

impl PatchBlockInner {
    pub fn disassemble(&self) {
        match self.kind {
            PatchBlockKind::Code => {
                let base = self.block.as_ptr() as usize;
                println!(
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
                disassemble_code(&self.block.as_slice()[0..size], pointers);
            }

            PatchBlockKind::Data => {
                let base = self.block.as_ptr() as usize;
                eprintln!("data_rw@{:#08x}+{:#x}", base, self.block.size);
                eprint_bytes(&self.block.as_slice()[0..self.block.size]);
            }
        }
    }
}

impl PatchDataBlock {
    pub fn disassemble(&self) {
        let mut pointers = im::HashMap::new();
        let base = self.block.as_ptr() as usize;
        println!(
            "Data Block Disassemble: Base: {:#08x}, Name: {}",
            base, &self.name
        );
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
        let size = self.block.0.size;
        let buf = &self.block.as_slice()[0..size];
        //disassemble_data(&self.block.as_slice()[0..size], pointers);
        println!(" buf: {:?}", buf);
    }
}

impl PatchCodeBlock {
    pub fn disassemble(&self) {
        let base = self.block.as_ptr() as usize;
        println!(
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
        let size = self.block.0.size;
        let buf = &self.block.as_slice()[0..size];
        disassemble_code(&self.block.as_slice()[0..size], pointers);
    }
}

impl ExecutableCodeBlock {
    pub fn disassemble(&self) {
        let base = self.as_ptr() as usize;
        println!("code@{:#08x}", base);
        let _pointers = im::HashMap::new();
        disassemble_code(&self.as_slice()[0..self.0.size], _pointers);
    }
}

impl ReadonlyDataBlock {
    pub fn disassemble(&self) {
        let base = self.as_ptr() as usize;
        println!("data_ro@{:#08x}+{:#x}", base, self.0.size);
        eprint_bytes(&self.as_slice()[0..self.size()]);
        //let _pointers = im::HashMap::new();
        //disassemble_data(&self.as_slice()[0..self.0.size], _pointers);
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

impl WritableDataBlock {
    pub fn disassemble(&self) {
        let base = self.as_ptr() as usize;
        eprintln!("data_rw@{:#08x}+{:#x}", base, self.0.size);
        eprint_bytes(&self.as_slice()[0..self.size()]);
        //let _pointers = im::HashMap::new();
        //disassemble_data(&self.as_slice()[0..self.0.size], _pointers);
    }
}

/*
pub fn disassemble_data(buf: &[u8], pointers: im::HashMap<usize, String>) {
    for (ptr, name) in pointers {
        println!("ptr: {:?}", (ptr, name));
    }
    println!("buf: {:?}", buf);
}
*/

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
                    last_name = Some(v);
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
