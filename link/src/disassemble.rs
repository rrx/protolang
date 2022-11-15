use capstone::prelude::*;
use super::*;

impl CodePageInner {
    pub fn disassemble(&self) {
        println!("code: {}, {:?}", &self.name, &self);
        let buf = &self.m.split_at(self.code_size).0;
        //let buf = &self.m.as_ref()[0..self.code_size];
        let mut pointers = im::HashMap::new();
        let base = self.m.as_ptr() as usize;
        for (name, ptr) in &self.symbols {
            pointers.insert(*ptr as usize - base, name.clone());
        }
        println!("start: {:#08x}", &base);
        disassemble(self.kind, buf, pointers);
        match self.kind {
            CodePageKind::Data => {
            }
            _ => ()
        }

    }
}

impl UnpatchedCodePage {
    pub fn disassemble(&self) {
        println!("code: {}, {:?}", &self.name, &self);
        let buf = &self.m.as_ref()[0..self.code_size];
        let mut pointers = im::HashMap::new();
        let base = self.m.as_ptr() as usize;
        for (name, ptr) in &self.symbols {
            pointers.insert(*ptr as usize - base, name.clone());
        }
        disassemble(self.kind, buf, pointers);
    }
}


pub fn disassemble(kind: CodePageKind, buf: &[u8], pointers: im::HashMap<usize, String>) {
    match kind {
        CodePageKind::Data => disassemble_data(buf, pointers),
        CodePageKind::Code => disassemble_code(buf, pointers),
    }
}

pub fn disassemble_data(buf: &[u8], pointers: im::HashMap<usize, String>) {
    println!("pointers: {:?}", pointers);
    for (ptr, name) in pointers {
        println!("buf: {:?}", (ptr, name));
    }
    println!("buf: {:?}", buf);
}

pub fn disassemble_code(buf: &[u8], pointers: im::HashMap<usize, String>) {

    // disassemble the code we are generating
    let cs = capstone::Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode64)
        .build()
        .unwrap();
    let insts = cs
        .disasm_all(&buf, 0)
        .expect("disassemble");
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

        println!(
            "  {:#06x} {}\t\t{}",
            &addr,
            instr.mnemonic().expect("no mnmemonic found"),
            instr.op_str().expect("no op_str found")
            );
    }
}

