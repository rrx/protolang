use capstone::prelude::*;
use object::{
    Object, ObjectSection, ObjectSymbol, ObjectSymbolTable, RelocationKind, RelocationTarget,
    Relocation, SymbolScope,
    Symbol
};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::path::Path;
use std::fmt;
use std::sync::Arc;

#[derive(Debug)]
pub enum LinkError {
    NotFound,
    MissingSymbol
}
impl std::error::Error for LinkError {}
impl fmt::Display for LinkError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LinkError: {:?}", &self)
    }
}

pub struct LiveLink {
    unlinked: Vec<UnlinkedCode>,
    names: HashMap<String, CodePointer>,
}

pub struct CodePointer {
    // we need to hold a reference here so we don't deallocate the code page
    #[allow(dead_code)]
    code: Arc<CodePage>,
    ptr: *const()
}

#[derive(Debug)]
pub struct Reloc {
    reloc_offset: isize,
    symbol_name: String,
    r: Relocation
}

pub struct UnlinkedCode {
    code_size: usize,
    bytes: Vec<u8>,
    m: memmap::MmapMut,
}

impl UnlinkedCode {
    pub fn create(buf: &[u8]) -> Result<Self, Box<dyn Error>> {
        //let obj_file = object::File::parse(buf)?;
        let obj_file = object::File::parse(buf)?;
        if let Some(section) = obj_file.section_by_name(".text") {
            let data = section.uncompressed_data()?;
            use memmap::MmapMut;
            let data_size = page_align(data.len()); // round up to page alignment

            // allocate page aligned memory and copy the functions over
            let mut mmap = MmapMut::map_anon(data_size)?;
            // only copy the first part, the remainder is uninitialized
            let (a, _) = mmap.split_at_mut(data.len());
            a.copy_from_slice(&data);

            Ok(Self {
                m: mmap,
                code_size: data.len(),
                bytes: buf.to_vec()
            })
        } else {
            unimplemented!()
        }
    }

    pub fn symbols(&self) -> Result<Vec<(String, *const())>, Box<dyn Error>> {
        let mut out = vec![];
        let obj_file = object::File::parse(self.bytes.as_slice())?;
        if let Some(symbols) = obj_file.symbol_table() {
            for s in symbols.symbols() {
                println!("symbol: {:?}", &s);
                if s.scope() == SymbolScope::Dynamic {
                    let name = s.name()?.to_string();
                    unsafe {
                        let offset = self.m.as_ptr().offset(s.address() as isize) as *const ();
                        out.push((name, offset));
                    }
                }
            }
        }
        Ok(out)
    }

    pub fn relocations(&self) -> Result<Vec<Reloc>, Box<dyn Error>> {
        let mut out = vec![];
        let obj_file = object::File::parse(self.bytes.as_slice())?;
        if let Some(section) = obj_file.section_by_name(".text") {
            for (reloc_offset, r) in section.relocations() {
                let symbol = if let RelocationTarget::Symbol(symbol_index) = r.target() {
                    obj_file
                        .symbol_table()
                        .unwrap()
                        .symbol_by_index(symbol_index)?
                } else {
                    unimplemented!()
                };
                out.push(Reloc {
                    reloc_offset: reloc_offset as isize,
                    symbol_name: symbol.name()?.to_string(),
                    r
                });
            }
        }
        Ok(out)
    }

    pub fn patch(&mut self, symbols: &HashMap<String, *const()>) -> Result<(), Box<dyn Error>> {
        let obj_file = object::File::parse(self.bytes.as_slice())?;
        if let Some(_) = obj_file.section_by_name(".text") {
            for rel in self.relocations()? {
                println!("r:{:#04x}: {:?}", &rel.reloc_offset, &rel.r);
                match rel.r.kind() {
                    RelocationKind::PltRelative => {
                        // L + A - P, 32 bit output
                        // L = address of the symbols entry within the procedure linkage table
                        // A = value of the Addend
                        // P = address of the place of the relocation

                        let symbol = if let RelocationTarget::Symbol(symbol_index) = rel.r.target() {
                            obj_file
                                .symbol_table()
                                .unwrap()
                                .symbol_by_index(symbol_index)?
                        } else {
                            unreachable!()
                        };
                        let name = symbol.name()?;
                        //let symbol_addr = symbol.address();
                        println!("look up: {}", name);
                        let symbol_addr = *symbols.get(name).unwrap();
                        let addend = rel.r.addend();
                        let _place = symbol.address();

                        // complicated pointer arithmetic to update the relocations
                        //
                        unsafe {
                            let patch_base = self.m.as_mut_ptr() as *mut u8;

                            //let text_runtime_base = 0;
                            // we are relocating within the same section
                            //let section_runtime_base = 0;

                            let patch = patch_base.offset(rel.reloc_offset as isize);

                            let symbol_address =
                                symbol_addr as isize + addend as isize - patch as isize;

                            // patch as 32 bit
                            let patch = patch as *mut u32;
                            patch.replace(symbol_address as u32);

                            println!(
                                "rel {}: patch:{:#08x} patchv:{:#08x} addend:{:#08x} addr:{:#08x} symbol:{:#08x}",
                                name,
                                patch as usize,
                                std::ptr::read(patch),
                                addend,
                                symbol_addr as isize,
                                symbol_address as isize,
                                );
                        }
                    }
                    _ => unimplemented!(),
                }
            }
        }
        Ok(())
    }
}

pub struct CodePage {
    m: memmap::Mmap,
    names: HashMap<String, *const ()>,
    //address: HashMap<*const , String>,
    code_size: usize,
}

fn page_align(n: usize) -> usize {
    // hardwired for now, but we can get this from the target we are running at at runtime
    let p = 4096;
    return (n + (p - 1)) & !(p - 1);
}

impl CodePage {
    /*
    pub fn create(buf: &[u8]) -> Result<Self, Box<dyn Error>> {

            //let w = object::write::Object::new(obj_file.format(), obj_file.architecture(), obj_file.endianness());
            for (reloc_offset, r) in section.relocations() {
                println!("r:{:#04x}: {:?}", reloc_offset, &r);
                match r.kind() {
                    RelocationKind::PltRelative => {
                        // L + A - P, 32 bit output
                        // L = address of the symbols entry within the procedure linkage table
                        // A = value of the Addend
                        // P = address of the place of the relocation

                        let symbol = if let RelocationTarget::Symbol(symbol_index) = r.target() {
                            obj_file
                                .symbol_table()
                                .unwrap()
                                .symbol_by_index(symbol_index)?
                        } else {
                            unreachable!()
                        };
                        let name = symbol.name()?;
                        let symbol_addr = symbol.address();
                        let addend = r.addend();
                        let _place = symbol.address();

                        // complicated pointer arithmetic to update the relocations
                        //
                        unsafe {
                            let base = mmap.as_mut_ptr() as *mut u8;

                            //let text_runtime_base = 0;
                            // we are relocating within the same section
                            //let section_runtime_base = 0;

                            let patch_offset = reloc_offset as isize;

                            let symbol_offset =
                                symbol_addr as isize + addend as isize - patch_offset as isize;
                            let symbol_address = symbol_offset;

                            // patch as 32 bit
                            let patch = base.offset(patch_offset) as *mut u32;
                            patch.replace(symbol_address as u32);

                            println!(
                                "x:{:#08x} {:#08x} {:#08x} {}",
                                patch as usize,
                                std::ptr::read(patch),
                                symbol_address as usize,
                                name
                            );
                        }
                    }
                    _ => unimplemented!(),
                }
            }
    */
    pub fn create(mut code: UnlinkedCode) -> Result<Self, Box<dyn Error>> {
        let mut names = HashMap::new();


        // make a map of all of the symbol names, so we can perform a look up
        //let mut address = HashMap::new();

        unsafe {
            //let base = mmap.as_ptr();
            for (name, ptr) in code.symbols()? {//obj_file.symbol_table().unwrap().symbols() {
                //let name = s.name()?.to_string();
                //let offset = s.address() as isize;
                //let ptr = base.offset(offset) as *const ();
                names.insert(name.clone(), ptr);
                //address.insert(s.address(), name);
            }
        }

        let mmap = code.m.make_exec()?; // make read only

        Ok(Self {
            m: mmap,
            names,
            //address,
            code_size: code.code_size
        })
    }

    pub fn disassemble(&self) {
        // disassemble the code we are generating
        let cs = Capstone::new()
            .x86()
            .mode(arch::x86::ArchMode::Mode64)
            .build()
            .unwrap();
        let insts = cs
            .disasm_all(&self.m.split_at(self.code_size).0, 0)
            .expect("disassemble");
        for instr in insts.as_ref() {
            let addr = instr.address();
            unsafe {
                let ptr = self.m.as_ptr().offset(addr as isize) as usize;
                println!(
                    "{:#08x}  {:#06x} {}\t\t{}",
                    ptr,
                    &addr,
                    instr.mnemonic().expect("no mnmemonic found"),
                    instr.op_str().expect("no op_str found")
                    );
            }
        }
    }

        /*
    //pub fn disassemble(&self) {
        // disassemble the code we are generating
        let cs = Capstone::new()
            .x86()
            .mode(arch::x86::ArchMode::Mode64)
            .build()
            .unwrap();
        let insts = cs
            .disasm_all(&self.m.split_at(self.code_size).0, 0)
            .expect("disassemble");
        let mut last_name = None;
        for instr in insts.as_ref() {
            let addr = instr.address();
            if let Some(v) = self.address.get(&addr) {
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
*/
    pub fn invoke<P, T>(&self, name: &str, args: P) -> Result<T, Box<dyn Error>> {
        // call the main function
        unsafe {
            let ptr = *self.names.get(name).unwrap() as isize;
            type MyFunc<P, T> = unsafe extern "cdecl" fn(P) -> T;
            let v: MyFunc<P, T> = std::mem::transmute(ptr);
            let ret = v(args);
            Ok(ret)
        }
    }
}

impl LiveLink {
    pub fn create(
    ) -> Result<Self, Box<dyn Error>> {
        Ok(Self {
            unlinked: vec![],
            names: HashMap::new()
        })
    }

    pub fn load_object_file(&mut self, path: &Path) -> Result<(), Box<dyn Error>> {
        use std::fs;
        let buf = fs::read(path)?;
        println!("read: {}", buf.len());
        self.add_object_file(buf.as_slice())
    }

    pub fn add_object_file(&mut self, buf: &[u8]) -> Result<(), Box<dyn Error>> {
        let code = UnlinkedCode::create(buf)?;
        self.unlinked.push(code);
        Ok(())
    }

    pub fn link(&mut self) -> Result<(), Box<dyn Error>> {
        let mut locations = HashMap::new();
        let mut symbols = HashSet::new();
        let mut relocs = HashSet::new();
        for code in &self.unlinked {
            for (symbol, offset) in code.symbols()? {
                println!("symbol: {:#30}: {:#010x}", symbol, offset as usize);
                symbols.insert(symbol.clone());
                locations.insert(symbol, offset); 
            }

            for r in code.relocations()? {
                println!("relocation: {:?}", &r);
                relocs.insert(r.symbol_name);
            }
        }

        println!("symbols: {:?}", &symbols);
        println!("relocations: {:?}", &relocs);
        let diff = relocs.difference(&symbols).collect::<Vec<_>>();
        for s in &diff {
            println!("Missing symbol: {}", s);
        }

        if diff.len() > 0 {
            Err(LinkError::MissingSymbol.into())
        } else {
            for mut code in self.unlinked.drain(..) {
                code.patch(&locations)?;
                let code = CodePage::create(code)?;
                code.disassemble();
                let names = code.names.clone();
                let code = Arc::new(code);
                for (name, ptr) in names.iter() {
                    let pointer = CodePointer {
                        code: code.clone(),
                        ptr: *ptr
                    };
                    self.names.insert(name.clone(), pointer);
                }
            }
            Ok(())
        }

    }

    pub fn invoke<P, T>(&self, name: &str, args: P) -> Result<T, Box<dyn Error>> {
        // call the main function
        unsafe {
            let ptr = self.names.get(name).ok_or(LinkError::NotFound)?.ptr as *const();
            type MyFunc<P, T> = unsafe extern "cdecl" fn(P) -> T;
            let v: MyFunc<P, T> = std::mem::transmute(ptr);
            let ret = v(args);
            Ok(ret)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hotreload() {
        let mut e = LiveLink::create().unwrap();
        e.link().unwrap();
        let ret: u64 = e.invoke("main", ()).unwrap();
        println!("ret1: {}", ret);
        assert_eq!(55, ret);
    }

    #[test]
    fn link_extern() {
        let mut e = LiveLink::create().unwrap();
        // load test functions
        e.load_object_file(Path::new("/home/rrx/code/protolang/tmp/testfunction.o")).unwrap();
        e.link().unwrap();
        let ret: u64 = e.invoke("main", ()).unwrap();
        println!("ret1: {}", ret);
        assert_eq!(10012, ret);
    }
}

