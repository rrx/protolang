use capstone::prelude::*;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{CodeModel, FileType, RelocMode};
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::OptimizationLevel;
use object::{
    Object, ObjectSection, ObjectSymbol, ObjectSymbolTable, RelocationKind, RelocationTarget,
    Relocation, SymbolScope,
    Symbol
};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::path::Path;
use std::fmt;
use codegen_ir::hir::*;
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

pub struct LiveLink<'a> {
    context: &'a Context,
    unlinked: Vec<UnlinkedCode>,
    optimizer: PassManager<Module<'a>>,
    target_machine: TargetMachine,
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

impl<'a> LiveLink<'a> {
    pub fn create(
        context: &'a Context,
        optimization_level: OptimizationLevel,
        size_level: u32,
    ) -> Result<Self, Box<dyn Error>> {
        // Initialize for the host machine, we aren't doing any cross compiling here
        let config = InitializationConfig::default();
        Target::initialize_native(&config)?;
        //Target::initialize_all(&config);
        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple)?;

        let features = TargetMachine::get_host_cpu_features();

        let target_machine = target
            .create_target_machine(
                &triple,
                "",
                features.to_str()?,
                OptimizationLevel::None,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .expect("Unable to create target machine");

        eprintln!("Default: {:?}", triple.as_str());
        eprintln!("Host: {}", TargetMachine::get_host_cpu_name().to_str()?);

        let pass_manager_builder = PassManagerBuilder::create();

        pass_manager_builder.set_optimization_level(optimization_level);
        pass_manager_builder.set_size_level(size_level);

        let pass_manager = PassManager::create(());
        pass_manager_builder.populate_module_pass_manager(&pass_manager);

        // Do LTO optimizations afterward mosty for function inlining
        let link_time_optimizations = PassManager::create(());
        pass_manager_builder.populate_lto_pass_manager(&link_time_optimizations, false, true);

        Ok(Self {
            context,
            unlinked: vec![],
            optimizer: pass_manager,
            target_machine,
            names: HashMap::new()
        })
    }

    pub fn load_object_file(&mut self, path: &Path) -> Result<(), Box<dyn Error>> {
        use std::fs;
        let buf = fs::read(path)?;
        println!("read: {}", buf.len());
        self.add_object_file(buf.as_slice())
    }

    pub fn compile(&mut self, name: &str, ast: &Ast) -> Result<(), Box<dyn Error>> {
        println!("AST: {}", &ast.to_ron());
        let mut defmap = crate::DefinitionMap::default();
        let context = &self.context;
        let module = crate::generate(context, name, &ast, &mut defmap).unwrap();

        self.optimizer.run_on(&module);
        module.verify()?;

        let asm_buf = self
            .target_machine
            .write_to_memory_buffer(&module, FileType::Assembly)
            .unwrap();
        let obj_buf = self
            .target_machine
            .write_to_memory_buffer(&module, FileType::Object)
            .unwrap();
        self.target_machine
            .write_to_file(&module, FileType::Object, &Path::new("out.o"))
            .unwrap();
        println!("asm {}", std::str::from_utf8(asm_buf.as_slice()).unwrap());
        self.add_object_file(obj_buf.as_slice())
    }

    fn add_object_file(&mut self, buf: &[u8]) -> Result<(), Box<dyn Error>> {
        let code = UnlinkedCode::create(buf)?;
        self.unlinked.push(code);
        Ok(())
    }

    fn link(&mut self) -> Result<(), Box<dyn Error>> {
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

    //pub fn link(&mut self, name: &str, buf: &[u8]) -> Result<CodePage, Box<dyn Error>> {
    //let code = CodePage::create(buf)?;
    //code.disassemble();
    //Ok(code)
    //}
}

use std::io::{Write};

// macro used to print & flush without printing a new line
macro_rules! print_flush {
    ( $( $x:expr ),* ) => {
        print!( $($x, )* );

        std::io::stdout().flush().expect("Could not flush to standard output.");
    };
}

#[no_mangle]
pub extern "C" fn putchard(x: f64) -> f64 {
    print_flush!("{}", x as u8 as char);
    x
}

#[no_mangle]
pub extern "C" fn printd(x: f64) -> f64 {
    println!("{}", x);
    x
}

// Adding the functions above to a global array,
// so Rust compiler won't remove them.
#[used]
static EXTERNAL_FNS: [extern "C" fn(f64) -> f64; 2] = [putchard, printd];

#[cfg(test)]
mod tests {
    use super::*;
    use codegen_ir::testing::*;

    #[test]
    fn hotreload() {
        let context = Context::create();
        let mut defs = Definitions::new();
        let ast = gen_fib(&mut defs);
        let mut e = LiveLink::create(&context, OptimizationLevel::None, 0).unwrap();
        let _ = e.compile("test", &ast).unwrap();
        e.link().unwrap();
        let ret: u64 = e.invoke("main", ()).unwrap();
        println!("ret1: {}", ret);
        assert_eq!(55, ret);
    }

    #[test]
    fn link_extern() {
        let context = Context::create();
        let mut defs = Definitions::new();
        let ast = gen_extern(&mut defs);
        let mut e = LiveLink::create(&context, OptimizationLevel::None, 0).unwrap();
        // load test functions
        e.load_object_file(Path::new("/home/rrx/code/protolang/tmp/testfunction.o")).unwrap();
        // load code that refers to the other object file
        e.compile("test2", &ast).unwrap();
        e.link().unwrap();
        let ret: u64 = e.invoke("main", ()).unwrap();
        println!("ret1: {}", ret);
        assert_eq!(10012, ret);
    }
}
