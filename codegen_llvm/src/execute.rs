use inkwell::context::Context;
use inkwell::module::{Module};
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::execution_engine::ExecutionEngine;
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::error::Error;
use inkwell::targets::{CodeModel, RelocMode, FileType, TargetTriple};
use capstone::prelude::*;
use std::path::Path;
use object::{Object, ObjectSection, ObjectSymbolTable, ObjectSymbol, SymbolScope, RelocationKind, RelocationTarget};

use codegen_ir::hir::*;
//use std::io::Write;

pub type ModuleMap<'a> = HashMap<String, Module<'a>>;

pub struct Executor<'a> {
    //modules: Vec<Module<'a>>,
    optimizer: PassManager<Module<'a>>,
    link_optimizer: PassManager<Module<'a>>,
    ee: Option<ExecutionEngine<'a>>,
    init_config: InitializationConfig,
    target_machine: TargetMachine,
    capstone: Capstone,
}

fn  page_align(n: usize) -> usize {
    // hardwired for now, but we can get this from the target we are running at at runtime
    let p = 4096;
    return (n + (p - 1)) & !(p - 1);
}

impl<'a> Executor<'a> {
    pub fn new(optimization_level: OptimizationLevel, size_level: u32) -> Self {
        let cs = Capstone::new().x86().mode(arch::x86::ArchMode::Mode64).build().unwrap();
        let config = InitializationConfig::default();
        //Target::initialize_native(&config).expect("Unable to initialize JIT");
        Target::initialize_all(&config);
        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).unwrap();

        let reloc = RelocMode::Default;
        let model = CodeModel::Default;
        let features = TargetMachine::get_host_cpu_features();
        //let cpu = TargetMachine::get_cpu();
        //let target_machine = target.create_target_machine(
            //&triple, "x86-64", features.to_str().unwrap(),
            //optimization_level, reloc, model).unwrap();

        let target_machine = target
            .create_target_machine(&triple, "", "", OptimizationLevel::None, RelocMode::PIC, CodeModel::Default)
            .unwrap();

        eprintln!("Default: {:?}", triple.as_str());
        eprintln!("Host: {}", TargetMachine::get_host_cpu_name().to_str().unwrap());

        let pass_manager_builder = PassManagerBuilder::create();

        pass_manager_builder.set_optimization_level(optimization_level);
        pass_manager_builder.set_size_level(size_level);

        let pass_manager = PassManager::create(());
        pass_manager_builder.populate_module_pass_manager(&pass_manager);

        // Do LTO optimizations afterward mosty for function inlining
        let link_time_optimizations = PassManager::create(());
        pass_manager_builder.populate_lto_pass_manager(&link_time_optimizations, false, true);

        Self {
            //modules: vec![],
            init_config: config,
            optimizer: pass_manager,
            link_optimizer: link_time_optimizations,
            ee: None,
            target_machine,
            capstone: cs
        }
    }

    pub fn remove(&mut self, module: &Module<'a>) -> Result<(), Box<dyn Error>> {
        Ok(match self.ee.as_ref() {
            Some(ee) => ee.remove_module(module)?,
            None => ()
        })
    }

    fn add_module(&mut self, module: &Module<'a>) -> Result<(), Box<dyn Error>> {
        Ok(match self.ee.as_ref() {
            Some(ee) => ee.add_module(module).expect("Module already exists"),
            None => {
                self.ee = Some(module.create_jit_execution_engine(OptimizationLevel::None)?);
                ()
            }
        })
    }

    pub fn add(&mut self, module: &Module<'a>) -> Result<(), Box<dyn Error>> {
        self.optimizer.run_on(&module);
        //let name = module.get_name().to_str().unwrap();

        match module.verify() {
            Ok(_) => {
                //module.print_to_stderr();
                self.add_module(module)?;
                //self.modules.push(module);

                Ok(())
            }
            Err(error) => {
                //module.print_to_stderr();
                Err(error.into())
            }
        }
    }

    pub fn compile(&mut self, name: &str, ast: &Ast, context: &'a Context) -> Result<Module<'a>, Box<dyn Error>> {
        println!("AST: {}", &ast.to_ron());
        let mut defmap = crate::DefinitionMap::default();
        let module = crate::generate(&context, name, &ast, &mut defmap).unwrap();
        let asm_buf = self.target_machine.write_to_memory_buffer(&module, FileType::Assembly).unwrap();
        let obj_buf = self.target_machine.write_to_memory_buffer(&module, FileType::Object).unwrap();
        self.target_machine.write_to_file(&module, FileType::Object, &Path::new("out.o")).unwrap();
        let obj_file = object::File::parse(&*obj_buf.as_slice())?;
        println!("asm {}", std::str::from_utf8(asm_buf.as_slice()).unwrap());

        let syms = obj_file.symbol_map();
        self.capstone.set_detail(true).unwrap();
        if let Some(section) = obj_file.section_by_name(".text") {
            let data = section.uncompressed_data()?;
            use memmap::MmapMut;
            let data_size = page_align(data.len()); // round up to page alignment

            // allocate page aligned memory and copy the functions over
            let mut mmap = MmapMut::map_anon(data_size)?;
            // only copy the first part, the remainder is uninitialized
            let (a, b) = mmap.split_at_mut(data.len());
            a.copy_from_slice(&data);

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
                            obj_file.symbol_table().unwrap().symbol_by_index(symbol_index)?
                        } else {
                            unreachable!()
                        };
                        let name = symbol.name()?;
                        let symbol_addr = symbol.address();
                        let addend = r.addend();
                        let place = symbol.address();

                        // complicated pointer arithmetic to update the relocations
                        //
                        unsafe {
                            let base = mmap.as_mut_ptr() as *mut u8;

                            //let text_runtime_base = 0;
                            // we are relocating within the same section
                            //let section_runtime_base = 0;

                            let patch_offset = reloc_offset as isize;

                            let symbol_offset = symbol_addr as isize + addend as isize - patch_offset as isize ;
                            let symbol_address = symbol_offset;

                            // patch as 32 bit
                            let patch = base.offset(patch_offset) as *mut u32;
                            patch.replace(symbol_address as u32);

                            println!("x:{:#08x} {:#08x} {:#08x} {}",
                                     patch as usize,
                                     std::ptr::read(patch),
                                     symbol_address as usize,
                                     name);
                        }
                    }
                    _ => unimplemented!()
                }
            }

            let mmap = mmap.make_exec()?; // make read only

            // disassemble the code we are generating
            let insts = self.capstone.disasm_all(&mmap.split_at(data.len()).0, 0x0).expect("disassemble");
            let mut last_name = None;
            for instr in insts.as_ref() {
                let addr = instr.address();
                if let Some(v) = syms.get(addr) {

                    let display_symbol = if let Some(name) = last_name {
                        if name != v.name() {
                            last_name = Some(v.name());
                            Some(v.name())
                        } else {
                            None
                        }
                    } else {
                        Some(v.name())
                    };

                    if let Some(_) = display_symbol {
                        println!("fn {}: {:#06x}",
                                 v.name(),
                                 &addr);
                    }

                    println!("  {:#06x} {}\t\t{}", 
                             &addr,
                             instr.mnemonic().expect("no mnmemonic found"),
                             instr.op_str().expect("no op_str found"));

                    last_name = Some(v.name());
                }
            }

            // make a map of all of the symbol names, so we can perform a look up
            let mut names = HashMap::new();
            for s in obj_file.symbol_table().unwrap().symbols() {
                names.insert(s.name()?, s.address());
            }

            // call the main function
            unsafe {
                let base = mmap.as_ptr();
                let offset = *names.get("main").unwrap() as isize;
                let ptr = base.offset(offset) as *const ();
                //println!("ptr: {:?}, {:?}", base, ptr);
                let v = std::mem::transmute::<*const (), fn() -> i64>(ptr);
                let ret = v() as u64;
                println!("ret1: {}", ret)
            }
        }



        Ok(module)
    }

    pub fn run<T>(&self) -> Result<T, Box<dyn Error>> {
        unsafe {
            let f = self.ee.as_ref().unwrap()
                .get_function::<unsafe extern "C" fn(i32) -> T>("main")
                .unwrap();
            let ret = f.call(0);
            Ok(ret)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use codegen_ir::testing::*;

    #[test]
    fn hotreload() {
        let context = Context::create();
        let mut defs = Definitions::new();
        let ast = gen_fib(&mut defs);
        let mut e = Executor::new(OptimizationLevel::None, 0);
        let module = e.compile("test", &ast, &context).unwrap();
        e.add(&module).unwrap();
        let ret = e.run::<i64>().unwrap();
        assert_eq!(ret, 55);
        e.remove(&module).unwrap();

        let module = e.compile("test2", &ast, &context).unwrap();
        e.add(&module).unwrap();
        let ret = e.run::<i64>().unwrap();
        assert_eq!(ret, 55);
        e.remove(&module).unwrap();
    }
}
