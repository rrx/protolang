use capstone::prelude::*;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{CodeModel, FileType, RelocMode};
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::OptimizationLevel;
use object::{
    Object, ObjectSection, ObjectSymbol, ObjectSymbolTable, RelocationKind, RelocationTarget,
};
use std::collections::HashMap;
use std::error::Error;
use std::path::Path;

use codegen_ir::hir::*;

pub struct LiveLink<'a> {
    optimizer: PassManager<Module<'a>>,
    link_optimizer: PassManager<Module<'a>>,
    init_config: InitializationConfig,
    target_machine: TargetMachine,
}

pub struct CodePage {
    m: memmap::Mmap,
    names: HashMap<String, u64>,
    address: HashMap<u64, String>,
    code_size: usize,
}

fn page_align(n: usize) -> usize {
    // hardwired for now, but we can get this from the target we are running at at runtime
    let p = 4096;
    return (n + (p - 1)) & !(p - 1);
}

impl CodePage {
    pub fn create(buf: &[u8]) -> Result<Self, Box<dyn Error>> {
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

            let mmap = mmap.make_exec()?; // make read only

            // make a map of all of the symbol names, so we can perform a look up
            let mut names = HashMap::new();
            let mut address = HashMap::new();
            for s in obj_file.symbol_table().unwrap().symbols() {
                let name = s.name()?.to_string();
                names.insert(name.clone(), s.address());
                address.insert(s.address(), name);
            }

            Ok(Self {
                m: mmap,
                names,
                address,
                code_size: data.len(),
            })
        } else {
            unimplemented!()
        }
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

                println!(
                    "  {:#06x} {}\t\t{}",
                    &addr,
                    instr.mnemonic().expect("no mnmemonic found"),
                    instr.op_str().expect("no op_str found")
                );

                last_name = Some(v);
            }
        }
    }

    pub fn run<T>(&self) -> Result<T, Box<dyn Error>> {
        // call the main function
        unsafe {
            let base = self.m.as_ptr();
            let offset = *self.names.get("main").unwrap() as isize;
            let ptr = base.offset(offset) as *const ();
            let v = std::mem::transmute::<*const (), fn() -> T>(ptr);
            let ret = v();
            Ok(ret)
        }
    }
}

impl<'a> LiveLink<'a> {
    pub fn create(optimization_level: OptimizationLevel, size_level: u32) -> Result<Self, Box<dyn Error>> {
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
            ).expect("Unable to create target machine");

        eprintln!("Default: {:?}", triple.as_str());
        eprintln!(
            "Host: {}",
            TargetMachine::get_host_cpu_name().to_str()?
        );

        let pass_manager_builder = PassManagerBuilder::create();

        pass_manager_builder.set_optimization_level(optimization_level);
        pass_manager_builder.set_size_level(size_level);

        let pass_manager = PassManager::create(());
        pass_manager_builder.populate_module_pass_manager(&pass_manager);

        // Do LTO optimizations afterward mosty for function inlining
        let link_time_optimizations = PassManager::create(());
        pass_manager_builder.populate_lto_pass_manager(&link_time_optimizations, false, true);

        Ok(Self {
            init_config: config,
            optimizer: pass_manager,
            link_optimizer: link_time_optimizations,
            target_machine,
        })
    }

    pub fn compile(
        &mut self,
        name: &str,
        ast: &Ast,
        context: &'a Context,
    ) -> Result<CodePage, Box<dyn Error>> {
        println!("AST: {}", &ast.to_ron());
        let mut defmap = crate::DefinitionMap::default();
        let module = crate::generate(&context, name, &ast, &mut defmap).unwrap();
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
        let code = CodePage::create(obj_buf.as_slice())?;
        code.disassemble();
        Ok(code)
        //Ok(obj_buf.as_slice())
    }

    //pub fn link(&mut self, name: &str, buf: &[u8]) -> Result<CodePage, Box<dyn Error>> {
    //let code = CodePage::create(buf)?;
    //code.disassemble();
    //Ok(code)
    //}
}

#[cfg(test)]
mod tests {
    use super::*;

    use codegen_ir::hir::*;
    use codegen_ir::testing::*;

    #[test]
    fn hotreload() {
        let context = Context::create();
        let mut defs = Definitions::new();
        let ast = gen_fib(&mut defs);
        let mut e = LiveLink::create(OptimizationLevel::None, 0).unwrap();
        let code = e.compile("test", &ast, &context).unwrap();
        let ret: u64 = code.run().unwrap();
        println!("ret1: {}", ret);
        assert_eq!(55, ret);
    }
}
