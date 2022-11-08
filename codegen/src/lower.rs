use crate::{
    hir::*,
    llvm::generator::{CodeGen, Generator, generate_definitions},
};
pub use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::OptimizationLevel;
use std::collections::{HashMap, HashSet};
use std::error::Error;

pub type ModuleMap<'a> = HashMap<String, Module<'a>>;

pub struct Lower<'a> {
    modules: ModuleMap<'a>,
    optimizer: PassManager<Module<'a>>,
    link_optimizer: PassManager<Module<'a>>,
}

impl<'a> Lower<'a> {
    pub fn new(optimization_level: OptimizationLevel, size_level: u32) -> Self {
        let pass_manager_builder = PassManagerBuilder::create();

        pass_manager_builder.set_optimization_level(optimization_level);
        pass_manager_builder.set_size_level(size_level);

        let pass_manager = PassManager::create(());
        pass_manager_builder.populate_module_pass_manager(&pass_manager);

        // Do LTO optimizations afterward mosty for function inlining
        let link_time_optimizations = PassManager::create(());
        pass_manager_builder.populate_lto_pass_manager(&link_time_optimizations, false, true);

        Self { modules: ModuleMap::new(), optimizer: pass_manager, link_optimizer: link_time_optimizations }
    }

    pub fn compile_module(&mut self, context: &'a Context, name: &str, ast: &Ast) -> Result<(), Box<dyn Error>> {
        let module = context.create_module(name);

        //let mut definitions = HashMap::new();
        //generate_definitions(&mut definitions, ast);

        let mut codegen = Generator {
            module,
            context,
            builder: context.create_builder(),
            definitions: HashMap::new(),
            //definitions,
            auto_derefs: HashSet::new(),
            current_function_info: None,
            current_definition_name: None,
        };

        ast.codegen(&mut codegen);

        self.optimizer.run_on(&codegen.module);

        match codegen.module.verify() {
            Ok(_) => {
                codegen.module.print_to_stderr();
                self.modules.insert(name.to_string(), codegen.module);
                Ok(())
            },
            Err(error) => {
                codegen.module.print_to_stderr();
                Err(error.into())
            },
        }
    }

    pub fn run(&self, context: &'a Context) -> Result<i64, Box<dyn Error>> {
        run_jit(context, &self.modules)
    }
}

pub fn run_jit<'a, T>(context: &'a Context, modules: &ModuleMap<'a>) -> Result<T, Box<dyn Error>> {
    let config = InitializationConfig::default();
    Target::initialize_native(&config)?;
    eprintln!("Default: {:?}", TargetMachine::get_default_triple().as_str());
    eprintln!("Host: {}", TargetMachine::get_host_cpu_name().to_str()?);

    // dummy module to just create the execution engine
    let module = context.create_module("__xmain__");
    let ee = module.create_jit_execution_engine(OptimizationLevel::None)?;

    for (_name, module) in modules {
        ee.add_module(&module).unwrap();
    }

    unsafe {
        let f = ee.get_function::<unsafe extern "C" fn(i32) -> T>("main").unwrap();
        let ret = f.call(0);
        Ok(ret)
    }
}
