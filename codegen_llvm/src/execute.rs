use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::module::{Linkage, Module};
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType, PointerType};
use inkwell::values::{
    BasicValue, BasicValueEnum, CallableValue, FunctionValue, InstructionOpcode, IntValue,
};
use inkwell::OptimizationLevel;
use inkwell::{AddressSpace, FloatPredicate, IntPredicate};
use std::collections::HashMap;
use std::error::Error;

use codegen_ir::util::fmap;
use codegen_ir::{
    hir::{self, *},
    *,
};

pub type ModuleMap<'a> = HashMap<String, Module<'a>>;

pub struct Executor<'a> {
    modules: Vec<Module<'a>>,
    optimizer: PassManager<Module<'a>>,
    link_optimizer: PassManager<Module<'a>>,
}

impl<'a> Executor<'a> {
    pub fn new(optimization_level: OptimizationLevel, size_level: u32) -> Self {
        let pass_manager_builder = PassManagerBuilder::create();

        pass_manager_builder.set_optimization_level(optimization_level);
        pass_manager_builder.set_size_level(size_level);

        let pass_manager = PassManager::create(());
        pass_manager_builder.populate_module_pass_manager(&pass_manager);

        // Do LTO optimizations afterward mosty for function inlining
        let link_time_optimizations = PassManager::create(());
        pass_manager_builder.populate_lto_pass_manager(&link_time_optimizations, false, true);

        Self {
            modules: vec![],
            optimizer: pass_manager,
            link_optimizer: link_time_optimizations,
        }
    }

    pub fn add(&mut self, module: Module<'a>) -> Result<(), Box<dyn Error>> {
        self.optimizer.run_on(&module);
        //let name = module.get_name().to_str().unwrap();

        match module.verify() {
            Ok(_) => {
                module.print_to_stderr();
                self.modules.push(module);
                Ok(())
            }
            Err(error) => {
                module.print_to_stderr();
                Err(error.into())
            }
        }
    }

    pub fn run<T>(&self) -> Result<T, Box<dyn Error>> {
        let config = InitializationConfig::default();
        Target::initialize_native(&config)?;
        eprintln!(
            "Default: {:?}",
            TargetMachine::get_default_triple().as_str()
        );
        eprintln!("Host: {}", TargetMachine::get_host_cpu_name().to_str()?);

        let mut iter = self.modules.iter();
        let ee = iter
            .next()
            .expect("No modules")
            .create_jit_execution_engine(OptimizationLevel::None)?;
        for module in iter {
            ee.add_module(&module).unwrap();
        }

        unsafe {
            let f = ee
                .get_function::<unsafe extern "C" fn(i32) -> T>("main")
                .unwrap();
            let ret = f.call(0);
            Ok(ret)
        }
    }
}
