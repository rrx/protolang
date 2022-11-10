use inkwell::context::Context;
use inkwell::module::{Module};
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::execution_engine::ExecutionEngine;
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::error::Error;

use codegen_ir::{
    hir::*,
};

pub type ModuleMap<'a> = HashMap<String, Module<'a>>;

pub struct Executor<'a> {
    //modules: Vec<Module<'a>>,
    optimizer: PassManager<Module<'a>>,
    link_optimizer: PassManager<Module<'a>>,
    ee: Option<ExecutionEngine<'a>>
}

impl<'a> Executor<'a> {
    pub fn new(optimization_level: OptimizationLevel, size_level: u32) -> Self {
        let config = InitializationConfig::default();
        Target::initialize_native(&config).expect("Unable to initialize JIT");
        eprintln!(
            "Default: {:?}",
            TargetMachine::get_default_triple().as_str()
        );
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
            optimizer: pass_manager,
            link_optimizer: link_time_optimizations,
            ee: None
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
                module.print_to_stderr();
                self.add_module(module)?;
                //self.modules.push(module);

                Ok(())
            }
            Err(error) => {
                module.print_to_stderr();
                Err(error.into())
            }
        }
    }

    pub fn compile(&mut self, name: &str, ast: &Ast, context: &'a Context) -> Result<Module<'a>, Box<dyn Error>> {
        println!("AST: {}", &ast.to_ron());
        let mut defmap = crate::DefinitionMap::default();
        let module = crate::generate(&context, name, &ast, &mut defmap).unwrap();
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
