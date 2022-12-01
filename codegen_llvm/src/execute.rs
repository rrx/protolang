use codegen_ir::Ast;
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{CodeModel, RelocMode};
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::error::Error;

pub type ModuleMap<'a> = HashMap<String, Module<'a>>;

pub struct Executor<'a> {
    optimizer: PassManager<Module<'a>>,
    ee: Option<ExecutionEngine<'a>>,
    pub target_machine: TargetMachine,
}

impl<'a> Executor<'a> {
    pub fn create(
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

        log::debug!("Default: {:?}", triple.as_str());
        log::debug!("Host: {}", TargetMachine::get_host_cpu_name().to_str()?);

        let pass_manager_builder = PassManagerBuilder::create();

        pass_manager_builder.set_optimization_level(optimization_level);
        pass_manager_builder.set_size_level(size_level);

        let pass_manager = PassManager::create(());
        pass_manager_builder.populate_module_pass_manager(&pass_manager);

        // Do LTO optimizations afterward mosty for function inlining
        let link_time_optimizations = PassManager::create(());
        pass_manager_builder.populate_lto_pass_manager(&link_time_optimizations, false, true);

        Ok(Self {
            optimizer: pass_manager,
            ee: None,
            target_machine,
        })
    }

    pub fn remove(&mut self, module: &Module<'a>) -> Result<(), Box<dyn Error>> {
        Ok(match self.ee.as_ref() {
            Some(ee) => ee.remove_module(module)?,
            None => (),
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
        match module.verify() {
            Ok(_) => {
                self.add_module(module)?;
                Ok(())
            }
            Err(error) => {
                let s = module.print_to_string();
                log::debug!("Module: {}", s);
                Err(error.into())
            }
        }
    }

    pub fn compile(
        &mut self,
        name: &str,
        ast: &Ast,
        context: &'a Context,
    ) -> Result<Module<'a>, Box<dyn Error>> {
        log::debug!("AST: {}", &ast.to_ron());
        let mut defmap = crate::DefinitionMap::default();
        let module = crate::generate(&context, name, &ast, &mut defmap).unwrap();
        self.optimizer.run_on(&module);
        module.verify()?;
        Ok(module)
    }

    pub fn run<T>(&self) -> Result<T, Box<dyn Error>> {
        unsafe {
            let f = self
                .ee
                .as_ref()
                .unwrap()
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

    use codegen_ir::hir::*;
    use codegen_ir::testing::*;

    #[test]
    fn jit_module_reload() {
        let context = Context::create();
        let mut defs = Definitions::new();
        let ast = gen_fib(&mut defs);
        let mut defmap = crate::DefinitionMap::default();
        let module = crate::generate(&context, "test", &ast, &mut defmap).unwrap();

        let mut e = Executor::create(OptimizationLevel::None, 0).unwrap();
        e.add(&module).unwrap();
        let ret = e.run::<i64>().unwrap();
        assert_eq!(ret, 55);
        e.remove(&module).unwrap();

        let mut defmap = crate::DefinitionMap::default();
        let module = crate::generate(&context, "test", &ast, &mut defmap).unwrap();

        e.add(&module).unwrap();
        let ret = e.run::<i64>().unwrap();
        assert_eq!(ret, 55);
        e.remove(&module).unwrap();
    }
}
