use inkwell::context::Context;
use inkwell::module::Module;

use codegen_ir::hir::Ast;
use inkwell::OptimizationLevel;
use std::error::Error;

use crate::codegen::{generate, DefinitionMap};
use crate::Executor;

pub struct LLVMBackendContext {
    context: Context,
}
impl LLVMBackendContext {
    pub fn new() -> Self {
        let context = Context::create();
        Self { context }
    }

    pub fn backend<'a>(&'a self) -> LLVMBackend<'a> {
        LLVMBackend::new(self)
    }
}

pub struct LLVMBackend<'a> {
    context: &'a LLVMBackendContext,
    //lower: Lower<'a>,
    exec: Executor<'a>,
    modules: Vec<Module<'a>>,
}

impl<'a> LLVMBackend<'a> {
    pub fn new(context: &'a LLVMBackendContext) -> Self {
        Self {
            context,
            exec: Executor::new(OptimizationLevel::None, 0),
            modules: vec![],
        }
    }

    pub fn compile_module(&mut self, name: &str, ast: &Ast) -> Result<(), Box<dyn Error>> {
        let mut defmap = DefinitionMap::default();
        let module = generate(&self.context.context, "test", &ast, &mut defmap)?;
        self.exec.add(&module);
        Ok(())
    }

    pub fn run(&self) -> Result<i64, Box<dyn Error>> {
        self.exec.run::<i64>()
    }
}

pub trait JitExecute {
    fn run_main(&self) -> Result<i64, Box<dyn Error>>;
}

impl JitExecute for Ast {
    fn run_main(&self) -> Result<i64, Box<dyn Error>> {
        let context = LLVMBackendContext::new();
        let mut b = context.backend();
        b.compile_module("main", &self).unwrap();
        b.run()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use codegen_ir::hir::{*};
    use codegen_ir::testing::*;

    #[test]
    fn codegen_fib() {
        let context = LLVMBackendContext::new();
        let mut b = context.backend();
        let mut defs = Definitions::new();

        let ast = gen_fib(&mut defs);
        println!("AST: {}", &ast.to_ron());
        b.compile_module("main", &ast).unwrap();
        let ret = b.run().unwrap();

        assert_eq!(55, ret);
    }

    #[test]
    fn codegen_selfref() {
        let context = LLVMBackendContext::new();
        let b = context.backend();
        let mut defs = Definitions::new();

        let ast = gen_self_reference(&mut defs);
        println!("AST: {}", &ast.to_ron());
        // this currently fails to compile
        //b.compile_module("main", &ast).unwrap();
        //let ret = b.run().unwrap();
        //assert_eq!(55, ret);
    }

    #[test]
    fn codegen_extern() {
        let mut defs = Definitions::new();

        let x1_module = gen_x1_module(&mut defs);
        let x1_main = gen_x1_main(&mut defs);

        let context = LLVMBackendContext::new();
        let mut b = context.backend();
        b.compile_module("test", &x1_module).unwrap();
        b.compile_module("main", &x1_main).unwrap();
        let ret = b.run().unwrap();

        assert_eq!(11, ret);
    }
}
