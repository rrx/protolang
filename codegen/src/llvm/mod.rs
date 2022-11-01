//! llvm/mod.rs - Defines the LLVM backend for ante's codegen pass.
//! Currently, there are no other backends, but in the future the codegen
//! pass may have the choice between several backends for e.g. faster debug builds.
//!
//! The codegen pass follows the lifetime inference pass, and is the final pass of
//! the compiler. The goal of this pass is to produce native code that is executable
//! by a computer. The majority of this pass is implemented via the CodeGen trait
//! which walks the Ast with a Generator for context. This walk starts in the main
//! function and lazily codegens each Definition that is used so that only what is
//! used is actually compiled into the resulting binary. Once this walk is finished
//! the resulting inkwell::Module is optimized then linked with gcc.
//!
//! Note that ante currently does whole program compilation - the entire program
//! is compiled into a single inkwell::Module which can then be optimized later.
//! Any libraries need to have their source code included anyway since ante does
//! not have a stable ABI.
//!
//! The reccomended starting point while reading through this pass is the `run`
//! function which is called directly from `main`. This function sets up the
//! Generator, walks the Ast, then optimizes and links the resulting Module.

mod builtin;
mod compile;
mod decisiontree;
pub mod generator;

pub use compile::*;
use generator::*;
use inkwell::context::Context;

use crate::lower::Lower;
use crate::hir::Ast;

use std::error::Error;
use std::rc::Rc;

pub struct LLVMBackendContext {
    context: Context
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
    lower: Lower<'a>
}

impl<'a> LLVMBackend<'a> {
    pub fn new(context: &'a LLVMBackendContext) -> Self {
        let lower = Lower::new();
        Self { context, lower }
    }

    pub fn module(&mut self, name: &str, ast: Ast) -> Result<(), Box<dyn Error>> {
        self.lower.module(&self.context.context, name, ast).unwrap();
        Ok(())
    }

    pub fn run(&self) -> Result<i64, Box<dyn Error>> {
        self.lower.run(&self.context.context)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lower::{self, Definitions};
    use crate::hir::*;

    #[test]
    fn test_recursive() {
        let mut defs = Definitions::new();

        // single parameter function type
        let typ = FunctionType::export(vec![Type::i64(), Type::i64()]);
        
        // variable for the function
        let xv0 = defs.named_variable("v0");
        // variable for the single parameter in the function
        let param = defs.new_variable();
        
        // call the function that we've created and then increment
        let call = FunctionCall::new(xv0.clone().into(), vec![param.clone().into()], typ.clone());
        let call_and_add = lower::add(Ast::i64(1), call.clone().into());
        
        // increment param as the body of the function
        let condition = lower::lt(param.clone().into(), Ast::i64(10));
        let then: Ast = call_and_add.into();//lower::add(Ast::i64(1), param.clone().into());
        let otherwise = Ast::i64(1000);
        let result_type = Type::i64();
        let branch = If {
            condition: condition.into(),
            then: then.into(),
            otherwise: Some(otherwise.into()),
            result_type
        };

        // create a function to associate with the variable
        let f = Lambda::new(vec![param.clone()], branch.into(), typ.clone());

        // define the function using the definition id
        let xd0 = Definition::variable(xv0.clone(), f.into());
        
        // call the recursive function
        let call = FunctionCall::new(xv0.clone().into(), vec![Ast::i64(0)], typ.clone());

        // main function
        let extern1 = Extern::new("v0".to_string(), typ.clone().into());
        // call extern
        let call_extern = FunctionCall::new(extern1.clone().into(), vec![Ast::i64(10)], typ.clone());

        let f_main = Lambda::new(vec![], call_extern.into(), typ.clone());

        let df_main = defs.new_definition("main", f_main.into());

        //let asts = vec![xd0.into(), df_main.into()];

        let context = LLVMBackendContext::new();
        let mut b = context.backend();
        b.module("test", Sequence::new(vec![xd0.into()]).into()).unwrap();
        b.module("main", Sequence::new(vec![df_main.into()]).into()).unwrap();
        b.run().unwrap();
    }
}
