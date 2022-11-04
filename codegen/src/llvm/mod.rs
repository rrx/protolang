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

mod builtin;
mod compile;
mod decisiontree;
pub mod generator;

pub use compile::*;
use generator::*;
use inkwell::context::Context;

use crate::hir::Ast;
use crate::lower::Lower;
use inkwell::OptimizationLevel;
use std::error::Error;

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
    lower: Lower<'a>,
}

impl<'a> LLVMBackend<'a> {
    pub fn new(context: &'a LLVMBackendContext) -> Self {
        let lower = Lower::new(OptimizationLevel::None, 0);
        Self { context, lower }
    }

    pub fn compile_module(&mut self, name: &str, ast: Ast) -> Result<(), Box<dyn Error>> {
        self.lower.compile_module(&self.context.context, name, ast)
    }

    pub fn run(&self) -> Result<i64, Box<dyn Error>> {
        self.lower.run(&self.context.context)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hir::*;
    use crate::lower::{self, Definitions};

    #[test]
    fn codegen_fib() {
        let mut defs = Definitions::new();
        /*
        function fibonacci(a, b, n) {
          if (n === 0) {
            return a;
          }

          if (n === 1) {
            return b;
          }

           return fibonacci(b, a + b, n - 1);
        }
        */

        // fib function type
        let typ = FunctionType::export(vec![Type::i64(), Type::i64(), Type::i64(), Type::i64()]);

        // variable for the function
        let fib = defs.named_variable("fib");
        // variable for the single parameter in the function
        let a = defs.new_variable();
        let b = defs.new_variable();
        let n = defs.new_variable();

        let eq0 = lower::eq(n.clone().into(), Ast::i64(0));

        let ret_a: Ast = Return::new(a.clone().into()).into();
        let branch1 = If {
            condition: eq0.into(),
            then: ret_a.into(),
            otherwise: None,
            result_type: Type::Primitive(PrimitiveType::Unit),
        };

        let eq1 = lower::eq(n.clone().into(), Ast::i64(1));
        let ret_b: Ast = Return::new(b.clone().into()).into();
        let branch2 = If {
            condition: eq1.into(),
            then: ret_b.into(),
            otherwise: None,
            result_type: Type::Primitive(PrimitiveType::Unit),
        };

        // call the function that we've created and then increment
        let call = FunctionCall::new(
            fib.clone().into(),
            vec![
                b.clone().into(),
                lower::add(a.clone().into(), b.clone().into()),
                lower::sub(n.clone().into(), Ast::i64(1)),
            ],
            typ.clone(),
        );

        let block = Sequence::new(vec![branch1.into(), branch2.into(), call.into()]);
        // create a function to associate with the variable
        let f = Lambda::new(vec![a.clone(), b.clone(), n.clone()], block.into(), typ.clone());

        // define the function using the definition id
        let dfib = Definition::variable(fib.clone(), f.into());

        let call = FunctionCall::new(fib.clone().into(), vec![Ast::i64(0), Ast::i64(1), Ast::i64(10)], typ.clone());

        // single parameter function type for main
        let typ = FunctionType::export(vec![Type::i64(), Type::i64()]);
        let f_main = Lambda::new(vec![], call.into(), typ.clone());
        let df_main = defs.new_definition("main", f_main.into());

        let context = LLVMBackendContext::new();
        let mut b = context.backend();
        b.compile_module("main", Sequence::new(vec![dfib.into(), df_main.into()]).into()).unwrap();
        let ret = b.run().unwrap();
        assert_eq!(55, ret);
    }

    #[test]
    fn codegen_extern() {
        let mut defs = Definitions::new();

        // single parameter function type
        let typ = FunctionType::export(vec![Type::i64(), Type::i64()]);

        let x1 = {
            // x1(x) => x+1
            // increment by 1
            let p = defs.new_variable();
            Lambda::new(vec![p.clone()], lower::add(p.clone().into(), Ast::i64(1)), typ.clone())
        };
        let dx1 = defs.new_definition("x1", x1.into());

        // main function
        let extern1 = Extern::new("x1".to_string(), typ.clone().into());
        // call extern
        let call_extern = FunctionCall::new(extern1.clone().into(), vec![Ast::i64(10)], typ.clone());
        let f_main = Lambda::new(vec![], call_extern.into(), typ.clone());

        let df_main = defs.new_definition("main", f_main.into());
        let context = LLVMBackendContext::new();
        let mut b = context.backend();
        b.compile_module("test", Sequence::new(vec![dx1.into()]).into()).unwrap();
        b.compile_module("main", Sequence::new(vec![df_main.into()]).into()).unwrap();
        let ret = b.run().unwrap();
        assert_eq!(11, ret);
    }
}
