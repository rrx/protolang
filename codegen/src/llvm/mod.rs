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

use crate::lower::Lower;
use codegen_ir::hir::Ast;
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

    pub fn compile_module(&mut self, name: &str, ast: &Ast) -> Result<(), Box<dyn Error>> {
        self.lower.compile_module(&self.context.context, name, ast)
    }

    pub fn run(&self) -> Result<i64, Box<dyn Error>> {
        self.lower.run(&self.context.context)
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
