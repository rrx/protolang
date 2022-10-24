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
