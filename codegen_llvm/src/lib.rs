mod backend;
mod codegen;
mod execute;

pub use backend::*;
pub use codegen::*;
pub use execute::*;

pub use inkwell::context::Context;
pub use inkwell::targets::{FileType, TargetMachine};
pub use inkwell::OptimizationLevel;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {}
}
