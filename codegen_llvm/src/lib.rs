mod backend;
mod codegen;
mod execute;

pub use backend::*;
pub use codegen::*;
pub use execute::*;

pub use inkwell::context::Context;
pub use inkwell::OptimizationLevel;
pub use inkwell::targets::{FileType, TargetMachine};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}
