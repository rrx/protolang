mod backend;
mod codegen;
mod execute;
mod livelink;

pub use backend::*;
pub use codegen::*;
pub use execute::*;
pub use livelink::*;

pub use inkwell::context::Context;
pub use inkwell::OptimizationLevel;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}
