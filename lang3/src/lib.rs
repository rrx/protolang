mod ast;
mod builder;
mod env;
mod types;
mod visitor;

use ast::*;
use builder::*;
use env::*;
use types::*;
use visitor::*;

pub type SymbolTable = logic::SymbolTable<Type>;

pub type Environment = EnvLayers<String, Ast>;
impl LayerKey for String {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {}
}
