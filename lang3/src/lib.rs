mod types;
mod ast;
mod env;
mod builder;
mod visitor;

use types::*;
use ast::*;
use env::*;
use builder::*;
use visitor::*;

pub type SymbolTable = logic::SymbolTable<Type>;

pub type Environment = EnvLayers<String, Ast>;
impl LayerKey for String {}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
    }
}
