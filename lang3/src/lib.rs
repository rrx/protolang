mod ast;
mod builder;
mod env;
mod types;
mod visitor;
mod printer;

pub use ast::*;
pub use builder::*;
use env::*;
pub use types::*;
use visitor::*;
use printer::*;

pub type SymbolTable = logic::SymbolTable<Type>;

pub type Environment = EnvLayers<String, Ast>;
impl LayerKey for String {}

