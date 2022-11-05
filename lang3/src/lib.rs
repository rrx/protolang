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
use std::fmt;

use logic::{UnifyResult, UnifyValue};
use serde::{Serialize, ser::{Serializer, SerializeStruct}};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Serialize)]
pub struct DefinitionId(pub usize);
impl logic::UnifyKey for DefinitionId {}

impl From<usize> for DefinitionId {
    fn from(item: usize) -> Self {
        Self(item)
    }
}

impl fmt::Display for DefinitionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Debug for DefinitionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub type UnifyExpr = logic::Expr<DefinitionId, Type>;
pub type ExprSeq = Vec<UnifyExpr>;
pub type SymbolTable = logic::SymbolTable<DefinitionId, Type>;

pub type Environment = EnvLayers<String, Ast>;
impl LayerKey for String {}

