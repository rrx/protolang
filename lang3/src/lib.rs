mod ast;
mod builder;
mod env;
mod printer;
mod types;
mod visitor;

pub use ast::*;
pub use builder::*;
use env::*;
use printer::*;
use std::fmt;
pub use types::*;
use visitor::*;

pub use logic::{UnifyResult, UnifyValue};
use serde::{
    ser::{SerializeStruct, Serializer},
    Serialize,
};

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

pub type UnifyExpr = logic::Expr<DefinitionId, Type, Ast>;
pub type ExprSeq = Vec<UnifyExpr>;
pub type SymbolTable = logic::SymbolTable<DefinitionId, Ast>;

pub type Environment = EnvLayers<String, Ast>;
impl LayerKey for String {}
