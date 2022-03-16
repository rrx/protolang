use crate::lexer::{Location, Surround};
use crate::tokens::Token;
use std::fmt::{Debug, Display};

pub trait Context: Debug + Display {
    fn from_location(loc: &Location) -> Self;
    fn from_token(token: &Token) -> Self;
}

#[derive(PartialEq, Debug, Clone, Default)]
pub struct NodeContextWithLocation {
    pub s: Surround,
    pub loc: Location,
}

pub type NodeContext = NodeContextWithLocation;

impl NodeContextWithLocation {
    pub fn move_token(token: Token) -> Self {
        let loc = token.to_location();
        Self { s: token.s, loc }
    }
}

impl std::fmt::Display for NodeContextWithLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "context()")
    }
}

impl Context for NodeContextWithLocation {
    fn from_location(loc: &Location) -> Self {
        Self {
            s: Surround::default(),
            loc: loc.clone(),
        }
    }

    fn from_token(token: &Token) -> Self {
        let loc = token.to_location();
        Self {
            s: token.s.clone(),
            loc: loc.clone(),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct NodeContextNull {}

impl Context for NodeContextNull {
    fn from_location(_: &Location) -> Self {
        Self {}
    }
    fn from_token(_: &Token) -> Self {
        Self {}
    }
}

impl std::fmt::Display for NodeContextNull {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "context()")
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct NodeWithLocationImpl<C: Context, V> {
    pub context: C,
    pub value: V,
}
