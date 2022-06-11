use crate::lexer::{Location, Surround};
use crate::tokens::{FileId, Tok, Token};
use std::fmt::{Debug, Display};
use std::marker::Sized;

pub trait FromContext: Debug + Display + Clone {
    fn from_location(loc: &Location) -> Self
    where
        Self: Sized;
    fn from_token(token: &Token) -> Self
    where
        Self: Sized;
}

pub trait Context: Debug + Display {
    fn pre(&self) -> Vec<Tok>;
    fn post(&self) -> Vec<Tok>;
    fn box_clone(&self) -> Box<dyn Context>;
}

impl Clone for Box<dyn Context> {
    fn clone(&self) -> Self {
        self.box_clone()
    }
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

impl FromContext for NodeContextWithLocation {
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

impl Context for NodeContextWithLocation {
    fn pre(&self) -> Vec<Tok> {
        self.s.pre.clone()
    }
    fn post(&self) -> Vec<Tok> {
        self.s.post.clone()
    }
    fn box_clone(&self) -> Box<dyn Context> {
        Box::new((*self).clone())
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct MaybeNodeContext(Option<NodeContext>);

impl Default for MaybeNodeContext {
    fn default() -> Self {
        Self(None)
    }
}

impl MaybeNodeContext {
    pub fn move_token(token: Token) -> Self {
        MaybeNodeContext(Some(NodeContext::move_token(token)))
    }

    pub fn has_location(&self) -> bool {
        self.0.is_some()
    }

    pub fn get_file_id(&self) -> FileId {
        self.to_location().file_id
    }

    pub fn to_location(&self) -> Location {
        match &self.0 {
            Some(c) => c.loc.clone(),
            None => Location::default(),
        }
    }

    pub fn unparse(&self, tokens: Vec<Tok>) -> Vec<Tok> {
        match &self.0 {
            Some(c) => c.s.unparse(tokens),
            None => vec![],
        }
    }

    pub fn append(&mut self, tokens: Vec<Tok>) {
        match &mut self.0 {
            Some(c) => c.s.append(tokens),
            None => (),
        }
    }

    pub fn prepend(&mut self, tokens: Vec<Tok>) {
        match &mut self.0 {
            Some(c) => c.s.prepend(tokens),
            None => (),
        }
    }

    pub fn line(&self) -> usize {
        match &self.0 {
            Some(c) => c.loc.line,
            None => 0,
        }
    }

    pub fn range(&self) -> std::ops::Range<usize> {
        let offset = match &self.0 {
            Some(c) => c.loc.offset,
            None => 0,
        };
        offset..offset
    }
}

impl std::fmt::Display for MaybeNodeContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "context()")
    }
}

impl FromContext for MaybeNodeContext {
    fn from_location(v: &Location) -> Self {
        MaybeNodeContext(Some(NodeContext::from_location(v)))
    }
    fn from_token(v: &Token) -> Self {
        MaybeNodeContext(Some(NodeContext::from_token(v)))
    }
}

impl Context for MaybeNodeContext {
    fn pre(&self) -> Vec<Tok> {
        match &self.0 {
            Some(c) => c.s.pre.clone(),
            None => vec![],
        }
    }
    fn post(&self) -> Vec<Tok> {
        match &self.0 {
            Some(c) => c.s.post.clone(),
            None => vec![],
        }
    }
    fn box_clone(&self) -> Box<dyn Context> {
        Box::new((*self).clone())
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct NodeContextNull {}

impl FromContext for NodeContextNull {
    fn from_location(_: &Location) -> Self {
        Self {}
    }
    fn from_token(_: &Token) -> Self {
        Self {}
    }
}

impl Context for NodeContextNull {
    fn pre(&self) -> Vec<Tok> {
        vec![]
    }
    fn post(&self) -> Vec<Tok> {
        vec![]
    }
    fn box_clone(&self) -> Box<dyn Context> {
        Box::new((*self).clone())
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
