use crate::lexer::{Location, Surround};
use crate::tokens::Token;
use std::fmt::{Debug, Display};
/*
#[derive(PartialEq, Debug, Clone)]
pub enum PrattValue {
    Empty,
    Prefix(Tok, Box<ASTNode>),
    Binary(Tok, Box<ASTNode>, Box<ASTNode>),
    Ternary(Tok, Box<ASTNode>, Box<ASTNode>, Box<ASTNode>),
    Postfix(Tok, Box<ASTNode>),
    Literal(Tok),
    Ident(Tok),
    Error(String),
    List(Vec<ASTNode>),
    Chain(Tok, Vec<ASTNode>),
    Expr(Box<ASTNode>),
    Null,
    //Callable(CallableNode),
    Invalid(String),
}
*/

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

/*
impl Default for NodeContext {
    fn default() -> Self {
        Self { s: Surround::default(), loc: Location::default() }
    }
}
*/

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

/*
impl Unparse for ASTNode {
    fn unparse(&self) -> Vec<Tok> {
        let mut out = vec![];
        match &self.value {
            PrattValue::Empty => (),
            PrattValue::Null => (),
            PrattValue::Invalid(_) => (),
            PrattValue::Error(_) => (),
            PrattValue::Chain(_,_) => (),
            PrattValue::Ternary(_,_,_,_) => (),
            PrattValue::Ident(x) => {
                out.push(x.clone());
                //out.append(&mut x.unparse());
            }
            PrattValue::Literal(x) => {
                out.push(x.clone());
            }
            PrattValue::Expr(x) => {
                out.append(&mut x.unparse());
            }
            PrattValue::Prefix(tok, expr) => {
                out.push(tok.clone());
                out.append(&mut expr.unparse());
            }
            PrattValue::Postfix(tok, expr) => {
                out.append(&mut expr.unparse());
                out.push(tok.clone());
            }
            PrattValue::Binary(op, left, right) => {
                out.append(&mut left.unparse());
                out.push(op.clone());
                //out.append(&mut op.unparse());
                out.append(&mut right.unparse());
            }
            PrattValue::List(elements) => {
                for e in elements {
                    out.append(&mut e.unparse());
                }
            }
        };
        self.context.s.unparse(out)
    }
}
impl SExpr for ASTNode {
    fn sexpr(&self) -> SResult<S> {
        match &self.value {
            PrattValue::Expr(x) => x.sexpr(),
            //PrattValue::Ident(x) => x.sexpr(),
            PrattValue::Binary(op, left, right) => {
                let sleft = left.sexpr()?;
                let sright = right.sexpr()?;
                Ok(S::Cons(op.unlex(), vec![sleft, sright]))
            }
            PrattValue::Prefix(prefix, expr) => {
                let s = expr.sexpr()?;
                Ok(S::Cons(prefix.unlex(), vec![s]))
            }
            PrattValue::Postfix(postfix, expr) => {
                let s = expr.sexpr()?;
                Ok(S::Cons(postfix.unlex(), vec![s]))
            }
            PrattValue::List(elements) => {
                let s_args = elements
                    .iter()
                    .filter_map(|a| a.sexpr().ok())
                    .collect::<Vec<_>>();
                Ok(S::Cons("list".into(), s_args))
            }
            _ => unreachable!()
        }
    }
}

pub type ASTNode = ASTNodeWithLocationImpl<NodeContextWithLocation>;

impl ASTNode {
    pub fn new(value: PrattValue, loc: &Location) -> Self {
        Self {
            context:  NodeContextWithLocation::from_location(loc),
            value,
        }
    }
}
*/
