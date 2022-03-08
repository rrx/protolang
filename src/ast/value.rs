use crate::ast::{Unparse};
use super::function::CallableNode;
use crate::sexpr::*;
use crate::tokens::Tok;
use std::convert::TryFrom;
use itertools::Itertools;

#[derive(Debug, Clone)]
pub enum Value {
    IntLiteral(u64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    StringLiteral(String),
    List(Vec<Value>),
    Null,
    Callable(CallableNode),
    Invalid(String),
}

impl Value {
    pub fn from_token(token: Tok) -> Option<Value> {
        Self::try_from(&token).ok()
    }
}

impl Unparse for Value {
    fn unparse(&self) -> Vec<Tok> {
        match &self {
            Self::IntLiteral(x) => vec![Tok::IntLiteral(*x)],
            Self::FloatLiteral(x) => vec![Tok::FloatLiteral(*x)],
            Self::BoolLiteral(x) => vec![Tok::BoolLiteral(*x)],
            Self::StringLiteral(x) => vec![Tok::StringLiteral(x.clone())],
            Self::List(x) => {
                let tokens_list = x.clone().into_iter().map(|v| v.unparse()).collect::<Vec<Vec<Tok>>>();
                vec![
                    vec![Tok::LBracket],
                    tokens_list.into_iter().intersperse(vec![Tok::Comma]).flatten().collect::<Vec<_>>(),
                    vec![Tok::RBracket]
                ].concat()
            }
            Self::Invalid(x) => vec![Tok::Invalid(x.clone())],
            Self::Null => vec![Tok::Null],
            Self::Callable(_) => vec![Tok::Ident("callable".into())],
        }
    }
}

impl TryFrom<&Tok> for Value {
    type Error = ();
    fn try_from(value: &Tok) -> Result<Self, Self::Error> {
        match value {
            Tok::IntLiteral(u) => Ok(Value::IntLiteral(*u)),
            Tok::FloatLiteral(u) => Ok(Value::FloatLiteral(*u)),
            Tok::StringLiteral(s) => Ok(Value::StringLiteral(s.clone())),
            Tok::BoolLiteral(b) => Ok(Value::BoolLiteral(*b)),
            Tok::Null => Ok(Value::Null),
            //Tok::Invalid(s) => Some(Value::Invalid(s.clone())),
            _ => Err(()),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl SExpr for Value {
    fn sexpr(&self) -> SResult<S> {
        use Value::*;
        match &self {
            IntLiteral(x) => Ok(S::Atom(x.to_string())),
            FloatLiteral(x) => Ok(S::Atom(x.to_string())),
            BoolLiteral(x) => Ok(S::Atom(x.to_string())),
            StringLiteral(x) => Ok(S::Atom(x.to_string())),
            List(values) => {
                let s_args = values
                    .iter()
                    .filter_map(|a| a.sexpr().ok())
                    .collect::<Vec<_>>();
                Ok(S::Cons("list".into(), s_args))
            }
            Invalid(s) => Err(SError::Invalid(s.clone())),
            Null => Ok(S::Null),
            Callable(_) => Ok(S::Atom("callable".into())),
        }
    }
}
