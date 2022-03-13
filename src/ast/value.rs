use crate::ast::{Unparse};
use super::function::CallableNode;
use crate::sexpr::*;
use crate::tokens::Tok;
use std::convert::TryFrom;
use itertools::Itertools;
//use crate::pratt::{PrattValue};

#[derive(Debug, Clone)]
pub enum Value {
    //IntLiteral(u64),
    //FloatLiteral(f64),
    //BoolLiteral(bool),
    Literal(Tok),
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
            //Self::IntLiteral(x) => vec![Tok::IntLiteral(*x)],
            //Self::FloatLiteral(x) => vec![Tok::FloatLiteral(*x)],
            //Self::BoolLiteral(x) => vec![Tok::BoolLiteral(*x)],
            Self::Literal(x) => vec![x.clone()],//Tok::StringLiteral(x.clone())],
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
/*
impl TryFrom<&PrattValue> for Value {
    type Error = ();
    fn try_from(value: &PrattValue) -> Result<Self, Self::Error> {
        match value {
            PrattValue::Literal(v) => Ok(Value::Literal(v.clone())),
            PrattValue::Null => Ok(Value::Null),
            //PrattValue::Empty => Ok(Value::Null),
            //Callable(CallableNode),
            //Invalid(String),
            //Tok::Invalid(s) => Some(Value::Invalid(s.clone())),
            _ => Err(()),
        }
    }
}
*/

impl TryFrom<&Tok> for Value {
    type Error = ();
    fn try_from(value: &Tok) -> Result<Self, Self::Error> {
        match value {
            Tok::IntLiteral(_) => Ok(Value::Literal(value.clone())),
            Tok::FloatLiteral(_) => Ok(Value::Literal(value.clone())),
            Tok::StringLiteral(_) => Ok(Value::Literal(value.clone())),//s.clone())),
            Tok::BoolLiteral(_) => Ok(Value::Literal(value.clone())),
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
            //IntLiteral(x) => Ok(S::Atom(x.to_string())),
            //FloatLiteral(x) => Ok(S::Atom(x.to_string())),
            //BoolLiteral(x) => Ok(S::Atom(x.to_string())),
            Literal(x) => Ok(S::Atom(x.unlex())),
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
