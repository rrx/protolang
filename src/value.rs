use crate::ast::{Ident, Unparse};
use crate::function::CallableNode;
use crate::sexpr::*;
use crate::tokens::Tok;

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
        match token {
            Tok::IntLiteral(u) => Some(Value::IntLiteral(u)),
            Tok::FloatLiteral(u) => Some(Value::FloatLiteral(u)),
            Tok::StringLiteral(s) => Some(Value::StringLiteral(s.clone())),
            Tok::BoolLiteral(b) => Some(Value::BoolLiteral(b)),
            //Tok::Invalid(s) => Some(Value::Invalid(s.clone())),
            _ => None,
        }
    }
    pub fn token(&self) -> Tok {
        use Value::*;
        match &self {
            IntLiteral(x) => Tok::IntLiteral(*x),
            FloatLiteral(x) => Tok::FloatLiteral(*x),
            BoolLiteral(x) => Tok::BoolLiteral(*x),
            StringLiteral(x) => Tok::StringLiteral(x.clone()),
            List(x) => Tok::Ident("list".into()),
            Invalid(x) => Tok::Invalid(x.clone()),
            Null => Tok::Null,
            Callable(x) => Tok::Ident("callable".into()), //x.unlex())
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
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
            Callable(s) => Ok(S::Atom("callable".into())),
        }
    }
}
