use crate::tokens::Tok;
use crate::ast::{Ident, Unparse};

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    IntLiteral(u64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    StringLiteral(String),
    Null,
    Callable(Ident),
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
            Invalid(x) => Tok::Invalid(x.clone()),
            Null => Tok::Null,
            Callable(x) => Tok::Ident(x.unlex())
        }
    }
}


