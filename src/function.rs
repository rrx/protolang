use crate::sexpr::*;
use crate::ast::*;
use crate::value::*;
use crate::interpreter::*;
use crate::tokens::{Tok, Token};
use std::{
    any::Any,
    fmt::{Debug, Display}
};

pub trait Callable: Debug + Display {
    fn arity(&self) -> usize;
    fn call(&self, interp: &mut Interpreter, args: Vec<Value>) -> Result<Value, InterpretError>;
    fn box_clone(&self) -> Box<dyn Callable>;
    fn as_any(&self) -> &dyn Any;
}

impl Clone for Box<dyn Callable> {
    fn clone(&self) -> Self {
        self.box_clone()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Params {
    pub s: Surround,
    pub value: Vec<Ident>,
}
impl Params {
    pub fn new(value: Vec<Ident>) -> Self {
        Self {
            s: Surround::default(),
            value,
        }
    }
}
impl Unparse for Params {
    fn unparse(&self) -> Vec<Tok> {
        self.s.unparse(self.value.iter().map(|s| s.unparse()).flatten().collect::<Vec<_>>())
    }

    fn unlex(&self) -> String {
        self.unparse().iter().map(|t| t.unlex()).collect::<Vec<_>>().join("")
    }
}
impl SExpr for Params {
    fn sexpr(&self) -> SResult<S> {
        let params = self.value.iter().map_while(|v| v.sexpr().ok()).collect::<Vec<_>>();
        if params.len() < self.value.len() {
            return Err(SError::Invalid("Unable to parse parameters".into()));
        }
        Ok(S::Cons("params".into(), params))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Lambda {
    pub s: Surround,
    pub params: Params,
    pub expr: Box<StmtNode>,
    pub loc: Location,
}
impl Lambda {
    pub fn new(params: Params, expr: StmtNode, loc: Location) -> Self {
        Self {
            s: Surround::default(),
            params, expr: Box::new(expr),
            loc
        }
    }
}

impl std::fmt::Display for Lambda {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<func arity:{}>", self.arity())
    }
}


impl Callable for Lambda {
    fn arity(&self) -> usize {
        self.params.value.len()
    }

    fn call(&self, interpreter: &mut Interpreter, args: Vec<Value>) -> Result<Value, InterpretError> {
        Ok(Value::IntLiteral(0))
    }

    fn box_clone(&self) -> Box<dyn Callable> {
        Box::new((*self).clone())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl Unparse for Lambda  {
    fn unparse(&self) -> Vec<Tok> {
        vec![vec![Tok::Backslash], self.params.unparse(), vec![Tok::LeftArrow], self.expr.unparse()].into_iter().flatten().collect()
    }

    fn unlex(&self) -> String {
        vec![self.params.unlex(), Tok::LeftArrow.unlex(), self.expr.unlex()].join("")
    }
}
impl SExpr for Lambda {
    fn sexpr(&self) -> SResult<S> {
        Ok(S::Cons("lambda".into(), vec![self.params.sexpr()?, self.expr.sexpr()?]))
    }
}


