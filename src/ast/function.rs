use crate::ast::*;
use crate::eval::*;
use crate::lexer::Location;
use crate::parser::Unparse;
use crate::sexpr::*;
use crate::tokens::Tok;
use std::fmt;
use std::fmt::{Debug, Display};

#[derive(Debug, Clone)]
pub struct CallableNode {
    pub value: Box<dyn Callable>,
    pub context: MaybeNodeContext,
}

impl CallableNode {
    pub fn new(value: Box<dyn Callable>, loc: Location) -> Self {
        Self {
            value,
            context: MaybeNodeContext::from_location(&loc),
        }
    }
}

pub trait Callable: Debug + Display {
    fn arity(&self) -> usize {
        0
    }
    fn call<'a>(
        &self,
        interp: &mut Interpreter<'a>,
        env: Environment<'a>,
        args: Vec<ExprRef>,
    ) -> Result<ExprRefWithEnv<'a>, InterpretError>;
    fn box_clone(&self) -> Box<dyn Callable>;
    //fn as_any(&self) -> &dyn Any;
}

impl Clone for Box<dyn Callable> {
    fn clone(&self) -> Self {
        self.box_clone()
    }
}

#[derive(Debug, Clone)]
pub struct Params {
    pub context: MaybeNodeContext,
    pub value: Vec<ExprNode>,
}
impl Params {
    pub fn new(value: Vec<ExprNode>, loc: &Location) -> Self {
        Self {
            context: MaybeNodeContext::from_location(loc),
            value,
        }
    }
}
impl Unparse for Params {
    fn unparse(&self) -> Vec<Tok> {
        self.context.unparse(
            self.value
                .iter()
                .map(|s| s.unparse())
                .flatten()
                .collect::<Vec<_>>(),
        )
    }

    fn unlex(&self) -> String {
        self.unparse()
            .iter()
            .map(|t| t.unlex())
            .collect::<Vec<_>>()
            .join("")
    }
}
impl SExpr for Params {
    fn sexpr(&self) -> SResult<S> {
        let params = self
            .value
            .iter()
            .map_while(|v| v.sexpr().ok())
            .collect::<Vec<_>>();
        if params.len() < self.value.len() {
            return Err(SError::Invalid("Unable to parse parameters".into()));
        }
        Ok(S::Cons("params".into(), params))
    }
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub context: MaybeNodeContext,
    pub params: Params,
    pub expr: Box<ExprNode>,
}
impl Lambda {
    pub fn new(params: Params, expr: ExprNode, loc: &Location) -> Self {
        Self {
            context: MaybeNodeContext::from_location(loc),
            params,
            expr: Box::new(expr),
        }
    }
    pub fn node(&self) -> CallableNode {
        let c = self.box_clone();
        CallableNode::new(c, self.context.to_location())
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

    fn call<'a>(
        &self,
        interp: &mut Interpreter<'a>,
        env: Environment<'a>,
        args: Vec<ExprRef>,
    ) -> Result<ExprRefWithEnv<'a>, InterpretError> {
        interp.call(env, self, &self.params, &args, self.expr.clone().into())
    }

    fn box_clone(&self) -> Box<dyn Callable> {
        Box::new((*self).clone())
    }

    //fn as_any(&self) -> &dyn Any {
    //self
    //}
}

impl Unparse for Lambda {
    fn unparse(&self) -> Vec<Tok> {
        vec![
            vec![Tok::Backslash],
            self.params.unparse(),
            vec![Tok::LeftArrow],
            self.expr.unparse(),
        ]
        .into_iter()
        .flatten()
        .collect()
    }

    fn unlex(&self) -> String {
        vec![
            self.params.unlex(),
            Tok::LeftArrow.unlex(),
            self.expr.unlex(),
        ]
        .join("")
    }
}
impl SExpr for Lambda {
    fn sexpr(&self) -> SResult<S> {
        Ok(S::Cons(
            "lambda".into(),
            vec![self.params.sexpr()?, self.expr.sexpr()?],
        ))
    }
}
