use crate::ast::*;
use crate::eval::*;
use crate::lexer::Location;
use crate::parser::Unparse;
use crate::sexpr::*;
use crate::tokens::Tok;
use std::{
    any::Any,
    fmt::{Debug, Display},
};
use std::fmt;

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
    fn call(
        & self,
        interp: &mut Interpreter,
        env: Environment,
        args: Vec<ExprRef>,
    ) -> Result<ExprRefWithEnv, InterpretError>;
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

    fn call(
        &self,
        interp: &mut Interpreter,
        env: Environment,
        args: Vec<ExprRef>,
    ) -> Result<ExprRefWithEnv, InterpretError> {
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


use std::collections::HashMap;
pub struct CallContainer {
    funcs: HashMap<String, Callback>
}

impl CallContainer {
    pub fn new() -> Self {
        Self { funcs: HashMap::new() }
    }

    pub fn add(&mut self, name: String, cb: Callback) {
        self.funcs.insert(name, cb);
    }
}

pub type Callback = Box<dyn Fn(Environment, Vec<ExprRef>) -> Result<ExprRefWithEnv, InterpretError> + 'static>;

//pub trait CallFunction: Clone + FnMut(&mut Interpreter, Environment, Vec<ExprRef>) -> Result<ExprRefWithEnv, InterpretError> + 'static {}

//pub type GenericCallable = Generic<Box<dyn CallFunction>>;

#[derive(Clone)]
pub struct Generic<F> where F: Clone {
    arity: usize,
    cb: F,
    //p: std::marker::PhantomData<&'a F>
}

impl<F> Generic<F> 
where F: Clone + Fn(&mut Interpreter, Environment, Vec<ExprRef>) -> Result<ExprRefWithEnv, InterpretError> {
    pub fn new(arity: usize, cb: F) -> Self {
        Self { cb, arity } //, p: std::marker::PhantomData }
    }

    fn call(
        &self,
        interp: &mut Interpreter,
        env: Environment,
        args: Vec<ExprRef>,
    ) -> Result<ExprRefWithEnv, InterpretError> {
        (self.cb)(interp, env, args)
    }

}

impl<F> fmt::Debug for Generic<F> where F: Clone {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<builtin fn generic>")
    }
}

impl<F> fmt::Display for Generic<F> where F: Clone {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<builtin fn generic>")
    }
}

