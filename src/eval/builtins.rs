use crate::{
    ast::{Callable, Expr},
    eval::{Environment, ExprRef, ExprRefWithEnv, InterpretError, Interpreter},
    tokens::Tok,
};
use std::{
    any::Any,
    fmt,
    time::{SystemTime, UNIX_EPOCH},
};

#[derive(Clone, Debug)]
pub struct ShowStack;
impl ShowStack {
    pub fn value() -> Expr {
        Expr::Callable(Box::new(Self))
    }
}

impl fmt::Display for ShowStack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<builtin fn showstack>")
    }
}

impl Callable for ShowStack {
    fn call(
        &self,
        _: &mut Interpreter,
        env: Environment,
        _: Vec<ExprRef>,
    ) -> Result<ExprRefWithEnv, InterpretError> {
        env.debug();
        Ok(ExprRefWithEnv::new(Expr::Void.into(), env))
    }

    fn box_clone(&self) -> Box<dyn Callable> {
        Box::new((*self).clone())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone, Debug)]
pub struct Clock;

impl Clock {
    pub fn value() -> Expr {
        Expr::Callable(Box::new(Self))
    }
}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<builtin fn clock>")
    }
}

impl Callable for Clock {
    fn arity(&self) -> usize {
        0
    }

    fn call(
        &self,
        _: &mut Interpreter,
        env: Environment,
        _: Vec<ExprRef>,
    ) -> Result<ExprRefWithEnv, InterpretError> {
        let secs = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("we mustn't travel back in time")
            .as_secs_f64();

        Ok(ExprRefWithEnv::new(
            Expr::Literal(Tok::FloatLiteral(secs)).into(),
            env,
        ))
    }

    fn box_clone(&self) -> Box<dyn Callable> {
        Box::new((*self).clone())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

#[derive(Clone, Debug)]
pub struct Assert;

impl Assert {
    pub fn value() -> Expr {
        Expr::Callable(Box::new(Self))
    }
}

impl fmt::Display for Assert {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<builtin fn assert>")
    }
}

impl Callable for Assert {
    fn arity(&self) -> usize {
        1
    }

    fn call(
        &self,
        _: &mut Interpreter,
        env: Environment,
        args: Vec<ExprRef>,
    ) -> Result<ExprRefWithEnv, InterpretError> {
        let node = args.get(0).unwrap().borrow();
        let line = node.context.line();
        let v = node.try_literal();
        match v {
            Some(Tok::BoolLiteral(true)) => Ok(ExprRefWithEnv::new(Expr::Void.into(), env)),
            Some(Tok::BoolLiteral(false)) => Err(node.context.error("Assertion error")),
            Some(expr) => Err(node.context.error(&format!("Invalid args, not a bool"))),
            _ => Err(node.context.error(&format!("Invalid Type: {:?}", args))),
        }
    }

    fn box_clone(&self) -> Box<dyn Callable> {
        Box::new((*self).clone())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
