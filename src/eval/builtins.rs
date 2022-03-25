use crate::{
    ast::{Callable, Expr},
    eval::{Environment, ExprRef, ExprRefWithEnv, InterpretError, Interpreter},
    tokens::Tok,
};
use std::{
    //any::Any,
    fmt,
    time::{SystemTime, UNIX_EPOCH},
};
//use thiserror::Error;

/*
#[derive(Clone)]
pub struct Generic<'a, F> where F: Clone {
    arity: usize,
    cb: F,
    p: std::marker::PhantomData<&'a F>
}

impl<'a, F> Generic<'a, F>
where F: Clone + Fn(&'a mut Interpreter, Environment, Vec<ExprRef>) -> Result<ExprRefWithEnv, InterpretError> {
    pub fn new(arity: usize, cb: F) -> Self {
        Self { cb, arity, p: std::marker::PhantomData }
    }
}

impl<'a, F> fmt::Debug for Generic<'a, F> where F: Clone {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<builtin fn generic>")
    }
}

impl<'a, F> fmt::Display for Generic<'a, F> where F: Clone {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<builtin fn generic>")
    }
}

impl<'a, F> Callable for Generic<'a, F>
where F: Clone + Fn(&'a mut Interpreter, Environment, Vec<ExprRef>) -> Result<ExprRefWithEnv, InterpretError>
{
    fn call(
        &'a self,
        interp: &'a mut Interpreter,
        env: Environment,
        args: Vec<ExprRef>,
    ) -> Result<ExprRefWithEnv, InterpretError> {
        (self.cb)(interp, env, args)
    }

    fn box_clone(&self) -> Box<dyn Callable> {
        Box::new(Self { arity: self.arity, cb: self.cb.clone(), p: self.p.clone() })//(*self).clone())
    }

    //fn as_any(&self) -> &dyn Any {
        //self
    //}
}
*/

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
    fn call<'a>(
        &self,
        _: &mut Interpreter<'a>,
        env: Environment<'a>,
        _: Vec<ExprRef>,
    ) -> Result<ExprRefWithEnv<'a>, InterpretError> {
        env.debug();
        Ok(ExprRefWithEnv::new(Expr::Void.into(), env))
    }

    fn box_clone(&self) -> Box<dyn Callable> {
        Box::new((*self).clone())
    }

    //fn as_any(&self) -> &dyn Any {
    //self
    //}
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

    fn call<'a> (
        &self,
        _: &mut Interpreter<'a>,
        env: Environment<'a>,
        _: Vec<ExprRef>,
    ) -> Result<ExprRefWithEnv<'a>, InterpretError> {
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

    //fn as_any(&self) -> &dyn Any {
    //self
    //}
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

    fn call<'a>(
        &self,
        _: &mut Interpreter<'a>,
        env: Environment<'a>,
        args: Vec<ExprRef>,
    ) -> Result<ExprRefWithEnv<'a>, InterpretError> {
        let node = args.get(0).unwrap().borrow();
        let v = node.try_literal();
        match v {
            Some(Tok::BoolLiteral(true)) => Ok(ExprRefWithEnv::new(Expr::Void.into(), env)),
            Some(Tok::BoolLiteral(false)) => Err(node.context.runtime_error("Assertion error")),
            Some(_) => Err(node
                .context
                .runtime_error(&format!("Invalid args, not a bool"))
                .into()),
            _ => Err(node
                .context
                .runtime_error(&format!("Invalid Type: {:?}", args))
                .into()),
        }
    }

    fn box_clone(&self) -> Box<dyn Callable> {
        Box::new((*self).clone())
    }

    //fn as_any(&self) -> &dyn Any {
    //self
    //}
}
