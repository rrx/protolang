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
        let v = args.get(0).unwrap().borrow().try_literal();
        if let Some(Tok::BoolLiteral(b)) = v {
            if b {
                Ok(ExprRefWithEnv::new(Expr::Void.into(), env))
            } else {
                Err(InterpretError::Runtime {
                    message: format!("Assertion error"),
                    line: 0,
                })
            }
        } else {
            Err(InterpretError::Runtime {
                message: format!("Invalid Type: {:?}", args),
                line: 0,
            })
        }
    }

    fn box_clone(&self) -> Box<dyn Callable> {
        Box::new((*self).clone())
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
