use crate::{
    ast::{Callable, Expr},
    eval::{InterpretError, Interpreter, ExprRef},
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

    fn call(&self, _: &mut Interpreter, _: Vec<ExprRef>) -> Result<ExprRef, InterpretError> {
        let secs = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("we mustn't travel back in time")
            .as_secs_f64();

        Ok(Expr::Literal(Tok::FloatLiteral(secs)).into())
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

    fn call(&self, _: &mut Interpreter, args: Vec<ExprRef>) -> Result<ExprRef, InterpretError> {
        let v = args.get(0).unwrap().borrow().try_literal();
        if let Some(Tok::BoolLiteral(b)) = v {
            if b {
                Ok(Expr::Void.into())
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
