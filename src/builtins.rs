use crate::{
    lexer::Location,
    ast::{Callable, CallableNode, Value},
    interpreter::{InterpretError, Interpreter},
};
use std::{
    any::Any,
    fmt,
    time::{SystemTime, UNIX_EPOCH},
};

#[derive(Clone, Debug)]
pub struct Clock;

impl Clock {
    pub fn value() -> Value {
        Value::Callable(CallableNode::new(
            Box::new(Self),
            Location::new(0, 0, 0, "".into()),
        ))
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

    fn call(&self, _: &mut Interpreter, _: Vec<Value>) -> Result<Value, InterpretError> {
        let secs = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("we mustn't travel back in time")
            .as_secs_f64();

        Ok(Value::FloatLiteral(secs))
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
    pub fn value() -> Value {
        Value::Callable(CallableNode::new(
            Box::new(Self),
            Location::new(0, 0, 0, "".into()),
        ))
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

    fn call(&self, _: &mut Interpreter, args: Vec<Value>) -> Result<Value, InterpretError> {
        let v = args.get(0).unwrap();
        if let Value::BoolLiteral(b) = v {
            if *b {
                Ok(Value::Null)
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
