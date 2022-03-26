use crate::{
    ast::{CallTable, Callable, Callback, Expr},
    eval::{Environment, ExprRef, ExprRefWithEnv, InterpretError},
    tokens::Tok,
};
use std::fmt;

pub fn builtins(mut builtins: CallTable) -> CallTable {
    builtins
        .add(
            "showstack".into(),
            Callback::new(|env, _| {
                env.debug();
                Ok(ExprRefWithEnv::new(Expr::Void.into(), env))
            }),
        )
        .add(
            "assert".into(),
            Callback::new(|env, args| {
                let node = args.get(0).unwrap().borrow();
                let v = node.try_literal();
                match v {
                    Some(Tok::BoolLiteral(true)) => Ok(ExprRefWithEnv::new(Expr::Void.into(), env)),
                    Some(Tok::BoolLiteral(false)) => {
                        Err(node.context.runtime_error("Assertion error"))
                    }
                    Some(_) => Err(node
                        .context
                        .runtime_error(&format!("Invalid args, not a bool"))
                        .into()),
                    _ => Err(node
                        .context
                        .runtime_error(&format!("Invalid Type: {:?}", args))
                        .into()),
                }
            }),
        )
        .add(
            "sexpr".into(),
            Callback::new(|env, args| {
                let mut out = vec![];
                use crate::sexpr::SExpr;
                for arg in args {
                    match arg.borrow().sexpr() {
                        Ok(sexpr) => {
                            out.push(Expr::new_string(sexpr.to_string()).into());
                            println!("SEXPR: {}", sexpr);
                        }
                        Err(e) => {
                            return Err(InterpretError::runtime(&format!(
                                "unable to parse: {:?}",
                                e
                            )));
                        }
                    }
                }
                Ok(ExprRefWithEnv::new(Expr::List(out).into(), env))
            }),
        )
        .add(
            "clock".into(),
            Callback::new(|env, _| {
                use std::time::{SystemTime, UNIX_EPOCH};
                let secs = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .expect("we mustn't travel back in time")
                    .as_secs_f64();

                Ok(ExprRefWithEnv::new(
                    Expr::Literal(Tok::FloatLiteral(secs)).into(),
                    env,
                ))
            }),
        )
}

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
        env: Environment,
        _: Vec<ExprRef>,
    ) -> Result<ExprRefWithEnv, InterpretError> {
        env.debug();
        Ok(ExprRefWithEnv::new(Expr::Void.into(), env))
    }

    fn box_clone(&self) -> Box<dyn Callable> {
        Box::new((*self).clone())
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

    fn call<'a>(
        &self,
        env: Environment,
        _: Vec<ExprRef>,
    ) -> Result<ExprRefWithEnv, InterpretError> {
        use std::time::{SystemTime, UNIX_EPOCH};
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
        env: Environment,
        args: Vec<ExprRef>,
    ) -> Result<ExprRefWithEnv, InterpretError> {
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
}
