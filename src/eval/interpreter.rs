use super::*;
use super::{ExprRef, ExprRefWithEnv};
use crate::ast::*;
use crate::parser::Unparse;
use crate::sexpr::SExpr;
use crate::tokens::Tok;
use log::debug;
//use std::borrow::Borrow;
//use std::convert::From;
use std::ops::Deref;
use std::rc::Rc;
use std::result::Result;

#[derive(Debug)]
pub struct Interpreter {}

impl Default for Interpreter {
    fn default() -> Self {
        Self {}
    }
}

impl Interpreter {}

#[derive(Debug)]
pub enum InterpretError {
    Invalid,
    Runtime { message: String, line: usize },
}

impl InterpretError {
    pub fn runtime(m: &str) -> InterpretError {
        InterpretError::Runtime {
            message: m.to_string(),
            line: 0,
        }
    }
}

impl MaybeNodeContext {
    pub fn error(&self, m: &str) -> InterpretError {
        InterpretError::Runtime {
            message: m.to_string(),
            line: self.line(),
        }
    }
}

impl Expr {
    fn check_bool(&self) -> Result<bool, InterpretError> {
        match self {
            Self::Literal(t) => match t {
                Tok::BoolLiteral(b) => Ok(*b),
                _ => Err(InterpretError::Runtime {
                    message: format!("Expecting a bool: {}", t.unlex()),
                    line: 0,
                }),
            },
            _ => Err(InterpretError::Runtime {
                message: format!("Expecting a literal: {:?}", self),
                line: 0,
            }),
        }
    }

    fn check_number(&self) -> Result<f64, InterpretError> {
        match self {
            Self::Literal(t) => match t {
                Tok::IntLiteral(u) => Ok(*u as f64),
                Tok::FloatLiteral(f) => Ok(*f),
                _ => Err(InterpretError::Runtime {
                    message: format!("Expecting a number: {:?}", self),
                    line: 0,
                }),
            },
            Self::Callable(_) => Err(InterpretError::Runtime {
                message: format!("Expecting a callable: {:?}", self),
                line: 0,
                //message: format!(
                //"Expecting a number, got a lambda: {:?} on line:{}, column:{}, fragment:{}",
                //self,
                //e.loc.line,
                //e.loc.col,
                //e.loc.fragment
                //),
                //line: e.loc.line,
            }),
            _ => Err(InterpretError::Runtime {
                message: format!("Expecting a number: {:?}", self),
                line: 0,
            }),
        }
    }

    fn check_numbers(a: &Expr, b: &Expr) -> Result<(f64, f64), InterpretError> {
        let a = a.check_number()?;
        let b = b.check_number()?;
        Ok((a, b))
    }

    pub fn plus(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(Self::new_float(left + right))
    }

    pub fn minus(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(Self::new_float(left - right))
    }

    pub fn exp(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(Self::new_float(left.powf(right)))
    }

    pub fn multiply(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(Self::new_float(left * right))
    }

    pub fn divide(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(Self::new_float(left / right))
    }

    pub fn lte(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(Self::new_bool(left <= right))
    }

    pub fn lt(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(Self::new_bool(left < right))
    }

    pub fn gte(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(Self::new_bool(left >= right))
    }

    pub fn gt(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(Self::new_bool(left > right))
    }

    pub fn eq(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(Self::new_bool(left == right))
    }

    pub fn ne(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(Self::new_bool(left != right))
    }

    pub fn postfix(&self, op: &Operator) -> Result<Self, InterpretError> {
        match op {
            Operator::Bang => {
                let right = self.check_number()?;
                // TODO: Fib not yet implemented
                Ok(Self::new_float(right))
            }
            _ => unimplemented!(),
        }
    }

    pub fn prefix(&self, prefix: &Operator) -> Result<Self, InterpretError> {
        match prefix {
            Operator::Plus => {
                let right = self.check_number()?;
                Ok(Self::new_float(right))
            }
            Operator::Minus => {
                let right = self.check_number()?;
                Ok(Self::new_float(right))
            }
            _ => unimplemented!(),
        }
    }
}

impl Interpreter {
    pub fn call(
        &mut self,
        mut env: Environment,
        f: &dyn Callable,
        params: &Params,
        args: &Vec<ExprRef>,
        expr: ExprRef,
    ) -> Result<ExprRefWithEnv, InterpretError> {
        if args.len() != f.arity() {
            return Err(InterpretError::Runtime {
                message: format!(
                    "Mismatched params on function. Expecting {}, got {}",
                    f.arity(),
                    args.len()
                ),
                line: params.context.line()
            });
        }

        let param_idents = params
            .value
            .iter()
            .map(|param| {
                match param.value.try_ident() {
                    Some(ident) => Ok(ident),
                    None => {
                        Err(InterpretError::Runtime {
                            message: format!("Invalid Argument on Lambda, got {:?}", param),
                            line: param.context.line(),
                        })
                    }
                }
            })
            .collect::<Vec<_>>();

        let (p, e): (
            Vec<Result<_, InterpretError>>,
            Vec<Result<_, InterpretError>>,
        ) = param_idents.into_iter().partition(|p| p.is_ok());

        if e.len() > 0 {
            return Err(InterpretError::Runtime {
                message: format!("Invalid Argument on Lambda, got {:?}", e),
                line: params.context.line()
            });
        }

        // push variables into scope
        for (p, v) in p.iter().zip(args) {
            env = env.define(p.as_ref().unwrap().clone(), v.clone().into());
        }

        // evaluate the result
        let r = self.evaluate(expr, env)?;
        // we drop any temporary variables, if they are no longer needed
        // we aren't able to drop the env currently, in case the function mutated something outside
        Ok(r)

        /*
                match &expr.value {
                    Expr::Callable(c) => {
                        self.interpret(
                        Ok(ExprNode::Literal(Tok::IntLiteral(0)))

                    }
                    Expr::Lambda(lam) => {
                        Ok(ExprNode::Literal(Tok::IntLiteral(0)))
                    }
                    _ => {
                        Err(InterpretError::Runtime {
                            message: format!("Invalid Argument on Lambda, got {:?}", e),
                            line: 0, //name.line(),
                        })
                    }
                }
        */
    }
    pub fn evaluate(
        &mut self,
        expr: ExprRef,
        env: Environment,
    ) -> Result<ExprRefWithEnv, InterpretError> {
        let node = &expr.as_ref().borrow();
        let expr = &node.value;
        debug!("EVAL: {:?}", &expr);
        match expr {
            Expr::Literal(lit) => Ok(ExprRefWithEnv::new(Expr::Literal(lit.clone()).into(), env)),
            Expr::Ident(ident) => {
                let v = env.get_at(&ident.name, &node.context)?;
                Ok(ExprRefWithEnv::new(v.into(), env))
            }
            Expr::Prefix(prefix, expr) => {
                let r = self.evaluate(expr.clone().into(), env)?;
                let eval = r.expr.as_ref().borrow().prefix(&prefix.value)?.into();
                Ok(ExprRefWithEnv::new(eval, r.env))
            }
            Expr::Postfix(op, expr) => {
                let r = self.evaluate(expr.clone().into(), env)?;
                let eval = r.expr.as_ref().borrow().postfix(&op.value)?;
                Ok(ExprRefWithEnv::new(eval.into(), r.env))
            }
            Expr::Binary(op, left, right) => {
                match op {
                    Operator::Declare => {
                        return if let Some(ident) = left.try_ident() {
                            let eval_right = self.evaluate(right.clone().into(), env)?;
                            debug!("Declare {:?} to {}", &eval_right, &ident.name);
                            let env = eval_right.env.define(ident, eval_right.expr.clone());
                            Ok(ExprRefWithEnv::new(eval_right.expr, env))
                        } else {
                            Err(InterpretError::Runtime {
                                message: format!("Invalid Assignment, LHS must be identifier"),
                                line: left.context.line(),
                            })
                        };
                    }
                    Operator::Assign => {
                        return if let Some(ident) = left.try_ident() {
                            let mut access = env.get_at(&ident.name, &node.context)?;
                            if access.modifier != VarModifier::Mutable {
                                node.debug();
                                env.debug();
                                return Err(node.context.error(&format!("Invalid Assignment, '{}' Not mutable", &ident.name)));
                            }

                            let eval_right = self.evaluate(right.clone().into(), env)?;
                            debug!("Assign {:?} to {}", &eval_right, &ident.name);
                            let expr = access.expr.mutate(Rc::try_unwrap(eval_right.expr.0).unwrap().into_inner());
                            //env.define(ident, eval_right.expr.clone());
                            Ok(ExprRefWithEnv::new(expr, eval_right.env))
                        } else {
                            Err(InterpretError::Runtime {
                                message: format!("Invalid Assignment, LHS must be identifier"),
                                line: left.context.line(),
                            })
                        };
                    }
                    _ => ()
                }

                let line = left.context.line();
                let v_left = self.evaluate(left.clone().into(), env)?;
                let v_right = self.evaluate(right.clone().into(), v_left.env)?;

                let eval_left = v_left.expr.as_ref().borrow();
                let eval_right = v_right.expr.as_ref().borrow();
                match op {
                    Operator::Plus => eval_left.plus(&eval_right),
                    Operator::Minus => eval_left.minus(&eval_right),
                    Operator::Exp => eval_left.exp(&eval_right),
                    Operator::Multiply => eval_left.multiply(&eval_right),
                    Operator::Divide => eval_left.divide(&eval_right),
                    Operator::GreaterThanEqual => eval_left.gte(&eval_right),
                    Operator::LessThanEqual => eval_left.lte(&eval_right),
                    Operator::GreaterThan => eval_left.gt(&eval_right),
                    Operator::LessThan => eval_left.lt(&eval_right),
                    Operator::Equal => eval_left.eq(&eval_right),
                    Operator::NotEqual => eval_left.ne(&eval_right),
                    _ => Err(InterpretError::Runtime {
                        message: format!("Unimplemented expression op: Operator::{:?}", op),
                        line,
                    }),
                }
                .map(|v| {
                    ExprRefWithEnv::new(ExprNode::new(v.into(), &left.context.to_location()).into(), v_right.env)
                })
            }
            Expr::List(elements) => {
                let mut eval_elements = vec![];
                let mut newenv = env;
                for e in elements.clone() {
                    let eref = self.evaluate(e.into(), newenv)?;
                    newenv = eref.env;
                    let e = eref.expr.as_ref().borrow().deref().clone();
                    eval_elements.push(e);
                }
                Ok(ExprRefWithEnv::new(
                    Expr::List(eval_elements).into(),
                    newenv,
                ))
            }

            Expr::Callable(e) => {
                debug!("Callable({:?})", &e);
                Err(InterpretError::Runtime {
                    message: format!("Unimplemented callable::{:?}", &e),
                    line: 0, //expr.context.loc.line,
                })
            }

            Expr::Lambda(e) => {
                debug!("Lambda({:?})", &e);
                Ok(ExprRefWithEnv::new(
                    Expr::Callable(Box::new(e.clone())).into(),
                    env,
                ))
            }

            Expr::Index(_ident, _args) => {
                // TODO: not yet implemented
                Ok(ExprRefWithEnv::new(
                    Expr::Literal(Tok::IntLiteral(0)).into(),
                    env,
                ))
            }

            Expr::Apply(expr, args) => {
                let f = match &expr.value {
                    Expr::Ident(ident) => {
                        let x: ExprAccessRef = env.get_at(&ident.name, &node.context)?.clone();

                        let expr = x.expr.as_ref().borrow();
                        match expr.try_callable() {
                            Some(c) => {
                                let mut eval_args = vec![];
                                let mut newenv = env;
                                for arg in args {
                                    let v = self.evaluate(arg.clone().into(), newenv)?;
                                    eval_args.push(v.expr);
                                    newenv = v.env;
                                }
                                debug!("Calling {:?}({:?})", c, eval_args);

                                // newenv.clone here creates a stack branch
                                let result = c.call(self, newenv.clone(), eval_args)?;
                                debug!("Call Result {:?}", &result);
                                Some(Ok(result)) //ExprRefWithEnv::new(result.into(), newenv)))
                            }
                            _ => None,
                        }
                    }
                    _ => None,
                };

                //let env = Environment::default();
                if let Some(r) = f {
                    r
                } else {
                    Err(InterpretError::Runtime {
                        message: format!("Not a function: {:?}", f),
                        line: 0,
                    })
                }
            }

            Expr::Block(exprs) => {
                // default return value for a block is void
                let mut result = Expr::Void.into(); //List(vec![]).into();
                let original_env = env.clone();
                let mut newenv = env;
                for expr in exprs {
                    let r = self.evaluate(expr.clone().into(), newenv);
                    match r {
                        Ok(v) => {
                            newenv = v.env;
                            result = v.expr;
                        }
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }

                //return the original env
                Ok(ExprRefWithEnv::new(result.into(), original_env))
            }

            Expr::Program(exprs) => {
                let mut result = Expr::Void.into(); //List(vec![]).into();
                let mut newenv = env;
                for expr in exprs {
                    let r = self.evaluate(expr.clone().into(), newenv);
                    match r {
                        Ok(v) => {
                            newenv = v.env;
                            result = v.expr;
                        }
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
                Ok(ExprRefWithEnv::new(result.into(), newenv))
            }

            Expr::Ternary(op, x, y, z) => match op {
                Operator::Conditional => {
                    let v = self.evaluate(x.clone().into(), env)?;
                    let b = Expr::check_bool(&v.expr.as_ref().borrow())?;
                    if b {
                        self.evaluate(y.clone().into(), v.env)
                    } else {
                        self.evaluate(z.clone().into(), v.env)
                    }
                }
                _ => unimplemented!(),
            },

            Expr::Chain(_, _) => Ok(ExprRefWithEnv::new(Expr::new_int(0).into(), env)),
            Expr::Invalid(s) => Err(InterpretError::Runtime {
                message: format!("Invalid expr: {:?}", s),
                line: 0,
            }),
            Expr::Void => Ok(ExprRefWithEnv::new(Expr::Void.into(), env)),
        }
    }

    pub fn execute(
        &mut self,
        e: ExprRef,
        env: Environment,
    ) -> Result<ExprRefWithEnv, InterpretError> {
        let expr = e.as_ref().borrow();
        debug!("EXPR: {:?}", &expr);
        debug!("EXPR-unparse: {:?}", expr.unparse());
        debug!("EXPR-unlex: {:?}", expr.unlex());
        match expr.sexpr() {
            Ok(s) => {
                debug!("EXPR-sexpr: {}", s);
            }
            Err(e) => {
                debug!("ERROR: {:?}", e);
                return Err(InterpretError::Runtime {
                    message: "Unable to parse sexpr".into(),
                    line: 0,
                });
            }
        }
        drop(expr);
        self.evaluate(e, env)
    }

    //pub fn interpret(&mut self, program: ExprRef, env: Environment) -> Result<(Environment, ExprRef), InterpretError> {
    //match self.evaluate(program.into(), env) {
    //Ok(v) => {
    //debug!("Result: {:?}", &v);
    //Ok(v)
    //}
    //Err(error) => {
    //debug!("ERROR: {:?}", &error);
    //Err(
    //}
    //}
    //}
}
