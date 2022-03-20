use super::{ExprRef, ExprRefWithEnv};
use super::*;
use crate::ast::*;
use crate::parser::Unparse;
use crate::sexpr::SExpr;
use crate::tokens::Tok;
use log::debug;
use std::borrow::Borrow;
use std::convert::From;
use std::ops::Deref;
use std::rc::Rc;
use std::result::Result;

#[derive(Debug)]
pub struct Interpreter {
}

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
    ) -> Result<ExprRef, InterpretError> {
        if args.len() != f.arity() {
            return Err(InterpretError::Runtime {
                message: format!(
                    "Mismatched params on function. Expecting {}, got {}",
                    f.arity(),
                    args.len()
                ),
                line: 0, //name.line(),
            });
        }

        let params = params
            .value
            .iter()
            .map(|param| {
                match param.value.try_ident() {
                    Some(ident) => Ok(ident),
                    None => {
                        Err(InterpretError::Runtime {
                            message: format!("Invalid Argument on Lambda, got {:?}", param),
                            line: 0, //name.line(),
                        })
                    }
                }
            })
            .collect::<Vec<_>>();

        let (p, e): (
            Vec<Result<_, InterpretError>>,
            Vec<Result<_, InterpretError>>,
        ) = params.into_iter().partition(|p| p.is_ok()); //value.try_ident().ok());

        if e.len() > 0 {
            return Err(InterpretError::Runtime {
                message: format!("Invalid Argument on Lambda, got {:?}", e),
                line: 0, //name.line(),
            });
        }

        // push variables into scope
        for (p, v) in p.iter().zip(args) {
            env = env.define(p.as_ref().unwrap(), v.clone());
        }

        // evaluate the result
        let (_, r) = self.evaluate(expr, env)?;
        // we drop the env, it's no longer needed
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
    pub fn evaluate(&mut self, expr: ExprRef, env: Environment) -> Result<(Environment, ExprRef), InterpretError> {
        let expr = &expr.as_ref().borrow().value;
        debug!("EVAL: {:?}", &expr);
        match expr {
            Expr::Literal(lit) => Ok((env, Expr::Literal(lit.clone()).into())),
            Expr::Ident(ident) => {
                let v = env.get(&ident)?;
                Ok((env, v))
            }
            Expr::Prefix(prefix, expr) => {
                let (env, eval) = self.evaluate(expr.clone().into(), env)?;
                let eval = eval.as_ref().borrow().prefix(&prefix.value)?.into();
                Ok((env, eval))
            }
            Expr::Postfix(op, expr) => {
                let (env, eval) = self.evaluate(expr.clone().into(), env)?;
                let eval = eval.as_ref().borrow().postfix(&op.value)?;
                Ok((env, eval.into()))
            }
            Expr::Binary(op, left, right) => {
                if op == &Operator::Assign {
                    return if let Some(ident) = left.try_ident() {
                        let (env, eval_right) = self.evaluate(right.clone().into(), env)?;
                        debug!("Assign {:?} to {}", &eval_right, &ident);
                        Ok((env.define(&ident, eval_right.clone()), eval_right))
                    } else {
                        Err(InterpretError::Runtime {
                            message: format!("Invalid Assignment, LHS must be identifier"),
                            line: left.context.line(),
                        })
                    };
                }

                let line = left.context.line();
                let (env, v_left) = self.evaluate(left.clone().into(), env)?;
                let (env, v_right) = self.evaluate(right.clone().into(), env)?;

                let eval_left = v_left.as_ref().borrow();
                let eval_right = v_right.as_ref().borrow();
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
                .map(|v| (env, v.into()))
            }
            Expr::List(elements) => {
                let mut eval_elements = vec![];
                let mut newenv = env;
                for e in elements.clone() {
                    let (env, eref) = self.evaluate(e.into(), newenv)?;
                    newenv = env;
                    let e = eref.as_ref().borrow().deref().clone(); //.into();//Rc::try_unwrap(eref.0).unwrap();
                    eval_elements.push(e);
                }
                Ok((newenv, Expr::List(eval_elements).into()))
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
                Ok((env, Expr::Callable(Box::new(e.clone())).into()))
            }

            Expr::Index(_ident, _args) => {
                // TODO: not yet implemented
                Ok((env, Expr::Literal(Tok::IntLiteral(0)).into()))
            }

            Expr::Apply(expr, args) => {
                let f = match &expr.value {
                    Expr::Ident(ident) => {
                        let x: ExprRef = env.get(ident)?.clone();

                        let expr = x.as_ref().borrow();
                        match expr.try_callable() {
                            Some(c) => {
                                let mut eval_args = vec![];
                                let mut newenv = env;
                                for arg in args {
                                    let (env, v) = self.evaluate(arg.clone().into(), newenv)?;
                                    eval_args.push(v);
                                    newenv = env;
                                }
                                debug!("Calling {:?}({:?})", c, eval_args);

                                // newenv.clone here creates a stack branch
                                let result = c.call(self, newenv.clone(), eval_args)?;
                                debug!("Call Result {:?}", &result);
                                Some(Ok((newenv, result.into())))
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
                let mut result = Expr::List(vec![]).into();
                let mut newenv = env;
                for expr in exprs {
                    let r = self.evaluate(expr.clone().into(), newenv);
                    match r {
                        Ok((env, v)) => {
                            newenv = env;
                            result = v;
                        }
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
                Ok((newenv, result.into()))
            }

            Expr::Program(exprs) => {
                let mut result = Expr::List(vec![]).into();
                let mut newenv = env;
                for expr in exprs {
                    let r = self.evaluate(expr.clone().into(), newenv);
                    match r {
                        Ok((env, v)) => {
                            newenv = env;
                            result = v;
                        }
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
                Ok((newenv, result.into()))
            }

            Expr::Ternary(op, x, y, z) => match op {
                Operator::Conditional => {
                    let (env, v) = self.evaluate(x.clone().into(), env)?;
                    let b = Expr::check_bool(&v.as_ref().borrow())?;
                    if b {
                        self.evaluate(y.clone().into(), env)
                    } else {
                        self.evaluate(z.clone().into(), env)
                    }
                }
                _ => unimplemented!(),
            },

            Expr::Chain(_, _) => Ok((env, Expr::Literal(Tok::IntLiteral(0)).into())),
            Expr::Invalid(s) => Err(InterpretError::Runtime {
                message: format!("Invalid expr: {:?}", s),
                line: 0,
            }),
            Expr::Void => Ok((env, Expr::Void.into()))
        }
    }

    pub fn execute(&mut self, e: ExprRef, env: Environment) -> Result<(Environment, ExprRef), InterpretError> {
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

