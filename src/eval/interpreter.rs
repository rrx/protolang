use crate::ast::*;
use crate::parser::Unparse;
use crate::sexpr::SExpr;
use crate::tokens::Tok;
use log::debug;
use std::result::Result;
use super::*;
use super::ExprRef;
use std::convert::From;
use std::rc::Rc;
use std::ops::Deref;
use std::borrow::Borrow;


#[derive(Debug)]
pub struct Interpreter {
    globals: Environment,
}

impl Default for Interpreter {
    fn default() -> Self {
        let mut globals = Environment::default();
        use super::builtins::*;
        globals.define("clock", ExprRef::new(Clock::value()));
        globals.define("assert", ExprRef::new(Assert::value()));
        Self { globals }
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
        f: &dyn Callable,
        params: &Params,
        args: &Vec<ExprRef>,
        expr: &ExprNode,
    ) -> Result<ExprNode, InterpretError> {
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
            self.globals.define(p.as_ref().unwrap(), v.clone());
        }

        // evaluate the result
        let r = self.evaluate(&expr)?;

        // pop scope, TODO
        let mut out = expr.clone();
        out.value = r.as_ref().borrow().clone();//deref().clone();//(*r.0).clone();
        Ok(out)
        //r

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
    pub fn evaluate(&mut self, expr: &Expr) -> Result<ExprRef, InterpretError> {
        match expr.clone() {
            Expr::Literal(lit) => Ok(Expr::Literal(lit).into()), //ExprNode::Literal(lit.clone())),//lit.try_into()?.clone()),
            Expr::Ident(ident) => self.globals.get(&ident),
            Expr::Prefix(prefix, expr) => {
                let eval = self.evaluate(&expr)?;
                let eval = eval.as_ref().borrow().prefix(&prefix.value)?.into();
                Ok(eval)
            }
            Expr::Postfix(op, expr) => {
                let eval = self.evaluate(&expr)?;
                let eval = eval.as_ref().borrow().postfix(&op.value)?;
                Ok(eval.into())
            }
            Expr::Binary(op, left, right) => {
                if op == Operator::Assign {
                    return if let Some(ident) = left.try_ident() {
                        let eval_right = self.evaluate(&right)?;
                        debug!("Assign {:?} to {}", &eval_right, &ident);
                        self.globals.define(&ident, eval_right.clone());
                        Ok(eval_right)
                    } else {
                        Err(InterpretError::Runtime {
                            message: format!("Invalid Assignment, LHS must be identifier"),
                            line: left.context.line(),
                        })
                    };
                }

                let v_left = self.evaluate(&left)?;
                let v_right = self.evaluate(&right)?;

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
                        line: left.context.line(),
                    }),
                }.map(|v| v.into())
            }
            Expr::List(elements) => {
                let mut eval_elements = vec![];
                for mut e in elements.clone() {
                    let eref = self.evaluate(&e)?;
                    e.value = eref.as_ref().borrow().deref().clone();//Rc::try_unwrap(eref.0).unwrap();
                    eval_elements.push(e); //self.evaluate(&e)?);
                }
                Ok(Expr::List(eval_elements).into())
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
                Ok(Expr::Callable(Box::new(e)).into())
            }

            Expr::Index(_ident, _args) => {
                // TODO: not yet implemented
                Ok(Expr::Literal(Tok::IntLiteral(0)).into())
            }

            Expr::Apply(expr, args) => {
                let f = match &expr.value {
                    Expr::Ident(ident) => {
                        let x: ExprRef = self.globals.get(ident)?.clone();

                        let expr = x.as_ref().borrow();
                        match expr.try_callable() {
                            Some(c) => {
                                let mut eval_args = vec![];
                                for arg in args {
                                    eval_args.push(self.evaluate(&arg)?);
                                }
                                debug!("Calling {:?}({:?})", c, eval_args);
                                let result = c.call(self, eval_args);
                                debug!("Call Result {:?}", &result);
                                Some(result.map(|v| v.into()))
                            }
                            _ => None
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

                for expr in exprs {
                    let r = self.evaluate(&expr);
                    match r {
                        Ok(v) => {
                            result = v;
                        }
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
                Ok(result.into())
            }

            Expr::Program(exprs) => {
                let mut result = Expr::List(vec![]).into();

                for expr in exprs {
                    let r = self.evaluate(&expr);
                    match r {
                        Ok(v) => {
                            result = v;
                        }
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
                Ok(result.into())
            }

            Expr::Ternary(op, x, y, z) => match op {
                Operator::Conditional => {
                    let r = self.evaluate(&x)?;
                    let b = Expr::check_bool(&r.as_ref().borrow())?;
                    if b {
                        self.evaluate(&y)
                    } else {
                        self.evaluate(&z)
                    }
                }
                _ => unimplemented!(),
            },

            Expr::Chain(_, _) => Ok(Expr::Literal(Tok::IntLiteral(0)).into()),
            Expr::Invalid(s) => Err(InterpretError::Runtime {
                message: format!("Invalid expr: {:?}", s),
                line: 0,
            }),
            Expr::Void => Ok(Expr::Void.into()),
        }
    }

    pub fn execute(&mut self, expr: ExprNode) -> Result<ExprRef, InterpretError> {
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

        self.evaluate(&expr)
    }

    pub fn interpret(&mut self, program: ExprNode) {
        match self.evaluate(&program) {
            Ok(v) => {
                debug!("Result: {:?}", &v);
            }
            Err(error) => {
                debug!("ERROR: {:?}", &error);
                return;
            }
        }
    }
}
