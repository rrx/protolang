use crate::ast::*;
use crate::sexpr::SExpr;
use crate::tokens::Tok;
use std::collections::HashMap;
use std::result::Result;

#[derive(Debug)]
pub struct Environment {
    values: HashMap<String, Value>,
}
impl Default for Environment {
    fn default() -> Self {
        Self {
            values: HashMap::new(),
        }
    }
}
impl Environment {
    pub fn define(&mut self, name: &str, value: &Value) {
        self.values.insert(name.to_string(), value.clone());
    }
    pub fn get(&self, name: &str) -> Result<Value, InterpretError> {
        if let Some(value) = self.values.get(name) {
            return Ok(value.clone());
        }
        Err(InterpretError::Runtime {
            message: format!("Undefined variable '{}'.", name),
            line: 0, //name.line(),
        })
    }
}

#[derive(Debug)]
pub struct Interpreter {
    globals: Environment,
}

impl Default for Interpreter {
    fn default() -> Self {
        let mut globals = Environment::default();
        use crate::builtins::*;
        globals.define("clock", &Clock::value());
        globals.define("assert", &Assert::value());
        Self { globals }
    }
}

impl Interpreter {}

#[derive(Debug)]
pub enum InterpretError {
    Invalid,
    Runtime { message: String, line: usize },
}

impl Value {
    pub fn is_number(&self) -> bool {
        if let Self::Literal(tok) = self {
            match tok {
                Tok::IntLiteral(_) => true,
                Tok::FloatLiteral(_) => true,
                _ => false,
            }
        } else {
            false
        }
    }

    pub fn new_float(f: f64) -> Self {
        Self::Literal(Tok::FloatLiteral(f))
    }
    
    pub fn new_bool(b: bool) -> Self {
        Self::Literal(Tok::BoolLiteral(b))
    }

    fn check_bool(&self) -> Result<bool, InterpretError> {
        match self {
            Self::Literal(t) => match t {
                Tok::BoolLiteral(b) => Ok(*b),
                _ => Err(InterpretError::Runtime {
                    message: format!("Expecting a bool: {}", self.unlex()),
                    line: 0
                }),
            },
            _ => Err(InterpretError::Runtime {
                message: format!("Expecting a literal: {}", self.unlex()),
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
                    message: format!("Expecting a number: {}", self.unlex()),
                    line: 0,
                }),
            },
            Self::Callable(e) => Err(InterpretError::Runtime {
                message: format!(
                    "Expecting a number, got a lambda: {} on line:{}, column:{}, fragment:{}",
                    self.unlex(),
                    e.loc.line,
                    e.loc.col,
                    e.loc.fragment
                ),
                line: e.loc.line,
            }),
            _ => Err(InterpretError::Runtime {
                message: format!("Expecting a number: {}", self.unlex()),
                line: 0,
            }),
        }
    }

    fn check_numbers(a: &Value, b: &Value) -> Result<(f64, f64), InterpretError> {
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
    pub fn evaluate(&mut self, expr: &ExprNode) -> Result<Value, InterpretError> {
        match &expr.value {
            Expr::LitExpr(lit) => Ok(lit.clone()),
            Expr::Ident(ident) => self.globals.get(ident),
            Expr::Prefix(prefix, expr) => {
                let eval = self.evaluate(&expr)?;
                let eval = eval.prefix(&prefix.value)?;
                Ok(eval)
            }
            Expr::Postfix(op, expr) => {
                let eval = self.evaluate(&expr)?;
                let eval = eval.postfix(&op.value)?;
                Ok(eval)
            }
            Expr::Binary(op, left, right) => {
                if op == &Operator::Assign {
                    return if let Some(ident) = left.try_ident() {
                        let eval_right = self.evaluate(&right)?;
                        self.globals.define(&ident, &eval_right);
                        println!("Assign {:?} to {}", &eval_right, &ident);
                        Ok(eval_right)
                    } else {
                        Err(InterpretError::Runtime {
                            message: format!("Invalid Assignment, LHS must be identifier"),
                            line: expr.context.loc.line,
                        })
                    };
                }

                let eval_left = self.evaluate(&left)?;
                let eval_right = self.evaluate(&right)?;
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
                    _ => Err(InterpretError::Runtime {
                        message: format!("Unimplemented expression op: Operator::{:?}", op),
                        line: expr.context.loc.line,
                    }),
                }
            }
            Expr::List(elements) => {
                let mut eval_elements = vec![];
                for e in elements {
                    eval_elements.push(self.evaluate(&e)?);
                }
                Ok(Value::List(eval_elements))
            }

            Expr::Callable(e) => {
                println!("Callable({:?})", &e);
                Err(InterpretError::Runtime {
                    message: format!("Unimplemented callable::{:?}", &e),
                    line: expr.context.loc.line,
                })
            }

            Expr::Lambda(e) => {
                println!("Lambda({:?})", &e);
                Ok(Value::Callable(e.node()))
            }

            Expr::Index(_ident, _args) => {
                // TODO: not yet implemented
                Ok(Value::Literal(Tok::IntLiteral(0)))
            }

            Expr::Apply(expr, args) => {
                let f = match &expr.value {
                    Expr::Ident(ident) => Some(self.globals.get(ident)?),
                    _ => None,
                };

                //let env = Environment::default();
                match f {
                    Some(Value::Callable(c)) => {
                        let mut eval_args = vec![];
                        for arg in args {
                            eval_args.push(self.evaluate(arg)?);
                        }
                        println!("Calling {:?}({:?})", c, eval_args);
                        let result = c.value.call(self, eval_args);
                        println!("Result {:?}", &result);
                        result
                    }
                    _ => Err(InterpretError::Runtime {
                        message: format!("Not a function: {:?}", f),
                        line: 0,
                    }),
                }
            }
            Expr::Block(exprs) => {
                let mut result = Value::List(vec![]);

                for expr in exprs {
                    let r = self.evaluate(expr);
                    match r {
                        Ok(v) => {
                            result = v;
                        }
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
                Ok(result)
            }

            Expr::Ternary(op, x, y, z) => {
                match op {
                    Operator::Conditional => {
                        let r = self.evaluate(x)?;
                        let b = Value::check_bool(&r)?;
                        if b {
                            self.evaluate(y)
                        } else {
                            self.evaluate(z)
                        }
                    }
                    _ => unimplemented!()
                }
            }

            Expr::Chain(_, _) => Ok(Value::Literal(Tok::IntLiteral(0))),
            Expr::Invalid(s) => Err(InterpretError::Runtime {
                message: format!("Invalid expr: {:?}", s),
                line: 0,
            }),
            Expr::Void => Ok(Value::Null),
        }
    }

    pub fn execute(&mut self, expr: ExprNode) -> Result<Value, InterpretError> {
        println!("EXPR: {:?}", &expr);
        println!("EXPR-unparse: {:?}", expr.unparse());
        println!("EXPR-unlex: {:?}", expr.unlex());
        match expr.sexpr() {
            Ok(s) => {
                println!("EXPR-sexpr: {}", s);
            }
            Err(e) => {
                println!("ERROR: {:?}", e);
                return Err(InterpretError::Runtime {
                    message: "Unable to parse sexpr".into(),
                    line: 0,
                });
            }
        }

        self.evaluate(&expr)
    }

    pub fn interpret(&mut self, program: Program) {
        for expr in program.value {
            if let Err(error) = self.execute(expr) {
                println!("ERROR: {:?}", error);
                return;
            }
        }
    }
}
