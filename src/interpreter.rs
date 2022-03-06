use crate::ast::*;
use crate::value::*;
use crate::tokens::Tok;
use crate::function::*;
use crate::sexpr::SExpr;
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
        Self {
            globals
        }
    }
}

impl Interpreter {}

#[derive(Debug)]
pub enum InterpretError {
    Invalid,
    Runtime { message: String, line: usize },
}

impl Unparse for Value {
    fn unparse(&self) -> Vec<Tok> {
        vec![self.token()]
    }
}

impl Value {
    pub fn is_number(&self) -> bool {
        match self {
            Self::IntLiteral(_) => true,
            Self::FloatLiteral(_) => true,
            _ => false,
        }
    }

    fn check_number(&self) -> Result<f64, InterpretError> {
        match self {
            Self::IntLiteral(u) => Ok(*u as f64),
            Self::FloatLiteral(f) => Ok(*f),
            Self::Callable(e) => Err(InterpretError::Runtime {
                message: format!(
                             "Expecting a number, got a lambda: {} on line:{}, column:{}, fragment:{}",
                             self.unlex(), e.loc.line, e.loc.col, e.loc.fragment),
                line: e.loc.line,
            }),
            _ => Err(InterpretError::Runtime {
                message: format!("Expecting a number: {}", self.unlex()),
                line: 0,
            }),
        }
    }

    pub fn plus(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number().unwrap();
        let right = other.check_number().unwrap();
        let eval = left + right;
        Ok(Self::FloatLiteral(eval))
    }

    pub fn minus(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left - right;
        Ok(Self::FloatLiteral(eval))
    }

    pub fn exp(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left.powf(right);
        println!("{:?}", (&left, &right, &eval));
        Ok(Self::FloatLiteral(eval))
    }

    pub fn multiply(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left * right;
        Ok(Self::FloatLiteral(eval))
    }

    pub fn divide(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left / right;
        Ok(Self::FloatLiteral(eval))
    }

    pub fn lte(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left <= right;
        Ok(Self::BoolLiteral(eval))
    }

    pub fn lt(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left < right;
        Ok(Self::BoolLiteral(eval))
    }

    pub fn gte(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left >= right;
        Ok(Self::BoolLiteral(eval))
    }

    pub fn gt(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left > right;
        Ok(Self::BoolLiteral(eval))
    }

    pub fn prefix(&self, prefix: &Prefix) -> Result<Self, InterpretError> {
        match prefix {
            Prefix::PrefixPlus => {
                let right = self.check_number()?;
                Ok(Self::FloatLiteral(right))
            }
            Prefix::PrefixMinus => {
                let right = self.check_number()?;
                Ok(Self::FloatLiteral(-right))
            }
            _ => unimplemented!(),
        }
    }
}

impl Interpreter {
    pub fn evaluate(&mut self, expr: &ExprNode) -> Result<Value, InterpretError> {
        match &expr.value {
            Expr::LitExpr(lit) => Ok(lit.value.clone()),
            Expr::IdentExpr(ident) => self.globals.get(ident.value.as_str()),
            Expr::PrefixExpr(prefix, expr) => {
                let eval = self.evaluate(&expr)?;
                let eval = eval.prefix(&prefix.value)?;
                Ok(eval)
            }
            Expr::InfixExpr(op, left, right) => {
                let eval_left = self.evaluate(&left)?;
                let eval_right = self.evaluate(&right)?;
                match op.value {
                    Infix::Plus => eval_left.plus(&eval_right),
                    Infix::Minus => eval_left.minus(&eval_right),
                    Infix::Exp => eval_left.exp(&eval_right),
                    Infix::Multiply => eval_left.multiply(&eval_right),
                    Infix::Divide => eval_left.divide(&eval_right),
                    Infix::GreaterThanEqual => eval_left.gte(&eval_right),
                    Infix::LessThanEqual => eval_left.lte(&eval_right),
                    Infix::GreaterThan => eval_left.gt(&eval_right),
                    Infix::LessThan => eval_left.lt(&eval_right),
                    //Infix::Map => {
                        //Ok(InterpretValue::Lambda(e.clone()))
                    //}
                    _ => {
                        Err(InterpretError::Runtime {
                            message: format!("Unimplemented expression op: Infix::{:?}", op.value),
                            line: 0
                        })
                    }
                }
            }
            Expr::Lambda(e) => {
                Ok(Value::Callable(e.node()))
            }
            Expr::Apply(ident, args) => {
                let f = self.globals.get(ident.value.as_str())?;
                let env = Environment::default();
                match f {
                    Value::Callable(mut c) => {
                        let mut eval_args = vec![];
                        for arg in args {
                            eval_args.push(self.evaluate(arg)?);
                        }
                        println!("Calling {:?}({:?})", c, eval_args);
                        let result = c.value.call(self, eval_args);
                        println!("Result {:?}", &result);
                        result
                    }
                    _ => {
                        Err(InterpretError::Runtime {
                            message: format!("Not a function: {:?}", f),
                            line: 0
                        })
                    }
                }
                //env.define(
                //Ok(Value::IntLiteral(0))
            }
            Expr::Block(stmts) => {
                Ok(Value::IntLiteral(0))
            }
        }
    }

    pub fn execute(&mut self, stmt: StmtNode) -> Result<(), InterpretError> {
        match stmt.value {
            Stmt::Expr(expr) => {
                println!("sexpr Expr: {}", expr.sexpr().unwrap());
                println!("expr Expr: {}", expr.unlex());
                let value = self.evaluate(&expr)?;
                println!("Evaluate Expr: {} -> {}", expr.unlex(), value.unlex());
            }
            Stmt::Lit(lit) => {
                println!("Evaluate Literal: {:?}", lit.value);
            }
            Stmt::Assign(ident, expr) => {
                let value = self.evaluate(&expr)?;
                self.globals.define(ident.value.as_str(), &value);
                // assign value to ident
                println!("Save {:?} to {}", value, ident.value);
            }
            _ => {
                return Err(InterpretError::Runtime { message: format!("Unimplemented {:?}", stmt.value), line: 0 });
            }
        }
        Ok(())
    }

    pub fn interpret(&mut self, program: Program) {
        for stmt in program.value {
            if let Err(error) = self.execute(stmt) {
                println!("ERROR: {:?}", error);
                return;
            }
        }
    }
}
