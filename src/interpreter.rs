use crate::ast::*;
use crate::sexpr::SExpr;
use std::collections::HashMap;
use std::result::Result;

#[derive(Debug)]
pub struct Environment {
    values: HashMap<String, InterpretValue>,
}
impl Default for Environment {
    fn default() -> Self {
        Self {
            values: HashMap::new(),
        }
    }
}
impl Environment {
    pub fn define(&mut self, name: &str, value: &InterpretValue) {
        self.values.insert(name.to_string(), value.clone());
    }
    pub fn get(&self, name: &str) -> Result<InterpretValue, InterpretError> {
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
        Self {
            globals: Environment::default(),
        }
    }
}

impl Interpreter {}

#[derive(Debug)]
pub enum InterpretError {
    Invalid,
    Runtime { message: String, line: usize },
}

#[derive(Debug, Clone)]
pub enum InterpretValue {
    Literal(Literal),
    Lambda(Lambda),
}

impl InterpretValue {

    pub fn unlex(&self) -> String {
        match self {
            InterpretValue::Literal(x) => x.token().to_string(),
            InterpretValue::Lambda(x) => x.unlex(),
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Self::Literal(Literal::IntLiteral(_)) => true,
            Self::Literal(Literal::FloatLiteral(_)) => true,
            _ => false,
        }
    }

    fn check_number(&self) -> Result<f64, InterpretError> {
        match self {
            Self::Literal(Literal::IntLiteral(u)) => Ok(*u as f64),
            Self::Literal(Literal::FloatLiteral(f)) => Ok(*f),
            _ => Err(InterpretError::Runtime {
                message: "Expecting a number".into(),
                line: 0,
            }),
        }
    }

    pub fn plus(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left + right;
        Ok(Self::Literal(Literal::FloatLiteral(eval)))
    }

    pub fn minus(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left - right;
        Ok(Self::Literal(Literal::FloatLiteral(eval)))
    }

    pub fn exp(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left.powf(right);
        println!("{:?}", (&left, &right, &eval));
        Ok(Self::Literal(Literal::FloatLiteral(eval)))
    }

    pub fn multiply(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left * right;
        Ok(Self::Literal(Literal::FloatLiteral(eval)))
    }

    pub fn divide(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left / right;
        Ok(Self::Literal(Literal::FloatLiteral(eval)))
    }

    pub fn lte(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left <= right;
        Ok(Self::Literal(Literal::BoolLiteral(eval)))
    }

    pub fn lt(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left < right;
        Ok(Self::Literal(Literal::BoolLiteral(eval)))
    }

    pub fn gte(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left >= right;
        Ok(Self::Literal(Literal::BoolLiteral(eval)))
    }

    pub fn gt(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left > right;
        Ok(Self::Literal(Literal::BoolLiteral(eval)))
    }

    pub fn prefix(&self, prefix: &Prefix) -> Result<Self, InterpretError> {
        match prefix {
            Prefix::PrefixPlus => {
                let right = self.check_number()?;
                Ok(Self::Literal(Literal::FloatLiteral(right)))
            }
            Prefix::PrefixMinus => {
                let right = self.check_number()?;
                Ok(Self::Literal(Literal::FloatLiteral(-right)))
            }
            _ => unimplemented!(),
        }
    }
}

impl Interpreter {
    pub fn evaluate(&mut self, expr: &ExprNode) -> Result<InterpretValue, InterpretError> {
        match &expr.value {
            Expr::LitExpr(lit) => Ok(InterpretValue::Literal(lit.value.clone())),
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
                Ok(InterpretValue::Lambda(e.clone()))
            }
        }
    }

    pub fn execute(&mut self, stmt: StmtNode) -> Result<(), InterpretError> {
        match stmt.value {
            Stmt::Expr(expr) => {
                let value = self.evaluate(&expr)?;
                println!("sexpr Expr: {}", expr.sexpr().unwrap());
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
