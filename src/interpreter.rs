use crate::ast::*;
use std::result::Result;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Environment {
    values: HashMap<String, InterpretValue>
}
impl Default for Environment {
    fn default() -> Self {
        Self { values: HashMap::new() }
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
            line: 0//name.line(),
        })
    }
}

#[derive(Debug)]
pub struct Interpreter {
    globals: Environment,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self { globals: Environment::default() }
    }
}

impl Interpreter {
}

#[derive(Debug)]
pub enum InterpretError {
    Invalid,
    Runtime { message: String, line: usize }
}

#[derive(Debug, Clone)]
pub enum InterpretValue {
    Literal(Literal)
}

impl InterpretValue {
    pub fn is_number(&self) -> bool {
        match self {
            Self::Literal(Literal::IntLiteral(_)) => true,
            Self::Literal(Literal::FloatLiteral(_)) => true,
            _ => false
        }
    }
    fn check_number(&self) -> Result<f64, InterpretError> {
        match self {
            Self::Literal(Literal::IntLiteral(u)) => Ok(*u as f64),
            Self::Literal(Literal::FloatLiteral(f)) => Ok(*f),
            _ => Err(InterpretError::Runtime {
                message: "Expecting a number".into(),
                line: 0
            })
        }
    }

    pub fn plus(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left + right;
        Ok(Self::Literal(Literal::FloatLiteral(eval)))
    }

    pub fn prefix(&self, prefix: &Prefix) -> Result<Self, InterpretError> {
        match prefix {
            Prefix::PrefixPlus =>  {
                let right = self.check_number()?;
                Ok(Self::Literal(Literal::FloatLiteral(right)))
            }
            Prefix::PrefixMinus => {
                let right = self.check_number()?;
                Ok(Self::Literal(Literal::FloatLiteral(-right)))
            }
            _ => unimplemented!()
        }
    }
}

impl Interpreter {
    pub fn evaluate(&mut self, expr: &ExprNode) -> Result<InterpretValue, InterpretError> {
        match &expr.value {
            Expr::LitExpr(lit) => Ok(InterpretValue::Literal(lit.value.clone())),
            Expr::IdentExpr(ident) => {
                self.globals.get(ident.value.as_str())
            }
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
                    _ => unimplemented!()
                }
            }
            _ => unimplemented!()
        }
    }

    pub fn execute(&mut self, stmt: StmtNode) -> Result<(), InterpretError> {
        match stmt.value {
            Stmt::Expr(expr) => {
                let value = self.evaluate(&expr)?;
                println!("Evaluate Expr: {:?}", value);
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
            _ => unimplemented!()
        }
        Ok(())
    }

    pub fn interpret(&mut self, program: Program) {
        for stmt in program.value {
            if let Err(error) = self.execute(stmt) {
                println!("{:?}", error);
                return;
            }
        }
    }
}
