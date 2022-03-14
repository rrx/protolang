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

    fn check_number(&self) -> Result<f64, InterpretError> {
        match self {
            Self::Literal(t) => {
                match t {
                    Tok::IntLiteral(u) => Ok(*u as f64),
                    Tok::FloatLiteral(f) => Ok(*f),
                    _ => Err(InterpretError::Runtime {
                        message: format!("Expecting a number: {}", self.unlex()),
                        line: 0,
                    }),
                }
            }
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

    pub fn plus(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number().unwrap();
        let right = other.check_number().unwrap();
        let eval = left + right;
        Ok(Self::Literal(Tok::FloatLiteral(eval)))
    }

    pub fn minus(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left - right;
        Ok(Self::Literal(Tok::FloatLiteral(eval)))
    }

    pub fn exp(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left.powf(right);
        println!("{:?}", (&left, &right, &eval));
        Ok(Self::Literal(Tok::FloatLiteral(eval)))
        //Ok(Self::FloatLiteral(eval))
    }

    pub fn multiply(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left * right;
        Ok(Self::Literal(Tok::FloatLiteral(eval)))
        //Ok(Self::FloatLiteral(eval))
    }

    pub fn divide(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left / right;
        Ok(Self::Literal(Tok::FloatLiteral(eval)))
        //Ok(Self::FloatLiteral(eval))
    }

    pub fn lte(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left <= right;
        Ok(Self::Literal(Tok::BoolLiteral(eval)))
        //Ok(Self::BoolLiteral(eval))
    }

    pub fn lt(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left < right;
        Ok(Self::Literal(Tok::BoolLiteral(eval)))
        //Ok(Self::BoolLiteral(eval))
    }

    pub fn gte(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left >= right;
        Ok(Self::Literal(Tok::BoolLiteral(eval)))
        //Ok(Self::BoolLiteral(eval))
    }

    pub fn gt(&self, other: &Self) -> Result<Self, InterpretError> {
        let left = self.check_number()?;
        let right = other.check_number()?;
        let eval = left > right;
        Ok(Self::Literal(Tok::BoolLiteral(eval)))
        //Ok(Self::BoolLiteral(eval))
    }

    pub fn postfix(&self, op: &Operator) -> Result<Self, InterpretError> {
        match op {
            Operator::Bang => {
                let right = self.check_number()?;
                Ok(Self::Literal(Tok::FloatLiteral(right)))
            }
            _ => unimplemented!(),
        }
    }

    pub fn prefix(&self, prefix: &Operator) -> Result<Self, InterpretError> {
        match prefix {
            Operator::Plus => {
                let right = self.check_number()?;
                Ok(Self::Literal(Tok::FloatLiteral(right)))
                //Ok(Self::FloatLiteral(right))
            }
            Operator::Minus => {
                let right = self.check_number()?;
                Ok(Self::Literal(Tok::FloatLiteral(-right)))
                //Ok(Self::FloatLiteral(-right))
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
                    //Operator::Map => {
                    //Ok(InterpretValue::Lambda(e.clone()))
                    //}
                    _ => Err(InterpretError::Runtime {
                        message: format!("Unimplemented expression op: Operator::{:?}", op),
                        line: 0,
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
                    line: 0,
                })
            }
            Expr::Lambda(e) => {
                println!("Lambda({:?})", &e);
                Ok(Value::Callable(e.node()))
            }
            Expr::Index(ident, args) => {
                Ok(Value::Literal(Tok::IntLiteral(0)))
            }
            Expr::Apply(expr, args) => {
                let f = match &expr.value {
                    Expr::Ident(ident) => {
                        Some(self.globals.get(ident)?)
                    }
                    _ => None
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
                //env.define(
                //Ok(Value::IntLiteral(0))
            }
            Expr::Block(_) => Ok(Value::Literal(Tok::IntLiteral(0))),
            Expr::Ternary(_,_,_,_) => Ok(Value::Literal(Tok::IntLiteral(0))),
            Expr::Chain(_,_) => Ok(Value::Literal(Tok::IntLiteral(0))),
        }
    }

    pub fn execute(&mut self, stmt: StmtNode) -> Result<(), InterpretError> {
        println!("STMT-unparse: {:?}", stmt.unparse());
        println!("STMT-unlex: {:?}", stmt.unlex());
        match stmt.sexpr() {
            Ok(s) => {
                println!("STMT-sexpr: {}", s);
            }
            Err(e) => {
                println!("ERROR: {:?}", e);
                return Err(InterpretError::Runtime {
                    message: "Unable to parse sexpr".into(),
                    line: 0,
                });
            }
        }

        match stmt.value {
            Stmt::Expr(expr) => {
                let value = self.evaluate(&expr)?;
                println!("Evaluate Expr: {} -> {}", expr.unlex(), value.unlex());
                Ok(())
            }
            Stmt::Lit(lit) => {
                println!("Evaluate Literal: {:?}", lit);//.value);
                Ok(())
            }
            Stmt::Assign(ident_expr, expr) => {
                if let Expr::Ident(ident) = ident_expr.value {
                    let value = self.evaluate(&expr)?;
                    self.globals.define(&ident, &value);
                    // assign value to ident
                    println!("Assign {:?} to {}", &value, &ident);
                    Ok(())
                } else {
                    Err(InterpretError::Runtime {
                        message: format!("Invalid Assignment, LHS must be identifier"),
                        line: ident_expr.context.loc.line,
                    })
                }
            }
            Stmt::Invalid(line) => Err(InterpretError::Runtime {
                message: format!("Invalid Statement {}", line),
                line: stmt.loc.line,
            }),
            _ => Err(InterpretError::Runtime {
                message: format!("Unimplemented {:?}", stmt.value),
                line: stmt.loc.line,
            }),
        }
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
