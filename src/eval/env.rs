use crate::ast::*;
use log::debug;
use std::collections::HashMap;
use std::result::Result;
use super::*;


#[derive(Debug)]
pub struct Environment {
    values: HashMap<String, Expr>,
}
impl Default for Environment {
    fn default() -> Self {
        Self {
            values: HashMap::new(),
        }
    }
}
impl Environment {
    pub fn define(&mut self, name: &str, value: &Expr) {
        self.values.insert(name.to_string(), value.clone());
    }
    pub fn get(&self, name: &str) -> Result<Expr, InterpretError> {
        if let Some(value) = self.values.get(name) {
            return Ok(value.clone());
        }
        Err(InterpretError::Runtime {
            message: format!("Undefined variable '{}'.", name),
            line: 0, //name.line(),
        })
    }
}

