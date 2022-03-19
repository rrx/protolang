use crate::ast::*;
use log::debug;
use std::collections::HashMap;
use std::result::Result;
use super::*;
use kaktus::PushPop;
use std::rc::Rc;
use std::convert::From;

#[derive(Debug, Clone)]
pub struct ExprRef(pub Rc<Expr>);

impl std::ops::Deref for ExprRef {
    type Target = Rc<Expr>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for ExprRef {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}


impl From<Expr> for ExprRef {
    fn from(item: Expr) -> Self {
        ExprRef(Rc::new(item))
    }
}

#[derive(Debug)]
pub struct Layer {
    values: HashMap<String, Rc<Expr>>,
}
impl Default for Layer {
    fn default() -> Self {
        Self {
            values: HashMap::new(),
        }
    }
}

impl Layer {
    pub fn define(&mut self, name: &str, value: &Expr) {
        self.values.insert(name.to_string(), Rc::new(value.clone()));
    }

    pub fn get(&self, name: &str) -> Option<ExprRef> {
        match self.values.get(name) {
            Some(v) => Some(ExprRef(v.clone())),
            None => None
        }
    }
}

#[derive(Debug)]
pub struct Environment {
    stack: kaktus::Stack<Layer>,
    base: Layer,
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            stack: kaktus::Stack::root_default(),
            base: Layer::default()
        }
    }
}

impl Environment {
    pub fn define(&mut self, name: &str, value: &Expr) {
        // XXX
        //self.stack.peek().unwrap().define(name, value);
        self.base.define(name, value);
    }

    pub fn resolve(&self, name: &str) -> Option<ExprRef> {
        // XXX
        self.base.get(name)
        //self.stack.walk().find(|layer| layer.values.contains_key(name)).map(|layer| {
            //layer.get(name).unwrap()
        //});
        //None
    }

    pub fn get(&self, name: &str) -> Result<ExprRef, InterpretError> {
        if let Some(value) = self.resolve(name) {
            return Ok(value);
        }
        Err(InterpretError::Runtime {
            message: format!("Undefined variable '{}'.", name),
            line: 0, //name.line(),
        })
    }
}

