use super::*;
use crate::ast::*;
use kaktus::PushPop;
use log::debug;
use rpds::HashTrieMap;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::From;
use std::fmt;
use std::rc::Rc;
use std::result::Result;

#[derive(Clone)]
//pub struct ExprRef(pub RefCell<Rc<Expr>>);
pub struct ExprRef(pub Rc<RefCell<ExprNode>>);

impl fmt::Debug for ExprRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ExprRef")
            .field("c", &Rc::strong_count(&self.0))
            .field("v", &self.as_ref().borrow())
            .finish()
    }
}

impl ExprRef {
    pub fn new(v: ExprNode) -> Self {
        Self(Rc::new(RefCell::new(v)))
        //Self(RefCell::new(Rc::new(v)))
    }

    /*
    pub fn _borrow<'a> (&'a self) -> &'a Rc<Expr> {
        //&self.as_ref().borrow()
        &self.borrow()
    }

    pub fn _borrow_mut<'a> (&'a mut self) -> &'a mut Rc<Expr> {
        //&mut self.as_ref().borrow_mut()
        &mut self.borrow_mut()
    }
    */
}

impl std::ops::Deref for ExprRef {
    //type Target = Rc<Expr>;
    type Target = Rc<RefCell<ExprNode>>;
    //type Target = RefCell<Rc<Expr>>;

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
        Self::new(item.into())
    }
}

impl From<ExprNode> for ExprRef {
    fn from(item: ExprNode) -> Self {
        Self::new(item)
    }
}

impl From<Box<ExprNode>> for ExprRef {
    fn from(item: Box<ExprNode>) -> Self {
        Self::new(*item) //Box::into_inner(item))
    }
}

//#[derive(Debug)]
pub struct Layer {
    values: HashTrieMap<String, ExprRef>,
}
impl Default for Layer {
    fn default() -> Self {
        Self {
            values: HashTrieMap::new(),
        }
    }
}

impl fmt::Debug for Layer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.values.iter().map(|(k, v)| (k, v)))
            .finish()
    }
}

impl Layer {
    pub fn define(&self, name: &str, value: ExprRef) -> Layer {
        Layer {
            values: self.values.insert(name.to_string(), value),
        }
    }

    pub fn contains(&self, name: &str) -> bool {
        self.values.contains_key(name)
    }

    pub fn get(&self, name: &str) -> Option<ExprRef> {
        match self.values.get(name) {
            Some(v) => Some(v.clone()),
            None => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    stack: kaktus::Stack<Layer>,
}

impl Default for Environment {
    fn default() -> Self {
        let stack: kaktus::Stack<Layer> = kaktus::Stack::root_default();
        let env = Self { stack };
        use super::builtins::*;
        env.define("clock", Clock::value().into());
        env.define("assert", Assert::value().into());
        env
    }
}

impl Environment {
    pub fn define(&self, name: &str, value: ExprRef) -> Self {
        let stack = &self.stack;
        let top = if stack.depth() == 0 || stack.peek().unwrap().contains(name) {
            stack.push_default()
        } else {
            stack.clone()
        };
        let top = top.define(name, value);
        let stack = stack.push(top);
        Environment { stack }
    }

    pub fn resolve(&self, name: &str) -> Option<ExprRef> {
        self.stack
            .walk()
            .find(|layer| layer.values.contains_key(name))
            .map(|layer| layer.get(name).unwrap())
    }

    pub fn debug(&self) {
        self.stack.walk().for_each(|layer| {
            debug!("Layer: {:?}", layer);
        })
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

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test() {
        let env = Environment::default();
        let env = env.define("x", Expr::new_int(1).into());
        let x1 = env.resolve("x").unwrap();
        let env = env.define("x", Expr::new_int(2).into());
        let env = env.define("y", Expr::new_int(3).into());
        let env = env.define("z", Expr::new_int(4).into());
        let x2 = env.resolve("x").unwrap();
        env.debug();
        drop(env);
        debug!("1:{:?}", x1);
        debug!("2:{:?}", x2);
        assert_eq!(2, Rc::strong_count(&x1.0));
        assert_eq!(2, Rc::strong_count(&x2.0));
    }
}
