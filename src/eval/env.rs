use super::*;
use crate::ast::*;
use log::debug;
use rpds::HashTrieMap;
use std::cell::RefCell;
use std::convert::From;
use std::fmt;
use std::rc::Rc;
use std::result::Result;

#[derive(Clone)]
//pub struct ExprRef(pub RefCell<Rc<Expr>>);
pub struct ExprRef(pub Rc<RefCell<ExprNode>>);

#[derive(Clone, Debug)]
pub struct ExprAccessRef {
    pub expr: ExprRef,
    pub modifier: VarModifier,
}
impl ExprAccessRef {
    pub fn new(expr: ExprRef, modifier: VarModifier) -> Self {
        Self { expr, modifier }
    }
    pub fn is_mut(&self) -> bool {
        self.modifier == VarModifier::Mutable
    }
}

#[derive(Debug)]
pub struct ExprRefWithEnv {
    pub expr: ExprRef,
    pub env: Environment,
}

impl ExprRefWithEnv {
    pub fn new(expr: ExprRef, env: Environment) -> Self {
        Self { expr, env }
    }
}

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

    pub fn mutate(self, expr: ExprNode) -> Self {
        self.replace(expr);
        self
    }
}

impl std::ops::Deref for ExprRef {
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

impl From<Expr> for ExprAccessRef {
    fn from(item: Expr) -> Self {
        Self::new(item.into(), VarModifier::Default)
    }
}

impl From<ExprAccessRef> for ExprRef {
    fn from(item: ExprAccessRef) -> Self {
        item.expr
    }
}

impl From<ExprNode> for ExprRef {
    fn from(item: ExprNode) -> Self {
        Self::new(item)
    }
}

impl From<Box<ExprNode>> for ExprRef {
    fn from(item: Box<ExprNode>) -> Self {
        Self::new(*item)
    }
}

#[derive(Clone)]
pub struct Layer {
    values: HashTrieMap<String, ExprAccessRef>,
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
    pub fn define(&self, identifier: Identifier, value: ExprRef) -> Layer {
        let name = identifier.name.to_string();
        let accessref = ExprAccessRef::new(value, identifier.modifier);
        Layer {
            values: self.values.insert(name, accessref),
        }
    }

    pub fn contains(&self, name: &str) -> bool {
        self.values.contains_key(name)
    }

    pub fn get(&self, name: &str) -> Option<ExprAccessRef> {
        match self.values.get(name) {
            Some(expr) => Some(expr.clone()),
            None => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    stack: im::vector::Vector<Layer>,
}

impl Default for Environment {
    fn default() -> Self {
        let stack = im::Vector::new();
        let env = Self { stack };
        use super::builtins::*;
        env.define("clock".into(), Clock::value().into())
            .define("assert".into(), Assert::value().into())
            .define("showstack".into(), ShowStack::value().into())
    }
}

impl Environment {
    pub fn define(mut self, identifier: Identifier, value: ExprRef) -> Self {
        let name = identifier.name.clone();
        if self.stack.len() > 0 && self.stack.front().unwrap().contains(&identifier.name) {
            let layer = Layer::default().define(identifier, value);
            //println!("[{}]dup:{}", self.stack.len(), &name);
            self.stack.push_front(layer);
            self
            //Environment { stack: self.stack.clone() }
        } else {
            match self.stack.pop_front() {
                Some(layer) => {
                    //println!("[{}]pop:{}", self.stack.len(), &name);
                    let layer = layer.define(identifier, value);
                    self.stack.push_front(layer);
                    self
                    //Environment { stack : stack.clone() }
                }
                None => {
                    //println!("[{}]none:{}", self.stack.len(), &name);
                    let layer = Layer::default().define(identifier, value);
                    self.stack.push_front(layer);
                    self
                    //Environment { stack : stack.clone() }
                }
            }
        }
    }

    pub fn resolve(&self, name: &str) -> Option<ExprAccessRef> {
        self.stack
            .iter()
            .find(|layer| layer.values.contains_key(name))
            .map(|layer| layer.get(name).unwrap())
    }

    pub fn debug(&self) {
        self.stack.iter().enumerate().for_each(|(i, layer)| {
            debug!("Layer: {:?}", i);
            layer.values.iter().for_each(|(k, v)| {
                debug!("\t{}: {:?}", k, v);
            });
        })
    }

    pub fn get_at(
        &self,
        name: &str,
        context: &MaybeNodeContext,
    ) -> Result<ExprAccessRef, InterpretError> {
        if let Some(value) = self.resolve(name) {
            return Ok(value);
        }
        Err(context.error(&format!("Undefined variable '{}'.", name)))
    }

    pub fn get_(&self, name: &str) -> Result<ExprAccessRef, InterpretError> {
        if let Some(value) = self.resolve(name) {
            return Ok(value);
        }
        Err(InterpretError::runtime(&format!(
            "Undefined variable '{}'.",
            name
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;
    #[test]
    fn test() {
        let env = Environment::default();
        let env = env.define("x".into(), Expr::from(1).into());
        let x1 = env.resolve("x".into()).unwrap();

        let env = env.define("x".into(), Expr::from(2).into());
        let env = env.define("y".into(), Expr::from(3).into());
        let env = env.define("y".into(), Expr::from(3).into());
        let env_before_z = env.clone();
        let env = env.define("z".into(), Expr::from(4).into());
        env.debug();
        let x2 = env.resolve("x".into()).unwrap();
        let y = env.resolve("y".into()).unwrap();
        let _ = env.resolve("clock".into()).unwrap();

        debug!("1:{:?}", x1);
        debug!("2:{:?}", x2);
        debug!("3:{:?}", y);

        // make sure z isn't visible
        assert!(env_before_z.resolve("z".into()).is_none());

        assert_eq!(2, Rc::strong_count(&x1.expr.0));
        assert_eq!(2, Rc::strong_count(&x2.expr.0));

        // verify that the references are dropped after dropping env
        drop(env);
        drop(env_before_z);

        assert_eq!(1, Rc::strong_count(&x1.expr.0));
        assert_eq!(1, Rc::strong_count(&x2.expr.0));
    }
}
