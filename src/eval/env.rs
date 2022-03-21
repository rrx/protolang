use super::*;
use crate::ast::*;
use kaktus::PushPop;
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
    pub modifier: VarModifier
}
impl ExprAccessRef {
    pub fn new(expr: ExprRef, modifier: VarModifier) -> Self {
        Self { expr, modifier }
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

//#[derive(Debug)]
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
            values: self.values.insert(name, accessref)
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
    stack: kaktus::Stack<Layer>,
}

impl Default for Environment {
    fn default() -> Self {
        let stack: kaktus::Stack<Layer> = kaktus::Stack::root_default();
        let env = Self { stack };
        use super::builtins::*;
        env.define("clock".into(), Clock::value().into())
            .define("assert".into(), Assert::value().into())
            .define("showstack".into(), ShowStack::value().into())
    }
}

impl Environment {
    pub fn define(&self, identifier: Identifier, value: ExprRef) -> Self {
        let stack = &self.stack;
        let top = if stack.depth() == 0 || stack.peek().unwrap().contains(&identifier.name) {
            stack.push_default()
        } else {
            stack.clone()
        };
        let top = top.define(identifier, value);
        let stack = stack.push(top);
        Environment { stack }
    }

    pub fn resolve(&self, name: &str) -> Option<ExprAccessRef> {
        self.stack
            .walk()
            .find(|layer| layer.values.contains_key(name))
            .map(|layer| layer.get(name).unwrap())
    }

    pub fn debug(&self) {
        self.stack.walk().for_each(|layer| {
            debug!("Layer: {:?}", layer);
            layer.values.iter().for_each(|(k,v)| {
                debug!("\t{}: {:?}", k, v);
            });
        })
    }

    pub fn get_at(&self, name: &str, context: &MaybeNodeContext) -> Result<ExprAccessRef, InterpretError> {
        if let Some(value) = self.resolve(name) {
            return Ok(value);
        }
        Err(context.error(&format!("Undefined variable '{}'.", name)))
    }

    pub fn get_(&self, name: &str) -> Result<ExprAccessRef, InterpretError> {
        if let Some(value) = self.resolve(name) {
            return Ok(value);
        }
        Err(InterpretError::runtime(&format!("Undefined variable '{}'.", name)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test() {
        let env = Environment::default();
        let env = env.define("x".into(), Expr::new_int(1).into());
        let x1 = env.resolve("x".into()).unwrap();
        let env = env.define("x".into(), Expr::new_int(2).into());
        let env = env.define("y".into(), Expr::new_int(3).into());
        let env = env.define("z".into(), Expr::new_int(4).into());
        let x2 = env.resolve("x".into()).unwrap();
        env.debug();
        drop(env);
        debug!("1:{:?}", x1);
        debug!("2:{:?}", x2);
        assert_eq!(2, Rc::strong_count(&x1.expr.0));
        assert_eq!(2, Rc::strong_count(&x2.expr.0));
    }
}
