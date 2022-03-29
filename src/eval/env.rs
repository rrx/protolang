use super::RefTypeSig;
use crate::ast::CallTable;
use crate::ast::*;
use crate::results::InterpretError;
use log::debug;
use rpds::HashTrieMap;
use std::cell::{Ref, RefCell};
use std::convert::From;
use std::fmt;
use std::rc::{Rc, Weak};
use std::result::Result;

#[derive(Clone)]
pub struct ExprRef(pub Rc<RefCell<ExprNode>>);

impl ExprRef {
    pub fn new(v: ExprNode) -> Self {
        Self(Rc::new(RefCell::new(v)))
    }

    pub fn mutate(self, expr: ExprNode) -> Self {
        self.replace(expr);
        self
    }

    pub fn downgrade(&self) -> ExprWeakRef {
        ExprWeakRef::new(self)
    }

    pub fn borrow(&self) -> Ref<ExprNode> {
        self.as_ref().borrow()
    }
}

impl std::ops::Deref for ExprRef {
    type Target = Rc<RefCell<ExprNode>>;

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

#[derive(Clone, Debug)]
pub struct ExprWeakRef(pub Weak<RefCell<ExprNode>>);

impl ExprWeakRef {
    pub fn new(v: &ExprRef) -> Self {
        Self(Rc::downgrade(v))
    }
}

#[derive(Clone, Debug)]
pub struct ExprAccessRef {
    pub expr: ExprRef,
    pub modifier: VarModifier,
}

impl ExprAccessRef {
    pub fn new(expr: ExprRef, modifier: &VarModifier) -> Self {
        Self {
            expr,
            modifier: modifier.clone(),
        }
    }
    pub fn is_mut(&self) -> bool {
        self.modifier == VarModifier::Mutable
    }
}

impl From<Expr> for ExprAccessRef {
    fn from(item: Expr) -> Self {
        Self::new(item.into(), &VarModifier::Default)
    }
}

#[derive(Clone, Debug)]
pub struct ExprAccessWeakRef {
    pub expr: ExprWeakRef,
    pub modifier: VarModifier,
}
impl ExprAccessWeakRef {
    pub fn new(expr: &ExprRef, modifier: VarModifier) -> Self {
        Self {
            expr: ExprWeakRef::new(&expr),
            modifier,
        }
    }
    pub fn is_mut(&self) -> bool {
        self.modifier == VarModifier::Mutable
    }
    pub fn upgrade(&self) -> Option<ExprAccessRef> {
        self.expr
            .0
            .upgrade()
            .map(|e| ExprAccessRef::new(ExprRef(e), &self.modifier))
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

#[derive(Clone)]
pub struct Layer {
    values: HashTrieMap<String, ExprAccessRef>,
    types: HashTrieMap<String, RefTypeSig>,
    weak: HashTrieMap<String, ExprAccessWeakRef>,
}

impl Default for Layer {
    fn default() -> Self {
        Self {
            values: HashTrieMap::new(),
            weak: HashTrieMap::new(),
            types: HashTrieMap::new(),
        }
    }
}

impl fmt::Debug for Layer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.values.iter().map(|(k, v)| (k, v)))
            //.entries(self.builtins.iter().map(|(k, _)| (k, "")))
            .finish()
    }
}

impl Layer {
    pub fn define_value(&self, identifier: Identifier, value: ExprRef) -> Layer {
        let name = identifier.name.to_string();
        let accessref = ExprAccessRef::new(value, &identifier.modifier);
        Layer {
            values: self.values.insert(name, accessref),
            weak: HashTrieMap::new(),
            types: HashTrieMap::new(),
        }
    }

    pub fn define_type(&self, identifier: Identifier, value: RefTypeSig) -> Layer {
        let name = identifier.name.to_string();
        Layer {
            types: self.types.insert(name, value),
            values: HashTrieMap::new(),
            weak: HashTrieMap::new(),
        }
    }

    pub fn define_weak(&self, identifier: Identifier, value: &ExprRef) -> Layer {
        let name = identifier.name.to_string();
        let accessref = ExprAccessWeakRef::new(&value, identifier.modifier);
        Layer {
            weak: self.weak.insert(name, accessref),
            values: HashTrieMap::new(),
            types: HashTrieMap::new(),
        }
    }

    pub fn contains(&self, name: &str) -> bool {
        self.values.contains_key(name)
    }

    pub fn get_type(&self, name: &str) -> Option<RefTypeSig> {
        self.types.get(name).map(|v| v.clone())
    }

    pub fn get_value(&self, name: &str) -> Option<ExprAccessRef> {
        match self.values.get(name) {
            Some(expr) => Some(expr.clone()),
            None => match self.weak.get(name) {
                Some(expr) => expr.upgrade(),
                None => None,
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    stack: im::vector::Vector<Layer>,
    builtins: CallTable,
}

impl Default for Environment {
    fn default() -> Self {
        let mut stack = im::Vector::new();

        let layer = Layer {
            values: HashTrieMap::new(),
            types: HashTrieMap::new(),
            weak: HashTrieMap::new(),
        };

        stack.push_front(layer);
        Self {
            stack,
            builtins: crate::eval::builtins::builtins(CallTable::new()),
        }
    }
}

impl Environment {
    pub fn define(mut self, identifier: Identifier, value: ExprRef) -> Self {
        if self.stack.len() > 0 && self.stack.front().unwrap().contains(&identifier.name) {
            let layer = Layer::default().define_value(identifier, value);
            self.stack.push_front(layer);
            self
        } else {
            match self.stack.pop_front() {
                Some(layer) => {
                    let layer = layer.define_value(identifier, value);
                    self.stack.push_front(layer);
                    self
                }
                None => {
                    let layer = Layer::default().define_value(identifier, value);
                    self.stack.push_front(layer);
                    self
                }
            }
        }
    }

    pub fn resolve_type(&self, name: &str) -> Option<RefTypeSig> {
        self.stack
            .iter()
            .find(|layer| layer.types.contains_key(name))
            .map(|layer| layer.get_type(name))
            .flatten()
    }

    pub fn resolve_value(&self, name: &str) -> Option<ExprAccessRef> {
        match self
            .stack
            .iter()
            .find(|layer| layer.values.contains_key(name))
            .map(|layer| layer.get_value(name))
            .flatten()
        {
            Some(b) => Some(b),
            None => self
                .builtins
                .get(name)
                .map(|b| Expr::Callback(b.clone()).into()),
        }
    }

    /*
    pub fn resolve_cb(&self, name: &str) -> Option<&Callback> {
        self.debug();
        self.stack
            .iter()
            .find(|layer| layer.builtins.contains_key(name))
            .map(|layer| layer.builtins.get(name))
            .flatten()
    }
    */

    pub fn debug(&self) {
        self.stack.iter().enumerate().for_each(|(i, layer)| {
            debug!("Layer: {:?}", i);
            layer.values.iter().for_each(|(k, v)| {
                debug!("\t{}: {:?}", k, v);
            });
            self.builtins.iter().for_each(|(k, _)| {
                debug!("\tbuiltin: {}", k);
            });
        })
    }

    pub fn get_at(
        &self,
        name: &str,
        context: &MaybeNodeContext,
    ) -> Result<ExprAccessRef, InterpretError> {
        if let Some(value) = self.resolve_value(name) {
            return Ok(value);
        }
        Err(context.runtime_error(&format!("Undefined variable '{}'.", name)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;
    #[test]
    fn test() {
        let env = Environment::default();
        let x1 = Expr::from(1).into();
        let env = env.define("x".into(), x1);
        let x1 = env.resolve_value("x".into()).unwrap();

        let env = env.define("x".into(), Expr::from(2).into());
        let env = env.define("y".into(), Expr::from(3).into());
        let env = env.define("y".into(), Expr::from(3).into());
        let env_before_z = env.clone();
        let env = env.define("z".into(), Expr::from(4).into());
        env.debug();
        let x2 = env.resolve_value("x".into()).unwrap();
        let y = env.resolve_value("y".into()).unwrap();
        let _ = env.resolve_value("clock".into()).unwrap();

        debug!("1:{:?}", x1);
        debug!("2:{:?}", x2);
        debug!("3:{:?}", y);

        // make sure z isn't visible
        assert!(env_before_z.resolve_value("z".into()).is_none());

        assert_eq!(2, Rc::strong_count(&x1.expr.0));
        assert_eq!(2, Rc::strong_count(&x2.expr.0));

        // verify that the references are dropped after dropping env
        drop(env);
        drop(env_before_z);

        assert_eq!(1, Rc::strong_count(&x1.expr.0));
        assert_eq!(1, Rc::strong_count(&x2.expr.0));
    }
}
