use std::cell::{Ref, RefCell};
use std::rc::Rc;

#[derive(Clone, PartialEq)]
pub enum TypeModifier {
    Ref,
    MutRef,
    Mut,
    Default,
}

#[derive(Clone, Debug)]
pub struct ErrorSig {}

#[derive(Clone, Debug)]
pub enum FunctionModifier {
    Pure,
    Default,
}

#[derive(Clone, Debug)]
pub enum Arity {
    // for varargs, the last arg is the type of the varargs
    NMore(usize),
    // fixed length
    N(usize),
    // Allow curry
    Curry(usize),
}
impl Arity {
    pub fn is_valid_arity(&self, arity: usize) -> bool {
        match self {
            Self::NMore(n) => &arity <= n,
            Self::N(n) => &arity == n,
            Self::Curry(n) => &arity <= n,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeSig {
    // can be anonymous
    name: Option<String>,

    modifier: FunctionModifier,
    pub arity: Arity,
    // 0 args is just a value
    args: Vec<TypeSig>,
    err: Vec<ErrorSig>,
    traits: Vec<Trait>,
}

impl Default for TypeSig {
    fn default() -> Self {
        Self {
            name: None,
            modifier: FunctionModifier::Default,
            arity: Arity::N(0),
            args: vec![],
            err: vec![],
            traits: vec![],
        }
    }
}

impl TypeSig {
    pub fn set_name(&self, name: String) -> Self {
        let mut s = self.clone();
        s.name = Some(name);
        s
    }

    pub fn value(t: TypeSig) -> Self {
        Self {
            name: None,
            modifier: FunctionModifier::Pure,
            arity: Arity::N(0),
            args: vec![t],
            err: vec![],
            traits: vec![],
        }
    }

    pub fn void() -> Self {
        Self {
            name: None,
            modifier: FunctionModifier::Default,
            arity: Arity::N(0),
            args: vec![],
            err: vec![],
            traits: vec![],
        }
    }

    pub fn with_arity(n: usize) -> Self {
        Self {
            name: None,
            modifier: FunctionModifier::Default,
            arity: Arity::N(n),
            args: vec![],
            err: vec![],
            traits: vec![],
        }
    }

    // A -> A
    pub fn single_pure(arg: TypeSig) -> Self {
        let ret = arg.clone();
        Self {
            name: None,
            modifier: FunctionModifier::Pure,
            arity: Arity::N(1),
            args: vec![arg, ret],
            err: vec![],
            traits: vec![],
        }
    }
}

#[derive(Clone, Debug)]
pub struct RefTypeSig(pub Rc<RefCell<TypeSig>>);

impl std::ops::Deref for RefTypeSig {
    type Target = RefCell<TypeSig>;

    fn deref(&self) -> &Self::Target {
        &self.0.as_ref()
    }
}

#[derive(Clone, Debug)]
pub struct Trait {
    pub sigs: Vec<TypeSig>,
}

impl Trait {
    pub fn bool() -> Self {
        Self { sigs: vec![] }
    }
}

#[derive(Clone, Debug)]
pub struct RefTrait(pub Rc<RefCell<Trait>>);

impl std::ops::Deref for RefTrait {
    type Target = RefCell<Trait>;

    fn deref(&self) -> &Self::Target {
        &self.0.as_ref()
    }
}
