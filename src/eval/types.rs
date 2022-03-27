use std::rc::Rc;
use std::cell::{Ref, RefCell};

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

#[derive(Debug, Clone)]
pub struct TypeSig {
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
            modifier: FunctionModifier::Default,
            arity: Arity::N(0),
            args: vec![],
            err: vec![],
            traits: vec![],
        }
    }
}


impl TypeSig {
    fn value(t: TypeSig) -> Self {
        Self {
            modifier: FunctionModifier::Pure,
            arity: Arity::N(0),
            args: vec![],
            err: vec![],
            traits: vec![],
        }

    }

    fn void() -> Self {
        Self {
            modifier: FunctionModifier::Default,
            arity: Arity::N(0),
            args: vec![],
            err: vec![],
            traits: vec![],
        }
    }

    // A -> A
    fn single_pure(arg: TypeSig) -> Self {
        let ret = arg.clone();
        Self {
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

#[derive(Clone, Debug)]
pub struct RefTrait(pub Rc<RefCell<Trait>>);

impl std::ops::Deref for RefTrait {
    type Target = RefCell<Trait>;

    fn deref(&self) -> &Self::Target {
        &self.0.as_ref()
    }
}

