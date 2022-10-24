mod builder;

pub use builder::*;

use crate::typesystem::*;

use itertools::Itertools;
use std::cell::RefCell;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

pub type Environment = crate::env::EnvLayers<String, AstNode>;

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Bool(bool),
    Int(u64),
    Float(f64),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstNodeInner {
    pub ty: Type,
    pub value: Ast,
}

impl AstNodeInner {
    pub fn new(value: Ast, ty: Type) -> Self {
        Self { value, ty }
    }
}

#[derive(Clone, PartialEq)]
pub struct AstNode(Rc<RefCell<AstNodeInner>>);

impl AstNode {
    fn new(value: Ast, ty: Type) -> Self {
        AstNodeInner::new(value, ty).into()
    }
}

impl From<AstNodeInner> for AstNode {
    fn from(item: AstNodeInner) -> Self {
        Self(Rc::new(RefCell::new(item)))
    }
}

impl Deref for AstNode {
    type Target = RefCell<AstNodeInner>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl crate::env::LayerValue for AstNode {}

impl fmt::Debug for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let inner = self.borrow();
        match &inner.value {
            Ast::Literal(x) => {
                write!(f, "{:?}", &x)?;
            }
            Ast::Variable(x) => {
                write!(f, "Var({:?})", &x)?;
            }
            _ => {
                write!(f, "{:?}:{:?}", &inner.value, &inner.ty);
            }
        }
        Ok(())
    }
}

impl fmt::Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let inner = self.borrow();
        match &inner.value {
            Ast::Block(exprs) => {
                write!(f, "Block({:?}, [", &inner.ty)?;
                for expr in exprs {
                    write!(f, "{:?}", expr)?;
                }
                write!(f, "]")?;
            }
            Ast::Variable(v) => match &v.bound {
                Some(bound) => {
                    write!(f, "({}:{:?})", &v.name, &bound)?;
                }
                None => {
                    write!(f, "({})", &v.name)?;
                }
            },
            Ast::Literal(x) => {
                write!(f, "{:?}", &x)?;
            }
            Ast::Apply(func, params) => {
                let s = vec![func]
                    .iter()
                    .map(|p| format!("{}", p))
                    .chain(params.iter().map(|p| format!("{:?}", p)))
                    .format(",")
                    .to_string();

                write!(f, "({})", s)?;
            }
            Ast::Extern(_exprs) => {
                write!(f, "Extern({:?})", &inner.ty)?;
            }
            Ast::Declare(name, rhs) => {
                write!(f, "Declare({}={}):{:?}", &name, &rhs, &inner.ty)?;
            }
            _ => {
                write!(f, "{:?}", &self)?;
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Variable {
    pub name: String,
    pub bound: Option<AstNode>,
}

impl Variable {
    pub fn new(name: String) -> Self {
        Self { name, bound: None }
    }
    pub fn bind(&mut self, ast: AstNode) {
        self.bound.replace(ast);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Ast {
    Type(Type),

    // Const value
    Literal(Literal),

    // Variable, represented by a String
    Variable(Variable),

    Block(Vec<AstNode>),

    // A function that is defined externally
    Extern(Vec<AstNode>), // args

    // A function that is defined as part of the program
    Function(Box<AstNode>, Vec<AstNode>), // body, arg names

    // function application
    Apply(Box<AstNode>, Vec<AstNode>), // function, parameters

    Assign(String, Box<AstNode>),

    Declare(String, Box<AstNode>),

    /// Error value
    Error(String),
}

impl From<Variable> for Ast {
    fn from(item: Variable) -> Self {
        Ast::Variable(item)
    }
}

impl From<Literal> for Ast {
    fn from(item: Literal) -> Self {
        Ast::Literal(item)
    }
}

pub fn make_binary_function(name: String, args: Vec<Type>, env: &mut Environment) {
    assert_eq!(args.len(), 3);
    let left_ty = args.get(0).unwrap().clone();
    let right_ty = args.get(1).unwrap().clone();
    let ret_ty = args.get(2).unwrap().clone();

    let left = AstNodeInner {
        value: Variable::new("left".into()).into(),
        ty: left_ty.clone(),
    };

    let right = AstNodeInner {
        value: Variable::new("right".into()).into(),
        ty: right_ty.clone(),
    };

    let ret = AstNodeInner {
        value: Variable::new("ret".into()).into(),
        ty: ret_ty.clone(),
    };

    let args = vec![left.into(), right.into(), ret.into()];

    let node = AstNodeInner {
        value: Ast::Extern(args),
        ty: Type::Func(vec![left_ty, right_ty, ret_ty]),
    };

    env.define(name, node.into());
}

pub fn base_env() -> Environment {
    let mut env = Environment::default();
    make_binary_function("*".into(), vec![Type::Int, Type::Int, Type::Int], &mut env);
    make_binary_function(
        "*".into(),
        vec![Type::Float, Type::Float, Type::Float],
        &mut env,
    );
    make_binary_function(
        "*".into(),
        vec![Type::Int, Type::Float, Type::Float],
        &mut env,
    );
    make_binary_function(
        "*".into(),
        vec![Type::Float, Type::Int, Type::Float],
        &mut env,
    );

    make_binary_function("+".into(), vec![Type::Int, Type::Int, Type::Int], &mut env);
    make_binary_function(
        "+".into(),
        vec![Type::Float, Type::Float, Type::Float],
        &mut env,
    );
    make_binary_function(
        "+".into(),
        vec![Type::Int, Type::Float, Type::Float],
        &mut env,
    );
    make_binary_function(
        "+".into(),
        vec![Type::Float, Type::Int, Type::Float],
        &mut env,
    );

    make_binary_function("-".into(), vec![Type::Int, Type::Int, Type::Int], &mut env);
    make_binary_function(
        "-".into(),
        vec![Type::Float, Type::Float, Type::Float],
        &mut env,
    );
    make_binary_function(
        "-".into(),
        vec![Type::Int, Type::Float, Type::Float],
        &mut env,
    );
    make_binary_function(
        "-".into(),
        vec![Type::Float, Type::Int, Type::Float],
        &mut env,
    );

    make_binary_function("^".into(), vec![Type::Int, Type::Int, Type::Int], &mut env);
    make_binary_function(
        "^".into(),
        vec![Type::Float, Type::Int, Type::Float],
        &mut env,
    );
    make_binary_function(">".into(), vec![Type::Int, Type::Int, Type::Bool], &mut env);
    make_binary_function(
        ">".into(),
        vec![Type::Float, Type::Float, Type::Bool],
        &mut env,
    );
    env
}

#[cfg(test)]
mod tests {
    use super::*;
    use log::debug;
    use test_log::test;
}
