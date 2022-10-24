mod builder;

pub use builder::*;

use crate::typesystem::*;

use log::*;
use rpds::HashTrieMap;
use std::fmt;

pub type Environment = crate::env::EnvLayers<String, AstNode>;

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Bool(bool),
    Int(u64),
    Float(f64),
    String(String),
}

#[derive(Clone, PartialEq)]
pub struct AstNode {
    pub ty: Type,
    pub value: Ast,
}

impl AstNode {
    pub fn new(value: Ast, ty: Type, env: Environment) -> Self {
        Self {
            value,
            ty,
        }
    }


}

impl crate::env::LayerValue for AstNode {}

impl fmt::Debug for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}:{:?}", self.value, self.ty)
        //let mut out = f.debug_struct("Ast");
        //out.field("v", &self.value);
        //out.field("ty", &self.ty);
        //out.finish()
    }
}

impl fmt::Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.value {
            Ast::Block(exprs) => {
                write!(f, "Block({:?})", self.ty)?;
                for expr in exprs {
                    write!(f, "\t{:?}", expr)?;
                }
                Ok(())
            }
            Ast::Extern(exprs) => {
                write!(f, "Extern({:?})", self.ty)?;
                Ok(())
            }
            _ => write!(f, "{:?}", &self),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Ast {
    Type(Type),

    // Const value
    Literal(Literal),

    // Variable, represented by a String
    Ident(String),

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

pub fn make_binary_function(name: String, args: Vec<Type>, mut env: Environment) -> Environment {
    assert_eq!(args.len(), 3);
    let left_ty = args.get(0).unwrap().clone();
    let right_ty = args.get(1).unwrap().clone();
    let ret_ty = args.get(2).unwrap().clone();

    let left = AstNode {
        value: Ast::Ident("left".into()),
        ty: left_ty.clone(),
    };

    let right = AstNode {
        value: Ast::Ident("right".into()),
        ty: right_ty.clone(),
    };

    let ret = AstNode {
        value: Ast::Ident("ret".into()),
        ty: ret_ty.clone(),
    };

    let args = vec![left, right, ret];

    let mut node = AstNode {
        value: Ast::Extern(args),
        ty: Type::Func(vec![left_ty, right_ty, ret_ty]),
    };

    env.define(name, node.clone());
    env
}

pub fn base_env() -> Environment {
    let mut env = Environment::default();
    env = make_binary_function("*".into(), vec![Type::Int, Type::Int, Type::Int], env);
    env = make_binary_function("*".into(), vec![Type::Float, Type::Float, Type::Float], env);
    env = make_binary_function("*".into(), vec![Type::Int, Type::Float, Type::Float], env);
    env = make_binary_function("*".into(), vec![Type::Float, Type::Int, Type::Float], env);

    env = make_binary_function("+".into(), vec![Type::Int, Type::Int, Type::Int], env);
    env = make_binary_function("+".into(), vec![Type::Float, Type::Float, Type::Float], env);
    env = make_binary_function("+".into(), vec![Type::Int, Type::Float, Type::Float], env);
    env = make_binary_function("+".into(), vec![Type::Float, Type::Int, Type::Float], env);

    env = make_binary_function("-".into(), vec![Type::Int, Type::Int, Type::Int], env);
    env = make_binary_function("-".into(), vec![Type::Float, Type::Float, Type::Float], env);
    env = make_binary_function("-".into(), vec![Type::Int, Type::Float, Type::Float], env);
    env = make_binary_function("-".into(), vec![Type::Float, Type::Int, Type::Float], env);

    env = make_binary_function("^".into(), vec![Type::Int, Type::Int, Type::Int], env);
    env = make_binary_function("^".into(), vec![Type::Float, Type::Int, Type::Float], env);
    env = make_binary_function(">".into(), vec![Type::Int, Type::Int, Type::Bool], env);
    env = make_binary_function(">".into(), vec![Type::Float, Type::Float, Type::Bool], env);
    env
}

#[cfg(test)]
mod tests {
    use super::*;
    use log::debug;
    use test_log::test;
}

