use crate::ast::*;
use crate::lexer::Location;
use crate::results::*;
use log::*;
use rpds::HashTrieMap;
use std::fmt;

mod check;
pub use check::*;

mod types;
pub use types::*;

pub type Environment = crate::compiler::env::EnvLayers<String, IR>;

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Bool(bool),
    Int(u64),
    Float(f64),
    String(String),
}

#[derive(Clone, PartialEq)]
pub struct IR {
    pub ty: Type,
    pub value: IRValue,
    pub loc: Location,
    pub env: Environment,
}

impl fmt::Debug for IR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = f.debug_struct("IR");
        out.field("v", &self.value);
        out.field("ty", &self.ty);
        out.finish()
    }
}

impl fmt::Display for IR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.value {
            IRValue::Block(exprs) => {
                write!(f, "Block({})", self.ty)?;
                for expr in exprs {
                    write!(f, "\t{}", expr)?;
                }
                Ok(())
            }
            _ => write!(f, "{:?}", &self),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum IRValue {
    Type(Type),

    // Const value
    Literal(Literal),

    // Variable, represented by a String
    Ident(String),

    Block(Vec<IR>),

    // A function that is defined externally
    Extern(Vec<IR>), // args

    // A function that is defined as part of the program
    Function(Box<IR>, Vec<IR>), // body, arg names

    // function application
    Apply(Box<IR>, Vec<IR>), // function, parameters

    Assign(String, Box<IR>),

    Declare(String, Box<IR>),

    /// Error value
    Error(String),
}

impl crate::compiler::env::LayerValue for IR {}

impl IR {
    pub fn new(v: IRValue, ty: Type, env: Environment) -> Self {
        Self::new_with_location(v, ty, Location::default(), env)
    }

    pub fn new_with_location(v: IRValue, ty: Type, loc: Location, env: Environment) -> Self {
        Self {
            value: v,
            ty,
            loc,
            env,
        }
    }
}

pub fn make_binary_function(name: String, args: Vec<Type>, mut env: Environment) -> IR {
    assert_eq!(args.len(), 3);
    let left_ty = args.get(0).unwrap().clone();
    let right_ty = args.get(1).unwrap().clone();
    let ret_ty = args.get(2).unwrap().clone();

    let left = IR::new(IRValue::Ident("left".into()), left_ty.clone(), env.clone());

    let right = IR::new(
        IRValue::Ident("right".into()),
        right_ty.clone(),
        env.clone(),
    );

    let ret = IR::new(IRValue::Ident("ret".into()), ret_ty.clone(), env.clone());

    let args = vec![left, right, ret];

    let mut node = IR::new(
        IRValue::Extern(args),
        Type::Func(vec![left_ty, right_ty, ret_ty]),
        env.clone(),
    );

    env.define(name, node.clone());
    node.env = env;
    node
}

pub fn base_env() -> Environment {
    let mut env = Environment::default();
    let node = make_binary_function("*".into(), vec![Type::Int, Type::Int, Type::Int], env);
    env = node.env;
    let node = make_binary_function("*".into(), vec![Type::Float, Type::Float, Type::Float], env);
    env = node.env;
    let node = make_binary_function("*".into(), vec![Type::Int, Type::Float, Type::Float], env);
    env = node.env;
    let node = make_binary_function("*".into(), vec![Type::Float, Type::Int, Type::Float], env);
    env = node.env;

    let node = make_binary_function("+".into(), vec![Type::Int, Type::Int, Type::Int], env);
    env = node.env;
    let node = make_binary_function("+".into(), vec![Type::Float, Type::Float, Type::Float], env);
    env = node.env;
    let node = make_binary_function("+".into(), vec![Type::Int, Type::Float, Type::Float], env);
    env = node.env;
    let node = make_binary_function("+".into(), vec![Type::Float, Type::Int, Type::Float], env);
    env = node.env;

    let node = make_binary_function("-".into(), vec![Type::Int, Type::Int, Type::Int], env);
    env = node.env;
    let node = make_binary_function("-".into(), vec![Type::Float, Type::Float, Type::Float], env);
    env = node.env;
    let node = make_binary_function("-".into(), vec![Type::Int, Type::Float, Type::Float], env);
    env = node.env;
    let node = make_binary_function("-".into(), vec![Type::Float, Type::Int, Type::Float], env);
    env = node.env;

    let node = make_binary_function("^".into(), vec![Type::Int, Type::Int, Type::Int], env);
    env = node.env;
    let node = make_binary_function("^".into(), vec![Type::Float, Type::Int, Type::Float], env);
    env = node.env;
    let node = make_binary_function(">".into(), vec![Type::Int, Type::Int, Type::Bool], env);
    env = node.env;
    let node = make_binary_function(">".into(), vec![Type::Float, Type::Float, Type::Bool], env);
    env = node.env;
    env
}

pub fn op_name(op: &Operator) -> String {
    match op {
        Operator::Plus => "+".into(),
        Operator::Minus => "-".into(),
        Operator::Exp => "^".into(),
        Operator::Multiply => "*".into(),
        Operator::Divide => "/".into(),
        Operator::GreaterThan => ">".into(),
        Operator::GreaterThanEqual => ">=".into(),
        Operator::LessThan => "<".into(),
        Operator::LessThanEqual => "<=".into(),
        Operator::Equal => "==".into(),
        _ => {
            debug!("Not implemented: {:?}", op);
            unimplemented!()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::check::SymbolTable;
    use super::*;
    use log::debug;
    use test_log::test;

    #[test]
    fn analyze() {
        let env = base_env();
        let mut c = TypeChecker::default();
        let s = SymbolTable::default();
        /*
        let f = \\x -> { x^2; };
        y = 2
        let mut x = 1.
        let z = f(2)";
                */
        let p = "
let x = 1
1+1
1-1
y = 1
x + 2
        ";
        let ir = c.parse_str(p, env).unwrap();
        debug!("{}", p);
        debug!("{}", ir);
        debug!("{:?}", s);
        for e in &c.type_equations {
            debug!("E: {}", e);
        }
        c.print();
    }

    #[test]
    fn func_match() {
        let c = TypeChecker::default();
        let s = SymbolTable::default();
        let out = c.unify(
            &Type::Func(vec![Type::Int, Type::Unknown(0)]),
            &vec![
                Type::Func(vec![Type::Float, Type::Float]),
                Type::Func(vec![Type::Int, Type::Int]),
            ],
            Some(s),
        );
        debug!("{:?}", out.as_ref().unwrap().iter().collect::<Vec<_>>());
        assert_eq!(out.unwrap().get(&0), Some(&Type::Int));
    }

    #[test]
    fn func_mismatch() {
        let c = TypeChecker::default();
        let s = SymbolTable::default();
        let out = c.unify(
            &Type::Func(vec![Type::Float, Type::Unknown(0)]),
            &vec![Type::Func(vec![Type::Int, Type::Int])],
            Some(s),
        );
        assert_eq!(out, None)
    }

    #[test]
    fn types_match() {
        let c = TypeChecker::default();
        let s = SymbolTable::default();
        let out = c.unify(&Type::Int, &vec![Type::Float, Type::Int], Some(s.clone()));
        assert_eq!(out, Some(s));
    }

    #[test]
    fn types_mismatch() {
        let c = TypeChecker::default();
        let s = SymbolTable::default();
        let out = c.unify(&Type::Int, &vec![Type::Float], Some(s));
        assert_eq!(out, None);
    }

    #[test]
    fn types_unknown() {
        let c = TypeChecker::default();
        let s = SymbolTable::default();
        let out = c.unify(&Type::Unknown(0), &vec![Type::Float], Some(s));
        assert_eq!(out.unwrap().get(&0), Some(&Type::Float));

        let s = SymbolTable::default();
        let out = c.unify(&Type::Unknown(0), &vec![Type::Float, Type::Int], Some(s));
        assert_eq!(out.unwrap().get(&0), Some(&Type::Float));

        let s = SymbolTable::default();
        let out = c
            .unify(&Type::Unknown(0), &vec![Type::Unknown(1)], Some(s))
            .unwrap();
        //assert_eq!(out.get("y".into()), Some(&Type::Float));
        assert_eq!(out.get(&0), Some(&Type::Unknown(1)));

        // no match
        let s = SymbolTable::default();
        let out = c.unify(
            &Type::Func(vec![Type::Int, Type::Float, Type::Unknown(0)]),
            &vec![
                Type::Func(vec![Type::Float, Type::Float, Type::Float]),
                Type::Func(vec![Type::Int, Type::Int, Type::Int]),
            ],
            Some(s),
        );
        assert_eq!(out, None);
    }
}
