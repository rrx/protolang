use crate::env::{EnvLayers, LayerKey, LayerValue};
use logic::{DefinitionId, TypeSignature, SymbolTable};
use std::fmt;
use super::*;
use codegen::hir;
use std::error::Error;

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Bool(bool),
    Int(u64),
    Float(f64),
    String(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Variable<T> {
    pub definition_id: DefinitionId,
    pub ty: Type,
    pub name: Option<String>,
    pub bound: Option<T>,
}

impl<T> Variable<T> {
    pub fn named(name: String, definition_id: DefinitionId, ty: Type) -> Self {
        Self { name: Some(name), bound: None, definition_id, ty }
    }
    pub fn unnamed(definition_id: DefinitionId, ty: Type) -> Self {
        Self { name: None, bound: None, definition_id, ty }
    }
    pub fn bind(&mut self, ast: T) {
        self.bound.replace(ast);
    }
}

#[derive(Clone, PartialEq)]
pub enum Ast<T> {
    Type(Type),

    // Const value
    Literal(Literal),

    // Variable, represented by a String
    Variable(Variable<T>),

    Block(Vec<Ast<T>>),

    // A function that is defined externally
    Extern(Vec<Type>), // args

    Builtin(String, Vec<Ast<T>>), // name, args

    // A function that is defined as part of the program
    Function { params: Vec<Variable<T>>, body: Box<Ast<T>>, ty: Vec<Type>},

    // function application
    Apply(Box<Ast<T>>, Vec<Ast<T>>), // function, parameters

    Assign(String, Box<Ast<T>>),

    Declare(String, Box<Ast<T>>),
}

impl<T: Clone> Ast<T> {
    pub fn resolve(&self, subst: SymbolTable<Type>) -> Self {
        match &self {
            Self::Variable(v) => {
                let mut v = v.clone();
                let (_, ty) = v.ty.resolve(subst);
                v.ty = ty;
                Self::Variable(v.clone())
            }
            Self::Function { params, body, ty } => {
                //let sig = ty.
                //let (_, new_ty) = ty.resolve(subst);
                //ty = new_ty;
                //Self::Function { params: params.clone(), body: body.clone(), ty }
                self.clone()
            }
            _ => self.clone() 
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            Self::Literal(Literal::Int(_)) => Type::Int,
            Self::Literal(Literal::Float(_)) => Type::Float,
            Self::Literal(Literal::Bool(_)) => Type::Bool,
            Self::Function { ty, .. } => Type::Func(ty.clone()),
            Self::Block(exprs) => exprs.last().expect("Empty Block").get_type(),
            Self::Apply(f, _) => f.get_type().children().last().expect("No Return Type").clone(),
            Self::Declare(_, expr) => expr.get_type(),
            Self::Assign(_, expr) => expr.get_type(),
            Self::Variable(v) => v.ty.clone(),
            _ => unimplemented!()
        }
    }
}


/*
impl<T> TypeSignature<Type> for Ast<T> {
    fn unknown(&self) -> Option<DefinitionId> {
        self.get_type().unknown()
    }
    fn children(&self) -> Vec<Type> {
        self.get_type().children()
    }
    fn var(u: DefinitionId) -> Type {
        unimplemented!()
    }
}
*/

impl<T> From<Variable<T>> for Ast<T> {
    fn from(item: Variable<T>) -> Self {
        Ast::Variable(item)
    }
}

impl<T> From<Literal> for Ast<T> {
    fn from(item: Literal) -> Self {
        Ast::Literal(item)
    }
}

impl<T: fmt::Debug> fmt::Debug for Ast<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ast::Literal(x) => {
                write!(f, "{:?}", &x)?;
            }
            Ast::Variable(x) => {
                write!(f, "Var({:?})", &x.definition_id)?;
            }
            Ast::Type(t) => {
                write!(f, "Type({:?})", &t)?;
            }

            Ast::Block(exprs) => {
                write!(f, "Block({:?})", &exprs)?;
            }

            Ast::Extern(types) => {
                write!(f, "Extern({:?})", &types)?;
            }

            Ast::Builtin(name, _) => write!(f, "Builtin({:?})", &name)?,

            Ast::Function { params, body, ty } => {
                write!(f, "Func({:?}, {:?}, {:?})", &params, &body, &ty)?;
            }

            Ast::Apply(func, args) => {
                write!(f, "Apply({:?}, {:?})", &func, &args)?;
            }

            Ast::Assign(name, expr) => {
                write!(f, "Assign({:?}, {:?})", &name, &expr)?;
            }

            Ast::Declare(name, expr) => {
                write!(f, "Declare({:?}, {:?})", &name, &expr)?;
            }
        }
        Ok(())
    }
}

impl<T: fmt::Debug> fmt::Display for Ast<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", &self)
    }
}

impl<T: fmt::Debug + fmt::Display + Clone> LayerValue for Ast<T> {}

impl<T: fmt::Debug + fmt::Display + Clone> Ast<T> {
    pub fn int(i: i64) -> Self {
        Self::Literal(Literal::Int(i as u64))
    }

    pub fn lower(&self, env: Environment<T>) -> Result<hir::Ast, Box<dyn Error>> {
        match self {
            Self::Literal(Literal::Bool(b)) => Ok(hir::Ast::bool(*b)),
            Self::Literal(Literal::Int(u)) => Ok(hir::Ast::i64(*u as i64)),
            Self::Literal(Literal::Float(f)) => Ok(hir::Ast::f64(*f)),
            Self::Variable(v) => Ok(hir::DefinitionId(v.definition_id.0).to_variable()),
            _ => unimplemented!()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;

    #[test]
    fn lower() {
    }
}


