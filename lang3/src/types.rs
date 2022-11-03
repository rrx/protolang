use crate::env::{EnvLayers, LayerKey};
use logic::{DefinitionId, TypeSignature, SymbolTable};
use std::fmt;
use std::error::Error;
use crate::Ast;

use codegen::hir;

pub type Environment<T> = EnvLayers<String, Ast<T>>;
impl LayerKey for String {}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Func(Vec<Type>),
    Variable(DefinitionId),
    Unit,
}

impl Default for Type {
    fn default() -> Self {
        Self::Unit
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", &self)
    }
}

impl TypeSignature<Type> for Type {
    fn unknown(&self) -> Option<DefinitionId> {
        match self {
            Type::Variable(id) => Some(*id),
            _ => None,
        }
    }
    fn children(&self) -> Vec<Type> {
        match self {
            Type::Func(args) => args.clone(),
            _ => vec![],
        }
    }
    fn var(u: DefinitionId) -> Type {
        Type::Variable(u)
    }
}

impl Type {
    pub fn lower(&self, subst: &SymbolTable<Type>) -> Result<hir::Type, Box<dyn Error>> {
        match self {
            Self::Func(sig) => {
                let mut out = vec![];
                for ty in sig {
                    out.push(ty.lower(subst)?);
                }
                Ok(hir::FunctionType::export(out).into())
            }
            Self::Variable(def) => {
                subst.get(def).expect("Variable not found").lower(subst)
            }
            Self::Int => Ok(hir::Type::i64()),
            Self::Float => Ok(hir::Type::f64()),
            Self::Bool => Ok(hir::Type::bool()),
            Self::Unit => Ok(hir::Type::unit())
        }
    }

    pub fn resolve(&self, subst: SymbolTable<Type>) -> (bool, Type) {
        match self {
            Type::Variable(v) => {
                match subst.get(&v) {
                    Some(ty) => (ty.unknown().is_none(), ty.clone()),
                    None => (false, self.clone()),
                }
            }
            Type::Func(sig) => {
                let resolved_sig = sig.iter().map(|t| {
                    let (_, ty) = t.resolve(subst.clone());
                    ty
                }).collect::<Vec<_>>();
                let ty = Type::Func(resolved_sig);
                (ty.unknown().is_none(), ty)
            }
            _ => (true, self.clone())
        }
    }

    fn substitute(&mut self, subst: &SymbolTable<Type>) -> Type {
        match self {
            Type::Variable(ty_id) => match logic::subst_get_type_by_id(&subst, &ty_id) {
                Some(v) => v,
                None => {
                    println!("Type missing from substitution table: {:?}", (ty_id));//, &self));
                    unimplemented!()
                }
            },
            Type::Func(sig) => {
                let new_sig = sig
                    .into_iter()
                    .map(|v| {
                        v.substitute(subst)
                    })
                    .collect();
                Type::Func(new_sig)
            }
            _ => self.clone(),
        }
    }
}

impl From<DefinitionId> for Type {
    fn from(item: DefinitionId) -> Self {
        Self::Variable(item)
    }
}


