use super::*;
use codegen::hir;
use std::error::Error;
use std::fmt;
use serde::Serialize;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Hash)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Func(Vec<Type>),
    Variable(DefinitionId),
    Unit,
    Type, // the type of a type
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

impl logic::UnifyType<DefinitionId> for Type {
    fn try_unknown(&self) -> Option<DefinitionId> {
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
}

impl Type {
    pub fn lower(&self, subst: &SymbolTable) -> Result<hir::Type, Box<dyn Error>> {
        match self {
            Self::Func(sig) => {
                Ok(hir::FunctionType::export(Self::lower_list(sig, subst)?).into())
            }
            Self::Variable(def) => match subst.get(def) {
                Some(v) => v.try_type().unwrap().lower(subst),
                None => {
                    eprintln!("Not found in subsitution: {:?}", def);
                    unreachable!()
                }
            },
            Self::Int => Ok(hir::Type::i64()),
            Self::Float => Ok(hir::Type::f64()),
            Self::Bool => Ok(hir::Type::bool()),
            Self::Unit => Ok(hir::Type::unit()),

            // types can't be lowered
            Self::Type => unimplemented!(),

            // codegen doesn't handle strings, we need to implement them at this layer
            Self::String => unimplemented!(),
        }
    }

    pub fn lower_list(
        types: &Vec<Type>,
        subst: &SymbolTable,
    ) -> Result<Vec<hir::Type>, Box<dyn Error>> {
        let mut out = vec![];
        for t in types {
            out.push(t.lower(&subst)?);
        }
        Ok(out)
    }

    pub fn resolve_list(types: &Vec<Type>, subst: &SymbolTable) -> Vec<Type> {
        types
            .clone()
            .into_iter()
            .map(|ty| ty.resolve(subst))
            .collect()
    }

    pub fn resolve(&self, subst: &SymbolTable) -> Type {
        match self {
            Type::Variable(v) => {
                match subst.get(&v) {
                    Some(v) => v.try_type().unwrap().clone(),
                    None => self.clone(),
                }
            },
            Type::Func(sig) => {
                let resolved_sig = sig.iter().map(|t| t.resolve(subst)).collect::<Vec<_>>();
                Type::Func(resolved_sig)
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
