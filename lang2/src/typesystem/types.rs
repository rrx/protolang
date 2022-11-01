use crate::ast::AstNode;
use crate::env::{EnvLayers, LayerKey};
use logic::{DefinitionId, TypeSignature};
use std::fmt;

pub type Environment<N> = EnvLayers<str, N>;
impl LayerKey for String {}

#[derive(Default)]
pub struct TypeSystemContext {
    next_id: usize,
}

impl TypeSystemContext {
    pub fn next_id(&mut self) -> DefinitionId {
        let id = DefinitionId(self.next_id);
        self.next_id += 1;
        id
    }
}

#[derive(Clone)]
pub struct TypeNodePair {
    pub ty: Type,
    pub node: AstNode,
}
impl TypeNodePair {
    pub fn new(ty: Type, node: AstNode) -> Self {
        Self { ty, node }
    }
}
impl fmt::Debug for TypeNodePair {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", &self.ty)
    }
}
impl PartialEq for TypeNodePair {
    fn eq(&self, other: &Self) -> bool {
        self.ty == other.ty
    }
}
impl From<AstNode> for TypeNodePair {
    fn from(node: AstNode) -> Self {
        let ty = node.borrow().ty.clone();
        Self { ty, node }
    }
}

impl TypeSignature<TypeNodePair> for TypeNodePair {
    fn unknown(&self) -> Option<DefinitionId> {
        self.ty.unknown()
    }
    fn children(&self) -> Vec<TypeNodePair> {
        self.ty
            .children()
            .into_iter()
            .map(|s| TypeNodePair::new(s, self.node.clone()))
            .collect()
    }
    fn var(_u: DefinitionId) -> TypeNodePair {
        unimplemented!()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Var(DefinitionId),
    Unit,
    Error,
    Func(FunctionSig),
    Type(String),
}

impl Type {
    pub fn var(s: DefinitionId) -> Self {
        Self::Var(s)
    }

    pub fn is_unknown(&self) -> bool {
        match self {
            Type::Var(_) => true,
            _ => false,
        }
    }

    pub fn is_unknown_recursive(&self) -> bool {
        match self {
            Type::Var(_) => true,
            Type::Func(sig) => sig.iter().any(|v| v.is_unknown()),
            _ => false,
        }
    }

    pub fn unknown_ids(&self) -> Vec<DefinitionId> {
        match self {
            Type::Var(id) => vec![*id],
            Type::Func(sig) => sig.iter().map(|v| v.unknown_ids()).flatten().collect(),
            _ => vec![],
        }
    }
}

impl TypeSignature<Type> for Type {
    fn unknown(&self) -> Option<DefinitionId> {
        match self {
            Type::Var(id) => Some(*id),
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
        Type::Var(u)
    }
}

type FunctionSig = Vec<Type>;
