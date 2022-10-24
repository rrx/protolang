use crate::env::EnvLayers;
use std::fmt;

pub type Environment<N> = EnvLayers<str, N>;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeDefinitionId(usize);

impl fmt::Display for TypeDefinitionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Debug for TypeDefinitionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Default)]
pub struct TypeSystemContext {
    next_id: usize
}

impl TypeSystemContext {
    pub fn next_id(&mut self) -> TypeDefinitionId {
        let id = TypeDefinitionId(self.next_id);
        self.next_id += 1;
        id
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Float,
    Bool,
    String,
    Unknown(TypeDefinitionId),
    Unit,
    Error,
    Func(FunctionSig),
    Type(String),
}

impl Type {
    pub fn new_unknown(s: TypeDefinitionId) -> Self {
        Self::Unknown(s)
    }

    pub fn is_unknown(&self) -> bool {
        match self {
            Type::Unknown(_) => true,
            _ => false
        }
    }

    pub fn is_unknown_recursive(&self) -> bool {
        match self {
            Type::Unknown(_) => true,
            Type::Func(sig) => {
                sig.iter().any(|v| v.is_unknown())
            }
            _ => false
        }
    }
}

type FunctionSig = Vec<Type>;

