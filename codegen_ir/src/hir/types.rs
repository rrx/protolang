use crate::util::fmap;
use serde::Serialize;
use std::fmt::{self, Display};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub enum FloatKind {
    F32,
    F64,
}

impl Display for FloatKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatKind::F32 => write!(f, "f32"),
            FloatKind::F64 => write!(f, "f64"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
pub enum IntegerKind {
    I8,
    I16,
    I32,
    I64,
    Isz,
    U8,
    U16,
    U32,
    U64,
    Usz,
}
impl IntegerKind {
    /// Returns whether this type is unsigned (and therefore whether it should be sign-extended).
    ///
    /// Will bind the integer to an i32 if this integer is an IntegerKind::Inferred
    /// that has not already been bound to a concrete type.
    pub fn is_unsigned_integer(&self) -> bool {
        use IntegerKind::*;
        match self {
            I8 | I16 | I32 | I64 | Isz => false,
            U8 | U16 | U32 | U64 | Usz => true,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize)]
pub enum PrimitiveType {
    Integer(IntegerKind),
    Float(FloatKind),
    Char,
    Boolean,
    Unit,
    Pointer, // An opaque pointer type
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize)]
pub struct FunctionType {
    pub parameters: Vec<Type>,
    pub return_type: Box<Type>,
    pub is_varargs: bool,
    pub export: bool,
}
impl FunctionType {
    pub fn internal(mut args: Vec<Type>) -> Self {
        let ret = args.split_off(args.len() - 1).pop().unwrap();
        Self {
            parameters: args,
            return_type: ret.into(),
            is_varargs: false,
            export: false,
        }
    }
    pub fn export(mut args: Vec<Type>) -> Self {
        let ret = args.split_off(args.len() - 1).pop().unwrap();
        Self {
            parameters: args,
            return_type: ret.into(),
            is_varargs: false,
            export: true,
        }
    }
}

/// A HIR type representation.
/// Removes all references to generics and user-defined types.
/// Union variants are also absent, being represented by a struct
/// value and a cast to a different struct type of the largest variant.
#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd, Hash, Serialize)]
pub enum Type {
    Primitive(PrimitiveType),
    Function(FunctionType),

    /// Tuples have a TypeId to allow for struct recursion
    Tuple(Vec<Type>),
}

impl From<FunctionType> for Type {
    fn from(item: FunctionType) -> Self {
        Self::Function(item)
    }
}

impl Type {
    pub fn i64() -> Self {
        Self::Primitive(PrimitiveType::Integer(IntegerKind::I64))
    }

    pub fn f64() -> Self {
        Self::Primitive(PrimitiveType::Float(FloatKind::F64))
    }

    pub fn bool() -> Self {
        Self::Primitive(PrimitiveType::Boolean)
    }

    pub fn unit() -> Self {
        Self::Primitive(PrimitiveType::Unit)
    }

    pub fn into_function(self) -> Option<FunctionType> {
        match self {
            Type::Function(f) => Some(f),
            _ => None,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Primitive(p) => match p {
                PrimitiveType::Integer(kind) => kind.fmt(f),
                PrimitiveType::Float(kind) => kind.fmt(f),
                PrimitiveType::Char => write!(f, "char"),
                PrimitiveType::Boolean => write!(f, "bool"),
                PrimitiveType::Unit => write!(f, "unit"),
                PrimitiveType::Pointer => write!(f, "ptr"),
            },
            Type::Function(function) => write!(f, "({})", function),
            Type::Tuple(elems) => {
                let elems = fmap(elems, ToString::to_string);
                write!(f, "{{{}}}", elems.join(", "))
            }
        }
    }
}

impl std::fmt::Display for FunctionType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for param in &self.parameters {
            write!(f, "{} -> ", param)?;
        }
        if self.is_varargs {
            write!(f, "... -> ")?;
        }
        write!(f, "{}", self.return_type)
    }
}

impl std::fmt::Display for IntegerKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntegerKind::I8 => write!(f, "i8"),
            IntegerKind::I16 => write!(f, "i16"),
            IntegerKind::I32 => write!(f, "i32"),
            IntegerKind::I64 => write!(f, "i64"),
            IntegerKind::Isz => write!(f, "isz"),
            IntegerKind::U8 => write!(f, "u8"),
            IntegerKind::U16 => write!(f, "u16"),
            IntegerKind::U32 => write!(f, "u32"),
            IntegerKind::U64 => write!(f, "u64"),
            IntegerKind::Usz => write!(f, "usz"),
        }
    }
}
