use super::types::*;

#[derive(Clone, Debug)]
pub enum TypedValue {
    Bool(bool),
    Int(i64),
    Float(f64),
    Any(Box<TypedValue>),
    Agg(usize, Box<TypedValue>),
    Composite(Vec<TypedValue>),
    Type(TypeSpecValue),
    //Pointer(String),
    Function(Vec<TypedValue>),
    //Eval(String),
    Void,
}
impl TypedValue {
    pub fn new_extern(params: Vec<TypedValue>) -> Self {
        Self::Function(params)
    }

    pub fn new_agg(len: usize, ty: TypedValue) -> Self {
        Self::Agg(len, Box::new(ty))
    }

    pub fn infer_type(&self) -> Type {
        match self {
            Self::Bool(_) => Type::TBool,
            Self::Int(_) => Type::TInt,
            Self::Float(_) => Type::TFloat,
            Self::Any(_) => Type::TAny,
            Self::Agg(_, _) => Type::TAggregate,
            Self::Composite(_) => Type::TComposite,
            Self::Type(_) => Type::TType,
            //Self::Pointer(_) => Type::TPointer,
            //Self::Eval(_) => Type::Eval,
            Self::Function(_) => Type::TFunction,
            Self::Void => Type::TVoid,
        }
    }

    pub fn try_bool(&self) -> Option<bool> {
        if let Self::Bool(u) = self {
            Some(*u)
        } else {
            None
        }
    }

    pub fn try_int(&self) -> Option<i64> {
        if let Self::Int(u) = self {
            Some(*u)
        } else {
            None
        }
    }

    pub fn try_float(&self) -> Option<f64> {
        if let Self::Float(u) = self {
            Some(*u)
        } else {
            None
        }
    }
}


