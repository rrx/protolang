use std::collections::BTreeSet;
use std::fmt;
//use std::hash::Hash;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Trait {
    name: usize,
}

impl Trait {
    pub fn new(name: &str) -> Self {
        Self { name: 0 } //name.to_string() }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    //I32,
    //U32,
    //U64,
    //F32,
    //F64,
    Int,
    Float,
    Bool,
    String,
    Unknown(usize, BTreeSet<Trait>),
    Void,
    Error,
    Func(FunctionSig),
    Type(String),
}

type FunctionSig = Vec<Type>;
/*
struct TypeSig(Vec<Type>);

impl TypeSig {
    fn substitute(&self, subst: &SymbolTable) -> Self {
        Self(
            self.0
                .iter()
                .map(|v| {
                    if let Type::Unknown(name) = v {
                        if subst.contains_key(name) {
                            return subst.get(name).unwrap().clone();
                        }
                    }
                    v.clone()
                })
                .collect::<Vec<_>>(),
        )
    }
}
*/

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unknown(s, _) => write!(f, "T:{}", s),
            Self::Int => write!(f, "T:Int"),
            Self::Bool => write!(f, "T:Bool"),
            Self::String => write!(f, "T:String"),
            Self::Float => write!(f, "T:Float"),
            Self::Void => write!(f, "T:Void"),
            Self::Error => write!(f, "T:Error"),
            Self::Func(s) => write!(f, "T:Func<{:?}>", s),
            Self::Type(s) => write!(f, "T:Type<{}>", s),
        }
    }
}

impl Type {
    pub fn new_unknown(s: usize) -> Self {
        Self::Unknown(s, BTreeSet::default())
    }

    pub fn is_unknown(&self) -> bool {
        if let Type::Unknown(_, _) = self {
            true
        } else {
            false
        }
    }

    /*
    fn flatten(&self) -> TypeSig {
        match self {
            Type::Func(args) => TypeSig(args.clone()),
            _ => TypeSig(vec![self.clone()]),
        }
    }
    */
}
