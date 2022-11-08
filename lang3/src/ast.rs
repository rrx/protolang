use super::*;
use crate::env::LayerValue;
use logic::UnifyType;
use ron::ser::{to_string_pretty, PrettyConfig};
use serde::{
    ser::{SerializeStruct, Serializer},
    Serialize,
};
use std::error::Error;
use std::fmt;

//#[derive(Clone, Debug, PartialEq, Serialize)]
//pub struct VariableId(pub usize);
pub type VariableId = DefinitionId;

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum Literal {
    Bool(bool),
    Int(u64),
    Float(f64),
    String(String),
}

#[derive(Clone, Debug)]
pub struct Variable {
    pub id: VariableId,
    pub ty: Type,
    pub name: String,
}
impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Serialize for Variable {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("Variable", 3)?;
        state.serialize_field("id", &self.id)?;
        state.serialize_field("ty", &self.ty)?;
        state.serialize_field("name", &self.name)?;
        state.end()
    }
}

impl Variable {
    pub fn named(name: String, id: VariableId, ty: Type) -> Self {
        Self { name, id, ty }
    }
    pub fn unnamed(id: VariableId, ty: Type) -> Self {
        let name = format!("v{}", id.0);
        Self { name, id, ty }
    }

    /// Just replace the type with the value in the symbol table
    pub fn resolve_type(&self, subst: &SymbolTable) -> Self {
        let mut v = self.clone();
        v.ty = v.ty.resolve(subst);
        v
    }

    /// resolve a variable
    /// If the variable resolves to another variable, then we are referencing and unbound function
    /// parameter.  If the variable resolves to a declaration, then we replace with the variable in
    /// the declaration.  We might want to implement this differently, so it's more clear what's
    /// stored in the environment.  We want to lookup a variable and check it it exists in the
    /// current environment.  If it exists, then it can be unbound (function parameter), or bound
    /// (regular variable declaration).
    pub fn resolve(&self, subst: &SymbolTable) -> Self {
        let var = match subst.get(&self.id) {
            Some(v) => {
                match v {
                    // declared variables
                    Ast::Declare(var, _) => {
                        // just return the variable
                        var.clone().resolve_type(subst)
                    }

                    // parameters in functions
                    Ast::Variable(var) => var.clone().resolve_type(subst),
                    Ast::Parameter(var) => var.clone().resolve_type(subst),
                    _ => unreachable!("unable to resolve this type: {:?}", v),
                }
            }

            // if it's not found, then it's probably a parameter
            None => {
                log::debug!("unable to resolve parameter: {:?}", &self);
                self.clone()
            }
        };
        var.resolve_type(subst)
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "V{}", &self.id.0)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Builtin {
    // This implementation is awkward and doesn't work very well.
    AddInt(Box<Ast>, Box<Ast>),
    SubInt(Box<Ast>, Box<Ast>),
    EqInt(Box<Ast>, Box<Ast>),
    AddFloat(Box<Ast>, Box<Ast>),
}
impl Builtin {
    pub fn children(&self) -> Vec<&Ast> {
        match self {
            Self::AddInt(a, b) => vec![a, b],
            Self::SubInt(a, b) => vec![a, b],
            Self::EqInt(a, b) => vec![a, b],
            Self::AddFloat(a, b) => vec![a, b],
            //_ => unimplemented!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum Ast {
    Type(Type),

    // Const value
    Literal(Literal),

    // Variable, represented by a String
    Variable(Variable),

    // A parameter is unbound
    Parameter(Variable),

    // A block is a lexical scope
    Block(Vec<Ast>),

    // A sequence is not a lexical scope
    Sequence(Vec<Ast>),

    // A function that is defined externally
    Extern(Vec<Type>), // args

    // similar to extern, but it get's handled internally
    Builtin(Vec<Type>), // args

    Internal(Builtin),

    // A function that is defined as part of the program
    Function {
        params: Vec<Variable>,
        body: Box<Ast>,
        ty: Type,
    },

    // function application
    Apply(Variable, Vec<Ast>), // function, args

    Assign(Variable, Box<Ast>),

    Declare(Variable, Box<Ast>),

    Return(Box<Ast>),

    Condition(Box<Ast>, Box<Ast>, Option<Box<Ast>>),
}

impl LayerValue for Ast {}

fn resolve_list(exprs: &Vec<Ast>, subst: &SymbolTable) -> Result<Vec<Ast>, Box<dyn Error>> {
    let mut out = vec![];
    for expr in exprs {
        let ast = expr.resolve(subst)?;
        out.push(ast);
    }
    Ok(out)
}

impl Ast {
    pub fn to_ron(&self) -> Result<String, ron::Error> {
        let pretty = PrettyConfig::new()
            //.depth_limit(3)
            .compact_arrays(true);
        to_string_pretty(&self, pretty)
    }

    pub fn block(exprs: Vec<Self>) -> Self {
        Self::Block(exprs)
    }

    pub fn seq(exprs: Vec<Self>) -> Self {
        Self::Sequence(exprs)
    }

    pub fn try_int(&self) -> Option<u64> {
        if let Self::Literal(Literal::Int(u)) = self {
            Some(*u)
        } else {
            None
        }
    }

    pub fn int(i: i64) -> Self {
        Self::Literal(Literal::Int(i as u64))
    }

    pub fn float(f: f64) -> Self {
        Self::Literal(Literal::Float(f))
    }

    pub fn string(s: String) -> Self {
        Self::Literal(Literal::String(s))
    }

    pub fn replace_variable(&self, subst: &SymbolTable) -> Self {
        match &self {
            Self::Variable(v) => Ast::Variable(v.clone().resolve(subst)),
            _ => unreachable!(),
        }
    }

    pub fn resolve(&self, subst: &SymbolTable) -> Result<Self, Box<dyn Error>> {
        let before = self.clone();
        let after = match &self {
            Self::Literal(_) => self.clone(),
            Self::Variable(_) => self.replace_variable(subst),
            Self::Parameter(_) => self.clone(),
            Self::Extern(types) => Self::Extern(Type::resolve_list(&types, subst)),
            Self::Builtin(types) => Self::Builtin(Type::resolve_list(&types, subst)),
            Self::Internal(v) => self.clone(),
            Self::Declare(var, expr) => {
                Self::Declare(var.resolve_type(subst), expr.resolve(subst)?.into())
            }
            Self::Assign(name, expr) => Self::Assign(name.clone(), expr.resolve(subst)?.into()),
            Self::Function { params, body, ty } => {
                // resolve the types in the params
                let params = params
                    .iter()
                    .map(|p| p.resolve_type(subst))
                    .collect::<Vec<_>>();
                let ty = ty.resolve(&subst);
                Self::Function {
                    params,
                    body: body.resolve(subst)?.into(),
                    ty,
                }
            }
            Self::Block(exprs) => Self::block(resolve_list(exprs, &subst)?),
            Self::Sequence(exprs) => Self::seq(resolve_list(exprs, &subst)?),
            Self::Apply(f, args) => {
                Self::Apply(f.resolve(subst).into(), resolve_list(args, subst)?)
            }

            Self::Type(ty) => Self::Type(ty.resolve(subst)),
            Self::Return(expr) => Self::Return(expr.resolve(subst)?.into()),
            Self::Condition(condition, istrue, isfalse) => {
                let condition = condition.resolve(subst)?;
                let istrue = istrue.resolve(subst)?;
                let isfalse = if let Some(v) = isfalse {
                    Some(v.resolve(subst)?.into())
                } else {
                    None
                };
                Self::Condition(condition.into(), istrue.into(), isfalse)
            }
        };
        //eprintln!("RESOLVE: {:?} => {:?}", &before, &after);
        Ok(after)
    }
}

impl logic::UnifyValue for Ast {
    type Key = VariableId;
    type Type = Type;
    fn new_type(ty: Type) -> Self {
        Self::Type(ty)
    }
    fn new_unknown(v_id: VariableId, ty: Type) -> Self {
        Self::Variable(Variable::unnamed(v_id, ty))
    }
    fn try_type(&self) -> Option<Type> {
        if let Self::Type(ty) = self {
            Some(ty.clone())
        } else {
            None
        }
    }
    fn get_type(&self) -> Type {
        match self {
            Self::Literal(Literal::Int(_)) => Type::Int,
            Self::Literal(Literal::Float(_)) => Type::Float,
            Self::Literal(Literal::Bool(_)) => Type::Bool,
            Self::Literal(Literal::String(_)) => Type::String,
            Self::Function { ty, .. } => ty.clone(),
            Self::Block(exprs) | Self::Sequence(exprs) => {
                // empty blocks just return unit type
                exprs.last().map_or(Type::Unit, |e| e.get_type())
            }
            Self::Apply(var, _) => var.ty.children().last().expect("No Return Type").clone(),
            Self::Declare(_, expr) => expr.get_type(),
            Self::Assign(_, expr) => expr.get_type(),
            Self::Variable(v) => v.ty.clone(),
            Self::Parameter(v) => v.ty.clone(),
            Self::Extern(args) | Self::Builtin(args) => Type::Func(args.clone()),
            Self::Internal(b) => {
                use Builtin::*;
                match b {
                    SubInt(_, _) | AddInt(_, _) | AddFloat(_, _) => Type::Int,
                    EqInt(_, _) => Type::Bool,
                }
            }
            Self::Type(_) => Type::Type,
            Self::Return(expr) => expr.get_type(),

            // get the type of istrue, we check to make sure these match elsewhere
            Self::Condition(_, istrue, isfalse) => istrue.get_type(),
        }
    }

    fn try_unknown(&self) -> Option<Self::Key> {
        if let Self::Variable(v) = self {
            Some(v.id)
        } else {
            None
        }
    }
}

impl From<Variable> for Ast {
    fn from(item: Variable) -> Self {
        Ast::Variable(item)
    }
}

impl From<Literal> for Ast {
    fn from(item: Literal) -> Self {
        Ast::Literal(item)
    }
}

impl fmt::Display for Ast {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ast::Literal(Literal::Int(x)) => {
                write!(f, "{}int", &x)?;
            }
            Ast::Literal(x) => {
                write!(f, "{:?}", &x)?;
            }
            Ast::Variable(x) => {
                write!(f, "V{}", &x.id.0)?;
            }

            Ast::Parameter(x) => {
                write!(f, "P{}", &x.id.0)?;
            }

            Ast::Type(t) => {
                write!(f, "Type({})", &t)?;
            }

            Ast::Block(exprs) => {
                write!(f, "Block({})", format_list(&exprs))?;
            }

            Ast::Sequence(exprs) => {
                write!(f, "Sequence({})", format_list(&exprs))?;
            }

            Ast::Extern(types) => {
                write!(f, "Extern({})", format_list(&types))?;
            }

            Ast::Builtin(types) => write!(f, "Builtin({})", format_list(&types))?,
            Ast::Internal(v) => write!(f, "Internal({:?})", &v)?,

            Ast::Function { params, body, ty } => {
                write!(f, "Func(({}), {}, {})", format_list(&params), &body, &ty)?;
            }

            Ast::Apply(func, args) => {
                write!(f, "Apply({}, {})", &func, format_list(&args))?;
            }

            Ast::Assign(name, expr) => {
                write!(f, "Assign({}, {})", &name, &expr)?;
            }

            Ast::Declare(var, expr) => {
                write!(f, "Declare(V{}, {})", &var.id.0, &expr)?;
            }

            Ast::Return(expr) => {
                write!(f, "Return({})", &expr)?;
            }
            Ast::Condition(condition, istrue, isfalse) => {
                if isfalse.is_some() {
                    write!(
                        f,
                        "If({}, {}, {})",
                        &condition,
                        &istrue,
                        &isfalse.as_ref().unwrap()
                    )?;
                } else {
                    write!(f, "If({}, {})", &condition, &istrue)?;
                }
            }
        }
        Ok(())
    }
}
