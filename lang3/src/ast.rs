use crate::env::{LayerValue};
use logic::{TypeSignature};
use std::fmt;
use super::*;

#[derive(Clone, Debug, PartialEq)]
pub struct VariableId(pub usize);

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Bool(bool),
    Int(u64),
    Float(f64),
    String(String),
}

pub type AstType = Type;

#[derive(Clone, Debug)]
pub struct Variable {
    pub id: VariableId,
    pub ty: Type,
    pub name: String,
    pub bound: Option<AstType>,
    pub env: Option<Environment>
}
impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}


impl Variable {
    pub fn named(name: String, id: VariableId, ty: Type) -> Self {
        Self { name, bound: None, id, ty, env: None  }
    }
    pub fn unnamed(id: VariableId, ty: Type) -> Self {
        let name = format!("v{}", id.0);
        Self { name, bound: None, id, ty, env: None }
    }
    pub fn bind(&mut self, ast: AstType) {
        self.bound.replace(ast);
    }
    pub fn resolve(&self, subst: &SymbolTable) -> Self {
        let mut v = self.clone();
        v.ty = v.ty.resolve(subst);
        v
    }
}
impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "V{}", &self.id.0)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Ast {
    Type(Type),

    // Const value
    Literal(Literal),

    // Variable, represented by a String
    Variable(Variable),

    Block(Vec<Ast>),

    // A function that is defined externally
    Extern(Vec<Type>), // args

    // similar to extern, but it get's handled internally
    Builtin(Vec<Type>), // args

    // A function that is defined as part of the program
    Function { params: Vec<Variable>, body: Box<Ast>, ty: Type},

    // function application
    Apply(Box<Ast>, Vec<Ast>), // function, args

    Assign(String, Box<Ast>),

    Declare(Variable, Box<Ast>),
}

fn resolve_list(exprs: &Vec<Ast>, subst: &SymbolTable) -> Vec<Ast> {
    exprs.iter().cloned().map(|e| e.resolve(subst)).collect()
}

impl Ast {
    pub fn resolve(&self, subst: &SymbolTable) -> Self {
        let before = self.clone();
        let after = match &self {
            Self::Literal(_) => self.clone(),
            Self::Variable(v) => {
                Self::Variable(v.resolve(subst))
            }
            Self::Extern(types) => {
                Self::Extern(Type::resolve_list(&types, subst))
            }
            Self::Builtin(types) => {
                Self::Builtin(Type::resolve_list(&types, subst))
            }
            Self::Declare(var, expr) => {
                Self::Declare(var.resolve(subst), expr.resolve(subst).into())
            }
            Self::Assign(name, expr) => {
                Self::Assign(name.clone(), expr.resolve(subst).into())
            }
            Self::Function { params, body, ty } => {
                // resolve the types in the params
                let params = params.iter().map(|p| p.resolve(subst)).collect::<Vec<_>>();
                let ty = ty.resolve(&subst);
                Self::Function { params, body: body.resolve(subst).into(), ty }
            }
            Self::Block(exprs) => {
                Self::Block(resolve_list(exprs, &subst))
            }
            Self::Apply(f, args) => {
                Self::Apply(f.resolve(subst).into(), resolve_list(args, subst))
            }

            Self::Type(ty) => Self::Type(ty.resolve(subst)),
        };
        eprintln!("RESOLVE: {:?} => {:?}", &before, &after);
        after
    }

    pub fn get_type(&self) -> Type {
        match self {
            Self::Literal(Literal::Int(_)) => Type::Int,
            Self::Literal(Literal::Float(_)) => Type::Float,
            Self::Literal(Literal::Bool(_)) => Type::Bool,
            Self::Literal(Literal::String(_)) => Type::String,
            Self::Function { ty, .. } => ty.clone(),
            Self::Block(exprs) => exprs.last().expect("Empty Block").get_type(),
            Self::Apply(f, _) => f.get_type().children().last().expect("No Return Type").clone(),
            Self::Declare(_, expr) => expr.get_type(),
            Self::Assign(_, expr) => expr.get_type(),
            Self::Variable(v) => v.ty.clone(),
            Self::Extern(args) | Self::Builtin(args) => Type::Func(args.clone()),
            Self::Type(_) => Type::Type,
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
            Ast::Type(t) => {
                write!(f, "Type({})", &t)?;
            }

            Ast::Block(exprs) => {
                write!(f, "Block({})", format_list(&exprs))?;
            }

            Ast::Extern(types) => {
                write!(f, "Extern({})", format_list(&types))?;
            }

            Ast::Builtin(types) => write!(f, "Builtin({})", format_list(&types))?,

            Ast::Function { params, body, ty } => {
                write!(f, "Func({}, {}, {})", format_list(&params), &body, &ty)?;
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
        }
        Ok(())
    }
}

fn format_list<T: fmt::Display>(args: &Vec<T>) -> String {
    args.iter().map(|x| format!("{}", x)).collect::<Vec<_>>().join(", ")
}

/*
impl<T: fmt::Debug> fmt::Debug for Ast<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            //Ast::Variable(x) => {
                //write!(f, "V{}", &x.id.0)?;
            //}
            _ => {
                write!(f, "{:?}", &self)
            }
        }
    }
}
*/

impl LayerValue for Ast {}

impl Ast {
    pub fn int(i: i64) -> Self {
        Self::Literal(Literal::Int(i as u64))
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


