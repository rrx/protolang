use super::*;
use crate::env::LayerValue;
use logic::TypeSignature;
use std::fmt;
use serde::Serialize;

#[derive(Clone, Debug, PartialEq)]
pub struct VariableId(pub usize);

#[derive(Clone, Debug, PartialEq)]
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
    env: Option<Environment>,
}
impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Variable {
    pub fn named(name: String, id: VariableId, ty: Type) -> Self {
        Self {
            name,
            id,
            ty,
            env: None,
        }
    }
    pub fn unnamed(id: VariableId, ty: Type) -> Self {
        let name = format!("v{}", id.0);
        Self {
            name,
            id,
            ty,
            env: None,
        }
    }
    pub fn bind(&mut self, env: Environment) {
        self.env.replace(env);
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
        if let Some(env) = self.env.as_ref() {
            if let Some(v) = env.resolve(&self.name) {
                match v {
                    // declared variables
                    Ast::Declare(var, _) => {
                        // just return the variable
                        return var.clone().resolve_type(subst);
                    }

                    // parameters in functions
                    Ast::Variable(var) => {
                        return var.clone().resolve_type(subst);
                    }
                    _ => unreachable!(),
                }
            }
        }
        self.resolve_type(subst)
    }

    /// generate type equations for the variable that can be used for unification
    pub fn generate_equations(&self) -> logic::Expr<Type> {

        let var_env = self.env.as_ref().unwrap();
        match &self.ty {
            Type::Func(_) => {
                // resolve the name of the function
                // This can yield multiple results and they need to match with parameters
                // We accomplish this here by making use of the type unification system
                //
                // create equation for all matches for the function
                // We could filter this based on the known arguments so far
                // It could be none, and so we could return an error now, rather
                // than waiting for unification
                let possible = var_env
                    .resolve_all(&self.name)
                    .iter()
                    .cloned()
                    .map(|v| v.get_type())
                    .collect::<Vec<_>>();
                logic::Expr::OneOf(self.ty.clone(), possible)
            }
            _ => {
                match var_env.resolve(&self.name) {
                    Some(resolved_v) => {
                        let var_ty = self.ty.clone();
                        let resolved_ty = resolved_v.get_type();

                        // variable matches the type of resolve
                        logic::Expr::Eq(var_ty, resolved_ty)
                    }
                    None => {
                        unimplemented!("unresolved variable: {} {:?}", &self.name, &self);
                    }
                }
            }
        }
    }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "V{}", &self.id.0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Builtin {
    // This implementation is awkward and doesn't work very well.
    AddInt(Box<Ast>, Box<Ast>),
    AddFloat(Box<Ast>, Box<Ast>),
}
impl Builtin {
    pub fn children(&self) -> Vec<&Ast> {
        match self {
            Self::AddInt(a, b) => vec![a, b],
            Self::AddFloat(a, b) => vec![a, b],
            _ => unimplemented!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Ast {
    Type(Type),

    // Const value
    Literal(Literal),

    // Variable, represented by a String
    Variable(Variable),

    Block(Vec<Ast>, Type),

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

    Return(Box<Ast>)
}

impl LayerValue for Ast {}

fn resolve_list(exprs: &Vec<Ast>, subst: &SymbolTable) -> Vec<Ast> {
    exprs.iter().cloned().map(|e| e.resolve(subst)).collect()
}

impl Ast {

    pub fn block(exprs: Vec<Self>) -> Self {
         let ty = exprs.last().expect("Empty Block").get_type();
         Self::Block(exprs, ty)
    }

    pub fn int(i: i64) -> Self {
        Self::Literal(Literal::Int(i as u64))
    }

    pub fn string(s: String) -> Self {
        Self::Literal(Literal::String(s))
    }

    pub fn replace_variable(&self, subst: &SymbolTable) -> Self {
        match &self {
            Self::Variable(v) => {
                Ast::Variable(v.clone().resolve(subst))
            }
            _ => unreachable!(),
        }
    }

    pub fn resolve(&self, subst: &SymbolTable) -> Self {
        let before = self.clone();
        let after = match &self {
            Self::Literal(_) => self.clone(),
            Self::Variable(_) => {
                self.replace_variable(subst)
            }
            Self::Extern(types) => Self::Extern(Type::resolve_list(&types, subst)),
            Self::Builtin(types) => Self::Builtin(Type::resolve_list(&types, subst)),
            Self::Internal(v) => self.clone(),
            Self::Declare(var, expr) => {
                Self::Declare(var.resolve_type(subst), expr.resolve(subst).into())
            }
            Self::Assign(name, expr) => Self::Assign(name.clone(), expr.resolve(subst).into()),
            Self::Function { params, body, ty } => {
                // resolve the types in the params
                let params = params.iter().map(|p| p.resolve_type(subst)).collect::<Vec<_>>();
                let ty = ty.resolve(&subst);
                Self::Function {
                    params,
                    body: body.resolve(subst).into(),
                    ty,
                }
            }
            Self::Block(exprs, ty) => {
                Self::block(resolve_list(exprs, &subst))//, ty.clone()),
            }
            Self::Apply(f, args) => Self::Apply(f.resolve(subst).into(), resolve_list(args, subst)),

            Self::Type(ty) => Self::Type(ty.resolve(subst)),
            Self::Return(expr) => Self::Return(expr.resolve(subst).into()),
        };
        //eprintln!("RESOLVE: {:?} => {:?}", &before, &after);
        after
    }

    pub fn get_type(&self) -> Type {
        match self {
            Self::Literal(Literal::Int(_)) => Type::Int,
            Self::Literal(Literal::Float(_)) => Type::Float,
            Self::Literal(Literal::Bool(_)) => Type::Bool,
            Self::Literal(Literal::String(_)) => Type::String,
            Self::Function { ty, .. } => ty.clone(),
            Self::Block(exprs, ty) => ty.clone(),//exprs.last().expect("Empty Block").get_type(),
            Self::Apply(var, _) => var.ty
                .children()
                .last()
                .expect("No Return Type")
                .clone(),
            Self::Declare(_, expr) => expr.get_type(),
            Self::Assign(_, expr) => expr.get_type(),
            Self::Variable(v) => v.ty.clone(),
            Self::Extern(args) | Self::Builtin(args) => Type::Func(args.clone()),
            Self::Internal(_) => {
                /// TODO: not correct, hardwired for now
                Type::Int
            }
            Self::Type(_) => Type::Type,
            Self::Return(expr) => expr.get_type()
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

            Ast::Block(exprs, ty) => {
                write!(f, "Block({},{})", format_list(&exprs), &ty)?;
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
        }
        Ok(())
    }
}

