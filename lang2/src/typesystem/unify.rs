use std::fmt;
use thiserror::Error;
use super::types::*;
use crate::env::{EnvLayers, LayerValue, LayerKey};

pub type SymbolTable = rpds::HashTrieMap<TypeDefinitionId, Type>;
pub type Environment<N> = EnvLayers<String, N>;

impl LayerKey for String {}

#[derive(Error, Debug, Clone)]
pub enum TypeError {
    #[error("Warning: {0}")]
    Warning(String),
    #[error("Error: {0}")]
    Error(String),
}

pub struct TypeChecker<N: LayerValue> {
    context: TypeSystemContext,
    type_equations: Vec<TypeEquation<N>>,
    syms: SymbolTable,
    results: Vec<TypeError>
}

impl<N: LayerValue> Default for TypeChecker<N> {
    fn default() -> Self {
        Self {
            context: TypeSystemContext::default(),
            type_equations: vec![],
            syms: SymbolTable::default(),
            results: vec![]
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeEquation<N: LayerValue> {
    left: Type,
    // ordered set - should be ordered from general to specific
    right: Vec<Type>,
    node: N,
    env: Environment<N>,
}

impl<N: LayerValue> TypeEquation<N> {
    pub fn new(left: Type, right: Vec<Type>, node: N, env: Environment<N>) -> Self {
        Self {
            left,
            right,
            node,
            env,
        }
    }
}

impl<N: LayerValue> TypeChecker<N> {
    pub fn new_unknown_type(&mut self) -> Type {
        Type::new_unknown(self.context.next_id())
    }

    pub fn add(&mut self, eq: TypeEquation<N>) {
        self.type_equations.push(eq);
    }

    pub fn resolve_type(&self, ty: &Type) -> Option<Type> {
        if let Type::Unknown(id) = ty {
            self.get_type_by_id(id)
        } else {
            Some(ty.clone())
        }
    }

    fn get_type_by_id(&self, ty_id: &TypeDefinitionId) -> Option<Type> {
        let mut ty_id = ty_id;
        loop {
            match self.syms.get(&ty_id) {
                Some(v) => {
                    if let Type::Unknown(id) = v {
                        ty_id = id;
                    } else {
                        return Some(v.clone())
                    }
                }
                None => break
            }
        }
        None
    }

    pub fn unify_all(&mut self) {
        //let mut subst = SymbolTable::default();
        let mut errors = vec![];
        for eq in &self.type_equations {
            match unify(&eq.left, &eq.right, Some(self.syms.clone())) {
                Some(s) => {
                    self.syms = s;
                }
                None => {
                    errors.push(eq);
                }
            }
        }

        for eq in errors {
            let msg = format!("Unable to unify: {:?} :: {:?}", &eq.left, &eq.right);
            self.results
                .push(TypeError::Error(msg));
        }
    }
}

fn subs_if_exists<'a>(mut ty: &'a Type, subst: &'a SymbolTable) -> &'a Type {
    if let Type::Unknown(type_id) = ty {
        if subst.contains_key(&type_id) {
            ty = subst.get(&type_id).unwrap();
        }
    }
    ty
}

// Does ty1 occur in ty2?
fn occurs_check(ty1: &Type, ty2: &Type, subst: &SymbolTable) -> bool {
    if ty1 == ty2 {
        return true;
    }

    assert!(!ty1.is_unknown());

    let ty2 = subs_if_exists(ty2, &subst);

    if ty1 == ty2 {
        return true;
    }

    // if ty1 occurs in any of a functions parameters
    if let Type::Func(sig) = ty2 {
        return sig.iter().any(|s| occurs_check(ty1, s, subst));
    }
    false
}

fn unify_eq(ty1: &Type, ty2: &Type, subst: Option<SymbolTable>) -> Option<SymbolTable> {
    log::debug!("unify_eq: {:?} :: {:?}", ty1, ty2);

    if ty1 == ty2 {
        return subst;
    }

    let subst = subst.unwrap();

    // make substitutions
    let ty1 = subs_if_exists(ty1, &subst);
    let ty2 = subs_if_exists(ty2, &subst);

    if let Type::Unknown(type_id) = ty1 {
        return Some(subst.insert(*type_id, ty2.clone()));
    } else if let Type::Unknown(type_id) = ty2 {
        return Some(subst.insert(*type_id, ty1.clone()));
    }

    if ty1 == ty2 {
        return Some(subst);
    }

    if occurs_check(ty1, ty2, &subst) {
        return None;
    }

    if let Type::Func(sig1) = ty1 {
        if let Type::Func(sig2) = ty2 {
            return unify_fn(sig1, sig2, subst.clone());
        } else {
            return None;
        }
    }

    None
}

fn unify_fn(
    sig1: &Vec<Type>,
    sig2: &Vec<Type>,
    mut subst: SymbolTable,
) -> Option<SymbolTable> {
    log::debug!("unify_fn: {:?} :: {:?}", sig1, sig2);

    if sig1.len() != sig2.len() {
        return None;
    }

    let mut local_subst = SymbolTable::default();
    for (a1, a2) in sig1.iter().zip(sig2.iter()) {
        match unify(a1, &Vec::from([a2.clone()]), Some(local_subst)) {
            Some(s) => {
                local_subst = s;
            }
            None => {
                return None;
            }
        }
    }

    for (k, v) in local_subst.iter() {
        subst = subst.insert(k.clone(), v.clone());
    }

    log::debug!("unify_fn: {:?}", &subst);
    Some(subst)
}

fn unify(
    left: &Type,
    right: &Vec<Type>,
    subst: Option<SymbolTable>,
) -> Option<SymbolTable> {
    log::debug!("unify: {:?} :: {:?}", left, right);
    if subst == None {
        return None;
    }

    let mut local_subst = None;
    for r in right {
        if let Some(s) = unify_eq(left, r, subst.clone()) {
            local_subst = Some(s);
            break;
        }
    }

    // return the solution if it was found
    local_subst
}


impl<N: fmt::Debug + LayerValue> fmt::Display for TypeEquation<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} :: {:?}, from: {:?}",
            self.left, self.right, &self.node//.loc
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;

    type Node = usize;

    #[test]
    fn unify() {
        let mut checker = TypeChecker::default();
        let env = Environment::default();
        let ty_0 = checker.new_unknown_type(); 
        let eq1 = TypeEquation::new(ty_0.clone(), vec![Type::Int, Type::Float], 0, env.clone());
        let ty_1 = checker.new_unknown_type(); 
        let ty_2 = checker.new_unknown_type(); 
        let eq2 = TypeEquation::new(ty_1.clone(), vec![ty_2.clone()], 1, env.clone());
        let eq3 = TypeEquation::new(ty_2.clone(), vec![Type::Bool], 2, env);
        checker.add(eq1);
        checker.add(eq2);
        checker.add(eq3);
        let syms = checker.unify_all();
        checker.syms.iter().for_each(|(k, v)| {
            println!("{:?} => {:?}", k, v);
        });

        println!("{:?}", &ty_0);
        assert_eq!(Some(Type::Int), checker.resolve_type(&ty_0));
        assert_eq!(Some(Type::Bool), checker.resolve_type(&ty_1));
        assert_eq!(Some(Type::Bool), checker.resolve_type(&ty_2));
    }
}
