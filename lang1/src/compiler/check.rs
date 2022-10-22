use crate::ir::*;
use crate::results::LangError;
use std::fmt;

pub struct TypeChecker<N> {
    pub type_equations: Vec<TypeEquation<N>>,
    type_counter: usize,
    results: Vec<LangError>
}

impl<N> Default for TypeChecker<N> {
    fn default() -> Self {
        Self { type_equations: vec![], type_counter: 0, results: vec![] }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeEquation<N> {
    left: Type,
    // ordered set - should be ordered from general to specific
    right: Vec<Type>,
    node: N,
    env: Environment,
}

impl<N> TypeEquation<N> {
    pub fn new(left: Type, right: Vec<Type>, node: N, env: Environment) -> Self {
        Self {
            left,
            right,
            node,
            env,
        }
    }
}

impl<N> TypeChecker<N> {
    pub fn next_type_counter(&mut self) -> usize {
        let x = self.type_counter;
        self.type_counter += 1;
        x
    }

    pub fn get_fresh_typename(&mut self) -> usize {
        self.next_type_counter()
    }

    pub fn new_unknown_type(&mut self) -> Type {
        Type::new_unknown(self.get_fresh_typename())
    }

    fn subs_if_exists<'a>(mut ty: &'a Type, subst: &'a SymbolTable) -> &'a Type {
        if let Type::Unknown(type_id, traits) = ty {
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

        let ty2 = Self::subs_if_exists(ty2, &subst);

        if ty1 == ty2 {
            return true;
        }

        // if ty1 occurs in any of a functions parameters
        if let Type::Func(sig) = ty2 {
            return sig.iter().any(|s| Self::occurs_check(ty1, s, subst));
        }
        false
    }

    fn unify_eq(&self, ty1: &Type, ty2: &Type, subst: Option<SymbolTable>) -> Option<SymbolTable> {
        log::debug!("unify_eq: {:?} :: {:?}", ty1, ty2);

        if ty1 == ty2 {
            return subst;
        }

        let subst = subst.unwrap();

        // make substitutions
        let ty1 = Self::subs_if_exists(ty1, &subst);
        let ty2 = Self::subs_if_exists(ty2, &subst);

        if let Type::Unknown(name, traits) = ty1 {
            return Some(subst.insert(name.clone(), ty2.clone()));
        } else if let Type::Unknown(name, traits) = ty2 {
            return Some(subst.insert(name.clone(), ty1.clone()));
        }

        if ty1 == ty2 {
            return Some(subst);
        }

        if Self::occurs_check(ty1, ty2, &subst) {
            return None;
        }

        if let Type::Func(sig1) = ty1 {
            if let Type::Func(sig2) = ty2 {
                return self.unify_fn(sig1, sig2, subst.clone());
            } else {
                return None;
            }
        }

        None
    }

    fn unify_fn(
        &self,
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
            match self.unify(a1, &Vec::from([a2.clone()]), Some(local_subst)) {
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

    pub fn unify(
        &self,
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
            if let Some(s) = self.unify_eq(left, r, subst.clone()) {
                local_subst = Some(s);
                break;
            }
        }

        // return the solution if it was found
        local_subst
    }

    /*
    pub fn unify_all(&mut self) -> Option<SymbolTable> {
        let mut subst = Some(SymbolTable::default());
        let mut errors = vec![];
        for eq in &self.type_equations {
            match self.unify(&eq.left, &eq.right, subst.clone()) {
                Some(s) => {
                    subst = Some(s);
                }
                None => {
                    errors.push(eq);
                }
            }
        }
        for eq in errors {
            let msg = format!("Unable to unify: {:?} :: {:?}", &eq.left, &eq.right);
            self.results
                .push(LangError::error(msg.clone(), eq.node.loc.clone()));
        }
        subst
    }
    */

    pub fn add(&mut self, eq: TypeEquation<N>) {
        self.type_equations.push(eq);
    }
}

impl<N: fmt::Debug> fmt::Display for TypeEquation<N> {
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



    #[test]
    fn function() {
        //let checker = TypeChecker::default();
    }
}
