use super::types::*;
use crate::env::LayerKey;
use crate::ast::AstNode;
use std::fmt;
use thiserror::Error;
use std::cmp::PartialEq;

#[derive(Clone, Debug)]
pub struct TypeNodePair {
    pub ty: Type,
    pub node: AstNode
}
impl TypeNodePair {
    pub fn new(ty: Type, node: AstNode) -> Self {
        Self { ty, node }
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

pub type SymbolTable = rpds::HashTrieMap<TypeDefinitionId, TypeNodePair>;

impl LayerKey for String {}

#[derive(Error, Debug, Clone)]
pub enum TypeError {
    #[error("Warning: {0}")]
    Warning(String),
    #[error("Error: {0}")]
    Error(String),
}

pub struct TypeChecker {
    context: TypeSystemContext,
    type_equations: Vec<TypeEquation>,
    syms: SymbolTable,
    results: Vec<TypeError>,
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self {
            context: TypeSystemContext::default(),
            type_equations: vec![],
            syms: SymbolTable::default(),
            results: vec![],
        }
    }
}

impl fmt::Display for TypeChecker {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for v in self.type_equations.iter() {
            write!(f, "Eq: {:?}\n", v)?;
        }
        for (k, v) in self.syms.iter() {
            write!(f, "Subs: {:?} => {:?}\n", k, v.ty)?;
        }
        Ok(())
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct TypeEquation {
    left: TypeNodePair,
    // ordered set - should be ordered from general to specific
    right: Vec<TypeNodePair>,
}

impl TypeEquation {
    pub fn new(left: TypeNodePair, right: Vec<TypeNodePair>) -> Self {
        Self { left, right }
    }
}

impl TypeChecker {
    pub fn new_unknown_type(&mut self) -> Type {
        Type::new_unknown(self.context.next_id())
    }

    pub fn add(&mut self, eq: TypeEquation) {
        self.type_equations.push(eq);
    }

    pub fn resolve_type(&self, p: &TypeNodePair) -> Option<TypeNodePair> {
        if let Type::Unknown(id) = p.ty {
            self.get_type_by_id(&id)
        } else {
            Some(p.clone())
        }
    }

    pub fn get_type_by_id(&self, ty_id: &TypeDefinitionId) -> Option<TypeNodePair> {
        let mut ty_id = *ty_id;
        loop {
            match self.syms.get(&ty_id) {
                Some(v) => {
                    if let Type::Unknown(id) = v.ty {
                        ty_id = id;
                    } else {
                        return Some(v.clone());
                    }
                }
                None => break,
            }
        }
        None
    }

    pub fn unify_all(&mut self) -> SymbolTable {
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
            self.results.push(TypeError::Error(msg));
        }
        self.syms.clone()
    }
}

fn subs_if_exists<'a>(mut p: &'a TypeNodePair, subst: &'a SymbolTable) -> &'a TypeNodePair {
    if let Type::Unknown(type_id) = p.ty {
        if subst.contains_key(&type_id) {
            p = subst.get(&type_id).unwrap();
        }
    }
    p
}

// Does ty1 occur in ty2?
fn occurs_check(p1: &TypeNodePair, p2: &TypeNodePair, subst: &SymbolTable) -> bool {
    if p1 == p2 {
        return true;
    }

    assert!(!p1.ty.is_unknown());

    let p2 = subs_if_exists(p2, &subst);

    if p1 == p2 {
        return true;
    }

    // if ty1 occurs in any of a functions parameters
    if let Type::Func(sig) = &p2.ty {
        return sig.iter().any(|s| occurs_check(p1, &TypeNodePair::new(s.clone(), p2.node.clone()), subst));
    }
    false
}

fn unify_eq(ty1: &TypeNodePair, ty2: &TypeNodePair, subst: Option<SymbolTable>) -> Option<SymbolTable> {
    log::debug!("unify_eq: {:?} :: {:?}", ty1, ty2);

    if ty1.ty == ty2.ty {
        return subst;
    }

    let subst = subst.unwrap();

    // make substitutions
    let ty1 = subs_if_exists(ty1, &subst);
    let ty2 = subs_if_exists(ty2, &subst);

    if let Type::Unknown(type_id) = ty1.ty {
        return Some(subst.insert(type_id, ty2.clone()));
    } else if let Type::Unknown(type_id) = ty2.ty {
        return Some(subst.insert(type_id, ty1.clone()));
    }

    if ty1 == ty2 {
        return Some(subst);
    }

    if occurs_check(ty1, ty2, &subst) {
        return None;
    }

    if let Type::Func(sig1) = &ty1.ty {
        if let Type::Func(sig2) = &ty2.ty {
            let sig1 = sig1.into_iter().map(|s| {
                TypeNodePair::new(s.clone(), ty1.node.clone())
            }).collect::<Vec<TypeNodePair>>();
            let sig2 = sig2.into_iter().map(|s| {
                TypeNodePair::new(s.clone(), ty1.node.clone())
            }).collect::<Vec<TypeNodePair>>();
            return unify_fn(&sig1, &sig2, subst.clone());
        } else {
            return None;
        }
    }

    None
}

fn unify_fn(sig1: &Vec<TypeNodePair>, sig2: &Vec<TypeNodePair>, mut subst: SymbolTable) -> Option<SymbolTable> {
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

fn unify(left: &TypeNodePair, right: &Vec<TypeNodePair>, subst: Option<SymbolTable>) -> Option<SymbolTable> {
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

impl fmt::Display for TypeEquation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} :: {:?}", self.left, self.right)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;
    use crate::ast::{AstNodeInner, AstBuilder, Variable};

    struct Data {
        int: TypeNodePair,
        float: TypeNodePair,
        boolean: TypeNodePair,
        x: TypeNodePair,
        y: TypeNodePair,
        z: TypeNodePair,
        b: AstBuilder,
    }
    impl Data {
        fn new() -> Self {
            let mut b = AstBuilder::default();
            Self {
                int: b.int(1).into(),
                float: b.float(1.).into(),
                boolean: b.boolean(false).into(),
                x: b.var("x").into(),
                y: b.var("y").into(),
                z: b.var("z").into(),
                b,
            }
        }
    }

    #[test]
    fn test_unify() {
        let mut data = Data::new();
        let eq1 = TypeEquation::new(data.x.clone(), vec![data.int.clone(), data.float.clone()]);
        let eq2 = TypeEquation::new(data.y.clone(), vec![data.z.clone()]);
        let eq3 = TypeEquation::new(data.z.clone(), vec![data.boolean.clone()]);
        data.b.check.add(eq1);
        data.b.check.add(eq2);
        data.b.check.add(eq3);
        let _ = data.b.check.unify_all();
        assert_eq!(data.int, data.b.check.resolve_type(&data.x.clone()).unwrap());
        assert_eq!(data.boolean, data.b.check.resolve_type(&data.y.clone()).unwrap());
        assert_eq!(data.boolean, data.b.check.resolve_type(&data.z.clone()).unwrap());
    }

    fn create_func_node(name: &str, args: Vec<Type>) -> TypeNodePair {
        let f_ty = Type::Func(args);
        let node = AstNodeInner::new(Variable::new(name.into()).into(), f_ty.clone()).into();
        TypeNodePair::new(f_ty, node)
    }

    #[test]
    fn test_unify_func_match() {
        let mut data = Data::new();
        // solve for the type of x
        let ty = data.x.ty.clone();
        let unknown = create_func_node("a", vec![Type::Int, ty.clone()]);
        data.b.check.add(TypeEquation::new(
            unknown,
            vec![
                create_func_node("b", vec![Type::Float, Type::Float]),
                create_func_node("c", vec![Type::Int, Type::Int]),
            ]));
        let _ = data.b.check.unify_all();
        assert_eq!(data.b.check.resolve_type(&data.x.clone()).unwrap(), data.int);
    }

    #[test]
    fn func_mismatch() {
        let data = Data::new();
        
        // solve for the type of x
        let unknown = create_func_node("a", vec![Type::Float, data.x.ty.clone()]);
        let s = SymbolTable::new();
        let out = unify(
            &unknown,
            &vec![create_func_node("b", vec![Type::Int, Type::Int])],
            Some(s),
        );
        assert_eq!(out, None)
    }

    #[test]
    fn types_match() {
        let data = Data::new();
        let s = SymbolTable::default();
        let out = unify(&data.int.clone(), &vec![data.float.clone(), data.int.clone()], Some(s.clone()));
        assert_eq!(out, Some(s));
    }

    #[test]
    fn types_mismatch() {
        let data = Data::new();
        let s = SymbolTable::default();
        let out = unify(&data.int.clone(), &vec![data.float.clone()], Some(s));
        assert_eq!(out, None);
    }

    #[test]
    fn types_match_float() {
        let mut data = Data::new();
        data.b.check.add(TypeEquation::new(data.x.clone(), vec![data.float.clone()]));
        let _ = data.b.check.unify_all();
        assert_eq!(data.b.check.resolve_type(&data.x).unwrap(), data.float.clone());
    }

    #[test]
    fn t2() {
        let mut data = Data::new();
        data.b.check.add(TypeEquation::new(data.x.clone(), vec![data.float.clone(), data.int.clone()]));
        let _ = data.b.check.unify_all();
        assert_eq!(data.b.check.resolve_type(&data.x.clone()).unwrap(), data.float.clone());
    }

    #[test]
    fn test_no_solution() {
        let mut data = Data::new();
        data.b.check.add(TypeEquation::new(data.x.clone(), vec![data.y.clone()]));
        let s = data.b.check.unify_all();
        println!("syms{}", &data.b.check);
        assert_eq!(s.get(&TypeDefinitionId(0)).unwrap(), &data.y.clone());
    }

    #[test]
    fn t4() {
        // no match
        let data = Data::new();
        let s = SymbolTable::default();
        //let x: TypeNodePair = b.var("x").into();
        let unknown = create_func_node("f", vec![Type::Int, Type::Float, data.x.ty]);
        //let ty = c.new_unknown_type();
        let out = unify(
            //&Type::Func(vec![Type::Int, Type::Float, ty]),
            &unknown,
            &vec![
                //Type::Func(vec![Type::Float, Type::Float, Type::Float]),
                create_func_node("y", vec![Type::Float, Type::Float, Type::Float]),
                create_func_node("z", vec![Type::Int, Type::Int, Type::Int]),
            ],
            Some(s),
        );
        assert_eq!(out, None);
    }
}
