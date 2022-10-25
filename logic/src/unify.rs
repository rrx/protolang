use std::fmt;
pub type SymbolTable<T> = rpds::HashTrieMap<DefinitionId, T>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefinitionId(pub usize);

impl From<usize> for DefinitionId {
    fn from(item: usize) -> Self {
        Self(item)
    }
}

#[derive(Debug, PartialEq)]
pub enum UnifyResult {
    NoSolution,
    MismatchShape,
    OccursCheck,
    Incomplete,
    Ok
}

pub trait TypeSignature<T> {
    fn sig(&self) -> Vec<T>;
    fn unknown(&self) -> Option<DefinitionId>;
    fn var(u: DefinitionId) -> T;
}

use self::Expr::*;

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum Expr<T> {
    OneOf(T, Vec<T>),
    Eq(T, T),
}

impl<T: TypeSignature<T> + Clone + fmt::Debug + PartialEq> Expr<T> {
    fn unify(&self, subst: SymbolTable<T>) -> (UnifyResult, SymbolTable<T>) {
        match self {
            Eq(ty1, ty2) => {
                unify_eq(ty1.sig(), ty2.sig(), subst)
            }
            OneOf(ty1, types) => {
                let s = subst;
                for ty2 in types {
                    match unify_eq(ty1.sig(), ty2.sig(), s.clone()) {
                        (UnifyResult::Ok, s) => {
                            return (UnifyResult::Ok, s);
                        }
                        _ => ()
                    }
                }
                (UnifyResult::NoSolution, s)
            }
            _ => unimplemented!()
        }

    }
}

fn subs_if_exists<T: Clone + TypeSignature<T>>(ty: &T, subst: SymbolTable<T>) -> T {
    let mut ty = ty.clone();
    if let Some(type_id) = ty.unknown() {
        if subst.contains_key(&type_id) {
            ty = subst.get(&type_id).unwrap().clone();
        }
    }
    ty
}

fn unify<T: PartialEq + Clone + fmt::Debug + TypeSignature<T>>(ty1: T, ty2: T, subst: SymbolTable<T>) -> (UnifyResult, SymbolTable<T>) { 
    // substitute
    let ty1 = subs_if_exists(&ty1, subst.clone());
    let ty2 = subs_if_exists(&ty2, subst.clone());

    let u1 = ty1.unknown();
    let u2 = ty2.unknown();

    let sig1 = ty1.sig();
    let sig2 = ty2.sig();

    //println!("{:?}", (&u1, &u2));

    if sig1.len() > 1 {
        return unify_eq(sig1, sig2, subst);
    } else if u1.is_none() && u2.is_none() {
        // both are known
        if occurs_check(&ty1, &sig2) {
            return (UnifyResult::OccursCheck, subst);
        }

        if sig1.len() == 1 {
            if ty1 != ty2 {
                return (UnifyResult::NoSolution, subst);
            }
        } else {

        }
    } else if u1.is_none() && u2.is_some() {
        // one unknown
        return (UnifyResult::Ok, subst.insert(u2.unwrap(), ty1.clone()));
    } else if u1.is_some() && u2.is_none() {
        // one unknown
        return (UnifyResult::Ok, subst.insert(u1.unwrap(), ty2.clone()));
    } else if u1.is_some() && u2.is_some() {
        // two unknowns, nothing to do
        if u1 == u2 {
            // they should never be equal
            unreachable!()
        } else {
            return (UnifyResult::Incomplete, subst);
        }
    }

    if ty1 == ty2 {
        return (UnifyResult::Ok, subst);
    }
    (UnifyResult::Ok, subst)
}

fn unify_eq<T: fmt::Debug + PartialEq + Clone + TypeSignature<T>>(ty1: Vec<T>, ty2: Vec<T>, mut subst: SymbolTable<T>) -> (UnifyResult, SymbolTable<T>) {
    log::debug!("unify_eq: {:?} :: {:?}", ty1, ty2);

    // ensure we have the same shape
    if ty1.len() != ty2.len() {
        return (UnifyResult::MismatchShape, subst);
    }

    if ty1 == ty2 {
        return (UnifyResult::Ok, subst);
    }

    for (a1, a2) in ty1.into_iter().zip(ty2.into_iter()) {
        let (res, new_subst) = unify(a1.clone(), a2.clone(), subst);
        log::debug!("unify: {:?} :: {:?} :: {:?}", &a1, &a2, res);
        match res {
            UnifyResult::Ok => {
                subst = new_subst;
            }
            _ => {
                return (res, new_subst);
            }
        }
    }

    (UnifyResult::Ok, subst)
}

// Does ty1 occur in ty2?
fn occurs_check<T: PartialEq + TypeSignature<T>>(ty1: &T, sig: &Vec<T>) -> bool {
    // if ty1 occurs in any of a functions parameters
    return sig.iter().any(|s| {
        s == ty1 
    });
}

fn unify_all<T: TypeSignature<T> + Clone + PartialEq + fmt::Debug>(equations: Vec<Expr<T>>, mut subst: SymbolTable<T>) -> (Vec<Expr<T>>, SymbolTable<T>) {
    let mut out = vec![];
    println!("Start");
    for (i, eq) in equations.into_iter().enumerate() {
        let (res, s) = eq.unify(subst.clone());
        println!("Unify[{}]: {:?} => {:?}", i, &eq, res);
        match res {
            UnifyResult::Ok => {
                subst = s;
            }
            _ => {
                println!("no unify on {:?}", (res, &eq));
                out.push(eq);
            }
        }
    }

    println!("x:{}, {}", out.len(), subst.size());
    //for i in 0..subst.size() {
        //let v = subst.get(&i.into()).unwrap();
        //println!("{:?}={:?}", i, v);
    //}
    for (k, v) in subst.iter() {
        //let v = subst.get(&i.into()).unwrap();
        println!("{:?}={:?}", k, v);
    }
    for v in out.iter() {
        println!("Eq: {:?}", v);
    }

    (out, subst)
}

pub fn unify_start<T: TypeSignature<T> + Clone + PartialEq + fmt::Debug>(mut equations: Vec<Expr<T>>) -> (UnifyResult, SymbolTable<T>) {
    let mut subst = SymbolTable::default();
    let mut count = equations.len();
    let mut res = UnifyResult::Ok;

    loop {
        let (results, s) = unify_all(equations, subst.clone());
        subst = s;
        equations = results;

        // Are we out of equations?
        if equations.len() == 0 {
            break;
        }

        // check if anything has changed
        if subst.size() == count {
            res = UnifyResult::NoSolution;
            break;
        }
        count = subst.size();
    }
    (res, subst)
}

#[cfg(test)]
mod test {
    use super::*;
    use test_log::test;
    use Type::*;

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum Type {
        Int,
        Float,
        Name(String),
        Seq(Vec<Type>),
        Var(DefinitionId)
    }

    impl TypeSignature<Type> for Type {
        fn unknown(&self) -> Option<DefinitionId> {
            match self {
                Type::Var(id) => Some(*id),
                _ => None
            }
        }
        fn sig(&self) -> Vec<Type> {
            match self {
                Seq(args) => args.clone(),
                _ => vec![self.clone()]
            }
        }
        fn var(u: DefinitionId) -> Type {
            Type::Var(u)
        }
    }


    fn var(u: usize) -> Type {
        Type::Var(u.into())
    }

    fn seq(args: Vec<Type>) -> Type {
        Type::Seq(args)
    }

    fn name(s: &str) -> Type {
        Type::Name(s.into())
    }


    #[test]
    fn logic_occurs() {
        assert_eq!(true, occurs_check(&name("a"), &vec![name("b"), name("a")]));
        assert_eq!(false, occurs_check(&name("a"), &vec![name("b")]));

        // check occurs using an unknown
        let x = var(0);
        let s = seq(vec![x.clone(), x.clone()]);
        assert_eq!(true, occurs_check(&x, &s.sig()));
    }

    #[test]
    fn logic_solution() {
        let start = vec![
            Eq(var(8), var(7)),
            Eq(var(0), Type::Int),
            Eq(var(1), var(0)),
            Eq(var(6), var(1)),
            Eq(var(7), var(6)),
            Eq(seq(vec![name("a"), name("b")]), seq(vec![var(2), var(3)])),
            Eq(seq(vec![name("c"), var(4)]), seq(vec![var(5), name("d")])),

            Eq(var(10), Type::Float),
            Eq(var(11), name("w")),
            OneOf(var(9), vec![var(10), var(11)]),
            OneOf(name("y"), vec![var(12)]),
            OneOf(name("z"), vec![var(13), var(14)]),
            Eq(var(14), name("x")),
        ];

        let (res, subst) = unify_start(start);
        assert_eq!(UnifyResult::Ok, res);
        assert_eq!(subst.get(&8.into()), Some(&Type::Int));
        assert_eq!(subst.get(&13.into()), Some(&name("z")));
    }

    #[test]
    fn logic_no_solution() {
        let start: Vec<Expr<Type>> = vec![
            Eq(name("a"), name("b")),
        ];

        let (res, _) = unify_start(start);

        // no solution, because types do not match
        assert_eq!(UnifyResult::NoSolution, res);
    }

    #[test]
    fn logic_no_solution_2() {
        let start: Vec<Expr<Type>> = vec![
            Eq(
                seq(vec![name("a"), name("b")]),
                seq(vec![name("c"), name("d")]),
            ),
        ];

        let (res, _) = unify_start(start);
        // no solution, because types do not match
        assert_eq!(UnifyResult::NoSolution, res);
    }

    #[test]
    fn logic_nested() {
        let s1 = seq(vec![
                     seq(vec![name("a"), var(0)]),
                     seq(vec![var(1), name("b")]),
        ]);

        let s2 = seq(vec![
                     seq(vec![var(3), name("d")]),
                     seq(vec![name("c"), var(2)]),
        ]);

        let start: Vec<Expr<Type>> = vec![
            Eq(s1, s2),
        ];

        let (res, _) = unify_start(start);
        // no solution, because types do not match
        assert_eq!(UnifyResult::Ok, res);
    }
}


