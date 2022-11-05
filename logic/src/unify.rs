use std::fmt;
pub type SymbolTable<K, T> = rpds::HashTrieMap<K, T>;

pub fn subst_get_type_by_id<K: UnifyKey, T: UnifyValue<Key=K, Value=T>>(
    subst: &SymbolTable<K, T>,
    ty_id: &K,
) -> Option<T> {
    let mut ty_id = *ty_id;
    loop {
        match subst.get(&ty_id) {
            Some(v) => {
                if let Some(id) = v.unknown() {
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

pub trait UnifyKey: Clone + Copy + PartialEq + std::cmp::Eq + std::hash::Hash + fmt::Debug + fmt::Display {}
pub trait UnifyValue: Clone + fmt::Debug + PartialEq {
    type Key;
    type Value;
    fn children(&self) -> Vec<Self::Value>;
    fn unknown(&self) -> Option<Self::Key>;
    fn var(u: Self::Key) -> Self::Value;
}

#[derive(Debug, PartialEq)]
pub enum UnifyResult {
    NoSolution,
    MismatchShape,
    OccursCheck,
    Incomplete,
    Ok,
}

//pub trait TypeSignature<D, T>: UnifyValue {
    //fn children(&self) -> Vec<T>;
    //fn unknown(&self) -> Option<D>;
    //fn var(u: D) -> T;
//}

use self::Expr::*;

type ExprFunc<K, T> = fn(T, T, T, SymbolTable<K, T>) -> (UnifyResult, SymbolTable<K, T>);

#[derive(Clone, Debug, Hash)]
pub enum Expr<K, T> {
    OneOf(T, Vec<T>),
    Eq(T, T),
    Or(Vec<Expr<K, T>>),
    Func(T, T, T, ExprFunc<K, T>),
}

impl<K: UnifyKey, T: UnifyValue<Key=K, Value=T>> Expr<K, T> {
    fn unify(&self, subst: SymbolTable<K, T>) -> (UnifyResult, SymbolTable<K, T>) {
        match self {
            Eq(ty1, ty2) => unify_eq(vec![ty1.clone()], vec![ty2.clone()], subst),
            OneOf(ty1, types) => {
                let s = subst;
                for ty2 in types {
                    match unify(ty1.clone(), ty2.clone(), s.clone()) {
                        (UnifyResult::Ok, s) => {
                            return (UnifyResult::Ok, s);
                        }
                        _ => (),
                    }
                }
                (UnifyResult::NoSolution, s)
            }
            Or(exprs) => {
                if exprs.len() == 0 {
                    (UnifyResult::Ok, subst)
                } else {
                    let s = subst;
                    for expr in exprs {
                        let (res1, s1) = expr.unify(s.clone());
                        log::debug!("or: {:?}", (&expr, &res1));
                        if res1 == UnifyResult::Ok {
                            return (res1, s1);
                        }
                    }
                    (UnifyResult::NoSolution, s)
                }
            }
            Func(x, y, z, f) => f(x.clone(), y.clone(), z.clone(), subst),
        }
    }
}

fn subs_if_exists<K: UnifyKey, T: UnifyValue<Key=K, Value=T>>(ty: &T, subst: SymbolTable<K, T>) -> T {
    let mut ty = ty.clone();
    if let Some(type_id) = ty.unknown() {
        if subst.contains_key(&type_id) {
            ty = subst.get(&type_id).unwrap().clone();
        }
    }
    ty
}

fn unify<K: UnifyKey, T: UnifyValue<Key=K, Value=T>>(
    ty1: T,
    ty2: T,
    subst: SymbolTable<K, T>,
) -> (UnifyResult, SymbolTable<K, T>) {
    // substitute
    let ty1 = subs_if_exists(&ty1, subst.clone());
    let ty2 = subs_if_exists(&ty2, subst.clone());

    let u1 = ty1.unknown();
    let u2 = ty2.unknown();

    let (res, subst) = if u1.is_none() && u2.is_none() {
        // both are known
        let children1 = ty1.children();
        let children2 = ty2.children();

        if occurs_check(&ty1, &ty2) {
            (UnifyResult::OccursCheck, subst)
        } else if children1.len() == children2.len() {
            // if there are no children, this is just a simple type and we can compare
            // directory.  Otherwise, unify on the children
            if children1.len() == 0 {
                if ty1 == ty2 {
                    (UnifyResult::Ok, subst)
                } else {
                    (UnifyResult::NoSolution, subst)
                }
            } else {
                // iterate over children
                unify_eq(children1, children2, subst)
            }
        } else {
            (UnifyResult::MismatchShape, subst)
        }
    } else if u1.is_none() && u2.is_some() {
        // one unknown
        (UnifyResult::Ok, subst.insert(u2.unwrap(), ty1.clone()))
    } else if u1.is_some() && u2.is_none() {
        // one unknown
        (UnifyResult::Ok, subst.insert(u1.unwrap(), ty2.clone()))
    } else if u1.is_some() && u2.is_some() {
        // two unknowns, nothing to do
        if u1 == u2 {
            // if they are equal, then they still match
            (UnifyResult::Ok, subst)
        } else {
            (UnifyResult::Incomplete, subst)
        }
    } else {
        (UnifyResult::Ok, subst)
    };

    log::debug!("unify: {:?} :: {:?} :: {:?}", &ty1, &ty2, res);
    (res, subst)
}

fn unify_eq<K: UnifyKey, T: UnifyValue<Key=K, Value=T>>(
    sig1: Vec<T>,
    sig2: Vec<T>,
    mut subst: SymbolTable<K, T>,
) -> (UnifyResult, SymbolTable<K, T>) {
    // ensure we have the same shape
    let (res, subst) = if sig1.len() != sig2.len() {
        (UnifyResult::MismatchShape, subst)
    } else if sig1.len() == 0 {
        (UnifyResult::MismatchShape, subst)
    } else if sig1 == sig2 {
        (UnifyResult::Ok, subst)
    } else {
        let mut new_res = UnifyResult::Ok;
        for (a1, a2) in sig1.iter().zip(sig2.iter()) {
            let (res, new_subst) = unify(a1.clone(), a2.clone(), subst);
            match res {
                UnifyResult::Ok => {
                    subst = new_subst;
                }
                _ => {
                    new_res = res;
                    subst = new_subst;
                    break;
                }
            }
        }
        (new_res, subst)
    };

    log::debug!("unify_eq: {:?} :: {:?} :: {:?}", sig1, sig2, &res);

    (res, subst)
}

// Does ty1 occur in ty2?
fn occurs_check<K, T: UnifyValue<Key=K, Value=T>>(ty: &T, ty2: &T) -> bool {
    let children = ty2.children();
    // if ty1 occurs in any of a functions parameters
    let res = children.iter().any(|s| s == ty);
    log::debug!("occurs_check: {:?} :: {:?} => {}", ty, children, &res);
    res
}

fn unify_all<K: UnifyKey, T: UnifyValue<Key=K, Value=T>>(
    equations: Vec<Expr<K, T>>,
    mut subst: SymbolTable<K, T>,
) -> (Vec<Expr<K, T>>, SymbolTable<K, T>) {
    let mut out = vec![];
    for (i, eq) in equations.into_iter().enumerate() {
        let (res, s) = eq.unify(subst.clone());
        log::debug!("Unify[{}]: {:?} => {:?}", i, &eq, res);
        match res {
            UnifyResult::Ok => {
                subst = s;
            }
            _ => {
                log::error!("no unify on {:?}", &eq);
                out.push(eq);
            }
        }
    }

    for (k, v) in subst.iter() {
        log::debug!("subst: {:?}={:?}", k, v);
    }
    for v in out.iter() {
        log::debug!("Unsatisfied Eq: {:?}", v);
    }

    (out, subst)
}

pub fn unify_start<K: UnifyKey, T: UnifyValue<Key=K, Value=T>>(
    mut equations: Vec<Expr<K, T>>,
) -> (UnifyResult, SymbolTable<K, T>) {
    let mut subst = SymbolTable::default();
    let mut count = subst.size();
    let mut res = UnifyResult::Ok;

    loop {
        // Are we out of equations?
        if equations.len() == 0 {
            break;
        }

        let (results, s) = unify_all(equations, subst.clone());
        subst = s;
        equations = results;
        
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

    #[derive(Clone, Copy, PartialEq, Eq, Hash)]
    pub struct DefinitionId(pub usize);

    impl UnifyKey for DefinitionId {}

    impl From<usize> for DefinitionId {
        fn from(item: usize) -> Self {
            Self(item)
        }
    }

    impl fmt::Display for DefinitionId {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    impl fmt::Debug for DefinitionId {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum Type {
        Int,
        Float,
        Name(String),
        Seq(Vec<Type>),
        Var(DefinitionId),
    }

    impl UnifyValue for Type {
        type Key = DefinitionId;
        type Value = Type;
        fn unknown(&self) -> Option<DefinitionId> {
            match self {
                Type::Var(id) => Some(*id),
                _ => None,
            }
        }
        fn children(&self) -> Vec<Type> {
            match self {
                Seq(args) => args.clone(),
                _ => vec![],
            }
        }
        fn var(u: DefinitionId) -> Type {
            Type::Var(u)
        }
    }

    type ExprSeq = Vec<Expr<DefinitionId, Type>>;

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
        assert_eq!(
            true,
            occurs_check(&name("a"), &seq(vec![name("b"), name("a")]))
        );
        assert_eq!(false, occurs_check(&name("a"), &seq(vec![name("b")])));
        assert_eq!(false, occurs_check(&name("a"), &seq(vec![])));

        // check occurs using an unknown
        let x = var(0);
        let s = seq(vec![x.clone(), x.clone()]);
        assert_eq!(true, occurs_check(&x, &s));
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
        let start: ExprSeq = vec![Eq(name("a"), name("b"))];

        let (res, _) = unify_start(start);

        // no solution, because types do not match
        assert_eq!(UnifyResult::NoSolution, res);
    }

    #[test]
    fn logic_no_solution_2() {
        let start: ExprSeq = vec![Eq(
            seq(vec![name("a"), name("b")]),
            seq(vec![name("c"), name("d")]),
        )];

        let (res, _) = unify_start(start);
        // no solution, because types do not match
        assert_eq!(UnifyResult::NoSolution, res);
    }

    #[test]
    fn logic_no_solution_3() {
        let start: ExprSeq = vec![
            Eq(Type::Int, Type::Float),
            OneOf(Type::Int, vec![Type::Float]),
        ];

        let (res, _) = unify_start(start);
        // no solution, because types do not match
        assert_eq!(UnifyResult::NoSolution, res);
    }

    #[test]
    fn logic_func() {
        let start: ExprSeq = vec![
            Eq(Type::Seq(vec![Type::Int]), Type::Seq(vec![var(0)])),
            Eq(var(1), Type::Float),
        ];

        let (res, _) = unify_start(start);
        // no solution, because types do not match
        assert_eq!(UnifyResult::Ok, res);
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

        let start: ExprSeq = vec![Eq(s1, s2)];

        let (res, _) = unify_start(start);
        assert_eq!(UnifyResult::Ok, res);
    }

    #[test]
    fn logic_or() {
        let eqs = vec![
            Or(vec![
                Eq(name("a"), name("b")),
                Eq(var(0), var(1)),
                Eq(var(2), name("c")),
            ]),
            Eq(name("b"), var(1)),
        ];
        let (res, _) = unify_start(eqs);
        assert_eq!(UnifyResult::Ok, res);
    }

    #[test]
    fn empty() {
        let (res, _) = unify_start::<DefinitionId, Type>(vec![]);
        assert_eq!(UnifyResult::Ok, res);
    }

    #[test]
    fn zebra_1() {
        let houses = seq(vec![var(0), var(1), var(2)]);
        let italian = seq(vec![name("italian"), var(3)]);
        let spanish = seq(vec![name("spanish"), var(4)]);
        let norwegian = seq(vec![name("norwegian"), name("blue")]);
        let red = seq(vec![var(5), name("red")]);

        let terms = vec![
            Eq(houses.clone(), var(6).into()),
            // italian lives in the second house
            Eq(var(1), italian),
            // spanish lives next to the red house
            OneOf(
                seq(vec![var(0), var(1), var(2)]),
                vec![
                    seq(vec![red.clone(), spanish.clone(), var(2)]),
                    seq(vec![var(0), red, spanish]),
                ],
            ),
            // norwegian is in one of the houses
            OneOf(norwegian, vec![var(0).into(), var(1).into(), var(2).into()]),
            // blue is already taken, but this algo isn't able to figure that out
            // there's no concept of exclusive here, so we could have duplicate
            // colors
            Or(vec![
                //Eq( var(3).into(), name("blue")),
                Eq(var(3).into(), name("orange")),
                Eq(var(3).into(), name("red")),
            ]),
            Or(vec![
                //Eq( var(4).into(), name("blue")),
                Eq(var(4).into(), name("orange")),
                Eq(var(4).into(), name("red")),
            ]),
        ];
        let (res, _) = unify_start(terms);
        assert_eq!(UnifyResult::Ok, res);
    }

    /*
    #[test]
    fn zebra_easy() {
        #[derive(Clone, Debug, PartialEq, Eq, Hash)]
        enum Color {
            Red, Blue, Orange
        }
        #[derive(Clone, Debug, PartialEq, Eq, Hash)]
        enum Language {
            Italian,
            Spanish,
            Norwegian
        }

        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        struct House {
            language: Box<Type>,
            color: Box<Type>
        }

        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum Type {
            Color(Color),
            Language(Language),
            House(House),
            Houses(Vec<Type>),
            Var(DefinitionId)
        }

        use Language::*;
        use Color::*;

        fn var(u: usize) -> Type {
            Type::Var(u.into())
        }

        impl TypeSignature<Type> for Type {
            fn unknown(&self) -> Option<DefinitionId> {
                match self {
                    Type::Var(id) => Some(*id),
                    _ => None
                }
            }
            fn children(&self) -> Vec<Type> {
                match self {
                    Type::Houses(houses) => houses.clone(),
                    _ => vec![]
                }
            }
            fn var(u: DefinitionId) -> Type {
                Type::Var(u)
            }
        }

        let houses = Type::Houses(vec![var(0).into(), var(1).into(), var(2).into()]);
        let italian = Type::House(House { language: Type::Language(Italian).into(), color: var(3).into() });
        let spanish = Type::House(House { language: Type::Language(Spanish).into(), color: var(4).into() });
        let red = Type::House(House { language: var(5).into(), color: Type::Color(Red).into() });
        let norwegian = Type::House(House { language: Type::Language(Norwegian).into(), color: Type::Color(Blue).into() });

        fn rightto(x: Type, y: Type, z: Type, s: SymbolTable<Type>) -> (UnifyResult, SymbolTable<Type>) {
            println!("func: {:?}", (&x, &y, &z));
            if let Type::Houses(hs) = z.clone() {
                if hs.len() < 2 {
                    (UnifyResult::NoSolution, s)
                } else {
                    let first = hs.get(0).unwrap().clone();
                    let second = hs.get(1).unwrap().clone();
                    let eqs = vec![
                        Eq(y, first),
                        Eq(x, second)
                    ];

                    //let (new_eqs, new_s) = unify_all(eqs, s.clone());
                    //if new_eqs.len() != eqs.len() {

                    //}
                    //(res, new_s)

                    //if first == &x && second == &y {
                        (UnifyResult::Ok, s)
                    //} else {
                        //let rest = Type::Houses(hs.iter().skip(1).cloned().collect::<Vec<_>>());
                        //rightto(hs.get(0).unwrap().clone(), hs.get(1).unwrap().clone(), z, s)
                    //}
                }
            } else {
                (UnifyResult::NoSolution, s)
            }
        }

        let terms = vec![
            Eq(houses.clone(), var(6).into()),

            // italian lives in the second house
            Eq(var(1).into(), italian),

            Or(vec![
                Eq( var(3).into(), Type::Color(Blue)),
                Eq( var(3).into(), Type::Color(Orange)),
                Eq( var(3).into(), Type::Color(Red)),
            ]),

            // spanish lives next to the red house
            Or(vec![
                Eq( Type::Houses(vec![var(0), var(1)]), Type::Houses(vec![red.clone(), spanish.clone()])),
                Eq( Type::Houses(vec![var(1), var(2)]), Type::Houses(vec![red, spanish]))
            ]),

            OneOf(norwegian, vec![var(0).into(), var(1).into(), var(2).into()]),
            //OneOf(spanish.clone(), vec![var(0).into(), var(1).into()]),
            //OneOf(spanish, vec![house1, house3]),
            //Func(spanish, red, houses, rightto)
        ];
        let (res, _) = unify_start(terms);
        assert_eq!(UnifyResult::Ok, res);


    }
    */
}
