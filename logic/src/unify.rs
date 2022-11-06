use std::fmt;
pub type SymbolTable<K, T> = rpds::HashTrieMap<K, T>;

pub trait UnifyKey: Clone + Copy + PartialEq + std::cmp::Eq + std::hash::Hash + fmt::Debug + fmt::Display {}
pub trait UnifyType<K: UnifyKey>: Clone + PartialEq + std::cmp::Eq + std::hash::Hash + fmt::Debug + fmt::Display {
    fn children(&self) -> Vec<Self>;
    fn try_unknown(&self) -> Option<K>;
}

pub trait UnifyValue: Clone + fmt::Debug  {
    type Key;
    type Type;
    fn new_type(ty: Self::Type) -> Self;
    fn new_unknown(v_id: Self::Key, ty: Self::Type) -> Self;
    fn get_type(&self) -> Self::Type;
    //fn children(&self) -> Vec<Self::Type>;
    fn unknown(&self) -> Option<Self::Key>;
    fn try_type(&self) -> Option<Self::Type>;
}

#[derive(Debug, PartialEq)]
pub enum UnifyResult {
    NoSolution,
    MismatchShape,
    OccursCheck,
    Incomplete,
    Ok,
}

use self::Expr::*;

#[derive(Clone, Debug, Hash)]
pub enum Expr<K, T, V> {
    OneOfValues(V, Vec<V>),
    OneOfTypes(T, Vec<T>),
    Eq(T, T),
    _Unreachable(PhantomData<K>)
}

impl <K: UnifyKey, T: UnifyType<K>, V: UnifyValue<Key=K, Type=T>> Expr<K, T, V> {
    fn unify(&self, subst: SymbolTable<K, V>) -> (UnifyResult, SymbolTable<K, V>) {
        match self {
            Eq(ty1, ty2) => Unify::unify_eq(vec![ty1.clone()], vec![ty2.clone()], subst),
            OneOfTypes(ty1, types) => {
                let s = subst;
                for ty2 in types {
                    match Unify::unify_type(ty1.clone(), ty2.clone(), s.clone()) {
                        (UnifyResult::Ok, s) => {
                            return (UnifyResult::Ok, s);
                        }
                        _ => (),
                    }
                }
                (UnifyResult::NoSolution, s)
            }
            OneOfValues(value, possibilities) => {
                let mut s = subst.clone();
                let mut result = UnifyResult::NoSolution;
                for p in possibilities {
                    match Unify::unify_value(value.clone(), p.clone(), s.clone()) {
                        (UnifyResult::Ok, new_s) => {
                            s = new_s;
                            result = UnifyResult::Ok;
                            break;
                        }
                        _ => (),
                    }
                }
                (result, s)
            }
            _ => unimplemented!(),
        }
    }
}

use std::marker::PhantomData;

#[derive(Default)]
struct Unify<K, T, V> {
    _k: PhantomData<K>,
    _t: PhantomData<T>,
    _v: PhantomData<V>,
}

impl<K: UnifyKey, T: UnifyType<K>, V: UnifyValue<Key=K, Type=T>> Unify<K, T, V> {
    fn subs_type(ty: &T, subst: SymbolTable<K, V>) -> T {
        let mut ty = ty.clone();
        if let Some(type_id) = ty.try_unknown() {
            if subst.contains_key(&type_id) {
                let v = subst.get(&type_id).unwrap().clone();
                if let Some(ty) = v.try_type() {
                    return ty
                } else {
                    unreachable!()
                }
            }
        }
        ty
    }

    fn subs_value(v: &V, subst: SymbolTable<K, V>) -> V {
        if let Some(v_id) = v.unknown() {
            let ty = v.get_type();
            let ty = Self::subs_type(&ty, subst);
            V::new_unknown(v_id, ty)
        } else {
            v.clone()
        }
    }


    fn unify_value(
        v1: V,
        v2: V,
        subst: SymbolTable<K, V>,
    ) -> (UnifyResult, SymbolTable<K, V>) {
        let v1 = Self::subs_value(&v1, subst.clone());
        let v2 = Self::subs_value(&v2, subst.clone());
        let u1 = v1.unknown();
        let ty1 = v1.get_type();
        let ty2 = v2.get_type();

        // if v1 is unknown and the type of v2 matches, then we unify the value to be the same
        // otherwise it's not a match

        if u1.is_some() && ty1 == ty2 {
            (UnifyResult::Ok, subst.insert(u1.unwrap(), v2))
        } else {
            (UnifyResult::NoSolution, subst)
        }
    }

    fn unify_type(
        ty1: T,
        ty2: T,
        subst: SymbolTable<K, V>,
    ) -> (UnifyResult, SymbolTable<K, V>) {
        // substitute
        let ty1 = Self::subs_type(&ty1, subst.clone());
        let ty2 = Self::subs_type(&ty2, subst.clone());

        let u1 = ty1.try_unknown();
        let u2 = ty2.try_unknown();

        let (res, subst) = if u1.is_none() && u2.is_none() {
            // both are known
            let children1 = ty1.children();
            let children2 = ty2.children();

            if Self::occurs_check(&ty1, &ty2) {
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
                    Self::unify_eq(children1, children2, subst)
                }
            } else {
                (UnifyResult::MismatchShape, subst)
            }
        } else if u1.is_none() && u2.is_some() {
            // one unknown
            (UnifyResult::Ok, subst.insert(u2.unwrap(), V::new_type(ty1.clone())))
        } else if u1.is_some() && u2.is_none() {
            // one unknown
            (UnifyResult::Ok, subst.insert(u1.unwrap(), V::new_type(ty2.clone())))
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

    fn unify_eq(
        sig1: Vec<T>,
        sig2: Vec<T>,
        mut subst: SymbolTable<K, V>,
    ) -> (UnifyResult, SymbolTable<K, V>) {
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
                let (res, new_subst) = Self::unify_type(a1.clone(), a2.clone(), subst);
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
    fn occurs_check(ty: &T, ty2: &T) -> bool {
        let children = ty2.children();
        // if ty1 occurs in any of a functions parameters
        let res = children.iter().any(|s| s == ty);
        log::debug!("occurs_check: {:?} :: {:?} => {}", ty, children, &res);
        res
    }

    fn unify_all(
        equations: Vec<Expr<K, T, V>>,
        mut subst: SymbolTable<K, V>,
    ) -> (Vec<Expr<K, T, V>>, SymbolTable<K, V>) {
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
        if subst.size() == 0 {
            log::debug!("no subs");
        }
        for v in out.iter() {
            log::debug!("Unsatisfied Eq: {:?}", v);
        }

        (out, subst)
    }

    pub fn start(
        mut equations: Vec<Expr<K, T, V>>,
    ) -> (UnifyResult, SymbolTable<K, V>) {
        let mut subst = SymbolTable::default();
        let mut count = subst.size();
        let mut res = UnifyResult::Ok;

        loop {
            // Are we out of equations?
            if equations.len() == 0 {
                break;
            }

            log::debug!("Equations remaining: {}", equations.len());

            let (results, s) = Self::unify_all(equations, subst.clone());
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
}

#[cfg(test)]
mod test {
    use super::*;
    use test_log::test;

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

    #[derive(Debug, Clone, PartialEq)]
    pub enum Value {
        Int(u64),
        Float(f64),
        String(String),
        TypeValue(Type),
        Unknown(DefinitionId, Type),
    }
    impl UnifyValue for Value {
        type Key = DefinitionId;
        type Type = Type;
        fn new_type(ty: Type) -> Self {
            Self::TypeValue(ty)
        }
        fn new_unknown(v_id: DefinitionId, ty: Type) -> Self {
            Self::Unknown(v_id, ty)
        }
        fn try_type(&self) -> Option<Self::Type> {
            if let Self::TypeValue(ty) = self {
                Some(ty.clone())
            } else {
                None
            }
        }
        fn get_type(&self) -> Self::Type {
            match self {
                Self::Int(_) => Type::Int,
                Self::Float(_) => Type::Float,
                Self::String(_) => Type::TString,
                Self::TypeValue(_) => Type::Type,
                Self::Unknown(_,ty) => ty.clone()
            }
        }
        //fn children(&self) -> Vec<Self::Type> {
            //vec![]
        //}

        fn unknown(&self) -> Option<Self::Key> {
            if let Self::Unknown(v_id, _) = self {
                Some(*v_id)
            } else {
                None
            }
        }
    }

    impl From<Type> for Value {
        fn from(item: Type) -> Self {
            Self::TypeValue(item)
        }
    }

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub enum Type {
        Int,
        Float,
        TString,
        Type,
        Name(String),
        Seq(Vec<Type>),
        Var(DefinitionId),
    }
    impl fmt::Display for Type {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:?}", self)
        }
    }

    impl UnifyType<DefinitionId> for Type {
        fn try_unknown(&self) -> Option<DefinitionId> {
            match self {
                Type::Var(id) => Some(*id),
                _ => None,
            }
        }
        fn children(&self) -> Vec<Type> {
            match self {
                Type::Seq(args) => args.clone(),
                _ => vec![],
            }
        }
    }

    type ExprSeq = Vec<Expr<DefinitionId, Type, Value>>;

    fn var(u: usize) -> Type {
        Type::Var(u.into())
    }

    fn seq(args: Vec<Type>) -> Type {
        Type::Seq(args)
    }

    fn name(s: &str) -> Type {
        Type::Name(s.into())
    }

    fn occurs_check(ty: &Type, ty2: &Type) -> bool {
        Unify::<DefinitionId, Type, Value>::occurs_check(ty, ty2)
    }

    fn start(eqs: ExprSeq) -> (UnifyResult, SymbolTable<DefinitionId, Value>) {
        Unify::<DefinitionId, Type, Value>::start(eqs)
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
        let x = var(8);
        let y = var(13);
        let eqs = vec![
            Eq(x, var(7)),
            Eq(var(0), Type::Int),
            Eq(var(1), var(0)),
            Eq(var(6), var(1)),
            Eq(var(7), var(6)),
            Eq(seq(vec![name("a"), name("b")]), seq(vec![var(2), var(3)])),
            Eq(seq(vec![name("c"), var(4)]), seq(vec![var(5), name("d")])),
            Eq(var(10), Type::Float),
            Eq(var(11), name("w")),
            OneOfTypes(var(9), vec![var(10).into(), var(11).into()]),
            OneOfTypes(name("y"), vec![var(12)]),
            OneOfTypes(name("z"), vec![var(13), var(14)]),
            Eq(var(14), name("x")),
        ];

        let (res, subst) = start(eqs);
        assert_eq!(UnifyResult::Ok, res);
        assert_eq!(subst.get(&8.into()).unwrap().try_type().unwrap(), Type::Int);
        assert_eq!(subst.get(&13.into()).unwrap().try_type().unwrap(), name("z"));
    }

    #[test]
    fn logic_unify_values() {
        // x is an unknown int
        let x = Value::Unknown(0.into(), Type::Int);
        let int_v = Value::Int(1);
        let float_v = Value::Float(1.);

        let eqs: ExprSeq = vec![
            OneOfValues(x.clone(), vec![int_v.clone(), float_v])
        ];

        let (res, subst) = start(eqs);

        // no solution, because types do not match
        assert_eq!(UnifyResult::Ok, res);
        let x_after = subst.get(&0.into()).unwrap();
        assert_eq!(x_after, &int_v);
    }

    #[test]
    fn logic_no_solution() {
        let start: ExprSeq = vec![Eq(name("a"), name("b"))];

        let (res, _) = Unify::<DefinitionId, Type, Value>::start(start);

        // no solution, because types do not match
        assert_eq!(UnifyResult::NoSolution, res);
    }

    #[test]
    fn logic_no_solution_2() {
        let start: ExprSeq = vec![Eq(
            seq(vec![name("a"), name("b")]),
            seq(vec![name("c"), name("d")]),
        )];

        let (res, _) = Unify::<DefinitionId, Type, Value>::start(start);
        // no solution, because types do not match
        assert_eq!(UnifyResult::NoSolution, res);
    }

    #[test]
    fn logic_no_solution_3() {
        let start: ExprSeq = vec![
            Eq(Type::Int, Type::Float),
            OneOfValues(Value::new_type(Type::Int), vec![Value::new_type(Type::Float)]),
        ];

        let (res, _) = Unify::<DefinitionId, Type, Value>::start(start);
        // no solution, because types do not match
        assert_eq!(UnifyResult::NoSolution, res);
    }

    #[test]
    fn logic_func() {
        let start: ExprSeq = vec![
            Eq(Type::Seq(vec![Type::Int]), Type::Seq(vec![var(0)])),
            Eq(var(1), Type::Float),
        ];

        let (res, _) = Unify::<DefinitionId, Type, Value>::start(start);
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

        let (res, _) = Unify::<DefinitionId, Type, Value>::start(start);
        assert_eq!(UnifyResult::Ok, res);
    }


    #[test]
    fn empty() {
        let (res, _) = Unify::<DefinitionId, Type, Value>::start(vec![]);
        assert_eq!(UnifyResult::Ok, res);
    }

}
