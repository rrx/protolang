use super::cps::Type;
use super::env::*;
use std::cell::{Ref, RefCell};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Debug)]
pub struct Registries {
    types: Registry<TypeSpec, TypeId>,
}

impl Registries {
    pub fn new() -> Self {
        Self {
            types: Registry::new(),
        }
    }

    fn add(&mut self) -> TypeId {
        self.types.add(TypeSpec {
            value: TypeSpecValue::Void,
            id: TypeId::create(0),
        })
    }
    pub fn add_type_composite(&mut self, c: CompositeTypeSpec) -> TypeId {
        let id = self.add();
        self.types
            .replace(&id, TypeSpec::new_composite(id.clone(), c));
        id
    }

    pub fn add_type_function(&mut self, c: FunctionSpec) -> TypeId {
        let id = self.add();
        self.types
            .replace(&id, TypeSpec::new_function(id.clone(), c));
        id
    }

    pub fn add_type_simple(&mut self, c: Type) -> TypeId {
        let id = self.add();
        self.types.replace(&id, TypeSpec::new_simple(id.clone(), c));
        id
    }

    pub fn type_is_recursive(&self, id: &TypeId) -> bool {
        self.types.get(id).is_recursive(id)
    }

    pub fn type_subtypes(&self, id: &TypeId) -> Vec<&TypeSpec> {
        self.types
            .get(id)
            .subtypes()
            .iter()
            .map(|t_id| self.types.get(t_id))
            .collect()
    }

    pub fn type_size(&self, id: &TypeId) -> Option<usize> {
        use std::mem::size_of;
        if self.type_is_recursive(id) {
            return None;
        }

        let t = self.types.get(id);
        match &t.value {
            TypeSpecValue::Simple(t) => match t {
                Type::TInt => Some(size_of::<i64>()),
                _ => unreachable!(),
            },
            TypeSpecValue::Composite(c) => {
                c.as_ref()
                    .borrow()
                    .sig
                    .iter()
                    .try_fold(0, |acc, (k, t_id)| {
                        //acc + self.type_size(t_id)
                        self.type_size(t_id).map(|x| x + acc)
                    })
            }
            TypeSpecValue::Function(_) => Some(size_of::<usize>()),
            TypeSpecValue::Void => Some(0),
        }
    }
}

#[derive(Debug, Clone, Default)]
struct Environment {
    types: EnvLayers<Ident, TypeId>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionSpec {
    sig: Vec<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CompositeTypeSpec {
    sig: im::OrdMap<String, TypeId>,
}
impl CompositeTypeSpec {
    pub fn new() -> Self {
        Self {
            sig: im::OrdMap::new(),
        }
    }

    pub fn set(&mut self, name: String, id: TypeId) {
        self.sig.insert(name, id);
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct TypeSpec {
    value: TypeSpecValue,
    id: TypeId,
}

#[derive(Clone, PartialEq, Eq)]
pub enum TypeSpecValue {
    Function(FunctionSpec),
    Simple(Type),
    Composite(Rc<RefCell<CompositeTypeSpec>>),
    Void,
}

impl fmt::Debug for TypeSpec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TypeSpecValue::*;
        match &self.value {
            Composite(c) => {
                write!(f, "TypeSpec({}, {:?})", self.id, c.as_ref().borrow().sig)
            }
            Function(t) => {
                write!(f, "TypeSpec({}, {:?})", self.id, t)
            }
            Simple(t) => {
                write!(f, "TypeSpec({}, {:?})", self.id, t)
            }
            Void => {
                write!(f, "TypeSpec({}, Void)", self.id)
            }
        }
    }
}

impl Hash for TypeSpec {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // the is unique
        self.id.hash(state)
        /*
        use TypeSpecValue::*;
        match &self.value {
            Function(c) => c.hash(state),
            Simple(c) => c.hash(state),
            Void => self.hash(state),
            Composite(c) => c.as_ref().borrow().hash(state),
        }
        */
    }
}

impl TypeSpec {
    fn new_composite(id: TypeId, c: CompositeTypeSpec) -> Self {
        TypeSpec {
            id,
            value: TypeSpecValue::Composite(Rc::new(RefCell::new(c))),
        }
    }

    fn new_function(id: TypeId, c: FunctionSpec) -> Self {
        TypeSpec {
            id,
            value: TypeSpecValue::Function(c),
        }
    }

    fn new_simple(id: TypeId, c: Type) -> Self {
        TypeSpec {
            id,
            value: TypeSpecValue::Simple(c.into()),
        }
    }

    pub fn set(&self, name: String, id: &TypeId) -> &Self {
        use TypeSpecValue::Composite;
        match &self.value {
            Composite(c) => {
                c.as_ref().borrow_mut().sig.insert(name, id.clone());
            }
            _ => unreachable!(),
        }
        self
    }

    fn is_recursive(&self, id: &TypeId) -> bool {
        let h = im::HashSet::new();
        let subs = self.gen_subtypes(h);
        subs.contains(&id)
    }

    fn subtypes(&self) -> Vec<TypeId> {
        let h = im::HashSet::new();
        let subs = self.gen_subtypes(h);
        subs.into_iter().collect::<Vec<_>>()
    }

    fn gen_subtypes(&self, mut h: im::HashSet<TypeId>) -> im::HashSet<TypeId> {
        match &self.value {
            TypeSpecValue::Composite(c) => {
                for (_, v) in c.as_ref().borrow().sig.iter() {
                    h.insert(v.clone());
                }
                h
            }
            _ => h,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;

    #[test]
    fn composite() {
        let mut r = Registries::new();
        let t1 = Type::TInt;
        let t1_inx = r.add_type_simple(t1);
        let c1_inx = r.add_type_composite(CompositeTypeSpec::new());
        r.types
            .get_mut(&c1_inx)
            .set("1".into(), &t1_inx)
            .set("2".into(), &c1_inx);
        assert!(r.types.get(&c1_inx).is_recursive(&c1_inx));
        assert!(!r.types.get(&t1_inx).is_recursive(&t1_inx));

        assert_eq!(None, r.type_size(&c1_inx));
        assert_eq!(Some(8), r.type_size(&t1_inx));

        println!("{:?}", r.types.get(&t1_inx).subtypes());
        println!("{:?}", r.types.get(&c1_inx).subtypes());
        assert_eq!(0, r.type_subtypes(&t1_inx).len());
        assert_eq!(2, r.type_subtypes(&c1_inx).len());
        println!("{:?}", r.type_subtypes(&t1_inx));
        println!("{:?}", r.type_subtypes(&c1_inx));
    }
}
