use log::debug;
use rpds::HashTrieMap;
use std::fmt;
use std::hash::Hash;
use std::rc::Rc;

pub trait LayerKey: Hash + Eq + fmt::Display + fmt::Debug + Clone {}
pub trait LayerValue: fmt::Debug + Clone {}

#[derive(Clone, PartialEq)]
pub struct Layer<K: LayerKey, V> {
    values: HashTrieMap<K, V>,
}

impl<K: LayerKey, V> Default for Layer<K, V> {
    fn default() -> Self {
        Self {
            values: HashTrieMap::new(),
        }
    }
}

impl<K: LayerKey, V: LayerValue> fmt::Debug for Layer<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.values.iter().map(|(k, v)| (k, v)))
            .finish()
    }
}

impl<K: LayerKey, V> Layer<K, V> {
    pub fn define(&self, name: K, value: V) -> Layer<K, V> {
        Layer {
            values: self.values.insert(name, value),
        }
    }

    pub fn contains(&self, name: &K) -> bool {
        self.values.contains_key(name)
    }

    pub fn get(&self, name: &K) -> Option<&V> {
        match self.values.get(name) {
            Some(v) => Some(v),
            None => None,
        }
    }
}

#[derive(Debug)]
pub struct Registry<T, I> {
    v: Vec<T>,
    _i: I,
}

impl<T, I: Index> Registry<T, I> {
    pub fn new() -> Self {
        Self {
            v: vec![],
            _i: I::create(0),
        }
    }

    pub fn add(&mut self, v: T) -> I {
        let inx = self.v.len();
        self.v.push(v);
        I::create(inx)
    }

    pub fn replace(&mut self, id: &I, v: T) {
        self.v[id.inx()] = v;
    }

    pub fn get(&self, inx: &I) -> &T {
        self.v.get(inx.inx()).unwrap()
    }

    pub fn get_mut(&mut self, inx: &I) -> &mut T {
        self.v.get_mut(inx.inx()).unwrap()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnvLayers<K: LayerKey, V: LayerValue> {
    enclosing: Option<Box<EnvLayers<K, V>>>,
    layers: im::vector::Vector<Layer<K, V>>,
}

impl<K: LayerKey, V: LayerValue> Default for EnvLayers<K, V> {
    fn default() -> Self {
        let mut layers = im::Vector::new();
        let layer = Layer::default();
        layers.push_front(layer);
        Self {
            layers,
            enclosing: None,
        }
    }
}

pub struct EnvLayersIterator<'a, K, V> {
    values: Vec<(&'a K, &'a V)>,
}

impl<'a, K, V> Iterator for EnvLayersIterator<'a, K, V> {
    type Item = (&'a K, &'a V);
    fn next(&mut self) -> Option<Self::Item> {
        self.values.pop()
    }
}

impl<K: LayerKey, V: LayerValue> EnvLayers<K, V> {
    pub fn iter<'a>(&'a self) -> EnvLayersIterator<'a, K, V> {
        let mut values = vec![];
        for layer in &self.layers {
            for (k, v) in layer.values.iter() {
                values.push((k, v));
            }
        }
        EnvLayersIterator { values }
    }

    pub fn push(&mut self) {
        let enclosing = Some(Box::new(self.clone()));
        let layers = im::Vector::new();
        self.enclosing = enclosing;
        self.layers = layers;
    }

    pub fn pop(&mut self) {
        let orig = self.enclosing.take().unwrap();
        self.enclosing = orig.enclosing;
        self.layers = orig.layers;
    }

    pub fn define(&mut self, name: K, value: V) {
        if self.layers.len() > 0 && self.layers.front().unwrap().contains(&name) {
            let layer = Layer::default().define(name, value);
            self.layers.push_front(layer);
        } else {
            match self.layers.pop_front() {
                Some(layer) => {
                    let layer = layer.define(name, value);
                    self.layers.push_front(layer);
                }
                None => {
                    let layer = Layer::default().define(name, value);
                    self.layers.push_front(layer);
                }
            }
        }
    }

    pub fn resolve(&self, name: &K) -> Option<&V> {
        self.layers
            .iter()
            .find(|layer| layer.values.contains_key(name))
            .map(|layer| layer.get(name))
            .flatten()
    }

    pub fn resolve_all(&self, name: &K) -> Vec<&V> {
        self.layers
            .iter()
            .filter(|layer| layer.values.contains_key(name))
            .map(|layer| layer.get(name).unwrap())
            .collect()
    }

    pub fn debug(&self) {
        self.layers.iter().enumerate().for_each(|(i, layer)| {
            debug!("Layer: {:?}", i);
            layer.values.iter().for_each(|(k, v)| {
                debug!("\t{}: {:?}", k, v);
            });
        })
    }
}

pub type Ident = String;
impl LayerKey for Ident {}

pub trait Index {
    fn create(x: usize) -> Self;
    fn inx(&self) -> usize;
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct TypeId(usize);
impl LayerValue for TypeId {}
impl Index for TypeId {
    fn create(x: usize) -> Self {
        Self(x)
    }
    fn inx(&self) -> usize {
        self.0
    }
}
impl fmt::Display for TypeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TypeId({})", self.0)
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct ValueId(usize);
impl LayerValue for ValueId {}
impl Index for ValueId {
    fn create(x: usize) -> Self {
        Self(x)
    }
    fn inx(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
struct Environment {
    types: EnvLayers<Ident, TypeId>,
    values: EnvLayers<Ident, ValueId>,
}

#[derive(Debug)]
struct Type(usize);
#[derive(Debug)]
struct Value(usize);

#[derive(Debug)]
struct Registries {
    types: Registry<Type, TypeId>,
    values: Registry<Value, ValueId>,
}
impl Registries {
    fn new() -> Self {
        Self {
            types: Registry::new(),
            values: Registry::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;

    #[test]
    fn lexical_scope() {
        let mut r = Registries::new();
        let t1 = Type(1);
        let t2 = Type(2);
        let v = Value(2);
        let mut env = Environment::default();
        let v_id = r.values.add(v);
        env.values.define("asdf".into(), v_id);
        let t1_id = r.types.add(t1);
        let t2_id = r.types.add(t2);
        env.types.define("Asdf".into(), t1_id.clone());
        assert_eq!(Some(&t1_id), env.types.resolve(&"Asdf".into()));
        env.types.define("Asdf".into(), t2_id.clone());
        assert_eq!(Some(&t2_id), env.types.resolve(&"Asdf".into()));
        println!("{:?}", (&env));

        let all_asdf = env.types.resolve_all(&"Asdf".to_string());
        assert_eq!(all_asdf.len(), 2);

        let env1 = env.clone();
        let env2 = env1.clone();
        let mut env3 = env2.clone();
        env3.types.define("Asdf2".into(), t1_id.clone());
        assert_eq!(env1, env2);
        assert!(env1 != env3);
        assert_eq!(Some(&t1_id), env3.types.resolve(&"Asdf2".into()));
        assert_eq!(None, env.types.resolve(&"Asdf2".into()));
        assert_eq!(None, env2.types.resolve(&"Asdf2".into()));

        env3.types.iter().for_each(|(k, v)| {
            println!("x: {:?}", (k, v));
        });
    }

    #[test]
    fn push_pop() {
        let mut env = EnvLayers::default();
        env.push();
        env.define(String::from("asdf"), ValueId(0));
        assert_eq!(Some(&ValueId(0)), env.resolve(&"asdf".into()));
        env.pop();
        assert_eq!(None, env.resolve(&"asdf".into()));
    }
}
