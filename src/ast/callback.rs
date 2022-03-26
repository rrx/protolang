use crate::eval::*;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct CallTable<'a> {
    funcs: im::HashMap<String, Callback<'a>>,
}

impl<'a> CallTable<'a> {
    pub fn new() -> Self {
        Self {
            funcs: im::HashMap::new(),
        }
    }

    pub fn add(&mut self, name: String, cb: Callback<'a>) -> Self {
        self.funcs.insert(name, cb);
        self.clone()
    }

    pub fn get(&self, name: &str) -> Option<&Callback<'a>> {
        self.funcs.get(name)
    }

    pub fn call(
        &mut self,
        name: &str,
        args: Vec<ExprRef>,
        env: Environment<'a>,
    ) -> Result<ExprRefWithEnv<'a>, InterpretError> {
        match self.get(name) {
            Some(cb) => {
                let result = cb(env, args)?;
                Ok(result)
            }
            _ => unreachable!(),
        }
    }
}

pub type CallbackFn<'a> =
    dyn Fn(Environment<'a>, Vec<ExprRef>) -> Result<ExprRefWithEnv<'a>, InterpretError> + 'static;

#[derive(Clone)]
pub struct Callback<'a> {
    cb: Rc<CallbackFn<'a>>,
    arity: usize,
}

impl<'a> Callback<'a> {
    pub fn new<'b, F>(f: F) -> Callback<'a>
    where
        F: Fn(Environment<'a>, Vec<ExprRef>) -> Result<ExprRefWithEnv<'a>, InterpretError>
            + 'static,
    {
        Callback {
            cb: Rc::new(f),
            arity: 0,
        }
    }
}

impl<'a> fmt::Debug for Callback<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<callback>")
    }
}

impl<'a> std::ops::Deref for Callback<'a> {
    type Target = Rc<CallbackFn<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.cb
    }
}

impl<'a> std::ops::DerefMut for Callback<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.cb
    }
}
