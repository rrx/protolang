use crate::eval::*;
use std::fmt;
use std::rc::Rc;

// This was a great help in figuring out how to do callbacks.  The trick is to ensure that the
// function is 'static
// https://gist.github.com/aisamanra/da7cdde67fc3dfee00d3
//
#[derive(Clone, Debug)]
pub struct CallTable {
    funcs: im::HashMap<String, Callback>,
}

impl CallTable {
    pub fn new() -> Self {
        Self {
            funcs: im::HashMap::new(),
        }
    }

    pub fn add(&mut self, name: String, cb: Callback) -> Self {
        self.funcs.insert(name, cb);
        self.clone()
    }

    pub fn get(&self, name: &str) -> Option<&Callback> {
        self.funcs.get(name)
    }

    pub fn call(
        &mut self,
        name: &str,
        args: Vec<ExprRef>,
        env: Environment,
    ) -> Result<ExprRefWithEnv, InterpretError> {
        match self.get(name) {
            Some(cb) => {
                let result = cb(env, args)?;
                Ok(result)
            }
            _ => unreachable!(),
        }
    }
}

impl std::ops::Deref for CallTable {
    type Target = im::HashMap<String, Callback>;

    fn deref(&self) -> &Self::Target {
        &self.funcs
    }
}

pub type CallbackFn =
    dyn Fn(Environment, Vec<ExprRef>) -> Result<ExprRefWithEnv, InterpretError> + 'static;

#[derive(Clone)]
pub struct Callback {
    cb: Rc<CallbackFn>,
    arity: usize,
}

impl Callback {
    pub fn new<F>(f: F) -> Callback
    where
        F: Fn(Environment, Vec<ExprRef>) -> Result<ExprRefWithEnv, InterpretError> + 'static,
    {
        Callback {
            cb: Rc::new(f),
            arity: 0,
        }
    }
}

impl fmt::Debug for Callback {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<callback>")
    }
}

impl std::ops::Deref for Callback {
    type Target = Rc<CallbackFn>;

    fn deref(&self) -> &Self::Target {
        &self.cb
    }
}

impl std::ops::DerefMut for Callback {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.cb
    }
}