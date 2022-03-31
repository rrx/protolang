use crate::eval::*;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct CallWithType {
    pub f: Callback,
    pub t: TypeSig,
}

impl CallWithType {
    pub fn new(f: Callback, t: TypeSig) -> Self {
        Self { f, t }
    }
}

// This was a great help in figuring out how to do callbacks.  The trick is to ensure that the
// function is 'static
// https://gist.github.com/aisamanra/da7cdde67fc3dfee00d3
//
#[derive(Clone, Debug)]
pub struct CallTable {
    funcs: im::HashMap<String, CallWithType>,
}

impl CallTable {
    pub fn new() -> Self {
        Self {
            funcs: im::HashMap::new(),
        }
    }

    pub fn add(&mut self, name: String, cb: Callback, t: TypeSig) -> Self {
        self.funcs.insert(name, CallWithType::new(cb, t));
        self.clone()
    }

    pub fn get(&self, name: &str) -> Option<&CallWithType> {
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
                let result = (cb.f)(env, args)?;
                Ok(result)
            }
            _ => unreachable!(),
        }
    }
}

impl std::ops::Deref for CallTable {
    type Target = im::HashMap<String, CallWithType>;

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
