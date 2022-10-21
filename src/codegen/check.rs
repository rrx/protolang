use crate::ir::*;

use std::fmt;

pub struct TypeChecker<N> {
    pub type_equations: Vec<TypeEquation<N>>,
    type_counter: usize,
}

impl<N> Default for TypeChecker<N> {
    fn default() -> Self {
        Self { type_equations: vec![], type_counter: 0 }
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


