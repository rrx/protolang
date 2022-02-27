use std::fmt;

#[derive(PartialEq, Debug, Eq, Clone)]
pub enum S {
    Null,
    Atom(String),
    Cons(String, Vec<S>),
}

pub type SProgram = Vec<S>;

#[derive(PartialEq, Debug, Eq, Clone)]
pub enum SError {
    Invalid
}

pub type SResult<A> = Result<A, SError>;

impl fmt::Display for S {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            S::Null => write!(f, "()"),
            S::Atom(i) => write!(f, "{}", i),
            S::Cons(head, rest) => {
                write!(f, "({}", head)?;
                for s in rest {
                    write!(f, " {}", s)?
                }
                write!(f, ")")
            }
        }
    }
}
