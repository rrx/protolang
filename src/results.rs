use crate::ast::{Expr, MaybeNodeContext};
use crate::eval::{Environment, ExprRefWithEnv};
use thiserror::Error;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

#[derive(Error, Debug)]
pub enum LangError {
    #[error("Warning: {0}")]
    Warning(String, MaybeNodeContext),
    #[error("Error: {0}")]
    Error(String, MaybeNodeContext),
}

impl LangError {
    pub fn warning(message: String, context: MaybeNodeContext) -> Self {
        Self::Warning(message, context)
    }
    pub fn error(message: String, context: MaybeNodeContext) -> Self {
        Self::Error(message, context)
    }
}

#[derive(Debug, Error, Clone)]
pub struct InterpretError {
    pub context: MaybeNodeContext,
    pub kind: InterpretErrorKind,
}

impl std::fmt::Display for InterpretError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Error, Clone)]
pub enum InterpretErrorKind {
    #[error("Invalid error")]
    Invalid,
    #[error("Runtime error: {0}")]
    Runtime(String),
    #[error("Expecting boolean")]
    ExpectBool,
    #[error("Assertion")]
    Assertion,
    #[error("Not Implemented")]
    NotImplemented,
}

impl InterpretError {
    pub fn runtime(m: &str) -> Self {
        Self {
            kind: InterpretErrorKind::Runtime(m.to_string()),
            context: MaybeNodeContext::default(),
        }
    }
}

impl MaybeNodeContext {
    pub fn runtime_error(&self, m: &str) -> InterpretError {
        InterpretError {
            kind: InterpretErrorKind::Runtime(m.to_string()),
            context: self.clone(),
        }
    }
    pub fn error(&self, kind: InterpretErrorKind) -> InterpretError {
        InterpretError {
            kind,
            context: self.clone(),
        }
    }
}
