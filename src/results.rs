use crate::tokens::FileId;
use crate::ast::{Expr, MaybeNodeContext};
use crate::eval::{Environment, ExprRefWithEnv};
use thiserror::Error;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};


#[derive(Error, Debug, Clone)]
pub enum LangErrorKind {
    #[error("Warning: {0}")]
    Warning(String),
    #[error("Error: {0}")]
    Error(String),
    #[error("Expecting boolean")]
    ExpectBool,
    #[error("Not Implemented")]
    NotImplemented,
    #[error("Not Found")]
    NotFound,
    #[error("Invalid")]
    Invalid,
    #[error("Invalid number of arguments")]
    InvalidNumberArgs,
}

#[derive(Debug, Error, Clone)]
pub struct LangError {
    pub context: MaybeNodeContext,
    pub kind: LangErrorKind,
}

impl std::fmt::Display for LangError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl LangError {
    pub fn warning(message: String, context: MaybeNodeContext) -> Self {
        Self {
            context,
            kind: LangErrorKind::Warning(message),
        }
    }
    pub fn error(message: String, context: MaybeNodeContext) -> Self {
        Self {
            context,
            kind: LangErrorKind::Error(message),
        }
    }

    pub fn diagnostic(&self) -> Diagnostic<FileId> {
        let file_id = self.context.get_file_id();
        match &self.kind {
            LangErrorKind::Warning(msg) | LangErrorKind::Error(msg) => Diagnostic::warning()
                .with_message(msg)
                .with_labels(vec![Label::primary(file_id, self.context.range())]),
            _ => Diagnostic::warning()
                .with_message(&format!("{}", &self))
                .with_labels(vec![Label::primary(file_id, self.context.range())]),
        }
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
    #[error("Analysis Failed")]
    AnalysisFailed,
}

impl InterpretError {
    pub fn runtime(m: &str) -> Self {
        Self {
            kind: InterpretErrorKind::Runtime(m.to_string()),
            context: MaybeNodeContext::default(),
        }
    }

    pub fn diagnostic(&self) -> Diagnostic<FileId> {
        let file_id = self.context.get_file_id();
        match &self.kind {
            InterpretErrorKind::Runtime(message) => Diagnostic::error()
                .with_message(message)
                .with_labels(vec![Label::primary(file_id, self.context.range())]),
            _ => Diagnostic::error()
                .with_message(format!("{}", self))
                .with_labels(vec![Label::primary(file_id, self.context.range())]),
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
    pub fn lang_error(&self, kind: LangErrorKind) -> LangError {
        LangError {
            kind,
            context: self.clone(),
        }
    }
}

pub struct CompileResults {
    pub diagnostics: Vec<Diagnostic<FileId>>,
    pub files: SimpleFiles<String, String>,
}

impl Default for CompileResults {
    fn default() -> Self {
        Self {
            diagnostics: vec![],
            files: SimpleFiles::new(),
        }
    }
}

impl CompileResults {
    pub fn add_source(&mut self, filename: String, source: String) -> FileId {
        self.files.add(filename, source)
    }

    pub fn print(&self) {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();
        for diagnostic in self.diagnostics.iter() {
            let _ = term::emit(&mut writer.lock(), &config, &self.files, &diagnostic);
        }
    }
}

