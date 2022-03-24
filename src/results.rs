//use miette::{Diagnostic, SourceSpan, NamedSource, Report};
use thiserror::Error;
use crate::ast::{MaybeNodeContext};

use std::collections::{HashSet, HashMap};
use std::path::Path;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};


//#[derive(Error, Debug, Diagnostic)]
//#[error("oops!")]
//#[diagnostic(code(oops::my::bad), url("https://example.com"), help("HELP"))]
//#[derive(Error, Debug)]
//pub struct ReportError {
    //#[source_code]
    //pub src: NamedSource,
    //#[related]
    //others: Vec<LangError>
//}
//impl ReportError {
    //pub fn new(filename: String, source: String, results: Vec<LangError>) -> Self{
        //Self { src: NamedSource::new(filename, source), others: results }
    //}
//}

//#[derive(Error, Debug, Diagnostic)]
//#[error("oops!")]
//#[diagnostic(code(oops::my::bad), url("https://example.com"), help("HELP"))]
//#[derive(Error, Debug)]
//pub struct ReplError {
    //#[source_code]
    //pub source_code: String,
    //#[related]
    //others: Vec<LangError>
//}
//impl ReplError {
    //pub fn new(filename: String, source: String, results: Vec<LangError>) -> Self{
        //Self { source_code: source, others: results }
    //}
//}

//#[derive(Error, Debug, Diagnostic)]
//#[error("oops!")]
//#[diagnostic()]
#[derive(Error, Debug)]
pub enum LangError {
    #[error("Warning: {0}")]
    Warning(String, MaybeNodeContext),
    #[error("Error: {0}")]
    Error(String, MaybeNodeContext)
}

type FileId = usize;

impl LangError {
    pub fn warning(message: String, context: MaybeNodeContext) -> Self {
        Self::Warning(message, context)
    }
    pub fn error(message: String, context: MaybeNodeContext) -> Self {
        Self::Error(message, context)
    }

    pub fn diagnostic(&self, file_id: FileId) -> Diagnostic<FileId> {
        match self {
            Self::Warning(msg, context) | Self::Error(msg, context) => {
                Diagnostic::warning()
                    .with_message(msg)
                    .with_labels(vec![
                                 Label::primary(file_id, context.range())
                    ])
            }
        }
    }
}

pub struct Results {
    diagnostics: Vec<Diagnostic<FileId>>,
    files: SimpleFiles<String, String>
}

impl Results {
    pub fn new() -> Self {
        Self { diagnostics: vec![], files: SimpleFiles::new() }
    }
    
    pub fn add_source(&mut self, filename: String, source: String) -> FileId {
        self.files.add(filename, source)
    }

    pub fn print(&self) {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();
        for diagnostic in self.diagnostics.iter() {
            term::emit(&mut writer.lock(), &config, &self.files, &diagnostic);
        }
    }

}
impl std::ops::Deref for Results {
    type Target = Vec<Diagnostic<FileId>>;

    fn deref(&self) -> &Self::Target {
        &self.diagnostics
    }
}

impl std::ops::DerefMut for Results {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.diagnostics
    }
}

#[derive(Debug, Error)]
pub struct InterpretError {
    pub context: MaybeNodeContext,
    pub kind: InterpretErrorKind
}

impl std::fmt::Display for InterpretError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Error)]
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
            context: MaybeNodeContext::default()
        }
    }

    pub fn diagnostic(&self, file_id: FileId) -> Diagnostic<FileId> {
        match &self.kind {
            InterpretErrorKind::Runtime(message) => {
                Diagnostic::error()
                    .with_message(message)
                    .with_labels(vec![
                                 Label::primary(file_id, self.context.range())
                    ])
            }
            _ => {
                Diagnostic::error()
                    .with_message(format!("{}", self))
                    .with_labels(vec![
                                 Label::primary(file_id, self.context.range())
                    ])
            }
        }
    }
}

impl MaybeNodeContext {
    pub fn runtime_error(&self, m: &str) -> InterpretError {
        InterpretError { kind: InterpretErrorKind::Runtime(m.to_string()), context: self.clone() }
    }
    pub fn error(&self, kind: InterpretErrorKind) -> InterpretError {
        InterpretError { kind, context: self.clone() }
    }
}

