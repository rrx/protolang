use crate::ast::MaybeNodeContext;
use crate::lexer::Location;
use crate::tokens::FileId;
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

    // Interpret Errors
    #[error("Runtime error: {0}")]
    Runtime(String),
    #[error("Lexer Failed")]
    LexerFailed,
    #[error("Parser Failed")]
    ParserFailed,
    #[error("Analysis Failed")]
    AnalysisFailed,
    #[error("Assertion")]
    Assertion,
}

impl LangErrorKind {
    pub fn error(&self) -> LangError {
        LangError {
            loc: Location::default(),
            kind: self.clone(),
        }
    }
    pub fn into_error(self) -> LangError {
        LangError {
            loc: Location::default(),
            kind: self,
        }
    }
}

#[derive(Debug, Error, Clone)]
pub struct LangError {
    pub loc: Location,
    pub kind: LangErrorKind,
}

impl std::fmt::Display for LangError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl LangError {
    pub fn warning(message: String, loc: Location) -> Self {
        Self {
            loc,
            kind: LangErrorKind::Warning(message),
        }
    }
    pub fn error(message: String, loc: Location) -> Self {
        Self {
            loc,
            kind: LangErrorKind::Error(message),
        }
    }

    pub fn runtime(m: &str) -> Self {
        Self {
            kind: LangErrorKind::Runtime(m.to_string()),
            loc: Location::default(),
        }
    }

    pub fn diagnostic(&self) -> Diagnostic<FileId> {
        let file_id = self.loc.file_id;
        match &self.kind {
            LangErrorKind::Warning(msg)
            | LangErrorKind::Error(msg)
            | LangErrorKind::Runtime(msg) => Diagnostic::warning()
                .with_message(msg)
                .with_labels(vec![Label::primary(file_id, self.loc.range())]),
            _ => Diagnostic::warning()
                .with_message(&format!("{}", &self))
                .with_labels(vec![Label::primary(file_id, self.loc.range())]),
        }
    }
}

impl MaybeNodeContext {
    pub fn runtime_error(&self, m: &str) -> LangError {
        LangError {
            kind: LangErrorKind::Runtime(m.to_string()),
            loc: self.to_location(),
        }
    }
    pub fn error(&self, kind: LangErrorKind) -> LangError {
        LangError {
            kind,
            loc: self.to_location(),
        }
    }
}

pub struct CompileResults {
    results: Vec<LangError>,
    pub diagnostics: Vec<Diagnostic<FileId>>,
    files: SimpleFiles<String, String>,
    pub has_errors: bool,
}

impl Default for CompileResults {
    fn default() -> Self {
        Self {
            has_errors: false,
            results: vec![],
            diagnostics: vec![],
            files: SimpleFiles::new(),
        }
    }
}

impl CompileResults {
    pub fn clear(&mut self) {
        self.results.clear();
        self.diagnostics.clear();
        self.has_errors = false;
    }

    pub fn add_source(&mut self, filename: String, source: String) -> FileId {
        self.files.add(filename, source)
    }

    pub fn push(&mut self, r: LangError) {
        if let LangErrorKind::Warning(_) = r.kind {
        } else {
            self.has_errors = true;
        }

        self.results.push(r.clone());
        self.diagnostics.push(r.diagnostic());
    }

    pub fn print(&self) {
        let writer = StandardStream::stdout(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();
        for diagnostic in self.diagnostics.iter() {
            //println!("{:?}", diagnostic);
            term::emit(&mut writer.lock(), &config, &self.files, &diagnostic).unwrap();
        }
    }
}
