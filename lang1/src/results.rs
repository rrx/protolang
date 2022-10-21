use crate::ast::{ExprNode, MaybeNodeContext};
use crate::lexer::Location;
use thiserror::Error;
use nom::InputIter;
use crate::tokens::{FileId, Tok, TokensList};

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

pub struct Compiler {
    results: Vec<LangError>,
    pub diagnostics: Vec<Diagnostic<FileId>>,
    files: SimpleFiles<String, String>,
    pub has_errors: bool,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            has_errors: false,
            results: vec![],
            diagnostics: vec![],
            files: SimpleFiles::new(),
        }
    }
}

impl Compiler {
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

    pub fn parse_str(
        &mut self,
        v: &str,
    ) -> anyhow::Result<ExprNode> {
        let file_id = self.add_source("<repl>".into(), v.to_string());
        self.parse(v, file_id)
    }

    pub fn parse(&mut self, s: &str, file_id: FileId) -> anyhow::Result<ExprNode> {
        let mut lexer = crate::lexer::LexerState::default().set_file_id(file_id);
        match lexer.lex(s) {
            Ok((_, tokens)) => {
                tokens.iter_elements().for_each(|t| {
                    if let Tok::Invalid(s) = &t.tok {
                        let error = t
                            .to_context()
                            .error(LangErrorKind::Warning(format!("Invalid Token: {}", s)));
                        self.push(error);
                    }
                });

                match crate::parser::parse_program(tokens) {
                    Ok((_, expr)) => Ok(expr),
                    Err(nom::Err::Error(e)) => {
                        for (tokens, err) in e.errors {
                            log::error!("error {:?}", (&err, tokens.toks()));
                        }
                        let error = LangError::runtime("Error parsing");
                        self.push(error.clone());
                        Err(error.into())
                    }
                    Err(e) => Err(LangError::runtime(&format!("Error parsing: {:?}", e)).into()),
                }
            }
            Err(e) => {
                return Err(LangError::runtime(&format!("Error lexing: {:?}", e)).into());
            }
        }
    }

    pub fn parse_file(&mut self, filename: &str) -> anyhow::Result<ExprNode> {
        let contents = std::fs::read_to_string(filename.clone())?.to_string();
        let file_id = self.add_source(filename.into(), contents.clone());
        self.parse(&contents, file_id)
    }

}
