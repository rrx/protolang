use miette::{Diagnostic, SourceSpan, NamedSource, Report};
use thiserror::Error;
use crate::ast::{MaybeNodeContext};

use std::collections::{HashSet, HashMap};
use std::path::Path;


#[derive(Error, Debug, Diagnostic)]
#[error("oops!")]
#[diagnostic(code(oops::my::bad), url("https://example.com"), help("HELP"))]
pub struct ReportError {
    #[source_code]
    pub src: NamedSource,
    #[related]
    others: Vec<LangError>
}
impl ReportError {
    pub fn new(filename: String, source: String, results: Vec<LangError>) -> Self{
        Self { src: NamedSource::new(filename, source), others: results }
    }
}

#[derive(Error, Debug, Diagnostic)]
#[error("oops!")]
#[diagnostic(code(oops::my::bad), url("https://example.com"), help("HELP"))]
pub struct ReplError {
    #[source_code]
    pub source_code: String,
    #[related]
    others: Vec<LangError>
}
impl ReplError {
    pub fn new(filename: String, source: String, results: Vec<LangError>) -> Self{
        Self { source_code: source, others: results }
    }
}

#[derive(Error, Debug, Diagnostic)]
#[error("oops!")]
#[diagnostic()]
pub enum LangError {
    Warning(String, MaybeNodeContext),
    Error(String, MaybeNodeContext)
}

impl LangError {
    pub fn warning(message: String, context: MaybeNodeContext) -> Self {
        Self::Warning(message, context)
    }
    pub fn error(message: String, context: MaybeNodeContext) -> Self {
        Self::Error(message, context)
    }
}

