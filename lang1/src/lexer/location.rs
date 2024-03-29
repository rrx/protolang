use crate::results::{LangError, LangErrorKind};
use crate::tokens::{FileId, Span, Tokens, TokensList};
use std::fmt;

#[derive(PartialEq, Clone)]
pub struct Location {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub col: usize,
    pub fragment: String,
    pub file_id: FileId,
}

impl fmt::Debug for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Loc")
            .field("start", &self.start)
            .field("end", &self.end)
            .field("frag", &self.fragment)
            //.field("line", &self.line)
            //.field("col", &self.col)
            .finish()
    }
}

impl Default for Location {
    fn default() -> Self {
        Self {
            start: 0,
            end: 0,
            line: 0,
            col: 0,
            fragment: "".into(),
            file_id: 0,
        }
    }
}

impl Location {
    pub fn new(start: usize, end: usize, line: usize, col: usize, fragment: String) -> Self {
        Self {
            start,
            end,
            line,
            col,
            fragment,
            file_id: 0,
        }
    }

    pub fn set_file_id(mut self, file_id: FileId) -> Self {
        self.file_id = file_id;
        self
    }

    pub fn range(&self) -> std::ops::Range<usize> {
        self.start..self.end
    }

    pub fn from_tokens_position(start: &Tokens, end: &Tokens) -> Self {
        let mut loc = start.to_location();
        loc.end = end.to_location().end;
        loc
    }

    pub fn from_position(start: &Span, end: &Span) -> Self {
        Self {
            start: start.location_offset(),
            end: end.location_offset(),
            line: 0,
            col: 0,
            fragment: "".into(),
            file_id: start.extra.file_id,
        }
    }

    //pub fn span(&self) -> SourceSpan {
    //SourceSpan::new(self.offset, self.fragment.len())
    //}
    //

    pub fn runtime_error(&self, m: &str) -> LangError {
        LangError {
            kind: LangErrorKind::Runtime(m.to_string()),
            loc: self.clone(),
        }
    }

    pub fn into_error(self, kind: LangErrorKind) -> LangError {
        LangError { kind, loc: self }
    }

    pub fn error(&self, kind: LangErrorKind) -> LangError {
        self.clone().into_error(kind)
    }
}
