use crate::tokens::{FileId, Tok};
use miette::SourceSpan;
use std::fmt;

#[derive(PartialEq, Clone)]
pub struct Location {
    pub offset: usize,
    pub line: usize,
    pub col: usize,
    pub fragment: String,
    pub file_id: FileId,
}

impl fmt::Debug for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Loc")
            .field("line", &self.line)
            .field("col", &self.col)
            .finish()
    }
}

impl Default for Location {
    fn default() -> Self {
        Self {
            offset: 0,
            line: 0,
            col: 0,
            fragment: "".into(),
            file_id: 0,
        }
    }
}

impl Location {
    pub fn new(offset: usize, line: usize, col: usize, fragment: String) -> Self {
        Self {
            offset,
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

    //pub fn span(&self) -> SourceSpan {
    //SourceSpan::new(self.offset, self.fragment.len())
    //}
}

#[derive(PartialEq, Clone)]
pub struct Surround {
    pub pre: Vec<Tok>,
    pub post: Vec<Tok>,
}

impl fmt::Debug for Surround {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Surround")
            .field("pre", &self.pre)
            .field("post", &self.post)
            .finish()
    }
}

impl Default for Surround {
    fn default() -> Self {
        Self {
            pre: vec![],
            post: vec![],
        }
    }
}

impl Surround {
    pub fn new(pre: Vec<Tok>, post: Vec<Tok>) -> Self {
        Self { pre, post }
    }

    pub fn prepend(&mut self, toks: Vec<Tok>) {
        if toks.len() > 0 {
            let mut v = toks;
            v.append(&mut self.pre);
            self.pre = v;
        }
    }

    pub fn append(&mut self, toks: Vec<Tok>) {
        if toks.len() > 0 {
            self.post.append(&mut toks.clone());
        }
    }

    pub fn unparse(&self, tokens: Vec<Tok>) -> Vec<Tok> {
        vec![self.pre.clone(), tokens, self.post.clone()]
            .into_iter()
            .flatten()
            .collect()
    }
}
