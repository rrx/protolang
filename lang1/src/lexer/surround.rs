use crate::tokens::Tok;
use std::fmt;

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
