use crate::tokens::Tok;

#[derive(PartialEq, Debug, Clone)]
pub struct Location {
    pub offset: usize,
    pub line: usize,
    pub col: usize,
    pub fragment: String,
}

impl Default for Location {
    fn default() -> Self {
        Self {
            offset: 0,
            line: 0,
            col: 0,
            fragment: "".into(),
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
        }
    }
}


#[derive(PartialEq, Debug, Clone)]
pub struct Linespace(pub usize, pub usize);
impl Default for Linespace {
    fn default() -> Self {
        Self(0, 0)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Surround {
    pub pre: Vec<Tok>,
    pub post: Vec<Tok>,
    pub has_trailing_linespace: bool,
    //pub has_preceding_linespace: bool,
    pub has_newline: bool,
    pub linespace: Linespace,
}

impl Default for Surround {
    fn default() -> Self {
        Self {
            pre: vec![],
            post: vec![],
            has_trailing_linespace: false,
            //has_preceding_linespace: false,
            has_newline: false,
            linespace: Linespace::default(),
        }
    }
}

impl Surround {
    pub fn new(pre: Vec<Tok>, post: Vec<Tok>) -> Self {
        let mut x = Self {
            pre,
            post,
            //has_preceding_linespace: false,
            has_trailing_linespace: false,
            has_newline: false,
            linespace: Linespace::default(),
        };
        x._update();
        x
    }

    fn _update(&mut self) {
        let mut has_trailing_linespace = false;
        let mut has_newline = false;
        let mut linespace_after = 0;
        for e in &self.post {
            if !has_newline {
                if e.is_indent() {
                } else if e.is_linespace() {
                    linespace_after += 1;
                } else if e.is_newline() {
                    has_newline = true;
                    if linespace_after > 0 {
                        has_trailing_linespace = true;
                    }
                    break;
                }
            }
        }
        self.has_trailing_linespace = has_trailing_linespace;
        self.has_newline = has_newline;

        //let mut has_preceding_linespace = false;
        let mut linespace_before = 0;
        for e in self.pre.iter().rev() {
            if e.is_indent() {
            } else if e.is_linespace() {
                linespace_before += 1;
            } else {
                break;
            }
        }

        //self.has_preceding_linespace = has_preceding_linespace;
        self.linespace = Linespace(linespace_before, linespace_after);
    }

    pub fn prepend(&mut self, toks: Vec<Tok>) {
        if toks.len() > 0 {
            let mut v = toks;
            v.append(&mut self.pre);
            self.pre = v;
            self._update();
        }
    }

    pub fn append(&mut self, toks: Vec<Tok>) {
        if toks.len() > 0 {
            self.post.append(&mut toks.clone());
            self._update();
        }
    }

    pub fn unparse(&self, tokens: Vec<Tok>) -> Vec<Tok> {
        vec![self.pre.clone(), tokens, self.post.clone()]
            .into_iter()
            .flatten()
            .collect()
    }
}


