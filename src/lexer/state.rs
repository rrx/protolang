use super::*;
use crate::tokens::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum LexNext<'a> {
    Token(Token<'a>),
    Newline(Token<'a>),
    Space(Token<'a>),
    Tab(Token<'a>),
    EOF,
}

//#[derive(Debug, Clone)]
//pub struct LexNext<'a>(LexType, Option<Token<'a>>);

impl<'a> LexNext<'a> {
    fn linespace_count(&self) -> usize {
        match self {
            Self::Space(t) | Self::Tab(t) => {
                match t.tok {
                    Tok::Spaces(n) => n,
                    Tok::Tabs(n) => n,
                    _ => 0
                }
            }
            _ => 0
        }
    }
}

fn lex_next(i: Span) -> PResult<Span, LexNext> {
    context(
        "next",
        alt((
            map(lex_newline, |v| LexNext::Newline(v)),
            map(lex_space, |v| LexNext::Space(v)),
            map(lex_tab, |v| LexNext::Tab(v)),
            map(lex_token, |v| LexNext::Token(v)),
            //map(lex_token_eof, |t| LexNext(LexType::EOF, vec![t])),
        )),
    )(i)
}

#[derive(Debug)]
pub struct LexerState<'a> {
    acc: Vec<Token<'a>>,
    indent_size: usize,
    whitespace: Vec<Token<'a>>,
    trail: Option<LexNext<'a>>,
    indent_stack: Vec<Token<'a>>
}

impl<'a> Default for LexerState<'a> {
    fn default() -> Self {
        Self {
            acc: vec![],
            whitespace: vec![],
            indent_size: 0,
            trail: None,
            indent_stack: vec![],
        }
    }
}
impl<'a> LexerState<'a> {
    pub fn from_str(s: &'a str) -> Option<Self> {
        let mut lexer = Self::default();
        match lexer.lex_eof(s.into()) {
            Ok((rest, _)) => {
                if rest.len() > 0 {
                    println!("remaining {:?}", (&rest));
                }
                Some(lexer)
            }
            Err(nom::Err::Error(e)) => {
                for (tokens, err) in e.errors {
                    println!("error {:?}", (&err, tokens));
                }
                None
            }
            _ => unreachable!(),
        }
    }

    pub fn push_token(&mut self, mut token: Token<'a>) {
        println!("Push: {:?}", &token);
        token.indent = self.indent_size;
        token.s.prepend(
            self.whitespace
                .drain(..)
                .map(|v| v.toks())
                .flatten()
                .collect::<Vec<_>>(),
        );
        self.acc.push(token);
    }

    //pub fn toks(&mut self) -> Vec<Tok> {
    //vec![]
    //if self.whitespace.len() > 0 && self.acc.len() == 0 {
    //self.whitespace.iter().map(|t| {
    //let mut token = t.clone();
    //Tok::Invalid(t.tok.unlex())
    //token
    //}).collect::<Vec<_>>()
    //}
    //}

    pub fn final_toks(&mut self) -> Vec<Tok> {
        self.flush();
        vec![&self.acc, &self.whitespace]
            .into_iter()
            .flatten()
            .map(|v| v.toks())
            .flatten()
            .collect()
    }

    pub fn expand_toks(&mut self) -> Vec<Tok> {
        self.flush();
        vec![&self.acc, &self.whitespace]
            .into_iter()
            .flatten()
            .map(|v| v.expand_toks())
            .flatten()
            .collect()
    }

    fn flush(&mut self) {
        if self.acc.len() > 0 {
            let ws = self.drain_whitespace();
            self.acc.last_mut().unwrap().s.append(ws);
        }
    }

    fn drain_whitespace(&mut self) -> Vec<Tok> {
        self.whitespace
            .drain(..)
            .map(|v| v.toks())
            .flatten()
            .collect::<Vec<_>>()
    }

    pub fn tokens(&'a mut self) -> Tokens<'a> {
        self.flush();
        self.dump();
        Tokens::new(&self.acc[..])
    }

    pub fn _token_vec(&mut self) -> Vec<Token<'a>> {
        if self.acc.len() == 0 {
            self.whitespace.clone()
        } else {
            let token = self.acc.last_mut().unwrap();
            token.s.append(
                self.whitespace
                    .drain(..)
                    .map(|v| v.toks())
                    .flatten()
                    .collect::<Vec<_>>(),
            );
            self.acc.clone()
        }
    }

    pub fn dump(&self) {
        println!("State");
        println!("\tStack: {:?}", self.indent_stack);
        for token in &self.acc {
            println!("\tToken: {:?}", token);
        }
        for next in &self.trail {
            println!("\tTrail: {:?}", next);
        }
    }

    pub fn push(&mut self, next: LexNext<'a>) {
        use LexNext::*;
        match &self.trail {
            Some(Token(_)) => {
                match next {
                    // [T] + L -> [T] - Ident unchanged - no identation, T.post += L
                    Space(t) | Tab(t) => {
                        // we must have a token in acc
                        self.acc.last_mut().unwrap().s.append(vec![t.tok.clone()]);
                    }
                    // [T] + T -> [T] -> Indent unchanged - reset to last T
                    Token(t) => {
                        self.push_token(t);
                    }
                    // [T] + N -> [] -> Indent([]) - T.post += N, reset
                    Newline(t) => {
                        // push indent to stack and reset
                        //if self.indent_size > 0 {
                            //self.indent_stack.push(self.acc.last().unwrap().clone());//self.indent_size);
                        //}
                        self.indent_size = 0;
                        
                        // append whitespace to the last token
                        self.acc.last_mut().unwrap().s.append(vec![t.tok.clone()]);
                        self.trail = None;
                    }
                    EOF => {
                        self.indent_size = 0;
                        self.trail = None;
                    }
                }
            }

            // this never gets run
            Some(Space(_) | Tab(_)) => {
                match next {
                    // [L] + N -> [],  Indent([]), reset, add to whitespace
                    Newline(t) => {
                        // ignore this spurious linespace, we don't change our indent stack, just
                        // reset
                        self.indent_size = 0;
                        self.trail = None;
                        self.whitespace.push(t);
                    }
                    EOF => {
                        self.indent_size = 0;
                        self.trail = None;
                    }
                    // [L] + T -> [T], indent unchanged, T.pre += L
                    Token(mut t) => {
                        t.s.prepend(self.drain_whitespace());
                        self.push_token(t.clone());
                        self.trail = Some(LexNext::Token(t));
                    }
                    _ => unreachable!()
                }
            }
            Some(_) => unreachable!(),
            None => {
                let linespace_count = next.linespace_count();
                match next {
                    // [] + L -> [], Indent([L]), pending indentation
                    Space(t) | Tab(t) => {
                        self.indent_size += linespace_count;
                        self.whitespace.push(t.clone());
                    }
                    // [] + T -> [T], Ident([]), got a token, waiting for L, or N, add ws to T.pre
                    // First token on a line
                    // check if we are closing out previous indentation
                    Token(mut t) => {
                        loop {
                            let prev_indent = if let Some(prev) = self.indent_stack.last() {
                                prev.indent
                            } else {
                                0
                            };

                            if self.indent_size < prev_indent {
                                // close out
                                let prev = self.indent_stack.pop().unwrap();
                                //self.push_token(crate::tokens::Token::new(Tok::IndentClose, prev.pos));
                                self.whitespace.push(crate::tokens::Token::new(Tok::IndentClose, prev.pos));
                            } else if self.indent_size > prev_indent {
                                //self.push_token(crate::tokens::Token::new(Tok::IndentOpen, t.pos));
                                self.whitespace.push(crate::tokens::Token::new(Tok::IndentOpen, t.pos));
                                //self.indent_stack.push(prev);
                                //
                                // update indentation before pushing
                                let mut x = t.clone();
                                x.indent = self.indent_size;
                                self.indent_stack.push(x);
                                break;
                            } else {
                                // do nothing
                                break;
                            }
                        }

                        t.s.prepend(self.drain_whitespace());
                        self.trail = Some(LexNext::Token(t.clone()));
                        self.push_token(t.clone());
                    }
                    // [] + N -> [] Indent([]) - reset
                    Newline(t) => {
                        // do nothing with the indent stack
                        self.indent_size = 0;
                        self.whitespace.push(t.clone());
                    }
                    EOF => {
                        loop {
                            if let Some(prev) = self.indent_stack.pop() {
                                // close out
                                self.push_token(crate::tokens::Token::new(Tok::IndentClose, prev.pos));
                            } else {
                                break;
                            };
                        }
                    }
                }
            }
        }
    }

    pub fn lex(&mut self, i: &'a str) -> PResult<Span<'a>, ()> {
        let (i, tokens) = many0(lex_next)(span(i))?;
        println!("all: {:?}", (&tokens));
        tokens.into_iter().for_each(|token| {
            println!("Next: {:?}", (&token));
            self.push(token);
            println!("State: {:?}", (&self));
        });
        Ok((i, ()))
    }

    pub fn eof(&mut self) {
        self.flush();
        self.push(LexNext::EOF);
    }

    pub fn lex_eof(&mut self, i: &'a str) -> PResult<Span<'a>, ()> {
        let (i, _) = self.lex(i)?;
        let (i, pos) = position(i)?;
        // flush before pushing EOF
        // we only want surround on EOF if we have a whitespace file
        self.flush();
        self.push_token(token(Tok::EOF, pos));
        self.eof();
        println!("state: {:?}", self);
        Ok((i, ()))
    }
}
