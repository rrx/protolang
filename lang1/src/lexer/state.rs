use super::lex::*;
use crate::tokens::{FileId, Span, Tok, Token, Tokens};
use log::debug;
use nom::{branch::alt, combinator::map, error::context, multi::many0};

#[derive(Debug, Clone, PartialEq)]
pub enum LexNext<'a> {
    Token(Token<'a>),
    Newline(Token<'a>),
    Space(Token<'a>),
    Comment(Token<'a>),
    Tab(Token<'a>),
    EOF,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IndentState {
    LineNotIndented,
    LineIndented,
}

impl<'a> LexNext<'a> {
    pub fn into_token(self) -> Option<Token<'a>> {
        match self {
            LexNext::Token(t) => Some(t),
            LexNext::Newline(t) => Some(t),
            LexNext::Space(t) => Some(t),
            LexNext::Tab(t) => Some(t),
            LexNext::Comment(t) => Some(t),
            _ => None,
        }
    }

    pub fn get_token(&self) -> Option<Token<'a>> {
        self.clone().into_token()
    }

    fn linespace_count(&self) -> usize {
        match self {
            Self::Space(t) | Self::Tab(t) => match t.tok {
                Tok::Spaces(n) => n,
                Tok::Tabs(n) => n,
                _ => 0,
            },
            _ => 0,
        }
    }
}

pub fn lex_next(i: Span) -> LResult<Span, LexNext> {
    context(
        "next",
        alt((
            map(lex_newline, |v| LexNext::Newline(v)),
            map(lex_space, |v| LexNext::Space(v)),
            map(lex_tab, |v| LexNext::Tab(v)),
            map(lex_comments, |v| LexNext::Comment(v)),
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
    indent_state: IndentState,
    indent_stack: Vec<Token<'a>>,
    file_id: FileId,
}

impl<'a> Default for LexerState<'a> {
    fn default() -> Self {
        Self {
            acc: vec![],
            whitespace: vec![],
            indent_size: 0,
            indent_state: IndentState::LineNotIndented,
            indent_stack: vec![],
            file_id: 0,
        }
    }
}

impl<'a> LexerState<'a> {
    pub fn set_file_id(mut self, file_id: FileId) -> Self {
        self.file_id = file_id;
        self
    }

    fn push_token(&mut self, mut token: Token<'a>) {
        token.indent = self.indent_size;
        token = token.set_file_id(self.file_id);
        token.s.prepend(
            self.whitespace
                .drain(..)
                .map(|v| v.toks())
                .flatten()
                .collect::<Vec<_>>(),
        );
        self.acc.push(token);
    }

    fn flush(&mut self) {
        // we can only flush if we have at least one in acc
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

    fn get_tokens(&'a mut self) -> Tokens<'a> {
        self.flush();
        Tokens::new(&self.acc[..], self.file_id)
    }

    fn _token_vec(&mut self) -> Vec<Token<'a>> {
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
        debug!("State");
        debug!("\tStack: {:?}", self.indent_stack);
        for token in &self.acc {
            debug!("\tToken: {:?}", token);
        }
        debug!("\tIndentState: {:?}", self.indent_state);
    }

    fn push(&mut self, next: LexNext<'a>) {
        use LexNext::*;
        match self.indent_state {
            IndentState::LineIndented => {
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
                    Newline(t) | Comment(t) => {
                        // push indent to stack and reset
                        self.indent_size = 0;

                        // append whitespace to the last token
                        self.acc.last_mut().unwrap().s.append(vec![t.tok.clone()]);
                        self.indent_state = IndentState::LineNotIndented;
                    }
                    EOF => {
                        self.indent_size = 0;
                        self.indent_state = IndentState::LineNotIndented;
                    }
                }
            }

            IndentState::LineNotIndented => {
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
                                let close = crate::tokens::Token::new(
                                    Tok::IndentClose,
                                    &prev.pos,
                                    &prev.pos,
                                );
                                if false {
                                    self.push_token(close);
                                } else {
                                    self.whitespace.push(close);
                                }
                            } else if self.indent_size > prev_indent {
                                let open =
                                    crate::tokens::Token::new(Tok::IndentOpen, &t.pos, &t.pos);
                                if false {
                                    self.push_token(open);
                                } else {
                                    self.whitespace.push(open);
                                }
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
                        self.indent_state = IndentState::LineIndented;
                        self.push_token(t.clone());
                    }
                    // [] + N -> [] Indent([]) - reset
                    Newline(t) | Comment(t) => {
                        // do nothing with the indent stack
                        self.indent_size = 0;
                        self.whitespace.push(t.clone());
                    }
                    EOF => {
                        loop {
                            if let Some(prev) = self.indent_stack.pop() {
                                // close out
                                self.whitespace.push(crate::tokens::Token::new(
                                    Tok::IndentClose,
                                    &prev.pos,
                                    &prev.pos,
                                ));
                            } else {
                                break;
                            };
                        }
                    }
                }
            }
        }
    }

    pub fn lex(&'a mut self, i: &'a str) -> LResult<Span<'a>, Tokens<'a>> {
        let (i, tokens) = many0(lex_next)(span(i, self.file_id))?;
        tokens.iter().for_each(|token| {
            self.push(token.clone());
        });
        Ok((i, self.get_tokens()))
    }

    fn eof(&mut self) {
        self.flush();
        self.push(LexNext::EOF);
    }

    pub fn lex_eof(&'a mut self, i: &'a str) -> LResult<Span<'a>, Tokens<'a>> {
        let (i, tokens) = many0(lex_next)(span(i, self.file_id))?;
        tokens.iter().for_each(|token| {
            self.push(token.clone());
        });
        self.push_token(Token::new(Tok::EOF, &i, &i));
        self.eof();
        Ok((i, self.get_tokens()))
    }
}
