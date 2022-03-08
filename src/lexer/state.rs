use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum LexType {
    Token,
    Newline,
    Space,
    Tab,
    EOF,
}

#[derive(Debug, Clone)]
pub struct LexNext<'a>(LexType, Token<'a>);

impl<'a> LexNext<'a> {
    fn linespace_count(&self) -> usize {
        match self.1.tok {
            Tok::Spaces(n) => n,
            Tok::Tabs(n) => n,
            _ => 0
        }
    }
}

fn lex_next(i: Span) -> PResult<Span, LexNext> {
    context(
        "next",
        alt((
            map(lex_newline, |v| LexNext(LexType::Newline, v)),
            map(lex_space, |v| LexNext(LexType::Space, v)),
            map(lex_tab, |v| LexNext(LexType::Tab, v)),
            map(lex_token, |v| LexNext(LexType::Token, v)),
            //map(lex_token_eof, |t| LexNext(LexType::EOF, vec![t])),
        )),
    )(i)
}

#[derive(Debug)]
pub struct LexerState<'a> {
    count: usize,
    acc: Vec<Token<'a>>,
    //tokens: Tokens<'a>,
    indent_size: usize,
    whitespace: Vec<Token<'a>>,
    //whitespace: Vec<Tok>,
    trail: Option<LexNext<'a>>,
}
impl<'a> Default for LexerState<'a> {
    fn default() -> Self {
        Self {
            count: 0,
            acc: vec![],
            whitespace: vec![],
            indent_size: 0,
            trail: None,
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
        for token in &self.acc {
            println!("\tToken: {:?}", token);
        }
        for next in &self.trail {
            println!("\tTrail: {:?}", next);
        }
    }

    pub fn push(&mut self, mut next: LexNext<'a>) {
        //if self.trail.len() == 3 {
            //self.trail.pop_back();
        //}
        //let maybe_front = self.trail.front();
        //let push_front = match maybe_front {
            //Some(front) => front.0 != next.0,
            //None => false,
        //};
        //if push_front {
            //self.trail.push_front(next.clone());
        //}
        //
        use LexType::*;

        match &self.trail {
            Some(LexNext(Token, token)) => {
                match next.0 {
                    // [T] + L -> [T] - Ident unchanged - no identation, T.post += L
                    Space | Tab => {
                        // we must have a token in acc
                        self.acc.last_mut().unwrap().s.append(vec![next.1.tok.clone()]);
                    }
                    // [T] + T -> [T] -> Indent unchanged - reset to last T
                    Token => {
                        self.push_token(next.1);
                    }
                    // [T] + N -> [] -> Indent([]) - T.post += N, reset
                    Newline | EOF => {
                        self.indent_size = 0;
                        self.acc.last_mut().unwrap().s.append(vec![next.1.tok.clone()]);
                        self.trail = None;
                    }
                }
            }
            Some(LexNext(Space | Tab, token)) => {
                match next.0 {
                    // [L] + N -> [],  Indent([]), reset, add to whitespace
                    Newline | EOF => {
                        self.indent_size = 0;
                        self.trail = None;
                        self.whitespace.push(next.1);
                    }
                    // [L] + T -> [T], indent unchanded, T.pre += L
                    Token => {
                        next.1.s.prepend(self.drain_whitespace());
                        self.trail = Some(next);
                    }
                    _ => unreachable!()
                }
            }
            Some(_) => unreachable!(),
            None => {
                match next.0 {
                    // [] + L -> [], Indent([L]), pending indentation
                    Space | Tab => {
                        self.indent_size += next.linespace_count();
                        self.whitespace.push(next.1);
                    }
                    // [] + T -> [T], Ident([]), got a token, waiting for L, or N, add ws to T.pre
                    Token => {
                        self.indent_size = 0;
                        next.1.s.prepend(self.drain_whitespace());
                        self.trail = Some(next.clone());
                        self.push_token(next.1);
                    }
                    // [] + N -> [] Indent([]) - reset
                    Newline | EOF => {
                        self.indent_size = 0;
                        self.whitespace.push(next.1);
                    }
                }
            }
        }
    /*
        match next.0 {
            LexType::Token | LexType::EOF => {
                let t = next.1.pop().unwrap();
                self.push_token(t);
            }
            LexType::Linespace => {
                self.whitespace.append(&mut next.1);
            }
            LexType::Newline => {
                self.whitespace.append(&mut next.1);
            }
        }
        */
    }

    //pub fn eof(&mut self, token: Token<'a>) {
    //self.push_token(token);
    //}

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

    pub fn lex_eof(&mut self, i: &'a str) -> PResult<Span<'a>, ()> {
        let (i, _) = self.lex(i)?;
        let (i, pos) = position(i)?;
        // flush before pushing EOF
        // we only want surround on EOF if we have a whitespace file
        self.flush();
        self.push_token(token(Tok::EOF, pos));
        println!("state: {:?}", self);
        Ok((i, ()))
    }
}
