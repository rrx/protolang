use super::*;

#[derive(Debug, Clone, PartialEq)]
pub enum LexType {
    Token,
    Newline,
    Linespace,
    EOF,
}

#[derive(Debug, Clone)]
pub struct LexNext<'a>(LexType, Vec<Token<'a>>);

fn lex_next(i: Span) -> PResult<Span, LexNext> {
    context(
        "next",
        alt((
            map(many1(lex_newline), |v| LexNext(LexType::Newline, v)),
            map(many1(lex_linespace), |v| LexNext(LexType::Linespace, v)),
            map(lex_token, |t| LexNext(LexType::Token, vec![t])),
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
    trail: VecDeque<LexNext<'a>>,
}
impl<'a> Default for LexerState<'a> {
    fn default() -> Self {
        Self {
            count: 0,
            acc: vec![],
            whitespace: vec![],
            indent_size: 0,
            trail: VecDeque::new(),
        }
    }
}
impl<'a> LexerState<'a> {
    pub fn from_str(s: &'a str) -> Option<Self> {
        let mut lexer = Self::default();
        match lexer.lex_eof(s.into()) {
            Ok((rest, r)) => Some(lexer),
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
        if let Some(last) = self.acc.last_mut() {
            last.s.append(
                self.whitespace
                    .drain(..)
                    .map(|v| v.toks())
                    .flatten()
                    .collect::<Vec<_>>(),
            );
        }
    }

    pub fn tokens(&'a mut self) -> Tokens<'a> {
        self.flush();
        self.dump();
        Tokens::new(&self.acc[..])
    }

    pub fn token_vec(&mut self) -> Vec<Token<'a>> {
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
        if self.trail.len() == 3 {
            self.trail.pop_back();
        }
        let maybe_front = self.trail.front();
        let push_front = match maybe_front {
            Some(front) => front.0 != next.0,
            None => false,
        };
        if push_front {
            self.trail.push_front(next.clone());
        }
        // [] + L -> [L], Indent([L]), pending indentation
        // [] + T -> [T], Ident([]), got a token, waiting for L, or N, add ws to T.pre
        // [] + N -> [] Indent([]) - reset
        // [T] + L -> [T] - Ident([]) - no identation, T.post += L
        // [T] + N -> [] -> Indent([]) - T.post += N, reset
        // [T] + T -> [T] -> Indent([]) - reset to last T
        // [L] + N -> [],  Indent([]), reset, add to whitespace
        // [L] + T -> [T], indent([L]), T.pre += L
        // [L] + L2 -> [L+L2], indent([L,L2])
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
