use crate::ast::{Location, Surround};
use crate::lexer::LexerState;
use crate::results::Results;
use nom::*;
use nom_locate::LocatedSpan;
use std::fmt;
use std::iter::Enumerate;
use std::ops::{Range, RangeFrom, RangeFull, RangeTo};

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, PartialEq)]
pub enum Tok {
    Spaces(usize),
    Tabs(usize),
    NL(usize),
    LF(usize),
    CRLF(usize),
    Invalid(String),
    StringLiteral(String),
    FloatLiteral(f64),
    IntLiteral(u64),
    BoolLiteral(bool),
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Ident(String),
    Colon,
    Comma,
    SemiColon,
    Backslash, // \\

    DoubleSlash, // //
    TripleQuote, // """
    RightArrow,  // ->
    LeftArrow,   // <-

    // Keywords
    If,
    Else,
    Return,
    Yield,
    Null,

    // Operators
    Assign,
    Equals,
    NotEquals,
    Not,
    Plus,
    Minus,
    Div,
    Mul,
    PlusEq,
    MinusEq,
    DivEq,
    MulEq,
    Percent,
    Question,
    Caret,
    GT,
    GTE,
    LT,
    LTE,
    And,
    Or,
    In,
    Is,

    EOF,
}

impl Tok {
    pub fn unlex(&self) -> String {
        use Tok::*;
        String::from(match self {
            Spaces(n) => " ".repeat(*n).into(),
            Tabs(n) => "\t".repeat(*n).into(),
            CRLF(n) => "\r\n".repeat(*n).into(),
            NL(n) => "\n".repeat(*n).into(),
            LF(n) => "\r".repeat(*n).into(),
            Invalid(s) => s.into(),
            Ident(s) => s.into(),
            Equals => "==".into(),
            NotEquals => "!=".into(),
            Assign => "=".into(),
            Mul => "*".into(),
            Div => "/".into(),
            Plus => "+".into(),
            Minus => "-".into(),
            Caret => "^".into(),
            Percent => "%".into(),
            LTE => "<=".into(),
            LT => "<".into(),
            GTE => ">=".into(),
            GT => ">".into(),
            Question => "?".into(),
            LParen => "(".into(),
            RParen => ")".into(),
            LBracket => "[".into(),
            RBracket => "]".into(),
            LBrace => "{".into(),
            RBrace => "}".into(),
            SemiColon => ";".into(),
            Comma => ",".into(),
            IntLiteral(x) => x.to_string(),
            FloatLiteral(x) => x.to_string(),
            BoolLiteral(x) => x.to_string(),
            StringLiteral(x) => format!("\"{}\"", x.to_string().escape_debug()),
            LeftArrow => "->".into(),
            RightArrow => "<-".into(),
            Backslash => "\\".into(),
            Null => "null".into(),

            EOF => "".into(),
            _ => {
                println!("Panic: Unable to unlex token: {:?}", self);
                unreachable!() //"[UNKNOWN]".into(),
            }
        })
    }

    pub fn is_newline(&self) -> bool {
        match self {
            Tok::NL(_) => true,
            Tok::CRLF(_) => true,
            Tok::LF(_) => true,
            _ => false,
        }
    }

    pub fn is_linespace(&self) -> bool {
        match self {
            Tok::Tabs(_) | Tok::Spaces(_) => true,
            _ => false,
        }
    }

    pub fn is_whitespace(&self) -> bool {
        self.is_linespace() || self.is_newline()
    }
}

#[derive(Clone, PartialEq)]
pub struct Token<'a> {
    pub s: Surround,
    //pub pre: Vec<Token<'a>>,
    //pub post: Vec<Token<'a>>,
    pub tok: Tok,
    pub pos: Span<'a>,
}

impl<'a> fmt::Debug for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Token")
            .field("tok", &self.tok)
            .field("pre", &self.s.pre)
            .field("post", &self.s.post)
            .field("line", &self.pos.location_line())
            .field("col", &self.pos.get_column())
            .finish()
    }
}

impl<'a> Token<'a> {
    pub fn toks_pre(&self) -> Vec<Tok> {
        self.s.pre.clone() //.iter().map(|t| t.toks()).flatten().collect()
    }

    pub fn toks_post(&self) -> Vec<Tok> {
        self.s.post.clone() //.iter().map(|t| t.toks()).flatten().collect()
    }

    pub fn toks(&self) -> Vec<Tok> {
        vec![self.tok.clone()]
    }

    pub fn expand_toks(&self) -> Vec<Tok> {
        vec![self.toks_pre(), vec![self.tok.clone()], self.toks_post()]
            .into_iter()
            .flatten()
            .collect::<Vec<_>>()
    }

    pub fn to_location(&self) -> Location {
        Location::new(
            self.pos.location_offset(),
            self.pos.location_line() as usize,
            self.pos.get_utf8_column(),
            self.pos.fragment().to_string(),
        )
    }

    pub fn to_string(&self) -> String {
        self.tok.unlex()
    }

    pub fn unlex(&self) -> String {
        let mut s = String::new();
        for frag in self.s.pre.iter().map(|v| v.clone()) {
            s.push_str(frag.unlex().as_str());
        }
        s.push_str(self.tok.unlex().as_str());
        for frag in self.s.post.iter().map(|v| v.clone()) {
            s.push_str(frag.unlex().as_str());
        }
        s
    }
}

pub fn token(tok: Tok, pos: Span) -> Token {
    Token {
        s: Surround::default(),
        tok,
        pos,
        //pre: vec![],
        //post: vec![],
    }
}

#[derive(Clone, Debug)]
#[repr(C)]
pub struct Tokens<'a> {
    pub tok: &'a [Token<'a>],
    pub start: usize,
    pub end: usize,
    pub results: Vec<Results>,
}

impl<'a> Tokens<'a> {
    pub fn new(vec: &'a [Token]) -> Self {
        Tokens {
            tok: vec,
            start: 0,
            end: vec.len(),
            results: vec![],
        }
    }

    //pub fn from_lexer(lexer: &'a mut LexerState) -> Self {
    //let toks = lexer.token_vec();
    //let tokens = Tokens::new(&toks[..]);
    //tokens
    //}

    //fn from_string(i: &'a str) -> IResult<Span<'a>, Tokens<'a>> {
    //let (i, toks) = crate::lexer::lex_eof(i)?;
    //Ok((i, Tokens::new(&toks[..])))
    //}
    pub fn to_location(&self) -> Location {
        if self.tok.len() > 0 {
            self.tok[0].to_location()
        } else {
            Location::new(0, 0, 0, "EOF".into())
        }
    }

    pub fn result(&mut self, result: Results) {
        self.results.push(result);
    }

    pub fn toks(&self) -> Vec<Tok> {
        self.iter_elements()
            .map(|v| v.toks())
            .flatten()
            .collect::<Vec<_>>()
    }

    pub fn expand_toks(&self) -> Vec<Tok> {
        self.iter_elements()
            .map(|v| v.expand_toks())
            .flatten()
            .collect::<Vec<_>>()
    }

    pub fn to_string(&self) -> String {
        let mut s = String::new();
        for frag in self.tok {
            s.push_str(frag.to_string().as_str());
        }
        s
    }

    pub fn unlex(&self) -> String {
        let mut s = String::new();
        for frag in self.tok {
            s.push_str(frag.unlex().as_str());
        }
        s
    }
}

impl<'a> InputLength for Tokens<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        self.tok.len()
    }
}

impl<'a> InputTake for Tokens<'a> {
    #[inline]
    fn take(&self, count: usize) -> Self {
        Tokens {
            tok: &self.tok[0..count],
            start: 0,
            end: count,
            results: vec![],
        }
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tok.split_at(count);
        let first = Tokens {
            tok: prefix,
            start: 0,
            end: prefix.len(),
            results: vec![],
        };
        let second = Tokens {
            tok: suffix,
            start: 0,
            end: suffix.len(),
            results: vec![],
        };
        (second, first)
    }
}

impl<'a> InputLength for Token<'a> {
    #[inline]
    fn input_len(&self) -> usize {
        1
    }
}

impl<'a> Slice<Range<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: Range<usize>) -> Self {
        Tokens {
            tok: self.tok.slice(range.clone()),
            start: self.start + range.start,
            end: self.start + range.end,
            results: vec![],
        }
    }
}

impl<'a> Slice<RangeTo<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: RangeTo<usize>) -> Self {
        self.slice(0..range.end)
    }
}

impl<'a> Slice<RangeFrom<usize>> for Tokens<'a> {
    #[inline]
    fn slice(&self, range: RangeFrom<usize>) -> Self {
        self.slice(range.start..self.end - self.start)
    }
}

impl<'a> Slice<RangeFull> for Tokens<'a> {
    #[inline]
    fn slice(&self, _: RangeFull) -> Self {
        Tokens {
            tok: self.tok,
            start: self.start,
            end: self.end,
            results: vec![],
        }
    }
}

impl<'a> InputIter for Tokens<'a> {
    type Item = &'a Token<'a>;
    type Iter = Enumerate<::std::slice::Iter<'a, Token<'a>>>;
    type IterElem = ::std::slice::Iter<'a, Token<'a>>;

    #[inline]
    fn iter_indices(&self) -> Enumerate<::std::slice::Iter<'a, Token<'a>>> {
        self.tok.iter().enumerate()
    }
    #[inline]
    fn iter_elements(&self) -> ::std::slice::Iter<'a, Token<'a>> {
        self.tok.iter()
    }
    #[inline]
    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.tok.iter().position(predicate)
    }
    #[inline]
    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        if self.tok.len() >= count {
            Ok(count)
        } else {
            Err(Needed::Unknown)
        }
    }
}
