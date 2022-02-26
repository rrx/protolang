use nom::*;
use std::iter::Enumerate;
use std::ops::{Range, RangeFrom, RangeFull, RangeTo};
use nom_locate::{position, LocatedSpan};

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
    True,
    False,
    Colon,
    Comma,
    SemiColon,

    // Keywords
    If, Else, Return, Yield,
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
    GT,GTE,LT,LTE,
    And, Or,
    In, Is,

    EOF
}

impl Tok {
    pub fn unlex(&self) -> String {
        use Tok::*;
        String::from(match self {
            Mul => "*".into(),
            IntLiteral(x) => x.to_string(),
            _ => "".into()
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token<'a> {
    pub pre: Vec<Token<'a>>,
    pub post: Vec<Token<'a>>,
    pub tok: Tok,
    pub pos: Span<'a>
}

impl<'a> Token<'a> {
    pub fn unlex(&self) -> String {
        let mut s = String::new();
        for frag in self.pre.iter().map(|v| v.tok.clone()) {
            s.push_str(frag.unlex().as_str());
        }
        s.push_str(self.tok.unlex().as_str());
        for frag in self.post.iter().map(|v| v.tok.clone()) {
            s.push_str(frag.unlex().as_str());
        }
        s
    }
}

pub fn token(tok: Tok, pos: Span) -> Token {
    Token { tok, pos, pre: vec![], post: vec![] }
}

#[derive(Clone, Copy, PartialEq, Debug)]
#[repr(C)]
pub struct Tokens<'a> {
    pub tok: &'a [Token<'a>],
    pub start: usize,
    pub end: usize,
}

impl<'a> Tokens<'a> {
    pub fn new(vec: &'a [Token]) -> Self {
        Tokens {
            tok: vec,
            start: 0,
            end: vec.len(),
        }
    }

    pub fn toks(&self) -> Vec<Tok> {
        self.iter_elements().map(|v| v.tok.clone()).collect::<Vec<_>>()
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
        }
    }

    #[inline]
    fn take_split(&self, count: usize) -> (Self, Self) {
        let (prefix, suffix) = self.tok.split_at(count);
        let first = Tokens {
            tok: prefix,
            start: 0,
            end: prefix.len(),
        };
        let second = Tokens {
            tok: suffix,
            start: 0,
            end: suffix.len(),
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
