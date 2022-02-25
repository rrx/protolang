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
    String(String),
    Float(f64),
    Integer(u64),
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
    In, Is
}

#[derive(Debug)]
pub struct Token<'a> {
    pub tok: Tok,
    pub pos: Span<'a>
}

pub fn token(tok: Tok, pos: Span) -> Token {
    Token { tok, pos }
}


