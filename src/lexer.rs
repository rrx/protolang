use nom::{
    IResult,
    bytes::complete::{tag, take_while, take_while1, take, escaped},
    character::{
        is_digit,
        complete::{alpha1, alphanumeric1, char, digit0, digit1, one_of, u64}
    },
    combinator::{cut, map, map_parser, map_res, not, recognize},
    branch::{alt},
    error::{context, ParseError, ErrorKind},
    multi::{fold_many0, many0, many1},
    number::complete::{double, float},
    sequence::{delimited, pair, preceded, terminated, tuple}
};

#[derive(Debug, Clone, PartialEq)]
pub enum Tok {
    Spaces(usize),
    Tabs(usize),
    NL(usize),
    LF(usize),
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
pub struct Token {
    pub tok: Tok,
    pub start: usize,
    pub end: usize
}

// Strings
fn parse_str(i: &str) -> IResult<&str, &str> {
  escaped(alphanumeric1, '\\', one_of("\"n\\"))(i)
}

fn string(
  i: &str,
) -> IResult<&str, &str> {
  context(
    "string",
    preceded(char('\"'), cut(terminated(parse_str, char('\"')))),
  )(i)
}

fn lex_string(i: &str) -> IResult<&str, Tok> {
    map(string, |s| Tok::String(s.into()))(i)
}


fn ws0(i: &str) -> IResult<&str, Vec<Tok>> {
    many0(lex_whitespace)(i)
}

fn lex_whitespace<'a>(i: &'a str) -> IResult<&'a str, Tok> {
    use Tok::*;

    alt((
            map(recognize(take_while1(|c| c == ' ')),  |s: &str| Spaces(s.len())),
            map(recognize(take_while1(|c| c == '\t')), |s: &str| Tabs(s.len())),
            map(recognize(take_while1(|c| c == '\n')), |s: &str| NL(s.len())),
            map(recognize(take_while1(|c| c == '\r')), |s: &str| LF(s.len()))
        ))(i)
}

fn lex_identifier_or_reserved(i: &str) -> IResult<&str, Tok> {
    use Tok::*;
    map(recognize(
            pair(
                alt((alpha1, tag("_"))),
                many0(alt((alphanumeric1, tag("_"))))
                )), 
        |s: &str| {
            match s {
                "if" => If,
                "else" => Else,
                "return" => Return,
                "yield" => Yield,
                "true" => True,
                "false" => False,
                _ => {

                    Ident(s.to_string())
                }
            }
        })(i)
}

fn maptag<'a>(s: &'a str, t: Tok) -> impl FnMut(&'a str) -> IResult<&'a str, Tok> {
    map(tag(s), move |_| t.clone())
}

fn lex_invalid(i: &str) -> IResult<&str, Tok> {
    map(take(1usize), |v: &str| Tok::Invalid(v.to_string()))(i)
}

fn lex_double(i: &str) -> IResult<&str, Tok> {
    map(map_parser(alt((
            recognize(tuple((tag("."), digit1))),
            recognize(tuple((digit1, tag("."), digit0))),
            recognize(tuple((digit1, tag("e"), digit1))),
            )), double), |f:f64| Tok::Float(f))(i)
}

fn lex_integer(i: &str) -> IResult<&str, Tok> {
    map(map_parser(alt((
            recognize(tuple((digit1, tag("u32")))),
            recognize(digit1),
            )), u64), |v:u64| Tok::Integer(v))(i)
}

fn lex_number(i: &str) -> IResult<&str, Tok> {
    alt((lex_double, lex_integer))(i)
}

fn lex_token(i: &str) -> IResult<&str, Tok> {
    alt((
            lex_op,
            lex_punc,
            lex_string,
            lex_number,
            lex_identifier_or_reserved,
            lex_invalid
            ))(i)
}

fn lex_token_with_whitespace(i: &str) -> IResult<&str, Vec<Tok>> {
    map(tuple((ws0, lex_token, ws0)), |(mut a,b,mut c)| {
        let mut v = vec![];
        v.append(&mut a);
        v.push(b);
        v.append(&mut c);
        v
    })(i)
}

fn lex_punc(i: &str) -> IResult<&str, Tok> {
    use Tok::*;
    alt((
            maptag("(", LParen),
            maptag(")", RParen),
            maptag("[", LBracket),
            maptag("]", RBracket),
            maptag("{", LBrace),
            maptag("}", RBrace),
            maptag(":", Colon),
            maptag(";", SemiColon),
            maptag(",", Comma),
        ))(i)
}

fn lex_tokens(i: &str) -> IResult<&str, Vec<Tok>> {
    fold_many0(lex_token_with_whitespace, Vec::new, |mut acc: Vec<_>, mut item| {
        acc.append(&mut item);
        acc
    })(i)
}

fn lex_op(i: &str) -> IResult<&str, Tok> {
    use Tok::*;
    alt((
            maptag("=", Assign),
            maptag("==", Equals),
            maptag("!=", NotEquals),
            maptag("+", Plus),
            maptag("-", Minus),
            maptag("*", Mul),
            maptag("/", Div),
            maptag("+=", PlusEq),
            maptag("-=", MinusEq),
            maptag("*=", MulEq),
            maptag("/=", DivEq),
            maptag("?", Percent),
            maptag("!", Not),
            maptag("&&", And),
            maptag("||", Or),
            maptag("in", In),
            maptag("is", Is),
        ))(i)
}

pub fn lex<'a>(s: &'a str) -> IResult<&'a str, Vec<Tok>> {
    lex_tokens(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use Tok::*;

    #[test]
    fn test_ws() {
        assert_eq!("\t".len(), 1);
        assert!(lex_whitespace("").is_err());
        assert_eq!(Tok::Spaces(1), lex_whitespace(" ").unwrap().1);
        assert_eq!(Tok::Tabs(1), lex_whitespace("\t").unwrap().1);
        let s = "\t  [] ";
        let (_, r) = lex(s.into()).unwrap();
        assert_eq!(vec![Tabs(1), Spaces(2), LBracket, RBracket, Spaces(1)], r);
    }

    #[test]
    fn test_maptag() {
        let s = " [ ] ";
        let (_, r) = lex(s.into()).unwrap();
        assert_eq!(vec![Spaces(1), LBracket, Spaces(1), RBracket, Spaces(1)], r);
    }

    #[test]
    fn test_string() {
        let s = " \"asdf\\nfdsa\" ";
        let (_, r) = lex(s.into()).unwrap();
        assert_eq!(vec![Spaces(1), String("asdf\\nfdsa".into()), Spaces(1)], r);
    }

    #[test]
    fn test_number() {
        let r = vec![
            (".1234", vec![Float(0.1234)]),
            ("0.1234", vec![Float(0.1234)]),
            ("00.1234", vec![Float(0.1234)]),
            ("1.1234", vec![Float(1.1234)]),
            ("+1.1234", vec![Plus,Float(1.1234)]),
            ("-1.1234", vec![Minus,Float(1.1234)]),
            ("1e1", vec![Float(10.)]),
            ("10.", vec![Float(10.)]),
            ("1", vec![Integer(1)]),
            ("0", vec![Integer(0)]),
            ("-0", vec![Minus, Integer(0)]),
            ("1+1", vec![Integer(1), Plus, Integer(1)]),
        ];
        r.iter().for_each(|(q, a)| {
            let (_, r) = lex(q).unwrap();
            assert_eq!(*a, r);
        });
    }
}
