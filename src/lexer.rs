use nom::{
    IResult,
    bytes::complete::{tag, take_while, take_while1, take, escaped},
    character::{
        is_digit,
        complete::{alpha1, alphanumeric1, char, crlf, digit0, digit1, one_of, u64}
    },
    combinator::{cut, map, map_parser, map_res, not, opt, recognize},
    branch::{alt},
    error::{context, ParseError, ErrorKind},
    multi::{fold_many0, many0, many1},
    number::complete::{double, float},
    sequence::{delimited, pair, preceded, terminated, tuple}
};

use nom_locate::position;
use crate::tokens::*;

// Strings
fn parse_str(i: Span) -> IResult<Span, Span> {
  escaped(alphanumeric1, '\\', one_of("\"n\\"))(i)
}

fn string(
  i: Span,
) -> IResult<Span, Span> {
  context(
    "string",
    preceded(char('\"'), cut(terminated(parse_str, char('\"')))),
  )(i)
}

fn lex_string(i: Span) -> IResult<Span, Token> {
    let (i, pos) = position(i)?;
    let (i, s) = string(i)?;
    Ok((i, token(Tok::String(s.fragment().to_string()), pos)))
}


fn ws0(i: Span) -> IResult<Span, Vec<Token>> {
    many0(lex_whitespace)(i)
}

fn ws1(i: Span) -> IResult<Span, Vec<Token>> {
    many1(lex_whitespace)(i)
}

fn lex_whitespace<'a>(i: Span) -> IResult<Span, Token> {
    use Tok::*;
    let (i, pos) = position(i)?;
    let (i, t) = alt((
            map(recognize(take_while1(|c| c == ' ')),  |s: Span| Spaces(s.len())),
            map(recognize(take_while1(|c| c == '\t')), |s: Span| Tabs(s.len())),
            map(recognize(take_while1(|c| c == '\n')), |s: Span| NL(s.len())),
            map(recognize(take_while1(|c| c == '\r')), |s: Span| LF(s.len())),
            map(recognize(crlf), |s: Span| CRLF(s.len()))
        ))(i)?;
    Ok((i, token(t, pos)))
}

fn lex_identifier_or_reserved(i: Span) -> IResult<Span, Token> {
    use Tok::*;
    let (i, pos) = position(i)?;
    let (i, s) = recognize(
            pair(
                alt((alpha1, tag("_"))),
                many0(alt((alphanumeric1, tag("_"))))
                ))(i)?;
    let t =  match *s.fragment() {
        "if" => If,
        "else" => Else,
        "return" => Return,
        "yield" => Yield,
        "true" => True,
        "false" => False,
        _ => {

            Ident(s.to_string())
        }
    };
    Ok((i, token(t, pos)))
}

fn maptag<'a>(s: &'a str, t: Tok) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Tok> {
    map(tag(s), move |_| t.clone())
}

fn lex_invalid(i: Span) -> IResult<Span, Token> {
    let (i, pos) = position(i)?;
    let (i, v) = take(1usize)(i)?;
    Ok((i, token(Tok::Invalid(v.to_string()), pos)))
}

fn lex_double(i: Span) -> IResult<Span, Token> {
    let (i, pos) = position(i)?;
    let (i, v) = map_parser(alt((
            recognize(tuple((tag("."), digit1))),
            recognize(tuple((digit1, tag("."), digit0))),
            recognize(tuple((digit1, tag("e"), digit1))),
            )), double)(i)?;
    Ok((i, token(Tok::Float(v), pos)))
}

fn lex_integer(i: Span) -> IResult<Span, Token> {
    let (i, pos) = position(i)?;
    let (i, v) = map_parser(alt((
            recognize(tuple((digit1, tag("u32")))),
            recognize(digit1),
            )), u64)(i)?;
    Ok((i, token(Tok::Integer(v), pos)))
}

fn lex_number(i: Span) -> IResult<Span, Token> {
    alt((lex_double, lex_integer))(i)
}

fn lex_token<'a>(i: Span<'a>) -> IResult<Span<'a>, Token<'a>> {
    alt((
            lex_op,
            lex_punc,
            lex_string,
            lex_number,
            lex_identifier_or_reserved,
            lex_invalid,
            ))(i)
}

fn lex_token_with_whitespace(i: Span) -> IResult<Span, Vec<Token>> {
    alt((
            map(tuple((ws0, lex_token, ws0)), |(mut a, b, mut c)| {
                let mut v = vec![];
                v.append(&mut a);
                v.push(b);
                v.append(&mut c);
                v
            }),
            ws1,
            ))(i)
}

fn lex_punc(i: Span) -> IResult<Span, Token> {
    use Tok::*;
    let (i, pos) = position(i)?;
    let (i, t) = alt((
            maptag("(", LParen),
            maptag(")", RParen),
            maptag("[", LBracket),
            maptag("]", RBracket),
            maptag("{", LBrace),
            maptag("}", RBrace),
            maptag(":", Colon),
            maptag(";", SemiColon),
            maptag(",", Comma),
        ))(i)?;
    Ok((i, token(t, pos)))
}

fn lex_tokens(i: Span) -> IResult<Span, Vec<Token>> {
    fold_many0(lex_token_with_whitespace, Vec::new, |mut acc: Vec<_>, mut item| {
        acc.append(&mut item);
        acc
    })(i)
}

fn lex_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Token<'a>> {
    use Tok::*;
    let (i, pos) = position(i)?;
    let (i, t) = alt((
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
        ))(i)?;

    Ok((i, token(t, pos)))
}

pub fn lex<'a>(i: &str) -> IResult<Span, Vec<Token>> {
    lex_tokens(span(i))
}

fn span(s: &str) -> Span {
    Span::new(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use Tok::*;

    #[test]
    fn test_ws() {
        assert_eq!("\t".len(), 1);
        assert!(lex_whitespace(span("")).is_err());

        let r = vec![
            ("\t  [] ", vec![Tabs(1), Spaces(2), LBracket, RBracket, Spaces(1)]),
            ("\t\t", vec![Tabs(2)]),
            ("\t", vec![Tabs(1)]),
            ("", vec![]),
            (" ", vec![Spaces(1)]),
        ];
        r.iter().for_each(|(q, a)| {
            let (_, result) = lex(q).unwrap();
            let tokens = result.iter().map(|v| v.tok.clone()).collect::<Vec<_>>();
            assert_eq!(tokens, *a);
        });
    }

    #[test]
    fn test_maptag() {
        let s = " [ ] ";
        let (_, r) = lex(s.into()).unwrap();
        let tokens = r.iter().map(|v| v.tok.clone()).collect::<Vec<_>>();
        assert_eq!(vec![Spaces(1), LBracket, Spaces(1), RBracket, Spaces(1)], tokens);
    }

    #[test]
    fn test_string() {
        let s = " \"asdf\\nfdsa\" ";
        let (_, r) = lex(s.into()).unwrap();
        let tokens = r.iter().map(|v| v.tok.clone()).collect::<Vec<_>>();
        assert_eq!(vec![Spaces(1), String("asdf\\nfdsa".into()), Spaces(1)], tokens);
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
            let (_, result) = lex(q).unwrap();
            let tokens = result.iter().map(|v| v.tok.clone()).collect::<Vec<_>>();
            assert_eq!(tokens, *a);
        });
    }
}
