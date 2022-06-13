use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_while, take_while1},
    character::complete::{alpha1, alphanumeric1, crlf, digit0, digit1, u64},
    combinator::{map, map_parser, recognize, verify},
    error::{ErrorKind, VerboseError},
    error_position,
    multi::many0,
    number::complete::double,
    sequence::{pair, tuple},
    IResult,
};

use super::state::{lex_next, LexNext};
use super::string::lex_string;
use crate::tokens::*;
use nom_locate::position;

pub(crate) type LResult<I, O> = IResult<I, O, VerboseError<I>>;

pub(crate) fn s_tag_token<'a>(t: Tok) -> impl FnMut(Span<'a>) -> LResult<Span<'a>, Token<'a>> {
    verify(lex_token_any, move |t0: &Token<'a>| t == t0.tok)
}

pub(crate) fn lex_token_any<'a>(i: Span<'a>) -> LResult<Span<'a>, Token<'a>> {
    let (i, next) = lex_next(i)?;
    match next.into_token() {
        Some(t) => Ok((i, t)),
        None => Err(nom::Err::Error(error_position!(i, ErrorKind::Eof))),
    }
}

pub fn lex_space(i: Span) -> LResult<Span, Token> {
    let (i, pos) = position(i)?;
    let (i, t) = map(recognize(take_while1(|c| c == ' ')), |s: Span| {
        Tok::Spaces(s.len())
    })(i)?;
    let t = Token::new(t, &pos, &i);
    Ok((i, t))
}

pub fn lex_tab(i: Span) -> LResult<Span, Token> {
    let (i, pos) = position(i)?;
    let (i, t) = map(recognize(take_while1(|c| c == '\t')), |s: Span| {
        Tok::Tabs(s.len())
    })(i)?;
    let t = Token::new(t, &pos, &i);
    Ok((i, t))
}

pub fn lex_newline(i: Span) -> LResult<Span, Token> {
    use Tok::*;
    let (i, pos) = position(i)?;
    let (i, t) = alt((
        map(recognize(take_while1(|c| c == '\n')), |s: Span| NL(s.len())),
        map(recognize(take_while1(|c| c == '\r')), |s: Span| LF(s.len())),
        map(recognize(crlf), |s: Span| CRLF(s.len())),
    ))(i)?;
    let t = Token::new(t, &pos, &i);
    Ok((i, t))
}

pub fn lex_identifier_or_reserved(i: Span) -> LResult<Span, Token> {
    use Tok::*;
    let (i, pos) = position(i)?;
    let (i, s) = recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(i)?;
    let t = match *s.fragment() {
        "if" => If,
        "else" => Else,
        "return" => Return,
        "yield" => Yield,
        "let" => Let,
        "mut" => Mut,
        "true" => BoolLiteral(true),
        "false" => BoolLiteral(false),
        _ => Ident(s.to_string()),
    };
    let t = Token::new(t, &pos, &i);
    Ok((i, t))
}

fn tag_token<'a>(s: &'a str, t: Tok) -> impl FnMut(Span<'a>) -> LResult<Span<'a>, Tok> {
    map(tag(s), move |_| t.clone())
}

pub fn lex_invalid(i: Span) -> LResult<Span, Token> {
    let (i, pos) = position(i)?;
    let (i, v) = take(1usize)(i)?;
    let t = Token::new(Tok::Invalid(v.to_string()), &pos, &i);
    Ok((i, t))
}

pub fn lex_double(i: Span) -> LResult<Span, Token> {
    let (i, pos) = position(i)?;
    let (i, v) = map_parser(
        alt((
            recognize(tuple((tag("."), digit1))),
            recognize(tuple((digit1, tag("."), digit0))),
            recognize(tuple((digit1, tag("e"), digit1))),
        )),
        double,
    )(i)?;
    let t = Token::new(Tok::FloatLiteral(v), &pos, &i);
    Ok((i, t))
}

pub fn lex_integer(i: Span) -> LResult<Span, Token> {
    let (i, pos) = position(i)?;
    let (i, v) = map_parser(
        alt((recognize(tuple((digit1, tag("u32")))), recognize(digit1))),
        u64,
    )(i)?;
    let t = Token::new(Tok::IntLiteral(v), &pos, &i);
    Ok((i, t))
}

fn lex_number(i: Span) -> LResult<Span, Token> {
    alt((lex_double, lex_integer))(i)
}

fn lex_until_eol(i: Span) -> LResult<Span, Span> {
    // also return if we get EOF
    if i.len() == 0 {
        return Ok((i.clone(), i));
    }
    take_while(|c: char| c != '\n')(i)
}

pub fn lex_comments(i: Span) -> LResult<Span, Token> {
    use Tok::*;
    let (i, pos) = position(i)?;
    let (i, s) = recognize(pair(
        alt((tag_token("//", DoubleSlash), tag_token("#", Pound))),
        lex_until_eol,
    ))(i)?;
    let t = Token::new(Tok::Comment(s.to_string()), &pos, &i);
    Ok((i, t))
}

fn lex_punc(i: Span) -> LResult<Span, Token> {
    use Tok::*;
    let (i, pos) = position(i)?;
    let (i, t) = alt((
        tag_token("(", LParen),
        tag_token(")", RParen),
        tag_token("[", LBracket),
        tag_token("]", RBracket),
        tag_token("{", LBrace),
        tag_token("}", RBrace),
        tag_token(":", Colon),
        tag_token(";", SemiColon),
        tag_token(",", Comma),
        tag_token("\\", Tok::Backslash),
    ))(i)?;
    let t = Token::new(t, &pos, &i);
    Ok((i, t))
}

fn lex_op<'a>(i: Span<'a>) -> LResult<Span<'a>, Token<'a>> {
    use Tok::*;
    let (i, pos) = position(i)?;
    let (i, t) = alt((
        lex_op_bool,
        tag_token("<=>", Spaceship),
        tag_token("?:", Elvis),
        tag_token("+=", PlusEq),
        tag_token("-=", MinusEq),
        tag_token("*=", MulEq),
        tag_token("/=", DivEq),
        tag_token("=", Assign),
        tag_token("+", Plus),
        tag_token("-", Minus),
        tag_token("*", Mul),
        tag_token("/", Div),
        tag_token("?", Question),
        tag_token("^", Caret),
        tag_token("%", Percent),
        tag_token("!", Exclamation),
        tag_token("in", In),
        tag_token("is", Is),
    ))(i)?;

    let t = Token::new(t, &pos, &i);
    Ok((i, t))
}

fn lex_op_bool(i: Span) -> LResult<Span, Tok> {
    alt((
        tag_token("==", Tok::Equals),
        tag_token("!=", Tok::NotEquals),
        tag_token("&&", Tok::And),
        tag_token("||", Tok::Or),
        tag_token("->", Tok::LeftArrow),
        tag_token("<-", Tok::RightArrow),
        tag_token(">=", Tok::GTE),
        tag_token("<=", Tok::LTE),
        tag_token(">", Tok::GT),
        tag_token("<", Tok::LT),
    ))(i)
}

pub fn lex_token<'a>(i: Span<'a>) -> LResult<Span<'a>, Token<'a>> {
    alt((
        lex_comments,
        lex_op,
        lex_punc,
        lex_string,
        lex_number,
        lex_identifier_or_reserved,
        lex_invalid,
    ))(i)
}

pub fn span<'a>(s: &'a str, file_id: FileId) -> Span<'a> {
    Span::new_extra(s, SpanExtra { file_id })
}

/*
pub fn lex<'a>(s: &'a str, file_id: FileId) -> Tokens<'a> {
    let mut lexer = super::state::LexerState::default().set_file_id(file_id);
    let (_, tokens) = lexer.lex(s).unwrap();
    tokens.clone()
}
*/

#[cfg(test)]
mod tests {
    use super::super::state::LexerState;
    use super::*;
    use log::debug;
    use test_log::test;
    use Tok::*;

    #[test]
    fn tokens() {
        let p = Tok::Minus;
        assert_eq!(p.unlex(), "-");
        let p = Tok::Plus;
        assert_eq!(p.unlex(), "+");
    }

    #[test]
    fn test_ws() {
        assert_eq!("\t".len(), 1);

        let r = vec![
            ("", vec![]),
            (" ", vec![]),
            ("\t  [] ", vec![/*IndentOpen, */ LBracket, RBracket]),
            ("\t\t", vec![]),
            ("\t", vec![]),
        ];
        r.into_iter().for_each(|(q, a)| {
            debug!("q: {:?}", q);
            let mut lexer = LexerState::default();
            let (_, tokens) = lexer.lex(q).unwrap();
            assert_eq!(tokens.toks(), *a);
        });
    }

    #[test]
    fn test_ws_only() {
        // test whitespace file
        let r = vec![
            ("", vec![]),
            (" ", vec![Spaces(1)]),
            ("\t\t", vec![Tabs(2)]),
            ("\t", vec![Tabs(1)]),
        ];
        r.into_iter().for_each(|(q, mut a)| {
            a.push(EOF);
            debug!("q: {:?}", q);
            let mut lexer = LexerState::default();
            let (_, tokens) = lexer.lex_eof(q).unwrap();
            assert_eq!(tokens.expand_toks(), *a);
        });
    }

    #[test]
    fn test_invalid() {
        let r = vec![
            ("$", vec![Invalid("$".into()), EOF]),
            (" $\t", vec![/* IndentOpen, */ Invalid("$".into()), EOF]),
            (
                " $\nasdf",
                vec![
                    /* IndentOpen, */ Invalid("$".into()),
                    Ident("asdf".into()),
                    EOF,
                ],
            ),
            (
                "$\nasdf",
                vec![Invalid("$".into()), Ident("asdf".into()), EOF],
            ),
        ];
        r.iter().for_each(|(q, a)| {
            let mut lexer = LexerState::default();
            let (_, tokens) = lexer.lex_eof(q).unwrap();
            assert_eq!(tokens.toks(), *a);
        });
    }

    #[test]
    fn test_tag_token() {
        let s = " [ ] ";
        let mut lexer = LexerState::default();
        let (_, tokens) = lexer.lex_eof(s).unwrap();
        let toks = tokens.toks();
        assert_eq!(vec![/*IndentOpen, */ LBracket, RBracket, EOF], toks);
    }

    fn lexer_losslessness(s: &str) -> bool {
        debug!("{:?}", &s);
        let mut lexer = LexerState::default();
        match lexer.lex_eof(s) {
            Ok((_, tokens)) => {
                let toks = tokens.toks();
                debug!("{:?}", &toks);
                let restored = tokens.unlex();
                debug!("{:?} ?= {:?}", s, &toks);
                restored == s
            }
            _ => false,
        }
    }

    #[test]
    fn token() {
        let r = vec![
            ("=", vec![Tok::Assign]),
            ("==", vec![Tok::Equals]),
            ("-", vec![Tok::Minus]),
        ];
        r.into_iter().for_each(|(q, mut a)| {
            let mut lexer = LexerState::default();
            let (_, tokens) = lexer.lex_eof(q).unwrap();
            let toks = tokens.toks();
            debug!("{:?}", (&toks));
            a.push(EOF);
            assert_eq!(toks, a);
            let restored = tokens.unlex();
            assert_eq!(&restored, q);
        });
    }

    #[test]
    fn comment() {
        use Tok::*;
        let r = vec![
            (
                "// asdf \n",
                vec![Tok::Comment("// asdf ".into()), Tok::NL(1)],
            ),
            ("// asdf", vec![Tok::Comment("// asdf".into())]),
            (
                "// asdf \n asdf ",
                vec![
                    Comment("// asdf ".into()),
                    NL(1),
                    Spaces(1),
                    IndentOpen,
                    Ident("asdf".into()),
                    Spaces(1),
                ],
            ),
            (
                "# asdf \n asdf ",
                vec![
                    Comment("# asdf ".into()),
                    NL(1),
                    Spaces(1),
                    IndentOpen,
                    Ident("asdf".into()),
                    Spaces(1),
                ],
            ),
            (
                "asdf #\n asdf ",
                vec![
                    Tok::Ident("asdf".into()),
                    Spaces(1),
                    Tok::Comment("#".into()),
                    NL(1),
                    Spaces(1),
                    IndentOpen,
                    Tok::Ident("asdf".into()),
                    Spaces(1),
                ],
            ),
            (
                "#asdf\n//asdf\n1",
                vec![
                    Tok::Comment("#asdf".into()),
                    Tok::NL(1),
                    Tok::Comment("//asdf".into()),
                    Tok::NL(1),
                    Tok::IntLiteral(1),
                ],
            ),
        ];
        r.into_iter().for_each(|(q, mut a)| {
            a.push(EOF);
            let mut lexer = LexerState::default();
            let (_, tokens) = lexer.lex_eof(q).unwrap();
            let toks = tokens.expand_toks();
            assert_eq!(a, toks);
            assert!(lexer_losslessness(q));
        });
    }

    #[test]
    fn lossless() {
        let r = vec![
            "\"\"",
            " \"asdf\\nfdsa\" ",
            "\"🎁\"",
            "\"\u{2764}\"",
            "x = 123",
            "321 ",
            "$",
            "$\n",
            "$\r\n",
            "$\t",
            "$\r",
            "\n$\n",
            "\r\n$\r\n",
            "\t$\t",
            "\r$\r",
            "1+2",
            "+ 1 / 2",
            "+ 1 / (2 - 5)",
            "x+1",
            "// asdf ",
            "(((((0)))))",
            "
            
                ",
            "
f +
1
",
            "
f = 2 +
    1
f +
    1
",
        ];

        r.iter().for_each(|q| assert!(lexer_losslessness(q)));
    }

    #[test]
    fn test_number() {
        let r = vec![
            (".1234", vec![FloatLiteral(0.1234)]),
            ("0.1234", vec![FloatLiteral(0.1234)]),
            ("00.1234", vec![FloatLiteral(0.1234)]),
            ("1.1234", vec![FloatLiteral(1.1234)]),
            ("+1.1234", vec![Plus, FloatLiteral(1.1234)]),
            ("-1.1234", vec![Minus, FloatLiteral(1.1234)]),
            ("1e1", vec![FloatLiteral(10.)]),
            ("10.", vec![FloatLiteral(10.)]),
            ("1", vec![IntLiteral(1)]),
            ("0", vec![IntLiteral(0)]),
            ("-0", vec![Minus, IntLiteral(0)]),
            ("1+1", vec![IntLiteral(1), Plus, IntLiteral(1)]),
        ];
        r.into_iter().for_each(|(q, mut a)| {
            let mut lexer = LexerState::default();
            let (_, tokens) = lexer.lex_eof(q).unwrap();
            let toks = tokens.toks();
            a.push(EOF);
            assert_eq!(toks, *a);
        });
    }

    #[test]
    fn lex_token() {
        let i = span("1 2", 0);
        let (i, t) = lex_token_any(i).unwrap();
        assert_eq!(t.tok, Tok::IntLiteral(1));
        let (i, t) = lex_token_any(i).unwrap();
        assert_eq!(t.tok, Tok::Spaces(1));
        let (i, t) = lex_token_any(i).unwrap();
        assert_eq!(t.tok, Tok::IntLiteral(2));
    }
}
