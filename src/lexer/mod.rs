use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_while1},
    character::complete::{alpha1, alphanumeric1, crlf, digit0, digit1, u64},
    combinator::{map, map_parser, recognize},
    error::{context, VerboseError},
    multi::{many0, many1},
    number::complete::double,
    sequence::{pair, tuple},
    IResult,
};

use crate::tokens::*;
use nom_locate::position;

mod error;
pub(crate) mod state;

mod string;
use string::lex_string;

pub(crate) use state::LexerState;
mod surround;
pub(crate) use surround::{Location, Surround};


pub(crate) type PResult<I, O> = IResult<I, O, VerboseError<I>>;

fn lex_space(i: Span) -> PResult<Span, Token> {
    let (i, pos) = position(i)?;
    let (i, t) = map(recognize(take_while1(|c| c == ' ')), |s: Span| {
        Tok::Spaces(s.len())
    })(i)?;
    Ok((i, token(t, pos)))
}

fn lex_tab(i: Span) -> PResult<Span, Token> {
    let (i, pos) = position(i)?;
    let (i, t) = map(recognize(take_while1(|c| c == '\t')), |s: Span| {
        Tok::Tabs(s.len())
    })(i)?;
    Ok((i, token(t, pos)))
}

fn lex_linespace(i: Span) -> PResult<Span, Token> {
    alt((lex_space, lex_tab))(i)
}

fn lex_newline(i: Span) -> PResult<Span, Token> {
    use Tok::*;
    let (i, pos) = position(i)?;
    let (i, t) = alt((
        map(recognize(take_while1(|c| c == '\n')), |s: Span| NL(s.len())),
        map(recognize(take_while1(|c| c == '\r')), |s: Span| LF(s.len())),
        map(recognize(crlf), |s: Span| CRLF(s.len())),
    ))(i)?;
    Ok((i, token(t, pos)))
}

fn lex_identifier_or_reserved(i: Span) -> PResult<Span, Token> {
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
        "true" => BoolLiteral(true),
        "false" => BoolLiteral(false),
        _ => Ident(s.to_string()),
    };
    Ok((i, token(t, pos)))
}

fn tag_token<'a>(s: &'a str, t: Tok) -> impl FnMut(Span<'a>) -> PResult<Span<'a>, Tok> {
    map(tag(s), move |_| t.clone())
}

fn lex_invalid(i: Span) -> PResult<Span, Token> {
    let (i, pos) = position(i)?;
    let (i, v) = take(1usize)(i)?;
    Ok((i, token(Tok::Invalid(v.to_string()), pos)))
}

fn lex_double(i: Span) -> PResult<Span, Token> {
    let (i, pos) = position(i)?;
    let (i, v) = map_parser(
        alt((
            recognize(tuple((tag("."), digit1))),
            recognize(tuple((digit1, tag("."), digit0))),
            recognize(tuple((digit1, tag("e"), digit1))),
        )),
        double,
    )(i)?;
    Ok((i, token(Tok::FloatLiteral(v), pos)))
}

fn lex_integer(i: Span) -> PResult<Span, Token> {
    let (i, pos) = position(i)?;
    let (i, v) = map_parser(
        alt((recognize(tuple((digit1, tag("u32")))), recognize(digit1))),
        u64,
    )(i)?;
    Ok((i, token(Tok::IntLiteral(v), pos)))
}

fn lex_number(i: Span) -> PResult<Span, Token> {
    alt((lex_double, lex_integer))(i)
}

fn lex_punc(i: Span) -> PResult<Span, Token> {
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
    Ok((i, token(t, pos)))
}

fn lex_op<'a>(i: Span<'a>) -> PResult<Span<'a>, Token<'a>> {
    use Tok::*;
    let (i, pos) = position(i)?;
    let (i, t) = alt((
        lex_op_bool,
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
        tag_token("!", Not),
        tag_token("in", In),
        tag_token("is", Is),
    ))(i)?;

    Ok((i, token(t, pos)))
}

fn lex_op_bool(i: Span) -> PResult<Span, Tok> {
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

fn lex_token<'a>(i: Span<'a>) -> PResult<Span<'a>, Token<'a>> {
    alt((
        lex_op,
        lex_punc,
        lex_string,
        lex_number,
        lex_identifier_or_reserved,
        //lex_newline,
        lex_invalid,
    ))(i)
}

//fn parse_token_space(i: Span) -> PResult<Span, Vec<Token>> {
    //alt((ws0, many1(lex_invalid)))(i)
//}

//fn lex_token_with_whitespace<'a>(i: Span<'a>) -> PResult<Span<'a>, Vec<Token<'a>>> {
    //alt((
        //map(
            //tuple((parse_token_space, lex_token, parse_token_space)),
            //|(mut a, mut b, mut c)| {
                //let mut v = vec![];
                //b.s.prepend(a.iter().map(|t| t.toks()).flatten().collect());
                //b.s.append(c.iter().map(|t| t.toks()).flatten().collect());
                //v.push(b);
                //v
            //},
        //),
        //ws1,
    //))(i)
//}

//fn _lex_token_with_linespace<'a>(i: Span<'a>) -> PResult<Span<'a>, Vec<Token<'a>>> {
    //alt((
        //map(
            //tuple((many0(lex_linespace), lex_token, many0(lex_linespace))),
            //|(a, mut b, c)| {
                //let mut v = vec![];
                //b.s.prepend(a.iter().map(|t| t.toks()).flatten().collect());
                //b.s.append(c.iter().map(|t| t.toks()).flatten().collect());
                //v.push(b);
                //v
            //},
        //),
        //ws1,
    //))(i)
//}

pub fn span<'a>(s: &'a str) -> Span<'a> {
    Span::new(s)
}

#[cfg(test)]
mod tests {
    use super::*;
    use state::*;
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
            ("\t  [] ", vec![LBracket, RBracket]),
            ("\t\t", vec![]),
            ("\t", vec![]),
        ];
        r.into_iter().for_each(|(q, mut a)| {
            a.push(EOF);
            println!("q: {:?}", q);
            let mut lexer = LexerState::from_str(q).unwrap();
            println!("state: {:?}", &lexer);
            assert_eq!(lexer.final_toks(), *a);
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
            println!("q: {:?}", q);
            let mut lexer = LexerState::from_str(q).unwrap();
            println!("state: {:?}", &lexer);
            assert_eq!(lexer.expand_toks(), *a);
        });
    }

    #[test]
    fn test_invalid() {
        let r = vec![
            ("$", vec![Invalid("$".into()), EOF]),
            (" $\t", vec![Invalid("$".into()), EOF]),
        ];
        r.iter().for_each(|(q, a)| {
            let mut lexer = LexerState::from_str(q).unwrap();
            assert_eq!(lexer.tokens().toks(), *a);
        });
    }

    #[test]
    fn test_tag_token() {
        let s = " [ ] ";
        let mut lexer = LexerState::from_str(s).unwrap();
        let toks = lexer.final_toks();
        assert_eq!(vec![LBracket, RBracket, EOF], toks);
    }

    fn lexer_losslessness(s: &str) -> bool {
        println!("{:?}", &s);
        match LexerState::from_str(s) {
            Some(mut lexer) => {
                let tokens = lexer.tokens();
                let toks = tokens.toks();
                println!("{:?}", &toks);
                let restored = tokens.unlex();
                println!("{:?} ?= {:?}", s, &toks);
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
            let mut lexer = LexerState::from_str(q).unwrap();
            let tokens = lexer.tokens();
            let toks = tokens.toks();
            println!("{:?}", (&toks));
            a.push(EOF);
            assert_eq!(toks, a);
            let restored = tokens.unlex();
            assert_eq!(&restored, q);
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
            let mut lexer = LexerState::from_str(q).unwrap();
            let tokens = lexer.tokens();
            let toks = tokens.toks();
            a.push(EOF);
            assert_eq!(toks, *a);
        });
    }

    #[test]
    fn surround() {
        use crate::lexer::surround::Linespace;
        let r = vec![
            (".1234", Linespace(0, 0)),
            (".1234\n", Linespace(0, 0)),
            ("\n.1234\n", Linespace(0, 0)),
            ("\n .1234 \n", Linespace(1, 1)),
            (" \n .1234 \n ", Linespace(1, 1)),
            (" \n.1234\n ", Linespace(0, 0)),
            (" \n     \n.1234\n    x    ", Linespace(0, 0)),
            ("\n.1234 + \n  x\n  y \n", Linespace(0, 1)),
        ];
        r.into_iter().for_each(|(q, ls)| {
            let mut lexer = LexerState::from_str(q).unwrap();
            let tokens = lexer.tokens();
            let tok = tokens.tok.get(0).unwrap();
            println!("q: {:?}", q);
            println!("tokens: {:?}", tokens);
            assert_eq!(tok.s.linespace, ls);
        });
    }

    #[test]
    fn indent() {
        use crate::lexer::surround::Linespace;
        let r = vec![
            (".1234", Linespace(0, 0)),
            (".1234\n", Linespace(0, 0)),
            //("\n.1234 + \n\t1 + \n\t2\n\t\t+3", Linespace(0, 0)),
            ("\
x = 1 ; y = 2;

x =
    5 +

    1 +
    2 / (
        x/y
    )

y + 1
    + 5
", Linespace(0,0)),
            //("\n .1234 \n", Linespace(1, 1)),
            //(" \n .1234 \n ", Linespace(1, 1)),
            //(" \n.1234\n ", Linespace(0, 0)),
            //(" \n     \n.1234\n    x    ", Linespace(0, 0)),
            //("\n.1234 + \n  x\n  y \n", Linespace(0, 1)),
        ];
        r.into_iter().for_each(|(q, ls)| {
            let mut lexer = LexerState::from_str(q).unwrap();
            let tokens = lexer.tokens();
            let tok = tokens.tok.get(0).unwrap();
            println!("q: {:?}", q);
            println!("tokens: {:?}", tokens);
            //assert_eq!(tok.s.linespace, ls);
            //assert_eq!(0, lexer.indent_size.len());
        });
    }
}
