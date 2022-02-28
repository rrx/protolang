use nom::{
    branch::alt,
    bytes::complete::{escaped, is_not, tag, take, take_while, take_while1, take_while_m_n},
    character::{
        complete::{
            alpha1, alphanumeric1, anychar, char, crlf, digit0, digit1, multispace1, one_of, u64,
        },
        is_digit,
    },
    combinator::{cut, map, map_opt, map_parser, map_res, not, opt, recognize, value, verify},
    error::{context, ErrorKind},
    multi::{fold_many0, many0, many1},
    number::complete::{double, float},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult, InputIter,
};

use crate::tokens::*;
use nom_locate::position;

// String parsing code was borrowed from: https://github.com/Geal/nom/blob/main/examples/string.rs

/// Parse a unicode sequence, of the form u{XXXX}, where XXXX is 1 to 6
/// hexadecimal numerals. We will combine this later with parse_escaped_char
/// to parse sequences like \u{00AC}.
fn parse_unicode(input: &str) -> IResult<&str, char> {
    // `take_while_m_n` parses between `m` and `n` bytes (inclusive) that match
    // a predicate. `parse_hex` here parses between 1 and 6 hexadecimal numerals.
    let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit());

    // `preceded` takes a prefix parser, and if it succeeds, returns the result
    // of the body parser. In this case, it parses u{XXXX}.
    let parse_delimited_hex = preceded(
        char('u'),
        // `delimited` is like `preceded`, but it parses both a prefix and a suffix.
        // It returns the result of the middle parser. In this case, it parses
        // {XXXX}, where XXXX is 1 to 6 hex numerals, and returns XXXX
        delimited(char('{'), parse_hex, char('}')),
    );

    // `map_res` takes the result of a parser and applies a function that returns
    // a Result. In this case we take the hex bytes from parse_hex and attempt to
    // convert them to a u32.
    let parse_u32 = map_res(parse_delimited_hex, move |hex| u32::from_str_radix(hex, 16));

    // map_opt is like map_res, but it takes an Option instead of a Result. If
    // the function returns None, map_opt returns an error. In this case, because
    // not all u32 values are valid unicode code points, we have to fallibly
    // convert to char with from_u32.
    map_opt(parse_u32, |value| std::char::from_u32(value))(input)
}

/// Parse an escaped character: \n, \t, \r, \u{00AC}, etc.
fn parse_escaped_char(input: &str) -> IResult<&str, char> {
    preceded(
        char('\\'),
        // `alt` tries each parser in sequence, returning the result of
        // the first successful match
        alt((
            parse_unicode,
            // The `value` parser returns a fixed value (the first argument) if its
            // parser (the second argument) succeeds. In these cases, it looks for
            // the marker characters (n, r, t, etc) and returns the matching
            // character (\n, \r, \t, etc).
            value('\n', char('n')),
            value('\r', char('r')),
            value('\t', char('t')),
            value('\u{08}', char('b')),
            value('\u{0C}', char('f')),
            value('\\', char('\\')),
            value('/', char('/')),
            value('"', char('"')),
        )),
    )(input)
}

/// Parse a backslash, followed by any amount of whitespace. This is used later
/// to discard any escaped whitespace.
fn parse_escaped_whitespace(input: &str) -> IResult<&str, &str> {
    preceded(char('\\'), multispace1)(input)
}

/// Parse a non-empty block of text that doesn't include \ or "
fn parse_literal(input: &str) -> IResult<&str, &str> {
    // `is_not` parses a string of 0 or more characters that aren't one of the
    // given characters.
    let not_quote_slash = is_not("\"\\");

    // `verify` runs a parser, then runs a verification function on the output of
    // the parser. The verification function accepts out output only if it
    // returns true. In this case, we want to ensure that the output of is_not
    // is non-empty.
    verify(not_quote_slash, |s: &str| !s.is_empty())(input)
}

/// A string fragment contains a fragment of a string being parsed: either
/// a non-empty Literal (a series of non-escaped characters), a single
/// parsed escaped character, or a block of escaped whitespace.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringFragment<'a> {
    Literal(&'a str),
    EscapedChar(char),
    EscapedWS,
}

/// Combine parse_literal, parse_escaped_whitespace, and parse_escaped_char
/// into a StringFragment.
fn parse_fragment<'a>(input: &'a str) -> IResult<&'a str, StringFragment<'a>> {
    alt((
        // The `map` combinator runs a parser, then applies a function to the output
        // of that parser.
        map(parse_literal, StringFragment::Literal),
        map(parse_escaped_char, StringFragment::EscapedChar),
        value(StringFragment::EscapedWS, parse_escaped_whitespace),
    ))(input)
}

/// Parse a string. Use a loop of parse_fragment and push all of the fragments
/// into an output string.
fn parse_string<'a>(input: &str) -> IResult<&str, String> {
    // fold_many0 is the equivalent of iterator::fold. It runs a parser in a loop,
    // and for each output value, calls a folding function on each output value.
    let build_string = fold_many0(
        // Our parser function– parses a single string fragment
        parse_fragment,
        // Our init value, an empty string
        String::new,
        // Our folding function. For each fragment, append the fragment to the
        // string.
        |mut string, fragment| {
            match fragment {
                StringFragment::Literal(s) => string.push_str(s),
                StringFragment::EscapedChar(c) => string.push(c),
                StringFragment::EscapedWS => {}
            }
            string
        },
    );

    // Finally, parse the string. Note that, if `build_string` could accept a raw
    // " character, the closing delimiter " would never match. When using
    // `delimited` with a looping parser (like fold_many0), be sure that the
    // loop won't accidentally match your closing delimiter!
    delimited(char('"'), build_string, char('"'))(input)
}

fn parse_string_with_context(i: &str) -> IResult<&str, String> {
    context("string", parse_string)(i)
}

// Strings
fn parse_str(i: Span) -> IResult<Span, String> {
    match parse_string_with_context(i.fragment()) {
        Ok((i2, s)) => Ok((span(i2), s)),
        Err(e) => Err(nom::Err::Error(nom::error::Error::new(i, ErrorKind::Tag))),
    }
}

fn lex_string(i: Span) -> IResult<Span, Token> {
    let (i, pos) = position(i)?;
    let (i, s) = parse_str(i)?;
    Ok((i, token(Tok::StringLiteral(s), pos)))
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
        map(recognize(take_while1(|c| c == ' ')), |s: Span| {
            Spaces(s.len())
        }),
        map(recognize(take_while1(|c| c == '\t')), |s: Span| {
            Tabs(s.len())
        }),
    ))(i)?;
    Ok((i, token(t, pos)))
}

fn lex_newline(i: Span) -> IResult<Span, Token> {
    use Tok::*;
    let (i, pos) = position(i)?;
    let (i, t) = alt((
        map(recognize(take_while1(|c| c == '\n')), |s: Span| NL(s.len())),
        map(recognize(take_while1(|c| c == '\r')), |s: Span| LF(s.len())),
        map(recognize(crlf), |s: Span| CRLF(s.len())),
    ))(i)?;
    Ok((i, token(t, pos)))
}

fn lex_identifier_or_reserved(i: Span) -> IResult<Span, Token> {
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
        "true" => True,
        "false" => False,
        _ => Ident(s.to_string()),
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

fn lex_integer(i: Span) -> IResult<Span, Token> {
    let (i, pos) = position(i)?;
    let (i, v) = map_parser(
        alt((recognize(tuple((digit1, tag("u32")))), recognize(digit1))),
        u64,
    )(i)?;
    Ok((i, token(Tok::IntLiteral(v), pos)))
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
        lex_newline,
        lex_invalid,
    ))(i)
}

fn parse_token_space(i: Span) -> IResult<Span, Vec<Token>> {
    alt((ws0, many1(lex_invalid)))(i)
}

fn lex_token_with_whitespace(i: Span) -> IResult<Span, Vec<Token>> {
    alt((
        map(tuple((parse_token_space, lex_token, parse_token_space)), |(mut a, mut b, mut c)| {
            let mut v = vec![];
            b.pre.append(&mut a);
            b.post.append(&mut c);
            v.push(b);
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
    fold_many0(
        lex_token_with_whitespace,
        Vec::new,
        |mut acc: Vec<_>, mut item| {
            acc.append(&mut item);
            acc
        },
    )(i)
}

fn lex_op<'a>(i: Span<'a>) -> IResult<Span<'a>, Token<'a>> {
    use Tok::*;
    let (i, pos) = position(i)?;
    let (i, t) = alt((
        maptag("==", Equals),
        maptag("!=", NotEquals),
        maptag("&&", And),
        maptag("||", Or),
        maptag("+=", PlusEq),
        maptag("-=", MinusEq),
        maptag("*=", MulEq),
        maptag("/=", DivEq),
        maptag("=", Assign),
        maptag("+", Plus),
        maptag("-", Minus),
        maptag("*", Mul),
        maptag("/", Div),
        maptag("?", Question),
        maptag("^", Caret),
        maptag("%", Percent),
        maptag("!", Not),
        maptag("in", In),
        maptag("is", Is),
    ))(i)?;

    Ok((i, token(t, pos)))
}

pub fn lex_eof<'a>(i: &'a str) -> IResult<Span<'a>, Vec<Token>> {
    let (i, pos) = position(span(i))?;
    let (i, mut r) = lex_tokens(i)?;
    r.push(token(Tok::EOF, pos));
    Ok((i, r))
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

    fn just_toks(r: &Vec<Token>) -> Vec<Tok> {
        r.iter().map(|v| v.tok.clone()).collect::<Vec<_>>()
    }

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
        assert!(lex_whitespace(span("")).is_err());

        let r = vec![
            ("\t  [] ", vec![LBracket, RBracket]),
            ("\t\t", vec![Tabs(2)]),
            ("\t", vec![Tabs(1)]),
            ("", vec![]),
            (" ", vec![Spaces(1)]),
        ];
        r.iter().for_each(|(q, a)| {
            let (_, result) = lex(q).unwrap();
            assert_eq!(just_toks(&result), *a);
        });
    }

    #[test]
    fn test_invalid() {
        let r = vec![
            ("$", vec![Invalid("$".into())]),
            (" $\t", vec![Invalid("$".into())]),
        ];
        r.iter().for_each(|(q, a)| {
            let (_, result) = lex(q).unwrap();
            assert_eq!(just_toks(&result), *a);
        });
    }

    #[test]
    fn test_maptag() {
        let s = " [ ] ";
        let (_, r) = lex(s.into()).unwrap();
        let tokens = r.iter().map(|v| v.tok.clone()).collect::<Vec<_>>();
        assert_eq!(vec![LBracket, RBracket], tokens);
        //assert_eq!(vec![Spaces(1), LBracket, Spaces(1), RBracket, Spaces(1)], tokens);
    }

    fn lexer_losslessness(s: &str) -> bool {
        println!("{:?}", &s);
        let (_, toks) = lex(s).unwrap();
        let tokens = Tokens::new(&toks[..]);
        println!("{:?}", tokens.toks());
        let restored = tokens.unlex();
        restored == tokens.unlex()
    }

    #[test]
    fn token() {
        let r = vec![
            ("=", vec![Tok::Assign]),
            ("==", vec![Tok::Equals]),
            ("-", vec![Tok::Minus]),
        ];
        r.iter().for_each(|(q, a)| {
            let (rest, result) = lex(q).unwrap();
            assert_eq!(rest.len(), 0);
            let tokens = Tokens::new(&result[..]);
            println!("{:?}", (&tokens.toks()));
            assert_eq!(&tokens.toks(), a);
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
            ];

        r.iter().for_each(|q| {
            assert!(lexer_losslessness(q))
        });
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
        r.iter().for_each(|(q, a)| {
            let (_, result) = lex(q).unwrap();
            let tokens = result.iter().map(|v| v.tok.clone()).collect::<Vec<_>>();
            assert_eq!(tokens, *a);
        });
    }
}
