use nom::*;

use crate::tokens::*;
use crate::ast::*;
use nom::branch::*;
use nom::bytes::complete::{take, take_while};
use nom::combinator::{map, opt, verify};
use nom::error::{Error, ErrorKind};
use nom::multi::many0;
use nom::sequence::*;
use nom::Err;
use std::result::Result::*;

//fn tag_token(t: Tok) -> impl FnMut(Tokens) -> IResult<Tokens, Tokens> {
    //verify(take(1usize), |t: &Tokens| t.tok[0].tok == t)
//}

fn parse_program(i: Tokens) -> IResult<Tokens, Program> {
    many0(parse_literal)(i)
}

fn parse_whitespace(i: Tokens) -> IResult<Tokens, Tokens> {
    let (i1, t1) = take(1usize)(i)?;
    if t1.tok.is_empty() {
        Err(Err::Error(Error::new(i, ErrorKind::Tag)))
    } else {
        match &t1.tok[0].tok {
            Tok::Tabs(_) => Ok((i1, t1)),
            Tok::Spaces(_) => Ok((i1, t1)),
            _ => Err(Err::Error(Error::new(i, ErrorKind::Tag))),
        }
    }

    //take_while(|c: Token| {
        //match c.tok {
            //Tok::Tabs(_) => true,
            //Tok::Spaces(_) => true,
            //_ => false
        //})(i)
}

//fn parse_token(i: Tokens) -> IResult<Tokens, Tokens> {
//}

fn parse_literal(input: Tokens) -> IResult<Tokens, Literal> {
    let (i1, t1) = take(1usize)(input)?;
    if t1.tok.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match &t1.tok[0].tok {
            Tok::IntLiteral(u) => Ok((i1, Literal::IntLiteral(*u))),
            Tok::StringLiteral(s) => Ok((i1, Literal::StringLiteral(s.clone()))),
            Tok::BoolLiteral(b) => Ok((i1, Literal::BoolLiteral(*b))),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokens::*;
    use crate::lexer::*;

    #[test]
    fn literal() {
        let r = vec![
            ("123", vec![Literal::IntLiteral(123)]),
            ("222 321", vec![Literal::IntLiteral(222), Literal::IntLiteral(321)]),
        ];
        r.iter().for_each(|(q, a)| {
            let (rest, result) = lex(q).unwrap();
            assert_eq!(rest.len(), 0);
            let tokens = Tokens::new(&result[..]);
            let (rest, result) = parse_program(tokens).unwrap();
            assert_eq!(rest.input_len(), 0);
            assert_eq!(result, *a);
        });

    }


}

