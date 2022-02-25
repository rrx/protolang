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

fn tag_token<'a>(t: Tok) -> impl FnMut(Tokens<'a>) -> IResult<Tokens<'a>, Tokens<'a>> {
    verify(take(1usize), move |tokens: &Tokens<'a>| tokens.tok[0].tok == t)
}

fn parse_program(i: Tokens) -> IResult<Tokens, Program> {
    many0(alt((parse_prefix_expr, parse_lit_expr)))(i)
}

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

fn parse_lit_expr(input: Tokens) -> IResult<Tokens, Expr> {
    map(parse_literal, Expr::LitExpr)(input)
}

fn parse_atom_expr(input: Tokens) -> IResult<Tokens, Expr> {
    alt((
        parse_lit_expr,
        //parse_ident_expr,
        parse_prefix_expr,
        //parse_paren_expr,
        //parse_array_expr,
        //parse_hash_expr,
        //parse_if_expr,
        //parse_fn_expr,
    ))(input)
}

fn parse_prefix_expr(input: Tokens) -> IResult<Tokens, Expr> {
    let (i1, t1) = alt((tag_token(Tok::Plus), tag_token(Tok::Minus), tag_token(Tok::Not)))(input)?;
    if t1.tok.is_empty() {
        Err(Err::Error(error_position!(input, ErrorKind::Tag)))
    } else {
        let (i2, e) = parse_atom_expr(i1)?;
        match &t1.tok[0].tok {
            Tok::Plus => Ok((i2, Expr::PrefixExpr(Prefix::PrefixPlus, Box::new(e)))),
            Tok::Minus => Ok((i2, Expr::PrefixExpr(Prefix::PrefixMinus, Box::new(e)))),
            Tok::Not => Ok((i2, Expr::PrefixExpr(Prefix::PrefixNot, Box::new(e)))),
            _ => Err(Err::Error(error_position!(input, ErrorKind::Tag))),
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
            ("123", vec![Expr::LitExpr(Literal::IntLiteral(123))]),
            ("222 321", vec![Expr::LitExpr(Literal::IntLiteral(222)), Expr::LitExpr(Literal::IntLiteral(321))]),
            ("-123+5", vec![Expr::LitExpr(Literal::IntLiteral(123))]),
            ("123+5", vec![Expr::LitExpr(Literal::IntLiteral(123))]),
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

