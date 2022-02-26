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
    terminated(many0(parse_expr), tag_token(Tok::EOF))(i)
}

fn parse_expr(input: Tokens) -> IResult<Tokens, Expr> {
    parse_pratt_expr(input, Precedence::PLowest)
}

fn parse_pratt_expr(input: Tokens, precedence: Precedence) -> IResult<Tokens, Expr> {
    let (i1, left) = parse_atom_expr(input)?;
    go_parse_pratt_expr(i1, precedence, left)
}

fn go_parse_pratt_expr(input: Tokens, precedence: Precedence, left: Expr) -> IResult<Tokens, Expr> {
    let (i1, t1) = take(1usize)(input)?;

    if t1.tok.is_empty() {
        Ok((i1, left))
    } else {
        let preview = &t1.tok[0];
        let p = infix_op(&preview.tok);
        match p {
            //(Precedence::PCall, _) if precedence < Precedence::PCall => {
                //let (i2, left2) = parse_call_expr(input, left)?;
                //go_parse_pratt_expr(i2, precedence, left2)
            //}
            //(Precedence::PIndex, _) if precedence < Precedence::PIndex => {
                //let (i2, left2) = parse_index_expr(input, left)?;
                //go_parse_pratt_expr(i2, precedence, left2)
            //}
            (ref peek_precedence, _) if precedence < *peek_precedence => {
                let (i2, left2) = parse_infix_expr(input, left)?;
                go_parse_pratt_expr(i2, precedence, left2)
            }
            _ => Ok((input, left)),
        }
    }
}

fn parse_ident(input: Tokens) -> IResult<Tokens, Ident> {
    let (i1, t1) = take(1usize)(input)?;
    if t1.tok.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match &t1.tok[0].tok {
            Tok::Ident(name) => Ok((i1, Ident(name.clone()))),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

fn parse_ident_expr(input: Tokens) -> IResult<Tokens, Expr> {
    map(parse_ident, Expr::IdentExpr)(input)
}

fn parse_literal(input: Tokens) -> IResult<Tokens, Literal> {
    let (i1, t1) = take(1usize)(input)?;
    if t1.tok.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match &t1.tok[0].tok {
            Tok::IntLiteral(u) => Ok((i1, Literal::IntLiteral(*u))),
            Tok::FloatLiteral(u) => Ok((i1, Literal::FloatLiteral(*u))),
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
        parse_ident_expr,
        parse_prefix_expr,
        parse_paren_expr,
        //parse_array_expr,
        //parse_hash_expr,
        //parse_if_expr,
        //parse_fn_expr,
    ))(input)
}

fn parse_paren_expr(i: Tokens) -> IResult<Tokens, Expr> {
    let (i, (left, expr, right)) = tuple((tag_token(Tok::LParen), parse_expr, tag_token(Tok::RParen)))(i)?;
    Ok((i, expr))
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

fn parse_infix_expr(input: Tokens, left: Expr) -> IResult<Tokens, Expr> {
    let (i1, t1) = take(1usize)(input)?;
    if t1.tok.is_empty() {
        Err(Err::Error(error_position!(input, ErrorKind::Tag)))
    } else {
        let next = &t1.tok[0];
        let (precedence, maybe_op) = infix_op(&next.tok);
        match maybe_op {
            None => Err(Err::Error(error_position!(input, ErrorKind::Tag))),
            Some(op) => {
                let (i2, right) = parse_pratt_expr(i1, precedence)?;
                Ok((i2, Expr::InfixExpr(op, Box::new(left), Box::new(right))))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokens::*;
    use crate::lexer::*;
    use Expr::*;
    use Literal::*;

    #[test]
    fn literal() {
        let r = vec![
            ("123 - 0", vec![InfixExpr(Infix::Minus, Box::new(LitExpr(IntLiteral(123))), Box::new(LitExpr(IntLiteral(0))))]),
            ("+123 / 1", vec![InfixExpr(
                    Infix::Divide,
                    Box::new(PrefixExpr(Prefix::PrefixPlus, Box::new(LitExpr(IntLiteral(123))))),
                    Box::new(LitExpr(IntLiteral(1)))
                    )]),
            ("123", vec![Expr::LitExpr(Literal::IntLiteral(123))]),
        ];
        r.iter().for_each(|(q, a)| {
            let (rest, result) = lex_eof(q).unwrap();
            assert_eq!(rest.len(), 0);
            let tokens = Tokens::new(&result[..]);
            let (rest, result) = parse_program(tokens).unwrap();
            println!("{:?}", (&rest, &result));
            assert_eq!(rest.input_len(), 0);
            assert_eq!(result, *a);
        });

    }


}

