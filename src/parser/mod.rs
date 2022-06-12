use nom::*;

use crate::ast::*;
use crate::lexer::*;
use crate::tokens::TokensList;
use crate::tokens::*;
use log::debug;
use nom::branch::*;
use nom::bytes::complete::take;
use nom::combinator::verify;
use nom::error::{context, ErrorKind};
use nom::multi::many0;
use nom::sequence::*;
use nom::Err;
use nom_locate::position;
use std::result::Result::*;

mod error;
mod pratt;
mod pratt1;
pub(crate) use error::{print_result, PResult};

mod unparse;
pub use unparse::{unparse_expr, Unparse};

pub(crate) fn tag_token<'a>(t: Tok) -> impl FnMut(Tokens<'a>) -> PResult<Tokens<'a>, Tokens<'a>> {
    verify(take_one_any, move |tokens: &Tokens<'a>| {
        let v = &tokens.tok[0].tok;
        v == &t
    })
}

// this function is super slow
pub(crate) fn tag_token2<'a>(t: Tok) -> impl FnMut(Tokens<'a>) -> PResult<Tokens<'a>, Tokens<'a>> {
    let s = t.into();
    context(
        s,
        verify(take_one_any, move |tokens: &Tokens<'a>| {
            let v: &'static str = tokens.tok[0].tok.clone().into();
            v == s
        }),
    )
}

/*
fn _parse_whitespace<'a>(i: Tokens<'a>) -> PResult<Tokens<'a>, Tokens<'a>> {
    verify(take_one_any, move |tokens: &Tokens<'a>| {
        let tok = &tokens.tok[0].tok;
        tok.is_whitespace()
    })(i)
}

fn _parse_whitespace_or_eof<'a>(i: Tokens<'a>) -> PResult<Tokens<'a>, Tokens<'a>> {
    context(
        "parse-whitespace-or-eof",
        alt((_parse_whitespace, tag_token(Tok::EOF))),
    )(i)
}
*/

fn parse_newline<'a>(i: Tokens<'a>) -> PResult<Tokens<'a>, Tokens<'a>> {
    verify(take_one_any, |tokens: &Tokens<'a>| {
        let tok = &tokens.tok[0].tok;
        if let Tok::Invalid(_) = tok {
            true
        } else {
            tok.is_newline()
        }
    })(i)
}

pub(crate) fn take_one_any(i: Tokens) -> PResult<Tokens, Tokens> {
    take(1usize)(i)
}

fn parse_not_stmt_end(i: Tokens) -> PResult<Tokens, Tokens> {
    context(
        "not-stmt-end",
        // take anything, even invalid tokens
        verify(take_one_any, |tokens: &Tokens| {
            let tok = &tokens.tok[0].tok;
            tok != &Tok::SemiColon && tok != &Tok::EOF
        }),
    )(i)
}

fn parse_stmt_end(i: Tokens) -> PResult<Tokens, Tokens> {
    context(
        "parse-stmt-end",
        alt((
            parse_newline,
            //parse_newline_or_eof,
            tag_token(Tok::EOF),
            tag_token(Tok::SemiColon),
        )),
    )(i)
}

pub fn parse_invalid(i: Tokens) -> PResult<Tokens, ExprNode> {
    context("parse-invalid", _parse_invalid)(i)
}
pub fn _parse_invalid(i: Tokens) -> PResult<Tokens, ExprNode> {
    // Don't return invalid if we have EOF
    if i.is_eof() {
        return Err(Err::Error(error_position!(i, ErrorKind::Eof)));
    }

    let loc = i.to_location().clone();
    let (i1, (r, end)) = pair(many0(parse_not_stmt_end), parse_stmt_end)(i)?;
    let s = r.iter().map(|t| t.unlex()).collect::<Vec<_>>().join("");
    let mut expr = ExprNode::new(Expr::Invalid(s), &loc);
    // handle trailing newline
    expr.context.append(end.expand_toks());
    Ok((i1, expr))
}

pub fn _parse_assignment_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    let (i, (mut ident, assign, mut expr, nl)) = tuple((
        parse_ident_expr,
        tag_token(Tok::Assign),
        parse_expr,
        parse_stmt_end,
    ))(i)?;

    // transfer surround from assign to nodes we store
    ident.context.append(assign.tok[0].s.pre.clone());
    expr.context.prepend(assign.tok[0].s.post.clone());

    // handle trailing newline
    expr.context.append(nl.expand_toks());

    Ok((i, expr))
}

pub fn parse_program(i: Tokens) -> PResult<Tokens, ExprNode> {
    let (i, (exprs, end)) = pair(
        context("program-start", many0(alt((parse_expr, parse_invalid)))),
        context("program-end", combinator::rest), //many0(parse_whitespace_or_eof)),
    )(i)?;

    let mut value = ExprNode::new(Expr::Program(exprs), &i.to_location());
    value.context.append(end.expand_toks());
    Ok((i, value))
}

pub fn parse_program_fn(i: Tokens) -> PResult<Tokens, ExprNode> {
    let (i, p) = parse_program(i)?;
    let loc = p.context.to_location();
    let params = Params::new(vec![], &loc);
    let f = Lambda::new(params, p, &loc);
    let value = ExprNode::new(Expr::Lambda(f), &i.to_location());
    Ok((i, value))
}

pub fn token_expr(i: Span) -> LResult<Span, ExprNode> {
    let t = Expr::Void;
    let mut node = ExprNode::new(t, &Location::default());

    /*
    let (i, mut node) = context(
        "parse-expr",
        alt((
            context("pratt-expr", pratt::parse_expr_pratt),
            context("declaration", ExprNode::parse_declaration),
            context("expr-lambda", ExprNode::parse_lambda),
        )),
    )(i)?;
    */

    // optionally end with a semicolon
    let (i, end) = many0(s_tag_token(Tok::SemiColon))(i)?;
    end.iter().for_each(|t| {
        node.context.append(t.expand_toks());
    });

    Ok((i, node))
}

pub fn parse_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    let (i, mut node) = context(
        "parse-expr",
        alt((
            //context("expr-eof", map(tag_token(Tok::EOF), |t| ExprNode::new_with_token(Expr::Void, &t.tok[0]))),
            context("pratt-expr", pratt::parse_expr_pratt),
            context("declaration", parse_declaration),
            context("expr-lambda", ExprNode::parse_lambda),
        )),
    )(i)?;

    // optionally end with a semicolon
    let (i, end) = many0(tag_token(Tok::SemiColon))(i)?;
    end.iter().for_each(|t| {
        node.context.append(t.expand_toks());
    });

    Ok((i, node))
}

pub fn token_block(i: Span) -> LResult<Span, ExprNode> {
    // consume LBrace
    let (i, pos) = position(i)?;
    let (i, left) = s_tag_token(Tok::LBrace)(i)?;
    // Parse a full expression
    let p = |i| token_expr(i);

    let (i, (t, right)) = sequence::pair(
        multi::many0(p),
        context("r-brace", s_tag_token(Tok::RBrace)),
    )(i)?;
    let t = Expr::Block(t);
    let loc = Location::from_position(&pos, &i);
    let mut node = ExprNode::new(t, &loc);
    node.context.prepend(left.expand_toks());
    node.context.append(right.expand_toks());
    Ok((i, node))
}

pub fn parse_block2(i: Tokens) -> PResult<Tokens, ExprNode> {
    // consume LBrace
    let (i, left) = tag_token(Tok::LBrace)(i)?;
    // Parse a full expression
    let p = |i| parse_expr(i);

    let (i, (t, right)) =
        sequence::pair(multi::many0(p), context("r-brace", tag_token(Tok::RBrace)))(i)?;
    let t = Expr::Block(t);
    let mut node = ExprNode::new(t, &i.to_location());
    node.context.prepend(left.expand_toks());
    node.context.append(right.expand_toks());
    Ok((i, node))
}

pub fn parse_block(i: Tokens) -> PResult<Tokens, ExprNode> {
    let (i, pos) = position(i)?;
    let (i, (left, expressions, right)) = tuple((
        tag_token(Tok::LBrace),
        many0(parse_expr),
        tag_token(Tok::RBrace),
    ))(i)?;
    let mut node = ExprNode::new(Expr::Block(expressions), &i.to_location());
    node.context.prepend(left.tok[0].expand_toks());
    node.context.append(right.tok[0].expand_toks());
    Ok((i, node))
}

pub fn token_group(i: Span) -> LResult<Span, ExprNode> {
    // consume LParen
    let (i, left) = s_tag_token(Tok::LParen)(i)?;

    // consume anything inside the parents, bp = 0
    let (i, mut node) = token_expr(i)?;

    let (i, right) = context("r-paren", s_tag_token(Tok::RParen))(i)?;
    node.context.prepend(left.expand_toks());
    node.context.append(right.expand_toks());
    Ok((i, node))
}

pub fn parse_group(i: Tokens) -> PResult<Tokens, ExprNode> {
    // consume LParen
    let (i, left) = tag_token(Tok::LParen)(i)?;

    // consume anything inside the parents, bp = 0
    let (i, mut node) = parse_expr(i)?;

    let (i, right) = context("r-paren", tag_token(Tok::RParen))(i)?;
    node.context.prepend(left.expand_toks());
    node.context.append(right.expand_toks());
    Ok((i, node))
}

pub fn token_index_expr(i: Span) -> LResult<Span, ExprNode> {
    let (i, left) = s_tag_token(Tok::LBracket)(i)?;
    // Parse a full expression
    let (i, mut node) = token_expr(i)?;
    let (i, right) = s_tag_token(Tok::RBracket)(i)?;
    node.context.prepend(left.expand_toks());
    node.context.append(right.expand_toks());
    Ok((i, node))
}

pub fn parse_index_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    let (i, left) = tag_token(Tok::LBracket)(i)?;
    // Parse a full expression
    let (i, mut node) = parse_expr(i)?;
    let (i, right) = tag_token(Tok::RBracket)(i)?;
    node.context.prepend(left.expand_toks());
    node.context.append(right.expand_toks());
    Ok((i, node))
}

pub fn token_list(i: Span) -> LResult<Span, ExprNode> {
    // consume LBracket
    let (i, pos) = position(i)?;
    let (i, left) = s_tag_token(Tok::LBracket)(i)?;
    let (i, t) = token_expr(i)?;
    // consume RBracket
    let (i, right) = context("r-bracket", s_tag_token(Tok::RBracket))(i)?;
    let t = Expr::List(vec![t]);
    let loc = Location::from_position(&pos, &i);
    let mut node = ExprNode::new(t, &loc);
    node.context.prepend(left.expand_toks());
    node.context.append(right.expand_toks());
    Ok((i, node))
}

pub fn parse_list(i: Tokens) -> PResult<Tokens, ExprNode> {
    // consume LBracket
    let (i, left) = tag_token(Tok::LBracket)(i)?;
    let (i, t) = parse_expr(i)?;
    // consume RBracket
    let (i, right) = context("r-bracket", tag_token(Tok::RBracket))(i)?;
    let t = Expr::List(vec![t]);
    let mut node = ExprNode::new(t, &i.to_location());
    node.context.prepend(left.expand_toks());
    node.context.append(right.expand_toks());
    Ok((i, node))
}

pub fn _parse_empty_stmt(i: Tokens) -> PResult<Tokens, Tokens> {
    tag_token(Tok::SemiColon)(i)
}

pub(crate) fn parse_declaration(i: Tokens) -> PResult<Tokens, ExprNode> {
    let (i, tlet) = tag_token(Tok::Let)(i)?;

    let token = &i.tok[0].tok;
    let is_mutable = token == &Tok::Mut;
    let mut m_toks = vec![];
    let i = if is_mutable {
        let (i, m) = take_one_any(i)?;
        m_toks = m.expand_toks();
        i
    } else {
        i
    };

    //debug!("is_mut {}", is_mutable);
    let (i, mut ident) = parse_ident(i)?;
    if is_mutable {
        ident.modifier = VarModifier::Mutable;
    }
    let mut node = ExprNode::new_with_token(Expr::Ident(ident), &i.tok[0]);
    node.context.prepend(m_toks);
    node.context.prepend(tlet.expand_toks());
    let (i, assign) = tag_token(Tok::Assign)(i)?;
    node.context.append(assign.expand_toks());

    let (i, rhs) = pratt::parse_expr_pratt(i)?;
    let loc = i.to_location();
    let expr = Expr::Binary(
        OperatorNode::new_with_location(Operator::Declare, loc),
        Box::new(node),
        Box::new(rhs),
    );
    let node = ExprNode::new(expr, &i.to_location());
    Ok((i, node))
}

fn parse_ident(i: Tokens) -> PResult<Tokens, Identifier> {
    let (i1, t1) = take_one_any(i)?;
    let token = &t1.tok[0];
    match &token.tok {
        Tok::Ident(s) => Ok((i1, Identifier::new(s.clone(), VarModifier::Default))),
        _ => Err(Err::Error(error_position!(i1, ErrorKind::Tag))),
    }
}

pub(crate) fn parse_ident_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    context("ident", _parse_ident_expr)(i)
}

fn _parse_ident_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    let (i1, t1) = take_one_any(i)?;
    let token = &t1.tok[0];
    match &token.tok {
        Tok::Ident(s) => {
            let expr = Expr::Ident(Identifier::new(s.clone(), VarModifier::Default));
            let node = ExprNode::new_with_token(expr, &token);
            Ok((i1, node))
        }
        _ => Err(Err::Error(error_position!(i1, ErrorKind::Tag))),
    }
}

impl ExprNode {
    fn parse_invalid(i: Tokens) -> PResult<Tokens, ExprNode> {
        let (i1, t1) = take_one_any(i)?;
        let token = &t1.tok[0];
        match &token.tok {
            Tok::Invalid(s) => {
                let expr = Expr::Invalid(s.clone());
                let node = ExprNode::new_with_token(expr, &token);
                Ok((i1, node))
            }
            _ => Err(Err::Error(error_position!(i1, ErrorKind::Tag))),
        }
    }

    pub(crate) fn parse_literal(i: Tokens) -> PResult<Tokens, ExprNode> {
        let (i1, t1) = take_one_any(i)?;
        let token = &t1.tok[0];
        let tok = &token.tok;

        if let Ok(lit) = tok.try_into() {
            let mut litnode = ExprNode::new(lit, &token.to_location());
            litnode.context.prepend(token.s.pre.clone());
            litnode.context.append(token.s.post.clone());
            Ok((i1, litnode))
        } else {
            Err(Err::Error(error_position!(i1, ErrorKind::Tag)))
        }
    }

    fn parse_lambda(i: Tokens) -> PResult<Tokens, ExprNode> {
        context("lambda-expr", Self::_parse_lambda)(i)
    }
    fn _parse_lambda(i: Tokens) -> PResult<Tokens, ExprNode> {
        let (i, (slash, idents, arrow)) = tuple((
            tag_token(Tok::Backslash),
            many0(parse_ident_expr),
            tag_token(Tok::LeftArrow),
        ))(i)?;

        let (i, mut body) = parse_expr(i)?;
        //debug!("slash: {:?}", &slash);
        //debug!("idents: {:?}", &idents);
        //debug!("body: {:?}", &body);
        let loc = slash.tok[0].to_location();
        let mut params = Params::new(idents, &loc);
        params.context.prepend(slash.tok[0].toks_post());
        params.context.append(arrow.tok[0].toks_pre());
        body.context.prepend(arrow.tok[0].toks_post());
        let mut lambda: ExprNode = Lambda::new(params, body, &loc).into();
        lambda.context.prepend(slash.tok[0].toks_pre());
        Ok((i, lambda))
    }
}

pub fn parse_file(filename: &str) -> anyhow::Result<ExprNode> {
    let contents = std::fs::read_to_string(filename.clone())
        .unwrap()
        .to_string();
    let mut lexer = crate::lexer::LexerState::default();
    let (_, _) = lexer.lex(contents.as_str()).unwrap();
    let (_, expr) = crate::parser::parse_program(lexer.tokens().clone()).unwrap();
    Ok(expr)
}

pub fn parse_str(s: &str) -> anyhow::Result<ExprNode> {
    let mut lexer = crate::lexer::LexerState::default();
    let (_, _) = lexer.lex(s).unwrap();
    let (_, expr) = crate::parser::parse_program(lexer.tokens().clone()).unwrap();
    Ok(expr)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::*;
    use crate::program::Program;
    use crate::sexpr::SExpr;
    use nom::multi::many1;
    use test_log::test;

    pub(crate) fn parser_losslessness(s: &str) -> bool {
        debug!("{:?}", &s);
        let mut lexer = LexerState::from_str_eof(s).unwrap();
        let tokens = lexer.tokens();
        //let toks = tokens.toks();
        //debug!("tokens {:?}", tokens);
        match parse_program(tokens) {
            Ok((prog_rest, prog)) => {
                if prog_rest.input_len() > 0 {
                    debug!("prog_rest {:?}", prog_rest.toks());
                }
                debug!("prog {:?}", (&prog));
                let s2 = prog.unlex();
                debug!("test {:?}", (s, &s2));
                s == s2
            }
            Err(nom::Err::Error(e)) => {
                //debug!("Error: {:?}", e);
                for (tokens, err) in e.errors {
                    debug!("error {:?}", (&err, tokens.toks()));
                }
                false
            }
            _ => unreachable!(),
        }
    }

    fn dump_expr(expr: &ExprNode) {
        debug!("Expr: {}", expr.unlex());
        debug!("\tSurround: {:?}", expr.context);
        for token in unparse_expr(expr, true) {
            debug!("\tToken:: {:?}", token);
        }
    }

    #[test]
    fn literal() {
        let r = vec!["1", "2 ", "\n1", "1\n"];
        r.iter().for_each(|v| {
            let mut lexer = LexerState::from_str(v).unwrap();
            let tokens = lexer.tokens();
            //let toks = tokens.toks();
            debug!("{:?}", (&tokens.toks()));
            let (rest, result) = ExprNode::parse_literal(tokens).unwrap();
            debug!("lit {:?}", (&result, rest.toks()));
            let restored = result.unlex();
            debug!("restored {:?}", (&restored));
            assert_eq!(v, &restored);
        });
    }

    #[test]
    fn ident() {
        let r = vec!["x", "x "];
        r.iter().for_each(|v| {
            let mut lexer = LexerState::from_str(v).unwrap();
            let tokens = lexer.tokens();
            //let toks = tokens.toks();
            debug!("{:?}", (&tokens.toks()));
            let (_, result) = parse_ident_expr(tokens).unwrap();
            debug!("ident {:?}", (&result));
            let restored = result.unlex();
            debug!("restored {:?}", (&restored));
            assert_eq!(v, &restored);
        });
    }

    #[test]
    fn expressions() {
        let r = vec![
            "\n1",
            "\n1\n",
            "x()",
            "x( 1  2 )",
            "x(\n)",
            "x( 1 \n 2 )",
            "(x)",
            "1+2",
            "1 + 2",
            " 1 + 2 ",
            "c()",
            "c[1]",
            "1 + 2\nx + 4",
            "1 + 2 + \n\tx + 4",
            "1 + 2  x + 4",
            "x = 1",
            "x = 1 y = 2",
            "x + y = 1 + 2",
            "y = 1 + 2",
            "\\x -> y^2 ",
            "\\x -> y^2\n",
            "\\x -> y^2;\n",
            "let x=1;(x+1);",
        ];
        r.iter().for_each(|v| {
            let mut lexer = LexerState::from_str_eof(v).unwrap();
            let tokens = lexer.tokens();
            debug!("v {:?}", (&v));
            //debug!("tokens: {:?}", (&tokens));

            let mut p = many1(parse_expr);
            match p(tokens) {
                Ok((rest, result)) => {
                    debug!("p {:?}", (&result));
                    debug!("rest {:?}", (&rest));
                    result.iter().for_each(|v| {
                        debug!("S: {}", (&v.sexpr().unwrap()));
                    });
                    let restored = result
                        .into_iter()
                        .map(|v| v.unlex())
                        .collect::<Vec<_>>()
                        .join("");
                    debug!("{:?}", (&v, &restored));
                    assert_eq!(v, &restored);
                }
                Err(nom::Err::Error(e)) => {
                    for (tokens, err) in e.errors {
                        debug!("error {:?}", (&err, tokens.toks()));
                    }
                    assert!(false);
                }
                _ => unreachable!(),
            }
        });
    }

    #[test]
    fn declaration() {
        // parse a single statement
        let r = vec![
            ("let x = 1", false),
            ("let mut x = y", true),
            //("let mut x = y\nlet x =2 ", true)
        ];
        r.iter().for_each(|(v, is_mut)| {
            let mut lexer = LexerState::from_str_eof(v).unwrap();
            let tokens = lexer.tokens();
            debug!("q: {}", v);
            let (i, exprs) = many1(parse_declaration)(tokens).unwrap();
            let expr = exprs.get(0).unwrap();
            match &expr.value {
                Expr::Binary(_, left, _) => {
                    let ident = left.try_ident().unwrap();
                    assert_eq!(ident.is_mut(), *is_mut);
                }
                _ => unreachable!(),
            }
            exprs.iter().for_each(|expr| {
                debug!("a: {:?}", &expr);
                expr.debug();
            });
            debug!("rest: {:?}", &i.toks());
            assert!(i.input_len() == 0 || i.toks() == vec![Tok::EOF]);
        });
    }

    #[test]
    fn statements() {
        // parse a single statement
        let r = vec![
            "1 + 2;",
            "1 ;",
            "1 + 2 ",
            "312 \n",
            "x=1",
            "x = 1\n",
            "x = y * 2\n",
            //";",
            "\\x -> y;",
            "\\x -> y^2;\n",
            "x + 1 +\n  2\n  \nx+1;\n",
            "f = \\x -> { x^2 };",
            "f = \\x -> { x^2; };",
            "f = \\x -> { x^2;;;; };;;;",
        ];
        r.iter().for_each(|v| {
            let mut lexer = LexerState::from_str_eof(v).unwrap();
            let tokens = lexer.tokens();
            //let toks = tokens.toks();
            debug!("q: {}", v);
            //debug!("toks: {:?}", (&toks));
            let mut p = many0(parse_expr);

            match p(tokens) {
                Ok((rest, results)) => {
                    results.iter().for_each(|r| {
                        debug!("result {:?}", (&r));
                    });

                    for expr in &results {
                        dump_expr(&expr);
                    }

                    let restored = results
                        .iter()
                        .map(|s| s.unlex())
                        .collect::<Vec<_>>()
                        .join("");
                    debug!("cmp {:?}", (&v, &restored));
                    assert_eq!(v, &restored);
                    //debug!("remaining {:?}", (&rest.toks()));

                    if rest.input_len() > 0 {
                        debug!("tokens remaining {:?}", (&rest));
                    }

                    //assert_eq!(rest.toks().len(), 0);
                    //assert!(rest.input_len() == 0);
                }
                Err(nom::Err::Error(e)) => {
                    for (tokens, err) in e.errors {
                        debug!("error {:?}", (&err, tokens.toks()));
                    }
                    assert!(false);
                }
                _ => unreachable!(),
            }
        });
    }

    #[test]
    fn invalid() {
        let mut r = vec![
            ("$", vec![Tok::Invalid("$".into())]),
            (
                "$\nasdf",
                vec![
                    Tok::Invalid("$".into()),
                    Tok::NL(1),
                    Tok::Ident("asdf".into()),
                ],
            ),
        ];
        r.iter_mut().for_each(|(q, a)| {
            debug!("q {:?}", (&q));
            a.push(Tok::EOF);
            let mut lexer = LexerState::from_str_eof(q).unwrap();
            let tokens = lexer.tokens();
            debug!("tokens: {:?}", (&tokens.toks()));
            let r = parse_program(tokens);
            //print_result(&r);
            match r {
                Ok((rest, prog)) => {
                    debug!("x{:?}", (&rest.toks(), &prog));
                    assert_eq!(unparse_expr(&prog, true), *a);
                    assert!(rest.input_len() == 0);
                }
                Err(nom::Err::Error(e)) => {
                    for (tokens, err) in e.errors {
                        debug!("error {:?}", (&err, tokens.toks()));
                    }
                    assert!(false);
                }
                _ => unreachable!(),
            }
            assert!(parser_losslessness(q));
        });
    }

    #[test]
    fn unparse() {
        let r = vec![
            "",
            "123\n",
            "\n123",
            "123",
            "321 ",
            "$",
            "$$",
            "$\n",
            "$\r\n",
            "$\t",
            "$\r",
            "\n$\n",
            "\r\n$\r\n",
            "\t$\t",
            "\r$\r",
            "1+2",
            "+ 1",
            "+ 1 / 2",
            " +  1  /  ( 2  -  5 ) ",
            "x+1",
            "(((((0)))))",
            "\"asdf\"",
            //utf-8 test
            "\"🎁\"",
            "3 - 0",
            "-x^(y+1)",
            "  y  <  y ",
            "true",
            "\\x -> x^2;\n",
            "x^2 + y;\n1+2;",
            "\\ x y -> (x^2 + y);\n",
            " \\ x  y z-> (x ^ 2 + y)\n;",
            "x( 1 2 3)",
            " { } ",
            " { x ;\n y \n; } ",
            "f = \\x -> x^2\n; ",
            //"f = \\x -> { \n}\n",
            "f = \\x -> { v = x^2; \n };\n",
            "f = \\x -> { y = 1;\n };\n",
            "\\x -> { y = 1;\n };\n",
            "\\x -> { cx=y;\n };\n",
            "x = \\ -> { 0\n; };\n",
            "x = \\ -> { true;\n };\n",
            "c()",
            ";",
            "\n;\n",
            "{\n;\n}",
            "(x))",
            "((x))",
            "x(y)",
            "x[y]",
            "x = 1 y = 2",
            "x=y=z",
            "x!",
            "a! ^ b",
            "let x=1;(x+1);",
        ];
        r.iter().for_each(|v| {
            assert!(parser_losslessness(v));
        });
    }

    #[test]
    fn block() {
        let r = vec!["x = 1 + 2 ", "x = \\x -> { 0\n } \n", "{ }"];
        r.iter().for_each(|v| {
            debug!("q {:?}", (&v));
            let mut lexer = LexerState::from_str_eof(v).unwrap();
            let tokens = lexer.tokens();
            debug!("tokens: {:?}", (&tokens.toks()));
            match parse_program(tokens) {
                Ok((rest, prog)) => {
                    debug!("x{:?}", (&rest.toks(), &prog));
                    assert!(rest.input_len() == 0);
                }
                Err(nom::Err::Error(e)) => {
                    for (tokens, err) in e.errors {
                        debug!("error {:?}", (&err, tokens.toks()));
                    }
                    assert!(false);
                }
                _ => unreachable!(),
            }
            assert!(parser_losslessness(v));
        });
    }

    #[test]
    fn sexpr_prog() {
        let r = vec![
            ("let x = 1;(x+1);", "(program (let x 1) (+ x 1))"),
            ("x = 1;(x+1);", "(program (= x 1) (+ x 1))"),
            ("+1", "(program (+ 1))"),
            ("+1;\n+1", "(program (+ 1) (+ 1))"),
            ("a < b < c", "(program (chain (< a b) (< b c)))"),
        ];

        r.iter().for_each(|(q, a)| {
            debug!("q {:?}", (&q));

            let mut lexer = LexerState::from_str(q).unwrap();
            let i = lexer.tokens();

            debug!("tokens: {:?}", (i.toks()));

            i.iter_elements().for_each(|t| {
                debug!("{:?}", t);
            });

            let r = parse_program(i);
            print_result(&r);
            match r {
                Ok((i, expr)) => {
                    debug!("NODE {:?}", (&expr.unparse()));
                    debug!("REM {:?}", (&i.toks()));
                    match expr.sexpr() {
                        Ok(sexpr) => {
                            let rendered = format!("{}", &sexpr);
                            debug!("sexpr {:?}", (&q, &sexpr, &rendered, a));
                            assert_eq!(rendered, a.to_string());
                            //assert_eq!(i.toks(), vec![Tok::EOF]); //i.input_len());
                            //assert_eq!(0, i.input_len());
                        }
                        Err(_) => {
                            assert!(false);
                        }
                    }
                }
                Err(_) => {
                    assert!(false);
                }
            }
        });
    }

    #[test]
    fn interpret_expressions() {
        let env = crate::eval::Environment::default();
        //let mut interp = crate::eval::Interpreter::default();
        let mut program = Program::new();
        let r = program.eval("assert(1 == 1 == 1 == 1)", env).unwrap();
        let r = program
            .eval(
                "
            let a = 1
            let b = 1
            let c = 1
            let d = 2
            assert(a == b == c != d)
        ",
                r.env,
            )
            .unwrap();
    }

    #[test]
    fn locations() {
        let env = crate::ir::base_env();
        let mut program = Program::new();
        let r = program.check_str("assert(1 == 1)", env).unwrap();
        program.print();
    }
}
