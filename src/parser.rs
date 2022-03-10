use nom::*;

use crate::ast::*;
use crate::results::*;
use crate::tokens::*;
use nom::branch::*;
use nom::bytes::complete::take;
use nom::combinator::{into, verify};
use nom::error::{context, ErrorKind, VerboseError};
use nom::multi::many0;
use nom::sequence::*;
use nom::Err;
use std::result::Result::*;

type PResult<I, O> = IResult<I, O, VerboseError<I>>;

fn tag_token<'a>(t: Tok) -> impl FnMut(Tokens<'a>) -> PResult<Tokens<'a>, Tokens<'a>> {
    context(
        "tag-token",
        verify(take_one_any, move |tokens: &Tokens<'a>| {
            tokens.tok[0].tok == t
        }),
    )
}

fn _parse_take1_with_whitespace<'a>(i: Tokens) -> PResult<Tokens, Token> {
    let (i1, (ws0, t1, ws1)) = tuple((
        many0(parse_whitespace),
        take_one_any,
        many0(parse_whitespace),
    ))(i)?;
    let mut token = t1.tok[0].clone();
    token
        .s
        .prepend(ws0.iter().map(|v| v.expand_toks()).flatten().collect());
    token
        .s
        .append(ws1.iter().map(|v| v.expand_toks()).flatten().collect());
    //token.s.append(ws1.toks());
    Ok((i1, token))
}

fn _tag_not_token<'a>(t: Tok) -> impl FnMut(Tokens<'a>) -> PResult<Tokens<'a>, Tokens<'a>> {
    context(
        "tag-not-token",
        verify(take_one_any, move |tokens: &Tokens<'a>| {
            tokens.tok[0].tok != t
        }),
    )
}

fn _tag_not_newline<'a>() -> impl FnMut(Tokens<'a>) -> PResult<Tokens<'a>, Tokens<'a>> {
    context(
        "tag-not-newline",
        verify(take_one_any, move |tokens: &Tokens<'a>| {
            !tokens.tok[0].tok.is_newline()
        }),
    )
}

fn parse_whitespace<'a>(i: Tokens<'a>) -> PResult<Tokens<'a>, Tokens<'a>> {
    verify(take_one_any, move |tokens: &Tokens<'a>| {
        let tok = &tokens.tok[0].tok;
        tok.is_whitespace()
    })(i)
}

fn parse_whitespace_or_eof<'a>(i: Tokens<'a>) -> PResult<Tokens<'a>, Tokens<'a>> {
    context(
        "parse-whitespace-or-eof",
        alt((parse_whitespace, tag_token(Tok::EOF))),
    )(i)
}

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

fn _parse_invalid<'a>(i: Tokens<'a>) -> PResult<Tokens<'a>, Tokens<'a>> {
    verify(take_one_any, |tokens: &Tokens<'a>| {
        let tok = &tokens.tok[0].tok;
        if let Tok::Invalid(_) = tok {
            true
        } else {
            false
        }
    })(i)
}

fn take_one_any(i: Tokens) -> PResult<Tokens, Tokens> {
    take(1usize)(i)
}

//fn _take_one_valid(i: Tokens) -> PResult<Tokens, Tokens> {
//let (i, (_, tokens)) = pair(many0(parse_invalid), take(1usize))(i)?;
//let tok = &tokens.tok[0].tok;
//if let Tok::Invalid(c) = tok {
//tokens.result(Results::Error(format!("Invalid Char: {:?}", c), 0));
//}
//Ok((i, tokens))
//}

fn parse_newline_or_eof(i: Tokens) -> PResult<Tokens, Tokens> {
    context(
        "newline-or-eof",
        verify(take_one_any, |tokens: &Tokens| {
            let tok = &tokens.tok[0].tok;
            tok.is_newline() || tok == &Tok::EOF
        }),
    )(i)
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

pub fn parse_statements0(i: Tokens) -> PResult<Tokens, Vec<StmtNode>> {
    many0(alt((parse_statement, parse_invalid_stmt)))(i)
}

pub fn parse_statement(i: Tokens) -> PResult<Tokens, StmtNode> {
    //println!("parse_statment: {:?}", i);
    let (i, (before, mut stmt, after)) = tuple((
        many0(parse_whitespace_or_eof),
        alt((
            //parse_block_stmt,
            parse_expr_stmt,
            parse_assignment_stmt,
            //parse_invalid_stmt,
            //parse_literal_stmt,
        )),
        many0(parse_stmt_end), //parse_whitespace_or_eof),
    ))(i)?;
    println!("{:?}", stmt);
    stmt.s.prepend(
        before
            .into_iter()
            .map(|v| v.tok[0].expand_toks())
            .flatten()
            .collect(),
    );
    stmt.s.append(
        after
            .into_iter()
            .map(|v| v.tok[0].expand_toks())
            .flatten()
            .collect(),
    );
    Ok((i, stmt))
}

pub fn parse_block_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    let (i, (left, stmts, right)) = tuple((
        tag_token(Tok::LBrace),
        many0(parse_statement),
        tag_token(Tok::RBrace),
    ))(i)?;
    let mut expr = ExprNode::new(Expr::Block(stmts), left.to_location());
    expr.s.prepend(left.tok[0].expand_toks());
    expr.s.append(right.tok[0].expand_toks());
    Ok((i, expr))
}

pub fn parse_block_stmt(i: Tokens) -> PResult<Tokens, StmtNode> {
    let (i, (left, stmts, right)) = tuple((
        tag_token(Tok::LBrace),
        many0(parse_statement),
        tag_token(Tok::RBrace),
    ))(i)?;
    let mut stmt = StmtNode::new(Stmt::Block(stmts), i.to_location());
    stmt.s.prepend(left.tok[0].expand_toks());
    stmt.s.append(right.tok[0].expand_toks());
    Ok((i, stmt))
}

pub fn parse_assignment_stmt(i: Tokens) -> PResult<Tokens, StmtNode> {
    let (i, (mut ident, assign, mut expr, nl)) = tuple((
        parse_ident,
        tag_token(Tok::Assign),
        parse_expr,
        parse_stmt_end,
    ))(i)?;

    // transfer surround from assign to nodes we store
    ident.s.append(assign.tok[0].s.pre.clone());
    expr.s.prepend(assign.tok[0].s.post.clone());

    let mut stmt = StmtNode::new(Stmt::Assign(ident, expr), i.to_location());
    // handle trailing newline
    stmt.s.append(nl.expand_toks());

    Ok((i, stmt))
}

pub fn _parse_invalid_stmt2(i: Tokens) -> PResult<Tokens, StmtNode> {
    let (mut i1, r) = many0(take_one_any)(i.clone())?;
    let s = r.iter().map(|t| t.unlex()).collect::<Vec<_>>().join("");
    i1.result(Results::Error(format!("Invalid Statement: {}", s), 0));
    let stmt = StmtNode::new(Stmt::Invalid(s), i.to_location());
    // handle trailing newline
    //stmt.s.append(end.expand_toks());
    //println!("invalid: {:?}", (i.toks(), &stmt, r, end));
    Ok((i1, stmt))
}

pub fn parse_invalid_stmt(i: Tokens) -> PResult<Tokens, StmtNode> {
    let (mut i1, (r, end)) = pair(many0(parse_not_stmt_end), parse_stmt_end)(i.clone())?;
    let s = r.iter().map(|t| t.unlex()).collect::<Vec<_>>().join("");
    i1.result(Results::Error(format!("Invalid Statement: {}", s), 0));
    let mut stmt = StmtNode::new(Stmt::Invalid(s), i.to_location());
    // handle trailing newline
    stmt.s.append(end.expand_toks());
    println!("invalid: {:?}", (i.toks(), &stmt, r, end));
    Ok((i1, stmt))
}

pub fn parse_expr_stmt(i: Tokens) -> PResult<Tokens, StmtNode> {
    let (i, (expr, nl)) = pair(parse_expr, parse_stmt_end)(i)?;
    let mut stmt: StmtNode = expr.into();
    stmt.s.append(nl.expand_toks());
    Ok((i, stmt))
}

pub fn parse_program_with_results(i: Tokens) -> (Option<Program>, Vec<Results>) {
    match parse_program(i) {
        Ok((prog_rest, prog)) => {
            let mut results = vec![];
            if prog_rest.tok.len() > 0 {
                results.push(Results::Warning(
                    format!("Not all tokens parsed: {:?}", prog_rest.toks()),
                    prog_rest.to_location().line,
                ));
                (None, results)
            } else {
                (Some(prog), results)
            }
        }
        Err(nom::Err::Error(e)) => {
            let results = e
                .errors
                .iter()
                .map(|(tokens, err)| {
                    Results::Error(format!("Error: {:?}, {:?}", err, tokens.toks()), 0)
                })
                .collect();
            (None, results)
        }
        _ => unreachable!(),
    }
}

pub fn parse_program(i: Tokens) -> PResult<Tokens, Program> {
    let (i, (stmts, end)) = pair(parse_statements0, many0(parse_whitespace_or_eof))(i)?;
    let mut value = Program::new(stmts);
    value
        .s
        .append(end.iter().map(|v| v.expand_toks()).flatten().collect());
    Ok((i, value))
}

pub fn parse_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    context("expr", _parse_expr)(i)
}
pub fn _parse_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    parse_pratt_expr(i, Precedence::PLowest)
}

fn parse_pratt_expr(input: Tokens, precedence: Precedence) -> PResult<Tokens, ExprNode> {
    let (i1, left) = parse_atom(input)?;
    println!("pratt atom: {:?}", &left.value);
    let r = go_parse_pratt_expr(i1, precedence, left);
    println!("pratt r: {:?}", &r);
    r
}

fn go_parse_pratt_expr(
    input: Tokens,
    precedence: Precedence,
    left: ExprNode,
) -> PResult<Tokens, ExprNode> {
    //println!("go: {:?}", &input);
    let (i1, t1) = take_one_any(input.clone())?;

    // if we have a LHS, and nothing remains, just return LHS
    if t1.tok.is_empty() {
        println!("go-empty");
        Ok((i1, left))
    } else {
        // inspect the next element, if it's a valid op
        let preview = &t1.tok[0];
        let p = infix_op(&preview.tok);
        //println!("infix: {:?}", (&preview, &p));
        match p {
            //(Precedence::PExp, _) if precedence < Precedence::PExp => {
            //let (i2, left2) = parse_caret_expr(input, left)?;
            //go_parse_pratt_expr(i2, precedence, left2)
            //}

            (Precedence::PCall, _) if precedence < Precedence::PCall => {
                let (i2, left2) = parse_call_expr(input, left)?;
                go_parse_pratt_expr(i2, precedence, left2)
            }

            (Precedence::PIndex, _) if precedence < Precedence::PIndex => {
                let (i2, left2) = parse_index_expr(input, left)?;
                go_parse_pratt_expr(i2, precedence, left2)
            }

            // if the precedence of the next op is greater then the current precedence,
            // then we include it in this expr, and try to parse the RHS
            (ref peek_precedence, _) if precedence < *peek_precedence => {
                let (i2, left2) = parse_infix_expr(input, left)?;
                go_parse_pratt_expr(i2, precedence, left2)
            }
            // otherwise we just return the LHS
            _ => Ok((input, left)),
        }
    }
}

fn parse_ident(i: Tokens) -> PResult<Tokens, Ident> {
    context("ident", _parse_ident)(i)
}
fn _parse_ident(i: Tokens) -> PResult<Tokens, Ident> {
    let (i1, t1) = take_one_any(i.clone())?;
    let token = &t1.tok[0];
    match Ident::from_token(token.clone()) {
        Some(ident) => Ok((i1, ident)),
        _ => Err(Err::Error(error_position!(i, ErrorKind::Tag))),
    }
}

fn parse_literal(i: Tokens) -> PResult<Tokens, LiteralNode> {
    let (i1, t1) = take_one_any(i.clone())?;
    let token = &t1.tok[0];
    let tok = &token.tok;

    if let Ok(lit) = tok.try_into() {
        let mut litnode = LiteralNode::new(lit, token.to_location());
        litnode.s.prepend(token.s.pre.clone());
        litnode.s.append(token.s.post.clone());
        //println!("LIT: {:?}", litnode);
        Ok((i1, litnode))
    } else {
        Err(Err::Error(error_position!(i, ErrorKind::Tag)))
    }
}

fn parse_atom(i: Tokens) -> PResult<Tokens, ExprNode> {
    context(
        "atom",
        alt((
            //caret really isn't an atom, but this is how we give it precedence over prefix
            //parse_apply2_expr,
            //parse_apply1_expr,
            parse_caret_expr,
            into(parse_literal),
            into(parse_ident),
            parse_prefix_expr,
            parse_paren_expr,
            parse_lambda_expr,
            parse_block_expr,
            //parse_array_expr,
            //parse_hash_expr,
            //parse_if_expr,
            //parse_fn_expr,
        )),
    )(i)
}

fn _parse_apply_end(i: Tokens) -> PResult<Tokens, Tokens> {
    alt((tag_token(Tok::SemiColon), parse_newline_or_eof))(i)
}

//fn _parse_apply1_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    //let (i, (left, ident, args, right)) = tuple((
        //tag_token(Tok::LParen),
        //parse_ident,
        //many0(parse_expr),
        //tag_token(Tok::RParen),
    //))(i)?;
    //let mut lambda = ExprNode::new(Expr::Apply(ident.clone(), args), ident.loc);
    //lambda.s.prepend(left.expand_toks());
    //lambda.s.append(right.expand_toks());
    //Ok((i, lambda))
//}

//fn parse_apply2_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    //let (i, (mut ident, left, args, right)) = tuple((
        //parse_ident,
        //tag_token(Tok::LParen),
        //many0(parse_expr),
        //tag_token(Tok::RParen),
    //))(i)?;
    //ident.s.append(left.expand_toks());
    //let mut lambda = ExprNode::new(Expr::Apply(ident.clone(), args), ident.loc);
    //lambda.s.append(right.expand_toks());
    //Ok((i, lambda))
//}

fn parse_lambda_end(i: Tokens) -> PResult<Tokens, Tokens> {
    alt((tag_token(Tok::SemiColon), parse_newline_or_eof))(i)
}

fn parse_lambda_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    context("lambda-expr", _parse_lambda_expr)(i)
}
fn _parse_lambda_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    let (i, (slash, idents, arrow, mut body, end)) = tuple((
        tag_token(Tok::Backslash),
        many0(parse_ident),
        tag_token(Tok::LeftArrow),
        alt((parse_block_stmt, into(parse_atom), parse_expr_stmt)),
        parse_lambda_end,
    ))(i)?;
    println!("slash: {:?}", &slash);
    println!("idents: {:?}", &idents);
    println!("body: {:?}", &body);
    let mut params = Params::new(idents);
    params.s.prepend(slash.tok[0].toks_post());
    params.s.append(arrow.tok[0].toks_pre());
    body.s.prepend(arrow.tok[0].toks_post());
    let loc = slash.tok[0].to_location();
    let mut lambda: ExprNode = Lambda::new(params, body, loc.clone()).into();
    lambda.s.prepend(slash.tok[0].toks_pre());
    lambda.s.append(end.expand_toks());
    println!("lambda: {:?}", &lambda);

    Ok((i, lambda))
}

fn parse_caret_side(i: Tokens) -> PResult<Tokens, ExprNode> {
    alt((into(parse_literal), into(parse_ident)))(i)
}

fn parse_caret_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    let (i, (left, op, right)) =
        tuple((parse_caret_side, tag_token(Tok::Caret), parse_caret_side))(i)?;
    let op = Binary::from_token(op.tok[0].clone()).unwrap();
    let loc = op.loc.clone();
    let expr = ExprNode::new(Expr::Binary(op, Box::new(left), Box::new(right)), loc);
    Ok((i, expr))
}

fn parse_paren_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    let (i, (left, mut expr, right)) =
        tuple((tag_token(Tok::LParen), parse_expr, tag_token(Tok::RParen)))(i)?;
    expr.s.prepend(left.expand_toks());
    expr.s.append(right.expand_toks());
    Ok((i, expr))
}

fn parse_prefix(i: Tokens) -> PResult<Tokens, Unary> {
    let (i, tokens) = alt((
        tag_token(Tok::Plus),
        tag_token(Tok::Minus),
        tag_token(Tok::Not),
    ))(i)?;

    Ok((i, Unary::from_token(tokens.tok[0].clone()).unwrap()))
}

fn parse_prefix_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    use Expr::Unary;
    let (i1, prefix) = parse_prefix(i)?;
    let (i2, expr1) = parse_atom(i1)?;
    Ok((
        i2,
        ExprNode::new(Unary(prefix.clone(), Box::new(expr1)), prefix.loc),
    ))
}

fn parse_infix(i: Tokens) -> PResult<Tokens, Binary> {
    let (i1, t1) = take_one_any(i.clone())?;
    let next = &t1.tok[0];
    let (_, maybe_op) = infix_op(&next.tok);
    match maybe_op {
        None => Err(Err::Error(error_position!(i, ErrorKind::Tag))),
        Some(_) => Ok((i1, Binary::from_token(next.clone()).unwrap())),
    }
}

fn parse_infix_expr(i: Tokens, left: ExprNode) -> PResult<Tokens, ExprNode> {
    let (i, infix) = parse_infix(i)?;
    let (i2, right) = parse_pratt_expr(i, infix.precedence.clone())?;
    Ok((
        i2,
        ExprNode::new(
            Expr::Binary(infix.clone(), Box::new(left), Box::new(right)),
            infix.loc,
        ),
    ))
}

fn parse_index_expr(i: Tokens, mut left: ExprNode) -> PResult<Tokens, ExprNode> {
    let (i, open) = tag_token(Tok::LBracket)(i)?;
    let (i2, index) = parse_pratt_expr(i, Precedence::PIndex)?;
    let (i3, close) = tag_token(Tok::RBracket)(i2)?;
    left.s.append(open.expand_toks());
    let loc = index.loc.clone();
    let mut expr = ExprNode::new(Expr::Index(Box::new(left), Box::new(index)), loc);
    expr.s.append(close.expand_toks());
    Ok((i3, expr))
}

fn parse_call_expr(i: Tokens, mut left: ExprNode) -> PResult<Tokens, ExprNode> {
    let (i, open) = tag_token(Tok::LParen)(i)?;
    //let (i2, args) = parse_pratt_expr(i, Precedence::PCall)?;//many0(parse_expr)(i)?;
    let (i2, args) = many0(parse_expr)(i)?;
    let (i3, close) = tag_token(Tok::RParen)(i2)?;
    left.s.append(open.expand_toks());
    let loc = open.to_location();
    let mut expr = ExprNode::new(Expr::Apply(Box::new(left), args), loc);
    //let mut expr = ExprNode::new(Expr::Lambda(Lambda::new(Box::new(left), args), loc);
    expr.s.append(close.expand_toks());
    Ok((i3, expr))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::*;
    use crate::sexpr::SExpr;

    fn parser_losslessness(s: &str) -> bool {
        println!("{:?}", &s);
        let mut lexer = LexerState::from_str(s).unwrap();
        let tokens = lexer.tokens();
        //let toks = tokens.toks();
        //println!("tokens {:?}", tokens);
        match parse_program(tokens) {
            Ok((prog_rest, prog)) => {
                if prog_rest.input_len() > 0 {
                    println!("prog_rest {:?}", prog_rest.toks());
                }
                println!("prog {:?}", (&prog));
                let s2 = prog.unlex();
                println!("test {:?}", (s, &s2));
                s == s2
            }
            Err(nom::Err::Error(e)) => {
                //println!("Error: {:?}", e);
                for (tokens, err) in e.errors {
                    println!("error {:?}", (&err, tokens.toks()));
                }
                false
            }
            _ => unreachable!(),
        }
    }

    fn dump_stmt(stmt: &StmtNode) {
        println!("Stmt: {}", stmt.unlex());
        println!("\tSurround: {:?}", stmt.s);
        for token in stmt.unparse() {
            println!("\tToken:: {:?}", token);
        }
    }

    #[test]
    fn literal() {
        let r = vec!["1", " 2 ", "\n1", "1\n"];
        r.iter().for_each(|v| {
            let mut lexer = LexerState::from_str(v).unwrap();
            let tokens = lexer.tokens();
            //let toks = tokens.toks();
            println!("{:?}", (&tokens.toks()));
            let (rest, result) = parse_literal(tokens).unwrap();
            println!("lit {:?}", (&result, rest.toks()));
            let restored = result.unlex();
            println!("restored {:?}", (&restored));
            assert_eq!(v, &restored);
        });
    }

    #[test]
    fn ident() {
        let r = vec!["x", " x "];
        r.iter().for_each(|v| {
            let mut lexer = LexerState::from_str(v).unwrap();
            let tokens = lexer.tokens();
            //let toks = tokens.toks();
            println!("{:?}", (&tokens.toks()));
            let (_, result) = parse_ident(tokens).unwrap();
            println!("ident {:?}", (&result));
            let restored = result.unlex();
            println!("restored {:?}", (&restored));
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
        ];
        r.iter().for_each(|v| {
            let mut lexer = LexerState::from_str(v).unwrap();
            let tokens = lexer.tokens();
            println!("v {:?}", (&v));
            println!("tokens: {:?}", (&tokens));

            match parse_expr(tokens) {
                Ok((rest, result)) => {
                    println!("p {:?}", (&result));
                    println!("rest {:?}", (&rest));
                    let restored = result.unlex();
                    println!("{:?}", (&v, &restored));
                    assert_eq!(v, &restored);
                }
                Err(nom::Err::Error(e)) => {
                    for (tokens, err) in e.errors {
                        println!("error {:?}", (&err, tokens.toks()));
                    }
                    assert!(false);
                }
                _ => unreachable!(),
            }
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
            ";",
            "\\x -> y;",
            "f = \\x -> { x^2 };",
            "x + 1 +\n  2\n  ;\nx+1;\n",
        ];
        r.iter().for_each(|v| {
            let mut lexer = LexerState::from_str(v).unwrap();
            let tokens = lexer.tokens();
            //let toks = tokens.toks();
            println!("q: {}", v);
            //println!("toks: {:?}", (&toks));
            match parse_statements0(tokens) {
                Ok((rest, results)) => {
                    results.iter().for_each(|r| {
                        println!("result {:?}", (&r));
                    });

                    if rest.input_len() > 0 {
                        println!("ERROR tokens remaining {:?}", (&rest));
                    }

                    for stmt in &results {
                        dump_stmt(&stmt);
                    }

                    assert!(rest.input_len() == 0);

                    let restored = results
                        .iter()
                        .map(|s| s.unlex())
                        .collect::<Vec<_>>()
                        .join("");
                    println!("cmp {:?}", (&v, &restored));
                    assert_eq!(v, &restored);
                    println!("remaining {:?}", (&rest.toks()));
                    assert_eq!(rest.toks().len(), 0);
                }
                Err(nom::Err::Error(e)) => {
                    for (tokens, err) in e.errors {
                        println!("error {:?}", (&err, tokens.toks()));
                    }
                    assert!(false);
                }
                _ => unreachable!(),
            }
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
        ];
        r.iter().for_each(|v| {
            assert!(parser_losslessness(v));
        });
    }

    #[test]
    fn block() {
        let r = vec![
            "x = 1 + 2 ",
            "x = \\x -> { 0\n } \n",
            //"{ }",
        ];
        r.iter().for_each(|v| {
            println!("q {:?}", (&v));
            let mut lexer = LexerState::from_str(v).unwrap();
            let tokens = lexer.tokens();
            println!("{:?}", (&tokens));
            match parse_assignment_stmt(tokens) {
                Ok((rest, prog)) => {
                    println!("x{:?}", (&rest, &prog));
                    assert!(rest.input_len() == 0);
                }
                Err(nom::Err::Error(e)) => {
                    for (tokens, err) in e.errors {
                        println!("error {:?}", (&err, tokens.toks()));
                    }
                }
                _ => unreachable!(),
            }
            //assert!(parser_losslessness(v));
        });
    }

    #[test]
    fn sexpr() {
        let r = vec![
            ("+1", "(+ 1)"),
            ("+ 1", "(+ 1)"),
            ("123", "123"),
            ("-123", "(- 123)"),
            ("- 1 / (2 - 5)", "(/ (- 1) (- 2 5))"),
            ("+ 1 / (2 - 5)", "(/ (+ 1) (- 2 5))"),
            // handle ambiguous div correctly
            ("1/2/3", "(/ (/ 1 2) 3)"),
            ("5^2", "(^ 5 2)"),
            ("1-5^2", "(- 1 (^ 5 2))"),
            ("-1-5^2", "(- (- 1) (^ 5 2))"),
            ("-5^2", "(- (^ 5 2))"),
            ("-x^y", "(- (^ x y))"),
            ("a*-b", "(* a (- b))"),
            ("-a+b", "(+ (- a) b)"),

            // TODO: this should parse as "(- (* a (- b)))"
            // It's fine because * is associative, but not generally
            ("-a*-b", "(* (- a) (- b))"),

            ("(x+y)^(y+x)", "(^ (+ x y) (+ y x))"),
            // there are two ways to handle multiple-carets
            // https://en.wikipedia.org/wiki/Order_of_operations#Serial_exponentiation
            // this one is consistent with how we handle div, eval from left to right
            ("2^3^4", "(^ (^ 2 3) 4)"),
            // this one is not, eval is from right to left, which is more math convention
            //("2^3^4", "(^ 2 (^ 3 4))"),

            // multiply should have higher priority than div, but if you evaluate from left to
            // right, that breaks down
            ("8/2*(2+2)", "(* (/ 8 2) (+ 2 2))"),
            // with proper precedence, this should be the answer
            //("8/2*(2+2)", "(/ 8 (* 2 (+ 2 2)))"),

            // tricky, what should it do?  The plus sort of implies that -2 is the base
            // ("0+−2^2", "(+ 0 (^ (- 2) 2))"),
            // +- could also be interpreted as just -
            // ("0+−2^2", "(- 0 (^ 2 2))"),
            // or the plus could be the infix op, and - the prefix
            ("0+-2^2", "(+ 0 (- (^ 2 2)))"),
            // this one has a unicode minus sign, which is invalid
            //("0+−2^2", "(+ 0 (- (^ 2 2)))"),
            ("y > y", "(> y y)"),
            ("\\x -> x^2;\n", "(lambda (params x) (^ x 2))"),
            ("\\ x y -> x^2;\n", "(lambda (params x y) (^ x 2))"),
            (
                "\\ x y -> (x^2 + 1);\n",
                "(lambda (params x y) (+ (^ x 2) 1))",
            ),
            ("x( 1 2 3)", "(apply x 1 2 3)"),
            ("(x+y)( 1 2 3)", "(apply (+ x y) 1 2 3)"),
        ];

        r.iter().for_each(|(q, a)| {
            println!("q {:?}", (&q));
            let prog = test_program(q).unwrap();
            let stmts = prog.value;
            assert!(stmts.len() > 0);
            let stmt = stmts.get(0).unwrap();
            println!("{:?}", (&stmt));
            match stmt.sexpr() {
                Ok(sexpr) => {
                    println!("sexpr {}", &sexpr);
                    let rendered = format!("{}", &sexpr);
                    println!("sexpr {:?}", (&q, &sexpr, &rendered, a));
                    assert_eq!(rendered, a.to_string());
                }
                Err(e) => {
                    println!("Error: {:?}", e);
                    assert!(false);
                }
            }
        });
    }

    fn test_program(s: &str) -> Option<Program> {
        let mut lexer = LexerState::default();
        let (_, _) = lexer.lex_eof(s).unwrap();
        let tokens = lexer.tokens();
        println!("{:?}", (&tokens.toks()));

        //let s = span(s);
        let (rest, prog) = parse_program(tokens).unwrap();
        if rest.input_len() > 0 {
            println!("Rest: {:?}", rest.toks());
        }
        Some(prog)
    }
}
