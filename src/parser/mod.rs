use nom::*;

use crate::ast::*;
use crate::results::*;
use crate::tokens::*;
use nom::branch::*;
use nom::bytes::complete::take;
use nom::combinator::{self, into, verify};
use nom::error::{context, ErrorKind, VerboseError};
use nom::multi::{many0};
use nom::sequence::*;
use nom::Err;
use std::result::Result::*;

mod pratt;
mod pratt1;

pub(crate) type PResult<I, O> = IResult<I, O, VerboseError<I>>;

pub(crate) fn tag_token<'a>(t: Tok) -> impl FnMut(Tokens<'a>) -> PResult<Tokens<'a>, Tokens<'a>> {
    //let s = format!("tag-token: {:?}", t);//.to_owned();
    context(
        //s.as_str(),
        "tag-token",
        verify(take_one_any, move |tokens: &Tokens<'a>| {
            tokens.tok[0].tok == t
        }),
    )
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

pub(crate) fn take_one_any(i: Tokens) -> PResult<Tokens, Tokens> {
    take(1usize)(i)
}

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
    many0(parse_statement)(i)//alt((parse_statement, parse_invalid_stmt)))(i)
}

pub fn parse_empty_stmt(i: Tokens) -> PResult<Tokens, StmtNode> {
    combinator::map(tag_token(Tok::SemiColon), |t| {
        let mut stmt = StmtNode::new(Stmt::Empty, t.to_location());
        stmt.s.append(t.expand_toks());
        stmt
    })(i)
}

pub fn parse_statement(i: Tokens) -> PResult<Tokens, StmtNode> {
    //println!("parse_statment: {:?}", i);
    context("statement", 
            alt((
                    parse_expr_stmt,
                    parse_empty_stmt,
                    parse_invalid_stmt
                    )))(i)
            //parse_assignment_stmt,
            //parse_invalid_stmt,
            //parse_literal_stmt,
        //)),
}

pub fn parse_block_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    let (i, (left, stmts, right)) = tuple((
        tag_token(Tok::LBrace),
        many0(parse_statement),
        tag_token(Tok::RBrace),
    ))(i)?;
    let mut expr = ExprNode::new(Expr::Block(stmts), &left.to_location());
    expr.context.s.prepend(left.tok[0].expand_toks());
    expr.context.s.append(right.tok[0].expand_toks());
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
    ident.context.s.append(assign.tok[0].s.pre.clone());
    expr.context.s.prepend(assign.tok[0].s.post.clone());

    let mut stmt = StmtNode::new(Stmt::Assign(ident.into(), expr), i.to_location());
    // handle trailing newline
    stmt.s.append(nl.expand_toks());

    Ok((i, stmt))
}

pub fn _parse_invalid_stmt2(i: Tokens) -> PResult<Tokens, StmtNode> {
    let (mut i1, r) = many0(take_one_any)(i)?;
    let s = r.iter().map(|t| t.unlex()).collect::<Vec<_>>().join("");
    i1.result(Results::Error(format!("Invalid Statement: {}", s), 0));
    let stmt = StmtNode::new(Stmt::Invalid(s), i1.to_location());
    // handle trailing newline
    //stmt.s.append(end.expand_toks());
    //println!("invalid: {:?}", (i.toks(), &stmt, r, end));
    Ok((i1, stmt))
}

pub fn parse_invalid_stmt(i: Tokens) -> PResult<Tokens, StmtNode> {
    let loc = i.to_location().clone();
    let (mut i1, (r, end)) = pair(many0(parse_not_stmt_end), parse_stmt_end)(i)?;
    let s = r.iter().map(|t| t.unlex()).collect::<Vec<_>>().join("");
    i1.result(Results::Error(format!("Invalid Statement: {}", s), 0));
    let mut stmt = StmtNode::new(Stmt::Invalid(s), loc);
    // handle trailing newline
    stmt.s.append(end.expand_toks());
    //println!("invalid: {:?}", (&i.toks(), &stmt, r, end));
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
    context("expr", 
            alt((
                    pratt::parse_expr,
                    parse_lambda_expr,
                    )))(i)
}

pub(crate) fn parse_ident(i: Tokens) -> PResult<Tokens, ExprNode> {
    context("ident", _parse_ident)(i)
}
fn _parse_ident(i: Tokens) -> PResult<Tokens, ExprNode> {
    let (i1, t1) = take_one_any(i)?;
    let token = &t1.tok[0];
    match &token.tok {
        Tok::Ident(_) => {
            let expr = ExprNode::from_token(token).unwrap();
            Ok((i1, expr))
        }
        _ => Err(Err::Error(error_position!(i1, ErrorKind::Tag))),
    }
}

pub(crate) fn parse_literal(i: Tokens) -> PResult<Tokens, ExprNode> {
    let (i1, t1) = take_one_any(i)?;
    let token = &t1.tok[0];
    let tok = &token.tok;

    if let Ok(lit) = tok.try_into() {
        let mut litnode = ExprNode::new(Expr::LitExpr(lit), &token.to_location());
        litnode.context.s.prepend(token.s.pre.clone());
        litnode.context.s.append(token.s.post.clone());
        //println!("LIT: {:?}", litnode);
        Ok((i1, litnode))
    } else {
        Err(Err::Error(error_position!(i1, ErrorKind::Tag)))
    }
}

fn parse_atom(i: Tokens) -> PResult<Tokens, ExprNode> {
    context(
        "atom",
        alt((
            //caret really isn't an atom, but this is how we give it precedence over prefix
            //parse_apply2_expr,
            //parse_apply1_expr,
            //parse_caret_expr,
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
    let (i, (slash, idents, arrow)) = tuple((
        tag_token(Tok::Backslash),
        many0(parse_ident),
        tag_token(Tok::LeftArrow)
        ))(i)?;
   
    let (i, mut body) = parse_expr(i)?;
        //alt((parse_block_stmt, into(parse_atom), parse_expr_stmt)),
        //into(parse_atom),
        //parse_expr_stmt,
        //parse_lambda_end,
    //))(i)?;
    println!("slash: {:?}", &slash);
    println!("idents: {:?}", &idents);
    println!("body: {:?}", &body);
    let mut params = Params::new(idents);
    params.s.prepend(slash.tok[0].toks_post());
    params.s.append(arrow.tok[0].toks_pre());
    body.context.s.prepend(arrow.tok[0].toks_post());
    let loc = slash.tok[0].to_location();
    let mut lambda: ExprNode = Lambda::new(params, body.into(), loc.clone()).into();
    lambda.context.s.prepend(slash.tok[0].toks_pre());
    //lambda.s.append(end.expand_toks());
    //println!("lambda: {:?}", &lambda);

    Ok((i, lambda))
}

fn parse_caret_side(i: Tokens) -> PResult<Tokens, ExprNode> {
    alt((into(parse_literal), into(parse_ident)))(i)
}

fn parse_caret_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    let (i, (left, _, right)) =
        tuple((parse_caret_side, tag_token(Tok::Caret), parse_caret_side))(i)?;
    let op = Operator::Exp;
    //let op = Binary::from_token(&op.tok[0]).unwrap();
    let loc = left.context.loc.clone();
    let expr = ExprNode::new(Expr::Binary(op, Box::new(left), Box::new(right)), &loc);
    Ok((i, expr))
}

fn parse_paren_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    let (i, (left, mut expr, right)) =
        tuple((tag_token(Tok::LParen), parse_expr, tag_token(Tok::RParen)))(i)?;
    expr.context.s.prepend(left.expand_toks());
    expr.context.s.append(right.expand_toks());
    Ok((i, expr))
}

fn parse_prefix(i: Tokens) -> PResult<Tokens, OperatorNode> {
    let (i, tokens) = alt((
        tag_token(Tok::Plus),
        tag_token(Tok::Minus),
        tag_token(Tok::Exclamation),
    ))(i)?;

    Ok((i, OperatorNode::from_prefix_token(&tokens.tok[0]).unwrap()))
}

fn parse_postfix(i: Tokens) -> PResult<Tokens, OperatorNode> {
    let (i, tokens) = alt((
        tag_token(Tok::Percent),
        tag_token(Tok::Exclamation),
    ))(i)?;

    Ok((i, OperatorNode::from_postfix_token(tokens.tok[0].clone()).unwrap()))
}


fn parse_prefix_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    use Expr::Prefix;
    let (i1, prefix) = parse_prefix(i)?;
    let (i2, expr1) = parse_atom(i1)?;
    let mut node = ExprNode::new(Prefix(prefix.clone(), Box::new(expr1)), &prefix.context.loc);
    node.context.s.prepend(prefix.unparse());//vec![prefix.token()]);
    Ok((i2, node))
}

fn parse_infix(i: Tokens) -> PResult<Tokens, Operator> {
    let (i1, t1) = take_one_any(i)?;
    let next = &t1.tok[0];
    match Operator::from_tok(&next.tok) {
        None => Err(Err::Error(error_position!(i1, ErrorKind::Tag))),
        Some(op) => Ok((i1, op))
    }
}

pub fn parse_assignment_expr(i: Tokens, mut left: ExprNode) -> PResult<Tokens, ExprNode> {
    match &left.value {
        Expr::Ident( _) => {
            let (i, (assign, mut expr)) = tuple((
                    tag_token(Tok::Assign),
                    parse_expr
                    ))(i)?;

            // transfer surround from assign to nodes we store
            left.context.s.append(assign.tok[0].s.pre.clone());
            expr.context.s.prepend(assign.tok[0].s.post.clone());

            let op = Operator::from_tok(&assign.tok[0].tok).unwrap();
            let value = Expr::Binary(op, Box::new(left), Box::new(expr));
            let expr = ExprNode::new(value, &i.to_location());
            Ok((i, expr))
        }
        _ => Err(Err::Error(error_position!(i, ErrorKind::Tag)))
    }

}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::*;
    use nom::multi::many1;
    use crate::sexpr::SExpr;

    pub(crate) fn parser_losslessness(s: &str) -> bool {
        println!("{:?}", &s);
        let mut lexer = LexerState::from_str_eof(s).unwrap();
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
        ];
        r.iter().for_each(|v| {
            let mut lexer = LexerState::from_str_eof(v).unwrap();
            let tokens = lexer.tokens();
            println!("v {:?}", (&v));
            //println!("tokens: {:?}", (&tokens));

            let mut p = many1(parse_expr);
            match p(tokens) {
                Ok((rest, result)) => {
                    println!("p {:?}", (&result));
                    println!("rest {:?}", (&rest));
                    result.iter().for_each(|v| {
                        println!("S: {}", (&v.sexpr().unwrap()));
                    });
                    let restored = result.into_iter().map(|v| v.unlex()).collect::<Vec<_>>().join("");
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
            "x + 1 +\n  2\n  \nx+1;\n",
            "\\x -> y^2;\n",
        ];
        r.iter().for_each(|v| {
            let mut lexer = LexerState::from_str_eof(v).unwrap();
            let tokens = lexer.tokens();
            //let toks = tokens.toks();
            println!("q: {}", v);
            //println!("toks: {:?}", (&toks));
            match parse_statements0(tokens) {
                Ok((rest, results)) => {
                    results.iter().for_each(|r| {
                        println!("result {:?}", (&r));
                    });

                    for stmt in &results {
                        dump_stmt(&stmt);
                    }


                    let restored = results
                        .iter()
                        .map(|s| s.unlex())
                        .collect::<Vec<_>>()
                        .join("");
                    println!("cmp {:?}", (&v, &restored));
                    assert_eq!(v, &restored);
                    //println!("remaining {:?}", (&rest.toks()));

                    if rest.input_len() > 0 {
                        println!("ERROR tokens remaining {:?}", (&rest));
                    }

                    //assert_eq!(rest.toks().len(), 0);
                    //assert!(rest.input_len() == 0);
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
            "x = 1 y = 2",
            "x=y=z",
            "x!",
            "a! ^ b",
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
            let mut lexer = LexerState::from_str_eof(v).unwrap();
            let tokens = lexer.tokens();
            println!("{:?}", (&tokens.toks()));
            match parse_assignment_stmt(tokens) {
                Ok((rest, prog)) => {
                    println!("x{:?}", (&rest.toks(), &prog));
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
