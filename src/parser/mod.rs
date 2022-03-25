use nom::*;

use crate::ast::*;
use crate::results::*;
use crate::tokens::TokensList;
use crate::tokens::*;
use log::debug;
use nom::branch::*;
use nom::bytes::complete::take;
use nom::combinator::verify;
use nom::error::{context, ContextError, ErrorKind, VerboseErrorKind};
use nom::multi::many0;
use nom::sequence::*;
use nom::Err;
use std::result::Result::*;

mod pratt;
mod pratt1;
mod unparse;
pub use unparse::{unparse_expr, Unparse};

#[derive(Debug)]
pub struct DebugError<I> {
    pub message: String,
    pub errors: Vec<(I, VerboseErrorKind)>,
}

impl<I: std::fmt::Debug + TokensList> nom::error::ParseError<I> for DebugError<I> {
    // on one line, we show the error code and the input that caused it
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        let message = format!("kind {:?}:\t{:?}\n", kind, input.toks());
        //debug!("{}", message);
        DebugError {
            message,
            errors: vec![(input, VerboseErrorKind::Nom(kind))],
        }
    }

    // if combining multiple errors, we show them one after the other
    fn append(input: I, kind: ErrorKind, mut other: Self) -> Self {
        other.message = format!("{}kind {:?}:\t{:?}\n", other.message, kind, input.toks());
        //debug!("{}", message);
        //DebugError {
        //message,
        other.errors.push((input, VerboseErrorKind::Nom(kind)));
        //}
        other
    }

    fn from_char(input: I, c: char) -> Self {
        let message = format!("'{}':\t{:?}\n", c, input.toks());
        //debug!("{}", message);
        DebugError {
            message,
            errors: vec![(input, VerboseErrorKind::Char(c))],
        }
    }

    fn or(self, other: Self) -> Self {
        let message = format!("{}\tOR\n{}\n", self.message, other.message);
        //debug!("{}", message);
        DebugError {
            message,
            errors: vec![],
        }
    }
}

impl<I: std::fmt::Debug + TokensList> ContextError<I> for DebugError<I> {
    fn add_context(input: I, ctx: &'static str, other: Self) -> Self {
        let message = format!(
            "{}\"context-{}\":\t{:?}\n",
            other.message,
            ctx,
            input.toks()
        );
        //debug!("{}", message);
        DebugError {
            message,
            errors: vec![],
        }
    }
}

pub(crate) type PResult<I, O> = IResult<I, O, DebugError<I>>;

pub fn print_result<
    I: std::fmt::Debug + crate::tokens::TokensList,
    O: std::fmt::Debug + crate::sexpr::SExpr,
>(
    r: &PResult<I, O>,
) {
    match r {
        Ok((_, expr)) => {
            //debug!("Ok({:?})", (&expr));
            match expr.sexpr() {
                Ok(sexpr) => {
                    debug!("sexpr {}", &sexpr);
                    let rendered = format!("{}", &sexpr);
                    debug!("sexpr {:?}", (&sexpr, &rendered));
                }
                Err(e) => {
                    debug!("Error: {:?}", e);
                }
            }
        }
        Err(nom::Err::Error(e)) => {
            debug!("err: {:?}", e);
            for (tokens, err) in &e.errors {
                debug!("error {:?}", (&err, tokens.toks()));
            }
        }
        Err(e) => {
            debug!("err: {}", e);
        }
    }
}

pub(crate) fn tag_token<'a>(t: Tok) -> impl FnMut(Tokens<'a>) -> PResult<Tokens<'a>, Tokens<'a>> {
    let s = t.into();
    context(
        s,
        verify(take_one_any, move |tokens: &Tokens<'a>| {
            let v: &'static str = tokens.tok[0].tok.clone().into();
            v == s
        }),
    )
}

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

pub fn parse_assignment_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    let (i, (mut ident, assign, mut expr, nl)) = tuple((
        ExprNode::parse_ident_expr,
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

pub fn parse_program_with_results(
    filename: String,
    i: Tokens,
) -> (Option<ExprNode>, Vec<LangError>) {
    let context = i.to_context();
    match parse_program(i) {
        Ok((prog_rest, prog)) => {
            let mut results = vec![];
            if prog_rest.tok.len() > 0 {
                results.push(LangError::Warning(
                    format!("Not all tokens parsed: {:?}", prog_rest.toks()),
                    prog_rest.to_context(),
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
                    LangError::Error(
                        format!("Error: {:?}, {:?}", err, tokens.toks()),
                        context.clone(),
                    )
                })
                .collect();
            (None, results)
        }
        _ => unreachable!(),
    }
}

pub fn parse_program(i: Tokens) -> PResult<Tokens, ExprNode> {
    let (i, (exprs, end)) = pair(
        context("program-start", many0(alt((parse_expr, parse_invalid)))),
        context("program-end", combinator::rest), //many0(parse_whitespace_or_eof)),
    )(i)?;
    debug!("program has {} expressions", exprs.len());
    debug!("program rest {:?}", end); //.expand_toks());

    let mut value = ExprNode::new(Expr::Program(exprs), &i.to_location());
    value.context.append(end.expand_toks());
    Ok((i, value))
}

pub fn parse_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    context(
        "parse-expr",
        alt((
            pratt::parse_expr,
            ExprNode::parse_declaration,
            ExprNode::parse_lambda,
        )),
    )(i)
}

impl ExprNode {
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

        debug!("is_mut {}", is_mutable);
        let (i, mut ident) = Self::parse_ident(i)?;
        if is_mutable {
            ident.modifier = VarModifier::Mutable;
        }
        let mut node = ExprNode::new_with_token(Expr::Ident(ident), &i.tok[0]);
        node.context.prepend(m_toks);
        node.context.prepend(tlet.expand_toks());
        let (i, assign) = tag_token(Tok::Assign)(i)?;
        node.context.append(assign.expand_toks());

        let (i, rhs) = parse_expr(i)?;
        let expr = Expr::Binary(Operator::Declare, Box::new(node), Box::new(rhs));
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
        context("ident", Self::_parse_ident_expr)(i)
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
            many0(Self::parse_ident_expr),
            tag_token(Tok::LeftArrow),
        ))(i)?;

        let (i, mut body) = parse_expr(i)?;
        debug!("slash: {:?}", &slash);
        debug!("idents: {:?}", &idents);
        debug!("body: {:?}", &body);
        let loc = slash.tok[0].to_location();
        let mut params = Params::new(idents, &loc);
        params.context.prepend(slash.tok[0].toks_post());
        params.context.append(arrow.tok[0].toks_pre());
        body.context.prepend(arrow.tok[0].toks_post());
        let mut lambda: ExprNode = Lambda::new(params, body, &loc).into();
        lambda.context.prepend(slash.tok[0].toks_pre());
        Ok((i, lambda))
    }

    pub fn parse_block(i: Tokens) -> PResult<Tokens, Self> {
        let (i, (left, expressions, right)) = tuple((
            tag_token(Tok::LBrace),
            many0(parse_expr),
            tag_token(Tok::RBrace),
        ))(i)?;
        let mut node = Self::new(Expr::Block(expressions), &i.to_location());
        node.context.prepend(left.tok[0].expand_toks());
        node.context.append(right.tok[0].expand_toks());
        Ok((i, node))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::*;
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
            let (_, result) = ExprNode::parse_ident_expr(tokens).unwrap();
            debug!("ident {:?}", (&result));
            let restored = result.unlex();
            debug!("restored {:?}", (&restored));
            assert_eq!(v, &restored);
        });
    }

    #[test]
    fn test_parse_whitespace_or_eof() {
        let pos = crate::tokens::Span::new("".into());
        let toks = vec![Token::new(Tok::EOF, pos)];
        let i = Tokens::new(&toks[..]);
        assert_eq!(
            _parse_whitespace_or_eof(i).unwrap().1.toks(),
            vec![Tok::EOF]
        );
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
            let (i, exprs) = many1(ExprNode::parse_declaration)(tokens).unwrap();
            let expr = exprs.get(0).unwrap();
            match &expr.value {
                Expr::Binary(op, left, right) => {
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
            ("+1", "(program (+ 1))"),
            ("+1;\n+1", "(program (+ 1) (+ 1))"),
            ("a < b < c", "(program (chain (< a b) (< b c)))"),
        ];

        r.iter().for_each(|(q, a)| {
            debug!("q {:?}", (&q));

            let mut lexer = LexerState::from_str_eof(q).unwrap();
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
        let mut interp = crate::eval::Interpreter::default();

        let r = interp.eval("assert(1 == 1 == 1 == 1)", env).unwrap();
        let r = interp
            .eval(
                "
            let a = 1
            let b = 1
            let c = 1
            let d = 2
            assert(a == b == c != d)
        ",
                r.value.unwrap().env,
            )
            .unwrap();
    }
}
