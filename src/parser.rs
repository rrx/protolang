use nom::*;

use crate::ast::*;
use crate::results::*;
use crate::tokens::*;
use nom::branch::*;
use nom::bytes::complete::take;
use nom::combinator::{verify};
use nom::error::{Error, ErrorKind};
use nom::multi::{many0, many1};
use nom::sequence::*;
use nom::Err;
use std::result::Result::*;

fn tag_token<'a>(t: Tok) -> impl FnMut(Tokens<'a>) -> IResult<Tokens<'a>, Tokens<'a>> {
    verify(take(1usize), move |tokens: &Tokens<'a>| {
        tokens.tok[0].tok == t
    })
}

fn parse_whitespace<'a>(i: Tokens<'a>) -> IResult<Tokens<'a>, Tokens<'a>> {
    verify(take(1usize), move |tokens: &Tokens<'a>| {
        let tok = &tokens.tok[0].tok;
        tok.is_whitespace()
    })(i)
}

fn parse_newline<'a>(i: Tokens<'a>) -> IResult<Tokens<'a>, Tokens<'a>> {
    verify(take_one_valid, |tokens: &Tokens<'a>| {
        let tok = &tokens.tok[0].tok;
        if let Tok::Invalid(_) = tok {
            true
        } else {
            tok.is_newline()
        }
    })(i)
}

fn take_one_any(i: Tokens) -> IResult<Tokens, Tokens> {
    take(1usize)(i)
}

fn take_one_valid(i: Tokens) -> IResult<Tokens, Tokens> {
    let (i, mut tokens) = take(1usize)(i)?;
    let tok = &tokens.tok[0].tok;
    if let Tok::Invalid(c) = tok {
        tokens.result(Results::Error(format!("Invalid Char: {:?}", c), 0));
    }
    Ok((i, tokens))
}

fn parse_newline_or_eof(i: Tokens) -> IResult<Tokens, Tokens> {
    verify(take_one_valid, |tokens: &Tokens| {
        let tok = &tokens.tok[0].tok;
        if let Tok::Invalid(c) = tok {
            true
        } else {
            tok.is_newline() || tok == &Tok::EOF
        }
    })(i)
}

pub fn parse_statement(i: Tokens) -> IResult<Tokens, StmtNode> {
    alt((
        parse_expr_stmt,
        parse_assignment_stmt,
        parse_invalid_stmt,
        //parse_literal_stmt,
    ))(i)
}

pub fn parse_assignment_stmt(i: Tokens) -> IResult<Tokens, StmtNode> {
    let (i, (mut ident, assign, mut expr, nl)) = tuple((
        parse_ident,
        tag_token(Tok::Assign),
        parse_expr,
        parse_newline_or_eof,
    ))(i)?;

    // transfer surround from assign to nodes we store
    ident.s.append(
        assign.tok[0]
            .pre
            .iter()
            .map(|t| t.toks())
            .flatten()
            .collect(),
    );
    expr.s.prepend(
        assign.tok[0]
            .post
            .iter()
            .map(|t| t.toks())
            .flatten()
            .collect(),
    );

    let mut stmt = StmtNode::new(Stmt::Assign(ident, expr));
    // handle trailing newline
    stmt.s.append(nl.toks());

    Ok((i, stmt))
}

pub fn parse_invalid_stmt(i: Tokens) -> IResult<Tokens, StmtNode> {
    let (mut i, (invalid, nl)) = pair(many1(take_one_valid), parse_newline_or_eof)(i)?;
    let s = invalid
        .iter()
        .map(|t| t.unlex())
        .collect::<Vec<_>>()
        .join("");
    i.result(Results::Error(format!("Invalid Statement: {}", s), 0));
    let mut stmt = StmtNode::new(Stmt::Invalid(s));
    // handle trailing newline
    stmt.s.append(nl.toks());
    Ok((i, stmt))
}

pub fn parse_expr_stmt(i: Tokens) -> IResult<Tokens, StmtNode> {
    let (i, (expr, nl)) = pair(parse_expr, parse_newline_or_eof)(i)?;
    let mut stmt = StmtNode::new(Stmt::Expr(expr));
    stmt.s.append(nl.toks());
    //println!("X: {:?}", (&stmt));
    Ok((i, stmt))
}

pub fn parse_program_with_results(i: Tokens) -> (Option<Program>, Vec<Results>) {
    match parse_program(i) {
        Ok((prog_rest, prog)) => {
            let mut results = vec![];
            if prog_rest.tok.len() > 0 {
                results.push(Results::Warning(
                    format!("Not all tokens parsed: {:?}", prog_rest.toks()),
                    0,
                ));
            }
            (Some(prog), results)
        }
        Err(e) => (
            None,
            vec![Results::Error(format!("Unable to parse: {:?}", e), 0)],
        ),
    }
}

pub fn parse_program(i: Tokens) -> IResult<Tokens, Program> {
    let (i, (pre, stmts, post)) = tuple((
        many0(parse_newline),
        many0(parse_statement),
        many0(parse_whitespace),
    ))(i)?;
    let pre = pre.iter().map(|v| v.toks()).flatten().collect();
    let post = post.iter().map(|v| v.toks()).flatten().collect();
    let value = Program::new(stmts, pre, post);
    Ok((i, value))
}

pub fn parse_expr(i: Tokens) -> IResult<Tokens, ExprNode> {
    parse_pratt_expr(i, Precedence::PLowest)
}

fn parse_pratt_expr(input: Tokens, precedence: Precedence) -> IResult<Tokens, ExprNode> {
    let (i1, left) = parse_atom_expr(input)?;
    go_parse_pratt_expr(i1, precedence, left)
}

fn go_parse_pratt_expr(
    input: Tokens,
    precedence: Precedence,
    left: ExprNode,
) -> IResult<Tokens, ExprNode> {
    let (i1, t1) = take_one_any(input.clone())?;

    if t1.tok.is_empty() {
        Ok((i1, left))
    } else {
        let preview = &t1.tok[0];
        let p = infix_op(&preview.tok);
        //println!("infix: {:?}", (&preview, &p));
        match p {
            //(Precedence::PExp, _) if precedence < Precedence::PExp => {
            //let (i2, left2) = parse_caret_expr(input, left)?;
            //go_parse_pratt_expr(i2, precedence, left2)
            //}
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

fn parse_ident(i: Tokens) -> IResult<Tokens, Ident> {
    let (i1, t1) = take_one_any(i.clone())?;
    let token = &t1.tok[0];
    match Ident::from_token(token.clone()) {
        Some(ident) => Ok((i1, ident)),
        _ => Err(Err::Error(Error::new(i, ErrorKind::Tag))),
    }
}

fn parse_ident_expr(i: Tokens) -> IResult<Tokens, ExprNode> {
    let (i, ident) = parse_ident(i)?;
    Ok((i, ExprNode::new(Expr::IdentExpr(ident), vec![], vec![])))
}

fn parse_literal_stmt(i: Tokens) -> IResult<Tokens, StmtNode> {
    let (i, lit) = parse_literal(i)?;
    Ok((i, StmtNode::new(Stmt::Lit(lit))))
}

fn parse_literal(i: Tokens) -> IResult<Tokens, LiteralNode> {
    let (i1, t1) = take_one_any(i.clone())?;
    let token = &t1.tok[0];
    let maybe_lit = match &token.tok {
        Tok::IntLiteral(u) => Some(Literal::IntLiteral(*u)),
        Tok::FloatLiteral(u) => Some(Literal::FloatLiteral(*u)),
        Tok::StringLiteral(s) => Some(Literal::StringLiteral(s.clone())),
        Tok::BoolLiteral(b) => Some(Literal::BoolLiteral(*b)),
        //Tok::Invalid(s) => Some(Literal::Invalid(s.clone())),
        _ => None,
    };
    if let Some(lit) = maybe_lit {
        let pre = token.pre.iter().map(|t| t.toks()).flatten().collect();
        let post = token.post.iter().map(|t| t.toks()).flatten().collect();
        let litnode = LiteralNode::new(lit, pre, post, token.to_location());
        //println!("LIT: {:?}", litnode);
        Ok((i1, litnode))
    } else {
        Err(Err::Error(Error::new(i, ErrorKind::Tag)))
    }
}

fn parse_literal_expr(i: Tokens) -> IResult<Tokens, ExprNode> {
    let (i, lit) = parse_literal(i)?;
    Ok((i, ExprNode::new(Expr::LitExpr(lit), vec![], vec![])))
}

fn parse_atom_expr(i: Tokens) -> IResult<Tokens, ExprNode> {
    alt((
        //caret really isn't an atom, but this is how we give it precedence over prefix
        parse_caret_expr,
        parse_literal_expr,
        parse_ident_expr,
        parse_prefix_expr,
        parse_paren_expr,
        parse_lambda_expr,
        parse_apply_expr,
        //parse_array_expr,
        //parse_hash_expr,
        //parse_if_expr,
        //parse_fn_expr,
    ))(i)
}

fn parse_apply_end(i: Tokens) -> IResult<Tokens, Tokens> {
    alt((tag_token(Tok::SemiColon), parse_newline_or_eof))(i)
}

fn parse_apply_expr(i: Tokens) -> IResult<Tokens, ExprNode> {
    let (i, (ident, args, end)) =
        tuple((parse_ident, many0(parse_expr), parse_atom_expr))(i)?;
    let lambda = ExprNode::new(Expr::Apply(ident, args), vec![], vec![]);
    Ok((i, lambda))
}

fn parse_lambda_expr(i: Tokens) -> IResult<Tokens, ExprNode> {
    let (i, (slash, idents, arrow, mut body)) =
        tuple((tag_token(Tok::Backslash), many0(parse_ident), tag_token(Tok::LeftArrow), parse_atom_expr))(i)?;
    println!("slash: {:?}", &slash);
    println!("idents: {:?}", &idents);
    let mut params = Params::new(idents);
    params.s.prepend(slash.tok[0].toks_post());
    params.s.append(arrow.tok[0].toks_pre());
    body.s.prepend(arrow.tok[0].toks_post());
    //params.s.append(arrow.pre);
    let mut lambda = ExprNode::new(Expr::Lambda(Lambda::new(params, body, i.tok[0].to_location())), vec![], vec![]);
    lambda.s.prepend(slash.tok[0].toks_pre());

    Ok((i, lambda))
}

fn parse_caret_side(i: Tokens) -> IResult<Tokens, ExprNode> {
    alt((parse_literal_expr, parse_ident_expr))(i)
}

fn parse_caret_expr(i: Tokens) -> IResult<Tokens, ExprNode> {
    let (i, (left, op, right)) =
        tuple((parse_caret_side, tag_token(Tok::Caret), parse_caret_side))(i)?;
    let expr = ExprNode::new(
        Expr::InfixExpr(
            InfixNode::from_token(op.tok[0].clone()).unwrap(),
            Box::new(left),
            Box::new(right),
        ),
        vec![],
        vec![],
    );
    Ok((i, expr))
}

fn parse_paren_expr(i: Tokens) -> IResult<Tokens, ExprNode> {
    let (i, (left, mut expr, right)) =
        tuple((tag_token(Tok::LParen), parse_expr, tag_token(Tok::RParen)))(i)?;
    expr.s.prepend(left.toks());
    expr.s.append(right.toks());
    Ok((i, expr))
}

fn parse_prefix(i: Tokens) -> IResult<Tokens, PrefixNode> {
    let (i, tokens) = alt((
        tag_token(Tok::Plus),
        tag_token(Tok::Minus),
        tag_token(Tok::Not),
    ))(i)?;

    Ok((i, PrefixNode::from_token(tokens.tok[0].clone()).unwrap()))
}

fn parse_prefix_expr(i: Tokens) -> IResult<Tokens, ExprNode> {
    use Expr::PrefixExpr;
    let (i1, prefix) = parse_prefix(i)?;
    let (i2, expr1) = parse_atom_expr(i1)?;
    Ok((
        i2,
        ExprNode::new(PrefixExpr(prefix, Box::new(expr1)), vec![], vec![]),
    ))
}

fn parse_infix(i: Tokens) -> IResult<Tokens, InfixNode> {
    let (i1, t1) = take_one_any(i.clone())?;
    let next = &t1.tok[0];
    let (_, maybe_op) = infix_op(&next.tok);
    match maybe_op {
        None => Err(Err::Error(error_position!(i, ErrorKind::Tag))),
        Some(_) => Ok((i1, InfixNode::from_token(next.clone()).unwrap())),
    }
}

fn parse_infix_expr(i: Tokens, left: ExprNode) -> IResult<Tokens, ExprNode> {
    let (i, infix) = parse_infix(i)?;
    let (i2, right) = parse_pratt_expr(i, infix.precedence.clone())?;
    Ok((
        i2,
        ExprNode::new(
            Expr::InfixExpr(infix, Box::new(left), Box::new(right)),
            vec![],
            vec![],
        ),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::*;
    use crate::sexpr::SExpr;

    fn parser_losslessness(s: &str) -> bool {
        println!("{:?}", &s);
        let (_, toks) = lex_eof(s).unwrap();
        println!("toks {:?}", toks);
        let tokens = Tokens::new(&toks[..]);
        println!("toks {:?}", tokens.toks());
        let (prog_rest, prog) = parse_program(tokens).unwrap();
        println!("prog {:?}", (&prog_rest.toks(), &prog));
        let s2 = prog
            .unparse()
            .iter()
            .map(|tok| tok.unlex())
            .collect::<Vec<_>>()
            .join("");
        println!("test {:?}", (s, &s2));
        s == s2
    }

    #[test]
    fn literal() {
        let r = vec!["1", " 2 "];
        r.iter().for_each(|v| {
            let (_, toks) = lex_eof(v).unwrap();
            let tokens = Tokens::new(&toks[..]);
            println!("{:?}", (&tokens.toks()));
            let (rest, result) = parse_literal_stmt(tokens).unwrap();
            println!("lit {:?}", (&result, rest.toks()));
            //assert_eq!(rest.toks().len(), 0);
            let restored = result.unlex();
            println!("restored {:?}", (&restored));
            assert_eq!(v, &restored);
        });
    }

    #[test]
    fn ident() {
        let r = vec!["x", " x "];
        r.iter().for_each(|v| {
            let (_, toks) = lex_eof(v).unwrap();
            let tokens = Tokens::new(&toks[..]);
            //println!("{:?}", (&tokens));
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
        let r = vec!["1+2", "1 + 2", " 1 + 2 "];
        r.iter().for_each(|v| {
            let (_, toks) = lex_eof(v).unwrap();
            let tokens = Tokens::new(&toks[..]);
            println!("{:?}", (&tokens.toks()));

            let (_, result) = parse_expr_stmt(tokens).unwrap();
            println!("p {:?}", (&result));
            let restored = result.unlex();
            //restored.push(Tok::EOF);
            //let ts = toks.iter().map(|v| v.tok.clone()).collect::<Vec<_>>();
            //println!("restored {:?}", (&ts, &restored));
            //let s = tokens.unlex();
            println!("{:?}", (&v, &restored));
            assert_eq!(v, &restored);
        });
    }

    #[test]
    fn statements() {
        // parse a single statement
        let r = vec![
            "1 + 2",
            "1 ",
            "1 + 2 ",
            "312 \n",
            "x=1",
            "x = 1\n",
            "x = y * 2\n",
        ];
        r.iter().for_each(|v| {
            let (_, toks) = lex_eof(v).unwrap();
            let tokens = Tokens::new(&toks[..]);
            println!("toks: {:?}", (&toks));
            println!("{:?}", (&tokens.toks()));
            let (rest, result) = parse_statement(tokens).unwrap();
            println!("p {:?}", (&result));
            let restored = result.unlex();
            println!("cmp {:?}", (&v, &restored));
            assert_eq!(v, &restored);
            println!("remaining {:?}", (&rest.toks()));
            //assert_eq!(rest.toks().len(), 0);
        });
    }

    #[test]
    fn unparse() {
        let r = vec![
            "",
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
            "+ 1 / (2 - 5)",
            "x+1",
            "(((((0)))))",
            "\"asdf\"",
            //utf-8 test
            "\"🎁\"",
            "3 - 0",
            "-x^(y+1)",
            "  y  <  y ",
            "\\x -> x^2",
            "\\ x y -> x^2 + y",
            " \\ x  y z-> x ^ 2 + y",
        ];
        r.iter().for_each(|v| {
            assert!(parser_losslessness(v));
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
            ("\\x -> x^2", "(lambda (params x) (^ x 2))"),
            ("\\ x y -> x^2", "(lambda (params x y) (^ x 2))"),
        ];

        r.iter().for_each(|(q, a)| {
            println!("q {:?}", (&q));
            let (_, toks) = lex_eof(q).unwrap();
            let tokens = Tokens::new(&toks[..]);
            println!("{:?}", (&toks));

            let (_, prog) = parse_program(tokens).unwrap();
            let stmts = prog.value;
            let stmt = stmts.get(0).unwrap();
            println!("{:?}", (&stmt));
            let sexpr = stmt.sexpr().unwrap();
            println!("sexpr {}", &sexpr);
            let rendered = format!("{}", &sexpr);
            println!("sexpr {:?}", (&q, &sexpr, &rendered, a));
            assert_eq!(rendered, a.to_string());
        });
    }
}
