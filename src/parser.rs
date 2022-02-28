use nom::*;

use crate::ast::*;
use crate::tokens::*;
use nom::branch::*;
use nom::bytes::complete::take;
use nom::combinator::{map, verify};
use nom::error::{Error, ErrorKind};
use nom::multi::many0;
use nom::sequence::*;
use nom::Err;
use std::result::Result::*;

fn tag_token<'a>(t: Tok) -> impl FnMut(Tokens<'a>) -> IResult<Tokens<'a>, Tokens<'a>> {
    verify(take(1usize), move |tokens: &Tokens<'a>| {
        tokens.tok[0].tok == t
    })
}

fn parse_newline<'a>(i: Tokens<'a>) -> IResult<Tokens<'a>, Tokens<'a>> {
    verify(take(1usize), move |tokens: &Tokens<'a>| {
        tokens.tok[0].tok.is_newline()
    })(i)
}

fn parse_newline_or_eof(i: Tokens) -> IResult<Tokens, Tokens> {
    verify(take(1usize), move |tokens: &Tokens| {
        let tok = &tokens.tok[0].tok;
        tok.is_newline() || tok == &Tok::EOF
    })(i)
}

pub fn parse_statement_node(i: Tokens) -> IResult<Tokens, Node> {
    alt((
            parse_expr_node,
            parse_assignment_node,
            ))(i)
}

pub fn parse_statement(i: Tokens) -> IResult<Tokens, Stmt> {
    alt((
            parse_expr_stmt,
            parse_assignment_stmt,
            ))(i)
}

pub fn parse_assignment_node(i: Tokens) -> IResult<Tokens, Node> {
    let (i, (ident, _, value, mut nl)) = tuple((
        parse_ident,
        tag_token(Tok::Assign),
        parse_expr,
        parse_newline_or_eof,
    ))(i)?;

    match value {
        Value::Expr(expr) => {
            let value = Value::Stmt(Stmt::Assign(ident, expr));
            let mut node = Node {
                pre: vec![],
                post: nl.toks(),
                value,
            };
            Ok((i, node))
        }
        _ => unreachable!(),
    }
    //match value {
        //Value::Expr(expr) => Ok((i, Stmt::Assign(ident, expr))),
        //_ => unreachable!(),
    //}
}

pub fn parse_assignment_stmt(i: Tokens) -> IResult<Tokens, Stmt> {
    let (i, (ident, _, value, nl)) = tuple((
        parse_ident,
        tag_token(Tok::Assign),
        parse_expr,
        parse_newline_or_eof,
    ))(i)?;
    match value {
        Value::Expr(expr) => Ok((i, Stmt::Assign(ident, expr))),
        _ => unreachable!(),
    }
}

pub fn parse_expr_stmt(i: Tokens) -> IResult<Tokens, Stmt> {
    let (i, (r, nl)) = pair(parse_expr_node, parse_newline_or_eof)(i)?;
    let value = match r.value {
        Value::Lit(x) => Stmt::Lit(x),
        Value::Expr(x) => Stmt::Expr(x),
        _ => unreachable!(),
    };

    Ok((i, value))
}

pub fn parse_program_node(i: Tokens) -> IResult<Tokens, Node> {
    let (i, (pre, mut stmts)) = pair(many0(parse_newline), many0(parse_statement_node))(i)?;
    let value = Value::Program(Program(stmts));
    let node = Node {
        pre: pre.iter().map(|v| v.tok[0].tok.clone()).collect(),
        post: vec![],
        value,
    };
    Ok((i, node))
}

fn parse_expr_node(input: Tokens) -> IResult<Tokens, Node> {
    parse_pratt_node(input, Precedence::PLowest)
}

fn parse_expr(input: Tokens) -> IResult<Tokens, Value> {
    parse_pratt_expr(input, Precedence::PLowest)
}

fn parse_atom_or_caret_node(i: Tokens) -> IResult<Tokens, Node> {
    alt((parse_caret_node, parse_atom_node))(i)
}

fn parse_atom_or_caret_expr(i: Tokens) -> IResult<Tokens, Value> {
    alt((parse_caret_expr, parse_atom_expr))(i)
}

fn parse_pratt_node(input: Tokens, precedence: Precedence) -> IResult<Tokens, Node> {
    //let (i1, left) = parse_atom_node(input)?;
    let (i1, left) = parse_atom_node(input)?;
    go_parse_pratt_node(i1, precedence, left)
}

fn parse_pratt_expr(input: Tokens, precedence: Precedence) -> IResult<Tokens, Value> {
    let (i1, left) = parse_atom_expr(input)?;
    go_parse_pratt_expr(i1, precedence, left)
}

fn go_parse_pratt_node(input: Tokens, precedence: Precedence, left: Node) -> IResult<Tokens, Node> {
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
                let (i2, left2) = parse_infix_node(input, left)?;
                go_parse_pratt_node(i2, precedence, left2)
            }
            _ => Ok((input, left)),
        }
    }
}

fn go_parse_pratt_expr(
    input: Tokens,
    precedence: Precedence,
    left: Value,
) -> IResult<Tokens, Value> {
    let (i1, t1) = take(1usize)(input)?;

    if t1.tok.is_empty() {
        Ok((i1, left))
    } else {
        let preview = &t1.tok[0];
        let p = infix_op(&preview.tok);
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
    let (i1, t1) = take(1usize)(i)?;
    if t1.tok.is_empty() {
        Err(Err::Error(Error::new(i, ErrorKind::Tag)))
    } else {
        let token = &t1.tok[0];
        match &token.tok {
            Tok::Ident(name) => {
                //println!("ident: {}", &name);
                Ok((i1, Ident(name.clone())))
            }
            _ => Err(Err::Error(Error::new(i, ErrorKind::Tag))),
        }
    }
}

fn parse_ident_node(input: Tokens) -> IResult<Tokens, Node> {
    let (i1, t1) = take(1usize)(input)?;
    if t1.tok.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        let token = &t1.tok[0];
        match &token.tok {
            Tok::Ident(name) => {
                let value = Value::Expr(Expr::IdentExpr(Ident(name.clone())));
                let node = Node {
                    pre: token.pre.iter().map(|v| v.tok.clone()).collect::<Vec<_>>(),
                    post: token.post.iter().map(|v| v.tok.clone()).collect::<Vec<_>>(),
                    //tokens: vec![token.tok.clone()],
                    value,
                };

                Ok((i1, node))
            }
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

fn parse_ident_expr(i: Tokens) -> IResult<Tokens, Value> {
    let (i, ident) = parse_ident_node(i)?;
    Ok((i, ident.value))
}

fn parse_literal_node(input: Tokens) -> IResult<Tokens, Node> {
    let (i1, t1) = take(1usize)(input)?;
    if t1.tok.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        let token = &t1.tok[0];
        let maybe_lit = match &token.tok {
            Tok::IntLiteral(u) => Some(Literal::IntLiteral(*u)),
            Tok::FloatLiteral(u) => Some(Literal::FloatLiteral(*u)),
            Tok::StringLiteral(s) => Some(Literal::StringLiteral(s.clone())),
            Tok::BoolLiteral(b) => Some(Literal::BoolLiteral(*b)),
            Tok::Invalid(s) => Some(Literal::Invalid(s.clone())),
            _ => None,
        };

        match maybe_lit {
            Some(lit) => {
                let value = Value::Expr(Expr::LitExpr(lit));
                let node = Node {
                    pre: token.pre.iter().map(|v| v.tok.clone()).collect::<Vec<_>>(),
                    post: token.post.iter().map(|v| v.tok.clone()).collect::<Vec<_>>(),
                    //tokens: vec![token.tok.clone()],
                    value,
                };
                Ok((i1, node))
            }
            None => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

fn parse_lit_expr(i: Tokens) -> IResult<Tokens, Value> {
    let (i, lit) = parse_literal_node(i)?;
    Ok((i, lit.value))
}

fn _parse_atom_noprefix_node(input: Tokens) -> IResult<Tokens, Node> {
    alt((
        parse_literal_node,
        parse_ident_node,
        parse_paren_node,
        //parse_caret_node,
        //parse_array_expr,
        //parse_hash_expr,
        //parse_if_expr,
        //parse_fn_expr,
    ))(input)
}

fn _parse_atom_noprefix_expr(i: Tokens) -> IResult<Tokens, Value> {
    let (i, node) = _parse_atom_noprefix_node(i)?;
    Ok((i, node.value))
}

fn parse_atom_node(input: Tokens) -> IResult<Tokens, Node> {
    alt((
        parse_caret_node,
        parse_literal_node,
        parse_ident_node,
        parse_prefix_node,
        parse_paren_node,
        //parse_array_expr,
        //parse_hash_expr,
        //parse_if_expr,
        //parse_fn_expr,
    ))(input)
}

fn parse_atom_expr(i: Tokens) -> IResult<Tokens, Value> {
    let (i, node) = parse_atom_node(i)?;
    Ok((i, node.value))
}

fn parse_caret_node(i: Tokens) -> IResult<Tokens, Node> {
    let (i, (left, expr, right)) =
        tuple((parse_lit_expr, tag_token(Tok::Caret), parse_lit_expr))(i)?;
    let value = Value::Expr(Expr::InfixExpr(Infix::Exp, Box::new(left), Box::new(right)));
    let node = Node {
        pre: vec![],  //token.pre.iter().map(|v| v.tok.clone()).collect::<Vec<_>>(),
        post: vec![], //token.post.iter().map(|v| v.tok.clone()).collect::<Vec<_>>(),
        //tokens: vec![token.tok.clone()],
        value,
    };
    Ok((i, node))
}

fn parse_caret_expr(i: Tokens) -> IResult<Tokens, Value> {
    let (i, (left, expr, right)) =
        tuple((parse_atom_expr, tag_token(Tok::Caret), parse_atom_expr))(i)?;
    Ok((
        i,
        Value::Expr(Expr::InfixExpr(Infix::Exp, Box::new(left), Box::new(right))),
    ))
}

fn parse_paren_node(i: Tokens) -> IResult<Tokens, Node> {
    let (i, (left, expr, right)) = tuple((
        tag_token(Tok::LParen),
        parse_expr_node,
        tag_token(Tok::RParen),
    ))(i)?;
    Ok((i, expr))
}

fn parse_paren_expr(i: Tokens) -> IResult<Tokens, Value> {
    let (i, (left, expr, right)) =
        tuple((tag_token(Tok::LParen), parse_expr, tag_token(Tok::RParen)))(i)?;
    Ok((i, expr))
}

fn parse_prefix_node(input: Tokens) -> IResult<Tokens, Node> {
    use Expr::PrefixExpr;
    let (i1, t1) = alt((
        tag_token(Tok::Plus),
        tag_token(Tok::Minus),
        tag_token(Tok::Not),
    ))(input)?;
    if t1.tok.is_empty() {
        Err(Err::Error(error_position!(input, ErrorKind::Tag)))
    } else {
        let (i2, e) = parse_atom_expr(i1)?;
        let token = &t1.tok[0];
        let maybe_prefix = match &token.tok {
            Tok::Plus => Some(Prefix::PrefixPlus),
            Tok::Minus => Some(Prefix::PrefixMinus),
            Tok::Not => Some(Prefix::PrefixNot),
            _ => None,
        };
        match maybe_prefix {
            Some(prefix) => {
                let value = Value::Expr(PrefixExpr(prefix, Box::new(e)));
                let node = Node {
                    pre: token.pre.iter().map(|v| v.tok.clone()).collect::<Vec<_>>(),
                    post: token.post.iter().map(|v| v.tok.clone()).collect::<Vec<_>>(),
                    //tokens: vec![token.tok.clone()],
                    value,
                };
                Ok((i2, node))
            }
            None => Err(Err::Error(error_position!(input, ErrorKind::Tag))),
        }
    }
}

fn parse_prefix_expr(i: Tokens) -> IResult<Tokens, Value> {
    map(parse_prefix_node, |n| n.value)(i)
}

fn parse_infix_expr(i: Tokens, left: Value) -> IResult<Tokens, Value> {
    use Expr::InfixExpr;
    let (i1, t1) = take(1usize)(i)?;
    if t1.tok.is_empty() {
        Err(Err::Error(error_position!(i, ErrorKind::Tag)))
    } else {
        let next = &t1.tok[0];
        let (precedence, maybe_op) = infix_op(&next.tok);
        match maybe_op {
            None => Err(Err::Error(error_position!(i, ErrorKind::Tag))),
            Some(op) => {
                let (i2, right) = parse_pratt_expr(i1, precedence)?;
                Ok((
                    i2,
                    Value::Expr(InfixExpr(op, Box::new(left), Box::new(right))),
                ))
            }
        }
    }
}

fn parse_infix_node(input: Tokens, left: Node) -> IResult<Tokens, Node> {
    use Expr::InfixExpr;
    let (i1, t1) = take(1usize)(input)?;
    if t1.tok.is_empty() {
        Err(Err::Error(error_position!(input, ErrorKind::Tag)))
    } else {
        let token = &t1.tok[0];
        let (precedence, maybe_op) = infix_op(&token.tok);
        match maybe_op {
            None => Err(Err::Error(error_position!(input, ErrorKind::Tag))),
            Some(op) => {
                let (i2, right) = parse_pratt_node(i1, precedence)?;
                let value = Value::Expr(InfixExpr(op, Box::new(left.value), Box::new(right.value)));
                let node = Node {
                    pre: token.pre.iter().map(|v| v.tok.clone()).collect::<Vec<_>>(),
                    post: token.post.iter().map(|v| v.tok.clone()).collect::<Vec<_>>(),
                    //tokens: vec![token.tok.clone()],
                    value,
                };
                Ok((i2, node))
                //Ok((i2, Value::Expr(InfixExpr(op, Box::new(left), Box::new(right)))))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::*;
    use crate::sexpr::SExpr;
    use Expr::*;
    use Literal::*;

    fn parser_losslessness(s: &str) -> bool {
        println!("{:?}", &s);
        let (_, toks) = lex_eof(s).unwrap();
        let tokens = Tokens::new(&toks[..]);
        println!("toks {:?}", tokens.toks());
        let (prog_rest, node) = parse_program_node(tokens).unwrap();
        println!("node {:?}", (&prog_rest.toks(), &node));
        let s2 = node.unparse().iter()
            .map(|tok| tok.unlex())
            .collect::<Vec<_>>()
            .join("")
            ;
        println!("test {:?}", (s, &s2));
        s == s2
    }

    #[test]
    fn literal() {
        let r = vec![
            (
                "123 - 0",
                Stmt::Expr(InfixExpr(
                    Infix::Minus,
                    Box::new(Value::Expr(LitExpr(IntLiteral(123)))),
                    Box::new(Value::Expr(LitExpr(IntLiteral(0)))),
                )),
            ),
            (
                "+123 / 1",
                Stmt::Expr(InfixExpr(
                    Infix::Divide,
                    Box::new(Value::Expr(PrefixExpr(
                        Prefix::PrefixPlus,
                        Box::new(Value::Expr(LitExpr(IntLiteral(123)))),
                    ))),
                    Box::new(Value::Expr(LitExpr(IntLiteral(1)))),
                )),
            ),
            ("123", Stmt::Expr(LitExpr(Literal::IntLiteral(123)))),
            ("123", Stmt::Expr(LitExpr(Literal::IntLiteral(123)))),
            (
                "\"asdf\"",
                Stmt::Expr(LitExpr(Literal::StringLiteral("asdf".into()))),
            ),
            //utf-8 test
            (
                "\"🎁\"",
                Stmt::Expr(LitExpr(Literal::StringLiteral("🎁".into()))),
            ),
        ];
        r.iter().for_each(|(q, a)| {
            let (rest, result) = lex_eof(q).unwrap();
            assert_eq!(rest.len(), 0);
            let tokens = Tokens::new(&result[..]);
            let (rest, node) = parse_program_node(tokens).unwrap();
            println!("{:?}", (&rest, &node));
            assert_eq!(rest.input_len(), 0);
            match node.value {
                Value::Stmt(stmt) => assert_eq!(&stmt, a),
                _ => unreachable!()
            };
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
            let result = parse_ident_node(tokens);
            println!("ident {:?}", (&result));
            result.unwrap();

            let result = parse_ident(tokens);
            println!("ident {:?}", (&result));
            result.unwrap();
        });
    }

    #[test]
    fn assign() {
        let r = vec!["x=1", "x = 1\n", "x = y * 2\n", "x = 1\ny=2"];
        r.iter().for_each(|v| {
            let (rest, toks) = lex_eof(v).unwrap();
            let tokens = Tokens::new(&toks[..]);
            //println!("{:?}", (&tokens));
            println!("{:?}", (&tokens.toks()));

            let result = parse_statement(tokens);
            println!("p {:?}", (&result));

            let (prog_rest, mut node) = parse_program_node(tokens).unwrap();
            println!("{:?}", (&node, prog_rest.toks()));
            //let stmt = stmts.get(0).unwrap();
            //println!("{:?}", (&stmt));
            let mut restored = node.unparse();
            restored.push(Tok::EOF);
            let ts = toks.iter().map(|v| v.tok.clone()).collect::<Vec<_>>();
            println!("restored {:?}", (&ts, &restored));
            let s = tokens.unlex();
            println!("{:?}", (&v, &s));
            assert_eq!(v, &s);
        });
    }

    #[test]
    fn unparse() {
        let r = vec![
            "\n123",
            "123",
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
        r.iter().for_each(|v| {
            assert!(parser_losslessness(v));
        });
    }

    #[test]
    fn sexpr() {
        let r = vec![
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
        ];

        r.iter().for_each(|(q, a)| {
            println!("q {:?}", (&q));
            let (_, toks) = lex_eof(q).unwrap();
            let tokens = Tokens::new(&toks[..]);
            println!("{:?}", (&toks));

            //let (prog_rest, mut expr) = parse_expr(tokens).unwrap();

            let (_, mut node) = parse_program_node(tokens).unwrap();
            println!("{:?}", (&node));
            //let stmt = stmts.get(0).unwrap();
            //println!("{:?}", (&stmt));
            let sexpr = node.sexpr().unwrap();
            println!("sexpr {}", &sexpr);
            let rendered = format!("{}", &sexpr);
            println!("sexpr {:?}", (&q, &sexpr, &rendered, a));
            assert_eq!(rendered, a.to_string());
        });
    }
}
