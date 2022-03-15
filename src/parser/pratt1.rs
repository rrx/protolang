use nom::*;

use crate::ast::*;
use crate::results::*;
use crate::tokens::*;
use nom::branch::*;
use nom::bytes::complete::take;
use nom::combinator::{self, into, opt, verify};
use nom::error::{context, ErrorKind, VerboseError};
use nom::multi::{many0};
use nom::sequence::*;
use nom::Err;
use std::result::Result::*;
use super::{parse_atom, parse_prefix, take_one_any, tag_token};

pub(crate) type PResult<I, O> = IResult<I, O, VerboseError<I>>;

pub fn parse_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    context("expr", _parse_expr)(i)
}
pub fn _parse_expr(i: Tokens) -> PResult<Tokens, ExprNode> {
    parse_pratt_expr(i, Precedence::PLowest)
}

fn parse_pratt_expr(input: Tokens, precedence: Precedence) -> PResult<Tokens, ExprNode> {
    // parse N token
    println!("parse-pratt {:?}", (&precedence, &input.toks()));
    let (i0, maybe_unary) = opt(parse_prefix)(input)?;
    let (i1, expr) = match maybe_unary {
        Some(unary) => {
            println!("pratt unary: {:?}", &unary);
            //let (_, _) = prefix_op(&unary.value.token());
            let (i1, expr) = parse_pratt_expr(i0, Precedence::PLessGreater)?;
            println!("pratt unary expr: {:?}", (&i1, &expr));
            let loc = unary.context.loc.clone();
            let mut node = ExprNode::new(Expr::Prefix(unary.clone(), Box::new(expr)), &loc);
            node.context.s.prepend(unary.unparse());

            println!("pratt unary result: {:?}", (&node));
            (i1, node)
        }
        None => {
            let (i1, left) = parse_atom(i0)?;
            println!("pratt atom: {:?}", &left.value);
            let (i2, r) = go_parse_pratt_expr(i1, precedence, left)?;
            println!("pratt rest: {:?}", (&i2, &r));
            (i2, r)
        }
    };
    Ok((i1, expr))

    /*
    let (i2, maybe_postfix) = opt(parse_postfix)(i1)?;
    match maybe_postfix {
        Some(unary) => {
            let loc = unary.loc.clone();
            let value = ExprNode::new(Expr::Prefix(unary, Box::new(expr)), &loc);
            Ok((i2, value))
        }
        None => Ok((i2, expr))
    }
    */
}

fn go_parse_pratt_expr(
    input: Tokens,
    precedence: Precedence,
    mut left: ExprNode,
) -> PResult<Tokens, ExprNode> {
    // parse L token
    let (i1, t1) = take_one_any(input.clone())?;

    // if we have a LHS, and nothing remains, just return LHS
    if t1.tok.is_empty() {
        println!("go-empty");
        Ok((i1, left))
    } else {
        // inspect the next element, if it's a valid op
        let preview = &t1.tok[0];

        let p = infix_op(&preview.tok);
        println!("infix: {:?}", (&preview, &p));
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

            (Precedence::PBang, _) if precedence < Precedence::PBang => {

                let (i2, left2) = parse_index_expr(input, left)?;
                go_parse_pratt_expr(i2, precedence, left2)
            }
            
            // otherwise we just return the LHS
            (Precedence::PHighest, _) => {
                println!("high: {:?}", &input);
                let (i2, token) = tag_token(Tok::SemiColon)(input)?;
                left.context.s.append(token.expand_toks());
                Ok((i2, left))
            }

            // if the precedence of the next op is greater then the current precedence,
            // then we include it in this expr, and try to parse the RHS
            (ref peek_precedence, _) if precedence < *peek_precedence => {
                println!("p nest");
                let (i2, left2) = parse_infix_expr(input, left)?;
                let (i, node) = go_parse_pratt_expr(i2, precedence, left2.clone())?;
                //node.context.s.prepend(left2.unparse());
                Ok((i, node))
            }
            _ => {
                println!("p exit");
                Ok((input, left))
            }
        }
    }
}

fn parse_call_expr(i: Tokens, mut left: ExprNode) -> PResult<Tokens, ExprNode> {
    let (i, open) = tag_token(Tok::LParen)(i)?;
    //let (i2, args) = parse_pratt_expr(i, Precedence::PCall)?;//many0(parse_expr)(i)?;
    let (i2, args) = many0(parse_expr)(i)?;
    let (i3, close) = tag_token(Tok::RParen)(i2)?;
    left.context.s.append(open.expand_toks());
    let loc = open.to_location();
    let mut expr = ExprNode::new(Expr::Apply(Box::new(left), args), &loc);
    //let mut expr = ExprNode::new(Expr::Lambda(Lambda::new(Box::new(left), args), loc);
    expr.context.s.append(close.expand_toks());
    Ok((i3, expr))
}

fn parse_index_expr(i: Tokens, mut left: ExprNode) -> PResult<Tokens, ExprNode> {
    let (i, open) = tag_token(Tok::LBracket)(i)?;
    let (i2, index) = parse_pratt_expr(i, Precedence::PIndex)?;
    let (i3, close) = tag_token(Tok::RBracket)(i2)?;
    left.context.s.append(open.expand_toks());
    let loc = index.context.loc.clone();
    let mut expr = ExprNode::new(Expr::Index(Box::new(left), Box::new(index)), &loc);
    expr.context.s.append(close.expand_toks());
    Ok((i3, expr))
}

fn parse_infix_expr(i: Tokens, left: ExprNode) -> PResult<Tokens, ExprNode> {
    let (i, t) = take_one_any(i.clone())?;
    let token = &t.tok[0];
    match infix_op(&token.tok) {
        (precedence, Some(infix)) => {
            println!("{:?}", (&precedence, &infix, &i.toks()));
            let (i2, mut right) = parse_pratt_expr(i, precedence)?;
            right.context.s.prepend(token.expand_toks());
            let node = ExprNode::new(
                Expr::Binary(infix, Box::new(left), Box::new(right)),
                &i2.to_location(),
                );
            Ok((i2, node))
        }
        _ => Err(Err::Error(error_position!(i, ErrorKind::Tag))),
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::*;
    use nom::multi::many1;
    use crate::sexpr::SExpr;

    use super::super::tests::parser_losslessness;

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
    fn sexpr() {
        let r = vec![
            ("+1", "(+ 1)"),
            ("+ 1", "(+ 1)"),
            ("123", "123"),
            ("-123", "(- 123)"),

            ("- 1 / (2 - 5)", "(- (/ 1 (- 2 5)))"),
            ("+ 1 / (2 - 5)", "(+ (/ 1 (- 2 5)))"),
            // handle ambiguous div correctly
            ("1/2/3", "(/ (/ 1 2) 3)"),
            ("a*-b", "(* a (- b))"),
            ("-a*b", "(- (* a b))"),
            ("-a/b", "(- (/ a b))"),

            // Not sure what's correct here
            // if the prefix has precedence over the infix
            ("-a-b", "(- (- a b))"),
            ("-a+b", "(- (+ a b))"),

            // exponents
            ("5^2", "(^ 5 2)"),
            ("1-5^2+1", "(+ (- 1 (^ 5 2)) 1)"),
            ("1-5^2", "(- 1 (^ 5 2))"),

            ("-1-5^2", "(- (- 1 (^ 5 2)))"),

            // handle prefix properly
            ("-5^2", "(- (^ 5 2))"),
            ("-x^y", "(- (^ x y))"),

            // make sure prefix works
            ("-a*-b", "(- (* a (- b)))"),

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
            ("x = 1", "(= x 1)"),
        ];

        r.iter().for_each(|(q, a)| {
            println!("q {:?}", (&q));
            let mut lexer = LexerState::default();
            let (_, _) = lexer.lex_eof(q).unwrap();
            let tokens = lexer.tokens();
            println!("{:?}", (&tokens.toks()));
            let (i, expr) = parse_expr(tokens).unwrap();
            println!("EXPR {:?}", (&expr));
            match expr.sexpr() {
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
}
