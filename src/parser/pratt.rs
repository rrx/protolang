/*
 * A Pratt Parser for Nom
 * Based heavily on this excellent article that explains Pratt Parsing
 * https://www.engr.mun.ca/~theo/Misc/pratt_parsing.htm
 */
//use crate::ast::*;
use crate::parser::{tag_token, take_one_any, PResult};
use crate::tokens::{Tok, Token, Tokens};
//use nom::{error_position, IResult};
use nom::error::{context, ErrorKind};
use nom::{multi, sequence};

use crate::ast::{Expr, ExprNode, Operator, OperatorNode, Unparse};

//type RNode<'a> = PResult<Tokens<'a>, ASTNode>;
type RNode<'a> = PResult<Tokens<'a>, ExprNode>;

type Prec = Option<i8>;

#[derive(Clone, Debug)]
struct Op {
    op: Tok,

    // left binding power
    lbp: Prec,

    // next binding power
    nbp: Prec,

    // right binding power determines the left binding power of the lowest precedence operator that
    // can be in a right operand
    // If rbp is None, then no right operand is possible
    // Only applicable for binary, or ternary operators
    rbp: Prec,
}

impl Op {
    pub fn new(op: &Tok, lbp: Prec, nbp: Prec, rbp: Prec) -> Self {
        Self {
            op: op.clone(),
            lbp,
            nbp,
            rbp,
        }
    }

    pub fn new_chaining(op: &Tok, p: i8) -> Self {
        Self::new(op, Some(p + 1), Some(p), Some(p + 2))
    }

    pub fn new_left_assoc(op: &Tok, p: i8) -> Self {
        // NBP=LBP=RBP-1
        Self::new(op, Some(p), Some(p), Some(p + 1))
    }

    pub fn new_right_assoc(op: &Tok, p: i8) -> Self {
        // NBP=LBP=RBP
        Self::new(op, Some(p), Some(p), Some(p))
    }

    pub fn new_default_left() -> Self {
        Self::new(&Tok::Assign, None, None, None)
    }

    pub fn try_from(op: &Tok) -> Option<Self> {
        match op {
            // non-associative ops, indicated by rbp = lbp + 1, nbp = lbp -1
            Tok::Assign => Some(Self::new_right_assoc(op, 10)),
            Tok::Elvis => Some(Self::new_left_assoc(op, 35)),
            Tok::Comma => Some(Self::new_left_assoc(op, 1)),
            Tok::SemiColon => Some(Self::new(op, Some(0), Some(127), None)),

            // Conditional
            Tok::Question => Some(Self::new_chaining(op, 35)),
            // O token associated with Conditional
            Tok::Colon => Some(Self::new_default_left()),

            // left associative ops, which are indicated by rbp = lbp + 1, nbp = lbp
            // NBP=LBP=RBP-1
            Tok::Plus | Tok::Minus => Some(Self::new_left_assoc(op, 20)),
            Tok::Mul | Tok::Div => Some(Self::new_left_assoc(op, 30)),
            Tok::Caret => Some(Self::new_left_assoc(op, 50)),

            // right associative ops, which are indicated by rbp = lbp
            // NBP=LBP=RBP
            //Tok::Caret => Some(Self::new_right_assoc(op, 50)),
            // Index
            Tok::LBracket => Some(Self::new_right_assoc(op, 60)),
            // Call
            Tok::LParen => Some(Self::new_right_assoc(op, 70)),

            // postfix ops, lack rbp
            // We bump up NBP, so that postfix operators are allowed to be left operands
            // NBP defines the highest precedence of an operator that this operator can be a left
            // operand of. 127, will allows to be the left operand for everything
            Tok::Exclamation => Some(Self::new(op, Some(40), Some(127), None)),

            Tok::Equals => Some(Self::new(op, Some(10), Some(9), Some(20))),
            Tok::NotEquals => Some(Self::new(op, Some(10), Some(9), Some(20))),

            Tok::LTE => Some(Self::new_left_assoc(op, 10)),
            Tok::LT => Some(Self::new_left_assoc(op, 10)),
            Tok::GTE => Some(Self::new_left_assoc(op, 10)),
            Tok::GT => Some(Self::new_left_assoc(op, 10)),

            Tok::Percent => Some(Self::new_right_assoc(op, 40)),
            _ => None,
        }
    }

    fn chain_LeD<'a>(&self, i: Tokens<'a>, x: &ASTNode, token: &Token, depth: usize) -> RNode<'a> {
        println!("chain_LeD1: {:?}", (&x, &token, &i.toks()));

        // parse the RHS, making sure the expression we are getting stops when we reach
        // a LBP that is equal to the current BP, this is why we pass in RBP+1
        let (i, y) = E(i, Some(self.rbp.unwrap() + 1), depth + 1)?;

        println!("chain_LeDx: {:?}", (&y, &token, &i.toks()));

        let maybe_op = Operator::from_tok(&token.tok);
        let op = maybe_op.unwrap();
        //let op = OperatorNode::from_location(&i, operator);
        //let t = PrattValue::Binary(op, Box::new(x.clone()), Box::new(y));

        //let op = Binary::from_token(&i.tok[0]).unwrap();
        let (left_op, c) = match &x.value {
            PrattValue::Chain(op, chain) => {
                //if false && op == token => {
                let mut c = chain.clone();
                c.push(y.clone());
                (op, c)
            }
            _ => (&op, vec![x.clone(), y.clone()]),
        };

        let n = i.peek().unwrap();
        //let next_tok = n.clone();//.tok.clone();
        println!("chain_LeD2: {:?}", (&left_op, &c, &n));

        //println!("chain: {:?}", (&i.toks()));
        if n.tok == Tok::EOF {
            println!("chain: got eof");
            let t = PrattValue::Binary(left_op.clone(), Box::new(x.clone()), Box::new(y));
            return i.node_success(t); //Ok((i, t));
        }

        let next_op = n.tok.op().unwrap();
        println!("chain next: {:?}", (self, &n));
        if self.lbp == next_op.lbp {
            // consume
            let (i, _) = take_one_any(i.clone())?;
            let (i, t) = self.chain_LeD(i, &y, &n, depth)?;
            println!("chain consume: {:?}", (self.lbp, next_op, &t));
            let t0 = PrattValue::Binary(left_op.clone(), Box::new(x.clone()), Box::new(y));
            let op = Operator::End; //Operator::from_tok(&Tok::End);
            let t = PrattValue::Chain(op, vec![i.node(t0), t]);
            i.node_success(t)
        } else {
            let t = PrattValue::Chain(left_op.clone(), c);
            println!("chain drop: {:?}", (self.lbp, next_op, &t));
            i.node_success(t)
        }
    }

    fn LeD<'a>(&self, i: Tokens<'a>, x: &ASTNode, token: &Token, depth: usize) -> RNode<'a> {
        // Given the LHS, and an op (token), return a Node
        //
        //println!("LeD: {:?}", (&x, &token, &i.toks()));
        let (i, mut t) = match token.tok {
            Tok::SemiColon => {
                let mut x = x.clone();
                x.context.s.append(token.expand_toks()); //vec![token.clone()]);
                (i, x)
            }

            // chaining
            Tok::Question => {
                // Ternary operator (x ? y : z)

                // match any precedence
                let (i, mut y) = E(i, Some(0), depth + 1)?;

                let (i, sep) = tag_token(Tok::Colon)(i)?;

                // match any precedence
                let (i, z) = E(i, Some(0), depth + 1)?;

                //let op = Binary::from_location(&i, Operator::Conditional);
                y.context.s.prepend(token.expand_toks());
                y.context.s.append(sep.expand_toks());
                let value = PrattValue::Ternary(
                    Operator::Conditional,
                    Box::new(x.clone()),
                    Box::new(y),
                    Box::new(z.clone()),
                );
                let node = i.node(value);
                (i, node)
            }
            /*
            Tok::Elvis =>{
                // match any precedence
                let (i, y) = E(i, Some(0), depth + 1)?;
                let op = Binary::from_location(&i, Operator::Elvis);
                let value = PrattValue::Binary(op, Box::new(x.clone()), Box::new(y));
                let node = i.node(value);
                (i, node)
            }
            */
            /*Tok::Assign |*/ //Tok::Comma => {//| Tok::LT | Tok::LTE | Tok::GT | Tok::GTE => {
                //let (i, y) = self.chain_LeD(i, x, &token.tok, depth)?;
                //(i, y)
            //}
            Tok::LParen => {
                let (i, (nodes, end)) =
                    sequence::pair(multi::many0(parse_expr), tag_token(Tok::RParen))(i)?;
                println!("nodes: {:?}", (&nodes, &end));
                //let (i, maybe_node) = Eopt(i, Some(0), depth+1)?;
                //let nodes = match maybe_node {
                //Some(node) => vec![node],
                //None => vec![]
                //};
                let op = Operator::Call;
                let mut f = x.clone();
                f.context.s.append(token.expand_toks());
                let mut node = ExprNode::new(Expr::Apply(Box::new(f), nodes), &i.to_location());
                node.context.s.append(end.expand_toks());
                (i, node)

                // application is slightly different than binary.  The RHS is optional
            }
            _ => {
                if token.tok.isBinary() {
                    // binary parses the RHS, and returns a binary node
                    let (i, y) = E(i, self.rbp, depth + 1)?;
                    println!("Binary: {:?} {:?} {:?}", &x, &token, &y);
                    println!("Binary: {:?}", &token);

                    let maybe_op = Operator::from_tok(&self.op);
                    let operator = maybe_op.unwrap();
                    //let op = Binary::from_location(&i, operator);
                    //let op = Binary::from_token(&token).unwrap();
                    let op = Operator::from_tok(&token.tok).unwrap();
                    let mut left = x.clone();
                    left.context.s.append(token.expand_toks()); //op.unparse());
                    let right = y;

                    let t = PrattValue::Binary(op, Box::new(left), Box::new(right));
                    let node = i.node(t);
                    (i, node)
                } else {
                    // postfix just returns, there's no RHS
                    println!("Postfix: {:?}", (&x, &token));
                    //let (_, maybe_op) = postfix_op(&self.op);
                    //let operator = maybe_op.unwrap();
                    //let op = OperatorNode::new(Operator::Bang);
                    let op = OperatorNode::from_postfix_token(token.clone()).unwrap();
                    let t = PrattValue::Postfix(op, Box::new(x.clone()));
                    let mut node = i.node(t);
                    node.context.s.append(token.expand_toks());

                    (i, node)
                }
            }
        };

        //
        // parse closing delimiter
        // we handle this as an exception, but it should be data driven
        // some ops expect closure, some do not
        let i = match &token.tok {
            /*
            Tok::LParen => {
                let (i, right) = tag_token(Tok::RParen)(i)?;
                t.context.s.append(right.expand_toks());
                i
            }
            */
            Tok::LBracket => {
                let (i, right) = tag_token(Tok::RBracket)(i)?;
                t.context.s.append(right.expand_toks());
                i
            }
            _ => i,
        };

        Ok((i, t))
    }
}

impl Tok {
    fn op(&self) -> Option<Op> {
        Op::try_from(self)
    }

    fn isBinary(&self) -> bool {
        if let Some(op) = self.op() {
            op.lbp.is_some() && op.rbp.is_some()
        } else {
            false
        }
    }

    fn isVariable(&self) -> bool {
        match self {
            Tok::IntLiteral(_) => true,
            Tok::Ident(_) => true,
            _ => false,
        }
    }

    fn nbp(&self) -> Prec {
        match self.op() {
            Some(op) => op.nbp,
            None => None,
        }
    }
    fn rbp(&self) -> Prec {
        match self.op() {
            Some(op) => op.rbp,
            None => None,
        }
    }
    fn lbp(&self) -> Prec {
        match self.op() {
            Some(op) => op.lbp,
            None => None,
        }
    }
}

pub type PrattValue = Expr;

pub type ASTNode = ExprNode;

impl<'a> Tokens<'a> {
    fn node(&self, value: PrattValue) -> ASTNode {
        ASTNode::new(value, &self.to_location())
    }

    fn node_success(self, value: PrattValue) -> RNode<'a> {
        let node = ASTNode::new(value, &self.to_location());
        Ok((self, node))
    }

    fn P(self, depth: usize) -> RNode<'a> {
        P(self, depth)
    }

    fn E(self, prec: Prec, depth: usize) -> RNode<'a> {
        E(self, prec, depth)
    }

    fn G(self, r: i8, t: ASTNode, prec: Prec, depth: usize) -> PResult<Tokens<'a>, (i8, ASTNode)> {
        G(self, r, t, prec, depth)
    }

    fn parse(self) -> RNode<'a> {
        parse(self)
    }
}

// Parse a prefix token, and return a full node
// Could be -(...), or (...), or a variable
fn P<'a>(i: Tokens<'a>, depth: usize) -> RNode<'a> {
    // P branch
    // peek
    let n = i.tok[0].tok.clone();
    let token = &i.tok[0];

    println!("P {:?}", (&n));

    let rbp = n.op().map_or(None, |t| t.rbp);

    // All possible N tokens
    match Some(&n) {
        Some(Tok::Minus) | Some(Tok::Plus) => {
            // consume prefix
            let (i, op_tokens) = take_one_any(i)?;

            // parse RHS of prefix operation
            let (i, t) = i.E(rbp, depth + 1)?;

            //let op = n.op().unwrap().op.clone();
            let op = OperatorNode::from_prefix_token(token).unwrap();
            let value = PrattValue::Prefix(op, Box::new(t));
            let mut node = i.node(value);
            node.context.s.prepend(op_tokens.expand_toks());
            Ok((i, node))
        }

        Some(Tok::Ident(_)) => {
            // consume variable
            ExprNode::parse_ident(i)
        }

        Some(Tok::IntLiteral(_) | Tok::StringLiteral(_) | Tok::FloatLiteral(_)) => {
            // consume literal
            ExprNode::parse_literal(i)
        }

        Some(Tok::Backslash) => ExprNode::parse_lambda(i),

        Some(Tok::LBrace) => {
            // consume LBrace
            let (i, left) = take_one_any(i)?;
            println!("prefix brace1: {:?}", (&i, &n, &left));
            let (i, t) = i.E(Some(0), depth + 1)?;
            println!("prefix brace2: {:?}", (&i, &t));
            // consume RBrace
            let (i, right) = context("r-brace", tag_token(Tok::RBrace))(i)?;
            let t = PrattValue::Block(vec![t]);
            let mut node = i.node(t);
            node.context.s.prepend(left.expand_toks());
            node.context.s.append(right.expand_toks());
            Ok((i, node))
        }

        // Array
        Some(Tok::LBracket) => {
            // consume LBracket
            let (i, left) = take_one_any(i)?;
            println!("prefix bracket1: {:?}", (&i, &n, &left));
            let (i, t) = i.E(Some(0), depth + 1)?;
            println!("prefix bracket2: {:?}", (&i, &t));
            // consume RBracket
            let (i, right) = context("r-bracket", tag_token(Tok::RBracket))(i)?;
            let t = PrattValue::List(vec![t]);
            let mut node = i.node(t);
            node.context.s.prepend(left.expand_toks());
            node.context.s.append(right.expand_toks());
            //i.node_success(t)
            Ok((i, node))
        }

        // Parenthesis
        Some(Tok::LParen) => {
            // consume LParen
            let (i, left) = take_one_any(i)?;
            println!("prefix paren1: {:?}", (&i.toks(), &left.toks()));

            // consume anything inside the parents, bp = 0
            let (i, mut node) = i.E(Some(0), depth + 1)?;

            //let (i, maybe_node) = Eopt(i, Some(0), depth+1)?;

            //let nodes = match maybe_node {
            //Some(node) => vec![node],
            //None => vec![]
            //};

            //let (i, mut maybe_node) = i.E(Some(0), depth+1)?;

            //let mut node = ExprNode::new(Expr::List(nodes), &i.to_location());
            println!("prefix paren2: {:?}", (&i.toks(), &node));
            let (i, right) = context("r-paren", tag_token(Tok::RParen))(i)?;
            node.context.s.prepend(left.expand_toks());
            node.context.s.append(right.expand_toks());
            //node_success(i, t)
            //let node = i.node(t);
            Ok((i, node))
        }

        // Non N-tokens, are handled as errors
        Some(Tok::EOF) | None => {
            println!("got eof3");
            // we got an EOF when we were expecting an N token
            Err(nom::Err::Error(nom::error_position!(i, ErrorKind::Eof)))
        }

        _ => {
            println!("got unexpected token {:?}", &n);
            Err(nom::Err::Error(nom::error_position!(i, ErrorKind::Tag)))
            //unreachable!()
        }
    }
}

// Optional E
fn Eopt<'a>(i: Tokens<'a>, prec: Prec, depth: usize) -> PResult<Tokens<'a>, Option<ExprNode>> {
    match E(i.clone(), Some(0), depth + 1) {
        Ok((i, node)) => Ok((i, Some(node))),
        Err(nom::Err::Error(_)) => Ok((i, None)),
        Err(e) => Err(e),
    }
}

// Parse an expression, it will return a Node
fn E<'a>(i: Tokens, prec: Prec, depth: usize) -> RNode {
    if i.is_eof() {
        return Err(nom::Err::Error(nom::error_position!(i, ErrorKind::Eof)));
    }

    // precondition p >= 0
    let _ = prec.unwrap();

    // r = +inf
    let r = 127;

    println!("E0 {:?}, prec: {:?}, depth:{}", i.toks(), prec, depth);

    // The P parser starts with an N token, and goes with it, returning a Node
    // This is the first element of the expression
    let (i, p) = i.P(depth)?;
    println!(
        "E1 {:?}, prec: {:?}, P:{:?}, depth:{}",
        i.toks(),
        prec,
        &p,
        depth
    );

    // get a chain of subsequent expressions
    // What follows could be a postfix operator, a binary operator, or a ternary operator
    // We figure this out by looking up the op
    // Given the p-node p, parse when follows.  p becomes the LHS, and we want to see what follows
    // It could be a postfix operator, or a binary/ternary op
    // G will parse what's next recursively, until we find something that has a lower precedence
    let (i, (r, t)) = i.G(r, p, prec, 0)?;

    println!("E2 {:?}", (&t, r, depth, i.toks()));
    Ok((i, t))
}

fn G<'a>(i: Tokens, r: i8, t: ASTNode, prec: Prec, depth: usize) -> PResult<Tokens, (i8, ASTNode)> {
    // Here we are going to take a look at the L token
    // A L token is a token that has a left operand (t)
    // An L token can never start an expression
    // We are given the LHS in `t`, and we are looking for an op
    // could be postfix, or binary, or ternary
    // We do this recursively until we find an operation that has a lower precedence
    // When we find an operation that has a lower precedence, we exit, returning the LHS
    //
    // peek

    println!("G: {:?}", (&i.toks(), &r, &prec, &r, depth));

    if i.is_eof() {
        return Ok((i, (r, t)));
    }
    let token = &i.tok[0]; //.tok.clone();
                           //if token.tok == Tok::EOF {
                           //println!("got eof");
                           //return Ok((i, (r, t)));
                           //}

    // get op from left
    // it could be any token
    let maybe_op = token.tok.op();
    let (i, op) = if let Some(op) = maybe_op {
        (i, op)
    } else {
        (i, Op::new_default_left())
    };

    println!("op: {:?}", (&token, &op));

    let lbp = op.lbp.unwrap_or(-1);
    println!(
        "guard: prec:{} <= lbp:{} <= r:{}\n\tLHS: {:?}\n\t{:?}",
        prec.unwrap(),
        lbp,
        r,
        t,
        (&op, &i.toks())
    );

    // lbp must be between r and prec, or we exit
    // if we have lbp greater than or equal to prec, then we parse and include the RHS
    // if lbp is great than r, then we are done
    // r is the previous operations nbp (next binding power)
    // if we set nbp low, we will match less in the next pass
    if prec.unwrap() <= lbp && lbp <= r {
        // consume
        let (i, _) = take_one_any(i)?;

        // LeD, parse the RHS, and return the appropriate node
        let t1 = t.clone();
        let (i, t) = op.LeD(i, &t, &token, depth)?;
        println!("LeD: {:?} -> {:?}", (&t1, &token), &t);

        // set r = NBP of op
        let r = op.nbp.unwrap();

        // loop recursively
        i.G(r, t, prec, depth + 1)
    } else {
        println!("guard exit: {:?}", &i.toks());
        Ok((i, (r, t)))
    }
}

pub fn parse_expr<'a>(i: Tokens) -> RNode {
    let (i, (mut node, end)) =
        sequence::pair(parse_expr1, multi::many0(tag_token(Tok::SemiColon)))(i)?;
    node.context
        .s
        .append(end.into_iter().map(|t| t.expand_toks()).flatten().collect());
    Ok((i, node))
}

pub fn parse_expr1<'a>(i: Tokens) -> RNode {
    i.E(Some(0), 0)
}

pub fn parse<'a>(i: Tokens) -> RNode {
    match parse_expr(i) {
        Ok((i, node)) => {
            println!("EXPR {:?}", (&node, &i.toks()));
            let (i, eof) = tag_token(Tok::EOF)(i)?;
            println!("EOF {:?}", (eof));
            Ok((i, node))
        }
        Err(nom::Err::Error(e)) => {
            for (tokens, err) in &e.errors {
                println!("error {:?}", (&err, tokens.toks()));
            }
            Err(nom::Err::Error(e))
        }
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::*;
    use crate::sexpr::SExpr;
    use nom::{InputIter, InputLength};

    #[test]
    fn expressions() {
        let r = vec![
            "\n1",
            "\n1\n",
            "1+2",
            "1 + 2",
            " 1 + 2 ",
            " x + y ",
            "- x * y ",
            "- 1 / (2 - 5)",
            "x!",
            "x^2",
            "-x!^2",
            "-x^2",
            "(1)+2!",
            "a!^b",
            "a^b!^c",
            "a!^b!^c!",
            "+1",
            "-x ?: y ",
            //"x+1 ; y+2",
            "(x)",
            "[x]",
            //"(1",
            "x ?: y",      // elvis
            "x ?: y ?: z", // chaining elvis
            "x ? y : z ",  // ternary conditional
            "a < b <= c",
            "a < b < c",
            "a=b=c",
            "x+1 = y+2",
            "1.2 + 3.4",
            "x[1]",
            "x(1)",
            "x()",
            "x,y",
            "x,y,z",
            "x1=y2=z2,y+1,z^2,x1=y=z",
            "x=y=z",
            "x!",
            "a! ^ b",
        ];
        r.iter().for_each(|v| {
            let mut lexer = LexerState::from_str_eof(v).unwrap();
            let i = lexer.tokens();
            println!("v {:?}", (&v));
            println!("tokens: {:?}", (i));

            i.iter_elements().for_each(|t| {
                println!("{:?}", t);
            });
            let (i, node) = i.parse().unwrap();
            let r = node.unlex();
            println!("v {:?}", (&v));
            //println!("NODE {:?}", (&node));
            println!("NODE {:?}", (&node.unparse()));
            println!("REM {:?}", (&i.toks()));
            println!("S {}", &node.sexpr().unwrap());
            println!("R {}", &r);
            assert_eq!(v, &r);
            assert_eq!(0, i.input_len());
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
            //("-a-b", "(- (- a b))"),
            ("-a-b", "(- (- a) b)"),
            //("-a+b", "(- (+ a b))"),
            ("-a+b", "(+ (- a) b)"),
            // exponents
            ("5^2", "(^ 5 2)"),
            ("1-5^2+1", "(+ (- 1 (^ 5 2)) 1)"),
            ("1-5^2", "(- 1 (^ 5 2))"),
            //("-1-5^2", "(- (- 1 (^ 5 2)))"),
            ("-1-5^2", "(- (- 1) (^ 5 2))"),
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
            //("x( 1 2 3)", "(apply x 1 2 3)"),
            //("(x+y)( 1 2 3)", "(apply (+ x y) 1 2 3)"),
            ("x = 1", "(= x 1)"),
            (
                // assignment is right associative
                // comma is left associative
                "x1=y2=z2,y+1,z^2,x1=y=z",
                "(, (, (, (= x1 (= y2 z2)) (+ y 1)) (^ z 2)) (= x1 (= y z)))",
            ),
            // comma in ternary op
            ("a ? b, c : d", "(? a (, b c) d)"),
            ("a! ^ b", "(^ (! a) b)"),
        ];

        r.iter().for_each(|(q, a)| {
            println!("q {:?}", (&q));

            let mut lexer = LexerState::from_str_eof(q).unwrap();
            let i = lexer.tokens();

            println!("tokens: {:?}", (i.toks()));

            i.iter_elements().for_each(|t| {
                println!("{:?}", t);
            });

            let (i, node) = i.parse().unwrap();
            println!("NODE {:?}", (&node.unparse()));
            println!("REM {:?}", (&i.toks()));
            //println!("S {}", &node.sexpr().unwrap());

            match node.sexpr() {
                Ok(sexpr) => {
                    let rendered = format!("{}", &sexpr);
                    println!("sexpr {:?}", (&q, &sexpr, a));
                    println!("S {}", &rendered);
                    assert_eq!(rendered, a.to_string());
                }
                Err(e) => {
                    println!("Error: {:?}", e);
                    assert!(false);
                }
            }
            assert_eq!(0, i.input_len());
        });
    }
}
