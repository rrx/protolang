use crate::ast::*;
use crate::tokens::{Tok,Tokens};
use nom::{InputLength, InputIter, InputTake, Slice};
type Prec = Option<i8>;
use crate::parser::{tag_token, PResult, take_one_any};
use nom::{error_position, IResult};
use nom::error::{context, ErrorKind};
type RNode<'a> = PResult<Tokens<'a>, Node>;

#[derive(Clone, Debug)]
struct Op {
    op: Operator,
    //isBinary: bool,
    // left binding power
    lbp: Prec,
    nbp: Prec,

    // right binding power determines the left binding power of the lowest precedence operator that
    // can be in a right operand
    // If rbp is None, then no right operand is possible
    // Only applicable for binary, or ternary operators
    rbp: Prec,
}

impl Op {
    pub fn new(op: Operator, lbp: Prec, nbp: Prec, rbp: Prec) -> Self {
        Self { op, lbp, nbp, rbp }
    }

    pub fn new_chaining(op: Operator, p: i8) -> Self {
        Self::new(op, Some(p), Some(p-1), Some(p+1))
    }

    pub fn new_left_assoc(op: Operator, p: i8) -> Self {
        // NBP=LBP=RBP-1
        Self::new(op, Some(p), Some(p), Some(p+1))
    }

    pub fn new_default_left() -> Self {
        Self::new(Operator::Assign, None, None, None)
    }

    pub fn from(op: Operator) -> Self {
        match op {
            // non-associative ops, indicated by rbp = lbp + 1, nbp = lbp -1 
            Operator::Assign => Self::new_chaining(op, 10),//Some(10), Some(9), Some(20)),
            Operator::Elvis => Self::new_chaining(op, 35),
            Operator::Conditional => Self::new_chaining(op, 35),
            // O token associated with Conditional
            Operator::ConditionalElse => Self::new_default_left(),

            // left associative ops, which are indicated by rbp = lbp + 1, nbp = lbp
            // NBP=LBP=RBP-1
            Operator::Plus | Operator::Minus => Self::new(op, Some(20), Some(20), Some(21)),
            Operator::Multiply | Operator::Divide => Self::new(op, Some(30), Some(30), Some(31)),

            // right associative ops, which are indicated by rbp = lbp 
            // NBP=LBP=RBP
            Operator::Exp => Self::new(op, Some(50), Some(50), Some(50)),
            Operator::Index => Self::new(op, Some(60), Some(60), Some(61)),
            Operator::Call => Self::new(op, Some(70), Some(70), Some(71)),

            // postfix ops, lack rbp
            // We bump up NBP, so that postfix operators are allowed to be left operands
            // NBP defines the highest precedence of an operator that this operator can be a left
            // operand of. 127, will allows to be the left operand for everything
            Operator::Bang => Self::new(op, Some(40), Some(127), None),

            Operator::Equal => Self::new(op, Some(10), Some(9), Some(20)),
            Operator::NotEqual => Self::new(op, Some(10), Some(9), Some(20)),
            Operator::LessThanEqual => Self::new(op, Some(10), Some(9), Some(20)),
            Operator::LessThan => Self::new(op, Some(10), Some(9), Some(20)),
            Operator::GreaterThanEqual => Self::new(op, Some(10), Some(9), Some(20)),
            Operator::GreaterThan => Self::new(op, Some(10), Some(9), Some(20)),
            Operator::Modulus => Self::new(op, Some(40), Some(40), Some(40)),

            // Final
            Operator::End => Self::new_default_left(),
        }
    }

    /*
    fn elvis_LeD<'a>(&self, i: Tokens<'a>, x: &Node, token: &Tok, depth: usize) -> RNode<'a> {

        // match any precedence
        let (i, y) = E(i, Some(0), depth + 1)?;

        let n = i.tok[0].tok.clone();
        //let n = i.peek().unwrap();
        println!("check ternary: {:?}", (&i.toks(), &y, &n, &token));
        if n == Tok::Colon {
            // consume
            let (i, _) = take_one_any(i)?;

            let (i, z) = E(i, Some(0), depth + 1)?;
            Ok((i, Node::Ternary(Operator::Elvis, Box::new(x.clone()), Box::new(y), Box::new(z.clone()))))
        } else {
            Ok((i, Node::Binary(Operator::Elvis, Box::new(x.clone()), Box::new(y))))
        }
    }
    */

    fn chain_LeD<'a>(&self, i: Tokens<'a>, x: &Node, token: &Tok, depth: usize) -> RNode<'a> {
        println!("chain_LeD1: {:?}", (&x, &token, &i.toks()));

        // parse the RHS, making sure the expression we are getting stops when we reach
        // a LBP that is equal to the current BP, this is why we pass in RBP+1
        let (i, y) = E(i, Some(self.rbp.unwrap()+1), depth+1)?;
        let t = Node::Binary(self.op.clone(), Box::new(x.clone()), Box::new(y.clone()));
        println!("chain_LeD2: {:?}", &t);

        let n = i.peek().unwrap();
        let tok = n.tok.clone();

        println!("chain: {:?}", (&t, &i.toks()));
        if tok == Tok::EOF {
            println!("got eof2");
            return Ok((i, t));
        }

        let next_op = tok.op().unwrap(); 
        if self.lbp == next_op.lbp {
            // consume
            let (i, _) = take_one_any(i)?;
            let (i, t1) = self.chain_LeD(i, &y, &tok, depth)?;
            let t = Node::Binary(Operator::Modulus, Box::new(t), Box::new(t1));
            Ok((i, t))
        } else {
            Ok((i, t))
        }
    }

    fn LeD<'a>(&self, i: Tokens<'a>, x: &Node, token: &Tok, depth: usize) -> RNode<'a> {
        // Given the LHS, and an op (token), return a Node
        //
        //println!("LeD: {:?}", (&x, &token, &i.toks()));
        let (i, t) = match token {
            // chaining
            Tok::Question => {
                // Ternary operator (x ? y : z)
                
                // match any precedence
                let (i, y) = E(i, Some(0), depth + 1)?;
        
                let (i, _) = tag_token(Tok::Colon)(i)?;

                // match any precedence
                let (i, z) = E(i, Some(0), depth + 1)?;

                (i, Node::Ternary(Operator::Conditional, Box::new(x.clone()), Box::new(y), Box::new(z.clone())))
            }
            Tok::Elvis =>{
                // match any precedence
                let (i, y) = E(i, Some(0), depth + 1)?;
                (i, Node::Binary(Operator::Elvis, Box::new(x.clone()), Box::new(y)))
            }
            Tok::Assign => {
                let (i, y) = self.chain_LeD(i, x, token, depth)?;
                (i, y)
            }
            _ => {
                if token.isBinary() {
                    // binary parses the RHS, and returns a binary node
                    let (i, y) = E(i, self.rbp, depth+1)?;
                    println!("Binary: {:?} {:?} {:?}", &x, &token, &y);
                    let t = Node::Binary(self.op.clone(), Box::new(x.clone()), Box::new(y));
                    (i, t)
                } else {
                    // postfix just returns, there's no RHS
                    println!("Postfix: {:?}", (&x, &token));
                    let t = Node::Postfix(self.op.clone(), Box::new(x.clone()));
                    (i, t)
                }
            }
        };

        //
        // parse closing delimiter
        // we handle this as an exception, but it should be data driven
        // some ops expect closure, some do not
        let i = match &token {
            Tok::LParen => {
                let (i, _) = tag_token(Tok::RParen)(i)?;
                i
            }
            Tok::LBracket => {
                let (i, _) = tag_token(Tok::RBracket)(i)?;
                i
            }
            _ => i
        };

        Ok((i, t))
    }
}

impl Tok {
    fn op(&self) -> Option<Op> {
        match self {
            Tok::Assign => Some(Op::from(Operator::Assign)),
            Tok::Plus => Some(Op::from(Operator::Plus)),
            Tok::Minus => Some(Op::from(Operator::Minus)),
            Tok::Mul => Some(Op::from(Operator::Multiply)),
            Tok::Div => Some(Op::from(Operator::Divide)),
            Tok::Exclamation => Some(Op::from(Operator::Bang)),
            Tok::Caret => Some(Op::from(Operator::Exp)),
            Tok::LBracket => Some(Op::from(Operator::Index)),
            Tok::LParen => Some(Op::from(Operator::Call)),
            Tok::Elvis => Some(Op::from(Operator::Elvis)),
            Tok::Colon => Some(Op::from(Operator::ConditionalElse)),
            Tok::Question => Some(Op::from(Operator::Conditional)),
            _ => None
        }
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
            _ => false
        }
    }

    fn nbp(&self) -> Prec {
        match self.op() {
            Some(op) => op.nbp,
            None => None
        }
    }
    fn rbp(&self) -> Prec {
        match self.op() {
            Some(op) => op.rbp,
            None => None
        }
    }
    fn lbp(&self) -> Prec {
        match self.op() {
            Some(op) => op.lbp,
            None => None
        }
    }
}

#[derive(Clone, Debug)]
enum Node {
    Empty,
    Prefix(Operator, Box<Node>),
    Binary(Operator, Box<Node>, Box<Node>),
    Ternary(Operator, Box<Node>, Box<Node>, Box<Node>),
    Postfix(Operator, Box<Node>),
    Var(String),
    Error(String),
    List(Vec<Node>),
}

// Parse a prefix token, and return a full node
// Could be -(...), or (...), or a variable
fn P<'a>(i: Tokens, depth: usize) -> RNode {
    // P branch
    // peek
    let n = i.tok[0].tok.clone();
    println!("P {:?}", (&n));

    let rbp = n.op().map_or(None, |t|t.rbp);

    // All possible N tokens
    match Some(&n) {
        Some(Tok::Minus) | Some(Tok::Plus) => {
            // consume prefix
            let (i, _) = take_one_any(i)?;
            let (i, t) = E(i, rbp, depth + 1)?;
            Ok((i, Node::Prefix(n.clone().op().unwrap().op.clone(), Box::new(t))))
        }
        Some(Tok::Ident(s)) => {
            // consume variable
            let (i, _) = take_one_any(i)?;
            let t = Node::Var(s.clone());
            Ok((i, t))
        }
        Some(Tok::IntLiteral(n)) => {
            // consume literal
            let (i, _) = take_one_any(i)?;
            let t = Node::Var(n.to_string());
            Ok((i, t))
        }

        // Array
        Some(Tok::LBracket) => {
            // consume LBracket
            let (i, _) = take_one_any(i)?;
            println!("exp1: {:?}", (&i, &n));
            let (i, t) = E(i, Some(0), depth+1)?;
            println!("exp2: {:?}", (&i, &t));
            // consume RBracket
            let (i, _) = context("r-bracket", tag_token(Tok::RBracket))(i)?;
            let t = Node::List(vec![t]);
            Ok((i, t))
        }

        // Parenthesis
        Some(Tok::LParen) => {
            // consume LParen
            let (i, x) = take_one_any(i)?;
            println!("L1: {:?}", (&i.toks(), &x.toks()));
            let (i, t) = E(i, rbp, depth+1)?;
            println!("L2: {:?}", (&i.toks(), &t));
            let (i, _) = context("r-paren", tag_token(Tok::RParen))(i)?;
            Ok((i, t))
        }

        // Non N-tokens, are handled as errors
        Some(Tok::EOF) | None => {
            println!("got eof3");
            // we got an EOF when we were expecting an N token
            Err(nom::Err::Error(nom::error_position!(i, ErrorKind::Eof)))
        }

        _ => {
            println!("got unexpected token {:?}", &n);
            Err(nom::Err::Error(nom::error_position!(i, ErrorKind::Fail)))
            //unreachable!()
        }
    }
}


// Parse an expression, it will return a Node
fn E<'a>(input: Tokens, prec: Prec, depth: usize) -> RNode {
    // precondition p >= 0
    let _ = prec.unwrap();

    // r = +inf
    let r = 127;

    // The P parser starts with an N token, and goes with it, returning a Node
    // This is the first element of the expression
    let (i, p) = P(input, depth)?;
    println!("E1 {:?}, prec: {:?}, P:{:?}, depth:{}", i.toks(), prec, &p, depth);

    // get a chain of subsequent expressions
    // What follows could be a postfix operator, a binary operator, or a ternary operator
    // We figure this out by looking up the op
    // Given the p-node p, parse when follows.  p becomes the LHS, and we want to see what follows
    // It could be a postfix operator, or a binary/ternary op
    // G will parse what's next recursively, until we find something that has a lower precedence
    let (i, (r, t)) = G(i, r, p, prec, 0)?;

    println!("E2 {:?}", (&t, r, depth, i.toks()));
    Ok((i, t))
}

fn G<'a>(i: Tokens, r: i8, t: Node, prec: Prec, depth: usize) -> PResult<Tokens,(i8, Node)> {
    // Here we are going to take a look at the L token
    // A L token is a token that has a left operand (t)
    // An L token can never start an expression
    // We are given the LHS in `t`, and we are looking for an op
    // could be postfix, or binary, or ternary
    // We do this recursively until we find an operation that has a lower precedence
    // When we find an operation that has a lower precedence, we exit, returning the LHS
    //
    // peek
    let token = i.tok[0].tok.clone();
    println!("G: {:?}", (&i.toks(), &r, &prec, &r, &token, depth));
    if token == Tok::EOF {
        println!("got eof");
        return Ok((i, (r, t)));
    }

    // get op from left
    // it could be any token
    let maybe_op = token.op();
    let (i, op) = if let Some(op) = maybe_op {
        (i, op)
    } else {
        (i, Op::new_default_left())
    };

    println!("op: {:?}", (&token, &op));

    let lbp = op.lbp.unwrap_or(-1);
    println!("guard: {:?}", (prec.unwrap(), lbp, r, &t));

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
        G(i, r, t, prec, depth+1)
    } else {
        println!("guard exit: {:?}", i);
        Ok((i, (r, t)))
    }
}

fn parse<'a>(i: Tokens) -> RNode {
    match E(i, Some(0), 0) {
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
            "a=b=c",
            "x+1 = y+2",
            "(x)",
            "[x]",
            "x[1]",
            "x(1)",
            //"(1",
            "x ?: y",  // elvis
            "x ?: y ?: z", // chaining elvis
            "x ? y : z ",  // ternary conditional
        ];
        r.iter().for_each(|v| {
            let mut lexer = LexerState::from_str_eof(v).unwrap();
            let mut i = lexer.tokens();
            println!("v {:?}", (&v));
            println!("tokens: {:?}", (i));

            i.iter_elements().for_each(|t| {
                println!("{:?}", t);
            });
            let (i, node) = parse(i).unwrap();
            println!("NODE {:?}", (&node));
            println!("REM {:?}", (&i.toks()));
            assert_eq!(0, i.input_len());
        });
    }

}

