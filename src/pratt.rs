/*
 * A Pratt Parser for Nom
 * Based heavily on this excellent article that explains Pratt Parsing
 * https://www.engr.mun.ca/~theo/Misc/pratt_parsing.htm
 */
//use crate::ast::*;
use crate::tokens::{Tok,Tokens};
use crate::parser::{tag_token, PResult, take_one_any};
//use nom::{error_position, IResult};
use nom::error::{context, ErrorKind};
use crate::lexer::{Location, Surround};
use crate::ast::Unparse;
use crate::sexpr::{S, SExpr, SResult};

type RNode<'a> = PResult<Tokens<'a>, ASTNode>;
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
        Self { op: op.clone(), lbp, nbp, rbp }
    }

    pub fn new_chaining(op: &Tok, p: i8) -> Self {
        Self::new(op, Some(p+1), Some(p), Some(p+2))
    }

    pub fn new_left_assoc(op: &Tok, p: i8) -> Self {
        // NBP=LBP=RBP-1
        Self::new(op, Some(p), Some(p), Some(p+1))
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
            Tok::Assign => Some(Self::new_chaining(op, 10)),
            Tok::Elvis => Some(Self::new_chaining(op, 35)),
            Tok::Comma => Some(Self::new_chaining(op, 1)),

            // Conditional
            Tok::Question => Some(Self::new_chaining(op, 35)),
            // O token associated with Conditional
            Tok::Colon => Some(Self::new_default_left()),

            // left associative ops, which are indicated by rbp = lbp + 1, nbp = lbp
            // NBP=LBP=RBP-1
            Tok::Plus | Tok::Minus => Some(Self::new_left_assoc(op, 20)),
            Tok::Mul | Tok::Div => Some(Self::new_left_assoc(op, 30)),

            // right associative ops, which are indicated by rbp = lbp 
            // NBP=LBP=RBP
            Tok::Caret => Some(Self::new_right_assoc(op, 50)),
            Tok::LBracket => Some(Self::new_right_assoc(op, 60)),
            Tok::LParen => Some(Self::new_right_assoc(op, 70)),

            // postfix ops, lack rbp
            // We bump up NBP, so that postfix operators are allowed to be left operands
            // NBP defines the highest precedence of an operator that this operator can be a left
            // operand of. 127, will allows to be the left operand for everything
            Tok::Exclamation => Some(Self::new(op, Some(40), Some(127), None)),

            Tok::Equals => Some(Self::new(op, Some(10), Some(9), Some(20))),
            Tok::NotEquals => Some(Self::new(op, Some(10), Some(9), Some(20))),

            Tok::LTE => Some(Self::new_chaining(op, 10)),
            Tok::LT => Some(Self::new_chaining(op, 10)),
            Tok::GTE => Some(Self::new_chaining(op, 10)),
            Tok::GT => Some(Self::new_chaining(op, 10)),
            Tok::Percent => Some(Self::new_right_assoc(op, 40)),
            _ => None
        }
    }

    fn chain_LeD<'a>(&self, i: Tokens<'a>, x: &ASTNode, token: &Tok, depth: usize) -> RNode<'a> {

        println!("chain_LeD1: {:?}", (&x, &token, &i.toks()));

        // parse the RHS, making sure the expression we are getting stops when we reach
        // a LBP that is equal to the current BP, this is why we pass in RBP+1
        let (i, y) = E(i, Some(self.rbp.unwrap()+1), depth+1)?;

        let (left_op, c) = match &x.value {
            Value::Chain(op, chain) if false && op == token => {
                let mut c = chain.clone();
                c.push(y.clone());
                (op, c)
                //Value::Chain(op.clone(), c)
            }
            //_ => Value::Chain(self.op.clone(), vec![x.clone(), y.clone()])
            //_ => Value::Chain(token.clone(), vec![x.clone(), y.clone()])
            _ => (token, vec![x.clone(), y.clone()])
        };

        //let chain = vec![Box::new(x.clone()), Box::new(y.clone())];
        //chain.push(y.clone());
        //let mut chain = vec![x.clone(), y.clone()];//Box::new(x.clone()), Box::new(y.clone())];
        //let mut t = Value::Chain(self.op.clone(), chain);

        let n = i.peek().unwrap();
        let next_tok = n.tok.clone();
        println!("chain_LeD2: {:?}", (&left_op, &c, &n));

        //println!("chain: {:?}", (&i.toks()));
        if next_tok == Tok::EOF {
            println!("chain: got eof");
            //let mut t = Value::Chain(left_op.clone(), c);
            let t = Value::Binary(left_op.clone(), Box::new(x.clone()), Box::new(y));
            return i.node_success(t);//Ok((i, t));
        }

        let next_op = next_tok.op().unwrap(); 
        println!("chain next: {:?}", (self, &next_tok));
        if self.lbp == next_op.lbp {
            // consume
            let (i, _) = take_one_any(i)?;
            let (i, t) = self.chain_LeD(i, &y, &next_tok, depth)?;
            println!("chain consume: {:?}", (self.lbp, next_op, &t));
            /*
            let t = match t1 {
                Value::Chain(op, mut chain) if false && &op == left_op => {
                    //if &op == token {
                        c.append(&mut chain.iter().skip(1).cloned().collect::<Vec<_>>());
                        //let mut c = chain.clone();
                        //c.push(y.clone());
                        //c
                        Value::Chain(token.clone(), c)
                    //} else {
                        //Value::Chain(self.op.clone(), c)
                    //}
                }
                _ => {
                    //Value::Chain(next_tok, vec![Value::Chain(token.clone(), c), t1])
                    //Value::Chain(next_tok, vec![Value::Chain(token.clone(), c), t1])
                }
            };
            */
                            //Value::Chain(op.clone(), c)
            //c.push(t1);
            //let t0 = Value::Binary(self.op.clone(), Box::new(y.clone()), Box::new(t) );
            let t0 = Value::Binary(left_op.clone(), Box::new(x.clone()), Box::new(y));
            //let t0 = Value::Chain(self.op.clone(), c);
            //let t = Value::Chain(Tok::Percent, vec![t0, t]);//Box::new(t), Box::new(t1));
            let t = Value::Chain(Tok::Percent, vec![i.node(t0), t]);
            i.node_success(t)
        } else {
            let t = Value::Chain(left_op.clone(), c);
            println!("chain drop: {:?}", (self.lbp, next_op, &t));
            i.node_success(t)
            //Ok((i, t))
        }
    }

    fn LeD<'a>(&self, i: Tokens<'a>, x: &ASTNode, token: &Tok, depth: usize) -> RNode<'a> {
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

                //(i, Value::Ternary(Tok::Question, Box::new(x.clone()), Box::new(y), Box::new(z.clone())))
                let value = Value::Ternary(Tok::Question, Box::new(x.clone()), Box::new(y), Box::new(z.clone()));
                let node = i.node(value);
                (i, node)
            }
            Tok::Elvis =>{
                // match any precedence
                let (i, y) = E(i, Some(0), depth + 1)?;
                let value = Value::Binary(Tok::Elvis, Box::new(x.clone()), Box::new(y));
                let node = i.node(value);
                (i, node)
                //(i, Value::Binary(Tok::Elvis, Box::new(x.clone()), Box::new(y)))
            }
            Tok::Assign | Tok::Comma | Tok::LT | Tok::LTE | Tok::GT | Tok::GTE => {
                let (i, y) = self.chain_LeD(i, x, token, depth)?;
                (i, y)//node(i, y))
            }
            _ => {
                if token.isBinary() {
                    // binary parses the RHS, and returns a binary node
                    let (i, y) = E(i, self.rbp, depth+1)?;
                    println!("Binary: {:?} {:?} {:?}", &x, &token, &y);
                    let t = Value::Binary(self.op.clone(), Box::new(x.clone()), Box::new(y));
                    let node = i.node(t);
                    (i, node)
                } else {
                    // postfix just returns, there's no RHS
                    println!("Postfix: {:?}", (&x, &token));
                    let t = Value::Postfix(self.op.clone(), Box::new(x.clone()));
                    let node = i.node(t);
                    (i, node)
                    //(i, t)
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

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    Empty,
    Prefix(Tok, Box<ASTNode>),
    Binary(Tok, Box<ASTNode>, Box<ASTNode>),
    Ternary(Tok, Box<ASTNode>, Box<ASTNode>, Box<ASTNode>),
    Postfix(Tok, Box<ASTNode>),
    Literal(Tok),
    Ident(Tok),
    Error(String),
    List(Vec<ASTNode>),
    Chain(Tok, Vec<ASTNode>),
    Expr(Box<ASTNode>),
    Null,
    //Callable(CallableNode),
    Invalid(String),
}

impl Unparse for ASTNode {
    fn unparse(&self) -> Vec<Tok> {
        let mut out = vec![];
        match &self.value {
            Value::Empty => (),
            Value::Null => (),
            Value::Invalid(_) => (),
            Value::Error(_) => (),
            Value::Chain(_,_) => (),
            Value::Ternary(_,_,_,_) => (),
            Value::Ident(x) => {
                out.push(x.clone());
                //out.append(&mut x.unparse());
            }
            Value::Literal(x) => {
                out.push(x.clone());
            }
            Value::Expr(x) => {
                out.append(&mut x.unparse());
            }
            Value::Prefix(tok, expr) => {
                out.push(tok.clone());
                out.append(&mut expr.unparse());
            }
            Value::Postfix(tok, expr) => {
                out.append(&mut expr.unparse());
                out.push(tok.clone());
            }
            Value::Binary(op, left, right) => {
                out.append(&mut left.unparse());
                out.push(op.clone());
                //out.append(&mut op.unparse());
                out.append(&mut right.unparse());
            }
            Value::List(elements) => {
                for e in elements {
                    out.append(&mut e.unparse());
                }
            }
        };
        self.context.s.unparse(out)
    }
}
impl SExpr for ASTNode {
    fn sexpr(&self) -> SResult<S> {
        match &self.value {
            Value::Expr(x) => x.sexpr(),
            //Value::Ident(x) => x.sexpr(),
            Value::Binary(op, left, right) => {
                let sleft = left.sexpr()?;
                let sright = right.sexpr()?;
                Ok(S::Cons(op.unlex(), vec![sleft, sright]))
            }
            Value::Prefix(prefix, expr) => {
                let s = expr.sexpr()?;
                Ok(S::Cons(prefix.unlex(), vec![s]))
            }
            Value::Postfix(postfix, expr) => {
                let s = expr.sexpr()?;
                Ok(S::Cons(postfix.unlex(), vec![s]))
            }
            Value::List(elements) => {
                let s_args = elements
                    .iter()
                    .filter_map(|a| a.sexpr().ok())
                    .collect::<Vec<_>>();
                Ok(S::Cons("list".into(), s_args))
            }
            _ => unreachable!()
        }
    }
}

pub trait Context {
    fn from_location(loc: &Location) -> Self;
}

#[derive(PartialEq, Debug, Clone)]
pub struct NodeContextWithLocation {
    pub s: Surround,
    pub loc: Location,
}

impl Context for NodeContextWithLocation {
    fn from_location(loc: &Location) -> Self {
        Self { s: Surround::default(), loc: loc.clone() }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct NodeContextNull {
}

impl Context for NodeContextNull {
    fn from_location(_: &Location) -> Self {
        Self {}
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ASTNodeWithLocationImpl<C: Context> {
    pub context: C,
    pub value: Value,
}

pub type ASTNode = ASTNodeWithLocationImpl<NodeContextWithLocation>;

impl ASTNode {
    pub fn new(value: Value, loc: &Location) -> Self {
        Self {
            context:  NodeContextWithLocation::from_location(loc),
            value,
        }
    }
}

impl<'a> Tokens<'a> {
    fn node(&self, value: Value) -> ASTNode {
        ASTNode::new(value, &self.to_location())
    }

    fn node_success(self, value: Value) -> RNode<'a> {
        let node = ASTNode::new(value, &self.to_location());
        Ok((self, node))
    }

    fn P(self, depth: usize) -> RNode<'a> {
        P(self, depth)
    }

    fn E(self, prec: Prec, depth: usize) -> RNode<'a> {
        E(self, prec, depth)
    }

    fn G(self, r: i8, t: ASTNode, prec: Prec, depth: usize) -> PResult<Tokens<'a>,(i8, ASTNode)> {
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
    println!("P {:?}", (&n));

    let rbp = n.op().map_or(None, |t|t.rbp);

    // All possible N tokens
    match Some(&n) {
        Some(Tok::Minus) | Some(Tok::Plus) => {
            // consume prefix
            let (i, _) = take_one_any(i)?;
            let (i, t) = i.E(rbp, depth + 1)?;
            let op = n.op().unwrap().op.clone();
            let value = Value::Prefix(op, Box::new(t));
            i.node_success(value)
            //let node = ASTNode::new(value, &i.to_location());
            //Ok((i, node))
        }
        Some(Tok::Ident(s)) => {
            // consume variable
            let (i, t) = take_one_any(i)?;
            let value = Value::Ident(Tok::Ident(s.clone()));//s.clone());
            i.node_success(value)
            //let node = ASTNode::new(value, &i.to_location());
            //Ok((i, node))
        }
        Some(Tok::IntLiteral(n)) => {
            // consume literal
            let (i, _) = take_one_any(i)?;
            //let t = Value::Integer(*n);
            let t = Value::Literal(Tok::IntLiteral(*n));
            i.node_success(t)
            //Ok((i, t))
        }

        Some(Tok::FloatLiteral(n)) => {
            // consume literal
            let (i, _) = take_one_any(i)?;
            //let t = Value::Float(*n);
            let t = Value::Literal(Tok::FloatLiteral(*n));
            i.node_success(t)
            //Ok((i, t))
        }

        // Array
        Some(Tok::LBracket) => {
            // consume LBracket
            let (i, _) = take_one_any(i)?;
            println!("exp1: {:?}", (&i, &n));
            let (i, t) = i.E(Some(0), depth+1)?;
            println!("exp2: {:?}", (&i, &t));
            // consume RBracket
            let (i, _) = context("r-bracket", tag_token(Tok::RBracket))(i)?;
            let t = Value::List(vec![t]);
            i.node_success(t)
            //Ok((i, t))
        }

        // Parenthesis
        Some(Tok::LParen) => {
            // consume LParen
            let (i, x) = take_one_any(i)?;
            println!("L1: {:?}", (&i.toks(), &x.toks()));
            let (i, t) = i.E(rbp, depth+1)?;
            println!("L2: {:?}", (&i.toks(), &t));
            let (i, _) = context("r-paren", tag_token(Tok::RParen))(i)?;
            //node_success(i, t)

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
fn E<'a>(i: Tokens, prec: Prec, depth: usize) -> RNode {
    // precondition p >= 0
    let _ = prec.unwrap();

    // r = +inf
    let r = 127;

    // The P parser starts with an N token, and goes with it, returning a Node
    // This is the first element of the expression
    let (i, p) = i.P(depth)?;
    println!("E1 {:?}, prec: {:?}, P:{:?}, depth:{}", i.toks(), prec, &p, depth);

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

fn G<'a>(i: Tokens, r: i8, t: ASTNode, prec: Prec, depth: usize) -> PResult<Tokens,(i8, ASTNode)> {
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
    println!("guard: {:?}", (&op, prec.unwrap(), lbp, r, &t, &i.toks()));

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
        i.G(r, t, prec, depth+1)
    } else {
        println!("guard exit: {:?}", &i.toks());
        Ok((i, (r, t)))
    }
}

fn parse<'a>(i: Tokens) -> RNode {
    match i.E(Some(0), 0) {
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
    use nom::{InputLength, InputIter, InputTake, Slice};

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
            "x+1 = y+2",
            "(x)",
            "[x]",
            "x[1]",
            "x(1)",
            //"(1",
            "x ?: y",  // elvis
            "x ?: y ?: z", // chaining elvis
            "x ? y : z ",  // ternary conditional
            "x,y",
            "a < b <= c",
            "a < b < c",
            "a=b=c",
            "x1=y2=z2,y+1,z^2,x1=y=z",
            "x,y,z",
            "1.2 + 3.4",
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
            println!("NODE {:?}", (&node));
            println!("REM {:?}", (&i.toks()));
            assert_eq!(0, i.input_len());
        });
    }

}

