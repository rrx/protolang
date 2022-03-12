use crate::ast::*;
use crate::tokens::{Tok,Tokens};
use nom::{InputLength, InputIter, InputTake, Slice};
type Prec = Option<i8>;
use crate::parser::{tag_token, PResult, take_one_any};
use nom::IResult;
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
            Operator::ElvisElse => Self::new_chaining(op, 100),

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
            Operator::End => Self::new_default_left(),//Self::new(op, Some(0), Some(0), None),
        }
    }

    fn elvis_LeD<'a>(&self, i: Tokens<'a>, x: Node, token: Tok) -> RNode<'a> {
        //let (mut inx2, mut t) = self.chain_LeD(i, inx, x, token);
        let (i, y) = E(i, Some(0))?;//self.rbp.unwrap()+1));

        //let n = i.get(inx2).unwrap();
        let n = i.peek().unwrap();
        println!("check ternary: {:?}", (&i, &y, &n, &token));
        if n.tok == Tok::Colon {
            //if let Node::Binary(op, x, y) = t {
                // consume
                let (i, _) = take_one_any(i)?;

                let (i, z) = E(i, Some(0))?;//self.rbp.unwrap()+1));
                //inx2 = inx3;
                Ok((i, Node::Ternary(Operator::Elvis, Box::new(x), Box::new(y), Box::new(z.clone()))))
            //} else {
                //unreachable!()
            //}
        } else {
            Ok((i, Node::Binary(Operator::Elvis, Box::new(x), Box::new(y))))
        }
        //(inx2, t)
    }

    fn chain_LeD<'a>(&self, i: Tokens<'a>, x: Node, token: Tok) -> RNode<'a> {
        println!("chain_LeD1: {:?}", (&x, &token, &i.toks()));
        let (i, y) = E(i, Some(self.rbp.unwrap()+1))?;
        let t = Node::Binary(self.op.clone(), Box::new(x), Box::new(y.clone()));
        println!("chain_LeD2: {:?}", &t);

        let n = i.peek().unwrap();
        let tok = n.tok.clone();

        //let (i, n) = take_one_any(i)?;
        //let tok = n.tok[0].tok.clone();
        
        println!("chain: {:?}", (&t, &i.toks()));
        if tok == Tok::EOF {
            //let (i, _) = take_one_any(i)?;
            //let i = i.take(1);
            println!("got eof2");
            return Ok((i, t));
        }

        let nextOp = tok.op().unwrap(); 
        if self.lbp == nextOp.lbp {
            let (i, _) = take_one_any(i)?;
            //let i = i.take(1);//inx += 1; //consume
            let (i, t1) = self.chain_LeD(i, y, tok.clone())?;
            let t = Node::Binary(Operator::Modulus, Box::new(t), Box::new(t1));
            Ok((i, t))
        } else {
            Ok((i, t))
        }
    }

    fn LeD<'a>(&self, i: Tokens<'a>, x: Node, token: Tok) -> RNode<'a> {
        //println!("LeD: {:?}", (&x, &token, &i.toks()));
        match token {
            // chaining
            Tok::Elvis =>{
                let (i, y) = self.elvis_LeD(i, x.clone(), token.clone())?;
                Ok((i, y))
            }
            Tok::Assign => {
                let (i, y) = self.chain_LeD(i, x.clone(), token.clone())?;
                Ok((i, y))
            }
            _ => {
                if token.isBinary() {
                    // binary parses the RHS, and returns a binary node
                    let (i, y) = E(i, self.rbp)?;
                    println!("Binary: {:?} {:?} {:?}", &x, token, &y);
                    let t = Node::Binary(self.op.clone(), Box::new(x), Box::new(y));
                    Ok((i, t))//Node::Binary(self.op.clone(), Box::new(x), Box::new(y))))
                } else {
                    // postfix just returns, there's no RHS
                    println!("Postfix: {:?}", (&x, token));
                    let t = Node::Postfix(self.op.clone(), Box::new(x));
                    Ok((i, t))//Node::Postfix(self.op.clone(), Box::new(x))))
                }
            }
        }
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
            Tok::Colon => Some(Op::from(Operator::ElvisElse)),
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
    Error(String)
}

// Parse a prefix token, and return a full node
// Could be -(...), or (...), or a variable
fn P<'a>(i: Tokens) -> RNode {
    //let n = i.peek();
    //let (i, tokens) = take_one_any(i)?;
    // peek
    let n = i.tok[0].tok.clone();
    //let n = tokens.tok[0].tok.clone();
    println!("P {:?}", (&n));
    // P branch
    //let maybe_tok = n;//n.map(|t| t.tok.clone());
    let rbp = n.op().map_or(None, |t|t.rbp);

    match Some(&n) {
        Some(Tok::Minus) | Some(Tok::Plus) => {
            let (i, _) = take_one_any(i)?;
            //let i = i.take(1);
            //let op = &n.unwrap().op().unwrap();
            let (i, t) = E(i, rbp)?;
            Ok((i, Node::Prefix(n.clone().op().unwrap().op.clone(), Box::new(t))))
        }
        Some(Tok::Ident(s)) => {
            let (i, _) = take_one_any(i)?;
            //let i = i.take(1);
            let t = Node::Var(s.clone());
            Ok((i, t))
        }
        Some(Tok::IntLiteral(n)) => {
            let (i, _) = take_one_any(i)?;
            //let i = i.take(1);
            let t = Node::Var(n.to_string());
            Ok((i, t))
        }
        Some(Tok::LBracket) => {
            let (i, _) = take_one_any(i)?;
            //let i = i.take(1);
            //let op = &n.unwrap().op().unwrap();
            println!("exp1: {:?}", (&i, &n));
            let (i, t) = E(i, Some(0))?;// op.rbp);
            println!("exp2: {:?}", (&i, &t));
            let (i, _) = tag_token(Tok::RBracket)(i)?;
            Ok((i, t))
            //match expect(i, inx2, Tok::RBracket) {
                //Some(inx3) => (inx3, t),
                //None => (inx2, Node::Error("Mismatching brackets".into()))
            //}
        }
        Some(Tok::LParen) => {
            //let op = &n.unwrap().op().unwrap();
            //
            // consume LParen
            let (i, x) = take_one_any(i)?;

            //let i = i.take(1);
            println!("L1: {:?}", (&i.toks(), &x.toks()));
            let (i, t) = E(i, rbp)?;
            println!("L2: {:?}", (&i.toks(), &t));
            let (i, _) = tag_token(Tok::RParen)(i)?;
            Ok((i, t))
            //match expect(i, inx2, Tok::RParen) {
                //Some(inx3) => (inx3, t),
                //None => (inx, Node::Error("Mismatching parens".into()))
            //}
        }

        Some(Tok::EOF) => {
            println!("got eof3");
            Ok((i, Node::Empty))
        }
        None => {
            println!("got eof4");
            Ok((i, Node::Empty))
        }
        _ => {
            println!("got {:?}", &n);
            unreachable!()
        }
    }
    //println!("p returns : {:?}", (inx, &t));
    //Ok((i, t))
}
use std::io::Write;
use std::io::stdout;// Parse an expression, it will return a P node, and maybe more depending on the precedence


// It will return the initiating node
fn E<'a>(input: Tokens, prec: Prec) -> RNode {
    let p = prec.unwrap();
    let r = 127;
    let (i, t) = P(input)?;
    println!("E {:?}, prec: {:?}, P:{:?}", i.toks(), prec, &t);

    //return Ok((i, t));
    // get a chain of subsequent expressions
    // What follows could be a postfix operator, a binary operator, or a ternary operator
    // We figure this out by looking up the op
    let (i, (r, t)) = G(i, r, t, prec, 0)?;
    Ok((i, t))
    //loop {
        //let (i, (r, t)) = F(i, r, t, prec)?;
    //}
}

//type RRNoode<I, O> = IResult<I, O, VerboseError<I>>;

fn G<'a>(i: Tokens, r: i8, t: Node, prec: Prec, depth: usize) -> PResult<Tokens,(i8, Node)> {
    // peek 
    let n = i.tok[0].tok.clone();
    println!("G: {:?}", (&i.toks(), &r, &prec, &r, &n, depth));
    //stdout().flush();
    if n == Tok::EOF {
        println!("got eof");
        //stdout().flush();
        //let (i, _) = take_one_any(i.clone())?;
        return Ok((i, (r, t)));
        //break;
    }

    //let (i, (r, t)) = F(i, r, t, prec)?;

    let n = i.tok[0].tok.clone();
    if n == Tok::EOF {
        println!("got eof");
        //stdout().flush();
        //let (i, _) = take_one_any(i.clone())?;
        return Ok((i, (r, t)));
        //break;
    }

    // peek
    //let (i, n) = take_one_any(i.clone())?;
    let left = i.tok[0].tok.clone();
    //let (i, (r, t)) = F(i, r, t, prec)?;
    if left == Tok::EOF {
        println!("got eof");
        return Ok((i, (r, t)));
    }

    // get op from left
    let maybe_op = left.op();//left.tok[0].tok.op();
    let (i, op) = if let Some(op) = maybe_op {
        (i, op)
    } else {
        (i, Op::new_default_left())
    };

    println!("left: {:?}", (&left, &op));

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
        let t1 = t.clone();
        let (i, t) = op.LeD(i, t.clone(), left.clone())?;
        println!("LeD: {:?} -> {:?}", (&t1, left), &t);
        //t = t2;
        //i = i2;
        //inx = inx2;
        let r = op.nbp.unwrap();
        G(i, r, t, prec, depth+1)
        //Ok((i, (r, t)))
    } else {
        println!("guard exit: {:?}", i);//&i.slice(inx..));
        Ok((i, (r, t)))
        //break;
    }

    //G(i, r, t, prec)
}

fn F<'a>(i: Tokens, r: i8, t: Node, prec: Prec) -> PResult<Tokens,(i8, Node)> {
    //let maybe_left = i.get(inx);
    // peek
    //let left = i.tok[0].tok.clone();
    
    let (i, n) = take_one_any(i.clone())?;
    let left = i.tok[0].tok.clone();
    
    println!("F: {:?}", (&i.toks(), &r, &prec, &left));
    //stdout().flush();
    if left == Tok::EOF {
        println!("got eof");
        //stdout().flush();
        //let (i, left) = take_one_any(i.clone())?;
        //return Ok((i, (r, t)));
        //break;
    }

    //let (i, left) = take_one_any(i.clone())?;
    //println!("E take: {:?}", (&i, &left));
    //if left.tok[0].tok == Tok::EOF {
        //println!("got eof");
        //break;
        //stdout().flush();
        //let (i, left) = take_one_any(i.clone())?;
        //return Ok((i, (r, t)));
        //return Ok((i, t));
    //}
    //return Ok((i, t));
    //let maybe_left = i.peek();
    //let maybe_op = maybe_left.map(|op| op.tok.op()).flatten();
    //if maybe_left.is_none() {
    //println!("got eof");
    //return Ok((i, t));
    //}

    //let left = maybe_left.unwrap();
    //if left.tok == Tok::EOF {
    //println!("got eof");
    //return Ok((i, t));
    //}

    println!("left {:?}", (&left));//.toks()));
    let maybe_op = left.op();//left.tok[0].tok.op();
    let (i, op) = if let Some(op) = maybe_op {
        (i, op)
    } else {
        //let (i, _) = take_one_any(i)?;
        //let i = i.take(1);
        //inx += 1;
        (i, Op::new_default_left())
            //return Ok((i, t))
    };

    let lbp = op.lbp.unwrap_or(-1);
    println!("guard: {:?}", (prec.unwrap(), lbp, r, &t));

    // lbp must be between r and prec, or we exit
    // if we have lbp greater than or equal to prec, then we parse and include the RHS
    // if lbp is great than r, then we are done
    // r is the previous operations nbp (next binding power)
    // if we set nbp low, we will match less in the next pass
    if prec.unwrap() <= lbp && lbp <= r {
        //inx += 1;
        //let (i, _) = take_one_any(i)?;
        //let i = i.take(1);
        let (i, t) = op.LeD(i, t.clone(), left)?;//left.tok[0].tok.clone())?;
        //t = t2;
        //i = i2;
        //inx = inx2;
        let r = op.nbp.unwrap();
        Ok((i, (r, t)))
    } else {
        println!("guard exit: {:?}", i);//&i.slice(inx..));
        Ok((i, (r, t)))
        //break;
    }
    //Ok((i, t))
}


//fn expect(i: Tokens, inx: usize, tok: Tok) -> Option<usize> {
    //let n = i.get(inx);
    //let i2 = i.take();
    //match n {
        //Some(t) if t == &tok => Some(inx+1),
        //_ => None
    //}
//}

fn parse<'a>(i: Tokens) -> RNode {
    //let mut inx = 0; 
    //loop {
    //let (i, node) = E(i, Some(0))?;
    match E(i, Some(0)) {
        Ok((i, node)) => {
            println!("EXPR {:?}", (&i, &node));
            //let (i, eof) = tag_token(Tok::EOF)(i)?;
            //assert!(i.input_len() == 0);
            //println!("EOF {:?}", (eof));
            Ok((i, node))
        }
        Err(nom::Err::Error(e)) => {
            for (tokens, err) in &e.errors {
                println!("error {:?}", (&err, tokens.toks()));
            }
            //Err(nom::Err::Error(nom::error_position!(i, nom::error::ErrorKind::Tag)))
            Err(nom::Err::Error(e))
        }
        _ => unreachable!(),
    }

    //println!("{:?}", (&node, &i));
    //let (i, eof) = tag_token(Tok::EOF)(i)?;

    //let inx3 = expect(i, inx2, Tok::EOF).unwrap();
    //println!("{:?}", (eof));
    //let (inx2, node) = E(i, inx2, Some(0));

    //println!("{:?}", (inx2, &node));
    //if inx2 >= i.len() {
    //Ok((i, node))
        //}
        //inx = inx2;
        //}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::*;
    //use nom::multi::many1;
    //use crate::sexpr::SExpr;

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
            "x[1]",
            "x(1)",
            "+1",
            "-x ?: y ",
            "-x ?: y : z ",
            "x+1 ; y+2",
            "a=b=c",
            "x+1 = y+2",
            /*
            */
        ];
        r.iter().for_each(|v| {
            let mut lexer = LexerState::from_str_eof(v).unwrap();
            let mut i = lexer.tokens();//.toks();
            println!("v {:?}", (&v));
            println!("tokens: {:?}", (i));

            i.iter_elements().for_each(|t| {
                println!("{:?}", t);
            });
            let (i, node) = parse(i).unwrap();
            println!("NODE {:?}", (&node));//, &i));
            println!("REM {:?}", (&i.toks()));
            //assert_eq!(0, i.input_len());
        });
    }

}

