use crate::ast::*;
use crate::tokens::{Tok,Tokens};

type Prec = Option<i8>;

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
        }
    }

    fn elvis_LeD(&self, i: &[Tok], inx: usize, x: Node, token: Tok) -> (usize, Node) {
        //let (mut inx2, mut t) = self.chain_LeD(i, inx, x, token);
        let (inx2, y) = E(&i, inx, Some(0));//self.rbp.unwrap()+1));

        let n = i.get(inx2).unwrap();
        println!("check ternary: {:?}", (inx, inx2, &y, &n, &token, &i[inx2..]));
        if n == &Tok::Colon {
            //if let Node::Binary(op, x, y) = t {
                let (inx3, z) = E(&i, inx2+1, Some(0));//self.rbp.unwrap()+1));
                //inx2 = inx3;
                (inx3, Node::Ternary(Operator::Elvis, Box::new(x), Box::new(y), Box::new(z.clone())))
            //} else {
                //unreachable!()
            //}
        } else {
            (inx2, Node::Binary(Operator::Elvis, Box::new(x), Box::new(y)))
        }
        //(inx2, t)
    }

    fn chain_LeD(&self, i: &[Tok], mut inx: usize, x: Node, token: Tok) -> (usize, Node) {
        let (inx2, y) = E(&i, inx, Some(self.rbp.unwrap()+1));
        inx = inx2;
        let mut t = Node::Binary(self.op.clone(), Box::new(x), Box::new(y.clone()));

        let n = i.get(inx2).unwrap();
        println!("chain: {:?}", (inx, inx2, &t, &n));
        if n == &Tok::EOF {
            println!("got eof2");
            return (inx, t);
        }

        let nextOp = n.op().unwrap(); 
        if self.lbp == nextOp.lbp {
            inx += 1; //consume
            let (inx3, t1) = self.chain_LeD(i, inx, y, n.clone());
            inx = inx3;
            t = Node::Binary(Operator::Modulus, Box::new(t), Box::new(t1));
        }
        (inx, t)
    }

    fn LeD(&self, i: &[Tok], mut inx: usize, x: Node, token: Tok) -> (usize, Node) {
        match token {
            // chaining
            Tok::Elvis =>{
                let (inx2, y) = self.elvis_LeD(i, inx, x.clone(), token.clone());
                inx = inx2;
                (inx, y)
            }
            Tok::Assign => {
                let (inx2, y) = self.chain_LeD(i, inx, x.clone(), token.clone());
                inx = inx2;
                (inx, y)
            }
            _ => {
                if token.isBinary() {
                    // binary parses the RHS, and returns a binary node
                    let (inx2, y) = E(&i, inx, self.rbp);
                    inx = inx2;
                    (inx, Node::Binary(self.op.clone(), Box::new(x), Box::new(y)))
                } else {
                    // postfix just returns, there's no RHS
                    (inx, Node::Postfix(self.op.clone(), Box::new(x)))
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

struct Reader<'a> {
    tokens: &'a [Tok],
    pos: usize,
}
impl<'a> Reader<'a> {
    fn new(tokens: &'a [Tok]) -> Self {
        Self { tokens, pos: 0 }
    }

    fn next(&'a self) -> Option<&'a Tok> {
        self.tokens.get(self.pos)
    }

    fn consume(&'a mut self) -> Option<&'a Tok> {
        if self.tokens.len() > 0 {
            let n = self.next();
            //self.pos += 1;
            n
        } else {
            None
        }
    }
}

// Parse a prefix token, and return a full node
// Could be -(...), or (...), or a variable
fn P<'a>(i: &[Tok], inx: usize) -> (usize, Node) {
    let n = i.get(inx);
    println!("P {:?}", (&n, inx));

    // P branch
    let (mut inx, mut t) = match &n {
        Some(Tok::Minus) | Some(Tok::Plus) => {
            let op = &n.unwrap().op().unwrap();
            let (inx2, t) = E(i, inx+1, op.rbp);
            (inx2, Node::Prefix(op.op.clone(), Box::new(t)))
        }
        Some(Tok::Ident(s)) => {
            let t = Node::Var(s.clone());
            (inx+1, t)
        }
        Some(Tok::IntLiteral(n)) => {
            let t = Node::Var(n.to_string());
            (inx+1, t)
        }
        Some(Tok::LBracket) => {
            let op = &n.unwrap().op().unwrap();
            let (inx2, t) = E(i, inx+1, Some(0));// op.rbp);
            println!("exp: {:?}", (inx2, &t, &i[inx2..]));
            match expect(i, inx2, Tok::RBracket) {
                Some(inx3) => (inx3, t),
                None => (inx2, Node::Error("Mismatching brackets".into()))
            }
        }
        Some(Tok::LParen) => {
            let op = &n.unwrap().op().unwrap();
            let (inx2, t) = E(i, inx+1, op.rbp);
            match expect(i, inx2, Tok::RParen) {
                Some(inx3) => (inx3, t),
                None => (inx, Node::Error("Mismatching parens".into()))
            }
        }

        Some(Tok::EOF) => {
            println!("got eof3");
            (inx, Node::Empty)
        }
        None => {
            println!("got eof4");
            (inx, Node::Empty)
        }
        _ => {
            unreachable!()
        }
    };
    println!("p returns : {:?}", (inx, &t));
    (inx, t)
}

// Parse an expression, it will return a P node, and maybe more depending on the precedence
// It will return the initiating node
fn E<'a>(i: &[Tok], inx: usize, p: Prec) -> (usize, Node) {
    let mut r = 127;
    let (mut inx, mut t) = P(i, inx);
    println!("E {:?}, P:{:?}", &inx, &t);

    loop {
        let maybe_left = i.get(inx);
        match maybe_left {
            None | Some(Tok::EOF) => {
                println!("got eof");
                break;
            }
            Some(left) => {
                println!("left {:?}", (&left));
                let maybe_op = left.op();
                let op = if let Some(op) = maybe_op {
                    op
                } else {
                    inx += 1;
                    Op::new_default_left()
                };

                let lbp = left.lbp().unwrap_or(-1);
                // break if we have a token that does not have lbp
                println!("guard: {:?}", (p.unwrap(), lbp, r));
                if p.unwrap() <= lbp && lbp <= r {
                    inx += 1;
                    let op = left.op().unwrap();
                    let (inx2, y) = op.LeD(i, inx, t.clone(), left.clone());
                    t = y;
                    inx = inx2;
                    r = left.nbp().unwrap();
                } else {
                    println!("guard exit: {:?}", &i[inx..]);
                    break;
                }
            }
        }

    }
    (inx, t)
}


fn expect(i: &[Tok], inx: usize, tok: Tok) -> Option<usize> {
    let n = i.get(inx);
    match n {
        Some(t) if t == &tok => Some(inx+1),
        _ => None
    }
}

fn parse<'a>(i: &'a mut [Tok]) -> (usize, Node) {
    i.iter().for_each(|t| {
        println!("{:?}", t);
    });
    let mut inx = 0; 
    //loop {
        let (inx2, node) = E(i, inx, Some(0));

        println!("{:?}", (inx2, &node, &i[inx2..]));
        let inx3 = expect(i, inx2, Tok::EOF).unwrap();
        println!("{:?}", (inx3));
        //let (inx2, node) = E(i, inx2, Some(0));

        //println!("{:?}", (inx2, &node));
        //if inx2 >= i.len() {
            return (inx3, node);
        //}
        //inx = inx2;
    //}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::*;
    use nom::multi::many1;
    use crate::sexpr::SExpr;

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
            "a=b=c",
            "a!^b",
            "a^b!^c",
            "x[1]",
            "x(1)",
            "+1",
            "-x ?: y ",
            "-x ?: y : z ",
        ];
        r.iter().for_each(|v| {
            let mut lexer = LexerState::from_str_eof(v).unwrap();
            let mut tokens = lexer.tokens().toks();
            println!("v {:?}", (&v));
            println!("tokens: {:?}", (&tokens));

            let (inx, node) = parse(&mut tokens);
            println!("node {:?}", (&node, &tokens[inx..]));
            assert_eq!(inx, tokens.len());
        });
    }

}

