use crate::ast::*;
use crate::tokens::{Tok,Tokens};

type Prec = i8;

#[derive(Clone, Debug)]
struct Op {
    op: Operator,
    isBinary: bool,
    prec: Prec,
    rightPrec: Prec,
    nextPrec: Prec
}

impl Op {
    pub fn from(op: Operator) -> Self {
        match op {
            Operator::Assign => Self { op, prec: 10, rightPrec: 20, nextPrec: 9, isBinary: true },
            Operator::Equal => Self { op, prec: 10, rightPrec: 20, nextPrec: 9, isBinary: true },
            Operator::NotEqual => Self { op, prec: 10, rightPrec: 20, nextPrec: 9, isBinary: true },
            Operator::LessThanEqual => Self { op, prec: 10, rightPrec: 20, nextPrec: 9, isBinary: true },
            Operator::LessThan => Self { op, prec: 10, rightPrec: 20, nextPrec: 9, isBinary: true },
            Operator::GreaterThanEqual => Self { op, prec: 10, rightPrec: 20, nextPrec: 9, isBinary: true },
            Operator::GreaterThan => Self { op, prec: 10, rightPrec: 20, nextPrec: 9, isBinary: true },
            Operator::Plus | Operator::Minus => Self { op, prec: 20, rightPrec: 21, nextPrec: 20, isBinary: true },
            Operator::Multiply | Operator::Divide => Self { op, prec: 30, rightPrec: 31, nextPrec: 30, isBinary: true },
            Operator::Bang => Self { op, prec: 40, rightPrec: 0, nextPrec: 40, isBinary: false },
            Operator::Modulus => Self { op, prec: 40, rightPrec: 0, nextPrec: 40, isBinary: true },
            Operator::Exp => Self { op, prec: 50, rightPrec: 50, nextPrec: 49, isBinary: true },
        }
    }

    pub fn nextPrec(&self) -> Prec {
        self.rightPrec - 1
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
            _ => None
        }
    }

    fn isBinary(&self) -> bool {
        if let Some(op) = self.op() {
            op.isBinary
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

    fn nextPrec(&self) -> Prec {
        match self.op() {
            Some(op) => op.nextPrec,
            None => -1
        }
    }
    fn rightPrec(&self) -> Prec {
        match self.op() {
            Some(op) => op.rightPrec,
            None => -1
        }
    }
    fn prec(&self) -> Prec {
        match self.op() {
            Some(op) => op.prec,
            None => -1
        }
    }
}

#[derive(Clone, Debug)]
enum Node {
    Empty,
    Prefix(Operator, Box<Node>),
    Binary(Operator, Box<Node>, Box<Node>),
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

fn E<'a>(i: &[Tok], inx: usize, p: Prec) -> (usize, Node) {
    let (mut inx, mut t) = P(i, inx);
    let mut r = 127;
    println!("E {:?}", (&inx, &t));
    loop {
        let maybe_left = i.get(inx);
        match maybe_left {
            None | Some(Tok::EOF) => {
                return (inx, t);
            }
            Some(left) => {
                println!("left {:?}", (&left));
                if p <= left.prec() && left.prec() <= r {
                    inx += 1;
                    if left.isBinary() {
                        let (inx2, y) = E(&i, inx, left.rightPrec());
                        inx = inx2;
                        t = Node::Binary(left.op().unwrap().op, Box::new(t), Box::new(y));
                    } else {
                        t = Node::Postfix(left.op().unwrap().op, Box::new(t));
                        r = left.nextPrec();
                    }
                } else {
                    break;
                }
            }
        }

    }
    (inx, t)
}

fn expect(i: &[Tok], inx: usize, t: Tok) -> Option<usize> {
    let n = i.get(inx);
    match n {
        Some(t) => Some(inx+1),
        _ => None
    }
}

fn P<'a>(i: &'a [Tok], inx: usize) -> (usize, Node) {
    let n = i.get(inx);
    //inx += 1;
    println!("P {:?}", &n);

    match &n {
        Some(Tok::Minus) => {
            let (inx2, t) = E(i, inx+1, 30);
            (inx2, Node::Prefix(Tok::Minus.op().unwrap().op, Box::new(t)))
        }
        Some(Tok::Ident(s)) => {
            let t = Node::Var(s.clone());
            (inx+1, t)
        }
        Some(Tok::IntLiteral(n)) => {
            let t = Node::Var(n.to_string());
            (inx+1, t)
        }
        Some(Tok::LParen) => {
            let (inx2, t) = E(i, inx+1, 0);
            match expect(i, inx2, Tok::RParen) {
                Some(inx) => (inx, t),
                None => (inx, Node::Error("Mismatching parens".into()))
            }
        }

        Some(Tok::EOF) => {
            (inx, Node::Empty)
        }
        None => (inx, Node::Empty),
        _ => {
            unreachable!()
        }
    }
}

fn parse<'a>(i: &'a mut [Tok]) -> Node {
    i.iter().for_each(|t| {
        println!("{:?}", t);
    });

    let (inx, node) = E(i, 0, 0);
    node
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
        ];
        r.iter().for_each(|v| {
            let mut lexer = LexerState::from_str_eof(v).unwrap();
            let mut tokens = lexer.tokens().toks();
            println!("v {:?}", (&v));
            println!("tokens: {:?}", (&tokens));

            let node = parse(&mut tokens);
            println!("node {:?}", (&node));
        });
    }

}

