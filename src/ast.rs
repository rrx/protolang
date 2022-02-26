use crate::tokens::{Tok, Token};

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    Program(Program),
    Expr(Expr),
    Lit(Literal),
}

impl Value {
    pub fn unparse(&mut self) -> Vec<Tok> {
        use Value::*;
        let mut out = vec![];
        out.append(&mut match self {
            Program(prog) => prog.unparse(),
            Expr(expr) => expr.unparse(),
            Lit(lit) => lit.unparse(),
        });
        out
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Node {
    pub pre: Vec<Tok>,
    pub post: Vec<Tok>,
    pub tokens: Vec<Tok>,
    pub value: Value
}

impl Node {
    pub fn unparse(&mut self) -> Vec<Tok> {
        let mut out = vec![];
        out.append(&mut self.pre);
        //out.append(&mut self.tokens);
        out.append(&mut self.value.unparse());
        out.append(&mut self.post);
        out
    }
}


#[derive(PartialEq, Debug, Clone)]
pub struct Program(pub Vec<Node>);

impl Program {
    pub fn unparse(&mut self) -> Vec<Tok> {
        let mut out = vec![];
        for expr in self.0.iter_mut() {
            out.append(&mut expr.unparse());
        }
        out
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    IdentExpr(Ident),
    LitExpr(Literal),
    PrefixExpr(Prefix, Box<Value>),
    InfixExpr(Infix, Box<Value>, Box<Value>)
}
impl Expr {
    pub fn unparse(&mut self) -> Vec<Tok> {
        use Literal::*;
        use Expr::*;
        let mut out = vec![];
        match self {
            IdentExpr(x) => {
                out.append(&mut x.unparse());
            }
            LitExpr(x) => {
                out.append(&mut x.unparse());
            }
            PrefixExpr(prefix, expr) => {
                out.append(&mut prefix.unparse());
                out.append(&mut expr.unparse());
            }
            InfixExpr(op, left, right) => {
                out.append(&mut op.unparse());
                out.append(&mut left.unparse());
                out.append(&mut right.unparse());
            }
            _ => ()
        };
        out
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Prefix {
    PrefixPlus,
    PrefixMinus,
    PrefixNot
}
impl Prefix {
    pub fn unparse(&mut self) -> Vec<Tok> {
        use Prefix::*;
        let token = match self {
            PrefixPlus => Tok::Plus,
            PrefixMinus => Tok::Minus,
            PrefixNot => Tok::Not,
        };
        vec![token]
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Infix {
    Plus,
    Minus,
    Divide,
    Multiply,
    Equal,
    NotEqual,
    GreaterThanEqual,
    LessThanEqual,
    GreaterThan,
    LessThan,
}

impl Infix {
    pub fn unparse(&mut self) -> Vec<Tok> {
        use Prefix::*;
        let token = match self {
            Plus => Tok::Plus,
            Minus => Tok::Minus,
            Multiply => Tok::Mul,
            Divide => Tok::Div,
            _ => Tok::Not,
        };
        vec![token]
    }
}


pub fn infix_op(t: &Tok) -> (Precedence, Option<Infix>) {
    match *t {
        Tok::Equals => (Precedence::PEquals, Some(Infix::Equal)),
        Tok::NotEquals => (Precedence::PEquals, Some(Infix::NotEqual)),
        Tok::LTE => (Precedence::PLessGreater, Some(Infix::LessThanEqual)),
        Tok::GTE => (Precedence::PLessGreater, Some(Infix::GreaterThanEqual)),
        Tok::LT => (Precedence::PLessGreater, Some(Infix::LessThan)),
        Tok::GT => (Precedence::PLessGreater, Some(Infix::GreaterThan)),
        Tok::Plus => (Precedence::PSum, Some(Infix::Plus)),
        Tok::Minus => (Precedence::PSum, Some(Infix::Minus)),
        Tok::Mul => (Precedence::PProduct, Some(Infix::Multiply)),
        Tok::Div => (Precedence::PProduct, Some(Infix::Divide)),
        Tok::LParen => (Precedence::PCall, None),
        Tok::LBracket => (Precedence::PIndex, None),
        _ => (Precedence::PLowest, None),
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    IntLiteral(u64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    StringLiteral(String),
    Invalid(String)
}
impl Literal {
    pub fn unparse(&mut self) -> Vec<Tok> {
        use Literal::*;
        let token = match self {
            IntLiteral(x) => Tok::IntLiteral(*x),
            FloatLiteral(x) => Tok::FloatLiteral(*x),
            BoolLiteral(x) => Tok::BoolLiteral(*x),
            StringLiteral(x) => Tok::StringLiteral(x.clone()),
            Invalid(x) => Tok::Invalid(x.clone()),
        };
        vec![token]
    }
}

#[derive(PartialEq, Debug, Eq, Clone)]
pub struct Ident(pub String);
impl Ident {
    pub fn unparse(&self) -> Vec<Tok> {
        vec![Tok::Ident(self.0.clone())]
    }
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    PLowest,
    PEquals,
    PLessGreater,
    PSum,
    PProduct,
    PCall,
    PIndex,
}
