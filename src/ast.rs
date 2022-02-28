use crate::sexpr::*;
use crate::tokens::{Token, Tok};

pub trait Unparse {
    fn unparse(&self) -> Vec<Tok>;
    fn unlex(&self) -> String {
        self.unparse().iter().map(|t| t.unlex()).collect::<Vec<_>>().join("")
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    Program(Program),
    Stmt(StmtNode),
    Expr(ExprNode),
    Lit(Literal),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Stmt {
    Assign(Ident, ExprNode),
    Block(Vec<StmtNode>),
    Expr(ExprNode),
    Lit(Literal),
}
#[derive(PartialEq, Debug, Clone)]
pub struct StmtNode {
    pre: Vec<Tok>,
    post: Vec<Tok>,
    value: Stmt
}
impl StmtNode {
    pub fn new(value: Stmt) -> Self {
        Self { pre: vec![], post: vec![], value }
    }
}
impl Unparse for StmtNode {
    fn unparse(&self) -> Vec<Tok> {
        match &self.value {
            Stmt::Expr(expr) => expr.unparse(),
            Stmt::Lit(lit) => lit.unparse(),
            Stmt::Assign(ident, expr) => vec![ident.unparse(), vec![Tok::Assign], expr.unparse()]
                .into_iter()
                .flatten()
                .collect(),
            Stmt::Block(stmts) => stmts.iter().map(|s| s.unparse()).flatten().collect(),
        }
    }
}

impl SExpr for StmtNode {
    fn sexpr(&self) -> SResult<S> {
        match &self.value {
            Stmt::Lit(x) => x.sexpr(),
            Stmt::Expr(x) => x.sexpr(),
            Stmt::Assign(ident, expr) => {
                let sident = ident.sexpr()?;
                let sexpr = expr.sexpr()?;
                Ok(S::Cons("def".into(), vec![sident, sexpr]))
            }
            Stmt::Block(stmts) => Ok(S::Cons(
                "block".into(),
                stmts.into_iter().filter_map(|s| s.sexpr().ok()).collect(),
            )),
        }
    }
}

impl Unparse for Value {
    fn unparse(&self) -> Vec<Tok> {
        match self {
            Value::Program(prog) => prog.unparse(),
            Value::Expr(expr) => expr.unparse(),
            Value::Lit(lit) => lit.unparse(),
            Value::Stmt(stmt) => stmt.unparse(),
        }
    }
}

impl SExpr for Value {
    fn sexpr(&self) -> SResult<S> {
        use Value::*;
        match self {
            Expr(expr) => expr.sexpr(),
            Lit(lit) => lit.sexpr(),
            _ => Err(SError::Invalid),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Node {
    pub pre: Vec<Tok>,
    pub post: Vec<Tok>,
    //pub tokens: Vec<Tok>,
    pub value: Value,
}

impl Node {
    pub fn prepend(&mut self, toks: Vec<Tok>) {
        if toks.len() > 0 {
            let mut v = toks;
            v.append(&mut self.pre);
            self.pre = v;
        }
    }

    pub fn append(&mut self, toks: &mut Vec<Tok>) {
        self.post.append(toks);
    }
}

impl Unparse for Node {
    fn unparse(&self) -> Vec<Tok> {
        vec![self.pre.clone(), self.value.unparse(), self.post.clone()]
            .into_iter()
            .flatten()
            .collect()
    }
}

impl SExpr for Node {
    fn sexpr(&self) -> SResult<S> {
        self.value.sexpr()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Program {
    value: Vec<Node>,
    pre: Vec<Tok>,
    post: Vec<Tok>
}
impl Program {
    pub fn new(value: Vec<Node>, pre: Vec<Tok>, post: Vec<Tok>) -> Self {
        Self { pre, post, value }
    }
}

impl Unparse for Program {
    fn unparse(&self) -> Vec<Tok> {
        vec![
            self.pre.clone(),
            self.value.clone().into_iter().map(|expr| expr.unparse()).flatten().collect(),
            self.post.clone()
        ].into_iter()
            .flatten()
            .collect()
    }
}

impl SExpr for Program {
    fn sexpr(&self) -> SResult<S> {
        let results = self.value.iter().filter_map(|v| v.sexpr().ok()).collect();
        Ok(S::Cons("program".into(), results))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    IdentExpr(Ident),
    LitExpr(Literal),
    PrefixExpr(PrefixNode, Box<Node>),
    InfixExpr(InfixNode, Box<Value>, Box<Value>),
}

#[derive(PartialEq, Debug, Clone)]
pub struct ExprNode {
    pre: Vec<Tok>,
    post: Vec<Tok>,
    value: Expr
}
impl ExprNode {
    pub fn new(value: Expr) -> Self {
        Self { pre: vec![], post: vec![], value }
    }
}

impl Unparse for ExprNode {
    fn unparse(&self) -> Vec<Tok> {
        use Expr::*;
        let mut out = vec![];
        match &self.value {
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
                out.append(&mut left.unparse());
                out.append(&mut op.unparse());
                out.append(&mut right.unparse());
            }
        };
        out
    }
}
impl SExpr for ExprNode {
    fn sexpr(&self) -> SResult<S> {
        use Expr::*;
        match &self.value {
            LitExpr(x) => x.sexpr(),
            IdentExpr(x) => x.sexpr(),
            InfixExpr(op, left, right) => {
                let sleft = left.sexpr()?;
                let sright = right.sexpr()?;
                Ok(S::Cons(op.unlex(), vec![sleft, sright]))
            }
            PrefixExpr(prefix, expr) => {
                let s = expr.sexpr()?;
                Ok(S::Cons(prefix.unlex(), vec![s]))
            }
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Prefix {
    PrefixPlus,
    PrefixMinus,
    PrefixNot,
}

#[derive(PartialEq, Debug, Clone)]
pub struct PrefixNode {
    pre: Vec<Tok>,
    post: Vec<Tok>,
    value: Prefix
}

impl PrefixNode {
    pub fn from_token(token: Token) -> Option<Self> {
        let maybe_prefix = match token.tok {
            Tok::Plus => Some(Prefix::PrefixPlus),
            Tok::Minus => Some(Prefix::PrefixMinus),
            Tok::Not => Some(Prefix::PrefixNot),
            _ => None
        };
        match maybe_prefix {
            Some(prefix) => Some(Self {
                pre: token.pre.iter().map(|t| t.toks()).flatten().collect(),
                post: token.post.iter().map(|t| t.toks()).flatten().collect(),
                value: prefix }),
            None => None
        }
    }

    pub fn from_tokens(prefix: Prefix, pre: Vec<Tok>, post: Vec<Tok>) -> Self {
        Self { pre, post, value: prefix }
    }

    pub fn new(prefix: Prefix) -> Self {
        Self { pre: vec![], post: vec![], value: prefix }
    }

    pub fn token(&self) -> Tok {
        match self.value {
            Prefix::PrefixPlus => Tok::Plus,
            Prefix::PrefixMinus => Tok::Minus,
            Prefix::PrefixNot => Tok::Not,
        }
    }
}

impl Unparse for PrefixNode {
    fn unparse(&self) -> Vec<Tok> {
        vec![self.pre.clone(), vec![self.token()], self.post.clone()].into_iter().flatten().collect()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Infix {
    Plus,
    Minus,
    Divide,
    Multiply,
    Exp,
    Equal,
    NotEqual,
    GreaterThanEqual,
    LessThanEqual,
    GreaterThan,
    LessThan,
}

#[derive(PartialEq, Debug, Clone)]
pub struct InfixNode {
    pre: Vec<Tok>,
    post: Vec<Tok>,
    value: Infix,
    pub precedence: Precedence
}

impl InfixNode {
    pub fn new(infix: Infix, precedence: Precedence) -> Self {
        Self { pre: vec![], post: vec![], value: infix, precedence }
    }

    pub fn from_token(token: Token) -> Option<Self> {
        let (precedence, maybe_prefix) = infix_op(&token.tok);
        match maybe_prefix {
            Some(prefix) => Some(Self {
                pre: token.pre.iter().map(|t| t.toks()).flatten().collect(),
                post: token.post.iter().map(|t| t.toks()).flatten().collect(),
                value: prefix,
                precedence
            }),
            None => None
        }
    }

    pub fn from_tokens(infix: Infix, precedence: Precedence, pre: Vec<Tok>, post: Vec<Tok>) -> Self {
        Self { pre, post, value: infix, precedence }
    }

    pub fn unlex(&self) -> String {
        match self.token() {
            Some(token) => token.unlex(),
            None => "".into()
        }
    }

    pub fn token(&self) -> Option<Tok> {
        match self.value {
            Infix::Plus => Some(Tok::Plus),
            Infix::Minus => Some(Tok::Minus),
            Infix::Multiply => Some(Tok::Mul),
            Infix::Divide => Some(Tok::Div),
            Infix::Exp => Some(Tok::Caret),
            _ => None
        }
    }
}

impl Unparse for InfixNode {
    fn unparse(&self) -> Vec<Tok> {
        vec![self.pre.clone(), vec![self.token().unwrap()], self.post.clone()].into_iter().flatten().collect()
    }
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    PLowest,
    PEquals,
    PLessGreater,
    PSum,
    PProduct,
    PExp,
    PCall,
    PIndex,
}

pub fn infix_precedence(op: Infix) -> Precedence {
    use Infix::*;
    match op {
        Equal => Precedence::PEquals,
        NotEqual => Precedence::PEquals,
        LessThanEqual => Precedence::PLessGreater,
        GTE => Precedence::PLessGreater,
        LessThan => Precedence::PLessGreater,
        GreaterThan => Precedence::PLessGreater,
        Plus => Precedence::PSum,
        Minus => Precedence::PSum,
        Multiply => Precedence::PProduct,
        Divide => Precedence::PProduct,
        Exp => Precedence::PExp,
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
        Tok::Caret => (Precedence::PExp, Some(Infix::Exp)),
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
    Invalid(String),
}
impl Unparse for Literal {
    fn unparse(&self) -> Vec<Tok> {
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

impl SExpr for Literal {
    fn sexpr(&self) -> SResult<S> {
        use Literal::*;
        match self {
            IntLiteral(x) => Ok(S::Atom(x.to_string())),
            FloatLiteral(x) => Ok(S::Atom(x.to_string())),
            BoolLiteral(x) => Ok(S::Atom(x.to_string())),
            StringLiteral(x) => Ok(S::Atom(x.to_string())),
            _ => Err(SError::Invalid),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Ident {
    value: String,
    pre: Vec<Tok>,
    post: Vec<Tok>
}
impl Ident {
    pub fn from_token(token: Token) -> Option<Self> {
        let maybe_ident = match token.tok {
            Tok::Ident(s) => Some(s),
            _ => None
        };
        match maybe_ident {
            Some(ident) => Some(Self {
                pre: token.pre.iter().map(|t| t.toks()).flatten().collect(),
                post: token.post.iter().map(|t| t.toks()).flatten().collect(),
                value: ident
            }),
            None => None
        }
    }
}

impl Unparse for Ident {
    fn unparse(&self) -> Vec<Tok> {
        vec![Tok::Ident(self.value.clone())]
    }
}

impl SExpr for Ident {
    fn sexpr(&self) -> SResult<S> {
        Ok(S::Atom(self.value.clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prefix() {
        let p = PrefixNode::new(Prefix::PrefixMinus);
        assert_eq!(p.unlex(), "-");
    }

    #[test]
    fn infix() {
        let p = InfixNode::new(Infix::Minus, Precedence::PLowest);
        println!("{:?}", (&p, &p.token(), &p.unlex()));
        assert_eq!(p.unlex(), "-");
    }
}
