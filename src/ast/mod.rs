use crate::sexpr::*;
use crate::tokens::{Tok, Token};
use crate::lexer::{Location, Surround};

mod function;
pub use function::{Lambda, Callable, CallableNode, Params};

mod value;
pub use value::{Value};
//use std::fmt;

pub trait Unparse {
    fn unparse(&self) -> Vec<Tok>;
    fn unlex(&self) -> String {
        self.unparse()
            .iter()
            .map(|t| t.unlex())
            .collect::<Vec<_>>()
            .join("")
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Stmt {
    Assign(Ident, ExprNode),
    Block(Vec<StmtNode>),
    Expr(ExprNode),
    Lit(LiteralNode),
    Invalid(String),
}
#[derive(PartialEq, Debug, Clone)]
pub struct StmtNode {
    pub s: Surround,
    pub value: Stmt,
    pub loc: Location,
}
impl StmtNode {
    pub fn new(value: Stmt, loc: Location) -> Self {
        Self {
            s: Surround::default(),
            value,
            loc,
        }
    }
}
impl Unparse for StmtNode {
    fn unparse(&self) -> Vec<Tok> {
        self.s.unparse(match &self.value {
            Stmt::Expr(expr) => expr.unparse(),
            Stmt::Lit(lit) => lit.unparse(),
            Stmt::Assign(ident, expr) => vec![ident.unparse(), vec![Tok::Assign], expr.unparse()]
                .into_iter()
                .flatten()
                .collect(),
            Stmt::Block(stmts) => stmts.iter().map(|s| s.unparse()).flatten().collect(),
            Stmt::Invalid(s) => vec![Tok::Invalid(s.clone())],
        })
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
            Stmt::Invalid(s) => Err(SError::Invalid(s.clone())),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Program {
    pub value: Vec<StmtNode>,
    pub s: Surround,
}
impl Program {
    pub fn new(value: Vec<StmtNode>) -> Self {
        Self {
            s: Surround::default(),
            value,
        }
    }
}

impl Unparse for Program {
    fn unparse(&self) -> Vec<Tok> {
        self.s.unparse(
            self.value
                .clone()
                .into_iter()
                .map(|expr| expr.unparse())
                .flatten()
                .collect(),
        )
    }
    fn unlex(&self) -> String {
        self.unparse()
            .iter()
            .map(|t| t.unlex())
            .collect::<Vec<_>>()
            .join("")
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
    LitExpr(LiteralNode),
    Unary(Unary, Box<ExprNode>),
    Binary(Binary, Box<ExprNode>, Box<ExprNode>),
    Lambda(Lambda),
    List(Vec<ExprNode>),
    Apply(Ident, Vec<ExprNode>),
    Block(Vec<StmtNode>),
}

#[derive(PartialEq, Debug, Clone)]
pub struct ExprNode {
    pub s: Surround,
    pub value: Expr,
    pub loc: Location,
}
impl ExprNode {
    pub fn new(value: Expr, loc: Location) -> Self {
        Self {
            s: Surround::default(),
            value,
            loc,
        }
    }
}

impl From<ExprNode> for StmtNode {
    fn from(item: ExprNode) -> Self {
        let loc = item.loc.clone();
        Self::new(Stmt::Expr(item), loc)
    }
}

impl From<LiteralNode> for StmtNode {
    fn from(item: LiteralNode) -> Self {
        let loc = item.loc.clone();
        Self::new(Stmt::Lit(item), loc)
    }
}

impl From<LiteralNode> for ExprNode {
    fn from(item: LiteralNode) -> Self {
        let loc = item.loc.clone();
        Self::new(Expr::LitExpr(item), loc)
    }
}

impl From<Ident> for ExprNode {
    fn from(item: Ident) -> Self {
        let loc = item.loc.clone();
        Self::new(Expr::IdentExpr(item), loc)
    }
}

impl From<Lambda> for ExprNode {
    fn from(item: Lambda) -> Self {
        let loc = item.loc.clone();
        Self::new(Expr::Lambda(item), loc)
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
            Unary(prefix, expr) => {
                out.append(&mut prefix.unparse());
                out.append(&mut expr.unparse());
            }
            Binary(op, left, right) => {
                out.append(&mut left.unparse());
                out.append(&mut op.unparse());
                out.append(&mut right.unparse());
            }
            List(elements) => {
                for e in elements {
                    out.append(&mut e.unparse());
                }
            }
            Lambda(e) => {
                out.append(&mut e.unparse());
            }
            Apply(ident, args) => {
                out.append(&mut ident.unparse());
                for arg in args {
                    out.append(&mut arg.unparse());
                }
            }
            Block(stmts) => {
                out.append(&mut stmts.into_iter().map(|s| s.unparse()).flatten().collect());
            }
        };
        self.s.unparse(out)
    }
}
impl SExpr for ExprNode {
    fn sexpr(&self) -> SResult<S> {
        use Expr::*;
        match &self.value {
            LitExpr(x) => x.sexpr(),
            IdentExpr(x) => x.sexpr(),
            Binary(op, left, right) => {
                let sleft = left.sexpr()?;
                let sright = right.sexpr()?;
                Ok(S::Cons(op.unlex(), vec![sleft, sright]))
            }
            Unary(prefix, expr) => {
                let s = expr.sexpr()?;
                Ok(S::Cons(prefix.unlex(), vec![s]))
            }
            List(elements) => {
                let s_args = elements
                    .iter()
                    .filter_map(|a| a.sexpr().ok())
                    .collect::<Vec<_>>();
                Ok(S::Cons("list".into(), s_args))
            }
            Lambda(e) => e.sexpr(),
            Apply(ident, args) => {
                let s_args = args
                    .iter()
                    .filter_map(|a| a.sexpr().ok())
                    .collect::<Vec<_>>();
                Ok(S::Cons(ident.value.clone(), s_args))
            }
            Block(stmts) => Ok(S::Cons(
                "block".into(),
                stmts.into_iter().filter_map(|s| s.sexpr().ok()).collect(),
            )),
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
pub struct Unary {
    pub s: Surround,
    pub value: Prefix,
    pub loc: Location,
}

impl Unary {
    pub fn from_token(token: Token) -> Option<Self> {
        let maybe_prefix = match token.tok {
            Tok::Plus => Some(Prefix::PrefixPlus),
            Tok::Minus => Some(Prefix::PrefixMinus),
            Tok::Not => Some(Prefix::PrefixNot),
            _ => None,
        };
        match maybe_prefix {
            Some(prefix) => {
                let loc = token.to_location();
                Some(Self {
                    //s: Surround {
                    //pre: token.pre.iter().map(|t| t.toks()).flatten().collect(),
                    //post: token.post.iter().map(|t| t.toks()).flatten().collect(),
                    //},
                    s: token.s,
                    value: prefix,
                    loc,
                })
            }
            None => None,
        }
    }

    pub fn from_tokens(prefix: Prefix, pre: Vec<Tok>, post: Vec<Tok>) -> Self {
        Self {
            s: Surround::new(pre, post),
            value: prefix,
            loc: Location::default(),
        }
    }

    pub fn new(prefix: Prefix) -> Self {
        Self {
            s: Surround::default(),
            value: prefix,
            loc: Location::default(),
        }
    }

    pub fn token(&self) -> Tok {
        match self.value {
            Prefix::PrefixPlus => Tok::Plus,
            Prefix::PrefixMinus => Tok::Minus,
            Prefix::PrefixNot => Tok::Not,
        }
    }
}

impl Unparse for Unary {
    fn unparse(&self) -> Vec<Tok> {
        self.s.unparse(vec![self.token()])
    }

    fn unlex(&self) -> String {
        self.token().unlex()
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
    //Map,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Binary {
    pub s: Surround,
    pub value: Infix,
    pub precedence: Precedence,
    pub loc: Location,
}

impl Binary {
    pub fn new(infix: Infix, precedence: Precedence, loc: Location) -> Self {
        Self {
            s: Surround::default(),
            value: infix,
            precedence,
            loc,
        }
    }

    pub fn from_token(token: Token) -> Option<Self> {
        let (precedence, maybe_prefix) = infix_op(&token.tok);
        match maybe_prefix {
            Some(prefix) => {
                let loc = token.to_location();
                Some(Self {
                    //s: Surround {
                    //pre: token.pre.iter().map(|t| t.toks()).flatten().collect(),
                    //post: token.post.iter().map(|t| t.toks()).flatten().collect(),
                    //},
                    s: token.s,
                    value: prefix,
                    precedence,
                    loc,
                })
            }
            None => None,
        }
    }

    pub fn from_tokens(
        infix: Infix,
        precedence: Precedence,
        pre: Vec<Tok>,
        post: Vec<Tok>,
    ) -> Self {
        Self {
            s: Surround::new(pre, post),
            value: infix,
            precedence,
            loc: Location::default(),
        }
    }

    pub fn token(&self) -> Tok {
        match self.value {
            Infix::Plus => Tok::Plus,
            Infix::Minus => Tok::Minus,
            Infix::Multiply => Tok::Mul,
            Infix::Divide => Tok::Div,
            Infix::Exp => Tok::Caret,
            Infix::Equal => Tok::Equals,
            Infix::NotEqual => Tok::NotEquals,
            Infix::LessThanEqual => Tok::LTE,
            Infix::GreaterThanEqual => Tok::GTE,
            Infix::LessThan => Tok::LT,
            Infix::GreaterThan => Tok::GT,
            //Infix::Map => Tok::LeftArrow,
        }
    }
}

impl Unparse for Binary {
    fn unparse(&self) -> Vec<Tok> {
        self.s.unparse(vec![self.token()])
    }

    fn unlex(&self) -> String {
        self.token().unlex()
    }
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    PLowest,
    //PMap,
    PEquals,
    PLessGreater,
    PSum,
    PProduct,
    PExp,
    PCall,
    PIndex,
}

pub fn infix_precedence(op: Infix) -> Precedence {
    match op {
        Infix::Equal => Precedence::PEquals,
        Infix::NotEqual => Precedence::PEquals,
        Infix::LessThanEqual => Precedence::PLessGreater,
        Infix::GreaterThanEqual => Precedence::PLessGreater,
        Infix::LessThan => Precedence::PLessGreater,
        Infix::GreaterThan => Precedence::PLessGreater,
        Infix::Plus => Precedence::PSum,
        Infix::Minus => Precedence::PSum,
        Infix::Multiply => Precedence::PProduct,
        Infix::Divide => Precedence::PProduct,
        Infix::Exp => Precedence::PExp,
        //Infix::Map => Precedence::PMap,
    }
}

pub fn infix_op(t: &Tok) -> (Precedence, Option<Infix>) {
    match *t {
        Tok::Equals => (Precedence::PEquals, Some(Infix::Equal)),
        Tok::NotEquals => (Precedence::PEquals, Some(Infix::NotEqual)),
        //Tok::LeftArrow => (Precedence::PMap, Some(Infix::Map)),
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
pub struct LiteralNode {
    pub value: Value,
    pub s: Surround,
    pub loc: Location,
}
impl LiteralNode {
    pub fn new(value: Value, loc: Location) -> Self {
        Self {
            value,
            s: Surround::default(),
            loc,
        }
    }
}
impl Unparse for LiteralNode {
    fn unparse(&self) -> Vec<Tok> {
        self.s.unparse(self.value.unparse())//vec![self.value.token()])
    }
    //fn unlex(&self) -> String {
    //self.value.token().unlex()
    //}
}

impl SExpr for LiteralNode {
    fn sexpr(&self) -> SResult<S> {
        self.value.sexpr()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Ident {
    pub value: String,
    pub s: Surround,
    pub loc: Location,
}
impl Ident {
    pub fn from_token(token: Token) -> Option<Self> {
        let maybe_ident = match &token.tok {
            Tok::Ident(s) => Some(s),
            _ => None,
        };
        match maybe_ident {
            Some(ident) => Some(Self {
                //s: Surround {
                //pre: token.pre.iter().map(|t| t.toks()).flatten().collect(),
                //post: token.post.iter().map(|t| t.toks()).flatten().collect(),
                //},
                s: token.s.clone(),
                value: ident.clone(),
                loc: token.to_location(),
            }),
            None => None,
        }
    }

    pub fn tok(&self) -> Tok {
        Tok::Ident(self.value.clone())
    }
    //pub fn token(&self) -> Token {
    //Token { pre: self.s.pre, post: self.s.post, tok: self.tok() }
    //}
}

impl Unparse for Ident {
    fn unparse(&self) -> Vec<Tok> {
        self.s.unparse(vec![Tok::Ident(self.value.clone())])
    }
    //fn unlex(&self) -> String {
    //self.unparse().iter().map(|t| t.unlex()).collect::<Vec<_>>().join("")
    //}
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
        let p = Unary::new(Prefix::PrefixMinus);
        assert_eq!(p.unlex(), "-");
    }

    #[test]
    fn infix() {
        let p = Binary::new(Infix::Minus, Precedence::PLowest, Location::default());
        //println!("{:?}", (&p, &p.token(), &p.unlex()));
        assert_eq!(p.unlex(), "-");
    }
}
