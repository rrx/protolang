use crate::sexpr::*;
use crate::tokens::{Tok, Token};

#[derive(PartialEq, Debug, Clone)]
pub struct Surround {
    pre: Vec<Tok>,
    post: Vec<Tok>,
}
impl Default for Surround {
    fn default() -> Self {
        Self {
            pre: vec![],
            post: vec![],
        }
    }
}

impl Surround {
    pub fn new(pre: Vec<Tok>, post: Vec<Tok>) -> Self {
        Self { pre, post }
    }

    pub fn prepend(&mut self, toks: Vec<Tok>) {
        if toks.len() > 0 {
            let mut v = toks;
            v.append(&mut self.pre);
            self.pre = v;
        }
    }

    pub fn append(&mut self, toks: Vec<Tok>) {
        self.post.append(&mut toks.clone());
    }

    pub fn unparse(&self, tokens: Vec<Tok>) -> Vec<Tok> {
        vec![self.pre.clone(), tokens, self.post.clone()]
            .into_iter()
            .flatten()
            .collect()
    }
}

pub trait Unparse {
    fn unparse(&self) -> Vec<Tok>;
    fn unlex(&self) -> String {
        self.unparse()
            .iter()
            .map(|t| t.unlex())
            .collect::<Vec<_>>()
            .join("")
    }
    fn to_string(&self) -> String;
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
}
impl StmtNode {
    pub fn new(value: Stmt) -> Self {
        Self {
            s: Surround::default(),
            value,
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
    fn to_string(&self) -> String {
        match &self.value {
            Stmt::Expr(expr) => expr.to_string(),
            Stmt::Lit(lit) => lit.to_string(),
            Stmt::Assign(ident, expr) => {
                vec![ident.to_string(), (Tok::Assign).unlex(), expr.to_string()].join("")
            }
            Stmt::Block(stmts) => stmts
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
                .join(""),
            Stmt::Invalid(s) => s.clone(),
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
    fn to_string(&self) -> String {
        self.unparse().iter()
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
pub struct Params {
    pub s: Surround,
    pub value: Vec<Ident>,
}
impl Params {
    pub fn new(value: Vec<Ident>) -> Self {
        Self {
            s: Surround::default(),
            value,
        }
    }
}
impl Unparse for Params {
    fn unparse(&self) -> Vec<Tok> {
        self.s.unparse(self.value.iter().map(|s| s.unparse()).flatten().collect::<Vec<_>>())
    }

    fn to_string(&self) -> String {
        self.unparse().iter().map(|t| t.to_string()).collect::<Vec<_>>().join("")
    }
}
impl SExpr for Params {
    fn sexpr(&self) -> SResult<S> {
        let params = self.value.iter().map_while(|v| v.sexpr().ok()).collect::<Vec<_>>();
        if params.len() < self.value.len() {
            return Err(SError::Invalid("Unable to parse parameters".into()));
        }
        Ok(S::Cons("params".into(), params))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Lambda {
    pub s: Surround,
    pub params: Params,
    pub expr: Box<ExprNode>,
    pub loc: Location,
}
impl Lambda {
    pub fn new(params: Params, expr: ExprNode, loc: Location) -> Self {
        Self {
            s: Surround::default(),
            params, expr: Box::new(expr),
            loc
        }
    }
}
impl Unparse for Lambda  {
    fn unparse(&self) -> Vec<Tok> {
        vec![vec![Tok::Backslash], self.params.unparse(), vec![Tok::LeftArrow], self.expr.unparse()].into_iter().flatten().collect()
    }

    fn to_string(&self) -> String {
        vec![self.params.to_string(), Tok::LeftArrow.to_string(), self.expr.to_string()].join("")
    }
}
impl SExpr for Lambda {
    fn sexpr(&self) -> SResult<S> {
        Ok(S::Cons("lambda".into(), vec![self.params.sexpr()?, self.expr.sexpr()?]))
    }
}



#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    IdentExpr(Ident),
    LitExpr(LiteralNode),
    PrefixExpr(PrefixNode, Box<ExprNode>),
    InfixExpr(InfixNode, Box<ExprNode>, Box<ExprNode>),
    Lambda(Lambda),
    Apply(Ident, Vec<ExprNode>),
}

#[derive(PartialEq, Debug, Clone)]
pub struct ExprNode {
    pub s: Surround,
    pub value: Expr,
}
impl ExprNode {
    pub fn new(value: Expr) -> Self {
        Self {
            s: Surround::default(),
            value,
        }
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
            Lambda(e) => {
                out.append(&mut e.unparse());
            }
            Apply(ident, args) => {
                out.append(&mut ident.unparse());
                for arg in args {
                    out.append(&mut arg.unparse());
                }
            }
        };
        self.s.unparse(out)
    }

    fn to_string(&self) -> String {
        use Expr::*;
        let mut out = vec![];
        match &self.value {
            IdentExpr(x) => {
                out.push(x.to_string());
            }
            LitExpr(x) => {
                out.push(x.to_string());
            }
            PrefixExpr(prefix, expr) => {
                out.push(prefix.to_string());
                out.push(expr.to_string());
            }
            InfixExpr(op, left, right) => {
                out.push(left.to_string());
                out.push(op.to_string());
                out.push(right.to_string());
            }
            Lambda(e) => {
                out.push(e.to_string());
            }
            Apply(ident, args) => {
                out.push(ident.to_string());
                for arg in args {
                    out.push(arg.to_string());
                }
            }
        };
        out.join("")
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
                Ok(S::Cons(prefix.to_string(), vec![s]))
            }
            Lambda(e) => e.sexpr(),
            Apply(ident, args) => {
                let s_args = args.iter().filter_map(|a| a.sexpr().ok()).collect::<Vec<_>>();
                Ok(S::Cons(ident.to_string(), s_args))
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
    pub s: Surround,
    pub value: Prefix,
}

impl PrefixNode {
    pub fn from_token(token: Token) -> Option<Self> {
        let maybe_prefix = match token.tok {
            Tok::Plus => Some(Prefix::PrefixPlus),
            Tok::Minus => Some(Prefix::PrefixMinus),
            Tok::Not => Some(Prefix::PrefixNot),
            _ => None,
        };
        match maybe_prefix {
            Some(prefix) => Some(Self {
                s: Surround {
                    pre: token.pre.iter().map(|t| t.toks()).flatten().collect(),
                    post: token.post.iter().map(|t| t.toks()).flatten().collect(),
                },
                value: prefix,
            }),
            None => None,
        }
    }

    pub fn from_tokens(prefix: Prefix, pre: Vec<Tok>, post: Vec<Tok>) -> Self {
        Self {
            s: Surround { pre, post },
            value: prefix,
        }
    }

    pub fn new(prefix: Prefix) -> Self {
        Self {
            s: Surround::default(),
            value: prefix,
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

impl Unparse for PrefixNode {
    fn unparse(&self) -> Vec<Tok> {
        self.s.unparse(vec![self.token()])
    }

    fn to_string(&self) -> String {
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
pub struct InfixNode {
    pub s: Surround,
    pub value: Infix,
    pub precedence: Precedence,
}

impl InfixNode {
    pub fn new(infix: Infix, precedence: Precedence) -> Self {
        Self {
            s: Surround::default(),
            value: infix,
            precedence,
        }
    }

    pub fn from_token(token: Token) -> Option<Self> {
        let (precedence, maybe_prefix) = infix_op(&token.tok);
        match maybe_prefix {
            Some(prefix) => Some(Self {
                s: Surround {
                    pre: token.pre.iter().map(|t| t.toks()).flatten().collect(),
                    post: token.post.iter().map(|t| t.toks()).flatten().collect(),
                },
                value: prefix,
                precedence,
            }),
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
            s: Surround { pre, post },
            value: infix,
            precedence,
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

impl Unparse for InfixNode {
    fn unparse(&self) -> Vec<Tok> {
        self.s.unparse(vec![self.token()])
    }

    fn unlex(&self) -> String {
        self.token().unlex()
    }

    fn to_string(&self) -> String {
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
pub enum Literal {
    IntLiteral(u64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    StringLiteral(String),
    Invalid(String),
}

impl Literal {
    pub fn from_token(token: Tok) -> Option<Literal> {
        match token {
            Tok::IntLiteral(u) => Some(Literal::IntLiteral(u)),
            Tok::FloatLiteral(u) => Some(Literal::FloatLiteral(u)),
            Tok::StringLiteral(s) => Some(Literal::StringLiteral(s.clone())),
            Tok::BoolLiteral(b) => Some(Literal::BoolLiteral(b)),
            //Tok::Invalid(s) => Some(Literal::Invalid(s.clone())),
            _ => None,
        }
    }
    pub fn token(&self) -> Tok {
        use Literal::*;
        match &self {
            IntLiteral(x) => Tok::IntLiteral(*x),
            FloatLiteral(x) => Tok::FloatLiteral(*x),
            BoolLiteral(x) => Tok::BoolLiteral(*x),
            StringLiteral(x) => Tok::StringLiteral(x.clone()),
            Invalid(x) => Tok::Invalid(x.clone()),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Location {
    pub offset: usize,
    pub line: usize,
    pub col: usize,
    pub fragment: String,
}

impl Location {
    pub fn new(offset: usize, line: usize, col: usize, fragment: String) -> Self {
        Self { offset, line, col, fragment }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct LiteralNode {
    pub value: Literal,
    pub s: Surround,
    pub loc: Location,
}
impl LiteralNode {
    pub fn new(value: Literal, loc: Location) -> Self {
        Self {
            value,
            s: Surround::default(),
            loc
        }
    }

}
impl Unparse for LiteralNode {
    fn unparse(&self) -> Vec<Tok> {
        self.s.unparse(vec![self.value.token()])
    }
    fn to_string(&self) -> String {
        self.value.token().unlex()
    }
}

impl SExpr for LiteralNode {
    fn sexpr(&self) -> SResult<S> {
        use Literal::*;
        match &self.value {
            IntLiteral(x) => Ok(S::Atom(x.to_string())),
            FloatLiteral(x) => Ok(S::Atom(x.to_string())),
            BoolLiteral(x) => Ok(S::Atom(x.to_string())),
            StringLiteral(x) => Ok(S::Atom(x.to_string())),
            Invalid(s) => Err(SError::Invalid(s.clone())),
        }
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
                s: Surround {
                    pre: token.pre.iter().map(|t| t.toks()).flatten().collect(),
                    post: token.post.iter().map(|t| t.toks()).flatten().collect(),
                },
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
    fn to_string(&self) -> String {
        self.value.clone()
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
        //println!("{:?}", (&p, &p.token(), &p.unlex()));
        assert_eq!(p.unlex(), "-");
    }
}
