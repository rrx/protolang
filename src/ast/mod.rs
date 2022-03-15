use crate::sexpr::*;
use crate::tokens::{Tok, Token};
use crate::lexer::{Location, Surround};
use std::fmt;
mod function;
pub use function::{Lambda, Callable, CallableNode, Params};

mod value;
pub use value::{Value};

mod node;
pub use node::{Context, NodeContext};

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

#[derive(Debug, Clone)]
pub enum Stmt {
    //Assign(ExprNode, ExprNode),
    //Block(Vec<StmtNode>),
    //Expr(ExprNode),
    //Lit(Value),
    Invalid(String),
    Empty
}
#[derive(Debug, Clone)]
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
            //Stmt::Expr(expr) => expr.unparse(),
            //Stmt::Lit(lit) => lit.unparse(),
            //
            /*
            Stmt::Assign(ident, expr) => vec![ident.unparse(), vec![Tok::Assign], expr.unparse()]
                .into_iter()
                .flatten()
                .collect(),
            */
            //Stmt::Block(stmts) => stmts.iter().map(|s| s.unparse()).flatten().collect(),
            Stmt::Invalid(s) => vec![Tok::Invalid(s.clone())],
            Stmt::Empty => vec![],
        })
    }
}
impl SExpr for StmtNode {
    fn sexpr(&self) -> SResult<S> {
        match &self.value {
            //Stmt::Lit(x) => x.sexpr(),
            //Stmt::Expr(x) => x.sexpr(),
            /*
            Stmt::Assign(ident, expr) => {
                let sident = ident.sexpr()?;
                let sexpr = expr.sexpr()?;
                Ok(S::Cons("def".into(), vec![sident, sexpr]))
            }
            */
//            Stmt::Block(stmts) => Ok(S::Cons(
 //               "block".into(),
  //              stmts.into_iter().filter_map(|s| s.sexpr().ok()).collect(),
   //         )),
            Stmt::Invalid(s) => Err(SError::Invalid(s.clone())),
            Stmt::Empty => Ok(S::Null)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Program {
    pub value: Vec<ExprNode>,
    pub context: NodeContext
}
impl Program {
    pub fn new(value: Vec<ExprNode>) -> Self {
        Self {
            context: NodeContext::default(),
            value,
        }
    }
}

impl Unparse for Program {
    fn unparse(&self) -> Vec<Tok> {
        self.context.s.unparse(
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

#[derive(Debug, Clone)]
pub enum Expr {
    Ident(String),
    LitExpr(Value),
    Prefix(OperatorNode, Box<ExprNode>),
    Postfix(OperatorNode, Box<ExprNode>),
    Binary(Operator, Box<ExprNode>, Box<ExprNode>),
    Ternary(Operator, Box<ExprNode>, Box<ExprNode>, Box<ExprNode>),
    Lambda(Lambda),
    Callable(Box<dyn Callable>),
    List(Vec<ExprNode>),
    Chain(Operator, Vec<ExprNode>),

    // Function application (the function, the params)
    Apply(Box<ExprNode>, Vec<ExprNode>),
    Index(Box<ExprNode>, Box<ExprNode>),
    Block(Vec<ExprNode>),
    Invalid(String)
}

#[derive(Clone)]
pub struct ExprNode {
    pub context: NodeContext,
    pub value: Expr,
}

impl fmt::Debug for ExprNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ExprNode")
            //.field("s", &self.context.s)
            //.field("loc", &self.context.loc)
            //
            .field("pre", &self.context.s.pre)
            .field("v", &self.value)
            .field("post", &self.context.s.post)
            .finish()
    }
}

impl ExprNode {
    pub fn new(value: Expr, loc: &Location) -> Self {
        Self {
            context: NodeContext::from_location(loc),
            value,
        }
    }

    pub fn is_ident(&self) -> bool {
        if let Expr::Ident(_) = self.value {
            true
        } else {
            false
        }
    }

    pub fn try_ident(&self) -> Option<String> {
        if let Expr::Ident(s) = &self.value {
            Some(s.clone())
        } else {
            None
        }
    }

    pub fn from_token(token: &Token) -> Option<Self> {
        let maybe = match &token.tok {
            Tok::Ident(s) => Some(Expr::Ident(s.clone())),
            _ => None,
        };
        if let Some(value) = maybe {
            let context = NodeContext::from_token(token);
            Some(Self { context, value })
        } else {
            None
        }
    }

    /*
    pub fn dump(&self) {
        println!("ExprNode");
        println!("\tStack: {:?}", self.indent_stack);
        for token in &self.acc {
            println!("\tToken: {:?}", token);
        }
        println!("\tIndentState: {:?}", self.indent_state);
    }
    */
}
/*

impl TryFrom<&PrattValue> for Expr {
    type Error = ();
    fn try_from(value: &PrattValue) -> Result<Self, Self::Error> {
        match value {
            PrattValue::Prefix(tok, node) => {
                Expr::Prefix(
            }
            PrattValue::Binary(Tok, Box<ASTNode>, Box<ASTNode>),
            PrattValue::Ternary(Tok, Box<ASTNode>, Box<ASTNode>, Box<ASTNode>),
            PrattValue::Postfix(Tok, Box<ASTNode>),
            PrattValue::Ident(Tok),
            PrattValue::Error(String),
            PrattValue::List(Vec<ASTNode>),
            PrattValue::Chain(Tok, Vec<ASTNode>),
            PrattValue::Expr(Box<ASTNode>),
            Tok::IntLiteral(_) => Ok(Value::Literal(value.clone())),
            Tok::FloatLiteral(_) => Ok(Value::Literal(value.clone())),
            Tok::StringLiteral(_) => Ok(Value::Literal(value.clone())),//s.clone())),
            Tok::BoolLiteral(_) => Ok(Value::Literal(value.clone())),
            Tok::Null => Ok(Value::Null),
            //Tok::Invalid(s) => Some(Value::Invalid(s.clone())),
            _ => Err(()),
        }
    }
}

*/


/*
impl From<ExprNode> for StmtNode {
    fn from(item: ExprNode) -> Self {
        let loc = item.context.loc.clone();
        Self::new(Stmt::Expr(item), loc)
    }
}
*/

impl From<Lambda> for ExprNode {
    fn from(item: Lambda) -> Self {
        let loc = item.loc.clone();
        Self::new(Expr::Lambda(item), &loc)
    }
}

impl Unparse for ExprNode {
    fn unparse(&self) -> Vec<Tok> {
        let mut out = vec![];
        match &self.value {
            Expr::Ternary(_,x,y,z) => {
                out.append(&mut x.unparse());
                out.append(&mut y.unparse());
                out.append(&mut z.unparse());
            }
            Expr::Chain(_,_) => {}
            Expr::Ident(x) => {
                out.push(Tok::Ident(x.clone()));
            }
            Expr::LitExpr(x) => {
                out.append(&mut x.unparse());
            }
            Expr::Prefix(unary, expr) => {
                out.append(&mut expr.unparse());
            }
            Expr::Postfix(unary, expr) => {
                out.append(&mut expr.unparse());
            }
            Expr::Binary(op, left, right) => {
                out.append(&mut left.unparse());
                out.append(&mut right.unparse());
            }
            Expr::List(elements) => {
                for e in elements {
                    out.append(&mut e.unparse());
                }
            }
            Expr::Lambda(e) => {
                out.append(&mut e.unparse());
            }
            Expr::Callable(_) => {
                //out.append(&mut e.unparse());
            }
            Expr::Index(expr, arg) => {
                out.append(&mut expr.unparse());
                out.append(&mut arg.unparse());
            }
            Expr::Apply(ident, args) => {
                out.append(&mut ident.unparse());
                for arg in args {
                    out.append(&mut arg.unparse());
                }
            }
            Expr::Block(stmts) => {
                out.append(&mut stmts.into_iter().map(|s| s.unparse()).flatten().collect());
            }
            Expr::Invalid(s) => {
                out.push(Tok::Invalid(s.clone()));
            }
        };
        self.context.s.unparse(out)
    }
}
impl SExpr for ExprNode {
    fn sexpr(&self) -> SResult<S> {
        use Expr::*;
        match &self.value {
            Ternary(op,x,y,z) => {
                Ok(S::Cons(op.token().unlex(), vec![x.sexpr()?,y.sexpr()?,z.sexpr()?]))
            }
            Chain(_,_) => {
                Ok(S::Cons("chain".into(), vec![]))
            }
            LitExpr(x) => x.sexpr(),
            Ident(x) => Ok(S::Atom(x.clone())),
            Binary(op, left, right) => {
                let sleft = left.sexpr()?;
                let sright = right.sexpr()?;
                Ok(S::Cons(op.token().unlex(), vec![sleft, sright]))
            }
            Prefix(op, expr) => {
                let s = expr.sexpr()?;
                Ok(S::Cons(op.unlex(), vec![s]))
            }
            Postfix(op, expr) => {
                let s = expr.sexpr()?;
                Ok(S::Cons(op.unlex(), vec![s]))
            }
            List(elements) => {
                let s_args = elements
                    .iter()
                    .filter_map(|a| a.sexpr().ok())
                    .collect::<Vec<_>>();
                Ok(S::Cons("list".into(), s_args))
            }
            Lambda(e) => e.sexpr(),
            Callable(_) => Ok(S::Cons("callable".into(), vec![])),
            Index(expr, arg) => {
                let sexpr = expr.sexpr()?;
                let sarg = arg.sexpr()?;
                Ok(S::Cons("index".into(), vec![sexpr, sarg]))
            }
            Apply(expr, args) => {
                let mut s_args = args
                    .iter()
                    .filter_map(|a| a.sexpr().ok())
                    .collect::<Vec<_>>();
                let mut v = vec![expr.sexpr()?];
                v.append(&mut s_args);
                Ok(S::Cons("apply".into(), v))
            }
            Block(stmts) => Ok(S::Cons(
                "block".into(),
                stmts.into_iter().filter_map(|s| s.sexpr().ok()).collect(),
            )),
            Invalid(s) => Err(SError::Invalid(s.clone())),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct OperatorNode {
    pub context: NodeContext,
    pub value: Operator,
}

impl OperatorNode {
    pub fn prefix_token(&self) -> Option<Tok> {
        match self.value {
            Operator::Plus => Some(Tok::Plus),
            Operator::Minus => Some(Tok::Minus),
            Operator::Not => Some(Tok::Exclamation),
            _ => None
        }
    }

    pub fn postfix_token(&self) -> Option<Tok> {
        match self.value {
            Operator::Bang => Some(Tok::Exclamation),
            _ => None
        }
    }

    pub fn from_postfix_tok(token: &Tok) -> Option<Operator> {
        match token {
            //Tok::Percent => Some(Operator::Bang),
            Tok::Exclamation => Some(Operator::Bang),
            _ => None,
        }
    }

    pub fn from_postfix_token(token: Token) -> Option<Self> {
        match Self::from_postfix_tok(&token.tok) {
            Some(postfix) => {
                Some(Self {
                    context: NodeContext::move_token(token),
                    value: postfix,
                })
            }
            None => None,
        }
    }

    pub fn from_prefix_tok(token: &Tok) -> Option<Operator> {
        match token {
            Tok::Plus => Some(Operator::Plus),
            Tok::Minus => Some(Operator::Minus),
            Tok::Exclamation => Some(Operator::Not),
            _ => None,
        }
    }
    pub fn from_prefix_token(token: &Token) -> Option<Self> {
        match Self::from_prefix_tok(&token.tok) {
            Some(prefix) => {
                Some(Self {
                    context: Context::from_token(&token),
                    value: prefix,
                })
            }
            None => None,
        }
    }

    pub fn from_token(token: &Token) -> Option<Self> {
        match Operator::from_tok(&token.tok) {
            Some(prefix) => {
                Some(Self {
                    context: Context::from_token(&token),
                    value: prefix,
                })
            }
            None => None,
        }
    }

    pub fn from_tokens(prefix: Operator, pre: Vec<Tok>, post: Vec<Tok>) -> Self {
        let s = Surround::new(pre, post);

        let context = NodeContext { s, loc: Location::default() };
        Self {
            value: prefix,
            context
        }
    }

    /*
    pub fn parse(i: Tokens) -> PResult<Tokens, Self> {
        let (_, maybe_op) = infix_op(&token);
        let operator = maybe_op.unwrap();
        let op = OperatorNode::from_location(&i, operator); 
    }
    */

    pub fn new(prefix: Operator) -> Self {
        Self {
            context: NodeContext::default(),
            value: prefix,
        }
    }

    pub fn token(&self) -> Tok {
        self.value.token()
    }

}

impl Unparse for OperatorNode {
    fn unparse(&self) -> Vec<Tok> {
        self.context.s.unparse(vec![self.value.token()])
    }

    fn unlex(&self) -> String {
        self.value.token().unlex()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Not,
    Divide,
    Multiply,
    Exp,
    Equal,
    NotEqual,
    GreaterThanEqual,
    LessThanEqual,
    GreaterThan,
    LessThan,
    Assign,
    Bang,
    Modulus,
    Index,
    Call,
    Elvis,
    Conditional,
    ConditionalElse,
    End,
    Comma
    //Map,
}
impl Operator {
    pub fn token(&self) -> Tok {
        match self {
            Operator::Plus => Tok::Plus,
            Operator::Minus => Tok::Minus,
            Operator::Not => Tok::Exclamation,
            Operator::Multiply => Tok::Mul,
            Operator::Divide => Tok::Div,
            Operator::Exp => Tok::Caret,
            Operator::Equal => Tok::Equals,
            Operator::NotEqual => Tok::NotEquals,
            Operator::LessThanEqual => Tok::LTE,
            Operator::GreaterThanEqual => Tok::GTE,
            Operator::LessThan => Tok::LT,
            Operator::GreaterThan => Tok::GT,
            Operator::Assign => Tok::Assign,
            Operator::Modulus => Tok::Percent,
            Operator::Bang => Tok::Exclamation,
            Operator::Index => Tok::LBracket,
            Operator::Call => Tok::LParen,
            Operator::Elvis => Tok::Elvis,
            Operator::ConditionalElse => Tok::Colon,
            Operator::Conditional => Tok::Question,
            Operator::End => Tok::SemiColon,
            Operator::Comma => Tok::Comma,
            //Operator::Map => Tok::LeftArrow,
        }
    }

    pub fn from_tok(token: &Tok) -> Option<Self> {
        match token {
            Tok::Equals => Some(Operator::Equal),
            Tok::NotEquals => Some(Operator::NotEqual),
            Tok::LTE => Some(Operator::LessThanEqual),
            Tok::GTE => Some(Operator::GreaterThanEqual),
            Tok::LT => Some(Operator::LessThan),
            Tok::GT => Some(Operator::GreaterThan),
            Tok::Plus => Some(Operator::Plus),
            Tok::Minus => Some(Operator::Minus),
            Tok::Mul => Some(Operator::Multiply),
            Tok::Div => Some(Operator::Divide),
            Tok::Caret => Some(Operator::Exp),
            Tok::LParen => Some(Operator::Call),
            Tok::LBracket => Some(Operator::Index),
            Tok::Assign => Some(Operator::Assign),
            Tok::Percent => Some(Operator::Modulus),
            Tok::Comma => Some(Operator::Comma),
            Tok::Elvis => Some(Operator::Elvis),
            _ => None,
        }
    }

}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    PLowest, // Parens, Start
    PAssign, // Assignment operator
    //PMap,
    PEquals, // Equality ==/!=
    PLessGreater,
    PSum,
    PPrefix,
    PProduct,
    PModulus,
    PExp,
    PCall,
    PIndex,
    PBang,
    PHighest,
}

pub fn infix_precedence(op: Operator) -> Precedence {
    match op {
        Operator::Equal => Precedence::PEquals,
        Operator::NotEqual => Precedence::PEquals,
        Operator::LessThanEqual => Precedence::PLessGreater,
        Operator::GreaterThanEqual => Precedence::PLessGreater,
        Operator::LessThan => Precedence::PLessGreater,
        Operator::GreaterThan => Precedence::PLessGreater,
        Operator::Plus => Precedence::PSum,
        Operator::Not => Precedence::PLowest,
        Operator::Minus => Precedence::PSum,
        Operator::Multiply => Precedence::PProduct,
        Operator::Divide => Precedence::PProduct,
        Operator::Exp => Precedence::PExp,
        Operator::Assign => Precedence::PAssign,
        Operator::Modulus => Precedence::PModulus,
        Operator::Bang => Precedence::PBang,
        Operator::Index => Precedence::PIndex,
        Operator::Call => Precedence::PCall,
        Operator::Elvis => Precedence::PCall,
        Operator::ConditionalElse => Precedence::PCall,
        Operator::Conditional => Precedence::PCall,
        Operator::End => Precedence::PLowest,
        Operator::Comma => Precedence::PLowest,

        //Operator::Map => Precedence::PMap,
    }
}

pub fn prefix_op(t: &Tok) -> (Precedence, Option<Operator>) {
    match *t {
        Tok::Plus => (Precedence::PPrefix, Some(Operator::Plus)),
        Tok::Minus => (Precedence::PPrefix, Some(Operator::Minus)),
        Tok::Exclamation => (Precedence::PPrefix, Some(Operator::NotEqual)),
        _ => (Precedence::PLowest, None),
    }
}

pub fn postfix_op(t: &Tok) -> (Precedence, Option<Operator>) {
    match *t {
        Tok::Exclamation => (Precedence::PBang, Some(Operator::Bang)), 
        _ => (Precedence::PLowest, None),
    }
}

pub fn infix_op(t: &Tok) -> (Precedence, Option<Operator>) {
    match *t {
        Tok::Equals => (Precedence::PEquals, Some(Operator::Equal)),
        Tok::NotEquals => (Precedence::PEquals, Some(Operator::NotEqual)),
        //Tok::LeftArrow => (Precedence::PMap, Some(Operator::Map)),
        Tok::LTE => (Precedence::PLessGreater, Some(Operator::LessThanEqual)),
        Tok::GTE => (Precedence::PLessGreater, Some(Operator::GreaterThanEqual)),
        Tok::LT => (Precedence::PLessGreater, Some(Operator::LessThan)),
        Tok::GT => (Precedence::PLessGreater, Some(Operator::GreaterThan)),
        Tok::Plus => (Precedence::PSum, Some(Operator::Plus)),
        Tok::Minus => (Precedence::PSum, Some(Operator::Minus)),
        Tok::Mul => (Precedence::PProduct, Some(Operator::Multiply)),
        Tok::Div => (Precedence::PProduct, Some(Operator::Divide)),
        Tok::Caret => (Precedence::PExp, Some(Operator::Exp)),
        Tok::LParen => (Precedence::PCall, Some(Operator::Call)),
        Tok::LBracket => (Precedence::PIndex, Some(Operator::Index)),
        Tok::Assign => (Precedence::PAssign, Some(Operator::Assign)),
        Tok::Percent => (Precedence::PModulus, Some(Operator::Modulus)),
        Tok::SemiColon => (Precedence::PHighest, None),
        Tok::Comma => (Precedence::PLowest, Some(Operator::Comma)),
        Tok::Elvis => (Precedence::PLowest, Some(Operator::Elvis)),
        _ => (Precedence::PLowest, None),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prefix() {
        let p = OperatorNode::new(Operator::Minus);
        assert_eq!(p.unlex(), "-");
    }
}
