use crate::lexer::{Location, Surround};
use crate::sexpr::*;
use crate::tokens::{Tok, Token};
use std::fmt;
mod function;
pub use function::{Callable, CallableNode, Lambda, Params};

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
pub enum Expr {
    Ident(String),
    Literal(Tok),
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
    Program(Vec<ExprNode>),
    Invalid(String),
    Void,
}

impl Expr {
    pub fn is_void(&self) -> bool {
        if let Expr::Void = self {
            true
        } else {
            false
        }
    }

    pub fn is_ident(&self) -> bool {
        if let Expr::Ident(_) = self {
            true
        } else {
            false
        }
    }

    pub fn try_ident(&self) -> Option<String> {
        if let Expr::Ident(s) = self {
            Some(s.clone())
        } else {
            None
        }
    }
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

    pub fn from_token(token: &Token) -> Option<Self> {
        let maybe = match &token.tok {
            Tok::Ident(s) => Some(Expr::Ident(s.clone())),
            Tok::Invalid(s) => Some(Expr::Invalid(s.clone())),
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

impl From<Lambda> for ExprNode {
    fn from(item: Lambda) -> Self {
        let loc = item.loc.clone();
        Self::new(Expr::Lambda(item), &loc)
    }
}

impl TryFrom<&Tok> for Expr {
    type Error = ();
    fn try_from(value: &Tok) -> Result<Self, Self::Error> {
        match value {
            Tok::IntLiteral(_) => Ok(Expr::Literal(value.clone())),
            Tok::FloatLiteral(_) => Ok(Expr::Literal(value.clone())),
            Tok::StringLiteral(_) => Ok(Expr::Literal(value.clone())),
            Tok::BoolLiteral(_) => Ok(Expr::Literal(value.clone())),
            Tok::Null => Ok(Expr::Void),
            //Tok::Invalid(s) => Some(Expr::Invalid(s.clone())),
            _ => Err(()),
        }
    }
}

impl Unparse for ExprNode {
    fn unparse(&self) -> Vec<Tok> {
        let mut out = vec![];
        match &self.value {
            Expr::Ternary(_, x, y, z) => {
                out.append(&mut x.unparse());
                out.append(&mut y.unparse());
                out.append(&mut z.unparse());
            }
            Expr::Chain(_, _) => {}
            Expr::Ident(x) => {
                out.push(Tok::Ident(x.clone()));
            }
            Expr::Literal(x) => {
                out.push(x.clone());
                //out.append(&mut x.unparse());
            }
            Expr::Prefix(_unary, expr) => {
                out.append(&mut expr.unparse());
            }
            Expr::Postfix(_unary, expr) => {
                out.append(&mut expr.unparse());
            }
            Expr::Binary(_op, left, right) => {
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
            Expr::Block(exprs) | Expr::Program(exprs) => {
                out.append(&mut exprs.into_iter().map(|s| s.unparse()).flatten().collect());
            }
            Expr::Invalid(s) => {
                out.push(Tok::Invalid(s.clone()));
            }

            // void returns nothing
            Expr::Void => (),
        };
        self.context.s.unparse(out)
    }
}
impl SExpr for ExprNode {
    fn sexpr(&self) -> SResult<S> {
        use Expr::*;
        match &self.value {
            Ternary(op, x, y, z) => Ok(S::Cons(
                op.token().unlex(),
                vec![x.sexpr()?, y.sexpr()?, z.sexpr()?],
            )),
            Chain(_, _) => Ok(S::Cons("chain".into(), vec![])),
            Literal(x) => Ok(S::Atom(x.unlex())),
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
            Block(exprs) => Ok(S::Cons(
                "block".into(),
                exprs.into_iter().filter_map(|s| s.sexpr().ok()).collect(),
            )),
            Program(exprs) => Ok(S::Cons(
                "program".into(),
                exprs.into_iter().filter_map(|s| s.sexpr().ok()).collect(),
            )),
            Invalid(s) => Err(SError::Invalid(s.clone())),
            Void => Ok(S::Null),
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
            _ => None,
        }
    }

    pub fn postfix_token(&self) -> Option<Tok> {
        match self.value {
            Operator::Bang => Some(Tok::Exclamation),
            _ => None,
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
            Some(postfix) => Some(Self {
                context: NodeContext::move_token(token),
                value: postfix,
            }),
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
            Some(prefix) => Some(Self {
                context: Context::from_token(&token),
                value: prefix,
            }),
            None => None,
        }
    }

    pub fn from_token(token: &Token) -> Option<Self> {
        match Operator::from_tok(&token.tok) {
            Some(prefix) => Some(Self {
                context: Context::from_token(&token),
                value: prefix,
            }),
            None => None,
        }
    }

    pub fn from_tokens(prefix: Operator, pre: Vec<Tok>, post: Vec<Tok>) -> Self {
        let s = Surround::new(pre, post);

        let context = NodeContext {
            s,
            loc: Location::default(),
        };
        Self {
            value: prefix,
            context,
        }
    }

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
    Comma, //Map,
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
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prefix() {
        let p = OperatorNode::new(Operator::Minus);
        assert_eq!(p.unlex(), "-");
    }
}
