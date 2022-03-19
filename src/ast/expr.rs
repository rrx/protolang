use crate::lexer::Location;
use crate::parser::Unparse;
use crate::sexpr::*;
use crate::tokens::{Tok, Token};

use super::function::{Callable, CallableNode, Lambda, Params};
use super::node::{Context, Context2, MaybeNodeContext, NodeContext, NodeContextNull};
use super::visitor::{visit_expr, ExprVisitor, VResult, VisitError};
use super::{Operator, OperatorNode};
use std::fmt;

#[derive(Debug, Clone)]
pub enum Expr {
    Ident(String),
    Literal(Tok),
    Prefix(OperatorNode, Box<ExprNode>),
    Postfix(OperatorNode, Box<ExprNode>),
    Binary(Operator, Box<ExprNode>, Box<ExprNode>),
    Ternary(Operator, Box<ExprNode>, Box<ExprNode>, Box<ExprNode>),
    //NAry(Operator, Vec<ExprNode>),
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
    pub context: MaybeNodeContext,
    pub value: Expr,
}

impl std::ops::Deref for ExprNode {
    type Target = Expr;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl std::ops::DerefMut for ExprNode {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl fmt::Debug for ExprNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ExprNode")
            //.field("s", &self.context.s)
            //.field("loc", &self.context.loc)
            //
            .field("pre", &self.context.pre())
            .field("v", &self.value)
            .field("post", &self.context.post())
            .finish()
    }
}

impl ExprNode {
    pub fn new(value: Expr, loc: &Location) -> Self {
        Self {
            context: MaybeNodeContext::from_location(loc),
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
            let context = MaybeNodeContext::from_token(token);
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
        let loc = item.context.to_location();
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
        self.context.unparse(out)
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
