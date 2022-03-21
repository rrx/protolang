use crate::lexer::Location;
use crate::parser::Unparse;
use crate::sexpr::*;
use crate::tokens::{Tok, Token};

use super::function::{Callable, Lambda};
use super::node::{Context, Context2, MaybeNodeContext};
use super::{visit_expr, ExprVisitor, VResult};
use super::{Operator, OperatorNode};
use log::debug;
use std::fmt;
use std::fmt::Write;

#[derive(Debug, Clone)]
pub enum VarModifier {
    Mutable,
    Default,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
    pub modifier: VarModifier,
}

impl Identifier {
    pub fn new(name: String, modifier: VarModifier) -> Self {
        Self { name, modifier }
    }
}

impl From<&str> for Identifier {
    fn from(item: &str) -> Self {
        Self::new(item.into(), VarModifier::Default)
    }
}

#[derive(Debug, strum::Display, Clone, strum_macros::EnumProperty, strum_macros::IntoStaticStr)]
pub enum Expr {
    Ident(Identifier),
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

    pub fn try_ident(&self) -> Option<Identifier> {
        if let Expr::Ident(s) = self {
            Some(s.clone())
        } else {
            None
        }
    }

    pub fn try_literal(&self) -> Option<Tok> {
        if let Expr::Literal(s) = self {
            Some(s.clone())
        } else {
            None
        }
    }

    pub fn try_callable(&self) -> Option<Box<dyn Callable>> {
        if let Expr::Callable(s) = self {
            Some(s.clone())
        } else {
            None
        }
    }

    pub fn is_number(&self) -> bool {
        if let Self::Literal(tok) = self {
            match tok {
                Tok::IntLiteral(_) => true,
                Tok::FloatLiteral(_) => true,
                _ => false,
            }
        } else {
            false
        }
    }

    pub fn new_float(f: f64) -> Self {
        Self::Literal(Tok::FloatLiteral(f))
    }

    pub fn new_int(u: u64) -> Self {
        Self::Literal(Tok::IntLiteral(u))
    }

    pub fn new_bool(b: bool) -> Self {
        Self::Literal(Tok::BoolLiteral(b))
    }
}

#[derive(Clone)]
pub struct ExprNode {
    pub context: MaybeNodeContext,
    pub value: Expr,
}

impl Default for ExprNode {
    fn default() -> Self {
        Self {
            context: MaybeNodeContext::default(),
            value: Expr::Void,
        }
    }
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

pub struct ExprFormatter {
    depth: usize,
}
impl ExprVisitor<String> for ExprFormatter {
    fn enter(&mut self, e: &ExprNode, f: &mut String) -> VResult {
        let indent: String = String::from_utf8(vec![b'\t'; self.depth]).unwrap();
        let s: &'static str = e.value.clone().into();
        let _ = match &e.value {
            Expr::Void => {
                write!(f, "{}{}\n", indent, s)
            }
            Expr::Ternary(op, _, _, _) => {
                write!(f, "{}{}({:?})\n", indent, s, op)
            }
            Expr::Chain(_, _) => {
                write!(f, "{}{}\n", indent, s)
            }
            Expr::Prefix(op, _) => {
                write!(f, "{}{}({:?})\n", indent, s, op.value)
            }
            Expr::Postfix(op, _) => {
                write!(f, "{}{}({:?})\n", indent, s, op)
            }
            Expr::Binary(op, _, _) => {
                write!(f, "{}{}({:?})\n", indent, s, op)
            }
            Expr::List(elements) => {
                write!(f, "{}{}(len={})\n", indent, s, elements.len())
            }
            Expr::Callable(_) => {
                write!(f, "{}{}\n", indent, s)
            }
            Expr::Index(_, _) => {
                write!(f, "{}{}\n", indent, s)
            }
            Expr::Apply(_, _) => {
                write!(f, "{}{}\n", indent, s)
            }
            Expr::Block(exprs) | Expr::Program(exprs) => {
                write!(f, "{}{}(len={})\n", indent, s, exprs.len())
            }
            Expr::Ident(x) => {
                write!(f, "{}{}({:?})\n", indent, s, x)
            }
            Expr::Literal(x) => {
                write!(f, "{}{}({:?})\n", indent, s, x)
            }
            Expr::Lambda(x) => {
                write!(f, "{}{}\n", indent, x.sexpr().unwrap())
            }
            Expr::Invalid(v) => {
                write!(f, "{}{}({})\n", indent, s, v)
            }
        };
        self.depth += 1;
        Ok(())
    }

    fn exit(&mut self, _: &ExprNode, _: &mut String) -> VResult {
        self.depth -= 1;
        Ok(())
    }
}

impl fmt::Debug for ExprNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = f.debug_struct("ExprN");
        let pre = self.context.pre();
        let post = self.context.post();
        let mut out = if pre.len() > 0 {
            out.field("pre", &pre);
            out.field("v", &self.value)
        } else {
            out.field("v", &self.value)
        };
        if post.len() > 0 {
            out = out.field("post", &post);
        }
        out.finish()
    }
}

impl ExprNode {
    pub fn new(value: Expr, loc: &Location) -> Self {
        Self {
            context: MaybeNodeContext::from_location(loc),
            value,
        }
    }

    pub fn new_with_token(value: Expr, token: &Token) -> Self {
        let context = MaybeNodeContext::from_token(token);
        Self { context, value }
    }

    pub fn debug(&self) {
        let mut p = ExprFormatter { depth: 0 };
        let mut s = String::new();
        let _ = visit_expr(&self, &mut p, &mut s).unwrap();
        debug!("{}", s);
    }
}

impl From<Expr> for ExprNode {
    fn from(value: Expr) -> Self {
        Self {
            context: MaybeNodeContext::default(),
            value,
        }
    }
}

impl From<ExprNode> for Expr {
    fn from(expr: ExprNode) -> Self {
        expr.value
    }
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
                out.push(Tok::Ident(x.name.clone()));
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
            Ident(x) => Ok(S::Atom(x.name.clone())),
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
