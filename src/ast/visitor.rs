use super::{ExprNode, Unparse, Expr};
use crate::tokens::Tok;

#[derive(Debug)]
pub enum VisitError {
    Error
}

pub type VResult = Result<(), VisitError>;

pub trait ExprVisitor<N> {
    fn enter(&mut self, _: &ExprNode, _: &mut N) -> VResult { Ok(()) }
    fn exit(&mut self, _: &ExprNode, _: &mut N) -> VResult { Ok(()) }
    fn leaf(&mut self, _: &ExprNode, _: &mut N) -> VResult { Ok(()) }
    fn ident(&mut self, _: &String, _: &mut N) -> VResult {Ok(())}
    fn literal(&mut self, _ok: &Tok, _: &mut N) -> VResult {Ok(())}
}

pub trait ExprVisitorMut {
    fn enter(&mut self, _: &mut ExprNode) -> bool;
    fn exit(&mut self, _: &mut ExprNode) -> bool;
    fn ident(&mut self, _: &mut String) -> bool {true}
    fn literal(&mut self, _: &mut Tok) -> bool {true}
}

pub struct DFS {}
impl ExprVisitor<Vec<Tok>> for DFS {
    fn leaf(&mut self, e: &ExprNode, n: &mut Vec<Tok>) -> VResult {
        n.append(&mut e.unparse());
        Ok(())
    }
}

pub struct BFS {}
impl ExprVisitor<Vec<Tok>> for BFS {
    fn exit(&mut self, e: &ExprNode, n: &mut Vec<Tok>) -> VResult {
        n.append(&mut e.unparse());
        Ok(())
    }
}

pub fn visit_expr<N>(e: &ExprNode, f: &mut impl ExprVisitor<N>, n: &mut N) -> VResult {
    f.enter(e, n)?;
    match &e.value {
        Expr::Ternary(_, x, y, z) => {
            visit_expr(&x, f, n)?;
            visit_expr(&y, f, n)?;
            visit_expr(&z, f, n)?;
        }
        Expr::Chain(_, _) => {
            f.leaf(e, n)?;
        }
        Expr::Prefix(_unary, expr) => {
            visit_expr(&expr, f, n)?;
        }
        Expr::Postfix(_unary, expr) => {
            visit_expr(&expr, f, n)?;
        }
        Expr::Binary(_op, left, right) => {
            visit_expr(&left, f, n)?;
            visit_expr(&right, f, n)?;
        }
        Expr::List(elements) => {
            for e in elements {
                visit_expr(&e, f, n)?;
            }
        }
        Expr::Callable(_) => {
            //out.append(&mut e.unparse());
            f.leaf(e, n)?;
        }
        Expr::Index(expr, arg) => {
            visit_expr(&expr, f, n)?;
            visit_expr(&arg, f, n)?;
        }
        Expr::Apply(ident, args) => {
            visit_expr(&ident, f, n)?;
            for arg in args {
                visit_expr(&arg, f, n)?;
            }
        }
        Expr::Block(exprs) | Expr::Program(exprs) => {
            for e in exprs {
                visit_expr(&e, f, n)?;
            }
        }
        Expr::Ident(x) => {
            f.ident(x, n)?;
            f.leaf(e, n)?;
        }
        Expr::Literal(x) => {
            f.literal(x, n)?;
            f.leaf(e, n)?;
        }
        Expr::Lambda(_) => {
            f.leaf(e, n)?;
        }
        Expr::Invalid(_) => {
            f.leaf(e, n)?;
        }
        _ => ()
    };
    f.exit(e, n)
}

