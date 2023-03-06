use super::{Expr, ExprNode, Identifier};
use crate::tokens::Tok;

#[derive(Debug)]
pub enum VisitError {
    Error,
}

pub type VResult = Result<(), VisitError>;

pub trait ExprVisitor<N> {
    fn enter(&mut self, _: &ExprNode, _: &mut N) -> VResult {
        Ok(())
    }
    fn exit(&mut self, _: &ExprNode, _: &mut N) -> VResult {
        Ok(())
    }
    fn leaf(&mut self, _: &ExprNode, _: &mut N) -> VResult {
        Ok(())
    }
    fn ident(&mut self, _: &Identifier, _: &mut N) -> VResult {
        Ok(())
    }
    fn literal(&mut self, _: &Tok, _: &mut N) -> VResult {
        Ok(())
    }
    fn void(&mut self, _: &ExprNode, _: &mut N) -> VResult {
        Ok(())
    }
}

pub trait ExprVisitorMut {
    fn enter(&mut self, _: &mut ExprNode) -> bool;
    fn exit(&mut self, _: &mut ExprNode) -> bool;
    fn ident(&mut self, _: &mut Identifier) -> bool {
        true
    }
    fn literal(&mut self, _: &mut Tok) -> bool {
        true
    }
}

impl Expr {
    pub fn children(&self) -> Vec<ExprNode> {
        let mut out = vec![];
        match self {
            Expr::Ternary(_, x, y, z) => {
                out.push(x.clone());
                out.push(y.clone());
                out.push(z.clone());
            }
            Expr::BinaryChain(elements) => {
                out.append(
                    &mut elements
                        .into_iter()
                        .map(|e| Box::new(e.clone()))
                        .collect::<Vec<_>>(),
                );
            }
            Expr::Chain(_, _) => {}
            Expr::Prefix(_unary, expr) => {
                out.push(expr.clone());
            }
            Expr::Postfix(_unary, expr) => {
                out.push(expr.clone());
            }
            Expr::Binary(_op, left, right) => {
                out.push(left.clone());
                out.push(right.clone());
            }
            Expr::List(elements) => {
                out.append(
                    &mut elements
                        .into_iter()
                        .map(|e| Box::new(e.clone()))
                        .collect::<Vec<_>>(),
                );
            }
            Expr::Callable(_) => {
                unimplemented!()
            }
            Expr::Callback(_) => {
                unimplemented!()
            }
            Expr::Index(expr, arg) => {
                out.push(expr.clone());
                out.push(arg.clone());
            }
            Expr::Apply(ident, args) => {
                out.push(ident.clone());
                for arg in args {
                    out.push(Box::new(arg.clone()));
                }
            }
            Expr::Block(exprs) | Expr::Program(exprs) | Expr::Loop(exprs) => {
                for e in exprs {
                    out.push(Box::new(e.clone()));
                }
            }
            Expr::Break(e) => {
                out.push(e.clone());
            }
            Expr::Continue => {}
            Expr::Ident(_) => {}
            Expr::Literal(_) => {}
            Expr::Lambda(_) => {}
            Expr::Invalid(_) => {}
            Expr::Void => {}
        }
        out.into_iter().map(|v| *v).collect::<Vec<_>>()
    }
}

pub fn visit_expr<N>(e: &ExprNode, f: &mut impl ExprVisitor<N>, n: &mut N) -> VResult {
    f.enter(e, n)?;
    match &e.value {
        Expr::Void => {
            f.void(e, n)?;
        }
        Expr::Ternary(_, x, y, z) => {
            visit_expr(&x, f, n)?;
            visit_expr(&y, f, n)?;
            visit_expr(&z, f, n)?;
        }
        Expr::BinaryChain(elements) => {
            for e in elements {
                visit_expr(&e, f, n)?;
            }
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
        Expr::Callback(_) => {
            f.leaf(e, n)?;
        }
        Expr::Callable(_) => {
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
        Expr::Block(exprs) | Expr::Program(exprs) | Expr::Loop(exprs) => {
            for e in exprs {
                visit_expr(&e, f, n)?;
            }
        }
        Expr::Break(e) => {
            f.leaf(e, n)?;
        }
        Expr::Continue => {
            f.leaf(e, n)?;
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
    };
    f.exit(e, n)
}
