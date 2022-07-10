use crate::ast::Context;
use crate::ast::{visit_expr, ExprVisitor, VResult};
use crate::ast::{Expr, ExprNode};
use crate::tokens::Tok;

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

pub struct Unparser {
    expand: bool,
}

impl ExprVisitor<Vec<Tok>> for Unparser {
    fn enter(&mut self, e: &ExprNode, n: &mut Vec<Tok>) -> VResult {
        if self.expand {
            n.append(&mut e.context.pre());
        }
        match &e.value {
            Expr::Ident(x) => {
                n.push(Tok::Ident(x.name.clone()));
            }
            Expr::Literal(x) => {
                n.push(x.clone());
            }
            Expr::Lambda(e) => {
                n.append(&mut e.unparse());
            }
            Expr::Invalid(s) => {
                n.push(Tok::Invalid(s.clone()));
            }
            _ => (),
        };
        Ok(())
    }

    fn exit(&mut self, e: &ExprNode, n: &mut Vec<Tok>) -> VResult {
        if self.expand {
            n.append(&mut e.context.post());
        }
        Ok(())
    }
}

pub fn unparse_expr(e: &ExprNode, expand: bool) -> Vec<Tok> {
    let mut v = Unparser { expand };
    let mut out = vec![];
    let _ = visit_expr(e, &mut v, &mut out).unwrap();
    out
}
