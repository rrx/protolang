use super::{ExprNode, Expr};
use crate::tokens::Tok;
use std::collections::VecDeque;

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
impl ExprVisitor<Vec<ExprNode>> for DFS {
    fn exit(&mut self, e: &ExprNode, n: &mut Vec<ExprNode>) -> VResult {
        n.push(e.clone());
        //n.append(&mut unparse_expr(e, false));
        Ok(())
    }
}

pub struct BFS { queue: VecDeque<ExprNode> }
impl BFS {
    pub fn new() -> Self {
        Self { queue: VecDeque::new() }
    }

    fn run(&mut self, e: &ExprNode, n: &mut Vec<ExprNode>) -> VResult {
        self.visit(e, &mut vec![])?;
        n.append(&mut self.queue.drain(..).collect::<Vec<_>>());
        Ok(())
    }

    fn visit(&mut self, e: &ExprNode, n: &mut Vec<ExprNode>) -> VResult {
        self.queue.append(&mut e.value.children().into_iter().collect());
        for c in e.value.children() {
            self.visit(&c, n)?;
        }
        Ok(())
    }
}

impl ExprVisitor<Vec<ExprNode>> for BFS {
    fn enter(&mut self, e: &ExprNode, n: &mut Vec<ExprNode>) -> VResult {
        //self.queue.append(&mut e.value.children());
        //n.push(*e);
        Ok(())
    }
}

impl Expr {
    fn children(&self) -> Vec<ExprNode> {
        let mut out = vec![];
        match self {
            Expr::Ternary(_, x, y, z) => {
                out.push(x.clone());
                out.push(y.clone());
                out.push(z.clone());
            }
            Expr::Chain(_, _) => {
            }
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
                out.append(&mut elements.into_iter().map(|e| Box::new(e.clone())).collect::<Vec<_>>());
            }
            Expr::Callable(_) => {
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
            Expr::Block(exprs) | Expr::Program(exprs) => {
                for e in exprs {
                    out.push(Box::new(e.clone()));
                }
            }
            Expr::Ident(x) => {
            }
            Expr::Literal(x) => {
            }
            Expr::Lambda(_) => {
            }
            Expr::Invalid(_) => {
            }
            Expr::Void => {
            }
        }
        out.into_iter().map(|v| *v).collect::<Vec<_>>()
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{parse_program, unparse_expr};
    use crate::lexer::LexerState;
    use crate::sexpr::SExpr;

    #[test]
    fn dfs() {
        let s = "1 + 2 * 3";
        let mut lexer = LexerState::from_str_eof(s).unwrap();
        let tokens = lexer.tokens2().clone();
        let (_, expr) = parse_program(tokens).unwrap();
        let mut dfs = DFS {};
        let mut out = vec![];
        let _ = visit_expr(&expr, &mut dfs, &mut out).unwrap();
        println!("out {:?}", (s, out));
    }

    #[test]
    fn bfs() {
        let s = "1 + 2 * 3";
        let mut lexer = LexerState::from_str_eof(s).unwrap();
        let tokens = lexer.tokens2().clone();
        let (_, expr) = parse_program(tokens).unwrap();
        let mut bfs = BFS::new();
        let mut out = vec![];
        let r = bfs.run(&expr, &mut out).unwrap();
        println!("SEXPR: {}", expr.sexpr().unwrap());
        for v in out {
            println!("bfs {:?}", (unparse_expr(&v, true)));
        }
    }
}

