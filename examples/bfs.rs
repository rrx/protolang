use log::debug;
use protolang::ast::{ExprNode, ExprVisitor, VResult};
use protolang::lexer::LexerState;
use protolang::parser::{parse_program, unparse_expr};
use protolang::sexpr::SExpr;
use std::collections::VecDeque;

pub struct BFS {
    queue: VecDeque<ExprNode>,
}
impl BFS {
    pub fn new() -> Self {
        Self {
            queue: VecDeque::new(),
        }
    }

    fn run(&mut self, e: &ExprNode, n: &mut Vec<ExprNode>) -> VResult {
        self.visit(e, &mut vec![])?;
        n.append(&mut self.queue.drain(..).collect::<Vec<_>>());
        Ok(())
    }

    fn visit(&mut self, e: &ExprNode, n: &mut Vec<ExprNode>) -> VResult {
        self.queue
            .append(&mut e.value.children().into_iter().collect());
        for c in e.value.children() {
            self.visit(&c, n)?;
        }
        Ok(())
    }
}

impl ExprVisitor<Vec<ExprNode>> for BFS {
    fn enter(&mut self, _: &ExprNode, _: &mut Vec<ExprNode>) -> VResult {
        //self.queue.append(&mut e.value.children());
        //n.push(*e);
        Ok(())
    }
}

fn main() {
    let s = "1 + 2 * 3";
    let mut lexer = LexerState::from_str_eof(s).unwrap();
    let tokens = lexer.tokens2().clone();
    let (_, expr) = parse_program(tokens).unwrap();
    let mut bfs = BFS::new();
    let mut out = vec![];
    let _ = bfs.run(&expr, &mut out).unwrap();
    debug!("SEXPR: {}", expr.sexpr().unwrap());
    for v in out {
        debug!("bfs {:?}", (unparse_expr(&v, true)));
    }
}