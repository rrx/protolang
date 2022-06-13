use log::debug;
use protolang::ast::{visit_expr, ExprNode, ExprVisitor, VResult};
use protolang::lexer::LexerState;
use protolang::parser::parse_program;

pub struct DFS {}
impl ExprVisitor<Vec<ExprNode>> for DFS {
    fn exit(&mut self, e: &ExprNode, n: &mut Vec<ExprNode>) -> VResult {
        n.push(e.clone());
        Ok(())
    }
}

fn main() {
    let s = "1 + 2 * 3";
    let mut lexer = LexerState::default();
    let (_, tokens) = lexer.lex_eof(s).unwrap();
    let (_, expr) = parse_program(tokens).unwrap();
    let mut dfs = DFS {};
    let mut out = vec![];
    let _ = visit_expr(&expr, &mut dfs, &mut out).unwrap();
    debug!("out {:?}", (s, out));
}
