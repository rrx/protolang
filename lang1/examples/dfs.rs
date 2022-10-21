use log::debug;
use lang1::ast::{visit_expr, ExprNode, ExprVisitor, VResult};
use lang1::lexer::LexerState;
use lang1::parser::parse_program;

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
