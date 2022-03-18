use criterion::{black_box, criterion_group, criterion_main, Criterion};
use protolang::lexer;
use protolang::ast::unparse_expr;
use protolang::parser::parse_program;
use protolang::repl::parse_file;
use protolang::interpreter::Interpreter;

const filename: &str = "benches/test.p";

fn parse(c: &mut Criterion) {
    let contents = std::fs::read_to_string(filename.clone()).unwrap().to_string();
    let mut lexer = lexer::LexerState::default();
    let (_,_) = lexer.lex(contents.as_str()).unwrap();
    let tokens = lexer.tokens().clone();
    c.bench_function("parse", |b| b.iter(|| {
        let (_, expr) = parse_program(tokens.clone()).unwrap();
        unparse_expr(&expr);
    }));
}

fn interpret(c: &mut Criterion) {
    let contents = std::fs::read_to_string(filename.clone()).unwrap().to_string();
    let mut lexer = lexer::LexerState::default();
    let (_,_) = lexer.lex(contents.as_str()).unwrap();
    let tokens = lexer.tokens().clone();
    let (_, expr) = parse_program(tokens.clone()).unwrap();
    unparse_expr(&expr);
    c.bench_function("interpret", |b| b.iter(|| {
        let mut interp = Interpreter::default();
        interp.interpret(expr.clone());
    }));
}

criterion_group!(benches, parse);


criterion_main!(benches);
