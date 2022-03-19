use criterion::{black_box, criterion_group, criterion_main, Criterion};
use protolang::eval::Interpreter;
use protolang::lexer;
use protolang::parser::{unparse_expr, parse_program, Unparse};
//use protolang::repl::parse_file;

const FILENAME: &str = "benches/test.p";

fn parse(c: &mut Criterion) {
    let contents = std::fs::read_to_string(FILENAME.clone())
        .unwrap()
        .to_string();
    let mut lexer = lexer::LexerState::default();
    let (_, _) = lexer.lex(contents.as_str()).unwrap();
    let tokens = lexer.tokens().clone();

    c.bench_function("parse", |b| {
        b.iter(|| {
            parse_program(tokens.clone()).unwrap();
        })
    });

    let (_, expr) = parse_program(tokens.clone()).unwrap();
    c.bench_function("unparse1", |b| {
        b.iter(|| {
            unparse_expr(&expr, true);
        })
    });

    c.bench_function("unparse2", |b| {
        b.iter(|| {
            expr.unparse();
        })
    });
}

fn interpret(c: &mut Criterion) {
    let contents = std::fs::read_to_string(FILENAME.clone())
        .unwrap()
        .to_string();
    let mut lexer = lexer::LexerState::default();
    let (_, _) = lexer.lex(contents.as_str()).unwrap();
    let tokens = lexer.tokens().clone();
    let (_, expr) = parse_program(tokens.clone()).unwrap();
    unparse_expr(&expr, true);
    c.bench_function("interpret", |b| {
        b.iter(|| {
            let mut interp = Interpreter::default();
            interp.interpret(expr.clone());
        })
    });
}

criterion_group!(benches, parse, interpret);

criterion_main!(benches);
