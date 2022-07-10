use criterion::{criterion_group, criterion_main, Criterion};
use protolang::eval::{Environment, Interpreter};
use protolang::lexer;
use protolang::parser::{parse_program, unparse_expr, Unparse};

const FILENAME: &str = "benches/test.p";

fn parse(c: &mut Criterion) {
    let contents = std::fs::read_to_string(FILENAME.clone())
        .unwrap()
        .to_string();

    c.bench_function("lexer", |b| {
        b.iter(|| {
            let mut lexer = lexer::LexerState::default();
            let (_, _) = lexer.lex(contents.as_str()).unwrap();
        })
    });

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
            let env = Environment::default();
            let _ = Interpreter::evaluate(expr.clone().into(), env).unwrap();
        })
    });
}

criterion_group!(benches, parse, interpret);

criterion_main!(benches);
