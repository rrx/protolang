use crate::ast::ExprNode;
use crate::eval::{Environment, ExprRefWithEnv, InterpretError, Interpreter};
use crate::lexer;
use crate::parser::{parse_program, parse_program_with_results};
use crate::results::*;
use crate::sexpr::SExpr;
use log::debug;
use rustyline::error::ReadlineError;
use rustyline::Editor;

pub fn cli() -> anyhow::Result<()> {
    for filename in std::env::args().skip(1) {
        run_file(filename.as_str())?;
    }
    Ok(())
}

pub fn parse_file(filename: &str) -> anyhow::Result<ExprNode> {
    let contents = std::fs::read_to_string(filename.clone())
        .unwrap()
        .to_string();
    let mut lexer = lexer::LexerState::default();
    let (_, _) = lexer.lex(contents.as_str()).unwrap();
    let (_, expr) = parse_program(lexer.tokens().clone()).unwrap();
    Ok(expr)
}

pub fn run_file(filename: &str) -> anyhow::Result<()> {
    let contents = std::fs::read_to_string(filename.clone())?.to_string();
    let mut interpreter = Interpreter::default();
    let env = Environment::default();
    match run(
        &mut interpreter,
        env.clone(),
        filename.to_string(),
        &contents,
    ) {
        Ok(r) => {
            r.env.debug();
            println!("> {:?}", r.expr);
            Ok(())
        }
        Err(e) => Err(e),
    }
}

pub fn run_prompt() -> anyhow::Result<()> {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        debug!("No previous history");
    }

    let mut interpreter = Interpreter::default();
    let mut env = Environment::default();

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match run(&mut interpreter, env.clone(), "<repl>".into(), &line) {
                    Ok(r) => {
                        println!("> {:?}", r.expr);
                        env = r.env;
                    }
                    _ => (),
                }
            }

            Err(ReadlineError::Interrupted) => {
                debug!("CTRL-C");
                break;
            }

            Err(ReadlineError::Eof) => {
                debug!("CTRL-D");
                break;
            }

            Err(err) => {
                debug!("Error: {:?}", err);
                break;
            }
        }
    }

    Ok(())
}

pub fn run(
    interpreter: &mut Interpreter,
    mut env: Environment,
    filename: String,
    source: &str,
) -> anyhow::Result<ExprRefWithEnv> {
    let mut lexer = lexer::LexerState::default();

    let mut results = Results::new();
    let file_id = results.add_source(filename, source.into());

    match lexer.lex_eof(source) {
        Ok((_, _)) => {
            let tokens = lexer.tokens();
            let (maybe_prog, returns) = parse_program_with_results("repl".to_string(), tokens);
            for r in returns {
                results.push(r.diagnostic(file_id));
            }

            if let Some(prog) = maybe_prog {
                prog.debug();
                let sexpr = prog.sexpr().unwrap();
                debug!("SEXPR: {}", &sexpr);
                match interpreter.evaluate(prog.into(), env) {
                    Ok(r) => {
                        results.print();
                        Ok(r)
                    }
                    Err(e) => {
                        results.push(e.diagnostic(file_id));
                        results.print();
                        Err(e.into())
                    }
                }
            } else {
                debug!("unable to parse");
                Err(InterpretError::runtime(&format!("Unable to parse")).into())
            }
        }
        Err(e) => {
            debug!("{:?}", e);
            Err(InterpretError::runtime(&format!("Unable to lex")).into())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;
    #[test]
    fn parse() {
        parse_file("examples/test.p");
    }

    #[test]
    fn example() {
        run_file("examples/test.p").unwrap()
    }

    #[test]
    fn test() {
        let mut interp = Interpreter::default();
        let env = Environment::default();
        let r = run(&mut interp, env, "".into(), "let a=1").unwrap();
        r.env.debug();
        assert!(r.env.resolve("a").is_some());
    }
}
