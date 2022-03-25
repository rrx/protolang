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

pub fn run<'a>(
    interpreter: &mut Interpreter<'a>,
    env: Environment<'a>,
    filename: String,
    source: &str,
) -> anyhow::Result<ExprRefWithEnv<'a>> {
    let env = Environment::default();
    let mut interp = Interpreter::default();
    let r = interp.eval(source, env)?;
    Ok(r.value)
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;
    #[test]
    fn parse() {
        let _ = parse_file("examples/test.p").unwrap();
    }

    #[test]
    fn example() {
        run_file("examples/test.p").unwrap()
    }

    #[test]
    fn test() {
        let mut interp = Interpreter::default();
        {
            let env = Environment::default();
            let r = run(&mut interp, env, "".into(), "let a=1").unwrap();
            r.env.debug();
            assert!(r.env.resolve("a").is_some());
        }
    }
}
