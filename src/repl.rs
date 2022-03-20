use crate::ast::ExprNode;
use crate::eval::Interpreter;
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

pub fn parse_file(filename: &str) -> ExprNode {
    let contents = std::fs::read_to_string(filename.clone())
        .unwrap()
        .to_string();
    let mut lexer = lexer::LexerState::default();
    let (_, _) = lexer.lex(contents.as_str()).unwrap();
    let (_, expr) = parse_program(lexer.tokens().clone()).unwrap();
    expr
}

pub fn run_file(filename: &str) -> anyhow::Result<()> {
    let contents = std::fs::read_to_string(filename.clone())?.to_string();
    let mut lexer = lexer::LexerState::default();
    let results = lexer.lex(contents.as_str());
    match results {
        Ok((_, _)) => {
            let tokens = lexer.tokens();
            /*
            match parse_program(tokens.clone()) {
                Ok((rest, prog)) => {
                    if rest.input_len() > 0 {
                        debug!("ERROR Not Parsed {:?}", rest.to_location());
                        return Ok(());
                    }
                    debug!("PROG: {:?}", prog.sexpr());
                }
                Err(e) => {
                    debug!("ERROR {:?}", e);
                    return Ok(());
                }
            }
            */

            let (maybe_prog, results) = parse_program_with_results(tokens);
            for r in results {
                match r {
                    Results::Warning(msg, line) => debug!("-W[{}] {}", line, msg),
                    Results::Error(msg, line) => debug!("*E[{}] {}", line, msg),
                }
            }

            if let Some(prog) = maybe_prog {
                debug!("{:?}", (&prog));
                match prog.sexpr() {
                    Ok(sexpr) => {
                        debug!("Ok expr {}", &sexpr);
                    }
                    Err(e) => {
                        debug!("Error sexpr {:?}", e);
                    }
                }
                let mut interp = Interpreter::default();
                interp.interpret(prog);
            }
            Ok(())
        }
        Err(e) => {
            debug!("[{}] {:?}", filename, e);
            Ok(())
            //Err(e.into())
        }
    }
}

pub fn run_prompt() -> anyhow::Result<()> {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        debug!("No previous history");
    }

    let mut interpreter = Interpreter::default();

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                run(&mut interpreter, &line);
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

pub fn run(interpreter: &mut Interpreter, source: &str) {
    let mut lexer = lexer::LexerState::default();
    match lexer.lex_eof(source) {
        Ok((_, _)) => {
            let tokens = lexer.tokens();
            let (maybe_prog, results) = parse_program_with_results(tokens);
            for r in results {
                match r {
                    Results::Warning(msg, line) => debug!("-W[{}] {}", line, msg),
                    Results::Error(msg, line) => debug!("*E[{}] {}", line, msg),
                }
            }

            if let Some(prog) = maybe_prog {
                debug!("PROG: {:?}", (&prog));
                let sexpr = prog.sexpr().unwrap();
                debug!("SEXPR: {}", &sexpr);
                match interpreter.evaluate(prog.into()) {
                    Ok(r) => debug!("R: {:?}", r),
                    Err(e) => debug!("E: {:?}", e),
                }
            }
        }
        Err(e) => {
            debug!("{:?}", e);
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn parse() {
        crate::repl::parse_file("examples/test.p");
    }
    #[test]
    fn example() {
        crate::repl::run_file("examples/test.p").unwrap()
    }
}
