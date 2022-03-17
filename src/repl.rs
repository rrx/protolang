use crate::interpreter::Interpreter;
use crate::lexer;
use crate::parser::{parse_program, parse_program_with_results};
use crate::results::*;
use crate::sexpr::SExpr;
use nom::InputLength;
use rustyline::error::ReadlineError;
use rustyline::Editor;
use std::{env, io::Write, process};

pub fn cli() -> anyhow::Result<()> {
    for filename in std::env::args().skip(1) {
        run_file(filename.as_str())?;
    }
    Ok(())
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
                        println!("ERROR Not Parsed {:?}", rest.to_location());
                        return Ok(());
                    }
                    println!("PROG: {:?}", prog.sexpr());
                }
                Err(e) => {
                    println!("ERROR {:?}", e);
                    return Ok(());
                }
            }
            */

            let (maybe_prog, results) = parse_program_with_results(tokens);
            for r in results {
                match r {
                    Results::Warning(msg, line) => println!("-W[{}] {}", line, msg),
                    Results::Error(msg, line) => println!("*E[{}] {}", line, msg),
                }
            }

            if let Some(prog) = maybe_prog {
                //println!("{:?}", (&prog));
                match prog.sexpr() {
                    Ok(sexpr) => {
                        println!("Ok expr {}", &sexpr);
                    }
                    Err(e) => {
                        println!("Error sexpr {:?}", e);
                    }
                }
                let mut interp = Interpreter::default();
                interp.interpret(prog);
            }
            Ok(())
        }
        Err(e) => {
            println!("[{}] {:?}", filename, e);
            Ok(())
            //Err(e.into())
        }
    }
}

pub fn run_prompt() -> anyhow::Result<()> {
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history");
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
                println!("CTRL-C");
                break;
            }

            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }

            Err(err) => {
                println!("Error: {:?}", err);
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
                    Results::Warning(msg, line) => println!("-W[{}] {}", line, msg),
                    Results::Error(msg, line) => println!("*E[{}] {}", line, msg),
                }
            }

            if let Some(prog) = maybe_prog {
                println!("PROG: {:?}", (&prog));
                let sexpr = prog.sexpr().unwrap();
                println!("SEXPR: {}", &sexpr);
                match interpreter.evaluate(&prog.value) {
                    Ok(r) => println!("R: {:?}", r),
                    Err(e) => println!("E: {:?}", e),
                }
            }
        }
        Err(e) => {
            println!("{:?}", e);
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn example() {
        crate::repl::run_file("examples/test.p").unwrap()
    }
}
