use crate::interpreter::Interpreter;
use crate::parser::parse_program_with_results;
use crate::sexpr::SExpr;
use crate::tokens::Tokens;
use crate::results::*;
use crate::lexer;
use std::{env, io::Write, process};
use rustyline::error::ReadlineError;
use rustyline::Editor;

pub fn cli() -> anyhow::Result<()> {
    for filename in std::env::args().skip(1) {
        run_file(filename.as_str())?;
    }
    Ok(())
}

pub fn run_file(filename: &str) -> anyhow::Result<()> {
    let contents = std::fs::read_to_string(filename.clone())?.to_string();
    let results = lexer::lex(contents.as_str());
    match results {
        Ok((_, toks)) => {
            //toks.iter().for_each(|x| {
            //println!("[{}] {:?}", filename, x);
            //});
            let tokens = Tokens::new(&toks[..]);
            //println!("{:?}", (&tokens));

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
    match lexer::lex_eof(source) {
        Ok((_, toks)) => {
            let tokens = Tokens::new(&toks[..]);
            let (maybe_prog, results) = parse_program_with_results(tokens);
            for r in results {
                match r {
                    Results::Warning(msg, line) => println!("-W[{}] {}", line, msg),
                    Results::Error(msg, line) => println!("*E[{}] {}", line, msg),
                }
            }

            if let Some(prog) = maybe_prog {
                //println!("{:?}", (&prog));
                let sexpr = prog.sexpr().unwrap();
                println!("XXXsexpr {}", &sexpr);
                interpreter.interpret(prog);
            }
        }
        Err(e) => {
            println!("{:?}", e);
        }
    }
}
