use crate::eval::Environment;
use crate::parser::Unparse;
use crate::program::Program;
use log::debug;
use rustyline::error::ReadlineError;
use rustyline::Editor;

pub fn cli() -> anyhow::Result<()> {
    let mut program = Program::new();
    let mut env = Environment::default();
    for filename in std::env::args().skip(1) {
        let r = program.eval_file(filename.as_str(), env)?;
        env = r.env;
        println!("> {:?}", r.expr);
    }
    Ok(())
}

pub fn repl() -> anyhow::Result<()> {
    let mut program = Program::new();

    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        debug!("No previous history");
    }

    let mut env = Environment::default();

    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                match program.eval(&line, env.clone()) {
                    Ok(r) => {
                        println!("> {}", r.expr.borrow().unlex());
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
