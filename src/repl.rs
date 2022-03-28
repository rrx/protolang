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
        match program.eval_file(filename.as_str(), env.clone()) {
            Ok(r) => {
                env = r.env;
                program.print();
                println!("> {:?}", r.expr);
            }
            Err(_) => {
                program.print();
                println!("> {:?}", &program.value.expr);
                break;
            }
        }
        program.diagnostics.clear();
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
                        program.print();
                        println!("> {}", r.expr.borrow().unlex());
                        env = r.env;
                    }
                    _ => {
                        program.print();
                    }
                }
                program.diagnostics.clear();
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
