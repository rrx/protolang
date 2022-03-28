use protolang::repl::*;
use std::error::Error;
use protolang::program::Program;
use protolang::eval::Environment;

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();
    let mut program = Program::new();
    let mut env = Environment::default();
    for filename in std::env::args().skip(1) {
        env = program.analyze_file(filename.as_str(), env.clone());
        env.debug();
    }
    program.print();
    program.diagnostics.clear();
    Ok(())
}

