use lang1::eval::Environment;
use lang1::program::Program;
use lang1::repl::*;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();
    let mut program = Program::new();
    let mut env = Environment::default();
    for filename in std::env::args().skip(1) {
        env = program.analyze_file(filename.as_str(), env.clone());
        env.debug();
    }
    program.print();
    program.clear();
    Ok(())
}
