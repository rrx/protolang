use protolang::{
    sexpr::SExpr,
    interpreter::Interpreter,
    lexer,
    parser::parse_program_with_results,
    results::*,
    repl::*,
    tokens::Tokens,
};
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    if std::env::args().len() <= 1 {
        run_prompt()?;
    } else {
        for filename in std::env::args().skip(1) {
            if let Err(e) = run_file(&filename) {
                println!("[{}] {:?}", filename, e);
            }
        }
    }
    Ok(())
}
