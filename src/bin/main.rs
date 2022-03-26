use protolang::repl::*;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();
    if std::env::args().len() <= 1 {
        repl()?;
    } else {
        cli()?;
    }
    Ok(())
}
