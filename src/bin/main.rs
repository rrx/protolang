use protolang::repl::*;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();
    if std::env::args().len() <= 1 {
        run_prompt()?;
    } else {
        for filename in std::env::args().skip(1) {
            if let Err(e) = run_file(&filename) {
                log::info!("[{}] {:?}", filename, e);
            }
        }
    }
    Ok(())
}
