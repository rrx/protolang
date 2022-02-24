use std::error::Error;
use protolang::lexer;

fn main() -> Result<(), Box<dyn Error>> {
    for filename in std::env::args().skip(1) {
        let contents = std::fs::read_to_string(filename.clone())?;
        let result = lexer::lex(contents.as_str());
        println!("[{}] {:?}", filename, result);
    }
    Ok(())
}
