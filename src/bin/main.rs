use std::error::Error;
use protolang::lexer;

fn main() -> Result<(), Box<dyn Error>> {
    for filename in std::env::args().skip(1) {
        let contents = std::fs::read_to_string(filename.clone())?;
        match lexer::lex(contents.as_str()) {
            Ok((i, v)) => {
                v.iter().for_each(|x| {
                    println!("[{}] {:?}", filename, x);
                });
            }
            Err(e) => {
                println!("[{}] {:?}", filename, e);
            }
        }
    }
    Ok(())
}
