use std::error::Error;
use protolang::lexer;
use protolang::tokens::Tokens;

fn main() -> Result<(), Box<dyn Error>> {
    for filename in std::env::args().skip(1) {
        let contents = std::fs::read_to_string(filename.clone())?;
        match lexer::lex(contents.as_str()) {
            Ok((_, v)) => {
                v.iter().for_each(|x| {
                    println!("[{}] {:?}", filename, x);
                });
                let tokens = Tokens::new(&v[..]);
            }
            Err(e) => {
                println!("[{}] {:?}", filename, e);
            }
        }
    }
    Ok(())
}
