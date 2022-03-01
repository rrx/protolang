use protolang::lexer;
use protolang::parser::parse_program;
use protolang::tokens::Tokens;
use std::error::Error;
use protolang::sexpr::SExpr;

fn main() -> Result<(), Box<dyn Error>> {
    for filename in std::env::args().skip(1) {
        let contents = std::fs::read_to_string(filename.clone())?;
        match lexer::lex(contents.as_str()) {
            Ok((_, toks)) => {
                toks.iter().for_each(|x| {
                    println!("[{}] {:?}", filename, x);
                });
                let tokens = Tokens::new(&toks[..]);
                println!("{:?}", (&tokens));

                let (prog_rest, prog) = parse_program(tokens).unwrap();
                if prog_rest.tok.len() > 0 {
                    println!("Unparsed Tokens: {:?}", prog_rest);
                }

                println!("{:?}", (&prog));
                let sexpr = prog.sexpr().unwrap();
                println!("sexpr {}", &sexpr);
            }
            Err(e) => {
                println!("[{}] {:?}", filename, e);
            }
        }
    }
    Ok(())
}
