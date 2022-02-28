use protolang::ast::Value;
use protolang::lexer;
use protolang::parser::parse_program_node;
use protolang::tokens::Tokens;
use std::error::Error;

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

                let (prog_rest, node) = parse_program_node(tokens).unwrap();
                if prog_rest.tok.len() > 0 {
                    println!("Unparsed Tokens: {:?}", prog_rest);
                }

                println!("{:?}", (&node));
                match node.value {
                    Value::Program(prog) => {
                        let sexpr = prog.sexpr().unwrap();
                        sexpr.iter().for_each(|v| {
                            println!("sexpr {}", &v);
                        });
                    }
                    _ => unreachable!(),
                };
            }
            Err(e) => {
                println!("[{}] {:?}", filename, e);
            }
        }
    }
    Ok(())
}
