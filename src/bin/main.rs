use protolang::{
    results::*,
    lexer,
    parser::{parse_program_with_results},
    //sexpr::SExpr,
    interpreter::Interpreter,
    tokens::Tokens,
};
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    for filename in std::env::args().skip(1) {
        let contents = std::fs::read_to_string(filename.clone())?;
        match lexer::lex(contents.as_str()) {
            Ok((_, toks)) => {
                //toks.iter().for_each(|x| {
                    //println!("[{}] {:?}", filename, x);
                //});
                let tokens = Tokens::new(&toks[..]);
                //println!("{:?}", (&tokens));

                let (maybe_prog, results) = parse_program_with_results(tokens);
                for r in results {
                    match r {
                        Results::Warning(msg, line) => println!("-W[{}] {}", line, msg),
                        Results::Error(msg, line) => println!("*E[{}] {}", line, msg),
                    }
                }

                if let Some(prog) = maybe_prog {
                    //println!("{:?}", (&prog));
                    //let sexpr = prog.sexpr().unwrap();
                    //println!("sexpr {}", &sexpr);
                    let mut interp = Interpreter::default();
                    interp.interpret(prog);
                }

            }
            Err(e) => {
                println!("[{}] {:?}", filename, e);
            }
        }
    }
    Ok(())
}
