use protolang::repl::*;
use std::error::Error;
use protolang::ir::{base_env, TypeChecker};
fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();
    let args = std::env::args().skip(1).collect::<Vec<_>>();
    if args.len() == 0 {
        repl()?;
    } else {
        let cmd = args.get(0).unwrap().as_str();
        println!("cmd: {}", cmd);
        match cmd {
            "repl" => {
                repl()?;
            }
            "exec" => {
                cli(args.iter().skip(2).cloned().collect::<Vec<_>>())?;
            }
            "analyze" => {
            }
            "check" => {
                let env = base_env();
                let mut c = TypeChecker::default();
                for filename in args.iter().skip(1) {
                    println!("filename: {}", filename);
                    let mut ir = c.parse_file(filename, env.clone()).unwrap();
                    //let s = c.assign_typenames(&mut ir, s);
                    //c.generate_equations(&ir);
                    //println!("{}", p);
                    println!("{}", ir);
                    //println!("{:?}", s);
                }
                for e in &c.type_equations {
                    println!("E: {}", e);
                }
                let s = c.unify_all().unwrap();
                for x in &s {
                    println!("subst: {:?}", x);
                }
                println!("has_errors: {}", c.results.has_errors);
                c.results.print();
            }
            _ => unimplemented!()
        }
    }
    Ok(())
}
