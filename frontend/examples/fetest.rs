use frontend::syntax::AstModule;
use frontend::syntax::Dialect;
use lang3::AstBuilder;

use std::env;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();
    let dialect = Dialect::Extended;
    for arg in env::args().skip(1) {
        let path = std::path::Path::new(&arg);
        let module = AstModule::parse_file(&path, &dialect)?;
        println!("{:#?}", module);
        println!("{:?}", module);

        let mut builder = AstBuilder::default();
        let ast = module.lower(&mut builder)?;
        println!("AST: {}", ast.to_ron()?);

        let ret = builder.run_jit_main(&ast)?;
        println!("Ret {}", &ret);
    }
    Ok(())
}
