use codegen_ir::hir::*;
use codegen_ir::testing::*;
use codegen_llvm::*;
use std::error::Error;
use watch::*;

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();
    let context = Context::create();
    let mut e = Runner::create(&context, OptimizationLevel::None, 0)?;
    let mut defs = Definitions::new();

    let mut count = 0;

    let x1_main = gen_x1_main(&mut defs);
    e.compile_ast("main", &x1_main)?;

    loop {
        // compile the module and link it
        let x1_module = gen_x1_module(&mut defs, count);
        e.compile_ast("test", &x1_module)?;
        let version = e.link()?;
        e.debug();

        // invoke the new function
        let ret: i64 = version.invoke("main", ())?;
        eprintln!("out: {}, {}", count, ret);
        assert_eq!(count + 10, ret);
        count += 1;
        if count > 1000 {
            break;
        }
    }

    Ok(())
}
