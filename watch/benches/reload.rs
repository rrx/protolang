use codegen_ir::hir::*;
use codegen_ir::testing::*;
use codegen_llvm::*;
use criterion::{criterion_group, criterion_main, Criterion};
use watch::*;

fn link(c: &mut Criterion) {
    let context = Context::create();
    let mut e = Runner::create(&context, OptimizationLevel::None, 0).unwrap();
    let mut defs = Definitions::new();

    let mut count = 0;

    let x1_main = gen_x1_main(&mut defs);
    e.compile_ast("main", &x1_main).unwrap();

    c.bench_function("link", |b| {
        b.iter(|| {
            // compile the module and link it
            let x1_module = gen_x1_module(&mut defs, count);
            e.compile_ast("test", &x1_module).unwrap();
            let version = e.link().unwrap();
            //e.debug();

            // invoke the new function
            let ret: i64 = version.invoke("main", ()).unwrap();
            //log::debug!("out: {}, {}", count, ret);
            assert_eq!(count + 10, ret);
            count += 1;
        })
    });
}

criterion_group!(benches, link);

criterion_main!(benches);
