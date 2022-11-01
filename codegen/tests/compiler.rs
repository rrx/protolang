#[cfg(test)]
mod tests {
    use codegen::hir;
    use codegen::llvm;
    //use test_log::test;

    /*
    #[test]
    fn codegen_function() {
        let mut b = hir::ModuleBuilder::new();
        let mut args = llvm::CompileArgs::default();
        args.stdout = true;

        let one = b.u64(1);
        let two = b.u64(2);
        let four = b.u64(4);

        let add1 = b.add(one, two);
        let add2 = b.add(four, add1.clone());

        let block1 = b.seq(vec![add1]);
        let f1 = b.function("f1", block1.clone()); 
        let f1_var: hir::Variable = f1.clone().into();

        let f2 = b.function("f2", block1); 
        let f2_var: hir::Variable = f2.clone().into();

        let call = b.apply(f1_var.into(), vec![]);
        let add3 = b.add(call, add2.clone());

        let block = b.seq(vec![add2, add3]);
        let main = b.main(block);
        let exprs = b.seq(vec![f1.into(), main.into()]);

        println!("{:?}", &exprs);

        let c = llvm::Compiler::new(args);

        let mut gen1 = c.create_module("test1");
        c.codegen(&mut gen1, exprs);

        let mut gen2 = c.create_module("test2");
        c.codegen(&mut gen2, f2.into());

        let r = c.run_jit(vec![gen1.module, gen2.module]).unwrap();
        println!("RET: {:?}", r);
    }
    */
}

