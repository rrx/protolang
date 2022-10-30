#[cfg(test)]
mod tests {
    use codegen::hir;
    use codegen::llvm;
    //use test_log::test;

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
        let f1 = b.function("f1", block1); 

        //let call = b.apply(f1.clone(), vec![]);
        //let add3 = b.add(call, add2);

        let block = b.seq(vec![add2]);
        let main = b.main(block);
        let exprs = b.seq(vec![f1, main]);

        println!("{:?}", &exprs);

        let c = llvm::Compiler::new(args);
        let m = c.module("test", exprs);
        let r = c.run_jit(vec![m]).unwrap();
        println!("RET: {:?}", r);
    }
}

