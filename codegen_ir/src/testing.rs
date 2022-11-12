use crate::hir::{self, *};

pub fn gen_fib(defs: &mut Definitions) -> Ast {
    /*
    function fibonacci(a, b, n) {
    if (n === 0) {
    return a;
    }

    if (n === 1) {
    return b;
    }

    return fibonacci(b, a + b, n - 1);
    }
    */

    // fib function type
    let typ = FunctionType::export(vec![Type::i64(), Type::i64(), Type::i64(), Type::i64()]);

    // variable for the function
    let fib = defs.named_variable("fib");
    // variable for the single parameter in the function
    let a = defs.named_variable("a");
    let b = defs.named_variable("b");
    let n = defs.named_variable("n");

    let eq0 = eq(n.clone().into(), hir::i64(0));

    let ret_a: Ast = hir::new_return(a.clone().into());
    let branch1 = hir::new_condition(
        eq0.into(),
        ret_a,
        None,
        Type::Primitive(PrimitiveType::Unit),
    );

    let eq1 = hir::eq(n.clone().into(), hir::i64(1));
    let ret_b: Ast = Return::new(b.clone().into()).into();
    let branch2 = hir::new_condition(
        eq1.into(),
        ret_b,
        None,
        Type::Primitive(PrimitiveType::Unit),
    );

    // call the function that we've created and then increment
    let call = hir::new_call(
        fib.clone().into(),
        vec![
            b.clone().into(),
            hir::add(a.clone().into(), b.clone().into()),
            hir::sub(n.clone().into(), hir::i64(1)),
        ],
        typ.clone(),
    );

    let block = Sequence::new(vec![branch1.into(), branch2.into(), call.into()]);
    // create a function to associate with the variable
    let f = hir::new_lambda(
        vec![a.clone(), b.clone(), n.clone()],
        block.into(),
        typ.clone(),
    );

    // define the function using the definition id
    let dfib = Definition::variable(fib.clone(), f.into());

    let call = hir::new_call(
        fib.clone().into(),
        vec![hir::i64(0), hir::i64(1), hir::i64(10)],
        typ.clone(),
    );

    // no parameters on main function
    let typ = FunctionType::export(vec![Type::i64()]);
    let f_main = hir::new_lambda(vec![], call.into(), typ.clone());
    let df_main = defs.new_definition("main", f_main.into());
    Sequence::new(vec![dfib.into(), df_main.into()]).into()
}

pub fn gen_x1_module(defs: &mut Definitions) -> Ast {
    // single parameter function type
    let typ = FunctionType::export(vec![Type::i64(), Type::i64()]);

    // x1(x) => x+1
    // increment by 1
    let p = defs.new_variable();
    let x1 = hir::new_lambda(
        vec![p.clone()],
        hir::add(p.clone().into(), hir::i64(1)),
        typ.clone(),
    );
    let dx1 = defs.new_definition("x1", x1.into());
    dx1.into()
}

pub fn gen_x1_main(defs: &mut Definitions) -> Ast {
    // single parameter function type
    let typ = FunctionType::export(vec![Type::i64(), Type::i64()]);

    // main function
    let extern1 = Extern::new("x1".to_string(), typ.clone().into());
    // call extern
    let call_extern = hir::new_call(extern1.clone().into(), vec![hir::i64(10)], typ.clone());
    let f_main = Lambda::new(vec![], call_extern.into(), typ.clone());

    let df_main = defs.new_definition("main", f_main.into());
    df_main.into()
}

pub fn gen_main_simple(defs: &mut Definitions) -> Ast {
    // x1(x) => x+1
    // increment by 1
    let typ = FunctionType::export(vec![Type::i64(), Type::i64()]);
    let p = defs.named_variable("a");
    let x1 = hir::new_lambda(
        vec![p.clone()],
        hir::add(p.clone().into(), hir::i64(1)),
        typ.clone(),
    );
    let dx1 = defs.new_definition("x1", x1.into());
    //let x1 = gen_x1_module(defs);
    let v = dx1.to_variable();
    let call_x1 = hir::new_call(v.into(), vec![hir::i64(5)], typ.clone());

    // 0-arity
    let typ = FunctionType::export(vec![Type::i64()]);
    // main function
    let f_main = Lambda::new(vec![], hir::i64(10).into(), typ.clone());
    let add = hir::add(hir::i64(4), call_x1);
    let df_main = defs.new_definition("main", add.into());
    let v = df_main.to_variable();
    let c = hir::new_call(v.into(), vec![], typ);
    hir::append(hir::append(dx1.into(), df_main.into()), c.into())
}

pub fn gen_self_reference(defs: &mut Definitions) -> Ast {
    let typ = FunctionType::export(vec![Type::i64()]);
    let a = defs.named_variable("a");
    let b = defs.named_variable("main");

    let call_a = hir::new_call(a.clone().into(), vec![], typ.clone());
    let call_b = hir::new_call(b.clone().into(), vec![], typ.clone());

    let f_a = hir::new_lambda(vec![], call_a, typ.clone());
    let f_b = hir::new_lambda(vec![], call_b, typ.clone());

    append(
        hir::definition_from_variable(&b, f_a).into(),
        hir::definition_from_variable(&a, f_b).into(),
    )
}

pub fn gen_extern(defs: &mut Definitions) -> Ast {
    let typ = FunctionType::export(vec![Type::i64()]);
    
    let ext = hir::new_extern_function("testfunction", typ.clone());

    let b = defs.named_variable("main");

    let call = hir::new_call(ext.into(), vec![], typ.clone());

    let f_main = hir::new_lambda(vec![], call, typ.clone());

    hir::definition_from_variable(&b, f_main).into()
}

