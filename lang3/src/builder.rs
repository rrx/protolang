use crate::*;
use codegen_ir::hir;
use codegen_llvm::JitExecute;
use logic::{UnifyType, UnifyValue};
use std::error::Error;

pub trait Lower {
    fn lower(&self, b: &mut AstBuilder) -> Result<Ast, Box<dyn Error>>;
}

#[derive(Default)]
pub struct AstBuilder {
    last_id: usize,
    pub equations: ExprSeq,
    base: Vec<Ast>,
}

impl Visitor<SymbolTable> for AstBuilder {
    fn enter(&mut self, _: &Ast, _: &mut SymbolTable) -> visitor::VResult {
        //println!("Visit AST: {}", e);
        Ok(())
    }
}

pub fn lower_variable(var: &Variable) -> hir::Variable {
    hir::Variable {
        definition_id: hir::DefinitionId(var.id.0),
        name: Some(var.name.clone()),
    }
}

impl AstBuilder {
    pub fn type_next_id(&mut self) -> DefinitionId {
        let d = DefinitionId(self.last_id);
        self.last_id += 1;
        d
    }

    pub fn var_next_id(&mut self) -> DefinitionId {
        let d = DefinitionId(self.last_id);
        self.last_id += 1;
        d
    }

    pub fn int(&self, i: u64) -> Ast {
        Ast::Literal(Literal::Int(i))
    }

    pub fn float(&self, f: f64) -> Ast {
        Ast::Literal(Literal::Float(f))
    }

    pub fn boolean(&self, b: bool) -> Ast {
        Ast::Literal(Literal::Bool(b))
    }

    pub fn type_unknown(&mut self) -> Type {
        Type::Variable(self.type_next_id())
    }

    pub fn var_unnamed(&mut self, ty: Type) -> Variable {
        Variable::unnamed(self.var_next_id(), ty)
    }

    pub fn var_named(&mut self, n: &str, ty: Type) -> Variable {
        Variable::named(n.to_string(), self.var_next_id(), ty)
    }

    pub fn node_unnamed(&mut self, ty: Type) -> Ast {
        let v = self.var_unnamed(ty.clone());
        v.into()
    }

    pub fn node_named(&mut self, n: &str, ty: Type) -> Ast {
        self.var_named(n, ty).into()
    }

    pub fn var_resolve(&mut self, n: &str, env: Environment) -> Ast {
        match env.resolve(&n.to_string()) {
            Some(v) => v.clone(),
            None => {
                eprintln!("Unable to find symbol: {}", n);
                unimplemented!();
            }
        }
    }

    pub fn block(&self, exprs: Vec<Ast>) -> Ast {
        Ast::block(exprs)
    }

    pub fn func(&mut self, params: Vec<Variable>, body: Ast, sig: Vec<Type>) -> Ast {
        Ast::Function {
            params,
            body: body.into(),
            ty: Type::Func(sig),
        }
    }

    pub fn apply(&mut self, var: &Variable, args: Vec<Ast>) -> Ast {
        Ast::Apply(var.clone(), args)
    }

    pub fn apply_name(&mut self, name: &str, args: Vec<Ast>) -> Ast {
        let mut arg_types = args.iter().map(|a| a.get_type()).collect::<Vec<_>>();
        let ret_ty = self.type_unknown();
        arg_types.push(ret_ty);
        let ty = Type::Func(arg_types);
        let var = self.var_named(name, ty);
        Ast::Apply(var, args)
    }

    pub fn binary(&mut self, name: &str, lhs: Ast, rhs: Ast) -> Ast {
        let ty_ret = self.type_unknown();
        let ty_f = Type::Func(vec![lhs.get_type(), rhs.get_type(), ty_ret]);
        let var = self.var_named(name.into(), ty_f);
        let args = vec![lhs, rhs];
        Ast::Apply(var, args)
    }

    fn name_resolve(&mut self, ast: &Ast, mut env: Environment) -> (Ast, Environment) {
        match ast {
            Ast::Literal(_) => (ast.clone(), env),
            Ast::Extern(_) => (ast.clone(), env),

            Ast::Assign(var, rhs) => {
                let name = var.name.clone();
                match env.resolve(&name) {
                    Some(v) => {
                        let (_, env) = self.name_resolve(rhs, env.clone());
                        (v.clone(), env)
                    }
                    None => {
                        //eprintln!("Unable to resolve: {:?}", &var);
                        // name doesn't resolve, so we can't assign, declare instead
                        // We could make this configurable behavior.  By default, we may not
                        // want to declare new variables, to prevent shadowing.  Python does this
                        // but it's not necessarily a good thing.
                        self.name_resolve(&Ast::Declare(var.clone(), rhs.clone()), env)
                    }
                }
            }

            Ast::Declare(var, rhs) => {
                let name = var.name.clone();

                // define an empty name, so that the body can refer to itself
                // the type needs to be correct here
                let d = Ast::Declare(var.clone(), rhs.clone());
                let mut temporary_env = env.clone();
                temporary_env.define(name.clone(), d.clone());

                // resolve rhs, ignore resulting env
                let (new_rhs, _) = self.name_resolve(rhs, temporary_env);

                // redeclare, on the original env
                let d = Ast::Declare(var.clone(), new_rhs.into());
                env.define(name, d.clone());
                (d, env)
            }

            Ast::Variable(ref v) => {
                // resolve the name from the environment
                match env.resolve(&v.name) {
                    Some(resolved_v) => {
                        let eq = logic::Expr::EqValue(ast.clone(), resolved_v.clone());
                        log::debug!("searching for var: {}, {:?}", &v.name, &eq);
                        self.equations.push(eq);
                        let var_ty = v.ty.clone();
                        let resolved_ty = resolved_v.get_type();

                        // variable matches the type of resolve
                        if var_ty.try_unknown().is_some() || resolved_ty.try_unknown().is_some() {
                            let eq = logic::Expr::Eq(var_ty, resolved_ty);
                            self.equations.push(eq);
                        }
                    }
                    None => {
                        unimplemented!("unresolved variable: {} {:?}", &v.name, &v);
                    }
                }

                (ast.clone(), env)
            }

            Ast::Apply(var, ref args) => {
                let mut env = env.clone();

                log::debug!("searching for function: {} for var {}", &var.name, &var.id);
                // get the list of possible matches
                // so we an resolve this as part of unification
                let possible = env
                    .resolve_all(&var.name)
                    .into_iter()
                    .cloned()
                    .collect::<Vec<_>>();

                if possible.len() == 0 {
                    // unable to find anything in the environment matching the name
                    //log::debug!("no match: {}", &env);
                    env.debug();
                    unreachable!("No match for {} in environment", &var.name)
                }

                self.equations
                    .push(logic::Expr::OneOfValues(var.clone().into(), possible));

                // resolve the args, which can be entire expressions
                for arg in args {
                    let (_f, this_env) = self.name_resolve(arg, env.clone());
                    env = this_env;
                }

                (Ast::Apply(var.clone(), args.clone()), env)
            }

            Ast::Function { params, body, ty } => {
                let mut local_env = env.clone();
                // The parameters on a function will be unbound, and don't need name resolution
                // When we resolve the body, we want the parameters to exist in the local environment
                let mut sig = vec![];
                for arg in params {
                    let name = arg.name.clone();
                    let arg = arg.clone();
                    sig.push(arg.ty.clone());
                    // declare variables as parameters, so we know they are unbound
                    local_env.define(name, Ast::Parameter(arg.into()));
                }
                let (body, _local_env) = self.name_resolve(body, local_env);

                // type should match the return type of the body
                sig.push(body.get_type());

                // make sure parameters are unified
                let internal_ty = Type::Func(sig);
                self.equations
                    .push(logic::Expr::Eq(ty.clone(), internal_ty));

                (
                    Ast::Function {
                        params: params.clone(),
                        body: body.into(),
                        ty: ty.clone(),
                    },
                    env,
                )
            }

            // Block is a lexical scope
            Ast::Block(exprs) => {
                let mut local_env = env.clone();
                let mut updated = vec![];
                for expr in exprs {
                    let (new_ast, new_env) = self.name_resolve(expr, local_env);
                    local_env = new_env;
                    updated.push(new_ast);
                }
                // return original scope
                (Ast::block(updated), env)
            }

            // Sequence is not a lexical scope
            Ast::Sequence(exprs) => {
                let mut updated = vec![];
                for expr in exprs {
                    let (new_ast, new_env) = self.name_resolve(expr, env);
                    env = new_env;
                    updated.push(new_ast);
                }
                (Ast::block(updated), env)
            }

            Ast::Internal(v) => {
                // we should build a new Internal, but that's currently hard, and needs to be done
                // for every variant
                let mut env = env.clone();
                for expr in v.children() {
                    let (_, new_env) = self.name_resolve(expr, env.clone());
                    env = new_env;
                }
                (Ast::Internal(v.clone()), env)
            }

            Ast::Return(expr) => self.name_resolve(expr, env),

            Ast::Condition(condition, istrue, isfalse) => {
                let (condition, env) = self.name_resolve(condition, env);

                // add an equation if condition is unknown
                let ty = condition.get_type();
                if let Some(ty_id) = ty.try_unknown() {
                    self.equations.push(logic::Expr::Eq(ty, Type::Bool));
                }

                let (istrue, env) = self.name_resolve(istrue, env);
                let (isfalse, env) = if let Some(v) = isfalse {
                    let ty = v.get_type();
                    if let Some(ty_id) = ty.try_unknown() {
                        self.equations.push(logic::Expr::Eq(ty, istrue.get_type()));
                    }
                    (
                        Some(self.name_resolve(&v, env.clone()).0.clone().into()),
                        env,
                    )
                } else {
                    (None, env)
                };
                (
                    Ast::Condition(condition.into(), istrue.clone().into(), isfalse),
                    env,
                )
            }
            _ => {
                unimplemented!("{:?}", &ast)
            }
        }
    }

    pub fn declare(&mut self, name: &str, rhs: Ast) -> Ast {
        let ty = rhs.get_type();
        let v = self.var_named(name, ty.clone());
        Ast::Declare(v, rhs.into())
    }

    pub fn assign(&mut self, name: &str, rhs: Ast) -> Ast {
        let ty = rhs.get_type();
        let v = self.var_named(name, ty.clone());
        Ast::Assign(v, rhs.into())
    }

    pub fn resolve_ast_with_base(
        &mut self,
        ast: &Ast,
    ) -> Result<(Ast, Environment, SymbolTable), Box<dyn Error>> {
        let env = self.base_env();
        let mut exprs = self.base.clone();
        exprs.push(ast.clone());
        self.resolve(&Ast::Sequence(exprs), env)
    }

    fn unify(eqs: ExprSeq) -> (UnifyResult, SymbolTable) {
        logic::Unify::<DefinitionId, Type, Ast>::start(eqs)
    }

    pub fn resolve(
        &mut self,
        ast: &Ast,
        env: Environment,
    ) -> Result<(Ast, Environment, SymbolTable), Box<dyn Error>> {
        // name resolution
        let (ast, env) = self.name_resolve(ast, env);

        // infer all types
        let eqs = self.equations.clone();
        for eq in &eqs {
            eprintln!("EQ: {:?}", &eq);
        }
        if eqs.len() == 0 {
            eprintln!("No equations");
        }

        let (res, subst) = Self::unify(eqs);
        let ast = ast.resolve(&subst)?;

        eprintln!("ENV: {}", env);
        eprintln!("RES: {:?}", res);
        eprintln!("SUB: {:?}", subst);
        eprintln!("AST: {}", &ast.to_ron().unwrap());
        if res == UnifyResult::Ok {
            //let _ = visitor::visit(&ast, self, &mut subst).unwrap();
        }
        match res {
            logic::UnifyResult::Ok => Ok((ast, env, subst)),
            _ => unimplemented!("unable to unify"),
        }
    }

    fn gen_add(&mut self) -> Ast {
        // define add
        let a = self.var_named("a", Type::Int);
        let b = self.var_named("b", Type::Int);
        let params = vec![a.clone(), b.clone()];
        let sig = vec![Type::Int, Type::Int, Type::Int];
        let body = Ast::Internal(Builtin::AddInt(Box::new(a.into()), Box::new(b.into())));
        self.func(params, body, sig)
    }

    fn gen_sub(&mut self) -> Ast {
        // define add
        let a = self.var_named("a", Type::Int);
        let b = self.var_named("b", Type::Int);
        let params = vec![a.clone(), b.clone()];
        let sig = vec![Type::Int, Type::Int, Type::Int];
        let body = Ast::Internal(Builtin::SubInt(Box::new(a.into()), Box::new(b.into())));
        self.func(params, body, sig)
    }

    fn gen_eq(&mut self) -> Ast {
        // define add
        let a = self.var_named("a", Type::Int);
        let b = self.var_named("b", Type::Int);
        let params = vec![a.clone(), b.clone()];
        let sig = vec![Type::Int, Type::Int, Type::Bool];
        let body = Ast::Internal(Builtin::EqInt(Box::new(a.into()), Box::new(b.into())));
        self.func(params, body, sig)
    }

    fn add_gen_to_env(&mut self, name: &str, ast: Ast, mut env: Environment) -> Environment {
        // declare
        let ty = ast.get_type();
        let v = self.var_named(name, ty.clone());
        let d = Ast::Declare(v.clone(), ast.into());
        env.define(name.to_string(), v.into());
        self.base.push(d);
        env
    }

    pub fn base_env(&mut self) -> Environment {
        let env = Environment::default();
        let add = self.gen_add();
        let sub = self.gen_sub();
        let eq = self.gen_eq();
        let env = self.add_gen_to_env("+", add, env);
        let env = self.add_gen_to_env("-", sub, env.clone());
        let env = self.add_gen_to_env("==", eq, env.clone());
        env
    }

    pub fn lower(&mut self, ast: &Ast) -> Result<hir::Ast, Box<dyn Error>> {
        match ast {
            Ast::Literal(Literal::Bool(b)) => Ok(hir::bool(*b)),
            Ast::Literal(Literal::Int(u)) => Ok(hir::i64(*u as i64)),
            Ast::Literal(Literal::Float(f)) => Ok(hir::f64(*f)),
            Ast::Literal(Literal::String(_)) => unimplemented!("Strings are not implemented yet"),
            Ast::Variable(v) => Ok(lower_variable(v).into()),
            Ast::Extern(sig) => {
                let subst = SymbolTable::default();
                let sig = Type::lower_list(&sig, &subst)?;
                Ok(hir::Extern::new("+".to_string(), hir::FunctionType::export(sig).into()).into())
            }
            Ast::Declare(v, expr) => {
                let d = hir::Definition {
                    variable: hir::DefinitionId(v.id.0),
                    name: Some(v.name.clone()),
                    expr: self.lower(expr)?.into(),
                }
                .into();
                Ok(hir::Ast::Definition(d))
            }
            Ast::Block(exprs) => {
                let mut out = vec![];
                for e in exprs {
                    out.push(self.lower(e)?);
                }
                Ok(hir::Sequence::new(out).into())
            }
            Ast::Function { params, body, ty } => {
                let mut new_params = vec![];
                for var in params {
                    new_params.push(lower_variable(var));
                }
                let body = self.lower(body)?;

                let subst = SymbolTable::default();
                if let Type::Func(sig) = ty {
                    let sig = Type::lower_list(sig, &subst)?;
                    let ty = hir::FunctionType::export(sig);
                    Ok(hir::Lambda::new(new_params, body, ty).into())
                } else {
                    unreachable!()
                }
            }

            Ast::Apply(var, args) => {
                // We need to map f to a concrete function
                // We need the definition id that represents the compiled version of the function
                let lowered_var = lower_variable(var);
                let args = self.lower_list(args)?;
                let subst = SymbolTable::default();
                let sig = var.ty.children();
                let sig = Type::lower_list(&sig, &subst)?;
                let ty = hir::FunctionType::export(sig);
                Ok(hir::FunctionCall::new(lowered_var.into(), args, ty).into())
            }

            Ast::Internal(v) => match v {
                Builtin::SubInt(a, b) => {
                    Ok(hir::Builtin::SubInt(self.lower(a)?.into(), self.lower(b)?.into()).into())
                }
                Builtin::EqInt(a, b) => {
                    Ok(hir::Builtin::EqInt(self.lower(a)?.into(), self.lower(b)?.into()).into())
                }
                Builtin::AddInt(a, b) => {
                    Ok(hir::Builtin::AddInt(self.lower(a)?.into(), self.lower(b)?.into()).into())
                }
                Builtin::AddFloat(a, b) => {
                    Ok(hir::Builtin::AddFloat(self.lower(a)?.into(), self.lower(b)?.into()).into())
                }
            },

            Ast::Condition(condition, istrue, isfalse) => {
                let result_type = istrue.get_type();
                let istrue = self.lower(istrue)?;
                let isfalse = if let Some(v) = isfalse {
                    let else_result_type = v.get_type();
                    let v = self.lower(v)?;
                    assert_eq!(result_type, else_result_type);
                    Some(v.into())
                } else {
                    None
                };

                let subst = SymbolTable::default();
                Ok(hir::If {
                    condition: self.lower(condition)?.into(),
                    then: istrue.into(),
                    otherwise: isfalse,
                    result_type: result_type.lower(&subst)?,
                }
                .into())
            }
            _ => {
                unimplemented!("lower: {:?}", &ast)
            }
        }
    }

    pub fn lower_list_ast<T: Lower>(&mut self, vs: &Vec<T>) -> Result<Vec<Ast>, Box<dyn Error>> {
        let mut out = vec![];
        for a in vs {
            out.push(a.lower(self)?);
        }
        Ok(out)
    }

    fn lower_list(&mut self, exprs: &Vec<Ast>) -> Result<Vec<hir::Ast>, Box<dyn Error>> {
        let mut out = vec![];
        for e in exprs {
            out.push(self.lower(e)?);
        }
        Ok(out)
    }

    pub fn run_jit_main(&mut self, ast: &Ast) -> Result<i64, Box<dyn Error>> {
        let low = self.lower(&ast)?;
        println!("HIR:{}", &low.to_ron());
        low.run_main()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use codegen_llvm::LLVMBackendContext;
    use test_log::test;

    struct Test {}
    impl Visitor<()> for Test {
        fn exit(&mut self, e: &Ast, _n: &mut ()) -> visitor::VResult {
            println!("AST: {}", e);
            Ok(())
        }
    }

    fn start(eqs: ExprSeq) -> (UnifyResult, SymbolTable) {
        logic::Unify::<DefinitionId, Type, Ast>::start(eqs)
    }

    #[test]
    fn test_unify() {
        let x = Variable::named("x".into(), DefinitionId(0).into(), Type::Float);
        let eqs = vec![
            logic::Expr::OneOfTypes(
                Type::Func(vec![Type::Variable(1.into()), Type::Int]),
                vec![Type::Func(vec![Type::Int, Type::Int])],
            ),
            logic::Expr::OneOfValues(x.into(), vec![Ast::int(2), Ast::float(2.)]),
        ];
        let (res, subst) = logic::Unify::start(eqs);
        println!("SUB: {:?}", subst);
        assert_eq!(res, UnifyResult::Ok);
    }

    #[test]
    fn test_resolve() {
        let mut b: AstBuilder = AstBuilder::default();
        let env = b.base_env();

        let v = b.int(1);
        let decl1 = b.declare("a", v);
        let ty = b.type_unknown();
        let v = b.var_named("a".into(), ty);
        let decl2 = b.declare("b", v.into());
        let block = b.block(vec![decl1, decl2]);
        let ast = b.declare("c", block.clone());

        let (ast, env, _) = b.resolve(&ast, env.clone()).unwrap();

        println!("a = {:?}", env.resolve(&"a".into()));
        println!("b = {:?}", env.resolve(&"b".into()));
        println!("c = {:?}", env.resolve(&"c".into()));

        //assert_eq!(res, UnifyResult::Ok);

        // a and b should not be visible from the outer lexical scope
        // c should be visible though
        assert_eq!(env.resolve(&"a".into()), None);
        assert_eq!(env.resolve(&"b".into()), None);
        assert!(env.resolve(&"c".into()).is_some());
    }

    #[test]
    fn test_function_types() {
        let mut b: AstBuilder = AstBuilder::default();
        let env = b.base_env();

        // function to increment by one
        // f(x) => x + 1
        let ty = b.type_unknown();
        let p = b.var_unnamed(ty.clone());
        let one = b.int(1);
        let add = b.binary("+", one, p.clone().into());
        let f = b.func(vec![p], add, vec![Type::Int, Type::Int]);
        let df = b.declare("f", f);

        // call f
        let call = b.apply_name("f", vec![b.int(2)]);

        let ast = b.block(vec![df, call]);

        let (ast, env, subst) = b.resolve(&ast, env.clone()).unwrap();
        println!("{}", env);
        println!("{}", &ast);

        // the block returns an int from the call
        let ty = ast.get_type().resolve(&subst);
        assert_eq!(ty, Type::Int);

        let ast = ast.resolve(&subst).unwrap();
        println!("{:?}", &ast.get_type());
        assert!(ast
            .resolve(&subst)
            .unwrap()
            .get_type()
            .try_unknown()
            .is_none());
        assert_eq!(ty, Type::Int);

        let hir = b.lower(&ast).unwrap();
        println!("HIR:{}", &hir.to_ron());
        println!("AST:{:?}", &ast);
    }

    #[test]
    fn test_binary() {
        let mut b: AstBuilder = AstBuilder::default();
        let one = b.int(1);
        let two = b.int(2);
        let add = b.binary("+", one, two);
        let ast = b.declare("f", add);

        let (ast, env, subst) = b.resolve_ast_with_base(&ast).unwrap();

        let call = env.resolve(&"f".into()).unwrap();
        println!("call = {:?}", call);

        // make sure type is correct
        let ty = ast.get_type();
        let ty = ty.resolve(&subst);
        println!("ty = {:?}", &ty);
        assert_eq!(ty, Type::Int);
    }

    #[test]
    fn name_resolve() {
        let mut b: AstBuilder = AstBuilder::default();
        let mut env = Environment::default();

        // predefined variable "a"
        let v = b.int(1);
        let d = b.declare("a", v);
        env.define("a".into(), d);

        // create a variable that references "a"
        let ty = b.type_unknown();
        let v = b.var_named("a", ty);

        println!("AST:{:?}", &v);
        let (ast, env, subst) = b.resolve(&v.into(), env).unwrap();
        println!("ENV:{}", env);
        println!("AST:{}", &ast);

        for (k, v) in subst.iter() {
            println!("subst = {:?} => {:?}", k, v);
        }

        // make sure the types match
        let ty = ast.resolve(&subst).unwrap().get_type();
        let ty = ty.resolve(&subst);
        assert_eq!(ty, Type::Int);
    }

    #[test]
    fn add_builtin() {
        let mut b: AstBuilder = AstBuilder::default();

        let add = b.binary("+", b.int(1), b.int(2));
        let main = b.func(vec![], add, vec![Type::Int]);
        let dmain = b.declare("main", main);

        let (ast, _env, _subst) = b.resolve_ast_with_base(&dmain).unwrap();

        println!("AST: {:?}", &ast);

        let hir = b.lower(&ast).unwrap();
        println!("HIR:{:?}", &hir);

        let context = LLVMBackendContext::new();
        let mut b = context.backend();
        b.compile_module("main", &hir).unwrap();
        let ret = b.run().unwrap();
        assert_eq!(3, ret);
    }

    #[test]
    fn call_with_param() {
        let mut b: AstBuilder = AstBuilder::default();
        let env = b.base_env();

        // f(x) => x
        let p = b.var_unnamed(Type::Int);
        let block = b.block(vec![p.clone().into()]);
        let f = b.func(vec![p.clone()], block.into(), vec![Type::Int, Type::Int]);
        let df = b.declare("f", f);

        // main
        // call f
        let call = b.apply_name("f", vec![b.int(3)]);
        let main = b.func(vec![], call, vec![Type::Int]);
        let dmain = b.declare("main", main);

        let ast = b.block(vec![df, dmain]);

        let (ast, env, subst) = b.resolve_ast_with_base(&ast).unwrap();

        println!("AST: {}", &ast.to_ron().unwrap());
        println!("AST: {}", &ast);

        let hir = b.lower(&ast).unwrap();
        println!("HIR:{:?}", &hir);

        let context = LLVMBackendContext::new();
        let mut b = context.backend();
        b.compile_module("main", &hir).unwrap();
        let ret = b.run().unwrap();
        assert_eq!(3, ret);
    }
}
