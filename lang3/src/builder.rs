use crate::*;
use codegen::hir;
use std::error::Error;
use codegen::llvm::JitExecute;
use logic::{UnifyValue, UnifyType};

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
    fn enter(&mut self, e: &Ast, n: &mut SymbolTable) -> visitor::VResult {
        //println!("Visit AST: {}", e);
        Ok(())
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
        let ty = Type::Func(sig.clone());
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

    fn name_resolve(&mut self, ast: Ast, env: Environment) -> (Ast, Environment) {
        match ast {
            Ast::Literal(_) => (ast, env),
            Ast::Extern(_) => (ast, env),

            Ast::Assign(var, rhs) => {
                let name = var.name.clone();
                match env.resolve(&name) {
                    Some(v) => {
                        let (_, env) = self.name_resolve(*rhs, env.clone());
                        (v.clone(), env)
                    }
                    None => {
                        //eprintln!("Unable to resolve: {:?}", &var);
                        // name doesn't resolve, so we can't assign, declare instead
                        // We could make this configurable behavior.  By default, we may not
                        // want to declare new variables, to prevent shadowing.  Python does this
                        // but it's not necessarily a good thing.
                        self.name_resolve(Ast::Declare(var, rhs), env)
                    }
                }
            }

            Ast::Declare(var, rhs) => {
                let (new_rhs, mut env) = self.name_resolve(*rhs, env);
                let name = var.name.clone();
                let d = Ast::Declare(var, new_rhs.into());
                env.define(name, d.clone());
                (d, env)
            }

            Ast::Variable(v) => {

                // resolve the name from the environment
                match env.resolve(&v.name) {
                    Some(resolved_v) => {
                        let var_ty = v.ty.clone();
                        let resolved_ty = resolved_v.get_type();

                        // variable matches the type of resolve
                        self.equations.push(logic::Expr::Eq(var_ty, resolved_ty));
                    }
                    None => {
                        unimplemented!("unresolved variable: {} {:?}", &v.name, &v);
                    }
                }

                (v.into(), env)
            }

            Ast::Apply(var, ref args) => {
                let mut env = env.clone();
                
                // get the list of possible matches
                // so we an resolve this as part of unification
                let possible = env
                    .resolve_all(&var.name)
                    .into_iter()
                    .cloned()
                    .collect::<Vec<_>>();
                self.equations.push(logic::Expr::OneOfValues(var.clone().into(), possible));

                // resolve the args, which can be entire expressions
                for arg in args {
                    let (_f, this_env) = self.name_resolve(arg.clone(), env.clone());
                    env = this_env;
                }

                (Ast::Apply(var, args.clone()), env)
            }

            Ast::Function { params, body, ty } => {
                let mut local_env = env.clone();
                // The parameters on a function will be unbound, and don't need name resolution
                // When we resolve the body, we want the parameters to exist in the local environment
                let mut sig = vec![];
                for arg in &params {
                    let name = arg.name.clone();
                    let arg = arg.clone();
                    sig.push(arg.ty.clone());
                    local_env.define(name, Ast::Variable(arg.into()));
                }
                let (body, local_env) = self.name_resolve(*body.clone(), local_env);
           
                // type should match the return type of the body
                sig.push(body.get_type());

                let internal_ty = Type::Func(sig);
                (
                    Ast::Function {
                        params,
                        body: body.into(),
                        ty,
                    },
                    env,
                )
            }

            Ast::Block(exprs, ty) => {
                let mut local_env = env.clone();
                let mut updated = vec![];
                for expr in exprs {
                    let (new_ast, new_env) = self.name_resolve(expr.clone(), local_env);
                    local_env = new_env;
                    updated.push(new_ast);
                }
                // return original scope
                (Ast::block(updated), env)
            }

            Ast::Internal(v) => {
                // we should build a new Internal, but that's currently hard, and needs to be done
                // for every variant
                let mut env = env.clone();
                for expr in v.children() {
                    let (new_ast, new_env) = self.name_resolve(expr.clone(), env.clone());
                    let env = new_env;
                }
                (Ast::Internal(v), env)
            }

            Ast::Return(expr) => {
                self.name_resolve(*expr, env)
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

    pub fn resolve_ast(&mut self, ast: Ast) -> Result<Ast, Box<dyn Error>> {
        let env = self.base_env();
        let (res, ast, env, subst) = self.resolve(ast, env.clone());
        assert_eq!(res, logic::UnifyResult::Ok);
        let ast = ast.resolve(&subst);
        Ok(ast)
    }

    fn unify(eqs: ExprSeq) -> (UnifyResult, SymbolTable) {
        logic::Unify::<DefinitionId, Type, Ast>::start(eqs)
    }

    pub fn resolve(
        &mut self,
        ast: Ast,
        env: Environment,
    ) -> (UnifyResult, Ast, Environment, SymbolTable) {
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

        eprintln!("AST: {}", &ast);
        eprintln!("ENV: {}", env);
        eprintln!("RES: {:?}", res);
        eprintln!("SUB: {:?}", subst);
        if res == UnifyResult::Ok {
            //let _ = visitor::visit(&ast, self, &mut subst).unwrap();
        }

        (res, ast, env, subst)
    }

    fn gen_add(&mut self) -> Ast {
        // define add
        let a = self.var_named("a", Type::Int);
        let b = self.var_named("b", Type::Int);
        let params = vec![a.clone(), b.clone()];
        let sig = vec![Type::Int, Type::Int, Type::Int];
        let body = Ast::Internal(Builtin::AddInt(Box::new(a.into()), Box::new(b.into()))); //sig.clone());
        self.func(params, body, sig)
    }

    pub fn base_env(&mut self) -> Environment {
        let mut env = Environment::default();

        let rhs = self.gen_add();

        // declare
        let ty = rhs.get_type();
        let mut v = self.var_named("+", ty.clone());
        let d = Ast::Declare(v.clone(), rhs.into());

        env.define("+".to_string(), v.into());
        self.base.push(d);

        env
    }

    pub fn lower(&mut self, ast: &Ast) -> Result<hir::Ast, Box<dyn Error>> {
        match ast {
            Ast::Literal(Literal::Bool(b)) => Ok(hir::Ast::bool(*b)),
            Ast::Literal(Literal::Int(u)) => Ok(hir::Ast::i64(*u as i64)),
            Ast::Literal(Literal::Float(f)) => Ok(hir::Ast::f64(*f)),
            Ast::Literal(Literal::String(f)) => unimplemented!("Strings are not implemented yet"),
            Ast::Variable(v) => Ok(hir::DefinitionId(v.id.0).to_variable()),
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
            Ast::Block(exprs, ty) => {
                let mut out = vec![];
                for e in exprs {
                    out.push(self.lower(e)?);
                }
                Ok(hir::Sequence::new(out).into())
            }
            Ast::Function { params, body, ty } => {
                let mut new_params = vec![];
                for p in params {
                    let d = hir::DefinitionId(p.id.0);
                    let v = d.into();
                    new_params.push(v);
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
                let lowered_var = hir::DefinitionId(var.id.0).to_variable();
                let args = self.lower_list(args)?;
                let subst = SymbolTable::default();
                let sig = var.ty.children();
                let sig = Type::lower_list(&sig, &subst)?;
                let ty = hir::FunctionType::export(sig);
                Ok(hir::FunctionCall::new(lowered_var, args, ty).into())
            }

            Ast::Internal(v) => match v {
                Builtin::AddInt(a, b) => Ok(hir::Builtin::AddInt(
                    self.lower(a)?.into(),
                    self.lower(b)?.into(),
                )
                .into()),
                Builtin::AddFloat(a, b) => Ok(hir::Builtin::AddFloat(
                    self.lower(a)?.into(),
                    self.lower(b)?.into(),
                )
                .into()),
            },
            _ => {
                unimplemented!("{:?}", &ast)
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


    fn lower_list(
        &mut self,
        exprs: &Vec<Ast>,
    ) -> Result<Vec<hir::Ast>, Box<dyn Error>> {
        let mut out = vec![];
        for e in exprs {
            out.push(self.lower(e)?);
        }
        Ok(out)
    }

    fn base_ast(&self) -> Ast {
        Ast::block(self.base.clone())
    }


    pub fn run_jit_main(&mut self, ast: &Ast) -> Result<i64, Box<dyn Error>> {
        let mut exprs = self.base.clone();
        match ast {
            Ast::Block(more, _) => exprs.extend(more.clone()),
            _ => exprs.push(ast.clone())
        }
        let block = self.block(exprs);

        let low = self.lower(&block)?;
        println!("HIR: {:#?}", &low);
        low.run_main()
    }


}

#[cfg(test)]
mod tests {
    use super::*;
    use codegen::llvm::LLVMBackendContext;
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

        let (res, _ast, env, _) = b.resolve(ast, env.clone());

        println!("a = {:?}", env.resolve(&"a".into()));
        println!("b = {:?}", env.resolve(&"b".into()));
        println!("c = {:?}", env.resolve(&"c".into()));

        assert_eq!(res, UnifyResult::Ok);

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

        let (res, ast, env, subst) = b.resolve(ast, env.clone());
        println!("{}", env);
        println!("{}", &ast);
        assert_eq!(res, UnifyResult::Ok);

        // the block returns an int from the call
        let ty = ast.get_type().resolve(&subst);
        assert_eq!(ty, Type::Int);

        assert!(ast.get_type().try_unknown().is_some());

        let ast = ast.resolve(&subst);
        println!("{:?}", &ast.get_type());
        assert!(ast.resolve(&subst).get_type().try_unknown().is_none());
        assert_eq!(ty, Type::Int);

        let hir = b.lower(&ast).unwrap();
        println!("HIR:{:?}", &hir);
        println!("AST:{:?}", &ast);
    }

    #[test]
    fn test_binary() {
        let mut b: AstBuilder = AstBuilder::default();
        let env = b.base_env();
        let one = b.int(1);
        let two = b.int(2);
        let add = b.binary("+", one, two);
        let ast = b.declare("f", add);

        let (res, ast, env, subst) = b.resolve(ast, env.clone());
        println!("{}", env);
        println!("{}", &ast);
        assert_eq!(res, UnifyResult::Ok);

        let call = env.resolve(&"f".into()).unwrap();
        println!("call = {:?}", call);

        for (k, v) in subst.iter() {
            println!("subst = {:?} => {:?}", k, v);
        }

        // make sure type is correct
        let ty = ast.resolve(&subst).get_type();
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
        let (res, ast, env, subst) = b.resolve(v.into(), env);
        println!("ENV:{}", env);
        println!("AST:{}", &ast);
        assert_eq!(res, UnifyResult::Ok);

        for (k, v) in subst.iter() {
            println!("subst = {:?} => {:?}", k, v);
        }

        // make sure the types match
        let ty = ast.resolve(&subst).get_type();
        let ty = ty.resolve(&subst);
        assert_eq!(ty, Type::Int);
    }

    #[test]
    fn add_builtin() {
        let mut b: AstBuilder = AstBuilder::default();
        let env = b.base_env();

        let add = b.binary("+", b.int(1), b.int(2));
        let main = b.func(vec![], add, vec![Type::Int]);
        let dmain = b.declare("main", main);

        let block = b.block(vec![b.base_ast(), dmain]);
        let (res, ast, env, subst) = b.resolve(block, env.clone());
        let ast = ast.resolve(&subst);
        println!("AST: {:?}", &ast);
        println!("AST: {}", &ast);

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

        // f() => 1
        let p = b.var_unnamed(Type::Int);
        let f = b.func(vec![p.clone()], p.into(), vec![Type::Int, Type::Int]);
        let df = b.declare("f", f);

        // main
        // call f
        let call = b.apply_name("f", vec![b.int(2)]);
        let main = b.func(vec![], call, vec![Type::Int]);
        let dmain = b.declare("main", main);

        let ast = b.block(vec![df, dmain]);

        let (res, ast, env, subst) = b.resolve(ast, env.clone());
        let ast = ast.resolve(&subst);
        println!("AST: {:?}", &ast);
        println!("AST: {}", &ast);

        let hir = b.lower(&ast).unwrap();
        println!("HIR:{:?}", &hir);

        let context = LLVMBackendContext::new();
        let mut b = context.backend();
        b.compile_module("main", &hir).unwrap();
        let ret = b.run().unwrap();
        assert_eq!(2, ret);
    }
}
