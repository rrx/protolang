use crate::*;
use codegen::hir;
use logic::{self, SymbolTable, TypeSignature, UnifyResult};
use std::error::Error;

#[derive(Default)]
pub struct AstBuilder {
    last_id: usize,
    pub equations: Vec<logic::Expr<Type>>,
    to_resolve: Vec<Variable>,
    base: Vec<Ast>,
}

impl Visitor<SymbolTable<Ast>> for AstBuilder {
    fn enter(&mut self, e: &Ast, n: &mut SymbolTable<Ast>) -> visitor::VResult {
        //println!("Visit AST: {}", e);
        Ok(())
    }
}

impl AstBuilder {
    pub fn type_next_id(&mut self) -> logic::DefinitionId {
        let d = logic::DefinitionId(self.last_id);
        self.last_id += 1;
        d
    }

    pub fn var_next_id(&mut self) -> VariableId {
        let d = VariableId(self.last_id);
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
        Ast::Block(exprs)
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

            Ast::Declare(var, rhs) => {
                let (new_rhs, mut env) = self.name_resolve(*rhs, env);
                let name = var.name.clone();
                let d = Ast::Declare(var, new_rhs.into());
                env.define(name, d.clone());
                (d, env)
            }

            Ast::Variable(mut v) => {
                v.bind(env.clone());
                self.to_resolve.push(v.clone());
                (v.into(), env)
            }

            Ast::Apply(mut var, ref args) => {
                let mut env = env.clone();

                // resolve the args, which can be entire expressions
                for arg in args {
                    let (_f, this_env) = self.name_resolve(arg.clone(), env.clone());
                    env = this_env;
                }

                var.bind(env.clone());
                self.to_resolve.push(var.clone());
                (Ast::Apply(var, args.clone()), env)
            }

            Ast::Function { params, body, ty } => {
                let mut local_env = env.clone();
                // The parameters on a function will be unbound, and don't need name resolution
                // When we resolve the body, we want the parameters to exist in the local environment
                for arg in &params {
                    let name = arg.name.clone();
                    local_env.define(name, Ast::Variable(arg.clone()));
                }
                let (body, local_env) = self.name_resolve(*body.clone(), local_env);
                (
                    Ast::Function {
                        params,
                        body: body.into(),
                        ty,
                    },
                    env,
                )
            }

            Ast::Block(exprs) => {
                let mut local_env = env.clone();
                let mut updated = vec![];
                for expr in exprs {
                    let (new_ast, new_env) = self.name_resolve(expr.clone(), local_env);
                    local_env = new_env;
                    updated.push(new_ast);
                }
                // return original scope
                (Ast::Block(updated), env)
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
            _ => {
                eprintln!("{:?}", &ast);
                unimplemented!()
            }
        }
    }

    pub fn declare(&mut self, name: &str, rhs: Ast) -> Ast {
        let ty = rhs.get_type();
        let v = self.var_named(name, ty.clone());
        Ast::Declare(v, rhs.into())
    }

    pub fn resolve(
        &mut self,
        ast: Ast,
        env: Environment,
    ) -> (UnifyResult, Ast, Environment, SymbolTable<Type>) {
        // name resolution
        let (ast, env) = self.name_resolve(ast, env);

        // Generate equations
        for v in &self.to_resolve {
            let eq = v.generate_equations();
            self.equations.push(eq);
        }

        // infer all types
        let eqs = self.equations.clone();
        for eq in &eqs {
            eprintln!("EQ: {:?}", &eq);
        }
        let (res, subst) = logic::unify_start(eqs);

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
        let v = self.var_named("+", ty.clone());
        let d = Ast::Declare(v.clone(), rhs.into());

        env.define("+".to_string(), v.into());
        self.base.push(d);

        env
    }

    pub fn lower(&mut self, ast: &Ast, env: Environment) -> Result<hir::Ast, Box<dyn Error>> {
        match ast {
            Ast::Literal(Literal::Bool(b)) => Ok(hir::Ast::bool(*b)),
            Ast::Literal(Literal::Int(u)) => Ok(hir::Ast::i64(*u as i64)),
            Ast::Literal(Literal::Float(f)) => Ok(hir::Ast::f64(*f)),
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
                    expr: self.lower(expr, env)?.into(),
                }
                .into();
                Ok(hir::Ast::Definition(d))
            }
            Ast::Block(exprs) => {
                let mut out = vec![];
                for e in exprs {
                    out.push(self.lower(e, env.clone())?);
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
                let body = self.lower(body, env.clone())?;

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
                let args = self.lower_list(args, env.clone())?;
                let subst = SymbolTable::default();
                let sig = var.ty.children();
                //eprintln!("f: {:?}", f);
                //eprintln!("sig: {:?}", sig);
                let sig = Type::lower_list(&sig, &subst)?;
                let ty = hir::FunctionType::export(sig);
                Ok(hir::FunctionCall::new(lowered_var, args, ty).into())
            }

            Ast::Internal(v) => match v {
                Builtin::AddInt(a, b) => Ok(hir::Builtin::AddInt(
                    self.lower(a, env.clone())?.into(),
                    self.lower(b, env.clone())?.into(),
                )
                .into()),
                Builtin::AddFloat(a, b) => Ok(hir::Builtin::AddFloat(
                    self.lower(a, env.clone())?.into(),
                    self.lower(b, env.clone())?.into(),
                )
                .into()),
            },
            _ => {
                eprintln!("{:?}", &ast);
                unimplemented!()
            }
        }
    }

    fn lower_list(
        &mut self,
        exprs: &Vec<Ast>,
        env: Environment,
    ) -> Result<Vec<hir::Ast>, Box<dyn Error>> {
        let mut out = vec![];
        for e in exprs {
            out.push(self.lower(e, env.clone())?);
        }
        Ok(out)
    }

    fn base_ast(&self) -> Ast {
        Ast::Block(self.base.clone())
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

    #[test]
    fn test_unify() {
        let eqs = vec![logic::Expr::OneOf(
            Type::Func(vec![Type::Variable(0.into()), Type::Int]),
            vec![Type::Func(vec![Type::Int, Type::Int])],
        )];
        let (res, subst) = logic::unify_start(eqs);
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

        assert!(ast.get_type().unknown().is_some());

        let ast = ast.resolve(&subst);
        println!("{:?}", &ast.get_type());
        assert!(ast.resolve(&subst).get_type().unknown().is_none());
        assert_eq!(ty, Type::Int);

        let hir = b.lower(&ast, env.clone()).unwrap();
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

        for eq in &b.equations {
            println!("EQ: {:?}", &eq);
        }

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
        println!("r: {:?}", &b.to_resolve);
        println!("AST: {:?}", &ast);
        println!("AST: {}", &ast);

        let hir = b.lower(&ast, env.clone()).unwrap();
        println!("HIR:{:?}", &hir);

        let context = LLVMBackendContext::new();
        let mut b = context.backend();
        b.compile_module("main", hir).unwrap();
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
        println!("r: {:?}", &b.to_resolve);
        println!("AST: {:?}", &ast);
        println!("AST: {}", &ast);

        let hir = b.lower(&ast, env.clone()).unwrap();
        println!("HIR:{:?}", &hir);

        let context = LLVMBackendContext::new();
        let mut b = context.backend();
        b.compile_module("main", hir).unwrap();
        let ret = b.run().unwrap();
        assert_eq!(2, ret);
    }
}
