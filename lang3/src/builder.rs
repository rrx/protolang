use crate::*;
use logic::{self, SymbolTable, TypeSignature, UnifyResult, DefinitionId};
use std::marker::PhantomData;
use codegen::hir;
use std::error::Error;
use std::ops::Deref;

pub type AstType = Type;

#[derive(Default)]
pub struct AstBuilder<T> {
    last_id: usize,
    pub equations: Vec<logic::Expr<Type>>,
    phantom: PhantomData<T>
}

impl Visitor<Type, SymbolTable<Ast<Type>>> for AstBuilder<Type> {
    fn enter(&mut self, e: &Ast<Type>, n: &mut SymbolTable<Ast<Type>>) -> visitor::VResult {
        println!("Visit AST: {}", e);
        Ok(())
    }
}

impl AstBuilder<AstType> {
    pub fn type_next_id(&mut self) -> DefinitionId {
        let d = DefinitionId(self.last_id);
        self.last_id += 1;
        d
    }

    pub fn var_next_id(&mut self) -> VariableId {
        let d = VariableId(self.last_id);
        self.last_id += 1;
        d
    }

    pub fn int(&self, i: u64) -> Ast<AstType> {
        Ast::Literal(Literal::Int(i))
    }

    pub fn float(&self, f: f64) -> Ast<AstType> {
        Ast::Literal(Literal::Float(f))
    }

    pub fn boolean(&self, b: bool) -> Ast<AstType> {
        Ast::Literal(Literal::Bool(b))
    }

    pub fn type_unknown(&mut self) -> Type {
        Type::Variable(self.type_next_id())
    }

    pub fn var_unnamed(&mut self, ty: Type) -> Variable<AstType> {
        Variable::unnamed(self.var_next_id(), ty)
    }

    pub fn var_named(&mut self, n: &str, ty: Type) -> Variable<AstType> {
        Variable::named(n.to_string(), self.var_next_id(), ty)
    }

    pub fn node_unnamed(&mut self, ty: Type) -> Ast<AstType> {
        let v = self.var_unnamed(ty.clone());
        v.into()
    }

    pub fn node_named(&mut self, n: &str, ty: Type) -> Ast<AstType> {
        self.var_named(n, ty).into()
    }

    pub fn var_resolve(&mut self, n: &str, env: Environment<AstType>) -> Ast<AstType> {
        match env.resolve(&n.to_string()) {
            Some(v) => v.clone(),
            None => {
                println!("Unable to find symbol: {}", n);
                unimplemented!();
            }
        }
    }

    pub fn block(&self, exprs: Vec<Ast<AstType>>) -> Ast<AstType> {
        Ast::Block(exprs)
    }

    pub fn func(&mut self, params: Vec<Variable<AstType>>, body: Ast<AstType>, sig: Vec<Type>) -> Ast<AstType> {
        let ty = Type::Func(sig.clone());
        Ast::Function { params, body: body.into(), ty: Type::Func(sig) }
    }

    pub fn apply(&mut self, f: Ast<AstType>, args: Vec<Ast<AstType>>) -> Ast<AstType> {
        Ast::Apply(f.into(), args)
    }

    pub fn apply_name(&mut self, name: &str, args: Vec<Ast<AstType>>) -> Ast<AstType> {
        let mut arg_types = args.iter().map(|a| a.get_type()).collect::<Vec<_>>();
        let ret_ty = self.type_unknown();
        arg_types.push(ret_ty);
        let ty = Type::Func(arg_types);
        let f = self.node_named(name, ty);
        Ast::Apply(f.into(), args)
    }

    pub fn binary(&mut self, name: &str, lhs: Ast<AstType>, rhs: Ast<AstType>) -> Ast<AstType> {
        let ty_ret = self.type_unknown();
        let ty_f = Type::Func(vec![lhs.get_type(), rhs.get_type(), ty_ret]);
        let f = self.node_named(name.into(), ty_f);
        let args = vec![lhs, rhs];
        Ast::Apply(f.into(), args)
    }

    fn name_resolve(&mut self, ast: Ast<AstType>, env: Environment<AstType>) -> (Ast<AstType>, Environment<AstType>) {
        match ast {
            Ast::Literal(_) => (ast, env),

            Ast::Declare(var, rhs) => {
                let (new_rhs, mut env) = self.name_resolve(*rhs, env);
                env.define(var.name.clone(), new_rhs.clone());
                (Ast::Declare(var, new_rhs.into()), env)
            }

            Ast::Variable(v) => {
                match env.resolve(&v.name) {
                    Some(resolved_v) => {
                        let var_ty = v.ty.clone();
                        let resolved_ty = resolved_v.get_type();
                        //let mut v = v.clone();
                        //v.bind(resolved_ty.clone());
                        let v = Ast::Variable(v.clone());

                        // variable matches the type of resolved
                        self.equations.push(logic::Expr::Eq(var_ty, resolved_ty));

                        (v, env)
                    }
                    None => {
                        eprintln!("unresolved variable: {:?}", &v);
                        unimplemented!();
                    }
                }
            }

            Ast::Apply(f, ref args) => {
                let mut env = env.clone();

                // resolve the args, which can be entire expressions
                for arg in args {
                    let (_f, this_env) = self.name_resolve(arg.clone(), env.clone());
                    env = this_env;
                }

                match *f {
                    Ast::Variable(ref v) => {
                        // resolve the name of the function
                        // This can yield multiple results and they need to match with parameters
                        // We accomplish this here by making use of the type unification system
                        // This also means we don't know the resolution until after types are 
                        // unified.
                        
                        let name = v.name.clone();
                        
                        // create equation for all matches for the function
                        // We could filter this based on the known arguments so far
                        // It could be none, and so we could return an error now, rather
                        // than waiting for unification
                        let possible = env
                            .resolve_all(&v.name)
                            .iter()
                            .cloned()
                            .map(|v| {
                                v.get_type()
                            })
                            .collect::<Vec<_>>();
                        self.equations.push(logic::Expr::OneOf(f.get_type(), possible));
                    }

                    Ast::Function { ref params, ref body, ref ty } => {
                        // Already resolved
                        // TODO:
                        // args are unbound
                        // bind any free variables in the body
                        //
                        // name resolve on the body
                        let (b, this_env) = self.name_resolve(*body.clone(), env);
                        env = this_env;
                        unreachable!()
                    }
                    _ => unimplemented!(),
                }
                (Ast::Apply(f.clone(), args.clone()), env)
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
                (Ast::Function { params, body: body.into(), ty }, env)
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
                (self.block(updated), env)
            }
            _ => {
                println!("{:?}", &ast);
                unimplemented!()
            }
        }
    }

    pub fn declare(&mut self, name: &str, rhs: Ast<AstType>) -> Ast<AstType> {
        let ty = rhs.get_type();
        let v = self.var_named(name, ty.clone());
        Ast::Declare(v, rhs.into())
    }

    pub fn resolve(
        &mut self,
        ast: Ast<AstType>,
        env: Environment<AstType>,
    ) -> (UnifyResult, Ast<AstType>, Environment<AstType>, SymbolTable<Type>) {
        // name resolution
        let (ast, env) = self.name_resolve(ast, env);

        // infer all types
        let eqs = self.equations.clone();
        for eq in &eqs {
            println!("EQ: {:?}", &eq);
        }
        let (res, subst) = logic::unify_start(eqs);

        println!("AST: {}", &ast);
        println!("ENV: {}", env);
        println!("RES: {:?}", res);
        println!("SUB: {:?}", subst);
        if res == UnifyResult::Ok {
            //let _ = visitor::visit(&ast, self, &mut subst).unwrap();
            //println!("SUB: {:?}", subst);
        }

        (res, ast, env, subst)
    }

    pub fn base_env(&mut self) -> Environment<AstType> {
        let mut env = Environment::default();

        let params = vec![self.var_named("a", Type::Int), self.var_named("b", Type::Int)];
        let sig = vec![Type::Int, Type::Int, Type::Int];
        let body = Ast::Extern(sig.clone());
        env.define("+".to_string(), self.func(params, body, sig));
        env
    }

    pub fn lower(&mut self, ast: &Ast<AstType>, env: Environment<AstType>) -> Result<hir::Ast, Box<dyn Error>> {
        match ast {
            Ast::Literal(Literal::Bool(b)) => Ok(hir::Ast::bool(*b)),
            Ast::Literal(Literal::Int(u)) => Ok(hir::Ast::i64(*u as i64)),
            Ast::Literal(Literal::Float(f)) => Ok(hir::Ast::f64(*f)),
            Ast::Variable(v) => Ok(hir::DefinitionId(v.id.0).to_variable()),
            Ast::Declare(v, expr) => {
                let d = hir::Definition {
                    variable: hir::DefinitionId(v.id.0),
                    name: Some(v.name.clone()),
                    expr: self.lower(expr, env)?.into()
                }.into(); 
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

            Ast::Apply(f, args) => {
                // We need to map f to a concrete function
                // We need the definition id that represents the compiled version of the function

                match Box::deref(f) {
                    Ast::Variable(_) => {
                        let lowered_f = self.lower(f, env.clone())?;
                        let args = self.lower_list(args, env.clone())?;
                        let subst = SymbolTable::default();
                        let sig = f.get_type().children();
                        eprintln!("f: {:?}", f);
                        eprintln!("sig: {:?}", sig);
                        let sig = Type::lower_list(&sig, &subst)?;
                        let ty = hir::FunctionType::export(sig);
                        Ok(hir::FunctionCall::new(lowered_f, args, ty).into())
                    }
                    _ => unreachable!()
                }
            }

            _ => {
                println!("{:?}", &ast);
                unimplemented!()
            }
        }
    }
    
    fn lower_list(&mut self, exprs: &Vec<Ast<AstType>>, env: Environment<AstType>) -> Result<Vec<hir::Ast>, Box<dyn Error>> {
        let mut out = vec![];
        for e in exprs {
            out.push(self.lower(e, env.clone())?);
        }
        Ok(out)
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;
    use codegen::llvm::LLVMBackendContext;

    struct Test {}
    impl Visitor<Type, ()> for Test {
        fn exit(&mut self, e: &Ast<Type>, _n: &mut ()) -> visitor::VResult {
            println!("AST: {}", e);
            Ok(())
        }
    }

    #[test]
    fn test_unify() {
        let eqs = vec![
            logic::Expr::OneOf(
                Type::Func(vec![Type::Variable(0.into()), Type::Int]),
                vec![Type::Func(vec![Type::Int, Type::Int])],
                )
        ];
        let (res, subst) = logic::unify_start(eqs);
        println!("SUB: {:?}", subst);
        assert_eq!(res, UnifyResult::Ok);
    }

    #[test]
    fn test_resolve() {
        let mut b: AstBuilder<Type> = AstBuilder::default();
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
    fn test_function() {
        let mut b: AstBuilder<Type> = AstBuilder::default();
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
    }

    #[test]
    fn test_binary() {
        let mut b: AstBuilder<Type> = AstBuilder::default();
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

        for (k,v) in subst.iter() {
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
        let mut b: AstBuilder<Type> = AstBuilder::default();
        let mut env = Environment::default();

        // predefined variable "a"
        env.define("a".into(), b.int(1));

        // create a variable that references "a"
        let ty = b.type_unknown();
        let v = b.var_named("a", ty);

        println!("AST:{:?}", &v);
        let (res, ast, env, subst) = b.resolve(v.into(), env);
        println!("ENV:{}", env);
        println!("AST:{}", &ast);
        assert_eq!(res, UnifyResult::Ok);

        for (k,v) in subst.iter() {
            println!("subst = {:?} => {:?}", k, v);
        }

        // make sure the types match
        let ty = ast.resolve(&subst).get_type();
        let ty = ty.resolve(&subst);
        assert_eq!(ty, Type::Int);
    }

    #[test]
    fn codegen() {
        let mut b: AstBuilder<Type> = AstBuilder::default();
        let env = b.base_env();
        let ty = b.type_unknown();

        // simple function that increments and returns the number
        // f(x) => x + 1
        let p = b.var_unnamed(ty.clone());
        let one = b.int(1);
        let add = b.binary("+", one, p.clone().into());
        let f = b.func(vec![p], add, vec![Type::Int, Type::Int]);
        let df = b.declare("main", f);

        let ast = b.block(vec![df]);

        let (res, ast, env, subst) = b.resolve(ast, env.clone());
        assert_eq!(res, UnifyResult::Ok);

        // the ast should have a function type, since it's the last item of block
        let ty = ast.get_type().resolve(&subst);
        assert_eq!(ty, Type::Func(vec![Type::Int, Type::Int]));

        let ast = &ast.resolve(&subst);
        println!("AST:{}", &ast);
        println!("AST:{:?}", &ast);

        let hir = b.lower(&ast, env.clone()).unwrap();
        println!("HIR:{:?}", &hir);
        let context = LLVMBackendContext::new();
        let mut b = context.backend();
        b.compile_module("main", hir);
        let ret = b.run().unwrap();
        assert_eq!(11, ret);
    }
}
