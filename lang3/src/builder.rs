use crate::*;
use logic::{self, SymbolTable, TypeSignature, UnifyResult, DefinitionId};
use std::fmt;
use std::marker::PhantomData;

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

impl<T: Clone + fmt::Debug + fmt::Display + PartialEq + TypeSignature<Type>> AstBuilder<T> {
    pub fn next_id(&mut self) -> DefinitionId {
        let d = DefinitionId(self.last_id);
        self.last_id += 1;
        d
    }

    pub fn int(&self, i: u64) -> Ast<T> {
        Ast::Literal(Literal::Int(i))
    }

    pub fn float(&self, f: f64) -> Ast<T> {
        Ast::Literal(Literal::Float(f))
    }

    pub fn boolean(&self, b: bool) -> Ast<T> {
        Ast::Literal(Literal::Bool(b))
    }

    pub fn type_unknown(&mut self) -> Type {
        Type::Variable(self.next_id())
    }

    pub fn var_unnamed(&mut self, ty: Type) -> Variable<T> {
        Variable::unnamed(self.next_id(), ty)
    }

    pub fn var_named(&mut self, n: &str, ty: Type) -> Variable<T> {
        Variable::named(n.to_string(), self.next_id(), ty)
    }

    pub fn node_unnamed(&mut self, ty: Type) -> Ast<T> {
        let v = self.var_unnamed(ty.clone());
        v.into()
    }

    pub fn node_named(&mut self, n: &str, ty: Type) -> Ast<T> {
        let v = Variable::named(n.to_string(), self.next_id(), ty);
        v.into()
    }

    pub fn var_resolve(&mut self, n: &str, env: Environment<T>) -> Ast<T> {
        match env.resolve(&n.to_string()) {
            Some(v) => v.clone(),
            None => {
                println!("Unable to find symbol: {}", n);
                unimplemented!();
            }
        }
    }

    pub fn block(&self, exprs: Vec<Ast<T>>) -> Ast<T> {
        Ast::Block(exprs)
    }

    pub fn func(&mut self, params: Vec<Variable<T>>, body: Ast<T>, sig: Vec<Type>) -> Ast<T> {
        let ty = Type::Func(sig.clone());
        Ast::Function { params, body: body.into(), ty: sig }
    }

    pub fn apply(&mut self, f: Ast<T>, args: Vec<Ast<T>>) -> Ast<T> {
        Ast::Apply(f.into(), args)
    }

    pub fn apply_name(&mut self, name: &str, args: Vec<Ast<T>>) -> Ast<T> {
        let mut arg_types = args.iter().map(|a| a.get_type()).collect::<Vec<_>>();
        let ret_ty = self.type_unknown();
        arg_types.push(ret_ty);
        let ty = Type::Func(arg_types);
        let f = self.node_named(name, ty);
        Ast::Apply(f.into(), args)
    }

    pub fn binary(&mut self, name: &str, lhs: Ast<T>, rhs: Ast<T>) -> Ast<T> {
        let ty_ret = self.type_unknown();
        let ty_f = Type::Func(vec![lhs.get_type(), rhs.get_type(), ty_ret]);
        let f = self.node_named(name.into(), ty_f);
        let args = vec![lhs, rhs];
        Ast::Apply(f.into(), args)
    }

    fn name_resolve(&mut self, ast: Ast<T>, env: Environment<T>) -> (Ast<T>, Environment<T>) {
        match ast {
            Ast::Literal(_) => (ast, env),

            Ast::Declare(name, rhs) => {
                let (new_rhs, mut env) = self.name_resolve(*rhs, env);
                env.define(name.clone(), new_rhs.clone());
                (Ast::Declare(name, new_rhs.into()), env)
            }

            Ast::Variable(v) => {
                match env.resolve(&v.name.as_ref().unwrap()) {
                    Some(resolved_v) => {
                        //v.bind(resolved_v.clone());
                        //let ty = resolved_v.borrow().ty.clone();
                        let v = Ast::Variable(v.clone());
                        (v, env)
                        //let new_ast = AstNode::new(v, ty);
                        //println!("resolve: {:?} => {:?}", &ast, &new_ast);
                        //new_ast
                    }
                    None => {
                        eprintln!("unresolved variable: {:?}", &v);
                        unimplemented!();
                    }
                }
            }

            Ast::Apply(f, ref args) => {
                let mut env = env.clone();
                //for arg in args {
                    //let (_f, this_env) = self.name_resolve(arg.clone(), env.clone());
                    //env = this_env;
                //}

                match *f {
                    Ast::Variable(ref v) => {
                        let name = v.name.clone().expect("should be named");
                        //let ty = f.get_type();
                        //let mut arg_types = args
                            //.iter()
                            //.map(|a| a.get_type())
                            //.collect::<Vec<_>>();
                        //arg_types.push(ast_ty);

                        // create equation for all matches for the function
                        let possible = env
                            .resolve_all(&v.name.as_ref().unwrap())
                            .iter()
                            .cloned()
                            .map(|v| {
                                v.get_type()
                            })
                            .collect::<Vec<_>>();
                        //let ty = TypeNodePair::new(Type::Func(arg_types), *f.clone());
                        self.equations.push(logic::Expr::OneOf(f.get_type(), possible));
                    }

                    Ast::Function { ref params, ref body, ref ty } => {
                        // Already resolved
                        // TODO:
                        // args are unbound
                        // bind any free variables in the body
                    }
                    _ => unimplemented!(),
                }
                (Ast::Apply(f.clone(), args.clone()), env)
            }

            Ast::Function { params, body, ty } => {
                let mut local_env = env.clone();
                //for arg in params {
                    //local_env.define(
                //}
                let (body, local_env) = self.name_resolve(*body.clone(), local_env);
                // nothing to do?
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

    pub fn declare(&self, name: &str, rhs: Ast<T>) -> Ast<T> {
        let ty = rhs.get_type();
        Ast::Declare(name.into(), rhs.into())
    }

    /*
    fn substitute(&mut self, p: Ast<T>, subst: &SymbolTable<Ast<T>>) -> Ast<T> {
        match p {
            Type::Variable(ty_id) => match logic::subst_get_type_by_id(&subst, &ty_id) {
                Some(v) => v,
                None => {
                    println!("Type missing from substitution table: {:?}", (ty_id, &p));
                    unimplemented!()
                }
            },
            Type::Func(sig) => {
                let new_sig = sig
                    .into_iter()
                    .map(|v| {
                        self.substitute(v, subst)
                    })
                    .collect();
                pTypeNodePair::new(Type::Func(new_sig), p.node)
            }
            _ => p.clone(),
        }
    }
    */

    pub fn resolve(
        &mut self,
        ast: Ast<T>,
        env: Environment<T>,
    ) -> (UnifyResult, Ast<T>, Environment<T>, SymbolTable<Type>) {
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

    pub fn base_env(&mut self) -> Environment<T> {
        let mut env = Environment::default();

        let params = vec![self.var_named("a", Type::Int), self.var_named("b", Type::Int)];
        let sig = vec![Type::Int, Type::Int, Type::Int];
        let body = Ast::Extern(sig.clone());
        env.define("+".to_string(), self.func(params, body, sig));
        env
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;

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
            //logic::Expr::OneOf(Type::Variable(0.into()), vec![Type::Int]),
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

        let (res, ast, env, _) = b.resolve(ast, env.clone());

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
        let ast = b.declare("f", f);

        let (res, ast, env, _) = b.resolve(ast, env.clone());
        println!("{}", env);
        println!("{}", &ast);
        assert_eq!(res, UnifyResult::Ok);

        //let call = env.resolve(&"f".into()).unwrap();
        //println!("{}", &call);
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
        let ty = ast.resolve(subst.clone()).get_type();//resolve_type(subst).unwrap();
        let (_, ty) = ty.resolve(subst);
        println!("ty = {:?}", &ty);
        assert_eq!(ty, Type::Int);
    }

    #[test]
    fn name_resolve() {
        let mut b: AstBuilder<Type> = AstBuilder::default();
        let mut env = Environment::default();
        env.define("a".into(), b.int(1));
        let ty = b.type_unknown();
        let v = b.var_named("a", ty);
        println!("AST:{:?}", &v);
        let (res, ast, env, subst) = b.resolve(v.into(), env);
        println!("ENV:{}", env);
        println!("AST:{}", &ast);

        for (k,v) in subst.iter() {
            println!("subst = {:?} => {:?}", k, v);
        }

        // make sure the types match
        let ty = ast.resolve(subst.clone()).get_type();//resolve_type(subst).unwrap();
        let (_, ty) = ty.resolve(subst);
        //assert_eq!(ty, Type::Int);
    }
}
