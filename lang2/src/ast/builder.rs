use crate::ast::*;
use crate::typesystem::{Type, TypeChecker, TypeNodePair};
use crate::visitor;

#[derive(Default)]
pub struct AstBuilder {
    pub check: TypeChecker,
}

impl visitor::Visitor<SymbolTable> for AstBuilder {
    fn exit(&mut self, e: AstNode, _n: &mut SymbolTable) -> visitor::VResult {
        println!("AST: {}", e);

        let is_unknown = {
            let ty = &e.borrow().ty;
            ty.is_unknown_recursive()
        };

        if is_unknown {
            let before: TypeNodePair = e.clone().into();
            let after = self.substitute(e.clone().into());
            //println!("replacing {:?} => {:?}", &before.ty, &after.ty);
            // only replace the type.  The node returned by substitute
            // isn't always going to be the same node.  It might be the
            // parent node for the type.
            let mut new_inner = before.node.borrow().clone();
            new_inner.ty = after.ty.clone();
            //println!("replacing {:?}", &new_inner);
            e.replace(new_inner);
        }

        /*
        e.replace_with(|a| {
            if a.ty.is_unknown_recursive() {
                let before = a.ty.clone();
                let after = self.substitute(AstNode::new(a.value.clone(), a.ty.clone()).into());
                println!("replacing {:?} => {:?}", &before, &after.ty);
            }

            // ensure that all variables are bound
            match &a.value {
                Ast::Variable(v) => {
                    //assert!(v.bound.is_some());
                }
                _ => ()
            }

            a.clone()
        });
        */


        Ok(())
    }
}

impl AstBuilder {
    pub fn int(&self, i: u64) -> AstNode {
        AstNode::new(Literal::Int(i).into(), Type::Int)
    }

    pub fn float(&self, f: f64) -> AstNode {
        AstNode::new(Literal::Float(f).into(), Type::Float)
    }

    pub fn boolean(&self, b: bool) -> AstNode {
        AstNode::new(Literal::Bool(b).into(), Type::Bool)
    }

    pub fn var(&mut self, n: &str) -> AstNode {
        AstNode::new(
            Variable::new(n.into()).into(),
            self.check.new_unknown_type(),
        )
    }

    pub fn var_resolve(&mut self, n: &str, env: Environment) -> AstNode {
        match env.resolve(&n.to_string()) {
            Some(v) => v.clone(),
            None => {
                println!("Unable to find symbol: {}", n);
                unimplemented!();
            }
        }
    }

    pub fn block(&self, exprs: Vec<AstNode>) -> AstNode {
        let ty = exprs
            .iter()
            .last()
            .map_or(Type::Unit, |x| x.borrow().ty.clone());
        AstNode::new(Ast::Block(exprs), ty)
    }

    pub fn binary(&mut self, name: &str, lhs: AstNode, rhs: AstNode) -> AstNode {
        let ty_ret = self.check.new_unknown_type();
        let ty_f = Type::Func(vec![
            lhs.borrow().ty.clone(),
            rhs.borrow().ty.clone(),
            ty_ret.clone(),
        ]);

        // type of this operation
        let f = AstNode::new(Variable::new(name.into()).into(), ty_f);

        let args = vec![lhs, rhs];
        AstNode::new(Ast::Apply(f.into(), args), ty_ret)
    }

    fn name_resolve(&mut self, ast: AstNode, env: Environment) -> (AstNode, Environment) {
        let mut inner = ast.borrow_mut();
        let ast_ty = inner.ty.clone();
        let value = &mut inner.value;

        match value {
            Ast::Literal(_) => (ast.clone(), env),

            Ast::Declare(name, rhs) => {
                let (new_rhs, mut env) = self.name_resolve(*rhs.clone(), env.clone());
                let ty = new_rhs.borrow().ty.clone();
                env.define(name.clone(), new_rhs.clone());
                (
                    AstNode::new(Ast::Declare(name.clone(), Box::new(new_rhs.clone())), ty),
                    env,
                )
            }

            Ast::Variable(v) => {
                let ast = match env.resolve(&v.name) {
                    Some(resolved_v) => {
                        v.bind(resolved_v.clone());
                        let ty = resolved_v.borrow().ty.clone();
                        let v = Ast::Variable(v.clone());
                        let new_ast = AstNode::new(v, ty);
                        //println!("resolve: {:?} => {:?}", &ast, &new_ast);
                        new_ast
                        //inner.into()
                        //v.replace_with(|a| {
                            //let mut b = a.clone();
                            //b.ty = ast_ty;
                            //b
                        //});
                    }
                    None => {
                        unimplemented!();
                    }
                };
                (ast, env)
            }

            Ast::Apply(f, ref args) => {
                for arg in args {
                    let (f, env) = self.name_resolve(arg.clone(), env.clone());
                }

                match &f.borrow().value {
                    Ast::Variable(v) => {
                        let mut arg_types = args
                            .iter()
                            .map(|a| a.borrow().ty.clone())
                            .collect::<Vec<Type>>();
                        arg_types.push(ast_ty);

                        // create equation for all matches for the function
                        let possible = env
                            .resolve_all(&v.name)
                            .iter()
                            .map(|v| {
                                let node = v.clone();
                                let ty = v.borrow().ty.clone();
                                TypeNodePair::new(ty, node.clone())
                            })
                            .collect();
                        let ty = TypeNodePair::new(Type::Func(arg_types), *f.clone());
                        self.check.add(TypeEquation::new(ty, possible));
                    }

                    Ast::Function(_body, _args) => {
                        // Already resolved
                        // TODO:
                        // args are unbound
                        // bind any free variables in the body
                    }
                    _ => unimplemented!(),
                }
                (ast.clone(), env)
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

    pub fn declare(&self, name: &str, rhs: AstNode) -> AstNode {
        let ty = rhs.borrow().ty.clone();

        let node = AstNode::new(Ast::Declare(name.into(), Box::new(rhs)), ty);
        node
    }

    fn substitute(&mut self, ty: TypeNodePair) -> TypeNodePair {
        match ty.ty {
            Type::Unknown(ty_id) => match self.check.get_type_by_id(&ty_id) {
                Some(v) => v,
                None => {
                    println!("Type missing from substitution table: {:?}", &ty);
                    unimplemented!()
                }
            },
            Type::Func(sig) => {
                let new_sig = sig.into_iter().map(|v| {
                    let p = self.substitute(TypeNodePair::new(v, ty.node.clone()));
                    p.ty
                }).collect();
                TypeNodePair::new(Type::Func(new_sig), ty.node)
            }
            _ => ty.clone(),
        }
    }

    pub fn resolve(&mut self, ast: AstNode, env: Environment) -> (AstNode, Environment) {
        // name resolution
        let (mut ast, mut env) = self.name_resolve(ast, env);
        ast.try_borrow_mut().unwrap();
        // infer all types
        let mut syms = self.check.unify_all();
        ast.try_borrow_mut().unwrap();

        println!("AST: {}", &ast);
        println!("C: {}", self.check);
        println!("ENV: {}", env);
        ast.try_borrow_mut().unwrap();

        let _ = visitor::visit(ast.clone(), self, &mut syms).unwrap();

        (ast, env)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;

    struct Test {}
    impl visitor::Visitor<()> for Test {
        fn exit(&mut self, e: AstNode, n: &mut ()) -> visitor::VResult {
            println!("AST: {}", e);
            //n.push(e.clone());
            Ok(())
        }
    }

    #[test]
    fn test_resolve() {
        let env = base_env();
        let mut b = AstBuilder::default();

        let v = b.int(1);
        let decl1 = b.declare("a", v);
        let v = b.var("a".into());
        let decl2 = b.declare("b", v);
        let block = b.block(vec![decl1, decl2]);
        let ast = b.declare("c", block.clone());

        let (ast, env) = b.resolve(ast, env.clone());

        println!("a = {:?}", env.resolve(&"a".into()));
        println!("b = {:?}", env.resolve(&"b".into()));
        println!("c = {:?}", env.resolve(&"c".into()));

        // a and b should not be visible from the outer lexical scope
        // c should be visible though
        assert_eq!(env.resolve(&"a".into()), None);
        assert_eq!(env.resolve(&"b".into()), None);
        assert!(env.resolve(&"c".into()).is_some());
    }

    #[test]
    fn test_binary() {
        let mut b = AstBuilder::default();
        let env = base_env();
        let one = b.int(1);
        let two = b.int(2);
        let add = b.binary("+", one, two);
        let ast = b.declare("f", add);

        let (ast, env) = b.resolve(ast, env.clone());
        println!("{}", env);
        //println!("{:?}", env.resolve(&"f".into()));
        println!("{}", &ast);
        println!("{}", b.check);

        let f = env.resolve(&"f".into()).unwrap();
        println!("f0 = {:?}", f);
        match &f.borrow().value {
            Ast::Apply(f, _args) => {
                println!("f1 = {:?}", f);
                println!("f2 = {:?}", f.borrow().ty);
            }
            _ => unreachable!()
        }

        // make sure type is correct
        assert_eq!(ast.borrow().ty, Type::Int);

        //let mut v = Test {};
        //let _ = visitor::visit(ast.clone(), &mut v, &mut ()).unwrap();

    }

    #[test]
    fn name_resolve() {
        let mut b = AstBuilder::default();
        let mut env = Environment::default();
        env.define("a".into(), b.int(1)); 
        let v = b.var("a");
        println!("AST:{}", &v);
        match &v.borrow().value {
            Ast::Variable(v) => {
                assert_eq!(v.bound, None);
            }
            _ => unreachable!()
        }
        let (ast, env) = b.name_resolve(v, env);
        println!("ENV:{}", env);
        println!("AST:{}", &ast);

        let inner = &ast.borrow();
        let ref_node = match &inner.value {
            Ast::Variable(v) => {
                v.bound.clone()
            }
            _ => unreachable!()
        };

        // make sure the types match
        assert_eq!(ref_node.unwrap().borrow().ty, Type::Int);
        assert_eq!(inner.ty, Type::Int);

    }

    #[test]
    fn visit() {
    }

}
