use crate::ast::*;
use crate::typesystem::{Type, TypeNodePair};
use crate::visitor;
use logic::{self, SymbolTable, TypeSignature, UnifyResult};

#[derive(Default)]
pub struct AstBuilder {
    pub state: TypeSystemContext,
    pub equations: Vec<logic::Expr<TypeNodePair>>,
}

impl visitor::Visitor<SymbolTable<TypeNodePair>> for AstBuilder {
    fn enter(&mut self, e: AstNode, n: &mut SymbolTable<TypeNodePair>) -> visitor::VResult {
        println!("Visit AST: {}", e);

        // this is crazy gymnastics to set the bound on a variable
        // there must be an easier way to do this.
        let maybe_v = {
            let inner = e.borrow();
            let value = &inner.value;
            match value {
                Ast::Apply(op, _args) => {
                    let op = op.clone();
                    let node = &mut op.borrow();
                    match &node.value {
                        Ast::Variable(v) => {
                            let unknown_ids = inner.ty.unknown_ids();
                            if let Some(type_id) = unknown_ids.iter().next() {
                                if let Some(r) = n.get(type_id) {
                                    let mut v = v.clone();
                                    v.bound = Some(r.node.clone());
                                    let ty = node.ty.clone();
                                    Some((op.clone(), v, ty))
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        }
                        _ => None,
                    }
                }
                _ => None,
            }
        };

        if let Some((op, v, ty)) = maybe_v {
            op.replace(AstNodeInner::new(Ast::Variable(v), ty));
        }

        let is_unknown = { e.borrow().ty.is_unknown_recursive() };

        // update the types in the AST
        if is_unknown {
            let before: TypeNodePair = e.clone().into();
            let after = self.substitute(e.clone().into(), n);
            println!("before {:?}", &before.node);
            println!("after {:?}", &after.node);
            println!("replacing {:?} => {:?}", &before.ty, &after.ty);
            // only replace the type.  The node returned by substitute
            // isn't always going to be the same node.  It might be the
            // parent node for the type.
            let mut new_inner = before.node.borrow().clone();
            new_inner.ty = after.ty.clone();
            //println!("replacing {:?}", &new_inner);
            e.replace(new_inner);
        }

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
            Type::var(self.state.next_id()),
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

    pub fn func(&mut self, sig: Vec<Type>, body: AstNode) -> AstNode {
        //let names = sig.iter().map(|p| {
        //AstNodeInner {
        //value: Variable::new("x".into()).into(),
        //ty: p.clone(),
        //}.into()
        //}).collect::<Vec<_>>();
        let ty = Type::Func(sig.clone());
        AstNode::new(Ast::Function { params: vec![], body: body.into(), ty: sig }, ty)
    }

    pub fn apply(&mut self, f: AstNode, args: Vec<AstNode>) -> AstNode {
        let inner = f.borrow();
        let ty = inner.ty.clone();
        let ty_ret = ty.children().last().unwrap().clone();
        AstNode::new(Ast::Apply(f.clone().into(), args), ty_ret)
    }

    pub fn binary(&mut self, name: &str, lhs: AstNode, rhs: AstNode) -> AstNode {
        let ty_ret = Type::var(self.state.next_id());
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
                let ast = match env.resolve(&v.name.as_ref().unwrap()) {
                    Some(resolved_v) => {
                        v.bind(resolved_v.clone());
                        let ty = resolved_v.borrow().ty.clone();
                        let v = Ast::Variable(v.clone());
                        let new_ast = AstNode::new(v, ty);
                        //println!("resolve: {:?} => {:?}", &ast, &new_ast);
                        new_ast
                    }
                    None => {
                        unimplemented!();
                    }
                };
                (ast, env)
            }

            Ast::Apply(f, ref args) => {
                let mut env = env.clone();
                for arg in args {
                    let (_f, this_env) = self.name_resolve(arg.clone(), env.clone());
                    env = this_env;
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
                            .resolve_all(&v.name.as_ref().unwrap())
                            .iter()
                            .map(|v| {
                                let node = v.clone();
                                let ty = v.borrow().ty.clone();
                                TypeNodePair::new(ty, node.clone())
                            })
                            .collect();
                        let ty = TypeNodePair::new(Type::Func(arg_types), *f.clone());
                        self.equations.push(logic::Expr::OneOf(ty, possible));
                    }

                    Ast::Function { params, body, ty } => {
                        // Already resolved
                        // TODO:
                        // args are unbound
                        // bind any free variables in the body
                    }
                    _ => unimplemented!(),
                }
                (ast.clone(), env)
            }

            Ast::Function { params, body, ty } => {
                let mut local_env = env.clone();
                for arg in params {
                    //local_env.define(
                }
                let (body, local_env) = self.name_resolve(*body.clone(), local_env);
                // nothing to do?
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
                println!("{:?}", &value);
                unimplemented!()
            }
        }
    }

    pub fn declare(&self, name: &str, rhs: AstNode) -> AstNode {
        let ty = rhs.borrow().ty.clone();

        let node = AstNode::new(Ast::Declare(name.into(), Box::new(rhs)), ty);
        node
    }

    fn substitute(&mut self, p: TypeNodePair, subst: &SymbolTable<TypeNodePair>) -> TypeNodePair {
        match p.ty {
            Type::Var(ty_id) => match logic::subst_get_type_by_id(&subst, &ty_id) {
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
                        let p = self.substitute(TypeNodePair::new(v, p.node.clone()), subst);
                        p.ty
                    })
                    .collect();
                TypeNodePair::new(Type::Func(new_sig), p.node)
            }
            _ => p.clone(),
        }
    }

    pub fn resolve(
        &mut self,
        ast: AstNode,
        env: Environment,
    ) -> (UnifyResult, AstNode, Environment) {
        // name resolution
        let (ast, env) = self.name_resolve(ast, env);
        ast.try_borrow_mut().unwrap();
        // infer all types

        let eqs = self.equations.clone();
        let (res, mut subst) = logic::unify_start(eqs);
        println!("AST: {}", &ast);
        println!("ENV: {}", env);
        println!("RES: {:?}", res);
        println!("SUB: {:?}", subst);
        if res == UnifyResult::Ok {
            let _ = visitor::visit(ast.clone(), self, &mut subst).unwrap();
            println!("SUB: {:?}", subst);
        }

        (res, ast, env)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;

    struct Test {}
    impl visitor::Visitor<()> for Test {
        fn exit(&mut self, e: AstNode, _n: &mut ()) -> visitor::VResult {
            println!("AST: {}", e);
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

        let (res, ast, env) = b.resolve(ast, env.clone());

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
    fn test_binary() {
        let mut b = AstBuilder::default();
        let env = base_env();
        let one = b.int(1);
        let two = b.int(2);
        let add = b.binary("+", one, two);
        let ast = b.declare("f", add);

        let (res, ast, env) = b.resolve(ast, env.clone());
        println!("{}", env);
        println!("{}", &ast);
        assert_eq!(res, UnifyResult::Ok);

        let call = env.resolve(&"f".into()).unwrap();
        println!("call = {:?}", call);
        match &call.borrow().value {
            Ast::Apply(op, _args) => {
                println!("op = {:?}", op);
                match &op.borrow().value {
                    Ast::Variable(s) => {
                        println!("var.bound = {:?}", s.bound);
                        // the variable for the operation should be bound
                        assert!(s.bound.is_some());
                    }
                    _ => unreachable!(),
                }
                println!("op.type = {:?}", op.borrow().ty);
            }
            _ => unreachable!(),
        }

        // make sure type is correct
        assert_eq!(ast.borrow().ty, Type::Int);
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
            _ => unreachable!(),
        }
        let (ast, env) = b.name_resolve(v, env);
        println!("ENV:{}", env);
        println!("AST:{}", &ast);

        let inner = &ast.borrow();
        let ref_node = match &inner.value {
            Ast::Variable(v) => v.bound.clone(),
            _ => unreachable!(),
        };

        // make sure the types match
        assert_eq!(ref_node.unwrap().borrow().ty, Type::Int);
        assert_eq!(inner.ty, Type::Int);
    }
}
