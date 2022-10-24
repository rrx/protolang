use crate::ast::*;
use crate::typesystem::{Type, TypeChecker};

#[derive(Default)]
struct AstBuilder {
    check: TypeChecker,
}

impl AstBuilder {
    pub fn int(&self, i: u64) -> AstNode {
        AstNode {
            value: Ast::Literal(Literal::Int(i)),
            ty: Type::Int,
        }
    }

    pub fn ident(&mut self, n: &str) -> AstNode {
        AstNode {
            value: Ast::Ident(n.into()),
            ty: self.check.new_unknown_type(),
        }
    }
    
    pub fn ident_resolve(&mut self, n: &str, env: Environment) -> AstNode {
        match env.resolve(&n.to_string()) {
            Some(v) => {
                v.clone()
            }
            None => {
                println!("Unable to find symbol: {}", n);
                unimplemented!();
            }
        }
    }

    pub fn block(&self, exprs: Vec<AstNode>) -> AstNode {
        let ty = exprs.iter().last().map_or(Type::Unit, |x| x.ty.clone());
        AstNode {
            value: Ast::Block(exprs),
            ty,
        }
    }

    pub fn binary(&mut self, name: &str, lhs: AstNode, rhs: AstNode) -> AstNode {
        let ty_ret = self.check.new_unknown_type();
        let ty_f = Type::Func(vec![lhs.ty.clone(), rhs.ty.clone(), ty_ret.clone()]);

        //let name = "+".into();
        
        // type of this operation
        let f = AstNode {
            value: Ast::Ident(name.into()),
            ty: ty_f,
        };

        let args = vec![lhs, rhs];
        AstNode {
            value: Ast::Apply(f.into(), args),
            ty: ty_ret,
        }
    }

    fn name_resolve(&mut self, mut ast: AstNode, mut env: Environment) -> (AstNode, Environment) {
        match ast.value {
            Ast::Literal(_) => (ast, env),

            Ast::Declare(name, rhs) => {
                let (new_rhs, mut env) = self.name_resolve(*rhs, env.clone());
                env.define(name.clone(), new_rhs.clone());
                (AstNode { value: Ast::Declare(name, Box::new(new_rhs.clone())), ty: new_rhs.ty }, env)
            }

            Ast::Ident(ref name) => {
                match env.resolve(&name) {
                    Some(v) => {
                        ast.ty = v.ty.clone();
                    }
                    None => {
                        unimplemented!();
                    }
                }
                (ast, env)
            }

            Ast::Apply(ref f, ref args) => {
                match &f.value {
                    Ast::Ident(name) => {
                        let mut arg_types = args.iter().map(|a| a.ty.clone()).collect::<Vec<Type>>();
                        arg_types.push(ast.ty.clone());

                        // create equation for all matches for the function
                        let possible = env.resolve_all(&name).iter().map(|v| v.ty.clone()).collect();
                        let ty = Type::Func(arg_types);
                        self.check.add(TypeEquation::new(ty, possible));
                    }

                    Ast::Function(body, args) => {
                        // Already resolved
                    }
                    _ => unimplemented!()
                }
                (ast, env)
            }

            Ast::Block(mut exprs) => {
                let mut local_env = env.clone();
                let mut updated = vec![];
                for mut expr in exprs {
                    let (new_ast, new_env) = self.name_resolve(expr, local_env);
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
        let ty = rhs.ty.clone();

        let node = AstNode {
            value: Ast::Declare(name.into(), Box::new(rhs)),
            ty,
        };
        node
    }

    fn substitute(&mut self, ty: Type) -> Type {
        match ty {
            Type::Unknown(ty_id) => {
                match self.check.get_type_by_id(&ty_id) {
                    Some(v) => v,
                    None => {
                        println!("Type missing from substitution table: {:?}", &ty);
                        unimplemented!()
                    }
                }
            }
            Type::Func(sig) => {
                let new_sig = sig.into_iter().map(|v| {
                    self.substitute(v)
                }).collect();
                Type::Func(new_sig)
            }
            _ => ty.clone()
        }
    }

    pub fn resolve(&mut self, ast: AstNode, env: Environment) -> (AstNode, Environment) {
        // name resolution
        let (mut ast, mut env) = self.name_resolve(ast, env);
        // infer all types
        self.check.unify_all();

        println!("AST: {:?}", &ast);
        println!("C: {}", self.check);
        println!("ENV: {:?}", env);

        /*
        if ast.ty.is_unknown_recursive() {
            ast.ty = self.substitute(ast.ty.clone());
        }

        match ast.value {
            Ast::Literal(Literal::Int(_)) => {
            }

            Ast::Literal(Literal::Float(_)) => {
            }

            Ast::Extern(_) => {
            }

            Ast::Apply(ref f, ref args) => {
                for arg in args {
                    //env = self.resolve(arg, env);
                }

                //env = self.resolve(f, env);

                /*
                match &f.value {
                    Ast::Ident(s) => {
                        match ast.env.resolve(&s) {
                            Some(v) => {
                                ast.value = v.value.clone();
                                ast.ty = v.ty.clone();
                            }
                            None => {
                                unimplemented!();
                            }
                        }
                    }
                    Ast::Extern(args) => {
                    }
                    _ => {
                        println!("{:?}", &f);
                        unimplemented!()
                    }
                }
                */
            }

            Ast::Ident(ref s) => {
                match env.resolve(&s) {
                    Some(v) => {
                        ast.ty = v.ty.clone();
                        //ast = v.clone()
                        //ast.value = v.value.clone();
                        //ast.ty = v.ty.clone();
                    }
                    None => {
                        unimplemented!();
                    }
                }
            }


            Ast::Block(ref mut exprs) => {
                //let mut updated = vec![];
                for expr in exprs {
                    //env = self.resolve(expr, env);
                    //updated.push(node);
                }
                //ast = self.block(updated, env.clone());
            }

            Ast::Declare(ref name, ref rhs) => {
                let (rhs, new_env) = self.resolve(*rhs.clone(), env.clone()); 
                //env = self.resolve(rhs, env);
                env.define(name.clone(), rhs);//*rhs.clone());
            }
            _ => {
                println!("{:?}", &ast);
                unimplemented!()
            }
        }
        */ */

        (ast, env)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use log::debug;
    use test_log::test;

    #[test]
    fn test_resolve() {
        let env = base_env();
        let mut b = AstBuilder::default();

        let v = b.int(1);
        let decl1 = b.declare("a", v);
        let v = b.ident("a".into());
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
        println!("{:?}", env);
        //println!("{:?}", env.resolve(&"f".into()));
        println!("{:?}", &ast);
        println!("{}", b.check);
    }
}



