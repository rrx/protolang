use crate::ast::*;
use crate::typesystem::{Type, TypeChecker};

#[derive(Default)]
struct AstBuilder {
    check: TypeChecker,
}

impl AstBuilder {
    pub fn int(&self, i: u64, env: Environment) -> AstNode {
        AstNode {
            value: Ast::Literal(Literal::Int(i)),
            ty: Type::Int,
            env
        }
    }

    pub fn ident(&mut self, n: &str, env: Environment) -> AstNode {
        let ty = match env.resolve(&n.to_string()) {
            Some(v) => {
                v.ty.clone()
            }
            None => {
                println!("Unable to find symbol: {}", n);
                unimplemented!();
            }
        };

        AstNode {
            value: Ast::Ident(n.into()),
            ty,
            env
        }
    }

    pub fn block(&self, exprs: Vec<AstNode>, env: Environment) -> AstNode {
        let ty = exprs.iter().last().map_or(Type::Unit, |x| x.ty.clone());
        AstNode {
            value: Ast::Block(exprs),
            ty,
            env
        }
    }

    pub fn binary(&mut self, lhs: AstNode, rhs: AstNode, env: Environment) -> AstNode {
        let ty_ret = self.check.new_unknown_type();
        let ty_f = Type::Func(vec![lhs.ty.clone(), rhs.ty.clone(), ty_ret.clone()]);

        let name = "+".into();
        
        // create equations for all matches
        let possible = env.resolve_all(&name).iter().map(|v| v.ty.clone()).collect();
        self.check.add(TypeEquation::new(ty_f.clone(), possible));

        // type of this operation
        let f = AstNode {
            value: Ast::Ident(name),
            ty: ty_f,
            env: env.clone()
        };

        let args = vec![lhs, rhs];
        AstNode {
            value: Ast::Apply(f.into(), args),
            ty: ty_ret,
            env
        }
    }

    pub fn declare(&self, name: String, rhs: AstNode, mut env: Environment) -> (AstNode, Environment) {
        let ty = rhs.ty.clone();

        // add rhs to the environment
        env.define(name.clone(), rhs.clone());

        let mut node = AstNode {
            value: Ast::Declare(name.clone(), Box::new(rhs)),
            ty,
            env: env.clone()
        };
        (node, env)
    }

    fn substitute(&mut self, ty: Type) -> Type {
        match ty {
            Type::Unknown(ty_id) => {
                self.check.get_type_by_id(&ty_id).expect("Type missing from substitution table")
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

    pub fn resolve(&mut self, ast: &mut AstNode, mut env: Environment) -> Environment {
        self.check.unify_all();
        if ast.ty.is_unknown_recursive() {
            ast.ty = self.substitute(ast.ty.clone());
        }

        match &mut ast.value {
            Ast::Literal(Literal::Int(_)) => {
            }

            Ast::Literal(Literal::Float(_)) => {
            }

            Ast::Extern(args) => {
            }

            Ast::Apply(f, args) => {
                for arg in args {
                    env = self.resolve(arg, env);
                }

                env = self.resolve(f, env);

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


            Ast::Block(ref mut exprs) => {
                //let mut updated = vec![];
                for expr in exprs {
                    env = self.resolve(expr, env);
                    //updated.push(node);
                }
                //ast = self.block(updated, env.clone());
            }

            Ast::Declare(ref name, ref mut rhs) => {
                env = self.resolve(rhs, env);
                env.define(name.clone(), *rhs.clone());
            }
            _ => {
                println!("{:?}", &ast);
                unimplemented!()
            }
        }
        env
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

        let block = {
            let v = b.int(1, env.clone());
            let (decl1, env) = b.declare("a".into(), v, env.clone());
            let v = b.ident("a".into(), env.clone());
            let (decl2, env) = b.declare("b".into(), v, env.clone());
            let block = b.block(vec![decl1, decl2], env.clone());
            block
        };
        let (decl3, env) = b.declare("c".into(), block.clone(), env.clone());
        let mut ast = b.block(vec![decl3], env.clone());

        let env = b.resolve(&mut ast, env.clone());

        println!("{:?}", env);
        println!("{:?}", &ast);
        println!("{}", b.check);
        println!("a = {:?}", env.resolve(&"a".into()));
        println!("b = {:?}", env.resolve(&"b".into()));
        println!("c = {:?}", env.resolve(&"c".into()));

        //env.resolve_all(&"+".into()).iter().for_each(|v| {
            //println!("{}", v);
        //});
        //println!("{:?}", ast);
    }


    #[test]
    fn test_binary() {
        let mut b = AstBuilder::default();
        let env = base_env();
        let f = {
            let one = b.int(1, env.clone());
            let two = b.int(2, env.clone());
            let add = b.binary(one, two, env.clone());
            let (f, env) = b.declare("f".into(), add, env.clone());
            f
        };
        let mut ast = b.block(vec![f], env.clone());
        let env = ast.env.clone();

        let env = b.resolve(&mut ast, env.clone());
        println!("{:?}", env);
        //println!("{:?}", env.resolve(&"f".into()));
        println!("{:?}", &ast);
        println!("{}", b.check);
    }
}



