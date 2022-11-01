use crate::ast::*;
use codegen::{hir};
use std::error::Error;
use crate::visitor::*;
use crate::typesystem::{Type};

struct Builder {
    next_id: usize
    //codegen: Lower<'a>
}

impl Visitor<()> for Builder {
    fn exit(&mut self, ast: AstNode, _: &mut ()) -> VResult {
        //lower(&ast).unwrap();
        Ok(())
    }
}

fn convert_type(ty: &Type) -> hir::Type {
    match ty {
        Type::Int => hir::Type::i64(),
        _ => unimplemented!()
    }
}

impl Builder {
    fn new() -> Self {
        Self { next_id: 0 } // codegen: Lower::new() }
    }

    fn next_definition(&mut self) -> hir::DefinitionId {
        let d = hir::DefinitionId(self.next_id);
        self.next_id += 1;
        d
    }

    fn lower(&mut self, ast: &AstNode) -> Result<hir::Ast, Box<dyn Error>> {
        match &ast.borrow().value {
            Ast::Literal(Literal::Bool(b)) => {
                Ok(hir::Ast::bool(*b))
            }

            Ast::Literal(Literal::Int(u)) => {
                Ok(hir::Ast::u64(*u))
            }

            Ast::Literal(Literal::Float(f)) => {
                Ok(hir::Ast::f64(*f))
            }

            Ast::Literal(Literal::String(s)) => {
                Ok(hir::Ast::string(s.clone()))
            }

            Ast::Block(exprs) => {
                let mut lowered = vec![];
                for expr in exprs {
                    lowered.push(self.lower(expr)?);
                }

                Ok(hir::Sequence::new(lowered).into())
            }

            Ast::Declare(name, rhs) => {
                let rhs = self.lower(rhs)?;
                let def = hir::Definition {
                    variable: hir::DefinitionId(0),
                    name: Some(name.clone()),
                    expr: rhs.into()
                };
                Ok(hir::Ast::Definition(def))
            }

            Ast::Builtin(name, args) => {
                let lhs = args.get(0).unwrap();
                let rhs = args.get(1).unwrap();
                Ok(hir::Ast::Builtin(hir::Builtin::AddInt(self.lower(&lhs)?.into(), self.lower(&rhs)?.into())))
            }

            Ast::Function(body, params, sig) => {
                let body = self.lower(body)?;

                let params = params.iter().map(|a| {
                    hir::Variable {
                        definition: None,
                        definition_id: self.next_definition(),
                        name: None
                    }
                }).collect::<Vec<_>>();

                let parameters = vec![];

                let return_type = convert_type(sig.get(sig.len()-1).unwrap());

                let f_type = hir::FunctionType {
                    parameters,
                    return_type: return_type.into(),
                    is_varargs: false,
                    export: true
                };

                Ok(hir::Ast::Lambda(hir::Lambda {
                    args: params,
                    body: body.into(),
                    typ: f_type
                }))
            }

            Ast::Apply(f, args) => {
                println!("apply {:?}", &f);
                let bound = match &f.borrow().value {
                    Ast::Variable(v) => {
                        v.clone().bound.unwrap()
                    }
                    _ => unreachable!()
                };

                let value = &bound.borrow().value;
                match &value {
                    Ast::Builtin(name, _args) => {
                        let lhs = args.get(0).unwrap();
                        let rhs = args.get(1).unwrap();
                        Ok(hir::Ast::Builtin(hir::Builtin::AddInt(self.lower(&lhs)?.into(), self.lower(&rhs)?.into())))
                    }
                    Ast::Function(body, args, sig) => {
                        let body = self.lower(body)?;

                        let args = args.iter().map(|a| {
                            hir::Variable {
                                definition: None,
                                definition_id: hir::DefinitionId(0),
                                name: None
                            }
                        }).collect::<Vec<_>>();

                        let parameters = vec![];
                        let return_type = hir::Type::Primitive(hir::PrimitiveType::Unit);

                        let f_type = hir::FunctionType {
                            parameters,
                            return_type: return_type.into(),
                            is_varargs: false,
                            export: true
                        };

                        Ok(hir::Ast::Lambda(hir::Lambda {
                            args,
                            body: body.into(),
                            typ: f_type
                        }))
                    }
                    _ => {
                        println!("{:?}", value);
                        unimplemented!()
                    }
                }
            }

            _ => {
                println!("{:?}", ast);
                unimplemented!()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use log::debug;
    use test_log::test;
    use logic::UnifyResult;
    use codegen::{lower::{Lower, Context}};

    #[test]
    fn lower_binary() {

        let mut b = AstBuilder::default();
        let env = base_env();
        let one = b.int(1);
        let two = b.int(2);
        let add = b.binary("+", one.clone(), two);
        //let f = b.declare("f", add);

        let f1 = {
            let p = b.var("p");
            let add = b.binary("+", p, one.clone());
            let f = b.func(vec![Type::Int, Type::Int], add);
            let f = b.declare("f1", f);
            f
        };

        let f2 = {
            let p = b.int(3);
            let add = b.binary("+", p, one);
            let f = b.func(vec![Type::Int], add);
            let f = b.declare("f2", f);
            f
        };

        //let call = b.apply(f, vec![add]);
        let block = b.block(vec![add]);
        let main_f = b.func(vec![Type::Int], block);
        let main = b.declare("main", main_f);

        let context = Context::create();
        let mut lower = Lower::new(&context);

        let (res, ast1, env) = b.resolve(main, env.clone());
        println!("{}", env);
        println!("{}", &ast1);
        assert_eq!(res, UnifyResult::Ok); 

        /*
        let (res, ast2, env) = b.resolve(f1, env.clone());
        println!("{}", env);
        println!("{}", &ast2);
        assert_eq!(res, UnifyResult::Ok); 
        */

        let (res, ast3, env) = b.resolve(f2, env.clone());
        println!("{}", env);
        println!("{}", &ast3);
        assert_eq!(res, UnifyResult::Ok); 

        let mut b = Builder::new();
        let hir1 = b.lower(&ast1).unwrap();
        println!("{}", &hir1);
        lower.module("test", hir1).unwrap();

        let hir2 = b.lower(&ast3).unwrap();
        lower.module("test2", hir2).unwrap();

        let ret = lower.run().unwrap();
        println!("ret: {}", &ret);
        assert_eq!(3, ret);
    }
}
