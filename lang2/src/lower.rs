use crate::ast::*;
use crate::typesystem::Type;
use codegen::hir;
use std::error::Error;

struct Builder {
    next_id: usize,
}

fn convert_type(ty: &Type) -> hir::Type {
    match ty {
        Type::Int => hir::Type::i64(),
        _ => unimplemented!(),
    }
}

impl Builder {
    fn new() -> Self {
        Self { next_id: 0 }
    }

    fn next_definition(&mut self) -> hir::DefinitionId {
        let d = hir::DefinitionId(self.next_id);
        self.next_id += 1;
        d
    }

    fn lower(&mut self, ast: &AstNode) -> Result<hir::Ast, Box<dyn Error>> {
        match &ast.borrow().value {
            Ast::Literal(Literal::Bool(b)) => Ok(hir::Ast::bool(*b)),

            Ast::Literal(Literal::Int(u)) => Ok(hir::Ast::u64(*u)),

            Ast::Literal(Literal::Float(f)) => Ok(hir::Ast::f64(*f)),

            Ast::Literal(Literal::String(s)) => Ok(hir::Ast::string(s.clone())),

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
                    expr: rhs.into(),
                };
                Ok(hir::Ast::Definition(def))
            }

            Ast::Builtin(name, args) => {
                let lhs = args.get(0).unwrap();
                let rhs = args.get(1).unwrap();
                Ok(hir::Ast::Builtin(hir::Builtin::AddInt(
                    self.lower(&lhs)?.into(),
                    self.lower(&rhs)?.into(),
                )))
            }

            Ast::Function { params, body, ty } => {
                let body = self.lower(body)?;

                let params = params
                    .iter()
                    .map(|a| hir::Variable {
                        definition: None,
                        definition_id: self.next_definition(),
                        name: None,
                    })
                    .collect::<Vec<_>>();

                let parameters = vec![];

                let return_type = convert_type(ty.get(ty.len() - 1).unwrap());

                let f_type = hir::FunctionType {
                    parameters,
                    return_type: return_type.into(),
                    is_varargs: false,
                    export: true,
                };

                Ok(hir::Ast::Lambda(hir::Lambda {
                    args: params,
                    body: body.into(),
                    typ: f_type,
                }))
            }

            Ast::Apply(f, args) => {
                println!("apply {:?}", &f);
                let bound = match &f.borrow().value {
                    Ast::Variable(v) => v.clone().bound.unwrap(),
                    _ => unreachable!(),
                };

                let value = &bound.borrow().value;
                match &value {
                    Ast::Builtin(name, _args) => {
                        let lhs = args.get(0).unwrap();
                        let rhs = args.get(1).unwrap();
                        Ok(hir::Ast::Builtin(hir::Builtin::AddInt(
                            self.lower(&lhs)?.into(),
                            self.lower(&rhs)?.into(),
                        )))
                    }
                    Ast::Function { params, body, ty } => {
                        let body = self.lower(body)?;

                        let params = params
                            .iter()
                            .map(|a| hir::Variable {
                                definition: None,
                                definition_id: hir::DefinitionId(0),
                                name: None,
                            })
                            .collect::<Vec<_>>();

                        let parameters = vec![];
                        let return_type = hir::Type::Primitive(hir::PrimitiveType::Unit);

                        let f_type = hir::FunctionType {
                            parameters,
                            return_type: return_type.into(),
                            is_varargs: false,
                            export: true,
                        };

                        Ok(hir::Ast::Lambda(hir::Lambda {
                            args: params,
                            body: body.into(),
                            typ: f_type,
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
    use codegen::{
        llvm::LLVMBackend,
        llvm::LLVMBackendContext,
        lower::{Context, Lower},
    };
    use log::debug;
    use logic::UnifyResult;
    use test_log::test;

    #[test]
    fn lower_binary() {
        let mut a = AstBuilder::default();
        let one = a.int(1);
        let two = a.int(2);
        let add = a.binary("+", one.clone(), two);
        //let f = b.declare("f", add);

        let f1 = {
            //let p = a.var("p");
            //let add = b.binary("+", p, one.clone());
            //a.func(vec![Type::Int, Type::Int], add.clone())
            a.func(vec![Type::Int], add.clone())
        };
        let df1 = a.declare("f1", f1.clone());

        let f2 = {
            let p = a.int(3);
            let add = a.binary("+", p, one);
            a.func(vec![Type::Int], add)
        };
        let df2 = a.declare("f2", f2.clone());

        //let call = b.apply(f, vec![add]);
        //let call = a.apply(f1, vec![]);
        let block = a.block(vec![add]);
        let main_f = a.func(vec![Type::Int], block);
        let main = a.declare("main", main_f);

        // name resolution and type inference
        let env = base_env();
        let (res, ast1, env) = a.resolve(main, env.clone());
        println!("{}", env);
        //println!("{}", &ast1);
        assert_eq!(res, UnifyResult::Ok);

        /*
        let (res, ast2, env) = a.resolve(f1, env.clone());
        println!("{}", env);
        println!("{}", &ast2);
        assert_eq!(res, UnifyResult::Ok);
        */

        let (res, ast3, env) = a.resolve(df2, env.clone());
        println!("{}", env);
        //println!("{}", &ast3);
        assert_eq!(res, UnifyResult::Ok);

        // lower to codegen ir
        let mut b = Builder::new();
        let hir1 = b.lower(&ast1).unwrap();
        let hir2 = b.lower(&ast3).unwrap();
        println!("{}", &hir1);
        println!("{}", &hir2);

        let context = LLVMBackendContext::new();
        let mut backend = LLVMBackend::new(&context);

        // add modules
        backend.compile_module("test", hir1).unwrap();
        backend.compile_module("test2", hir2).unwrap();

        // execute
        let ret = backend.run().unwrap();
        println!("ret: {}", &ret);
        assert_eq!(3, ret);
    }
}
