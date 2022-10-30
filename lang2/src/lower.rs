use crate::ast::*;
use codegen::hir;
use std::error::Error;

fn lower(ast: &AstNode) -> Result<hir::Ast, Box<dyn Error>> {
    match &ast.borrow().value {
        Ast::Literal(Literal::Bool(b)) => {
            Ok(hir::Ast::Literal(hir::Literal::Bool(*b)))
        }

        Ast::Literal(Literal::Int(u)) => {
            Ok(hir::Ast::Literal(hir::Literal::Integer(*u, hir::IntegerKind::U64)))
        }

        Ast::Literal(Literal::Float(f)) => {
            unsafe {
                let u = std::mem::transmute(f);
                Ok(hir::Ast::Literal(hir::Literal::Float(u, hir::FloatKind::F64)))
            }
        }

        Ast::Literal(Literal::String(s)) => {
            Ok(hir::Ast::Literal(hir::Literal::CString(s.clone())))
        }

        Ast::Block(exprs) => {
            let mut lowered = vec![];
            for expr in exprs {
                lowered.push(lower(expr)?);
            }

            Ok(hir::Ast::Sequence(hir::Sequence { statements: lowered }))
        }

        Ast::Declare(name, rhs) => {
            let rhs = lower(rhs)?;
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
            Ok(hir::Ast::Builtin(hir::Builtin::AddInt(lower(&lhs)?.into(), lower(&rhs)?.into())))
        }

        Ast::Function(body, args) => {
            let body = lower(body)?;

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
                    Ok(hir::Ast::Builtin(hir::Builtin::AddInt(lower(&lhs)?.into(), lower(&rhs)?.into())))
                }
                Ast::Function(body, args) => {
                    let body = lower(body)?;

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

#[cfg(test)]
mod tests {
    use super::*;
    use log::debug;
    use test_log::test;
    use logic::UnifyResult;

    #[test]
    fn lower_binary() {
        let mut b = AstBuilder::default();
        let env = base_env();
        let one = b.int(1);
        let two = b.int(2);
        let add = b.binary("+", one, two);
        let f = b.declare("f", add);
        let block = b.block(vec![f]);
        let main_f = b.func("main", vec![], block);
        let main = b.declare("main", main_f);

        let (res, ast, env) = b.resolve(main, env.clone());
        println!("{}", env);
        println!("{}", &ast);
        assert_eq!(res, UnifyResult::Ok); 

        let hir = lower(&ast).unwrap();
        println!("{}", &hir);
        let mut args = codegen::llvm::CompileArgs::default();
        args.stdout = true;

        codegen::llvm::compile_and_run(&"test".into(), hir, &args).unwrap();
    }
}
