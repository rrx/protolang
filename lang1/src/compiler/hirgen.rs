use crate::ast::*;
use codegen::hir;
use super::CodeGenLower;
use std::error::Error;
use crate::tokens::{Tok};
use std::rc::Rc;
use crate::results::{Compiler, LangError};

impl crate::compiler::env::LayerValue for hir::Definition {}
pub type Environment = crate::compiler::env::EnvLayers<String, hir::Definition>;

struct HIRCodeGen {
    next_definition_id: usize,
    results: Compiler,
    env: Environment
}

impl Default for HIRCodeGen {
    fn default() -> Self {
        Self { next_definition_id: 0, results: Compiler::default(), env: Environment::default() }
    }
}

impl HIRCodeGen {
    fn get_next_definition_id(&mut self) -> hir::DefinitionId {
        let d = hir::DefinitionId(self.next_definition_id);
        self.next_definition_id += 1;
        d
    }
}

impl CodeGenLower for HIRCodeGen {
    type Input = ExprNode;
    type Output = hir::Ast;
    type Error = Box<dyn Error>;
    fn lower(&mut self, node: &ExprNode) -> Result<hir::Ast, Box<dyn Error>> {
        match &node.value {
            Expr::Block(exprs) | Expr::Program(exprs) => {
                let mut ir_exprs = vec![];
                for p in exprs {
                    let node = self.lower(p)?;
                    ir_exprs.push(node);
                }

                Ok(hir::Ast::Sequence(hir::Sequence { statements: ir_exprs }))
            }

            Expr::Literal(Tok::FloatLiteral(f)) => {
                Ok(hir::Ast::Literal(hir::Literal::Integer(0, hir::IntegerKind::U64)))
            }
            Expr::Literal(Tok::IntLiteral(i)) => { 
                Ok(hir::Ast::Literal(hir::Literal::Integer(*i, hir::IntegerKind::U64)))
            }
            Expr::Literal(Tok::BoolLiteral(b)) => {
                Ok(hir::Ast::Literal(hir::Literal::Bool(*b)))
            }
            Expr::Literal(Tok::StringLiteral(s)) => {
                Ok(hir::Ast::Literal(hir::Literal::CString(s.clone())))
            }
            Expr::Literal(_) => unimplemented!(),

            Expr::Binary(op, left, right) => match &op.value {
                Operator::Assign => {
                    let ir_left = self.lower(left)?;
                    let ir_right = self.lower(right)?;
                    Ok(hir::Ast::Assignment(hir::Assignment { lhs: Box::new(ir_left), rhs: Box::new(ir_right) }))
                }

                Operator::Declare => {
                    let name = left.try_ident().unwrap().name;
                    let ir_right = self.lower(right)?;
                    let v = hir::Definition {
                        variable: self.get_next_definition_id(),
                        name: Some(name.clone()),
                        expr: Box::new(ir_right)
                    };
                    self.env.define(name.clone(), v.clone());
                    Ok(hir::Ast::Definition(v))
                }

                _ => {
                    let ir_left = self.lower(left)?;
                    let ir_right = self.lower(right)?;
                    let f = match &op.value {
                        Operator::Plus => hir::Builtin::AddInt(Box::new(ir_left), Box::new(ir_right)),
                        Operator::Minus => hir::Builtin::SubInt(Box::new(ir_left), Box::new(ir_right)),
                        _ => unimplemented!()
                    };

                    Ok(hir::Ast::Builtin(f))
                }
            },

            Expr::Lambda(f) => {
                let mut parameters = vec![];
                for p in &f.params.value {
                    let ty = hir::Type::Primitive(hir::PrimitiveType::Integer(hir::IntegerKind::U64));
                    parameters.push(ty);
                    //let ty = self.checker.new_unknown_type();
                    //let name = p.try_ident().unwrap().name;
                    //let ir = self.make_ident_from_name(name.clone(), ty, node.context.to_location());
                    //let ir = self.lower(p)?;
                    //self.env.define(name.clone(), node.clone());
                    //let ir = self.make_declare(name, ir, p.context.to_location());
                    //ir_args.push(ir);
                }

                //let return_type = hir::Type::Primitive(hir::PrimitiveType::Unit);
                let return_type = hir::Type::Primitive(hir::PrimitiveType::Integer(hir::IntegerKind::U64));

                let typ = hir::FunctionType {
                    parameters: parameters,
                    return_type: Box::new(return_type),
                    is_varargs: false,
                    export: true
                };

                let body = self.lower(&f.expr)?;
                let f = hir::Lambda {
                    args: vec![],
                    body: Box::new(body),
                    typ
                };

                Ok(hir::Ast::Lambda(f))
            }

            /*
            Expr::Ident(s) => {
                if let Some(def) = self.env.resolve(&s.name) {
                    Ok(def.clone().into())
                } else {
                    log::info!("not found: {}", &s.name);
                    unimplemented!()
                }
            }


            Expr::Apply(f, args) => {
                let ir_func = self.lower(f)?;

                let mut ir_args = vec![];
                for arg in args {
                    let node = self.lower(arg)?;
                    ir_args.push(node);
                }

                Ok(self.make_apply(ir_func, ir_args))
            }

            Expr::Lambda(f) => {
                let mut ir_args = vec![];
                let orig_env = self.env.clone();
                //self.env = self.env.clone();//push();
                for p in &f.params.value {
                    let ty = self.checker.new_unknown_type();
                    let name = p.try_ident().unwrap().name;
                    //let ir = self.make_ident_from_name(name.clone(), ty, node.context.to_location());
                    //let ir = self.lower(p)?;
                    //self.env.define(name.clone(), node.clone());
                    //let ir = self.make_declare(name, ir, p.context.to_location());
                    //ir_args.push(ir);
                }

                // use local environment when parsing body
                let body = self.lower(&f.expr)?;

                // use original context for the node
                let ir = IR::new_with_location(
                    IRValue::Function(Box::new(body), ir_args),
                    self.checker.new_unknown_type(),
                    node.context.to_location(),
                    self.env.clone(),
                );

                self.env = orig_env;//self.env.pop().unwrap();
                Ok(ir)
            }

            Expr::Prefix(op, right) => {
                let name = op_name(op);
                let ir_right = self.lower(right)?;
                Ok(self.make_apply_by_name(name, vec![ir_right], node.context.to_location()))
            }


            Expr::Ternary(op, a, b, c) => match op.value {
                Operator::Conditional => {
                    let ir_a = self.lower(a)?;
                    let ir_b = self.lower(b)?;
                    let ir_c = self.lower(c)?;
                    Ok(self.make_apply_by_name(
                        "cond".into(),
                        vec![ir_a, ir_b, ir_c],
                        node.context.to_location(),
                    ))
                }
                _ => unimplemented!(),
            },
            */

            _ => {
                log::debug!("Unimplemented: {:?}", &node);
                unimplemented!()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;
    use codegen::llvm;
    use codegen::hir;

    #[test]
    fn function() {
        let mut gen = HIRCodeGen::default();
        let mut args = llvm::CompileArgs::default();
        args.stdout = true;
        
        let one = hir::Ast::Literal(hir::Literal::Integer(1, hir::IntegerKind::U64));
        let two = hir::Ast::Literal(hir::Literal::Integer(2, hir::IntegerKind::U64));
        let add = hir::Ast::Builtin(hir::Builtin::AddInt(Box::new(one), Box::new(two)));
        let return_type = hir::Type::Primitive(hir::PrimitiveType::Integer(hir::IntegerKind::U64));

        let typ = hir::FunctionType {
            parameters: vec![],
            return_type: Box::new(return_type),
            is_varargs: false,
            export: true
        };

        let body = add;
        let f = hir::Ast::Lambda(hir::Lambda {
            args: vec![],
            body: Box::new(body),
            typ
        });

        let name = "main".to_string();
        let v = hir::Ast::Definition(hir::Definition {
            variable: gen.get_next_definition_id(),
            name: Some(name),
            expr: Box::new(f)
        });

        println!("{:?}", &v);
        llvm::compile(&String::from("test"), v, &args).unwrap();
    }

    #[test]
    fn hir_lower() {
        let mut args = llvm::CompileArgs::default();
        args.stdout = true;
        let mut gen = HIRCodeGen::default();
        let expr = gen.results.parse_str("let main = \\x -> 1 + 2").unwrap();
        println!("{:?}", expr);
        let ir = gen.lower(&expr).unwrap();
        println!("{:?}", ir);
        gen.results.print();
        llvm::compile(&String::from("test"), ir, &args).unwrap();
    }
}


