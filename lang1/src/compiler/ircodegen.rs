use crate::ast::*;
use crate::ir::*;

use crate::lexer::Location;
use crate::tokens::Tok;
use std::error::Error;

use super::check::{TypeChecker, TypeEquation};
use super::CodeGenLower;

use crate::results::{Compiler, LangError};

pub struct IRCodeGen {
    env: Environment,
    checker: TypeChecker<IR>,
    results: Compiler,
}

impl Default for IRCodeGen {
    fn default() -> Self {
        Self {
            env: base_env(),
            checker: TypeChecker::default(),
            results: Compiler::default(),
        }
    }
}

impl IRCodeGen {
    fn make_block(&mut self, nodes: Vec<IR>, loc: Location) -> IR {
        let block_ty = nodes.last().unwrap().ty.clone();
        let block = IR::new_with_location(
            IRValue::Block(nodes),
            block_ty.clone(),
            loc,
            self.env.clone(),
        );
        block
    }

    fn make_assign(&mut self, name: String, node: IR, loc: Location) -> IR {
        if let Some(v) = self.env.resolve(&name) {
            let left_ty = v.ty.clone();
            let right_ty = node.ty.clone();
            let result_ty = self.checker.new_unknown_type();
            let result = IR::new_with_location(
                IRValue::Assign(name, Box::new(node.clone())),
                result_ty.clone(),
                node.loc.clone(),
                self.env.clone(),
            );

            self.checker.add(TypeEquation::new(
                left_ty,
                Vec::from([right_ty.clone()]),
                node.clone(),
                self.env.clone(),
            ));

            self.checker.add(TypeEquation::new(
                result_ty,
                Vec::from([right_ty]),
                result.clone(),
                self.env.clone(),
            ));
            result
        } else {
            self.make_error(format!("Not found: {}", name), loc)
        }
    }

    fn make_declare(&mut self, name: String, node: IR, loc: Location) -> IR {
        let right_ty = node.ty.clone();
        self.env.define(name.clone(), node.clone());
        let expr = IR::new_with_location(
            IRValue::Declare(name, Box::new(node)),
            right_ty.clone(),
            loc,
            self.env.clone(),
        );
        expr
    }

    fn make_error(&mut self, msg: String, loc: Location) -> IR {
        self.results
            .push(LangError::error(msg.clone(), loc.clone()));
        IR::new_with_location(IRValue::Error(msg), Type::Error, loc, self.env.clone())
    }

    fn make_func_from_name(&mut self, name: String, ty: Type, loc: Location) -> IR {
        // get all of the types that match the name
        let mut fn_types = Vec::new();
        for f in self.env.resolve_all(&name) {
            match &f.value {
                IRValue::Function(_, _) | IRValue::Extern(_) => {
                    fn_types.push(f.ty.clone());
                }
                _ => (),
            }
        }
        if fn_types.len() == 0 {
            let msg = format!("Function Not found: {}", name);
            self.make_error(msg, loc)
        } else {
            let result =
                IR::new_with_location(IRValue::Ident(name), ty.clone(), loc, self.env.clone());
            self.checker.add(TypeEquation::new(
                ty.clone(),
                fn_types,
                result.clone(),
                self.env.clone(),
            ));
            result
        }
    }

    fn make_ident_from_name(&mut self, name: String, ty: Type, loc: Location) -> IR {
        if let Some(v) = self.env.resolve(&name) {
            let v_ty = v.ty.clone();
            let result =
                IR::new_with_location(IRValue::Ident(name), v_ty.clone(), loc, self.env.clone());

            self.checker.add(TypeEquation::new(
                ty.clone(),
                Vec::from([v_ty.clone()]),
                result.clone(),
                self.env.clone(),
            ));
            result
        } else {
            self.make_error(format!("Not found: {}", name), loc)
        }
    }

    fn make_ident(&mut self, node: IR) -> IR {
        match &node.value {
            IRValue::Ident(name) => {
                self.make_ident_from_name(name.clone(), node.ty.clone(), node.loc)
            }
            _ => {
                log::debug!("Unimplemented: {:?}", &node);
                unimplemented!()
            }
        }
    }

    fn make_apply_by_name(&mut self, name: String, ir_args: Vec<IR>, loc: Location) -> IR {
        let ret_ty = self.checker.new_unknown_type();
        let mut f_types = ir_args.iter().map(|v| v.ty.clone()).collect::<Vec<_>>();
        f_types.push(ret_ty.clone());
        let f_ty = Type::Func(f_types);
        let f = self.make_func_from_name(name.clone(), f_ty, loc.clone());
        IR::new_with_location(
            IRValue::Apply(Box::new(f), ir_args),
            ret_ty,
            loc,
            self.env.clone(),
        )
    }

    fn make_apply(&mut self, ir_func: IR, ir_args: Vec<IR>) -> IR {
        let ret_ty = self.checker.new_unknown_type();
        let ir_func = match &ir_func.value {
            IRValue::Ident(name) => {
                // make function signature
                let mut f_types = ir_args.iter().map(|v| v.ty.clone()).collect::<Vec<_>>();
                f_types.push(ret_ty.clone());
                let f_ty = Type::Func(f_types);

                // make a node with that signature
                self.make_func_from_name(name.clone(), f_ty, ir_func.loc)
            }
            IRValue::Extern(_) | IRValue::Function(_, _) => ir_func,
            IRValue::Error(_) => ir_func,
            _ => {
                log::debug!("Unimplemented: {:?}", &ir_func);
                unimplemented!()
            }
        };

        let loc = ir_func.loc.clone();
        IR::new_with_location(
            IRValue::Apply(Box::new(ir_func), ir_args),
            ret_ty,
            loc,
            self.env.clone(),
        )
    }
}

impl CodeGenLower for IRCodeGen {
    type Input = ExprNode;
    type Output = IR;
    type Error = Box<dyn Error>;
    fn lower(&mut self, node: &ExprNode) -> Result<IR, Box<dyn Error>> {
        match &node.value {
            Expr::Literal(Tok::FloatLiteral(f)) => Ok(IR::new_with_location(
                IRValue::Literal(Literal::Float(*f)),
                Type::Float,
                node.context.to_location(),
                self.env.clone(),
            )),
            Expr::Literal(Tok::IntLiteral(i)) => Ok(IR::new_with_location(
                IRValue::Literal(Literal::Int(*i)),
                Type::Int,
                node.context.to_location(),
                self.env.clone(),
            )),

            Expr::Literal(Tok::BoolLiteral(b)) => Ok(IR::new_with_location(
                IRValue::Literal(Literal::Bool(*b)),
                Type::Bool,
                node.context.to_location(),
                self.env.clone(),
            )),
            Expr::Literal(Tok::StringLiteral(s)) => Ok(IR::new_with_location(
                IRValue::Literal(Literal::String(s.clone())),
                Type::String,
                node.context.to_location(),
                self.env.clone(),
            )),
            Expr::Literal(_) => unimplemented!(),

            Expr::Ident(s) => {
                let ty = self.checker.new_unknown_type();
                Ok(self.make_ident_from_name(s.name.clone(), ty, node.context.to_location()))
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

                self.env = orig_env; //self.env.pop().unwrap();
                Ok(ir)
            }

            Expr::Block(exprs) | Expr::Program(exprs) => {
                let mut ir_exprs = vec![];
                for p in exprs {
                    let node = self.lower(p)?;
                    ir_exprs.push(node);
                }

                Ok(self.make_block(ir_exprs, node.context.to_location()))
            }

            Expr::Prefix(op, right) => {
                let name = op_name(op);
                let ir_right = self.lower(right)?;
                Ok(self.make_apply_by_name(name, vec![ir_right], node.context.to_location()))
            }

            Expr::Binary(op, left, right) => match &op.value {
                Operator::Assign => {
                    let name = left.try_ident().unwrap().name;
                    let ir_right = self.lower(right)?;
                    Ok(self.make_assign(name, ir_right, node.context.to_location()))
                }

                Operator::Declare => {
                    let name = left.try_ident().unwrap().name;
                    let ir_right = self.lower(right)?;
                    Ok(self.make_declare(name, ir_right, node.context.to_location()))
                }

                _ => {
                    let name = op_name(op);
                    let ir_left = self.lower(left)?;
                    let ir_right = self.lower(right)?;
                    Ok(self.make_apply_by_name(
                        name,
                        vec![ir_left, ir_right],
                        op.context.to_location(),
                    ))
                }
            },

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
    use crate::program::Program;

    #[test]
    fn codegen_lower() {
        let mut gen = IRCodeGen::default();
        let expr = gen.results.parse_str("let f = \\x -> 1+1").unwrap();
        println!("{:?}", expr);
        let ir = gen.lower(&expr).unwrap();
        println!("{:?}", ir);
        gen.results.print();
    }
}
