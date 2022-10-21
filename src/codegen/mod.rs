use ante::cranelift_backend;
use ante::llvm;
use ante::hir;
use crate::ast::*;
use crate::tokens::{Tok, Token};
use std::error::Error;

mod check;
mod codegen;

pub use codegen::*;

use crate::ast::*;

pub trait CodeGenLower {
    type Input;
    type Output;
    type Error;
    fn lower(&mut self, i: &Self::Input) -> Result<Self::Output, Self::Error>;
}

//pub fn lower<A, E>(expr: &Expr) -> Result<A, E> {
    //expr.codegen()
//}


/*
impl CodeGen for Expr {
    fn codegen(&self) -> Result<hir::Ast, Box<dyn Error>> {
        match self {
            Expr::Literal(Tok::FloatLiteral(nb)) => Ok(hir::Ast::Literal(hir::Literal::Float(0, hir::FloatKind::F64))),
            Expr::Literal(Tok::IntLiteral(nb)) => Ok(hir::Ast::Literal(hir::Literal::Integer(*nb, hir::IntegerKind::I64))),
            Expr::Program(exprs) => {
                let mut seq = vec![];
                for e in exprs {
                    seq.push(e.codegen()?);
                }
                Ok(hir::Ast::Sequence(hir::Sequence { statements: seq }))
            }


            Expr::Declare(
            Expr::Binary(op, lhs, rhs) => {
                match &op.value {
                    Operator::Plus => {
                        let lhs = lhs.value.codegen()?;
                        let rhs = rhs.value.codegen()?;
                        Ok(hir::Ast::Builtin(hir::Builtin::AddInt(Box::new(lhs), Box::new(rhs))))
                    }
                    _ => {
                        eprintln!("Unimplemented: {:?}", &self);
                        unimplemented!()
                    }

                }
            }
            _ => {
                eprintln!("Unimplemented: {:?}", &self);
                unimplemented!()
            }
        }
    }
}
*/

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::Environment;
    use crate::program::Program;
    use ante::llvm;

    #[test]
    fn codegen_literal() {
        let mut args = llvm::CompileArgs::default();
        let mut program = Program::new();
        let mut env = Environment::default();
        let expr = program.parse_str("let f = \\x -> 1+1").unwrap().value;
        //let node = exprref.borrow();
        //let expr = &node.value;
        println!("{:?}", expr);
        //assert_eq!(expr.try_literal().unwrap(), Tok::IntLiteral(1));
        //let v = lower(&expr).unwrap();
        //println!("{:?}", v);
        //llvm::compile(&"test".to_string(), v, &args);
    }

}
