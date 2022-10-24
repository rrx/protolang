use crate::ast::*;
use codegen::hir;

fn lower(ast: AstNode) -> hir::Ast {
    match ast.borrow().value {
        Ast::Literal(Literal::Int(u)) => {
            hir::Ast::Literal(hir::Literal::Integer(u, hir::IntegerKind::U64))
        }
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use log::debug;
    use test_log::test;
}
