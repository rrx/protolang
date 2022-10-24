use crate::ast::*;

#[derive(Debug)]
pub enum VisitError {
    Error,
}

pub type VResult = Result<(), VisitError>;

pub trait Visitor<N> {
    fn enter(&mut self, _: AstNode, _: &mut N) -> VResult {
        Ok(())
    }
    fn exit(&mut self, _: AstNode, _: &mut N) -> VResult {
        Ok(())
    }
    fn leaf(&mut self, _: AstNode, _: &mut N) -> VResult {
        Ok(())
    }
    fn variable(&mut self, _: &Variable, _: &mut N) -> VResult {
        Ok(())
    }
    fn literal(&mut self, _: &Literal, _: &mut N) -> VResult {
        Ok(())
    }
    fn void(&mut self, _: AstNode, _: &mut N) -> VResult {
        Ok(())
    }
}

pub trait VisitorMut {
    fn enter(&mut self, _: &mut AstNodeInner) -> bool;
    fn exit(&mut self, _: &mut AstNodeInner) -> bool;
    fn variable(&mut self, _: &mut Variable) -> bool {
        true
    }
    fn literal(&mut self, _: &mut Literal) -> bool {
        true
    }
}

fn visit_children<N>(e: AstNode, f: &mut impl Visitor<N>, n: &mut N) -> VResult {
    let inner = e.borrow();
    let value = &inner.value;

    match value {
        Ast::Literal(v) => {
            f.literal(v, n)?;
            f.leaf(e.clone(), n)?;
        }
        Ast::Variable(v) => {
            f.variable(v, n)?;
            f.leaf(e.clone(), n)?;
        }
        Ast::Declare(_name, rhs) => {
            visit(*rhs.clone(), f, n)?;
        }
        Ast::Block(exprs) => {
            for e in exprs {
                visit(e.clone(), f, n)?;
            }
        }
        Ast::Function(body, args) => {
            visit(*body.clone(), f, n)?;
            for arg in args {
                visit(arg.clone(), f, n)?;
            }
        }
        Ast::Apply(name, args) => {
            visit(*name.clone(), f, n)?;
            for arg in args {
                visit(arg.clone(), f, n)?;
            }
        }
        _ => unimplemented!()
    };
    Ok(())
}

pub fn visit<N>(e: AstNode, f: &mut impl Visitor<N>, n: &mut N) -> VResult {
    f.enter(e.clone(), n)?;
    visit_children(e.clone(), f, n)?;
    f.exit(e, n)
}
