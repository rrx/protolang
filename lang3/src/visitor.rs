use crate::*;

#[derive(Clone, Debug)]
pub enum VisitError {
    Error(String),
}

pub type VResult = Result<(), VisitError>;

pub trait Visitor<N> {
    fn enter(&mut self, _: &Ast, _: &mut N) -> VResult {
        Ok(())
    }
    fn exit(&mut self, _: &Ast, _: &mut N) -> VResult {
        Ok(())
    }
    fn leaf(&mut self, _: &Ast, _: &mut N) -> VResult {
        Ok(())
    }
    fn variable(&mut self, _: &Variable, _: &mut N) -> VResult {
        Ok(())
    }
    fn literal(&mut self, _: &Literal, _: &mut N) -> VResult {
        Ok(())
    }
    fn void(&mut self, _: &Ast, _: &mut N) -> VResult {
        Ok(())
    }
}

fn visit_children<N>(e: &Ast, f: &mut impl Visitor<N>, n: &mut N) -> VResult {
    match e {
        Ast::Literal(v) => {
            f.literal(v, n)?;
            f.leaf(e, n)?;
        }
        Ast::Variable(v) => {
            f.variable(v, n)?;
            f.leaf(e, n)?;
        }
        Ast::Declare(_name, rhs) => {
            visit(rhs, f, n)?;
        }
        Ast::Block(exprs) => {
            for e in exprs {
                visit(e, f, n)?;
            }
        }
        Ast::Function { params, body, ty } => {
            visit(body, f, n)?;
            //for p in params {
            //visit(p.clone(), f, n)?;
            //}
        }
        Ast::Apply(var, args) => {
            f.variable(var, n)?;
            for arg in args {
                visit(arg, f, n)?;
            }
        }
        _ => unimplemented!(),
    };
    Ok(())
}

pub fn visit<N>(e: &Ast, f: &mut impl Visitor<N>, n: &mut N) -> VResult {
    f.enter(e, n)?;
    visit_children(e, f, n)?;
    f.exit(e, n)
}
