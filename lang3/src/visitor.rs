use crate::*;

#[derive(Debug)]
pub enum VisitError {
    Error,
}

pub type VResult = Result<(), VisitError>;

pub trait Visitor<T, N> {
    fn enter(&mut self, _: &Ast<T>, _: &mut N) -> VResult {
        Ok(())
    }
    fn exit(&mut self, _: &Ast<T>, _: &mut N) -> VResult {
        Ok(())
    }
    fn leaf(&mut self, _: &Ast<T>, _: &mut N) -> VResult {
        Ok(())
    }
    fn variable(&mut self, _: &Variable<T>, _: &mut N) -> VResult {
        Ok(())
    }
    fn literal(&mut self, _: &Literal, _: &mut N) -> VResult {
        Ok(())
    }
    fn void(&mut self, _: &Ast<T>, _: &mut N) -> VResult {
        Ok(())
    }
}

fn visit_children<T, N>(e: &Ast<T>, f: &mut impl Visitor<T, N>, n: &mut N) -> VResult {
    match e {
        Ast::Literal(v) => {
            f.literal(v, n)?;
            f.leaf(e, n)?;
        }
        Ast::Variable(v) => {
            f.variable(v, n)?;
            f.leaf(e.clone(), n)?;
        }
        Ast::Declare(_name, rhs) => {
            visit(rhs, f, n)?;
        }
        Ast::Block(exprs) => {
            for e in exprs {
                visit(e.clone(), f, n)?;
            }
        }
        Ast::Function { params, body, ty } => {
            visit(body, f, n)?;
            //for p in params {
                //visit(p.clone(), f, n)?;
            //}
        }
        Ast::Apply(name, args) => {
            visit(name, f, n)?;
            for arg in args {
                visit(arg.clone(), f, n)?;
            }
        }
        _ => unimplemented!(),
    };
    Ok(())
}

pub fn visit<T, N>(e: &Ast<T>, f: &mut impl Visitor<T, N>, n: &mut N) -> VResult {
    f.enter(e, n)?;
    visit_children(e, f, n)?;
    f.exit(e, n)
}
