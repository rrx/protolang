use crate::{hir::{self,*}, *};

#[derive(Debug)]
pub enum VisitError {
    Error,
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
    fn definition(&mut self, _: &hir::Definition, _: &mut N) -> VResult {
        Ok(())
    }
    fn variable(&mut self, _: &Variable, _: &mut N) -> VResult {
        Ok(())
    }
    fn call(&mut self, _: &FunctionCall, _: &mut N) -> VResult {
        Ok(())
    }
    fn literal(&mut self, _: &Literal, _: &mut N) -> VResult {
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
        Ast::Definition(def) => {
            f.definition(def, n)?;
            visit(&*def.expr, f, n)?;
        }
        Ast::If(If {condition, then, otherwise, ..}) => {
            visit(condition, f, n)?;
            visit(then, f, n)?;
            if let Some(v) = otherwise {
                visit(v, f, n)?;
            }
        }
        Ast::Sequence(Sequence { statements }) => {
            for e in statements {
                visit(e, f, n)?;
            }
        }
        Ast::Return(Return { expression}) => {
            visit(expression, f, n)?;
        }
        Ast::Builtin(builtin) => {
            use Builtin::*;
            match builtin {
                AddInt(a, b) | EqInt(a,b) | SubInt(a,b) => {
                    visit(a, f, n)?;
                    visit(b, f, n)?;
                }
                _ => unimplemented!("{:?}", &e),
            }
        }
        Ast::Lambda(Lambda { args, body, typ }) => {
            visit(body, f, n)?;
        }
        Ast::FunctionCall(call) => {
            if let FunctionCall { function, args, function_type } = call {
                for arg in args {
                    visit(arg, f, n)?;
                }
                f.call(call, n)?;
            }
        }
        _ => unimplemented!("{:?}", &e),
    };
    Ok(())
}

pub fn visit<N>(e: &Ast, f: &mut impl Visitor<N>, n: &mut N) -> VResult {
    f.enter(e, n)?;
    visit_children(e, f, n)?;
    f.exit(e, n)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::*;
    //type DefinitionMap = rpds::HashTrieMap<DefinitionId, Ast>;
    type DefinitionMap = std::collections::HashMap<DefinitionId, Ast>;

    #[test]
    fn test_visit() {
        let mut defs = Definitions::new();
        let ast = gen_self_reference(&mut defs);
        println!("AST: {}", &ast.to_ron());
        struct Test {}
        impl Visitor<DefinitionMap> for Test {
            fn definition(&mut self, d: &hir::Definition, defmap: &mut DefinitionMap) -> VResult {
                //Ast::Definition(d)
                let ast: Ast = d.clone().into();
                println!("def: {}", ast.to_ron());
                defmap.insert(d.variable, ast);
                Ok(())
            }
        }
        let mut f = Test {};
        let mut defmap = DefinitionMap::default();
        visit(&ast, &mut f, &mut defmap).unwrap();
        println!("def: {:?}", defmap);
    }
}
