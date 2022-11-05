//use lang3::*;
use crate::syntax::{ast, AstModule, lexer};
use std::error::Error;
use lang3 as L;
use lang3::Lower;

pub type LResult = Result<lang3::Ast, Box<dyn Error>>;
type Builder = L::AstBuilder;
//use L::AstBuilder::lower_list_ast as lower_list;

impl AstModule {
    pub fn lower(&self, builder: &mut Builder) -> LResult {
        let ast = self.statement.lower(builder)?;
        builder.resolve_ast(ast)
    }
}
    
//impl<A: ast::AstPayload> Lower for ast::AssignP<A> {
    //fn lower(&self, b: &mut Builder) -> LResult {
        //match self {
            //Self::Identifier(x) => x.lower(b),
            //_ => unimplemented!("{:?}", &self)
        //}
    //}
//}

//impl<A: ast::AstPayload> Lower for ast::AssignIdentP<A> {
    //fn lower(&self, b: &mut Builder) -> LResult {
        //Ok(b.var_named(&self.0.clone(), ty).into())
    //}
//}

impl<A: ast::AstPayload> ast::AstParameterP<A> {
    fn to_variable(&self, b: &mut Builder) -> L::Variable {
        match &self.node {
            ast::ParameterP::Normal(ident, _) => {
                let ty = b.type_unknown();
                let name = ident.0.to_string();
                b.var_named(&name, ty) 
            },
            _ => unimplemented!()
        }
    }
}

impl<A: ast::AstPayload> Lower for ast::AstParameterP<A> {
    fn lower(&self, b: &mut Builder) -> LResult {
        Ok(self.to_variable(b).into())
    }
}

fn params_to_vars<A: ast::AstPayload>(params: &Vec<ast::AstParameterP<A>>, b: &mut Builder) -> Vec<L::Variable> {
    let mut out = vec![];
    for p in params {
        out.push(p.to_variable(b));
    }
    out
}

impl<A: ast::AstPayload> Lower for ast::AstStmtP<A> {
    fn lower(&self, b: &mut Builder) -> LResult {
        use ast::StmtP::*;
        match &self.node {
            Return(Some(expr)) => Ok(L::Ast::Return(expr.lower(b)?.into())),
            Return(None) => unimplemented!(),
            Expression(expr) => Ok(expr.lower(b)?),
            Assign(lhs, box (_, rhs)) => {
                let rhs = rhs.lower(b)?;
                let ty = rhs.get_type();

                // lhs is going to be a name that will need to be resolved later
                let lhs = match &lhs.node {
                    ast::AssignP::Identifier(x) => {
                        let name = x.0.to_string();
                        b.var_named(&name, ty)
                    }
                    _ => unimplemented!("{:?}", &self)
                };
                Ok(L::Ast::Assign(lhs.into(), rhs.into()))
            }
            //AssignModify(AstAssignP<P>, AssignOp, Box<AstExprP<P>>),
            Statements(stmts) => Ok(L::Ast::Block(b.lower_list_ast(&stmts)?)),
            Def(ident, params, _, body, _) => {
                let name: String = ident.node.0.clone();
                let body = body.lower(b)?;
                let ret_ty = b.type_unknown();

                let params = params_to_vars(params, b);
                let mut sig = params.iter().cloned().map(|p| p.ty).collect::<Vec<_>>();
                sig.push(ret_ty);
                let f = b.func(params, body, sig);
                Ok(b.declare(&name, f))
            }
            _ => unimplemented!("{:?}", &self.node)
        }
    }
}

impl<A: ast::AstPayload> Lower for ast::AstArgumentP<A> {
    fn lower(&self, b: &mut Builder) -> LResult {
        use crate::syntax::ast::ArgumentP::*;
        match &self.node {
            Positional(expr) => expr.lower(b),
            _ => unimplemented!()
        }
    }
}

impl Lower for ast::AstLiteral {
    fn lower(&self, b: &mut Builder) -> LResult {
        match self {
            Self::Int(int) => {
                match &int.node {
                    lexer::TokenInt::I32(i) => Ok(L::Ast::int(*i as i64)),
                    _ => unimplemented!()
                }
            }
            Self::String(v) => Ok(L::Ast::string(v.node.clone())),
            _ => unimplemented!()
        }
    }
}

impl<A: ast::AstPayload> ast::AstExprP<A> {
    pub(crate) fn lower(&self, b: &mut Builder) -> LResult {
        use ast::ExprP::*;
        match &self.node {
            //Tuple(Vec<AstExprP<P>>),
            //Dot(Box<AstExprP<P>>, AstString),
            Call(expr, args) => {
                let fexpr = expr.lower(b)?;
                let fargs = b.lower_list_ast(&args)?;
                unimplemented!()
            }
            //ArrayIndirection(Box<(AstExprP<P>, AstExprP<P>)>),
            //Slice(
            Identifier(s, p) => {
                let ty = b.type_unknown();
                Ok(b.var_named(&s.node, ty).into())
            },
            Lambda(params, body, p) => {
                unimplemented!("{:?}", &self)
            },
            Literal(x) => x.lower(b),
            //Not(Box<AstExprP<P>>),
            //Minus(Box<AstExprP<P>>),
            //Plus(expr) => {},
            //BitNot(Box<AstExprP<P>>),
            Op(lhs, op, rhs) => {
                let lhs = lhs.lower(b)?;
                let rhs = rhs.lower(b)?;
                match op {
                    ast::BinOp::Add => {
                        Ok(b.binary("+", lhs, rhs))
                    }
                    _ => unimplemented!("{}", &op)
                }
            }
            //If(Box<(AstExprP<P>, AstExprP<P>, AstExprP<P>)>), // Order: condition, v1, v2 <=> v1 if condition else v2
            //List(Vec<AstExprP<P>>),
            //Dict(Vec<(AstExprP<P>, AstExprP<P>)>),
            //ListComprehension(Box<AstExprP<P>>, Box<ForClauseP<P>>, Vec<ClauseP<P>>),
            //DictComprehension(
            _ => unimplemented!()
        }
    }
}

