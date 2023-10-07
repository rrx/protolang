use starlark_syntax::syntax;
use starlark_syntax::syntax::{Dialect};
use starlark_syntax::lexer;
use starlark_syntax::syntax::module::AstModuleFields;
use std::path::{Path, PathBuf};

use lang3 as L;
use lang3::Lower;
use lang3::UnifyValue;
use std::error::Error;

pub type LResult = Result<lang3::Ast, Box<dyn Error>>;
pub type Builder = L::AstBuilder;
//use L::AstBuilder::lower_list_ast as lower_list;

pub struct AstModule(pub syntax::AstModule);
struct AstStatement(pub syntax::ast::AstStmt);

impl AstModule {
    pub fn lower(&self, builder: &mut Builder) -> LResult {
        let ast = lower_statement(self.0.statement(), builder)?;
        let (ast, _env, _subst) = builder.resolve_ast_with_base(&ast)?;
        Ok(ast)
    }
    // eventually we want to be able to handle multiple input types
    // for now we only handle input from frontend, lowering to hir
    pub fn parse<'a>(path: &Path) -> LResult {
        let dialect = syntax::Dialect::Extended;
        let module = AstModule(syntax::AstModule::parse_file(&path, &dialect)?);
        let mut builder = L::AstBuilder::default();
        module.lower(&mut builder)
    }
}

fn lower_statement(stmt: &syntax::ast::AstStmt, b: &mut Builder) -> LResult {
    match stmt {
       // self.0.node.lower(b)
        _ => unimplemented!()
    }
}

impl Lower for AstStatement {
    fn lower(&self, b: &mut Builder) -> LResult {
        match self.0 {
           // self.0.node.lower(b)

            _ => unimplemented!()
        }
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

/*
struct AstPayload {}

impl<A: syntax::ask::AstPayload> syntax::ast::AstParameterP<A> {
    fn to_variable(&self, b: &mut Builder) -> L::Variable {
        match &self.node {
            syntax::ast::ParameterP::Normal(ident, _) => {
                let ty = b.type_unknown();
                let name = ident.0.to_string();
                b.var_named(&name, ty)
            }
            _ => unimplemented!(),
        }
    }
}

impl<A: ast::AstPayload> Lower for ast::AstParameterP<A> {
    fn lower(&self, b: &mut Builder) -> LResult {
        Ok(self.to_variable(b).into())
    }
}

fn params_to_vars<A: ast::AstPayload>(
    params: &Vec<ast::AstParameterP<A>>,
    b: &mut Builder,
) -> Vec<L::Variable> {
    let mut out = vec![];
    for p in params {
        out.push(p.to_variable(b));
    }
    out
}

impl<A: ast::AstPayload> Lower for ast::AstStmtP<A> {
    fn lower(&self, b: &mut Builder) -> LResult {
        self.node.lower(b)
    }
}

impl<A: ast::AstPayload> Lower for ast::StmtP<A> {
    fn lower(&self, b: &mut Builder) -> LResult {
        use ast::StmtP::*;
        match &self {
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
                    _ => unimplemented!("{:?}", &self),
                };
                Ok(L::Ast::Assign(lhs.into(), rhs.into()))
            }
            //AssignModify(AstAssignP<P>, AssignOp, Box<AstExprP<P>>),
            Statements(stmts) => Ok(L::Ast::block(b.lower_list_ast(&stmts)?)),
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
            If(condition, istrue) => {
                let condition = condition.lower(b)?;
                let istrue = istrue.lower(b)?;
                Ok(L::Ast::Condition(condition.into(), istrue.into(), None))
            }

            IfElse(condition, body) => {
                let istrue = &body.0;
                let isfalse = &body.1;
                let condition = condition.lower(b)?;
                let istrue = istrue.lower(b)?;
                let isfalse = isfalse.lower(b)?;
                Ok(L::Ast::Condition(
                    condition.into(),
                    istrue.into(),
                    Some(isfalse.into()),
                ))
            }

            _ => unimplemented!("{:?}", &self),
        }
    }
}

impl<A: ast::AstPayload> Lower for ast::AstArgumentP<A> {
    fn lower(&self, b: &mut Builder) -> LResult {
        use crate::syntax::ast::ArgumentP::*;
        match &self.node {
            Positional(expr) => expr.lower(b),
            _ => unimplemented!(),
        }
    }
}

impl Lower for ast::AstLiteral {
    fn lower(&self, _b: &mut Builder) -> LResult {
        match self {
            Self::Int(int) => match &int.node {
                lexer::TokenInt::I32(i) => Ok(L::Ast::int(*i as i64)),
                _ => unimplemented!(),
            },
            Self::String(v) => Ok(L::Ast::string(v.node.clone())),
            _ => unimplemented!(),
        }
    }
}

impl<A: ast::AstPayload> ast::AstExprP<A> {
    pub(crate) fn lower(&self, b: &mut Builder) -> LResult {
        use ast::ExprP::*;
        match &self.node {
            //Tuple(Vec<AstExprP<P>>),
            //Dot(Box<AstExprP<P>>, AstString),
            Call(expr, args) => match &expr.node {
                ast::ExprP::Identifier(s, _) => {
                    let fargs = b.lower_list_ast(&args)?;
                    let mut sig = fargs.iter().map(|t| t.get_type()).collect::<Vec<_>>();
                    let ret_ty = b.type_unknown();
                    sig.push(ret_ty);
                    let ty = L::Type::Func(sig);
                    let var = b.var_named(s, ty);
                    let fargs = b.lower_list_ast(&args)?;
                    log::debug!("call: {:?}", (&var, &fargs));
                    Ok(b.apply(&var, fargs))
                }
                _ => unimplemented!(),
            },
            //ArrayIndirection(Box<(AstExprP<P>, AstExprP<P>)>),
            //Slice(
            Identifier(s, _p) => {
                let ty = b.type_unknown();
                Ok(b.var_named(&s.node, ty).into())
            }
            Lambda(_params, _body, _p) => {
                unimplemented!("{:?}", &self)
            }
            Literal(x) => x.lower(b),
            //Not(Box<AstExprP<P>>),
            //Minus(Box<AstExprP<P>>),
            //Plus(expr) => {},
            //BitNot(Box<AstExprP<P>>),
            Op(lhs, op, rhs) => {
                let lhs = lhs.lower(b)?;
                let rhs = rhs.lower(b)?;
                let call = match op {
                    ast::BinOp::Add => b.binary("+", lhs, rhs),
                    ast::BinOp::Equal => b.binary("==", lhs, rhs),
                    ast::BinOp::Subtract => b.binary("-", lhs, rhs),
                    _ => unimplemented!("{}", &op),
                };

                log::debug!("call: {:?}", (&call));
                Ok(call)
            }
            //If(Box<(AstExprP<P>, AstExprP<P>, AstExprP<P>)>), // Order: condition, v1, v2 <=> v1 if condition else v2
            //List(Vec<AstExprP<P>>),
            //Dict(Vec<(AstExprP<P>, AstExprP<P>)>),
            //ListComprehension(Box<AstExprP<P>>, Box<ForClauseP<P>>, Vec<ClauseP<P>>),
            //DictComprehension(
            _ => unimplemented!(),
        }
    }
}
*/

