use crate::ast::*;
use crate::eval::*;
use crate::tokens::Tok;
use crate::program::Program;
use crate::results::*;
use log::*;
use std::fmt;

type FunctionSig = Vec<Type>;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    //I32,
    //U32,
    //U64,
    //F32,
    //F64,
    Int,
    Bool,
    Unknown,
    Void,
    Error,
    Func(FunctionSig),
    Type(String)
}

type Environment = crate::compiler::env::EnvLayers<String, IR>;

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Bool(bool),
    Int(u64),
    Float(f64)
}

#[derive(Clone, PartialEq)]
pub struct IR {
    ty: Option<Type>,
    value: IRValue,
    context: MaybeNodeContext
}

impl fmt::Debug for IR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = f.debug_struct("IR");
        out.field("v", &self.value);
        out.finish()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum IRValue {
    Type(Type),

    // Const value
    Literal(Literal),

    // Variable, represented by a String
    Ident(String),

    Block(Vec<IR>),

    // A function that is defined externally
    Extern(String, FunctionSig), // name, args

    // A function that is defined as part of the program
    Function(String, FunctionSig, Box<IR>), // name, args, body

    // function application
    Apply(String, Vec<IR>), // function sign, parameters

    Assign(String, Box<IR>),

    Declare(String, Box<IR>),
}

impl crate::compiler::env::LayerValue for IR {}

impl IR {
    fn new(v: IRValue) -> Self {
        Self { value: v, ty: None, context: MaybeNodeContext::default() }
    }

    fn new_with_context(v: IRValue, context: MaybeNodeContext) -> Self {
        Self { value: v, ty: None, context }
    }

    fn get_type(&self) -> Type {
        match &self.value {
            IRValue::Type(t) => t.clone(),
            IRValue::Extern(_, args) => args.last().unwrap().clone(),
            IRValue::Function(_, args, _) => args.last().unwrap().clone(),
            _ => unimplemented!()
        }
    }
}

fn base_env() -> Environment {
    let mut env = Environment::default();
    env
}

fn op_name(op: &Operator) -> String {
    match op {
        Operator::Plus => "+".into(),
        Operator::Minus => "-".into(),
        _ => unimplemented!()
    }
}

fn lookup_op(op: &Operator, env: Environment) -> Vec<FunctionSig> {
    let name = op_name(op);
    env.resolve_all(&name).iter().filter_map(|ir| {
        match &ir.value {
            IRValue::Function(_, sig, _) | IRValue::Extern(_, sig) => {
                Some(sig.clone())
            }
            _ => None
        }
    }).collect()
}

// given types, lookup the function
fn lookup_function(name: &String, args: Vec<Type>, env: Environment) -> Vec<FunctionSig> {
    env.resolve_all(&name).iter().filter_map(|ir| {
        match &ir.value {
            IRValue::Function(_, sig, _) | IRValue::Extern(_, sig) => {
                // must be equal length to match
                if sig.len() != args.len() {
                    None
                } else {
                    // Unknown will match everything
                    let m = sig.iter().zip(args.iter()).all(|(a, b)| {
                        b == &Type::Unknown || a == b
                    });

                    if m {
                        Some(sig.clone())
                    } else {
                        None
                    }
                }
            }
            _ => None
        }
    }).collect()
}

/*
fn infer_binary_type(op: &Operator, left: &Expr, right: &Expr, env: Environment) -> Type {
    let op_name = op_name(op);
    let t_left = infer_type(left, env);
    let t_right = infer_type(right, env);

    for v in env.resolve_all(&op_name) {
        if let IR::Type(Type::Function(sig)) = v {
            if sig.len() != 3 {
                continue;
            }
            let sig_left = sig.get(0).unwrap();
            let sig_right = sig.get(1).unwrap();
            if &t_left == sig_left && &t_right == sig_right {
                return sig.get(2).unwrap().clone();
            }
        }
    }
    // Not found
    Type::Error
}

fn infer_type(ast: &Expr, env: Environment) -> Type {
    match ast {
        Expr::Void => Type::Void,

        // Literals
        Expr::Literal(Tok::FloatLiteral(_)) => Type::F64,
        Expr::Literal(Tok::IntLiteral(_)) => Type::U64,
        Expr::Literal(Tok::BoolLiteral(_)) => Type::Bool,
        Expr::Literal(_) => unimplemented!(),

        Expr::Binary(op, left, right) => {
            infer_binary_type(op, left, right, env)
        }

        Expr::Ident(ident) => {
            if let Some(ir) = env.resolve(&ident.name) {
                ir.get_type()
            } else {
                Type::Error
            }
        }

        _ => unimplemented!()
    }
}
*/

#[derive(Default)]
pub struct TypeChecker {
    results: CompileResults,
    type_counter: usize
}

impl TypeChecker {
    fn next_type_counter(&mut self) -> usize {
        let x = self.type_counter;
        self.type_counter+=1;
        x
    }

    fn get_fresh_typename(&mut self) -> String {
        format!("t{}", self.next_type_counter())
    }

    fn assign_typenames(&mut self, ir: &IR, env: Environment) {
        match &ir.value {
            IRValue::Ident(s) => {
                if let Some(v) = env.resolve(s) {
                } else {
                    //self.results.diagnostics.push(LangError::error("Not found", ir.context).diagnostic());
                }
            }
            _ => unimplemented!()
        }
    }


    fn parse_ast(&mut self, node: &ExprNode) -> IR {
        match &node.value {
            Expr::Program(exprs) => {
                let mut body = vec![];
                for n in exprs {
                    let ir = self.parse_ast(n);
                    body.push(ir);
                }

                IR::new(IRValue::Function("main".into(), vec![], Box::new(IR::new(IRValue::Block(body)))))
            }

            Expr::Literal(Tok::FloatLiteral(f)) => IR::new_with_context(IRValue::Literal(Literal::Float(*f)), node.context.clone()),
            Expr::Literal(Tok::IntLiteral(i)) => IR::new_with_context(IRValue::Literal(Literal::Int(*i)), node.context.clone()),
            Expr::Literal(Tok::BoolLiteral(b)) => IR::new_with_context(IRValue::Literal(Literal::Bool(*b)), node.context.clone()),
            Expr::Literal(_) => unimplemented!(),

            Expr::Ident(s) => IR::new_with_context(IRValue::Ident(s.name.clone()), node.context.clone()),

            Expr::Apply(f, args) => {
                match &f.value {
                    Expr::Ident(s) => {
                        let ir_args = args.iter().map(|v| self.parse_ast(v)).collect::<Vec<_>>(); 
                        IR::new_with_context(IRValue::Apply(s.name.clone(), ir_args), node.context.clone())
                    } 
                    _ => unimplemented!()
                }
            }

            Expr::Binary(op, left, right) => {
                match op {
                    Operator::Assign => {
                        let name = left.try_ident().unwrap().name;
                        IR::new_with_context(IRValue::Assign(name, Box::new(self.parse_ast(right))), node.context.clone())
                    }
                    Operator::Declare => {
                        let name = left.try_ident().unwrap().name;
                        IR::new_with_context(IRValue::Declare(name, Box::new(self.parse_ast(right))), node.context.clone())
                    }
                    Operator::Plus | Operator::Minus => {
                        let name = op_name(op);
                        let ir_left = self.parse_ast(left);
                        let ir_right = self.parse_ast(right);
                        IR::new_with_context(IRValue::Apply(name, vec![ir_left, ir_right]), node.context.clone())
                    }
                    _ => unimplemented!()
                }
                //let f = op_name(op);
                //IR::Apply()
                    //f,
                //let t = infer_binary_type(op, left, right, env);
                //if t != Type::Error {
                    //Self::Apply(
                //}
                //infer_binary_type(op, left, right, env)
            }

            _ => {
                debug!("Unimplemented: {:?}", &node);
                unimplemented!()
            }
        }
    }

    pub fn parse_str(&mut self, s: &str) -> anyhow::Result<IR> {
        let node = Program::parse(s)?;
        debug!("parse {:?}", &node.value);
        let ir = self.parse_ast(&node);
        Ok(ir)
    }

    pub fn parse_file(&mut self, filename: &str) -> anyhow::Result<IR> {
        let node = Program::parse_file(filename)?;
        let ir = self.parse_ast(&node);
        Ok(ir)
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::program::Program;
    use test_log::test;

    #[test]
    fn analyze() {
        let mut c = TypeChecker::default();
        let ir = c.parse_str("1+1");
        debug!("{:?}", ir);
        let ir = c.parse_str("1-1");
        debug!("{:?}", ir);
        let ir = c.parse_str("let mut x = 1.");
        debug!("{:?}", ir);
    }
}

