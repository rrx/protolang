use crate::ast::*;
use crate::eval::*;
use crate::tokens::Tok;
use crate::program::Program;
use crate::results::*;
use log::*;
use std::fmt;
use rpds::HashTrieMap;

type FunctionSig = Vec<Type>;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    //I32,
    //U32,
    //U64,
    //F32,
    //F64,
    Int,
    Float,
    Bool,
    Unknown(String),
    Void,
    Error,
    Func(FunctionSig),
    Type(String)
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unknown(s) => write!(f, "T:{}", s),
            Self::Int => write!(f, "T:Int"),
            Self::Bool => write!(f, "T:Bool"),
            Self::Float => write!(f, "T:Float"),
            Self::Void => write!(f, "T:Void"),
            Self::Error => write!(f, "T:Error"),
            Self::Func(s) => write!(f, "T:Func<{:?}>", s),
            Self::Type(s) => write!(f, "T:Type<{}>", s),
            _ => unimplemented!()
        }
    }
}

impl Type {
    pub fn new_unknown(s: String) -> Self {
        Self::Unknown(s)
    }
}

type Environment = crate::compiler::env::EnvLayers<String, IR>;

type SymbolTable = HashTrieMap<String, Type>;

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Bool(bool),
    Int(u64),
    Float(f64)
}

#[derive(Clone, PartialEq)]
pub struct IR {
    ty: Type,
    value: IRValue,
    context: MaybeNodeContext
}

impl fmt::Debug for IR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut out = f.debug_struct("IR");
        out.field("v", &self.value);
        out.field("ty", &self.ty);
        out.finish()
    }
}

impl fmt::Display for IR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.value {
            IRValue::Block(exprs) => {
                write!(f, "Block({})", self.ty)?;
                for expr in exprs {
                    write!(f, "\t{}", expr)?;
                }
                Ok(())
            }
            _ => write!(f, "{:?}", &self)
        }
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
    Extern(Vec<IR>), // args

    // A function that is defined as part of the program
    Function(Box<IR>, Vec<IR>), // body, arg names

    // function application
    Apply(Box<IR>, Vec<IR>), // function, parameters

    Assign(String, Box<IR>),

    Declare(String, Box<IR>),
}

impl crate::compiler::env::LayerValue for IR {}

impl IR {
    fn new(v: IRValue, ty: Type, ) -> Self {
        Self { value: v, ty, context: MaybeNodeContext::default() }
    }

    fn new_with_context(v: IRValue, ty: Type, context: MaybeNodeContext) -> Self {
        Self { value: v, ty, context }
    }

    /*
    fn get_type(&self) -> Type {
        match &self.value {
            IRValue::Type(t) => t.clone(),
            IRValue::Extern(_, args) => args.last().unwrap().clone(),
            IRValue::Function(args, _) => args.last().unwrap().clone(),
            _ => unimplemented!()
        }
    }
    */
}

fn base_env() -> Environment {
    let mut env = Environment::default();
    env
}

fn op_name(op: &Operator) -> String {
    match op {
        Operator::Plus => "+".into(),
        Operator::Minus => "-".into(),
        Operator::Exp => "^".into(),
        _ => unimplemented!()
    }
}

/*
fn lookup_op(op: &Operator, env: Environment) -> Vec<FunctionSig> {
    let name = op_name(op);
    env.resolve_all(&name).iter().filter_map(|ir| {
        match &ir.value {
            IRValue::Function(sig, _) | IRValue::Extern(_, sig) => {
                Some(sig.clone())
            }
            _ => None
        }
    }).collect()
}
*/

// given types, lookup the function
/*
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
*/

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

#[derive(Clone, Debug, PartialEq)]
pub struct TypeEquation {
    left: Type,
    right: Type,
    node: IR,
    env: Environment,
}

impl TypeEquation {
    fn new(left: Type, right: Type, node: IR, env: Environment) -> Self {
        Self { left, right, node, env }
    }
}

impl fmt::Display for TypeEquation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} :: {}", self.left, self.right)
    }
}


#[derive(Default)]
pub struct TypeChecker {
    results: CompileResults,
    type_equations: Vec<TypeEquation>,
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

    fn generate_equations(&mut self, node: &IR, mut env: Environment) -> Environment {
        match &node.value {
            // already typed
            IRValue::Type(_) => (),
            IRValue::Literal(_) => {
            }
            IRValue::Ident(_) => (),

            IRValue::Declare(ident, expr) => {
                env.define(ident.clone(), *expr.clone());
                env = self.generate_equations(&expr, env);
                self.type_equations.push(TypeEquation::new(expr.ty.clone(), node.ty.clone(), node.clone(), env.clone()));
            }

            IRValue::Assign(ident, expr) => {
                if let Some(v) = env.resolve(ident) {
                    env = self.generate_equations(&expr, env);
                    self.type_equations.push(TypeEquation::new(expr.ty.clone(), node.ty.clone(), node.clone(), env.clone()));
                } else {
                    self.results.push(LangError::error("Not found".into(), node.context.clone()));
                }
            }

            IRValue::Apply(f, params) => {
                env = self.generate_equations(f, env);
                for p in params {
                    env = self.generate_equations(p, env);
                }
            }

            IRValue::Extern(args) => {
                // generate type signature for function
                let mut arg_types = args.iter().map(|v| v.ty.clone()).collect::<Vec<_>>();

                // function has a function type
                self.type_equations.push(TypeEquation::new(node.ty.clone(), Type::Func(arg_types), node.clone(), env.clone()));
            }

            IRValue::Function(body, args) => {
                //for arg in args {
                    //self.generate_equations(&arg);
                //}
                env = self.generate_equations(body, env);

                // generate type signature for function
                let mut arg_types = args.iter().map(|v| v.ty.clone()).collect::<Vec<_>>();
                // append return type
                arg_types.push(node.ty.clone());

                // function has a function type
                self.type_equations.push(TypeEquation::new(node.ty.clone(), Type::Func(arg_types), node.clone(), env.clone()));
            }

            IRValue::Block(exprs) => {
                for e in exprs {
                    env = self.generate_equations(&e, env);
                }
                let last = exprs.last().unwrap();
                self.type_equations.push(TypeEquation::new(node.ty.clone(), last.ty.clone(), node.clone(), env.clone()));
            }
            //_ => {
                //debug!("Unimplemented: {:?}", &node);
                //unimplemented!()
            //}
        }
        env.clone()
    }

    fn assign_typenames(&mut self, ir: &mut IR, mut syms: SymbolTable) -> SymbolTable {
        match &mut ir.value {

            IRValue::Ident(s) => {
                // check if the identifier is in the symbol table
                if let Some(ty) = syms.get(s) {
                    ir.ty = ty.clone();
                    syms
                } else {
                    self.results.push(LangError::error("Not found".into(), ir.context.clone()));
                    syms
                }
            }

            IRValue::Function(f, args) => {
                for arg in args {
                    match &arg.value {
                        IRValue::Ident(s) => {
                            syms = syms.insert(s.clone(), arg.ty.clone());
                        }
                        _ => unimplemented!()
                    }
                }
                syms = self.assign_typenames(f, syms);
                syms
            }

            IRValue::Apply(f, args) => {
                syms
            }

            IRValue::Block(_) => {
                syms
            }
            
            // already typed, and no children
            IRValue::Literal(_) => syms,
            _ => {
                debug!("Unimplemented: {:?}", &ir);
                unimplemented!()
            }
        }
    }


    fn new_unknown_type(&mut self) -> Type {
        Type::new_unknown(self.get_fresh_typename())
    }

    fn parse_ast(&mut self, node: &ExprNode) -> IR {
        match &node.value {
            Expr::Program(exprs) => {
                let mut body = vec![];
                for n in exprs {
                    let ir = self.parse_ast(n);
                    body.push(ir);
                }

                IR::new(
                    IRValue::Function(
                        Box::new(
                            IR::new(IRValue::Block(body), self.new_unknown_type())
                        ),
                        vec![],  // args
                    ), self.new_unknown_type())
            }

            Expr::Literal(Tok::FloatLiteral(f)) => IR::new_with_context(
                IRValue::Literal(Literal::Float(*f)),
                Type::Float,
                node.context.clone()),
            Expr::Literal(Tok::IntLiteral(i)) => IR::new_with_context(
                IRValue::Literal(Literal::Int(*i)),
                Type::Int,
                node.context.clone()),
            Expr::Literal(Tok::BoolLiteral(b)) => IR::new_with_context(
                IRValue::Literal(Literal::Bool(*b)),
                Type::Bool,
                node.context.clone()),
            Expr::Literal(_) => unimplemented!(),

            Expr::Ident(s) => IR::new_with_context(
                IRValue::Ident(s.name.clone()),
                self.new_unknown_type(),
                node.context.clone()),

            Expr::Apply(f, args) => {
                let ir_func = self.parse_ast(f);
                let ir_args = args.iter().map(|v| self.parse_ast(v)).collect::<Vec<_>>(); 
                IR::new_with_context(
                    IRValue::Apply(Box::new(ir_func), ir_args),
                    self.new_unknown_type(),
                    node.context.clone())
            }

            Expr::Lambda(f) => {
                let ir_args = f.params.value.iter().map(|a| {
                    self.parse_ast(a)
                }).collect();
                let body = self.parse_ast(&f.expr);
                IR::new_with_context(
                    IRValue::Function(Box::new(body), ir_args),
                    self.new_unknown_type(),
                    node.context.clone())
            }

            Expr::Block(exprs) => {
                let ir_exprs = exprs.iter().map(|a| {
                    self.parse_ast(a)
                }).collect();
                IR::new_with_context(
                    IRValue::Block(ir_exprs),
                    self.new_unknown_type(),
                    node.context.clone())
            }

            Expr::Binary(op, left, right) => {
                match op {
                    Operator::Assign => {
                        let name = left.try_ident().unwrap().name;
                        IR::new_with_context(
                            IRValue::Assign(name, Box::new(self.parse_ast(right))),
                            self.new_unknown_type(),
                            node.context.clone())
                    }
                    Operator::Declare => {
                        let name = left.try_ident().unwrap().name;
                        IR::new_with_context(
                            IRValue::Declare(name, Box::new(self.parse_ast(right))),
                            self.new_unknown_type(),
                            node.context.clone())
                    }
                    Operator::Plus | Operator::Minus | Operator::Exp => {
                        let name = op_name(op);
                        let ir_left = self.parse_ast(left);
                        let ir_right = self.parse_ast(right);
                        let f = IR::new_with_context(
                            IRValue::Ident(name),
                            self.new_unknown_type(),
                            node.context.clone());

                        IR::new_with_context(
                            IRValue::Apply(Box::new(f), vec![ir_left, ir_right]),
                            self.new_unknown_type(),
                            node.context.clone())
                    }
                    _ => {
                        debug!("Unimplemented: {:?}", &node);
                        unimplemented!()
                    }
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
        let env = base_env();
        let mut c = TypeChecker::default();
        let s = SymbolTable::default();
        let mut ir = c.parse_str("1+1").unwrap();
        let s = c.assign_typenames(&mut ir, s);
        debug!("{:?}", ir);
        let mut ir = c.parse_str("1-1").unwrap();
        let s = c.assign_typenames(&mut ir, s);
        debug!("{:?}", ir);
        let mut ir = c.parse_str("
let f = \\x -> { x^2; };
y = 2
let mut x = 1.
let z = f(2)
").unwrap();
        let s = c.assign_typenames(&mut ir, s);
        let env = c.generate_equations(&ir, env);
        debug!("{}", ir);
        debug!("{:?}", s);
        c.results.print();
        for e in c.type_equations {
            debug!("E: {}", e);
        }

    }
}

