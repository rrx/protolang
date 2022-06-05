use crate::ast::*;
use crate::eval::*;
use crate::tokens::Tok;
use crate::program::Program;
use crate::results::*;
use log::*;
use std::fmt;
use rpds::HashTrieMap;
use std::collections::HashSet;

type FunctionSig = Vec<Type>;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
    context: MaybeNodeContext,
    env: Environment
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

    /// Error value
    Error(String),
}

impl crate::compiler::env::LayerValue for IR {}

impl IR {
    fn new(v: IRValue, ty: Type, env: Environment) -> Self {
        Self::new_with_context(v, ty, MaybeNodeContext::default(), env)
    }

    fn new_with_context(v: IRValue, ty: Type, context: MaybeNodeContext, env: Environment) -> Self {
        Self { value: v, ty, context, env }
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

fn make_binary_function(name: String, args: Vec<Type>, mut env: Environment) -> IR {
    assert_eq!(args.len(), 3);
    let left_ty = args.get(0).unwrap().clone();
    let right_ty = args.get(1).unwrap().clone();
    let ret_ty = args.get(2).unwrap().clone();

    let left = IR::new(
        IRValue::Ident("left".into()),
        left_ty.clone(),
        env.clone());

    let right = IR::new(
        IRValue::Ident("right".into()),
        right_ty.clone(),
        env.clone());

    let ret = IR::new(
        IRValue::Ident("ret".into()),
        ret_ty.clone(),
        env.clone());

    let args = vec![left, right, ret];

    let mut node = IR::new(
        IRValue::Extern(args),
        Type::Func(vec![left_ty, right_ty, ret_ty]),
        env.clone());

    env.define(name, node.clone());
    node.env = env; 
    node
}


pub fn base_env() -> Environment {
    let mut env = Environment::default();
    let node = make_binary_function("+".into(), vec![Type::Int, Type::Int, Type::Int], env);
    env = node.env;
    let node = make_binary_function("+".into(), vec![Type::Float, Type::Float, Type::Float], env);
    env = node.env;
    let node = make_binary_function("-".into(), vec![Type::Int, Type::Int, Type::Int], env);
    env = node.env;
    let node = make_binary_function("-".into(), vec![Type::Float, Type::Float, Type::Float], env);
    env = node.env;
    let node = make_binary_function("^".into(), vec![Type::Int, Type::Int, Type::Int], env);
    env = node.env;
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
    left: HashSet<Type>,
    right: HashSet<Type>,
    node: IR,
    env: Environment,
}

impl TypeEquation {
    fn new(left: HashSet<Type>, right: HashSet<Type>, node: IR, env: Environment) -> Self {
        Self { left, right, node, env }
    }
}

impl fmt::Display for TypeEquation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} :: {:?}, from: {}", self.left, self.right, self.node)
    }
}


#[derive(Default)]
pub struct TypeChecker {
    pub results: CompileResults,
    pub type_equations: Vec<TypeEquation>,
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

    fn make_binary_function(&mut self, name: String, mut env: Environment) -> IR {
        let left_ty = self.new_unknown_type();
        let right_ty = self.new_unknown_type();
        let ret_ty = self.new_unknown_type();

        let left = IR::new(
            IRValue::Ident("left".into()),
            left_ty.clone(),
            env.clone());

        let right = IR::new(
            IRValue::Ident("right".into()),
            right_ty.clone(),
            env.clone());

        let ret = IR::new(
            IRValue::Ident("ret".into()),
            ret_ty.clone(),
            env.clone());

        let args = vec![left, right, ret];

        let node = IR::new(
            IRValue::Extern(args),
            Type::Func(vec![left_ty, right_ty, ret_ty]),
            env.clone());

        env.define(name, node.clone());

        node
    }



    fn generate_equations(&mut self, node: &IR) {
        match &node.value {
            // already typed
            IRValue::Type(_) => (),
            IRValue::Literal(_) => {
            }
            IRValue::Ident(_) => (),

            IRValue::Declare(ident, expr) => {
                //env.define(ident.clone(), *expr.clone());
                self.generate_equations(&expr);
                //self.type_equations.push(TypeEquation::new(expr.ty.clone(), node.ty.clone(), node.clone(), node.env.clone()));
            }

            IRValue::Assign(ident, expr) => {
                let v = node.env.resolve(ident).unwrap();
                self.generate_equations(&expr);
                //self.type_equations.push(TypeEquation::new(expr.ty.clone(), node.ty.clone(), node.clone(), node.env.clone()));
                //} else {
                    //self.results.push(LangError::error("Not found".into(), node.context.clone()));
                //}
            }

            IRValue::Apply(f, params) => {
                self.generate_equations(f);
                for p in params {
                    self.generate_equations(p);
                }
            }

            IRValue::Extern(args) => {
                // generate type signature for function
                let mut arg_types = args.iter().map(|v| v.ty.clone()).collect::<Vec<_>>();

                // function has a function type
                //self.type_equations.push(TypeEquation::new(node.ty.clone(), Type::Func(arg_types), node.clone(), node.env.clone()));
            }

            IRValue::Function(body, args) => {
                //for arg in args {
                    //self.generate_equations(&arg);
                //}
                self.generate_equations(body);

                // generate type signature for function
                let mut arg_types = args.iter().map(|v| v.ty.clone()).collect::<Vec<_>>();
                // append return type
                arg_types.push(node.ty.clone());

                // function has a function type
                //self.type_equations.push(TypeEquation::new(node.ty.clone(), Type::Func(arg_types), node.clone(), node.env.clone()));
            }

            /*
            IRValue::Block(exprs) => {
                for e in exprs {
                    self.generate_equations(&e);
                }
                let last = exprs.last().unwrap();
                self.type_equations.push(TypeEquation::new(node.ty.clone(), last.ty.clone(), node.clone(), node.env.clone()));
            }
            */
            _ => {
                debug!("Unimplemented: {:?}", &node);
                unimplemented!()
            }
        }
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

    pub fn parse_ast(&mut self, node: &ExprNode, env: Environment) -> IR {
        match &node.value {
            /*
            Expr::Program(exprs) => {
                let mut body = vec![];
                let mut local_env = env.clone();
                for n in exprs {
                    let ir = self.parse_ast(n, local_env);
                    local_env = ir.env.clone(); 
                    body.push(ir);
                }
                IR::new(IRValue::Block(body), self.new_unknown_type(), env)


                /*
                IR::new(
                    IRValue::Function(
                        Box::new(
                            IR::new(IRValue::Block(body), self.new_unknown_type())
                        ),
                        vec![],  // args
                    ), self.new_unknown_type())
                */
            }
            */
            Expr::Literal(Tok::FloatLiteral(f)) => IR::new_with_context(
                IRValue::Literal(Literal::Float(*f)),
                Type::Float,
                node.context.clone(),
                env),
            Expr::Literal(Tok::IntLiteral(i)) => IR::new_with_context(
                IRValue::Literal(Literal::Int(*i)),
                Type::Int,
                node.context.clone(), env),
            Expr::Literal(Tok::BoolLiteral(b)) => IR::new_with_context(
                IRValue::Literal(Literal::Bool(*b)),
                Type::Bool,
                node.context.clone(), env),
            Expr::Literal(_) => unimplemented!(),

            Expr::Ident(s) => IR::new_with_context(
                IRValue::Ident(s.name.clone()),
                self.new_unknown_type(),
                node.context.clone(), env),

            Expr::Apply(f, args) => {
                let ir_func = self.parse_ast(f, env.clone());

                let mut local_env = env.clone();
                let mut ir_args = vec![];
                for arg in args {
                    let node = self.parse_ast(arg, local_env);
                    local_env = node.env.clone();
                    ir_args.push(node);
                }

                self.make_apply(ir_func, ir_args, local_env)
            }

            Expr::Lambda(f) => {
                let mut local_env = env.clone();
                let mut ir_args = vec![];
                for p in &f.params.value {
                    let node = self.parse_ast(p, local_env);
                    local_env = node.env.clone();
                    ir_args.push(node);
                }

                // use local environment when parsing body
                let body = self.parse_ast(&f.expr, local_env);

                // use original context for the node
                IR::new_with_context(
                    IRValue::Function(Box::new(body), ir_args),
                    self.new_unknown_type(),
                    node.context.clone(), env)
            }

            Expr::Block(exprs) | Expr::Program(exprs) => {
                let mut local_env = env.clone();
                let mut ir_exprs = vec![];
                for p in exprs {
                    let node = self.parse_ast(p, local_env);
                    local_env = node.env.clone();
                    ir_exprs.push(node);
                }

                let last_ty = ir_exprs.last().unwrap().ty.clone();

                let block_ty = self.new_unknown_type();
                let block = IR::new_with_context(
                    IRValue::Block(ir_exprs),
                    block_ty.clone(),
                    node.context.clone(), env);
            
                let env = block.env.clone();
                self.type_equations.push(TypeEquation::new(
                        HashSet::from([block_ty]),
                        HashSet::from([last_ty]),
                        block.clone(), env));
                block
            }

            Expr::Binary(op, left, right) => {
                match op {
                    Operator::Assign => {
                        let name = left.try_ident().unwrap().name;
                        if let Some(v) = env.resolve(&name) {
                            let left_ty = v.ty.clone();
                            let ir_right = self.parse_ast(right, env.clone());
                            let right_ty = ir_right.ty.clone();
                            let result_ty = self.new_unknown_type();
                            let result = IR::new_with_context(
                                IRValue::Assign(name, Box::new(ir_right.clone())),
                                result_ty.clone(),
                                node.context.clone(), env.clone());

                            self.type_equations.push(TypeEquation::new(
                                    HashSet::from([left_ty]),
                                    HashSet::from([right_ty.clone()]),
                                    ir_right.clone(), env.clone()));

                            self.type_equations.push(TypeEquation::new(
                                    HashSet::from([result_ty]),
                                    HashSet::from([right_ty]),
                                    result.clone(), env));
                            result
                        } else {
                            self.results.push(LangError::error("Not found".into(), node.context.clone()));
                            IR::new_with_context(
                                IRValue::Error("Not found".into()),
                                Type::Error,
                                node.context.clone(),
                                env)
                        }
                    }

                    Operator::Declare => {
                        let mut local_env = env.clone();
                        let name = left.try_ident().unwrap().name;
                        let ir_right = self.parse_ast(right, env);
                        let right_ty = ir_right.ty.clone();
                        local_env.define(name.clone(), ir_right.clone());
                        let expr = IR::new_with_context(
                            IRValue::Declare(name, Box::new(ir_right)),
                            right_ty.clone(),
                            node.context.clone(), local_env);
                        let env = expr.env.clone();
                        //self.type_equations.push(TypeEquation::new(
                                //HashSet::from([expr.ty.clone()]),
                                //HashSet::from([right_ty]),
                                //expr.clone(), env));
                        expr
                    }

                    Operator::Plus | Operator::Minus | Operator::Exp => {
                        let name = op_name(op);
                        let ir_left = self.parse_ast(left, env.clone());
                        let ir_right = self.parse_ast(right, env.clone());
                        let ret_ty = self.new_unknown_type();
                        let f_ty = Type::Func(vec![ir_left.ty.clone(), ir_right.ty.clone(), ret_ty.clone()]);
                        let f = IR::new_with_context(
                            IRValue::Ident(name.clone()),
                            f_ty,
                            node.context.clone(),
                            env.clone());
                        //let f = self.make_func_from_name(name, f_ty, node.context.clone(), env.clone()); 
                        self.make_apply(f, vec![ir_left, ir_right], env)
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

    fn make_func_from_name(&mut self, name: String, ty: Type, context: MaybeNodeContext, env: Environment) -> IR {
        // get all of the types that match the name
        let mut fn_types = HashSet::new();
        for f in env.resolve_all(&name) {
            match &f.value {
                IRValue::Function(_,_) | IRValue::Extern(_) => {
                    fn_types.insert(f.ty.clone());
                }
                _ => ()
            }
        }
        if fn_types.len() == 0 {
            let err = format!("Function Not found: {}", name);
            self.results.push(LangError::error(err.clone(), context.clone()));
            IR::new_with_context(
                IRValue::Error(err),
                Type::Error,
                context,
                env)
        } else {
            let result = IR::new_with_context(
                IRValue::Ident(name),
                ty.clone(),
                context.clone(), env.clone());
            self.type_equations.push(TypeEquation::new(
                    HashSet::from([ty.clone()]),
                    fn_types,
                    result.clone(), env.clone()));
            result
        }
    }

    fn make_ident_from_name(&mut self, name: String, ty: Type, context: MaybeNodeContext, env: Environment) -> IR {
        if let Some(v) = env.resolve(&name) {
            let v_ty = v.ty.clone();
            let result = IR::new_with_context(
                IRValue::Ident(name),
                v_ty.clone(),
                context.clone(), env.clone());

            self.type_equations.push(TypeEquation::new(
                    HashSet::from([ty.clone()]),
                    HashSet::from([v_ty.clone()]),
                    result.clone(), env.clone()));
            result
        } else {
            self.results.push(LangError::error("Not found".into(), context.clone()));
            IR::new_with_context(
                IRValue::Error("Not found".into()),
                Type::Error,
                context,
                env)
        }
    }

    fn make_ident(&mut self, node: IR, env: Environment) -> IR {
        match &node.value {
            IRValue::Ident(name) => {
                self.make_ident_from_name(name.clone(), node.ty.clone(), node.context.clone(), env)
            }
            _ => {
                debug!("Unimplemented: {:?}", &node);
                unimplemented!()
            }
        }
    }

    fn make_apply(&mut self, ir_func: IR, ir_args: Vec<IR>, env: Environment) -> IR {
        //let mut fn_types = HashSet::new();

        let ret_ty = self.new_unknown_type();
        let ir_func = match &ir_func.value {
            IRValue::Ident(name) => {

                // make function signature
                let mut f_types = ir_args.iter().map(|v| v.ty.clone()).collect::<Vec<_>>();
                f_types.push(ret_ty.clone());
                let f_ty = Type::Func(f_types);
                //fn_types.insert(f_ty.clone());

                // make a node with that signature
                self.make_func_from_name(name.clone(), f_ty, ir_func.context.clone(), env.clone())
            }
            IRValue::Extern(_) | IRValue::Function(_, _) => {
                //fn_types.insert(ir_func.ty.clone());
                ir_func
            }
            IRValue::Error(_) => ir_func,
            _ => {
                debug!("Unimplemented: {:?}", &ir_func);
                unimplemented!()
            }
        };

        let context = ir_func.context.clone();
        let app = IR::new_with_context(
            IRValue::Apply(Box::new(ir_func), ir_args),
            ret_ty,
            context,
            env.clone());

        /*
        self.type_equations.push(TypeEquation::new(
                HashSet::from([app.ty.clone()]),
                fn_types,
                app.clone(), env));
        */
        app
    }

    pub fn parse_str(&mut self, s: &str, env: Environment) -> anyhow::Result<IR> {
        let node = Program::parse(s)?;
        debug!("parse {:?}", &node.value);
        let ir = self.parse_ast(&node, env);
        Ok(ir)
    }

    pub fn parse_file(&mut self, filename: &str, env: Environment) -> anyhow::Result<IR> {
        let node = Program::parse_file(filename)?;
        let ir = self.parse_ast(&node, env);
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
        /*
        let mut ir = c.parse_str("1+1").unwrap();
        let s = c.assign_typenames(&mut ir, s);
        debug!("{:?}", ir);
        let mut ir = c.parse_str("1-1").unwrap();
        let s = c.assign_typenames(&mut ir, s);
        debug!("{:?}", ir);
        let p = "
let f = \\x -> { x^2; };
y = 2
let mut x = 1.
let z = f(2)";
        */
        let p = "
let x = 1
y = 1
x + 2
        ";
        let mut ir = c.parse_str(p, env).unwrap();
        let s = c.assign_typenames(&mut ir, s);
        //c.generate_equations(&ir);
        println!("{}", p);
        println!("{}", ir);
        println!("{:?}", s);
        for e in c.type_equations {
            println!("E: {}", e);
        }
        c.results.print();
    }
}

