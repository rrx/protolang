use crate::ast::*;
use crate::results::*;
use crate::tokens::{FileId, Tok};
use crate::lexer::Location;
use log::*;
use nom::InputIter;
use rpds::HashTrieMap;
use std::fmt;

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
    String,
    Unknown(String),
    Void,
    Error,
    Func(FunctionSig),
    Type(String),
}

type FunctionSig = Vec<Type>;
struct TypeSig(Vec<Type>);

impl TypeSig {
    fn substitute(&self, subst: &SymbolTable) -> Self {
        Self(
            self.0
                .iter()
                .map(|v| {
                    if let Type::Unknown(name) = v {
                        if subst.contains_key(name) {
                            return subst.get(name).unwrap().clone();
                        }
                    }
                    v.clone()
                })
                .collect::<Vec<_>>(),
        )
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unknown(s) => write!(f, "T:{}", s),
            Self::Int => write!(f, "T:Int"),
            Self::Bool => write!(f, "T:Bool"),
            Self::String => write!(f, "T:String"),
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

    pub fn is_unknown(&self) -> bool {
        if let Type::Unknown(_) = self {
            true
        } else {
            false
        }
    }

    fn flatten(&self) -> TypeSig {
        match self {
            Type::Func(args) => TypeSig(args.clone()),
            _ => TypeSig(vec![self.clone()]),
        }
    }
}

pub type Environment = crate::compiler::env::EnvLayers<String, IR>;

type SymbolTable = HashTrieMap<String, Type>;

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Bool(bool),
    Int(u64),
    Float(f64),
    String(String),
}

#[derive(Clone, PartialEq)]
pub struct IR {
    ty: Type,
    value: IRValue,
    loc: Location,
    //context: MaybeNodeContext,
    env: Environment,
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
            _ => write!(f, "{:?}", &self),
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
        Self::new_with_location(v, ty, Location::default(), env)
    }

    fn new_with_location(v: IRValue, ty: Type, loc: Location, env: Environment) -> Self {
        Self {
            value: v,
            ty,
            loc,
            env,
        }
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

    let left = IR::new(IRValue::Ident("left".into()), left_ty.clone(), env.clone());

    let right = IR::new(
        IRValue::Ident("right".into()),
        right_ty.clone(),
        env.clone(),
    );

    let ret = IR::new(IRValue::Ident("ret".into()), ret_ty.clone(), env.clone());

    let args = vec![left, right, ret];

    let mut node = IR::new(
        IRValue::Extern(args),
        Type::Func(vec![left_ty, right_ty, ret_ty]),
        env.clone(),
    );

    env.define(name, node.clone());
    node.env = env;
    node
}

pub fn base_env() -> Environment {
    let mut env = Environment::default();
    let node = make_binary_function("*".into(), vec![Type::Int, Type::Int, Type::Int], env);
    env = node.env;
    let node = make_binary_function("*".into(), vec![Type::Float, Type::Float, Type::Float], env);
    env = node.env;
    let node = make_binary_function("*".into(), vec![Type::Int, Type::Float, Type::Float], env);
    env = node.env;
    let node = make_binary_function("*".into(), vec![Type::Float, Type::Int, Type::Float], env);
    env = node.env;

    let node = make_binary_function("+".into(), vec![Type::Int, Type::Int, Type::Int], env);
    env = node.env;
    let node = make_binary_function("+".into(), vec![Type::Float, Type::Float, Type::Float], env);
    env = node.env;
    let node = make_binary_function("+".into(), vec![Type::Int, Type::Float, Type::Float], env);
    env = node.env;
    let node = make_binary_function("+".into(), vec![Type::Float, Type::Int, Type::Float], env);
    env = node.env;

    let node = make_binary_function("-".into(), vec![Type::Int, Type::Int, Type::Int], env);
    env = node.env;
    let node = make_binary_function("-".into(), vec![Type::Float, Type::Float, Type::Float], env);
    env = node.env;
    let node = make_binary_function("-".into(), vec![Type::Int, Type::Float, Type::Float], env);
    env = node.env;
    let node = make_binary_function("-".into(), vec![Type::Float, Type::Int, Type::Float], env);
    env = node.env;

    let node = make_binary_function("^".into(), vec![Type::Int, Type::Int, Type::Int], env);
    env = node.env;
    let node = make_binary_function("^".into(), vec![Type::Float, Type::Int, Type::Float], env);
    env = node.env;
    let node = make_binary_function(">".into(), vec![Type::Int, Type::Int, Type::Bool], env);
    env = node.env;
    let node = make_binary_function(">".into(), vec![Type::Float, Type::Float, Type::Bool], env);
    env = node.env;
    env
}

fn op_name(op: &Operator) -> String {
    match op {
        Operator::Plus => "+".into(),
        Operator::Minus => "-".into(),
        Operator::Exp => "^".into(),
        Operator::Multiply => "*".into(),
        Operator::Divide => "/".into(),
        Operator::GreaterThan => ">".into(),
        Operator::GreaterThanEqual => ">=".into(),
        Operator::LessThan => "<".into(),
        Operator::LessThanEqual => "<=".into(),
        Operator::Equal => "==".into(),
        _ => {
            debug!("Not implemented: {:?}", op);
            //let msg = format!("Op Not Implemented: {}", name);
            //self.make_error(msg, node.context.clone(), env)
            unimplemented!()
        }
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

/*
enum PreparedType {
    Simple(Vec<PreparedType>),
    Structured(Vec<PreparedType>)
}

impl From<Type> for PreparedType {
    fn from(ty: Type) -> Self {

    }
}
*/

#[derive(Clone, Debug, PartialEq)]
pub struct TypeEquation {
    left: Type,
    // ordered set - should be ordered from general to specific
    right: Vec<Type>,
    node: IR,
    env: Environment,
}

impl TypeEquation {
    fn new(left: Type, right: Vec<Type>, node: IR, env: Environment) -> Self {
        Self {
            left,
            right,
            node,
            env,
        }
    }
}

impl fmt::Display for TypeEquation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} :: {:?}, from: {:?}",
            self.left,
            self.right,
            &self.node.loc
        )
    }
}

#[derive(Default)]
pub struct TypeChecker {
    results: CompileResults,
    pub type_equations: Vec<TypeEquation>,
    type_counter: usize,
}

impl TypeChecker {
    pub fn new_env() -> Environment {
        base_env()
    }

    pub fn print(&self) {
        self.results.print();
    }

    pub fn clear(&mut self) {
        self.results.clear();
    }

    pub fn push_error(&mut self, r: LangError) {
        self.results.push(r);
    }

    pub fn add_source(&mut self, filename: String, source: String) -> FileId {
        self.results.add_source(filename, source)
    }

    pub fn has_errors(&self) -> bool {
        self.results.has_errors
    }

    fn next_type_counter(&mut self) -> usize {
        let x = self.type_counter;
        self.type_counter += 1;
        x
    }

    fn get_fresh_typename(&mut self) -> String {
        format!("t{}", self.next_type_counter())
    }

    fn subs_if_exists<'a>(mut ty: &'a Type, subst: &'a SymbolTable) -> &'a Type {
        if let Type::Unknown(name) = ty {
            if subst.contains_key(name) {
                ty = subst.get(name).unwrap();
            }
        }
        ty
    }

    // Does ty1 occur in ty2?
    fn occurs_check(ty1: &Type, ty2: &Type, subst: &SymbolTable) -> bool {
        if ty1 == ty2 {
            return true;
        }

        assert!(!ty1.is_unknown());

        let ty2 = Self::subs_if_exists(ty2, &subst);

        if ty1 == ty2 {
            return true;
        }

        // if ty1 occurs in any of a functions parameters
        if let Type::Func(sig) = ty2 {
            return sig.iter().any(|s| Self::occurs_check(ty1, s, subst));
        }
        false
    }

    fn unify_eq(&self, ty1: &Type, ty2: &Type, subst: Option<SymbolTable>) -> Option<SymbolTable> {
        debug!("unify_eq: {:?} :: {:?}", ty1, ty2);

        if ty1 == ty2 {
            return subst;
        }

        let subst = subst.unwrap();

        // make substitutions
        let ty1 = Self::subs_if_exists(ty1, &subst);
        let ty2 = Self::subs_if_exists(ty2, &subst);

        //if !ty1.is_unknown() && !ty2.is_unknown()
        if let Type::Unknown(name) = ty1 {
            //if !ty2.is_unknown() {
            return Some(subst.insert(name.clone(), ty2.clone()));
            //}
        } else if let Type::Unknown(name) = ty2 {
            //if !ty1.is_unknown() {
            return Some(subst.insert(name.clone(), ty1.clone()));
            //}
        }

        if ty1 == ty2 {
            return Some(subst);
        }

        if Self::occurs_check(ty1, ty2, &subst) {
            return None;
        }

        if let Type::Func(sig1) = ty1 {
            if let Type::Func(sig2) = ty2 {
                return self.unify_fn(sig1, sig2, subst.clone());
            } else {
                return None;
            }
        }

        None
        //Some(subst)
    }

    fn unify_fn(
        &self,
        sig1: &Vec<Type>,
        sig2: &Vec<Type>,
        mut subst: SymbolTable,
    ) -> Option<SymbolTable> {
        debug!("unify_fn: {:?} :: {:?}", sig1, sig2);

        if sig1.len() != sig2.len() {
            return None;
        }

        let mut local_subst = SymbolTable::default();
        for (a1, a2) in sig1.iter().zip(sig2.iter()) {
            match self.unify(a1, &Vec::from([a2.clone()]), Some(local_subst)) {
                Some(s) => {
                    local_subst = s;
                }
                None => {
                    return None;
                }
            }
        }

        for (k, v) in local_subst.iter() {
            subst = subst.insert(k.clone(), v.clone());
        }

        debug!("unify_fn: {:?}", &subst);
        Some(subst)
        //if solutions.len() == 1 {
        //Some(solutions.get(0).unwrap().clone())
        //} else {
        //subst
        //}
    }

    pub fn unify(
        &self,
        left: &Type,
        right: &Vec<Type>,
        subst: Option<SymbolTable>,
    ) -> Option<SymbolTable> {
        debug!("unify: {:?} :: {:?}", left, right);
        if subst == None {
            return None;
        }

        let mut local_subst = None;
        for r in right {
            if let Some(s) = self.unify_eq(left, r, subst.clone()) {
                local_subst = Some(s);
                break;
            }
        }

        // return the solution if it was found
        local_subst
        //if local_subst == None {
        //None
        //} else {
        //local_subst
        //}

        //assert_eq!(eq.left.len(), 1);
        //assert!(eq.left.size() >= 1);
        //subst
        //match subst {
        //None => None,
        //Some(
        //}
    }

    pub fn unify_all(&mut self) -> Option<SymbolTable> {
        let mut subst = Some(SymbolTable::default());
        let mut errors = vec![];
        for eq in &self.type_equations {
            //let sig1 = flatten_type(eq.left);
            //let sig2 = flatten_type(eq.left);
            match self.unify(&eq.left, &eq.right, subst.clone()) {
                Some(s) => {
                    //for x in &s {
                    //debug!("subst: {:?}", x)
                    //}
                    subst = Some(s);
                }
                None => {
                    //debug!("Eq failed: {:?}", &eq);
                    errors.push(eq);
                    //break;
                }
            }
        }
        for eq in errors {
            let msg = format!("Unable to unify: {:?} :: {:?}", &eq.left, &eq.right);
            self.results
                .push(LangError::error(msg.clone(), eq.node.loc.clone()));
            //IR::new_with_location(
            //IRValue::Error(msg),
            //Type::Error,
            //eq.node.context.clone(),
            //eq.env);
            //self.make_error(format!("Unable to unify: {:?} :: {:?}", &eq.left, &eq.right), eq.node.context.clone(), eq.env.clone());
        }
        subst
    }

    /*
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
    */

    /*
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
    */

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
            Expr::Literal(Tok::FloatLiteral(f)) => IR::new_with_location(
                IRValue::Literal(Literal::Float(*f)),
                Type::Float,
                node.context.to_location(),
                env,
            ),
            Expr::Literal(Tok::IntLiteral(i)) => IR::new_with_location(
                IRValue::Literal(Literal::Int(*i)),
                Type::Int,
                node.context.to_location(),
                env,
            ),
            Expr::Literal(Tok::BoolLiteral(b)) => IR::new_with_location(
                IRValue::Literal(Literal::Bool(*b)),
                Type::Bool,
                node.context.to_location(),
                env,
            ),
            Expr::Literal(Tok::StringLiteral(s)) => IR::new_with_location(
                IRValue::Literal(Literal::String(s.clone())),
                Type::String,
                node.context.to_location(),
                env,
            ),
            Expr::Literal(_) => unimplemented!(),

            Expr::Ident(s) => {
                let ty = self.new_unknown_type();
                self.make_ident_from_name(s.name.clone(), ty, node.context.to_location(), env)
            }
            //IR::new_with_location(
            //IRValue::Ident(s.name.clone()),
            //self.new_unknown_type(),
            //node.context.clone(), env),
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
                IR::new_with_location(
                    IRValue::Function(Box::new(body), ir_args),
                    self.new_unknown_type(),
                    node.context.to_location(),
                    env,
                )
            }

            Expr::Block(exprs) | Expr::Program(exprs) => {
                let mut local_env = env.clone();
                let mut ir_exprs = vec![];
                for p in exprs {
                    let node = self.parse_ast(p, local_env);
                    local_env = node.env.clone();
                    ir_exprs.push(node);
                }

                self.make_block(ir_exprs, node.context.to_location(), env)
            }

            Expr::Prefix(op, right) => {
                let name = op_name(op);
                let ir_right = self.parse_ast(right, env.clone());
                self.make_apply_by_name(name, vec![ir_right], node.context.to_location(), env)
            }

            Expr::Binary(op, left, right) => {
                match &op.value {
                    Operator::Assign => {
                        let name = left.try_ident().unwrap().name;
                        let ir_right = self.parse_ast(right, env.clone());
                        self.make_assign(name, ir_right, node.context.to_location(), env.clone())
                    }

                    Operator::Declare => {
                        let name = left.try_ident().unwrap().name;
                        let ir_right = self.parse_ast(right, env.clone());
                        self.make_declare(name, ir_right, node.context.to_location(), env)
                    }

                    _ => {
                        let name = op_name(op);
                        log::debug!(
                            "{:?}",
                            (
                                &node.context.to_location(),
                                &left.context.to_location(),
                                &right.context.to_location()
                            )
                        );
                        let ir_left = self.parse_ast(left, env.clone());
                        let ir_right = self.parse_ast(right, env.clone());
                        self.make_apply_by_name(
                            name,
                            vec![ir_left, ir_right],
                            op.context.to_location(),
                            env,
                        )
                    } //_ => {
                      //let name = op_name(op);
                      //debug!("Unimplemented: {:?}", &node);
                      //let msg = format!("Op Not Implemented: {}", name);
                      //self.make_error(msg, node.context.clone(), env)
                      //unimplemented!()
                      //}
                }
            }

            Expr::Ternary(op, a, b, c) => match op.value {
                Operator::Conditional => {
                    let ir_a = self.parse_ast(a, env.clone());
                    let ir_b = self.parse_ast(b, env.clone());
                    let ir_c = self.parse_ast(c, env.clone());
                    self.make_apply_by_name(
                        "cond".into(),
                        vec![ir_a, ir_b, ir_c],
                        node.context.to_location(),
                        env,
                    )
                }
                _ => unimplemented!(),
            },

            _ => {
                debug!("Unimplemented: {:?}", &node);
                unimplemented!()
            }
        }
    }

    fn make_block(&mut self, nodes: Vec<IR>, loc: Location, env: Environment) -> IR {
        let block_ty = nodes.last().unwrap().ty.clone();
        let block = IR::new_with_location(
            IRValue::Block(nodes),
            block_ty.clone(),
            loc,
            env,
        );

        //let env = block.env.clone();
        //self.type_equations.push(TypeEquation::new(
        //HashSet::from([block_ty]),
        //HashSet::from([last_ty]),
        //block.clone(), env));
        block
    }

    fn make_assign(
        &mut self,
        name: String,
        node: IR,
        loc: Location,
        env: Environment,
    ) -> IR {
        if let Some(v) = env.resolve(&name) {
            let left_ty = v.ty.clone();
            let right_ty = node.ty.clone();
            let result_ty = self.new_unknown_type();
            let result = IR::new_with_location(
                IRValue::Assign(name, Box::new(node.clone())),
                result_ty.clone(),
                node.loc.clone(),
                env.clone(),
            );

            self.type_equations.push(TypeEquation::new(
                left_ty,
                Vec::from([right_ty.clone()]),
                node.clone(),
                env.clone(),
            ));

            self.type_equations.push(TypeEquation::new(
                result_ty,
                Vec::from([right_ty]),
                result.clone(),
                env,
            ));
            result
        } else {
            self.make_error(format!("Not found: {}", name), loc, env)
        }
    }

    fn make_declare(
        &mut self,
        name: String,
        node: IR,
        loc: Location,
        env: Environment,
    ) -> IR {
        let mut local_env = env.clone();
        let right_ty = node.ty.clone();
        local_env.define(name.clone(), node.clone());
        let expr = IR::new_with_location(
            IRValue::Declare(name, Box::new(node)),
            right_ty.clone(),
            loc,
            local_env,
        );
        //let env = expr.env.clone();
        //self.type_equations.push(TypeEquation::new(
        //HashSet::from([expr.ty.clone()]),
        //HashSet::from([right_ty]),
        //expr.clone(), env));
        expr
    }

    fn make_error(&mut self, msg: String, loc: Location, env: Environment) -> IR {
        self.results
            .push(LangError::error(msg.clone(), loc.clone()));
        IR::new_with_location(IRValue::Error(msg), Type::Error, loc, env)
    }

    fn make_func_from_name(
        &mut self,
        name: String,
        ty: Type,
        loc: Location,
        env: Environment,
    ) -> IR {
        // get all of the types that match the name
        let mut fn_types = Vec::new();
        for f in env.resolve_all(&name) {
            match &f.value {
                IRValue::Function(_, _) | IRValue::Extern(_) => {
                    fn_types.push(f.ty.clone());
                }
                _ => (),
            }
        }
        if fn_types.len() == 0 {
            let msg = format!("Function Not found: {}", name);
            self.make_error(msg, loc, env)
        } else {
            let result = IR::new_with_location(
                IRValue::Ident(name),
                ty.clone(),
                loc,
                env.clone(),
            );
            self.type_equations.push(TypeEquation::new(
                ty.clone(),
                fn_types,
                result.clone(),
                env.clone(),
            ));
            result
        }
    }

    fn make_ident_from_name(
        &mut self,
        name: String,
        ty: Type,
        loc: Location,
        env: Environment,
    ) -> IR {
        if let Some(v) = env.resolve(&name) {
            let v_ty = v.ty.clone();
            let result = IR::new_with_location(
                IRValue::Ident(name),
                v_ty.clone(),
                loc,
                env.clone(),
            );

            self.type_equations.push(TypeEquation::new(
                ty.clone(),
                Vec::from([v_ty.clone()]),
                result.clone(),
                env.clone(),
            ));
            result
        } else {
            self.make_error(format!("Not found: {}", name), loc, env)
        }
    }

    fn make_ident(&mut self, node: IR, env: Environment) -> IR {
        match &node.value {
            IRValue::Ident(name) => {
                self.make_ident_from_name(name.clone(), node.ty.clone(), node.loc, env)
            }
            _ => {
                debug!("Unimplemented: {:?}", &node);
                unimplemented!()
            }
        }
    }

    fn make_apply_by_name(
        &mut self,
        name: String,
        ir_args: Vec<IR>,
        loc: Location,
        env: Environment,
    ) -> IR {
        let ret_ty = self.new_unknown_type();
        let mut f_types = ir_args.iter().map(|v| v.ty.clone()).collect::<Vec<_>>();
        f_types.push(ret_ty.clone());
        let f_ty = Type::Func(f_types);
        //let f = IR::new_with_location(
        //IRValue::Ident(name.clone()),
        //f_ty,
        //context,
        //env.clone());
        let f = self.make_func_from_name(name.clone(), f_ty, loc.clone(), env.clone());
        IR::new_with_location(
            IRValue::Apply(Box::new(f), ir_args),
            ret_ty,
            loc,
            env.clone(),
        )
        //self.make_apply(f, ir_args, env)
    }

    fn make_apply(&mut self, ir_func: IR, ir_args: Vec<IR>, env: Environment) -> IR {
        let ret_ty = self.new_unknown_type();
        let ir_func = match &ir_func.value {
            IRValue::Ident(name) => {
                // make function signature
                let mut f_types = ir_args.iter().map(|v| v.ty.clone()).collect::<Vec<_>>();
                f_types.push(ret_ty.clone());
                let f_ty = Type::Func(f_types);

                // make a node with that signature
                self.make_func_from_name(name.clone(), f_ty, ir_func.loc, env.clone())
            }
            IRValue::Extern(_) | IRValue::Function(_, _) => ir_func,
            IRValue::Error(_) => ir_func,
            _ => {
                debug!("Unimplemented: {:?}", &ir_func);
                unimplemented!()
            }
        };

        let loc = ir_func.loc.clone();
        IR::new_with_location(
            IRValue::Apply(Box::new(ir_func), ir_args),
            ret_ty,
            loc,
            env.clone(),
        )
    }

    pub fn parse_str(&mut self, s: &str, env: Environment) -> anyhow::Result<IR> {
        self._parse("<repl>", s, env)
    }

    pub fn parse_file(&mut self, filename: &str, env: Environment) -> anyhow::Result<IR> {
        let contents = std::fs::read_to_string(filename.clone())
            .unwrap()
            .to_string();
        self._parse(filename, &contents, env)
    }

    fn _parse(&mut self, filename: &str, contents: &str, env: Environment) -> anyhow::Result<IR> {
        let file_id = self
            .results
            .add_source(filename.into(), contents.to_string());
        let mut lexer = crate::lexer::LexerState::from_str_eof(&contents)
            .unwrap()
            .set_file_id(file_id);
        let tokens = lexer.tokens();

        for t in tokens.iter_elements() {
            debug!("T: {:?}", t);
        }
        let (_, node) = crate::parser::parse_program(tokens).unwrap();
        debug!("Node: {:?}", node);
        let ir = self.parse_ast(&node, env);
        Ok(ir)
    }

    pub fn check_str(&mut self, s: &str, env: Environment) -> anyhow::Result<IR> {
        self._check("<repl>", s, env)
    }

    pub fn check_file(&mut self, filename: &str, env: Environment) -> anyhow::Result<IR> {
        let contents = std::fs::read_to_string(filename.clone())
            .unwrap()
            .to_string();
        self._check(filename, &contents, env)
    }

    fn _check(&mut self, filename: &str, contents: &str, env: Environment) -> anyhow::Result<IR> {
        let ir = self._parse(filename, contents, env.clone()).unwrap();
        debug!("{}", ir);

        for e in &self.type_equations {
            debug!("E: {}", e);
        }
        let s = self.unify_all().unwrap();
        for x in &s {
            debug!("subst: {:?}", x);
        }
        debug!("has_errors: {}", self.has_errors());
        self.print();
        Ok(ir)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use log::debug;
    use test_log::test;

    #[test]
    fn analyze() {
        let env = base_env();
        let mut c = TypeChecker::default();
        let s = SymbolTable::default();
        /*
        let f = \\x -> { x^2; };
        y = 2
        let mut x = 1.
        let z = f(2)";
                */
        let p = "
let x = 1
1+1
1-1
y = 1
x + 2
        ";
        let ir = c.parse_str(p, env).unwrap();
        debug!("{}", p);
        debug!("{}", ir);
        debug!("{:?}", s);
        for e in &c.type_equations {
            debug!("E: {}", e);
        }
        c.print();
    }

    #[test]
    fn func_match() {
        let c = TypeChecker::default();
        let s = SymbolTable::default();
        let out = c.unify(
            &Type::Func(vec![Type::Int, Type::Unknown("x".into())]),
            &vec![
                Type::Func(vec![Type::Float, Type::Float]),
                Type::Func(vec![Type::Int, Type::Int]),
            ],
            Some(s),
        );
        debug!("{:?}", out.as_ref().unwrap().iter().collect::<Vec<_>>());
        assert_eq!(out.unwrap().get("x".into()), Some(&Type::Int));
    }

    #[test]
    fn func_mismatch() {
        let c = TypeChecker::default();
        let s = SymbolTable::default();
        let out = c.unify(
            &Type::Func(vec![Type::Float, Type::Unknown("x".into())]),
            &vec![Type::Func(vec![Type::Int, Type::Int])],
            Some(s),
        );
        assert_eq!(out, None)
    }

    #[test]
    fn types_match() {
        let c = TypeChecker::default();
        let s = SymbolTable::default();
        let out = c.unify(&Type::Int, &vec![Type::Float, Type::Int], Some(s.clone()));
        assert_eq!(out, Some(s));
    }

    #[test]
    fn types_mismatch() {
        let c = TypeChecker::default();
        let s = SymbolTable::default();
        let out = c.unify(&Type::Int, &vec![Type::Float], Some(s));
        assert_eq!(out, None);
    }

    #[test]
    fn types_unknown() {
        let c = TypeChecker::default();
        let s = SymbolTable::default();
        let out = c.unify(&Type::Unknown("x".into()), &vec![Type::Float], Some(s));
        assert_eq!(out.unwrap().get("x".into()), Some(&Type::Float));

        let s = SymbolTable::default();
        let out = c.unify(
            &Type::Unknown("x".into()),
            &vec![Type::Float, Type::Int],
            Some(s),
        );
        assert_eq!(out.unwrap().get("x".into()), Some(&Type::Float));

        let s = SymbolTable::default();
        let out = c
            .unify(
                &Type::Unknown("x".into()),
                &vec![Type::Unknown("y".into())],
                Some(s),
            )
            .unwrap();
        //assert_eq!(out.get("y".into()), Some(&Type::Float));
        assert_eq!(out.get("x".into()), Some(&Type::Unknown("y".into())));

        // no match
        let s = SymbolTable::default();
        let out = c.unify(
            &Type::Func(vec![Type::Int, Type::Float, Type::Unknown("x".into())]),
            &vec![
                Type::Func(vec![Type::Float, Type::Float, Type::Float]),
                Type::Func(vec![Type::Int, Type::Int, Type::Int]),
            ],
            Some(s),
        );
        assert_eq!(out, None);
    }
}