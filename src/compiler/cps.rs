use super::env::*;
use super::types::*;
use crate::ast::CallTable;
use crate::ast::{Expr, ExprNode, Operator};
use crate::tokens::Tok;
use log::debug;
use rpds::HashTrieMap;
use std::borrow::BorrowMut;
use std::cell::{Ref, RefCell};
use std::collections::{HashMap, HashSet};
use std::convert::{From, TryFrom};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum Scope {
    Local,
    Global,
    Module,
    File,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Lifetime {
    Static, // static allocation
    Auto,   // auto allocation
    GC,     // garbage collected allocation
}

#[derive(Clone, Debug, PartialEq)]
pub enum StoreLocation {
    Stack,
    Heap,
    Extern,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    TBool,
    TInt,
    TFloat,
    //TPointer,
    TAggregate,
    TFunction,
    TComposite,
    TModule,
    TFile,
    TType,
    //Eval,
    TVoid,
    Unknown,
}
impl Type {}

#[derive(Clone, Debug, PartialEq)]
pub enum Access {
    Mutable,
    Default,
}

#[derive(Clone, Debug)]
pub struct TypeAccess {
    access: Access,
    is_ref: bool,
    spec: TypeSpec,
    loc: StoreLocation,
}

#[derive(Clone, Debug)]
pub enum TypedValue {
    Bool(bool),
    Int(i64),
    Float(f64),
    Type(Type),
    Agg(usize, Box<TypedValue>),
    Composite(Vec<TypedValue>),
    //Pointer(String),
    Function(Extern),
    //Eval(String),
    Void,
}
impl TypedValue {
    pub fn new_extern(ext: Extern) -> Self {
        Self::Function(ext)
    }

    pub fn new_agg(len: usize, ty: TypedValue) -> Self {
        Self::Agg(len, Box::new(ty))
    }

    pub fn infer_type(&self) -> Type {
        match self {
            Self::Bool(_) => Type::TBool,
            Self::Int(_) => Type::TInt,
            Self::Float(_) => Type::TFloat,
            Self::Type(_) => Type::TType,
            Self::Agg(_, _) => Type::TAggregate,
            Self::Composite(_) => Type::TComposite,
            //Self::Pointer(_) => Type::TPointer,
            //Self::Eval(_) => Type::Eval,
            Self::Function(_) => Type::TFunction,
            Self::Void => Type::TVoid,
        }
    }

    pub fn try_bool(&self) -> Option<bool> {
        if let Self::Bool(u) = self {
            Some(*u)
        } else {
            None
        }
    }

    pub fn try_int(&self) -> Option<i64> {
        if let Self::Int(u) = self {
            Some(*u)
        } else {
            None
        }
    }

    pub fn try_float(&self) -> Option<f64> {
        if let Self::Float(u) = self {
            Some(*u)
        } else {
            None
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StoredType {
    pub ty: Type,
    pub loc: StoreLocation,
    pub is_mutable: bool,
}
impl LayerValue for StoredType {}
impl StoredType {
    pub fn new(ty: Type) -> Self {
        Self {
            ty,
            loc: StoreLocation::Stack,
            is_mutable: false,
        }
    }

    pub fn new_extern() -> Self {
        Self {
            ty: Type::TFunction,
            loc: StoreLocation::Extern,
            is_mutable: false,
        }
    }

    pub fn infer_type(&self) -> Type {
        self.ty.clone()
    }

    pub fn make_mut(mut self) -> Self {
        self.is_mutable = true;
        self
    }
}

#[derive(Clone, Debug)]
pub struct StoredValue {
    pub value: TypedValue,
    loc: StoreLocation,
    pub is_mutable: bool,
}
impl StoredValue {
    pub fn infer_type(&self) -> Type {
        self.value.infer_type()
    }
}

#[derive(Clone, Debug)]
pub struct Pointer {
    id: u64,
}
impl Pointer {
    fn name(&self) -> String {
        String::from(format!("r{}", self.id))
    }
}

#[derive(Clone, Debug)]
pub struct Op {
    kind: Operator,
    args: Vec<Type>,
    ret: Type,
}
impl Op {
    pub fn arity(&self) -> usize {
        self.args.len()
    }
    pub fn infer_type(&self) -> Type {
        self.ret.clone()
    }

    pub fn prefix(
        builder: &mut CPSBuilder,
        scope: Environment,
        op: Operator,
        value: Expr,
    ) -> (Vec<CPSIR>, Type, Environment) {
        match op {
            Operator::Plus | Operator::Minus => {
                let (value, t_value, scope) = builder.from_expr(value, scope);
                let f = Op {
                    kind: op,
                    args: vec![t_value.clone()],
                    ret: t_value.clone(),
                };
                (
                    vec![value, vec![CPSIR::Call(f)]]
                        .into_iter()
                        .flatten()
                        .collect(),
                    t_value,
                    scope,
                )
            }
            _ => unimplemented!(),
        }
    }

    pub fn binary(
        builder: &mut CPSBuilder,
        scope: Environment,
        op: Operator,
        left: Expr,
        right: Expr,
    ) -> (Vec<CPSIR>, Type, Environment) {
        match op {
            Operator::Declare => {
                let ident = left.try_ident().unwrap();
                //let v = StoredValue { value: TypedValue
                let (mut out, t_right, mut scope) = builder.from_expr(right, scope);
                let mut st = StoredType::new(t_right.clone());
                if ident.is_mut() {
                    st = st.make_mut();
                }
                scope.values.define(ident.name, st.clone());
                out.push(CPSIR::Store(st));
                (out, t_right, scope)
            }
            Operator::Assign => {
                let ident = left.try_ident().unwrap();
                let st = scope.values.resolve(&ident.name).unwrap();
                (vec![CPSIR::Load(st.clone())], st.infer_type(), scope)
            }
            _ => {
                let (left, t_left, scope) = builder.from_expr(left, scope);
                let (right, t_right, scope) = builder.from_expr(right, scope);
                let f = Op {
                    kind: op,
                    args: vec![t_left.clone(), t_right],
                    ret: t_left.clone(),
                };
                (
                    vec![left, right, vec![CPSIR::Call(f)]]
                        .into_iter()
                        .flatten()
                        .collect(),
                    t_left,
                    scope,
                )
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct Extern {
    args: Vec<Type>,
    ret: Type,
}
impl Extern {
    pub fn into_value(self) -> TypedValue {
        TypedValue::new_extern(self)
    }

    pub fn arity(&self) -> usize {
        self.args.len()
    }
    pub fn infer_type(&self) -> Type {
        self.ret.clone()
    }

    pub fn interpret(interp: &CPSInterp) {}
}

pub type Label = u64;

#[derive(Clone, Debug)]
pub enum CPSIR {
    Value(TypedValue),
    Extern(String, Extern),
    Call(Op),
    Goto(Label),
    Label(Label),
    Store(StoredType),
    Load(StoredType),
    Exit,
}

impl CPSIR {
    pub fn infer_type(&self) -> Type {
        match self {
            //Self::Goto(_) | Self::Label(_) => Type::TPointer,
            Self::Value(tv) => tv.infer_type(),
            Self::Call(op) => op.infer_type(),
            Self::Extern(_, e) => e.infer_type(),
            Self::Load(st) => st.infer_type(),
            Self::Store(st) => st.infer_type(),
            _ => Type::Unknown,
        }
    }
}

pub struct Counter {
    id: u64,
}
impl Counter {
    pub fn new() -> Self {
        Self { id: 0 }
    }
    pub fn next(&mut self) -> u64 {
        let s = self.id;
        self.id += 1;
        s
    }
}

pub struct CPSBuilder {
    pub labels: Counter,
    pub symbols: Counter,
    pub scope: Environment,
    pub builtins: CallTable,
}

impl CPSBuilder {
    pub fn new() -> Self {
        Self {
            labels: Counter::new(),
            symbols: Counter::new(),
            scope: Environment::default(),
            builtins: crate::eval::builtins::builtins(CallTable::new()),
        }
    }
}

impl CPSBuilder {
    pub fn next_label(&mut self) -> Label {
        self.labels.next()
    }

    fn from_expr(&mut self, item: Expr, scope: Environment) -> (Vec<CPSIR>, Type, Environment) {
        match item {
            Expr::Ident(ident) => {
                println!("scope: {:?}", (&scope, &ident.name));
                let st = scope.values.resolve(&ident.name).unwrap();
                (vec![CPSIR::Load(st.clone())], st.infer_type(), scope)
            }
            Expr::Literal(Tok::IntLiteral(u)) => (
                vec![CPSIR::Value(TypedValue::Int(u.try_into().unwrap()))],
                Type::TInt,
                scope,
            ),
            Expr::Literal(Tok::FloatLiteral(u)) => (
                vec![CPSIR::Value(TypedValue::Float(u))],
                Type::TFloat,
                scope,
            ),
            Expr::Prefix(e_op, e_value) => Op::prefix(self, scope, e_op.value, e_value.value),
            Expr::Binary(e_op, e_left, e_right) => {
                Op::binary(self, scope, e_op, e_left.value, e_right.value)
            }
            Expr::Lambda(lambda) => {
                //let a = lambda.params.arity();
                let start = self.next_label();
                let end = self.next_label();
                let original_scope = scope.clone();
                let mut block_scope = scope.push();
                let param_idents = lambda
                    .params
                    .value
                    .iter()
                    .map(|param| match param.value.try_ident() {
                        Some(ident) => ident.name,
                        None => unreachable!()
                        //None => Err(param
                            //.context
                            //.runtime_error(&format!("Invalid Argument on Lambda, got {:?}", param))),
                    })
                    .collect::<Vec<_>>();
                for p in param_idents {
                    block_scope.values.define(p, StoredType::new(Type::Unknown));
                }

                let mut out = vec![CPSIR::Goto(end), CPSIR::Label(start)];
                let (mut vs, ty, scope) = self.from_expr(lambda.expr.value, block_scope);
                out.append(&mut vs);
                out.push(CPSIR::Label(end));
                let st = StoredType::new(Type::Unknown);
                (out, ty, original_scope)
            }

            Expr::Apply(func, args) => {
                let mut scope = scope.clone();
                let ident = func.try_ident().unwrap();
                let st = scope.values.resolve(&ident.name).unwrap();
                println!("st: {:?}", &st);
                let mut out = vec![];
                for arg in args {
                    //let (mut vs, ty, s) = self.from_expr(arg.value, scope);
                    //scope = s;
                    //out.append(&mut vs);
                }
                match st.loc {
                    StoreLocation::Extern => {
                        let ext = Extern {
                            args: vec![Type::TBool],
                            ret: Type::TVoid,
                        };
                        out.push(CPSIR::Extern(ident.name, ext));
                    }
                    _ => unimplemented!(),
                }

                (out, Type::Unknown, scope)
            }

            Expr::Block(exprs) => {
                let original_scope = scope.clone();
                let mut block_scope = scope.push();
                let start = self.next_label();
                let mut out = vec![CPSIR::Label(start)];
                let mut out_ty = None;
                for e in exprs {
                    let (mut vs, ty, scope) = self.from_expr(e.value, block_scope);
                    block_scope = scope;
                    out.append(&mut vs);
                    out_ty = Some(ty);
                }
                (out, out_ty.unwrap(), original_scope)
            }
            Expr::Program(exprs) => {
                let original_scope = scope.clone();
                let mut block_scope = scope.push();
                let start = self.next_label();
                let mut out = vec![CPSIR::Label(start)];
                let mut out_ty = None;
                for e in exprs {
                    let (mut vs, ty, scope) = self.from_expr(e.value, block_scope);
                    block_scope = scope;
                    out.append(&mut vs);
                    out_ty = Some(ty);
                }
                out.push(CPSIR::Exit);
                (out, out_ty.unwrap(), original_scope)
            }
            _ => {
                log::error!("Unimplemented: {:?}", item);
                unimplemented!()
            }
        }
    }
}

pub type Ident = String;

#[derive(Clone)]
pub struct Layer {
    values: HashTrieMap<Ident, StoredType>,
}

impl Default for Layer {
    fn default() -> Self {
        Self {
            values: HashTrieMap::new(),
        }
    }
}

impl fmt::Debug for Layer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(self.values.iter().map(|(k, v)| (k, v)))
            .finish()
    }
}

impl Layer {
    pub fn define(&self, name: Ident, value: StoredType) -> Layer {
        Layer {
            values: self.values.insert(name, value),
        }
    }

    pub fn contains(&self, name: &Ident) -> bool {
        self.values.contains_key(name)
    }

    pub fn get(&self, name: &Ident) -> Option<StoredType> {
        match self.values.get(name) {
            Some(expr) => Some(expr.clone()),
            None => None,
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Environment {
    types: EnvLayers<Ident, StoredType>,
    values: EnvLayers<Ident, StoredType>,
}

impl Environment {
    pub fn push(self) -> Self {
        let types = self.types.push();
        let values = self.values.push();
        Self { types, values }
    }

    pub fn pop(mut self) -> Self {
        let types = self.types.pop().unwrap();
        let values = self.values.pop().unwrap();
        Self { types, values }
    }
}

pub struct CPSInterp {}

impl CPSInterp {
    fn generate_blocks(program: &Vec<CPSIR>) -> HashMap<Label, usize> {
        let mut blocks = HashMap::new();

        // create label jump table
        for (inx, i) in program.iter().enumerate() {
            match i {
                CPSIR::Label(label) => {
                    blocks.insert(*label, inx);
                }
                _ => (),
            }
        }
        blocks
    }

    fn run(program: &Vec<CPSIR>) {
        let mut stack = vec![];
        let blocks = Self::generate_blocks(program);
        let mut ip = *blocks.get(&0).unwrap();
        let builtins = crate::eval::builtins::builtins(CallTable::new());

        loop {
            let i = program.get(ip).unwrap().clone();
            if let CPSIR::Exit = i {
                let v = stack.pop().unwrap();
                println!("stack: {:?}", stack);
                println!("result: {:?}", v);
                break;
            }
            ip = Self::eval_instruction(&ip, &i, &blocks, &mut stack, &builtins);
        }
    }

    fn eval_call(
        ip: &usize,
        i: &CPSIR,
        blocks: &HashMap<Label, usize>,
        stack: &mut Vec<TypedValue>,
        op: &Op,
    ) -> usize {
        let arity = op.arity();
        if arity == 2 {
            Self::eval_binary(ip, i, blocks, stack, op)
        } else if arity == 1 {
            Self::eval_unary(ip, i, blocks, stack, op)
        } else {
            unreachable!()
        }
    }

    fn eval_unary(
        ip: &usize,
        i: &CPSIR,
        blocks: &HashMap<Label, usize>,
        stack: &mut Vec<TypedValue>,
        op: &Op,
    ) -> usize {
        match op.kind {
            Operator::Plus => {
                let a = stack.pop().unwrap();
                let out = match a.infer_type() {
                    Type::TInt => {
                        let a = a.try_int().unwrap();
                        TypedValue::Int(a)
                    }
                    Type::TFloat => {
                        let a = a.try_float().unwrap();
                        TypedValue::Float(a)
                    }
                    _ => unimplemented!(),
                };
                stack.push(out);
            }
            Operator::Minus => {
                let a = stack.pop().unwrap();
                let out = match a.infer_type() {
                    Type::TInt => {
                        let a = a.try_int().unwrap();
                        TypedValue::Int(-a)
                    }
                    Type::TFloat => {
                        let a = a.try_float().unwrap();
                        TypedValue::Float(-a)
                    }
                    _ => unimplemented!(),
                };
                stack.push(out);
            }
            _ => unimplemented!(),
        }
        ip + 1
    }

    fn eval_binary(
        ip: &usize,
        i: &CPSIR,
        blocks: &HashMap<Label, usize>,
        stack: &mut Vec<TypedValue>,
        op: &Op,
    ) -> usize {
        match op.kind {
            Operator::Equal => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                assert_eq!(a.infer_type(), b.infer_type());
                let out = match a.infer_type() {
                    Type::TInt => {
                        let a = a.try_int().unwrap();
                        let b = b.try_int().unwrap();
                        TypedValue::Bool(a == b)
                    }
                    Type::TFloat => {
                        let a = a.try_float().unwrap();
                        let b = b.try_float().unwrap();
                        TypedValue::Bool(a == b)
                    }
                    _ => unimplemented!(),
                };
                stack.push(out);
            }
            Operator::Plus => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                assert_eq!(a.infer_type(), b.infer_type());
                let out = match a.infer_type() {
                    Type::TInt => {
                        let a = a.try_int().unwrap();
                        let b = b.try_int().unwrap();
                        TypedValue::Int(a + b)
                    }
                    Type::TFloat => {
                        let a = a.try_float().unwrap();
                        let b = b.try_float().unwrap();
                        TypedValue::Float(a + b)
                    }
                    _ => unimplemented!(),
                };
                stack.push(out);
            }
            Operator::Minus => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                assert_eq!(a.infer_type(), b.infer_type());
                let out = match a.infer_type() {
                    Type::TInt => {
                        let a = a.try_int().unwrap();
                        let b = b.try_int().unwrap();
                        TypedValue::Int(a - b)
                    }
                    Type::TFloat => {
                        let a = a.try_float().unwrap();
                        let b = b.try_float().unwrap();
                        TypedValue::Float(a - b)
                    }
                    _ => unimplemented!(),
                };
                stack.push(out);
            }
            Operator::Divide => {
                let a = stack.pop().unwrap();
                let b = stack.pop().unwrap();
                assert_eq!(a.infer_type(), b.infer_type());
                let out = match a.infer_type() {
                    Type::TInt => {
                        let a = a.try_int().unwrap();
                        let b = b.try_int().unwrap();
                        TypedValue::Int(a / b)
                    }
                    Type::TFloat => {
                        let a = a.try_float().unwrap();
                        let b = b.try_float().unwrap();
                        TypedValue::Float(a - b)
                    }
                    _ => unimplemented!(),
                };
                stack.push(out);
            }
            _ => unimplemented!(),
        }
        ip + 1
    }

    fn eval_extern(
        ip: &usize,
        blocks: &HashMap<Label, usize>,
        stack: &mut Vec<TypedValue>,
        name: &String,
        ext: &Extern,
    ) -> usize {
        match name.as_str() {
            "assert" => {
                let a = stack.pop().unwrap();
                let b = a.try_bool().unwrap();
                if !b {
                    panic!("Assert failed");
                }
            }
            _ => (),
        }
        ip + 1
    }

    fn eval_instruction(
        ip: &usize,
        i: &CPSIR,
        blocks: &HashMap<Label, usize>,
        stack: &mut Vec<TypedValue>,
        builtins: &CallTable,
    ) -> usize {
        match i {
            CPSIR::Label(_) => ip + 1,
            CPSIR::Value(u) => {
                stack.push(u.clone());
                ip + 1
            }
            CPSIR::Store(st) => ip + 1,
            CPSIR::Load(st) => ip + 1,
            CPSIR::Goto(label) => *blocks.get(label).unwrap(),
            CPSIR::Call(op) => Self::eval_call(ip, i, blocks, stack, op),
            CPSIR::Extern(name, spec) => Self::eval_extern(ip, blocks, stack, name, spec),
            //CPSIR::Exit => break,
            _ => unimplemented!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_str;
    use test_log::test;

    #[test]
    fn test() {
        // parse a single statement
        let r = vec![
            ("1+2", false),
            ("(1+2)/(1-3)", false),
            ("1.+2.", false),
            ("-1", false),
            ("let x = 1+2", false),
            ("let x = 1+2; x+1", false),
            //("let x = 1+2; { let x = 3; } x+1", false),
            //("let y = 5; let f = \\x -> x+y; f(2)", false),
            //("assert(1==1)", false),
            //("assert(1==2)", false),
        ];
        r.iter().for_each(|(v, _)| {
            let p = parse_str(v).unwrap();
            debug!("{:?}", v);
            debug!("{:?}", p);
            let mut b = CPSBuilder::new();
            let mut scope = Environment::default();
            //let start = b.next_label();
            let (vs, ty, scope) = b.from_expr(p.value, scope);
            println!("v: {:?}", v);
            for v in &vs {
                println!("\t{:?}", v);
            }
            CPSInterp::run(&vs);
        });
    }
}
