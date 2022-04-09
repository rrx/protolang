use crate::ast::{Expr, ExprNode, Operator};
use crate::tokens::Tok;
use log::debug;
use rpds::HashTrieMap;
use std::collections::HashMap;
use std::convert::From;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    TInt,
    TFloat,
    TPointer,
    Unknown,
    Eval,
}

#[derive(Clone, Debug)]
pub enum TypedValue {
    Int(i64),
    Float(f64),
    Pointer(String),
    Eval(String),
}
impl TypedValue {
    pub fn infer_type(&self) -> Type {
        match self {
            Self::Int(_) => Type::TInt,
            Self::Float(_) => Type::TFloat,
            Self::Pointer(_) => Type::TPointer,
            Self::Eval(_) => Type::Eval,
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

#[derive(Clone, Debug)]
pub enum StoredLocation {
    Stack,
    Heap,
}

#[derive(Clone, Debug)]
pub struct StoredType {
    pub ty: Type,
    loc: StoredLocation,
    pub is_mutable: bool,
}
impl StoredType {
    pub fn new(ty: Type) -> Self {
        Self {
            ty,
            loc: StoredLocation::Stack,
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
    loc: StoredLocation,
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

    pub fn prefix(builder: &mut CPSBuilder, op: Operator, value: Expr) -> (Vec<CPSIR>, Type) {
        match op {
            Operator::Plus | Operator::Minus => {
                let (value, t_value) = builder.from_expr(value);
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
                )
            }
            _ => unimplemented!(),
        }
    }

    pub fn binary(
        builder: &mut CPSBuilder,
        op: Operator,
        left: Expr,
        right: Expr,
    ) -> (Vec<CPSIR>, Type) {
        match op {
            Operator::Declare => {
                let ident = left.try_ident().unwrap();
                //let v = StoredValue { value: TypedValue
                let (mut out, t_right) = builder.from_expr(right);
                let mut st = StoredType::new(t_right.clone());
                if ident.is_mut() {
                    st = st.make_mut();
                }
                builder.define(ident.name, st.clone());
                out.push(CPSIR::Store(st));
                (out, t_right)
            }
            Operator::Assign => {
                let ident = left.try_ident().unwrap();
                let st = builder.resolve(ident.name);
                (vec![CPSIR::Load(st.clone())], st.infer_type())
            }
            _ => {
                let (left, t_left) = builder.from_expr(left);
                let (right, t_right) = builder.from_expr(right);
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
    pub fn arity(&self) -> usize {
        self.args.len()
    }
    pub fn infer_type(&self) -> Type {
        self.ret.clone()
    }
}

pub type Symbol = u64;
pub type Label = u64;

#[derive(Clone, Debug)]
pub enum CPSIR {
    Value(TypedValue),
    Extern(Pointer, Extern),
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
            Self::Goto(_) | Self::Label(_) => Type::TPointer,
            Self::Value(tv) => tv.infer_type(),
            Self::Call(op) => op.infer_type(),
            Self::Extern(_, e) => e.infer_type(),
            Self::Load(st) => st.infer_type(),
            Self::Store(st) => st.infer_type(),
            Self::Exit => Type::Unknown,
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
    pub env: Environment,
}

impl CPSBuilder {
    pub fn new() -> Self {
        Self {
            labels: Counter::new(),
            symbols: Counter::new(),
            env: Environment::default(),
        }
    }
}

impl CPSBuilder {
    pub fn next_label(&mut self) -> Label {
        self.labels.next()
    }

    fn define(&mut self, name: Ident, value: StoredType) {
        self.env = self.env.clone().define(name, value);
    }

    fn resolve(&self, name: Ident) -> StoredType {
        self.env.resolve(&name).unwrap()
    }

    fn from_expr(&mut self, item: Expr) -> (Vec<CPSIR>, Type) {
        match item {
            Expr::Ident(ident) => {
                let st = self.resolve(ident.name);
                (vec![CPSIR::Load(st.clone())], st.infer_type())
            }
            Expr::Literal(Tok::IntLiteral(u)) => {
                (vec![CPSIR::Value(TypedValue::Int(u.try_into().unwrap()))], Type::TInt)
            }
            Expr::Literal(Tok::FloatLiteral(u)) => {
                (vec![CPSIR::Value(TypedValue::Float(u))], Type::TFloat)
            }
            Expr::Prefix(e_op, e_value) => Op::prefix(self, e_op.value, e_value.value),
            Expr::Binary(e_op, e_left, e_right) => {
                Op::binary(self, e_op, e_left.value, e_right.value)
            }
            Expr::Program(exprs) => {
                let start = self.next_label();
                let mut out = vec![CPSIR::Label(start)];
                let mut out_ty = None;
                for e in exprs {
                    let (mut vs, ty) = self.from_expr(e.value);
                    out.append(&mut vs);
                    out_ty = Some(ty);
                }
                out.push(CPSIR::Exit);
                (out, out_ty.unwrap())
            }
            _ => {
                log::error!("Unimplemented: {:?}", item);
                unimplemented!()
            }
        }
    }
}

pub struct CPSScope {
    enclosing: Option<Box<CPSScope>>,
    env: Environment,
}
impl Default for CPSScope {
    fn default() -> Self {
        Self {
            enclosing: None,
            env: Environment::default(),
        }
    }
}
impl CPSScope {
    pub fn push(self) -> Self {
        Self {
            enclosing: Some(Box::new(self)),
            env: Environment::default(),
        }
    }

    pub fn pop(self) -> Option<Box<Self>> {
        self.enclosing
    }

    pub fn define(&mut self, name: Ident, value: StoredType) {
        self.env = self.env.clone().define(name, value);
    }
}

#[derive(Clone, Hash, PartialEq)]
pub struct SymbolAndId {
    name: String,
    id: Symbol,
}
impl SymbolAndId {
    pub fn new(name: String, id: Symbol) -> Self {
        Self { name, id }
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

#[derive(Debug, Clone)]
pub struct Environment {
    stack: im::vector::Vector<Layer>,
}

impl Default for Environment {
    fn default() -> Self {
        let mut stack = im::Vector::new();

        let layer = Layer {
            values: HashTrieMap::new(),
        };

        stack.push_front(layer);
        Self { stack }
    }
}

impl Environment {
    pub fn define(mut self, name: Ident, value: StoredType) -> Self {
        if self.stack.len() > 0 && self.stack.front().unwrap().contains(&name) {
            let layer = Layer::default().define(name, value);
            self.stack.push_front(layer);
            self
        } else {
            match self.stack.pop_front() {
                Some(layer) => {
                    let layer = layer.define(name, value);
                    self.stack.push_front(layer);
                    self
                }
                None => {
                    let layer = Layer::default().define(name, value);
                    self.stack.push_front(layer);
                    self
                }
            }
        }
    }

    pub fn resolve(&self, name: &Ident) -> Option<StoredType> {
        self.stack
            .iter()
            .find(|layer| layer.values.contains_key(name))
            .map(|layer| layer.get(name))
            .flatten()
    }

    pub fn debug(&self) {
        self.stack.iter().enumerate().for_each(|(i, layer)| {
            debug!("Layer: {:?}", i);
            layer.values.iter().for_each(|(k, v)| {
                debug!("\t{}: {:?}", k, v);
            });
        })
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
        loop {
            let i = program.get(ip).unwrap().clone();
            if let CPSIR::Exit = i {
                break;
            }
            ip = Self::eval_i(&ip, &i, &blocks, &mut stack);
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
            _ => unimplemented!()
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

    fn eval_i(
        ip: &usize,
        i: &CPSIR,
        blocks: &HashMap<Label, usize>,
        stack: &mut Vec<TypedValue>,
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
        ];
        r.iter().for_each(|(v, _)| {
            let p = parse_str(v).unwrap();
            debug!("{:?}", p);
            let mut b = CPSBuilder::new();
            //let start = b.next_label();
            let (vs, ty) = b.from_expr(p.value);
            println!("v: {:?}", v);
            for v in &vs {
                println!("\t{:?}", v);
            }
            CPSInterp::run(&vs);
        });
    }
}
