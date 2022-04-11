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
    TPointer,
    TFunction,
    TComposite,
    Eval,
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
/*
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionSpec {
    sig: Vec<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CompositeTypeSpec {
    sig: Rc<RefCell<HashMap<String, Rc<TypeSpec>>>>,
}
impl CompositeTypeSpec {
    pub fn new() -> Self {
        Self {
            sig: Rc::new(RefCell::new(HashMap::new())),
        }
    }
    pub fn add(self, name: String, ty: Rc<TypeSpec>) -> Self {
        self.sig.as_ref().borrow_mut().insert(name, ty);
        Self { sig: self.sig }
    }
}
impl Hash for CompositeTypeSpec {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.sig.as_ref().borrow().iter().for_each(|(_, v)| {
            v.hash(state);
        });
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeSpec(Rc<RefCell<TypeSpecInner>>);
impl TypeSpec {
    fn is_recursive(&self) -> bool {
        let subs = self.as_ref().borrow().subtypes();
        subs.contains(self) || subs.iter().any(|v| v.is_recursive())
    }

    fn new_composite() -> Self {
        TypeSpec(Rc::new(RefCell::new(TypeSpecInner::Composite(
            CompositeTypeSpec::new(),
        ))))
    }

    //fn get_composite_ref(&self) -> Rc<RefCell<HashMap<String, Rc<TypeSpec>>>> {
    //match self {
    //TypeSpecInner::Composite(c) => c.sig.clone(),
    //_ => unreachable!()
    //}
    //}

    fn new_function(c: FunctionSpec) -> Self {
        TypeSpec(Rc::new(RefCell::new(TypeSpecInner::Function(c))))
    }
    fn new_simple(c: Type) -> Self {
        TypeSpec(Rc::new(RefCell::new(TypeSpecInner::Simple(c.into()))))
    }

    //pub fn add(self, name: String, ty: Rc<TypeSpec>) -> Self {
    //(*self).get_mut().add_inner(name, ty);
    //let inner: &mut TypeSpecInner = self.as_ref().get_mut();

    //match inner {
    //TypeSpecInner::Composite(c) => {
    //c.add(name, ty);
    //}
    //_ => unreachable!()
    //}
    //(*self).borrow_mut().add(name, ty);
    //self //.clone()
    //Rc::get_mut(self).unwrap().borrow_mut().add(name, ty);
    //}
}

impl Hash for TypeSpec {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.as_ref().borrow().hash(state);
    }
}

impl std::ops::Deref for TypeSpec {
    type Target = Rc<RefCell<TypeSpecInner>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for TypeSpec {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TypeSpecInner {
    Function(FunctionSpec),
    Simple(Type),
    Composite(CompositeTypeSpec),
    Void,
}
impl TypeSpecInner {
    fn get_type(&self) -> Type {
        match self {
            Self::Simple(t) => t.clone(),
            Self::Composite(_) => Type::TComposite,
            Self::Function(_) => Type::TFunction,
            Self::Void => Type::TVoid,
        }
    }

    fn subtypes(&self) -> HashSet<Rc<TypeSpec>> {
        match self {
            Self::Composite(c) => {
                let mut acc = HashSet::new();
                for (_, v) in c.sig.as_ref().borrow().iter() {
                    acc.insert(v.clone());
                }
                acc
            }
            _ => HashSet::new(),
        }
    }

    //fn make_composite(mut self) -> Self {
    //}

    pub fn add_inner(self, name: String, ty: Rc<TypeSpec>) {
        match self {
            TypeSpecInner::Composite(c) => {
                c.add(name, ty);
            }
            _ => unreachable!(),
        }
    }

    fn new_pair(a: Rc<TypeSpec>, b: Rc<TypeSpec>) -> Self {
        let comp = CompositeTypeSpec::new()
            .add("0".into(), a)
            .add("1".into(), b);
        Self::Composite(comp)
    }

    pub fn size(&self) -> Option<usize> {
        use std::mem::size_of;
        match self {
            Self::Simple(t) => match t {
                Type::TInt => Some(size_of::<i64>()),
                _ => unreachable!(),
            },
            Self::Composite(c) => c
                .sig
                .as_ref()
                .borrow()
                .iter()
                .try_fold(0, |acc, (k, v)| v.as_ref().borrow().size().map(|x| x + acc)),
            Self::Function(_) => Some(size_of::<usize>()),
            Self::Void => Some(0),
        }
    }
}

//impl From<CompositeTypeSpec> for Rc<TypeSpec> {
//fn from(item: CompositeTypeSpec) -> Self {
//Rc::new(TypeSpec::new_composite(item))
//}
//}

//impl From<CompositeTypeSpec> for TypeSpec {
//fn from(item: CompositeTypeSpec) -> Self {
//TypeSpec::new_composite(item)
//}
//}

impl From<FunctionSpec> for TypeSpec {
    fn from(item: FunctionSpec) -> Self {
        TypeSpec::new_function(item)
    }
}

impl TryFrom<Type> for Rc<TypeSpec> {
    type Error = &'static str;

    fn try_from(item: Type) -> Result<Self, Self::Error> {
        let t: TypeSpec = TypeSpec::try_from(item)?;
        Ok(Rc::new(t))
    }
}

impl TryFrom<Type> for TypeSpec {
    type Error = &'static str;

    fn try_from(item: Type) -> Result<Self, Self::Error> {
        match item {
            Type::TBool | Type::TInt | Type::TFloat => Ok(TypeSpec::new_simple(item)),
            _ => Err("Not simple type"),
        }
    }
}
*/

#[derive(Clone, Debug)]
pub enum TypedValue {
    Bool(bool),
    Int(i64),
    Float(f64),
    Pointer(String),
    Function(Extern),
    Eval(String),
    Void,
}
impl TypedValue {
    pub fn new_extern(ext: Extern) -> Self {
        Self::Function(ext)
    }

    pub fn infer_type(&self) -> Type {
        match self {
            Self::Bool(_) => Type::TBool,
            Self::Int(_) => Type::TInt,
            Self::Float(_) => Type::TFloat,
            Self::Pointer(_) => Type::TPointer,
            Self::Eval(_) => Type::Eval,
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
    pub scope: Environment,
    pub builtins: CallTable,
}

impl CPSBuilder {
    pub fn new() -> Self {
        Self {
            labels: Counter::new(),
            symbols: Counter::new(),
            env: Environment::default(),
            scope: Environment::default(),
            builtins: crate::eval::builtins::builtins(CallTable::new()),
        }
    }
}

impl CPSBuilder {
    pub fn next_label(&mut self) -> Label {
        self.labels.next()
    }

    /*
    fn define(&mut self, name: Ident, value: StoredType) {
        self.env = self.env.clone().define(name, value);
    }

    fn resolve(&self, name: Ident) -> StoredType {
        match self.env.resolve(&name) {
            Some(st) => st,
            None => {
                let b = self.builtins.get(name).unwrap();
                let st = StoredType {};
                st
            }
        }
    }
    */

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

/*
#[derive(Clone, Debug)]
pub struct CPSScope {
    enclosing: Option<Rc<CPSScope>>,
    env: Environment,
}
impl Default for CPSScope {
    fn default() -> Self {
        let st = StoredType::new_extern();
        Self {
            enclosing: None,
            env: Environment::default(),
        }
        .define("assert".into(), st)
    }
}
impl CPSScope {
    pub fn push(self) -> Self {
        Self {
            enclosing: Some(Rc::new(self)),
            env: Environment::default(),
        }
    }

    pub fn pop(self) -> Option<Rc<Self>> {
        self.enclosing
    }

    pub fn define(self, name: Ident, value: StoredType) -> Self {
        let env = self.env.types.define(name, value);
        Self {
            enclosing: self.enclosing,
            env,
        }
    }

    pub fn resolve(&self, name: &Ident) -> Option<StoredType> {
        match self.env.resolve(name) {
            Some(st) => Some(st),
            None => match &self.enclosing {
                Some(scope) => scope.resolve(name),
                None => None,
            },
        }
    }
}
i*/

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

/*
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
*/

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
