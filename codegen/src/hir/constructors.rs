use super::*;

pub struct Definitions {
    next_id: usize,
}
impl Definitions {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }
    pub fn new_definition(&mut self, name: &str, ast: Ast) -> Definition {
        let d = Definition { variable: DefinitionId(self.next_id), name: Some(name.to_string()), expr: ast.into() };
        self.next_id += 1;
        d
    }

    pub fn named_variable(&mut self, name: &str) -> Variable {
        let v = Variable { definition: None, definition_id: DefinitionId(self.next_id), name: Some(name.to_string()) };
        self.next_id += 1;
        v
    }

    pub fn new_variable(&mut self) -> Variable {
        let v = Variable { definition: None, definition_id: DefinitionId(self.next_id), name: None };
        self.next_id += 1;
        v
    }
}

pub fn i64(u: i64) -> Ast {
    unsafe { Ast::Literal(Literal::Integer(std::mem::transmute(u), IntegerKind::I64)) }
}
pub fn u64(u: u64) -> Ast {
    Ast::Literal(Literal::Integer(u, IntegerKind::U64))
}
pub fn f64(f: f64) -> Ast {
    unsafe {
        let u = std::mem::transmute(f);
        Ast::Literal(Literal::Float(u, FloatKind::F64))
    }
}
pub fn bool(u: bool) -> Ast {
    Ast::Literal(Literal::Bool(u))
}
pub fn string(s: String) -> Ast {
    Ast::Literal(Literal::CString(s))
}

pub fn new_return(ast: Ast) -> Ast {
    Return::new(ast).into()
}

pub fn new_lambda(args: Vec<Variable>, body: Ast, typ: FunctionType) -> Ast {
    Lambda::new(args, body, typ).into()
}

pub fn new_condition(condition: Ast, then: Ast, otherwise: Option<Ast>, result_type: Type) -> Ast {
    If {
        condition: condition.into(),
        then: then.into(),
        otherwise: otherwise.map(|v| v.into()),
        result_type
    }.into()
}

pub fn new_call(f: Ast, args: Vec<Ast>, typ: FunctionType) -> Ast {
    FunctionCall::new(f, args, typ).into()
}

pub fn add(a: Ast, b: Ast) -> Ast {
    Builtin::AddInt(a.into(), b.into()).into()
}

pub fn sub(a: Ast, b: Ast) -> Ast {
    Builtin::SubInt(a.into(), b.into()).into()
}

pub fn lt(a: Ast, b: Ast) -> Ast {
    Builtin::LessSigned(a.into(), b.into()).into()
}

pub fn eq(a: Ast, b: Ast) -> Ast {
    Builtin::EqInt(a.into(), b.into()).into()
}


