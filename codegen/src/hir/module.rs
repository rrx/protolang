use super::*;

pub struct ModuleBuilder {
    last_id: usize
}

impl ModuleBuilder {
    pub fn new() -> Self {
        Self { last_id: 0 }
    }

    fn next_id(&mut self) -> DefinitionId {
        let result = DefinitionId(self.last_id);
        self.last_id += 1;
        result
    }

    pub fn define(&mut self, name: &str, expr: Ast) -> Ast {
        Ast::Definition(Definition {
            variable: self.next_id(),
            name: Some(name.to_string()),
            expr: expr.into(),
        })
    }

    pub fn u64(&self, u: u64) -> Ast {
        Literal::Integer(u, IntegerKind::U64).into()
    }

    pub fn add(&self, a: Ast, b: Ast) -> Ast {
        Builtin::AddInt(a.into(), b.into()).into()
            /*
        match (&a, &b) {
            (Ast::Literal(Literal::Integer(_, _)), Ast::Literal(Literal::Integer(_, _))) => {
                Builtin::AddInt(a.into(), b.into()).into()
            }
            _ => {
                println!("{:?}", (&a, &b));
                unimplemented!()
            }
        }
        */
    }

    pub fn apply(&self, f: Ast, args: Vec<Ast>) -> Ast {
        match f {
            Ast::Lambda(lambda) => {
                let function_type = lambda.typ.clone();
                Ast::FunctionCall(FunctionCall {
                    function: Ast::Lambda(lambda).into(),
                    args,
                    function_type
                })
            }

            Ast::Definition(def) => {
                self.apply(*def.expr, args)
            }

            _ => {
                println!("{:?}", (&f));
                unreachable!()
            }
        }
    }

    pub fn main(&mut self, body: Ast) -> Ast {
        self.function("main", body)
    }

    pub fn seq(&self, exprs: Vec<Ast>) -> Ast {
        Ast::Sequence(Sequence { statements: exprs })
    }

    pub fn function(&mut self, name: &str, body: Ast) -> Ast {
        let return_type = Type::Primitive(PrimitiveType::Integer(IntegerKind::I64));

        let typ = FunctionType {
            parameters: vec![],
            return_type: Box::new(return_type),
            is_varargs: false,
            export: true,
        };

        let f = Ast::Lambda(Lambda {
            args: vec![],
            body: body.into(),
            typ,
        });

        self.define(name, f)
    }

}
