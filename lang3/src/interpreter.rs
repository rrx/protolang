use crate::*;
use std::error::Error;

struct Interpreter {
}

fn gen_add(builder: &mut AstBuilder) -> Ast {
    // define add
    let a = builder.var_named("a", Type::Int);
    let b = builder.var_named("b", Type::Int);
    let params = vec![a.clone(), b.clone()];
    let sig = vec![Type::Int, Type::Int, Type::Int];
    let body = Ast::Internal(Builtin::AddInt(Box::new(a.into()), Box::new(b.into())));
    builder.func(params, body, sig)
}

pub fn base_env(mut env: Environment, builder: &mut AstBuilder) -> Environment {
    let rhs = gen_add(builder);

    // declare
    let ty = rhs.get_type();
    let v = builder.var_named("+", ty.clone());
    let d = Ast::Declare(v.clone(), rhs.into());

    env.define("+".to_string(), d);//.into());

    env
}


impl Interpreter {
    pub fn new() -> Self {
        Self { }
    }

    pub fn run(&mut self, ast: &Ast, env: Environment) -> Result<Ast, Box<dyn Error>> {
        eval(self, ast, env)
    }

    pub fn call(&mut self, builtin: &Builtin, args: Vec<Ast>, env: Environment) -> Result<Ast, Box<dyn Error>> {
        match builtin {
            Builtin::AddInt(_, _) => {
               let lhs = args.get(0).unwrap().try_int().unwrap();
               let rhs = args.get(1).unwrap().try_int().unwrap();
               Ok(Ast::int(lhs as i64 + rhs as i64))
            }
            _ => unimplemented!()
        }
    }
}

fn lookup_builtin_by_name(name: &str, env: Environment) -> Option<Builtin> {
    let v = env.resolve(&name.to_string());
    log::debug!("resolve {:?}", (&v));
    match v {
        Some(Ast::Declare(var, func)) => {
            match **func {
                Ast::Function { ref params, ref body, ref ty } => {
                    match **body {
                        Ast::Internal(ref b) => {
                            return Some(b.clone());
                        }
                        _ => ()
                    }
                }
                _ => ()
            }
        }
        _ => ()
    }
    None
}

fn eval(i: &mut Interpreter, ast: &Ast, env: Environment) -> Result<Ast, Box<dyn Error>> {
    match ast {
        Ast::Apply(var, args) => {
            if let Some(builtin) = lookup_builtin_by_name(&var.name, env.clone()) {
                let mut eval_args = vec![];
                for arg in args {
                    eval_args.push(eval(i, arg, env.clone())?);
                }
                return i.call(&builtin, eval_args, env.clone());
            } else {
                unimplemented!("unable to resolve {:?}", &var.name)
            }
        }
        Ast::Literal(_) => Ok(ast.clone()),
        _ => unimplemented!()

    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;
    use crate::AstBuilder;

    #[test]
    fn interpreter() {
        let mut b: AstBuilder = AstBuilder::default();
        let env = Environment::default();
        let env = base_env(env, &mut b);

        let one = b.int(1);
        let two = b.int(2);
        let ast = b.binary("+", one, two);

        let mut i = Interpreter::new();

        let (_res, ast, _env, subst) = b.resolve(ast, env.clone());
        let ast = ast.resolve(&subst);
        let ret = i.run(&ast, env).unwrap();
        println!("RET: {}", ret);
        assert_eq!(Ast::int(3), ret);
    }
}

