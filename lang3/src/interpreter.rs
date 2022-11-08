use crate::*;
use std::error::Error;

pub struct Interpreter {
    builder: AstBuilder,
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

    env.define("+".to_string(), d); //.into());

    env
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            builder: AstBuilder::default(),
        }
    }

    pub fn run(&mut self, ast: &Ast) -> Result<Ast, Box<dyn Error>> {
        let env = Environment::default();
        let env = base_env(env, &mut self.builder);
        let (ast, env, subst) = self.builder.resolve(ast, env.clone())?;

        //let (_res, ast, _env, subst) = self.builder.resolve(ast, env.clone());
        //let ast = ast.resolve(&subst);
        println!("Eval {}", &ast);
        let (ast, _) = eval(self, &ast, env)?;
        Ok(ast)
    }

    pub fn call(
        &mut self,
        builtin: &Builtin,
        args: Vec<Ast>,
        env: Environment,
    ) -> Result<(Ast, Environment), Box<dyn Error>> {
        match builtin {
            Builtin::AddInt(_, _) => {
                let lhs = args.get(0).unwrap().try_int().unwrap();
                let rhs = args.get(1).unwrap().try_int().unwrap();
                Ok((Ast::int(lhs as i64 + rhs as i64), env))
            }
            _ => unimplemented!(),
        }
    }
}

fn lookup_builtin_by_name(name: &str, env: Environment) -> Option<Builtin> {
    let v = env.resolve(&name.to_string());
    log::debug!("resolve {:?}", (&v));
    match v {
        Some(Ast::Declare(var, func)) => match **func {
            Ast::Function {
                ref params,
                ref body,
                ref ty,
            } => match **body {
                Ast::Internal(ref b) => {
                    return Some(b.clone());
                }
                _ => (),
            },
            _ => (),
        },
        _ => (),
    }
    None
}

fn eval(
    i: &mut Interpreter,
    ast: &Ast,
    mut env: Environment,
) -> Result<(Ast, Environment), Box<dyn Error>> {
    match ast {
        Ast::Apply(var, args) => {
            if let Some(builtin) = lookup_builtin_by_name(&var.name, env.clone()) {
                let mut eval_args = vec![];
                let mut env = env.clone();
                for arg in args {
                    let (arg, new_env) = eval(i, arg, env.clone())?;
                    env = new_env;
                    eval_args.push(arg);
                }
                return i.call(&builtin, eval_args, env);
            } else {
                unimplemented!("unable to resolve {:?}", &var.name)
            }
        }
        Ast::Declare(var, rhs) => {
            let (rhs, mut env) = eval(i, &rhs, env)?;
            env.define(var.name.clone(), rhs.clone());
            Ok((Ast::Declare(var.clone(), rhs.into()), env))
        }

        Ast::Block(exprs) => {
            let mut original_env = env.clone();
            let mut out = vec![];
            for e in exprs {
                let (e, new_env) = eval(i, e, env.clone())?;
                env = new_env;
                out.push(e);
            }
            Ok((Ast::Block(out), original_env))
        }
        Ast::Literal(_) | Ast::Function { .. } => Ok((ast.clone(), env)),
        _ => unimplemented!("eval: {:?}", ast),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::AstBuilder;
    use test_log::test;

    #[test]
    fn interpreter() {
        let mut b: AstBuilder = AstBuilder::default();
        let env = Environment::default();
        let env = base_env(env, &mut b);

        let one = b.int(1);
        let two = b.int(2);
        let ast = b.binary("+", one, two);

        let mut i = Interpreter::new();

        let ret = i.run(&ast).unwrap();
        println!("RET: {}", ret);
        assert_eq!(Ast::int(3), ret);
    }
}
