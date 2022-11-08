use crate::hir::*;
use crate::*;
use data::env::*;
use std::error::Error;
//use std::collections::HashMap;
use rpds::HashTrieMap;

impl LayerKey for DefinitionId {}
impl LayerValue for Ast {}

type DefinitionMap = HashTrieMap<DefinitionId, Ast>;
type Environment = EnvLayers<DefinitionId, Ast>;

pub struct Interpreter {
    defmap: DefinitionMap,
    b: Definitions,
}

/*
fn gen_add(builder: &mut Definitions) -> Ast {
    // define add
    let a = builder.new_variable();
    let b = builder.new_variable();
    let params = vec![a.clone(), b.clone()];
    let sig = vec![Type::i64(), Type::i64(), Type::i64()];
    let body = hir::add(a.clone().into(), b.clone().into());
    let typ = FunctionType::export(vec![Type::i64(), Type::i64()]);
    hir::new_lambda(vec![a.clone(), b.clone()], body.into(), typ.clone())
}
*/

pub fn base_env(mut env: Environment, builder: &mut Definitions) -> Environment {
    env
}

fn resolve(env: Environment, ast: &Ast) -> Result<(Ast, Environment), Box<dyn Error>> {
    match ast {
        Ast::Variable(var) => {
            match env.resolve(&var.definition_id) {
                //name.as_ref().unwrap().clone()) {
                Some(def) => Ok((def.clone(), env)),
                None => {
                    unreachable!("unable to find definition: {}", var.definition_id)
                },
            }
        },
        _ => Ok((ast.clone(), env)),
    }
}

impl Interpreter {
    pub fn new() -> Self {
        Self { defmap: DefinitionMap::default(), b: Definitions::new() }
    }

    pub fn run_main(&mut self, ast: &Ast) -> Result<(Ast, Environment, DefinitionMap), Box<dyn Error>> {
        let defmap = DefinitionMap::default();
        let env = Environment::default();
        let env = base_env(env, &mut self.b);
        //let (ast, env) = resolve(env.clone(), ast)?;

        println!("Eval {}", &ast);
        let (ast, env, defmap) = eval(&ast, env.clone(), defmap.clone())?;
        println!("Env {}", &env);
        println!("MAP: {}", &defmap);
        //let (ast, env, defmap) = call_function("main", vec![hir::i64(1)], env.clone(), defmap.clone())?;
        Ok((ast, env, defmap))
    }

    pub fn run(&mut self, ast: &Ast) -> Result<(Ast, Environment, DefinitionMap), Box<dyn Error>> {
        let defmap = DefinitionMap::default();
        let env = Environment::default();
        let env = base_env(env, &mut self.b);
        let (ast, env) = resolve(env.clone(), ast)?;

        println!("Eval {}", &ast);
        let (ast, env, defmap) = eval(&ast, env, defmap)?;
        println!("Env {}", &env);
        println!("MAP: {}", &defmap);
        Ok((ast, env, defmap))
    }

    pub fn call(
        &mut self, builtin: &Builtin, args: Vec<Ast>, env: Environment,
    ) -> Result<(Ast, Environment), Box<dyn Error>> {
        match builtin {
            Builtin::AddInt(_, _) => {
                let lhs = args.get(0).unwrap().try_i64().unwrap();
                let rhs = args.get(1).unwrap().try_i64().unwrap();
                Ok((hir::i64(lhs as i64 + rhs as i64), env))
            },
            _ => unimplemented!(),
        }
    }
}

fn eval_def(
    def: &Definition, env: Environment, defmap: DefinitionMap,
) -> Result<(Ast, Environment, DefinitionMap), Box<dyn Error>> {
    match defmap.clone().get(&def.variable) {
        Some(v) => eval(v, env, defmap),
        None => {
            match env.clone().resolve(&def.variable) {
                //name.as_ref().unwrap()) {
                Some(v) => eval(v, env, defmap),
                None => unimplemented!("unresolved"),
            }
        },
    }
}

/*
fn call_function_env(def_id: &DefinitionId, args: Vec<Ast>, env: Environment, defmap: DefinitionMap) -> Result<(Ast, Environment, DefinitionMap), Box<dyn Error>> {
    match env.clone().resolve(def_id) {
        Some(v) => {
            println!("Resolved {:?}", &v);
            //let (ast, _env, defmap) = eval(v, env.clone(), defmap)?;
            match v {
                Ast::Definition(ref def) => {
                    //println!("Eval {:?}", &def);
                    let (ast, env, defmap) = call_function(&def.variable, vec![hir::i64(1)], env.clone(), defmap.clone())?;
                    Ok((ast, env, defmap))
                }
                _ => unreachable!()
            }
        }
        _ => unreachable!("Unable to resolve: {}", def_id)
    }
}


fn call_function_defmap(def_id: &DefinitionId, args: Vec<Ast>, env: Environment, defmap: DefinitionMap) -> Result<(Ast, Environment, DefinitionMap), Box<dyn Error>> {
    match defmap.clone().get(def_id) {
        Some(v) => {
            println!("call resolved: {:?}", (&v));
            match v {
                Ast::Definition(ref def) => {
                    //eval(&*def.expr, env, defmap)
                    match &*def.expr {
                        Ast::Lambda(lambda) => call_lambda(lambda.clone(), args, env, defmap),
                        _ => unimplemented!("unresolved:{:?}", &def.expr)
                    }

                }
                _ => unimplemented!("unresolved")
            }
        }
        _ => unimplemented!("unresolved")
    }
}
*/

fn call_function(
    def_id: &DefinitionId, args: Vec<Ast>, env: Environment, defmap: DefinitionMap,
) -> Result<(Ast, Environment, DefinitionMap), Box<dyn Error>> {
    let v: Variable = Variable { definition_id: def_id.clone(), name: None };
    match resolve_var(&v, env.clone(), defmap.clone()) {
        Some(v) => {
            match v {
                Ast::Definition(def) => {
                    //eval(&*def.expr, env, defmap)
                    match &*def.expr {
                        Ast::Lambda(lambda) => call_lambda(lambda.clone(), args, env.clone(), defmap.clone()),
                        _ => unimplemented!("unresolved:{:?}", &def.expr),
                    }
                },
                _ => unimplemented!("unresolved"),
            }
        },
        _ => unimplemented!("unresolved"),
    }
}

fn call_lambda(
    lambda: Lambda, args: Vec<Ast>, env: Environment, defmap: DefinitionMap,
) -> Result<(Ast, Environment, DefinitionMap), Box<dyn Error>> {
    println!("call lambda: {:?}", (&lambda, &args));
    let mut local_env = env.clone();
    let args = args
        .iter()
        .zip(lambda.args.iter())
        .map(|(call_arg, f_arg)| {
            hir::definition_from_variable(f_arg, call_arg.clone())
            //Definition::named(f_arg.definition_id, &f_arg.name.as_ref().unwrap(), call_arg.clone())
        })
        .collect::<Vec<_>>();
    println!("lambda args: {:?}", (&args));
    for arg in args {
        local_env.define(arg.variable, *arg.expr);
    }
    eval(&lambda.body, local_env, defmap)
}

fn eval_func_call(
    call: &FunctionCall, env: Environment, defmap: DefinitionMap,
) -> Result<(Ast, Environment, DefinitionMap), Box<dyn Error>> {
    match call {
        FunctionCall { function, args, function_type } => {
            match **function {
                Ast::Variable(Variable { ref definition_id, ref name }) => {
                    call_function(definition_id, args.clone(), env, defmap)
                    /*
                    match defmap.clone().get(definition_id) {
                        Some(v) => {
                            println!("call resolved: {:?}", (&v));
                            match v {
                                Ast::Definition(ref def) => {
                                    // this is a lambda

                                    call_lambda(&*def.expr.clone(), args, env, defmap)
                                    //eval(&def.expr.clone(), env, defmap)
                                }
                                _ => unimplemented!("unresolved")
                            }
                        }
                        _ => unimplemented!("unresolved:{}, {:?}", definition_id, defmap)
                    }
                    */
                },
                _ => unimplemented!("unresolved:{:?}", function),
            }
        },
    }
}

fn eval_builtin(
    builtin: &Builtin, env: Environment, defmap: DefinitionMap,
) -> Result<(Ast, Environment, DefinitionMap), Box<dyn Error>> {
    match builtin {
        Builtin::AddInt(lhs, rhs) => {
            println!("add: {:?}", (&lhs, &rhs));
            let (lhs, env, defmap) = eval(lhs, env, defmap)?;
            let (rhs, env, defmap) = eval(rhs, env, defmap)?;
            println!("add: {:?}", (&lhs, &rhs));
            let lhs = lhs.try_i64().unwrap();
            let rhs = rhs.try_i64().unwrap();
            Ok((hir::i64(lhs as i64 + rhs as i64), env, defmap))
        },
        _ => unimplemented!(),
    }
}

fn resolve_var(var: &Variable, env: Environment, defmap: DefinitionMap) -> Option<Ast> {
    if let Some(name) = &var.name {
        match env.resolve(&var.definition_id) {
            Some(v) => {
                match v {
                    Ast::Definition(def) => {
                        return Some(*def.expr.clone());
                    },
                    //_ => unreachable!("{:?}", &v)
                    _ => {
                        return Some(v.clone());
                    },
                }
            },
            None => (),
        }
    }

    match defmap.clone().get(&var.definition_id) {
        Some(v) => Some(v.clone()),
        None => None,
    }
}

fn resolve_definition_id_expr(definition_id: &DefinitionId, env: Environment) -> Option<Ast> {
    resolve_definition_id(definition_id, env).map(|d| *d.expr.clone())
}

fn resolve_definition_id(definition_id: &DefinitionId, env: Environment) -> Option<Definition> {
    match env.resolve(definition_id) {
        Some(v) => match v {
            Ast::Definition(def) => Some(def.clone()),
            _ => None,
        },
        None => None,
    }
}

fn eval(
    ast: &Ast, mut env: Environment, mut defmap: DefinitionMap,
) -> Result<(Ast, Environment, DefinitionMap), Box<dyn Error>> {
    println!("eval: {:?}", ast);
    match ast {
        Ast::Builtin(builtin) => eval_builtin(builtin, env, defmap),

        //Ast::FunctionCall(FunctionCall { function, args: call_args, function_type }) => {
        Ast::FunctionCall(call) => {
            println!("call: {:?}", (&call));
            eval_func_call(call, env, defmap)
            /*
            match **function {
                Ast::Variable(Variable { ref definition_id, ref name }) => {
                    match defmap.clone().get(definition_id) {
                        Some(v) => {
                            println!("call resolved: {:?}", (&v));
                            match v {
                                Ast::Definition(ref def) => {
                                    eval(&def.expr.clone(), env, defmap)
                                }
                                _ => unimplemented!("unresolved")
                            }
                        }
                        None => {
                            match env.clone().resolve(&name.as_ref().unwrap()) {
                                Some(v) => {
                                    eval(v, env, defmap)
                                }
                                None => unimplemented!("unresolved")
                            }
                        }
                    }
                }
                Ast::Lambda(Lambda { args: ref lambda_args, ref body, ref typ }) => {
                    println!("call lambda: {:?}", (&lambda_args));
                    let mut local_env = env.clone();
                    let args = call_args.iter().zip(lambda_args.iter()).map(|(call_arg, f_arg)| {
                        Definition::named(f_arg.definition_id, &f_arg.name.as_ref().unwrap(), call_arg.clone())
                    });
                    for arg in args {
                        local_env.define(arg.name.unwrap(), *arg.expr);
                    }
                    let (ast, env, defmap) = eval(body, local_env, defmap)?;

                    Ok((ast, env, defmap))
                }
                _ => unimplemented!("{:?}", function)
            }
            */
        },

        Ast::Definition(Definition { variable, name, expr }) => {
            let (rhs, mut env, defmap) = eval(expr, env, defmap)?;
            let def = Ast::Definition(Definition { variable: *variable, name: name.clone(), expr: rhs.into() });

            // only add to env if name is defined
            if let Some(name) = name {
                env.define(*variable, def.clone());
            }

            // add the definition to the map
            Ok((def.clone(), env, defmap.insert(*variable, def)))
        },

        Ast::Sequence(Sequence { statements }) => {
            //let mut original_env = env.clone();
            let mut out = vec![];
            for e in statements {
                let (e, new_env, new_defmap) = eval(e, env.clone(), defmap.clone())?;
                env = new_env;
                defmap = new_defmap;
                out.push(e);
            }
            Ok((Sequence::new(out).into(), env, defmap))
        },
        Ast::Literal(_) | Ast::Lambda { .. } => Ok((ast.clone(), env, defmap)),

        Ast::Variable(var) => match resolve_var(var, env.clone(), defmap.clone()) {
            Some(ast) => Ok((ast.clone(), env, defmap)),
            None => unreachable!(),
        },
        _ => unimplemented!("eval: {:?}", ast),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::*;

    #[test]
    fn interp_add() {
        let mut b: Definitions = Definitions::new();
        let env = Environment::default();
        let env = base_env(env, &mut b);

        let one = hir::i64(1);
        let two = hir::i64(2);
        let ast = hir::add(one, two);

        let mut i = Interpreter::new();

        let (ret, env, defmap) = i.run(&ast).unwrap();
        println!("RET: {}", ret.to_ron().unwrap());
        assert_eq!(Some(3), ret.try_i64());
    }

    #[test]
    fn interp_main() {
        let mut b: Definitions = Definitions::new();
        let env = Environment::default();
        let env = base_env(env, &mut b);

        //let ast = hir::append(gen_x1_module(&mut b), gen_x1_main(&mut b));
        let ast = gen_main_simple(&mut b);

        let mut i = Interpreter::new();
        let (ret, env, defmap) = i.run_main(&ast).unwrap();
        println!("RET: {}", ret.to_ron().unwrap());
        //assert_eq!(Some(10), ret.try_i64());
    }

    #[test]
    fn interp_declare() {
        let mut b: Definitions = Definitions::new();
        let env = Environment::default();
        let env = base_env(env, &mut b);

        let v1 = b.new_variable();
        let v2 = b.named_variable("v2");
        let one = hir::i64(1);
        let two = hir::i64(2);
        let d1 = hir::definition_from_variable(&v1, one);
        let d2 = hir::definition_from_variable(&v2, two);
        let s = hir::append(d1.into(), d2.into());
        let mut i = Interpreter::new();

        let (ret, env, defmap) = i.run(&s).unwrap();
        println!("RET: {}", ret.to_ron().unwrap());

        let body = resolve_definition_id_expr(&v2.definition_id, env.clone()).unwrap();
        println!("{:?}", body);
        assert_eq!(Some(2), body.try_i64());

        //println!("{:?}", env.resolve(&"v2".to_string()).unwrap());
        //assert_eq!(Some(2), env.resolve(&"v2".to_string()).unwrap().try_i64());

        //assert_eq!(Some(3), ret.try_i64());
    }

    #[test]
    fn interp_assign() {
        let mut b: Definitions = Definitions::new();
        let env = Environment::default();
        let env = base_env(env, &mut b);

        let v: Ast = b.new_variable().into();
        let one = hir::i64(1);
        let ast = Ast::Assignment(Assignment { lhs: v.into(), rhs: one.into() });
        let mut i = Interpreter::new();

        //let (ret, env, defmap) = i.run(&ast).unwrap();
        //println!("RET: {}", ret.to_ron().unwrap());
        //assert_eq!(Some(3), ret.try_i64());
    }

    #[test]
    fn interp_fib() {
        let mut defs = Definitions::new();
        let ast = gen_fib(&mut defs);
        let mut i = Interpreter::new();
        let (ret, env, defmap) = i.run(&ast).unwrap();
        println!("RET: {:?}", ret);
        //assert_eq!(55, ret.try_i64().unwrap());
    }
}
