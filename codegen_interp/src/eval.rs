use codegen_ir::*;
use data::env::EnvLayers;

type DefinitionMap = std::collections::HashMap<DefinitionId, Ast>;
type DefinitionNames = std::collections::HashMap<String, DefinitionId>;
type Environment = EnvLayers<DefinitionId, Ast>;

use std::error::Error;
use std::fmt;

type EResult = Result<Ast, Box<dyn Error>>;

#[derive(Debug, Clone)]
enum InterpError {
    Error(String),
}
impl Error for InterpError {}
impl fmt::Display for InterpError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Default)]
struct Interp {
    names: DefinitionNames,
    env: Environment,
}
fn lookup_var<'a>(var: &Variable, defmap: &DefinitionMap) -> EResult {
    match defmap.get(&var.definition_id) {
        Some(value) => Ok(value.clone()),
        None => Err(InterpError::Error("Variable not found".into()).into()),
    }
}

impl Interp {
    fn scan(&mut self, ast: &Ast, defmap: &mut DefinitionMap) {
        for def in codegen_ir::scan::scan_definitions(ast) {
            //let _name = def.get_name();

            match &*def.expr {
                Ast::Lambda(_lambda) => {
                    defmap.insert(def.variable, ast.clone());
                }
                _ => unimplemented!(),
            }
        }
    }

    fn eval_variable(&mut self, var: &Variable, defmap: &mut DefinitionMap) -> EResult {
        let result = match self.env.resolve(&var.definition_id) {
            Some(r) => Ok(r.clone()),
            None => lookup_var(var, defmap),
        };
        //println!("find: {:?}", (&var, &result));
        result
    }

    fn eval_lambda(&mut self, lambda: &Lambda, _defmap: &mut DefinitionMap) -> EResult {
        Ok(lambda.clone().into())
    }

    fn eval_if(&mut self, v: &If, defmap: &mut DefinitionMap) -> EResult {
        let result = self.eval(&v.condition, defmap)?.try_bool().unwrap();
        if result {
            self.eval(&v.then, defmap)
        } else if let Some(otherwise) = &v.otherwise {
            self.eval(&otherwise, defmap)
        } else {
            Ok(hir::unit())
        }
    }

    fn eval_call(&mut self, call: &FunctionCall, defmap: &mut DefinitionMap) -> EResult {
        let FunctionCall {
            function,
            args,
            function_type: _,
        } = call;
        match **function {
            Ast::Variable(ref var) => {
                let value = lookup_var(&var, defmap)?;
                match value {
                    Ast::Lambda(lambda) => {
                        //println!("call({}, {:?}): {:?}", var.get_name(), args, lambda.args);
                        assert_eq!(lambda.args.len(), args.len());
                        let original_env = self.env.clone();
                        //self.env.push();

                        //println!("env({:?})", self.env);
                        let mut eval_args = vec![];
                        for (arg, lambda_arg) in args.iter().zip(lambda.args.iter()) {
                            let v = self.eval(arg, defmap)?.into();
                            //println!("push({},{}, {},{:?})", var.get_name(), arg, lambda_arg.definition_id, v);
                            //println!("push({},{:?})", var.get_name(), v);
                            eval_args.push((lambda_arg.definition_id, v));
                        }

                        println!("call({}, {:?})", var.get_name(), &eval_args);

                        for (definition_id, v) in eval_args {
                            self.env.define(definition_id, v);
                        }

                        let mut result = self.eval(&lambda.body, defmap)?;
                        match result {
                            Ast::Return(ret) => {
                                result = self.eval(&ret.expression, defmap)?;
                            }
                            _ => (),
                        }
                        self.env = original_env;
                        Ok(result)
                    }
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }

    fn eval_i64(&mut self, ast: &Ast, defmap: &mut DefinitionMap) -> Result<i64, Box<dyn Error>> {
        match self.eval(ast, defmap)?.try_i64() {
            Some(v) => Ok(v),
            None => Err(InterpError::Error("int cast failed".into()).into()),
        }
    }

    fn eval_builtin(&mut self, builtin: &Builtin, defmap: &mut DefinitionMap) -> EResult {
        match builtin {
            Builtin::AddInt(lhs, rhs) => {
                let lhs = self.eval_i64(lhs, defmap)?;
                let rhs = self.eval_i64(rhs, defmap)?;
                Ok(hir::i64(lhs as i64 + rhs as i64))
            }
            Builtin::SubInt(lhs, rhs) => {
                let lhs = self.eval_i64(lhs, defmap)?;
                let rhs = self.eval_i64(rhs, defmap)?;
                Ok(hir::i64(lhs as i64 - rhs as i64))
            }
            Builtin::EqInt(lhs, rhs) => {
                let lhs = self.eval_i64(lhs, defmap)?;
                let rhs = self.eval_i64(rhs, defmap)?;
                let result = hir::bool(lhs == rhs);
                //println!("eq: {}, {} => {}", lhs, rhs, result);
                Ok(result)
            }
            _ => unimplemented!(),
        }
    }

    fn eval_return(&mut self, ret: &Return, _defmap: &mut DefinitionMap) -> EResult {
        Ok(ret.clone().into())
        //self.eval(&ret.expression, defmap)
    }

    fn eval_definition(&mut self, def: &Definition, defmap: &mut DefinitionMap) -> EResult {
        let value = self.eval(&def.expr, defmap)?;
        defmap.insert(def.variable, value.clone());
        self.names.insert(def.get_name(), def.variable);
        let mut def = def.clone();
        def.expr = value.into();
        Ok(def.into())
    }

    fn eval(&mut self, ast: &Ast, defmap: &mut DefinitionMap) -> EResult {
        //println!("eval({:?})", ast);
        match ast {
            Ast::Variable(var) => self.eval_variable(var, defmap),
            Ast::Definition(def) => self.eval_definition(def, defmap),
            Ast::Lambda(lambda) => self.eval_lambda(lambda, defmap),
            Ast::Literal(literal) => Ok(literal.clone().into()),
            Ast::If(v) => self.eval_if(v, defmap),
            Ast::Builtin(builtin) => self.eval_builtin(builtin, defmap),
            Ast::Return(ret) => self.eval_return(ret, defmap),
            /*

            Ast::Extern(v) => codegen_extern(gen, v, defmap),
             */
            Ast::FunctionCall(ret) => self.eval_call(ret, defmap),
            Ast::Sequence(Sequence { statements }) => {
                assert!(!statements.is_empty());

                for statement in statements.iter().take(statements.len() - 1) {
                    let result = self.eval(statement, defmap)?;

                    // check if we got a return, and bail if so
                    match result {
                        Ast::Return(_) => {
                            return Ok(result);
                        }
                        _ => (),
                    }
                }

                self.eval(statements.last().unwrap(), defmap)
            }
            _ => unimplemented!("{:?}", ast),
        }
    }

    fn call_name(&mut self, name: &str, args: Vec<Ast>, defmap: &mut DefinitionMap) -> EResult {
        match self.names.get(name) {
            Some(def_id) => {
                let var = Variable {
                    definition_id: *def_id,
                    name: Some(name.to_string()),
                };
                let value = lookup_var(&var, defmap)?;
                match value {
                    Ast::Lambda(lambda) => {
                        let call = FunctionCall::new(var.into(), args, lambda.typ);
                        self.eval(&call.into(), defmap)
                    }
                    _ => unreachable!(),
                }
            }
            None => Err(InterpError::Error("name does not exist".into()).into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use codegen_ir::testing::*;

    #[test]
    fn interp_fib() {
        let mut defs = Definitions::new();
        let ast = gen_fib(&mut defs);
        println!("AST: {}", &ast.to_ron());

        let mut defmap = DefinitionMap::default();
        let mut i = Interp::default();
        let _ret = i.eval(&ast, &mut defmap).unwrap();
        let ret = i.call_name("main", vec![], &mut defmap).unwrap();
        println!("ret: {:?}", ret);
        assert_eq!(ret.try_i64(), Some(55));
    }
}
