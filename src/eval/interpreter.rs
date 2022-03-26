use super::*;
use super::{ExprRef, ExprRefWithEnv};
use crate::ast::*;
use crate::ast::{CallTable, Callback};
use crate::results::{InterpretError, InterpretErrorKind};
//use crate::sexpr::SExpr;
use crate::tokens::Tok;
//use log::debug;
//use rpds::HashTrieMap;
use std::fmt;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Clone)]
pub struct Interpreter<'a> {
    builtins: CallTable<'a>,
    //builtins: HashTrieMap<String, Callback<'a>>,
    p: std::marker::PhantomData<&'a String>,
}

impl<'a> Default for Interpreter<'a> {
    fn default() -> Self {
        Self {
            p: std::marker::PhantomData,
            builtins: crate::eval::builtins::builtins(CallTable::new()),
        }
    }
}

impl<'a> fmt::Debug for Interpreter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<interpreter>")
    }
}

impl ExprNode {
    fn check_bool(&self) -> Result<bool, InterpretError> {
        match &self.value {
            Expr::Literal(t) => match t {
                Tok::BoolLiteral(b) => Ok(*b),
                _ => Err(self.context.error(InterpretErrorKind::ExpectBool)),
            },
            _ => Err(InterpretError::runtime(&format!(
                "Expecting a literal: {:?}",
                self
            ))),
        }
    }

    fn check_number(&self) -> Result<f64, InterpretError> {
        match &self.value {
            Expr::Literal(t) => match t {
                Tok::IntLiteral(u) => Ok(*u as f64),
                Tok::FloatLiteral(f) => Ok(*f),
                _ => Err(self
                    .context
                    .runtime_error(&format!("Expecting a number: {:?}", self))),
            },
            Expr::Callable(_) => Err(self
                .context
                .runtime_error(&format!("Expecting a callable: {:?}", self))),
            _ => Err(self
                .context
                .runtime_error(&format!("Expecting a number: {:?}", self))),
        }
    }

    fn check_numbers(a: &Self, b: &Self) -> Result<(f64, f64), InterpretError> {
        let a = a.check_number()?;
        let b = b.check_number()?;
        Ok((a, b))
    }

    pub fn new_expr(&self, expr: Expr) -> Self {
        ExprNode::new_with_context(expr, self.context.clone())
    }

    pub fn plus(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(self.new_expr(Expr::new_float(left + right)))
    }

    pub fn minus(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(self.new_expr(Expr::new_float(left - right)))
    }

    pub fn exp(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(self.new_expr(Expr::new_float(left.powf(right))))
    }

    pub fn multiply(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(self.new_expr(Expr::new_float(left * right)))
    }

    pub fn divide(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(self.new_expr(Expr::new_float(left / right)))
    }

    pub fn lte(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(self.new_expr(Expr::new_bool(left <= right)))
    }

    pub fn lt(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(self.new_expr(Expr::new_bool(left < right)))
    }

    pub fn gte(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(self.new_expr(Expr::new_bool(left >= right)))
    }

    pub fn gt(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(self.new_expr(Expr::new_bool(left > right)))
    }

    /*
    pub fn eq_chain(args: Vec<Self>) -> Result<Self, InterpretError> {
        let mut numbers = vec![];
        for arg in args {
            numbers.push(arg.check_number()?);
        }

        let (left, right) = Self::check_numbers(self, other)?;
        Ok(Self::new_bool(left == right))
    }
    */

    pub fn eq(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(self.new_expr(Expr::new_bool(left == right)))
    }

    pub fn ne(&self, other: &Self) -> Result<Self, InterpretError> {
        let (left, right) = Self::check_numbers(self, other)?;
        Ok(self.new_expr(Expr::new_bool(left != right)))
    }

    pub fn postfix(&self, op: &Operator) -> Result<Self, InterpretError> {
        match op {
            Operator::Bang => {
                let _ = self.check_number()?;
                // TODO: Fib not yet implemented
                Err(self.context.error(InterpretErrorKind::NotImplemented))
            }
            _ => Err(self.context.error(InterpretErrorKind::NotImplemented)),
        }
    }

    pub fn prefix(&self, prefix: &Operator) -> Result<Self, InterpretError> {
        match prefix {
            Operator::Plus => {
                let right = self.check_number()?;
                Ok(self.new_expr(Expr::new_float(right)))
            }
            Operator::Minus => {
                let right = self.check_number()?;
                Ok(self.new_expr(Expr::new_float(right)))
            }
            _ => Err(self.context.error(InterpretErrorKind::NotImplemented)),
        }
    }
}

impl<'a> Interpreter<'a> {
    pub fn call(
        &mut self,
        mut env: Environment<'a>,
        f: &dyn Callable,
        params: &Params,
        args: &Vec<ExprRef>,
        expr: ExprRef,
    ) -> Result<ExprRefWithEnv<'a>, InterpretError> {
        if args.len() != f.arity() {
            return Err(params.context.runtime_error(&format!(
                "Mismatched params on function. Expecting {}, got {}",
                f.arity(),
                args.len()
            )));
        }

        let param_idents = params
            .value
            .iter()
            .map(|param| match param.value.try_ident() {
                Some(ident) => Ok(ident),
                None => Err(param
                    .context
                    .runtime_error(&format!("Invalid Argument on Lambda, got {:?}", param))),
            })
            .collect::<Vec<_>>();

        let (p, e): (
            Vec<Result<_, InterpretError>>,
            Vec<Result<_, InterpretError>>,
        ) = param_idents.into_iter().partition(|p| p.is_ok());

        if e.len() > 0 {
            return Err(params
                .context
                .runtime_error(&format!("Invalid Argument on Lambda, got {:?}", e)));
        }

        // push variables into scope
        for (p, v) in p.iter().zip(args) {
            env = env.define(p.as_ref().unwrap().clone(), v.clone().into());
        }

        // evaluate the result
        let r = self.evaluate(expr, env)?;
        // we drop any temporary variables, if they are no longer needed
        // we aren't able to drop the env currently, in case the function mutated something outside
        Ok(r)
    }

    pub fn evaluate(
        &mut self,
        exprref: ExprRef,
        env: Environment<'a>,
    ) -> Result<ExprRefWithEnv<'a>, InterpretError> {
        let e = exprref.clone();
        let node = exprref.borrow();
        let expr = &node.value;
        //debug!("EVAL: {:?}", &expr);
        match expr {
            Expr::Literal(_) => Ok(ExprRefWithEnv::new(e, env)),
            Expr::Ident(ident) => {
                let v = env.get_at(&ident.name, &node.context)?;
                Ok(ExprRefWithEnv::new(v.into(), env))
            }
            Expr::Prefix(prefix, expr) => {
                let r = self.evaluate(expr.clone().into(), env)?;
                let eval = r.expr.as_ref().borrow().prefix(&prefix.value)?.into();
                Ok(ExprRefWithEnv::new(eval, r.env))
            }
            Expr::Postfix(op, expr) => {
                let r = self.evaluate(expr.clone().into(), env)?;
                let eval = r.expr.as_ref().borrow().postfix(&op.value)?;
                Ok(ExprRefWithEnv::new(eval.into(), r.env))
            }

            Expr::BinaryChain(exprs) => {
                if exprs.len() < 2 {
                    unreachable!();
                }
                let mut eval_elements = vec![];
                let mut newenv = env;
                for e in exprs.clone() {
                    let eref = self.evaluate(e.into(), newenv)?;
                    newenv = eref.env;
                    let e = eref.expr.as_ref().borrow().deref().clone();
                    eval_elements.push(e);
                }
                Ok(ExprRefWithEnv::new(
                    eval_elements.pop().unwrap().into(),
                    newenv,
                ))
            }

            Expr::Binary(op, left, right) => self.evaluate_binary(op, left, right, env),

            Expr::List(elements) => {
                let mut eval_elements = vec![];
                let mut newenv = env;
                for e in elements.clone() {
                    let eref = self.evaluate(e.into(), newenv)?;
                    newenv = eref.env;
                    let e = eref.expr.as_ref().borrow().deref().clone();
                    eval_elements.push(e);
                }
                Ok(ExprRefWithEnv::new(
                    Expr::List(eval_elements).into(),
                    newenv,
                ))
            }

            Expr::Callable(e) => {
                //debug!("Callable({:?})", &e);
                Err(node
                    .context
                    .runtime_error(&format!("Unimplemented callable::{:?}", &e)))
            }

            Expr::Lambda(e) => {
                //debug!("Lambda({:?})", &e);
                Ok(ExprRefWithEnv::new(
                    Expr::Callable(Box::new(e.clone())).into(),
                    env,
                ))
            }

            Expr::Index(ident, _args) => {
                // TODO: not yet implemented
                Err(ident.context.error(InterpretErrorKind::NotImplemented))
            }

            Expr::Apply(expr, args) => {
                let f = match &expr.value {
                    Expr::Ident(ident) => {
                        let mut eval_args = vec![];
                        let mut newenv = env;
                        for arg in args {
                            let v = self.evaluate(arg.clone().into(), newenv)?;
                            //debug!(
                            //"arg context {:?}",
                            //(&arg.context, &v.expr.borrow().context)
                            //);
                            eval_args.push(v.expr);
                            newenv = v.env;
                        }
                        //debug!("Calling context {:?}", (&node.context));

                        match self.builtins.get(&ident.name) {
                            Some(cb) => {
                                let result = cb(newenv.clone(), eval_args)?;
                                //debug!("Call Result {:?}", &result);
                                Some(Ok(result))
                            }
                            _ => {
                                let x: ExprAccessRef =
                                    newenv.get_at(&ident.name, &node.context)?.clone();
                                let expr = x.expr.as_ref().borrow();
                                match expr.try_callable() {
                                    Some(c) => {
                                        //debug!("Calling context {:?}", (&x, &expr));
                                        //debug!("Calling {:?}({:?})", c, eval_args);

                                        // newenv.clone here creates a stack branch
                                        let result = c.call(self, newenv.clone(), eval_args)?;
                                        //debug!("Call Result {:?}", &result);
                                        Some(Ok(result))
                                    }
                                    _ => None,
                                }
                            }
                        }
                    }
                    _ => None,
                };

                if let Some(r) = f {
                    r
                } else {
                    Err(node
                        .context
                        .runtime_error(&format!("Not a function: {:?}", f)))
                }
            }

            Expr::Block(exprs) => {
                // default return value for a block is void
                let mut result = Expr::Void.into();
                let original_env = env.clone();
                let mut newenv = env;
                for expr in exprs {
                    let r = self.evaluate(expr.clone().into(), newenv);
                    match r {
                        Ok(v) => {
                            newenv = v.env;
                            result = v.expr;
                        }
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }

                //return the original env
                Ok(ExprRefWithEnv::new(result.into(), original_env))
            }

            Expr::Program(exprs) => {
                let mut result = Expr::Void.into();
                let mut newenv = env;
                for expr in exprs {
                    let r = self.evaluate(expr.clone().into(), newenv);
                    match r {
                        Ok(v) => {
                            newenv = v.env;
                            result = v.expr;
                        }
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
                Ok(ExprRefWithEnv::new(result.into(), newenv))
            }

            Expr::Ternary(op, x, y, z) => match op {
                Operator::Conditional => {
                    let v = self.evaluate(x.clone().into(), env)?;
                    let b = ExprNode::check_bool(&v.expr.as_ref().borrow())?;
                    if b {
                        self.evaluate(y.clone().into(), v.env)
                    } else {
                        self.evaluate(z.clone().into(), v.env)
                    }
                }
                _ => unimplemented!(),
            },

            Expr::Chain(_, _) => Err(node.context.runtime_error("Not implemented")),

            Expr::Invalid(s) => Err(node
                .context
                .runtime_error(&format!("Invalid expr: {:?}", s))),
            Expr::Void => Ok(ExprRefWithEnv::new(Expr::Void.into(), env)),
        }
    }

    fn evaluate_binary(
        &mut self,
        op: &Operator,
        left: &ExprNode,
        right: &ExprNode,
        env: Environment<'a>,
    ) -> Result<ExprRefWithEnv<'a>, InterpretError> {
        match op {
            Operator::Declare => {
                return if let Some(ident) = left.try_ident() {
                    let eval_right = self.evaluate(right.clone().into(), env)?;
                    //debug!("Declare {:?} to {}", &eval_right, &ident.name);
                    let env = eval_right.env.define(ident, eval_right.expr.clone());
                    Ok(ExprRefWithEnv::new(eval_right.expr, env))
                } else {
                    Err(left
                        .context
                        .runtime_error(&format!("Invalid Assignment, LHS must be identifier")))
                };
            }
            Operator::Assign => {
                return if let Some(ident) = left.try_ident() {
                    let access = env.get_at(&ident.name, &left.context)?;
                    if access.modifier != VarModifier::Mutable {
                        //left.debug();
                        //env.debug();
                        return Err(left.context.runtime_error(&format!(
                            "Invalid Assignment, '{}' Not mutable",
                            &ident.name
                        )));
                    }

                    let eval_right = self.evaluate(right.clone().into(), env)?;
                    //debug!("Assign {:?} to {}", &eval_right, &ident.name);
                    let expr = access
                        .expr
                        .mutate(Rc::try_unwrap(eval_right.expr.0).unwrap().into_inner());

                    Ok(ExprRefWithEnv::new(expr, eval_right.env))
                } else {
                    Err(left
                        .context
                        .runtime_error(&format!("Invalid Assignment, LHS must be identifier")))
                };
            }
            _ => (),
        }

        let v_left = self.evaluate(left.clone().into(), env)?;
        let v_right = self.evaluate(right.clone().into(), v_left.env)?;

        let eval_left = v_left.expr.as_ref().borrow();
        let eval_right = v_right.expr.as_ref().borrow();
        match op {
            Operator::Plus => eval_left.plus(&eval_right),
            Operator::Minus => eval_left.minus(&eval_right),
            Operator::Exp => eval_left.exp(&eval_right),
            Operator::Multiply => eval_left.multiply(&eval_right),
            Operator::Divide => eval_left.divide(&eval_right),
            Operator::GreaterThanEqual => eval_left.gte(&eval_right),
            Operator::LessThanEqual => eval_left.lte(&eval_right),
            Operator::GreaterThan => eval_left.gt(&eval_right),
            Operator::LessThan => eval_left.lt(&eval_right),
            Operator::Equal => eval_left.eq(&eval_right),
            Operator::NotEqual => eval_left.ne(&eval_right),
            _ => Err(eval_left
                .context
                .runtime_error(&format!("Unimplemented expression op: Operator::{:?}", op))),
        }
        .map(|v| {
            ExprRefWithEnv::new(
                ExprNode::new(v.into(), &left.context.to_location()).into(),
                v_right.env,
            )
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::program::Program;
    use log::debug;
    use test_log::test;

    #[test]
    fn test() {
        let env = Environment::default();
        let mut program = Program::new();
        let r = program
            .eval(
                "
        let x = 0;
        let mut x = 1;
        x = 2;
        ",
                env,
            )
            .unwrap();
        program.print();
        assert!(r.env.resolve("x").unwrap().is_mut());

        // blocks should hide visibility
        let r = program
            .eval(
                "
        # asdf should not be visible outside the block
        {
                let asdf1 = 1
        }
        ",
                r.env,
            )
            .unwrap();
        program.print();
        assert!(r.env.resolve("asdf1").is_none());

        let r = program
            .eval(
                "
        # verify that we can use non-local variables for calculationsa in a closure
        let mut nonlocal_x = 1
        let x = 2
        let f = \\x -> x + nonlocal_x
        assert(4 == f(3))
        ",
                r.env,
            )
            .unwrap();
        program.print();

        let r = program
            .eval(
                "
        # verify that we are able to modify non local variables from within the closure
        let f = \\x -> {
                nonlocal_x = 2
                x + 1
        }
        assert(2 == f(1))
        assert(nonlocal_x == 2)
        ",
                r.env,
            )
            .unwrap();
        program.print();

        let r = program
            .eval(
                "
        # check to make sure closures don't leak
        let f = \\x -> {
          # temporary variable created inside of the closure
          let super_local = 1
          nonlocal_x = 2
          x + 1
        }
        f(1)
        ",
                r.env,
            )
            .unwrap();
        program.print();
        assert!(r.env.resolve("super_local").is_none());
    }

    #[test]
    fn errors() {
        let env = Environment::default();
        let mut program = Program::new();
        let r = program.eval("assert(1)", env);
        if let Err(InterpretError { kind: _, context }) = r {
            assert!(context.has_location());
        } else {
            unreachable!();
        }

        let env = Environment::default();
        let r = program.eval("a", env);
        if let Err(InterpretError { kind: _, context }) = r {
            assert!(context.has_location());
        } else {
            unreachable!();
        }
    }

    #[test]
    fn builtin() {
        println!("x");
        let mut program = Program::new();
        let env = Environment::default();
        let r = program.eval("showstack()", env).unwrap();
        program.print();
        debug!("x {:?}", r.expr);
    }
}
