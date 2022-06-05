use crate::ast::*;
use crate::eval::*;
use crate::results::*;
use crate::tokens::Tok;
//use std::rc::Rc;
use crate::parser::Unparse;

pub struct Analysis {
    pub results: Vec<LangError>,
}

impl Analysis {
    pub fn new() -> Self {
        Self { results: vec![] }
    }

    pub fn analyze(&mut self, exprref: ExprRef, env: Environment) -> Environment {
        //let e = exprref.clone();
        let node = exprref.borrow();
        let expr = &node.value;

        let mut push = |v| {
            self.results.push(node.context.error(v));
        };

        //let mut push_error = |v| {
        //self.results.push(node.context.lang_error(LangErrorKind::Error(v)));
        //};

        //let mut push_warning = |v| {
        //self.results.push(node.context.lang_error(LangErrorKind::Warning(v)));
        //};

        match expr {
            Expr::Void => env,
            Expr::Literal(_) | Expr::Callback(_) => env.clone(),
            Expr::Ident(ident) => {
                if let Some(_) = env.resolve_value(&ident.name) {
                } else {
                    env.debug();
                    self.results
                        .push(node.context.error(LangErrorKind::Error(format!(
                            "Not found: {:?}",
                            node.unlex()
                        ))));
                }
                env.clone()
            }
            Expr::Prefix(prefix, expr) => {
                self.prefix_expr(&expr, &prefix.value);
                self.analyze(expr.clone().into(), env)
            }
            Expr::Postfix(op, expr) => {
                self.postfix_expr(&expr, &op.value);
                self.analyze(expr.clone().into(), env)
            }

            Expr::BinaryChain(exprs) => {
                if exprs.len() < 2 {
                    self.results
                        .push(node.context.error(LangErrorKind::Invalid));
                    unreachable!();
                }
                let mut newenv = env;
                for e in exprs.clone() {
                    newenv = self.analyze(e.into(), newenv);
                }
                newenv
            }

            Expr::Binary(op, left, right) => self.analyze_binary(op, left, right, env),

            Expr::List(elements) => {
                let mut newenv = env;
                for e in elements.clone() {
                    newenv = self.analyze(e.into(), newenv);
                }
                newenv
            }

            Expr::Callable(_) => {
                push(LangErrorKind::NotImplemented);
                env.clone()
            }

            Expr::Lambda(_) => env.clone(),

            Expr::Index(_ident, _args) => {
                push(LangErrorKind::NotImplemented);
                env.clone()
            }

            Expr::Apply(expr, args) => {
                match &expr.value {
                    Expr::Ident(ident) => {
                        let mut newenv = env.clone();
                        if let Some(x) = newenv.resolve_value(&ident.name) {
                            let expr = x.expr.as_ref().borrow();
                            if let Some(cb) = expr.try_callback() {
                                if !cb.t.arity.is_valid_arity(args.len()) {
                                    self.results.push(
                                        expr.context.error(LangErrorKind::InvalidNumberArgs),
                                    );
                                }
                            } else if let Some(_) = expr.try_callable() {
                            } else if let Some(_) = expr.try_lambda() {
                            } else {
                                self.results
                                    .push(expr.context.error(LangErrorKind::Error(format!(
                                        "Not a function1: {:?}",
                                        expr
                                    ))));
                            }
                        } else {
                            self.results
                                .push(expr.context.error(LangErrorKind::Error(format!(
                                    "Function not found: {:?}",
                                    expr.unlex()
                                ))));
                        }

                        for arg in args {
                            newenv = self.analyze(arg.clone().into(), newenv);
                        }
                    }
                    _ => {
                        self.results
                            .push(expr.context.error(LangErrorKind::Error(format!(
                                "Not a function2: {:?}",
                                expr.unlex()
                            ))));
                    }
                }

                // drop args from environment on return
                env
            }

            Expr::Loop(exprs) => {
                let mut newenv = env.clone();
                for expr in exprs {
                    newenv = self.analyze(expr.clone().into(), newenv);
                }
                // drop args from environment on return
                env
            }

            Expr::Block(exprs) => {
                let mut newenv = env.clone();
                for expr in exprs {
                    newenv = self.analyze(expr.clone().into(), newenv);
                }
                // drop args from environment on return
                env
            }

            Expr::Program(exprs) => {
                let mut newenv = env.clone();
                for expr in exprs {
                    newenv = self.analyze(expr.clone().into(), newenv);
                }
                newenv
            }

            Expr::Break(e) => self.analyze(e.clone().into(), env),

            Expr::Continue => env,

            Expr::Ternary(op, x, y, z) => match op {
                Operator::Conditional => {
                    // TODO: Check that the return type of conditional expr is bool
                    self.analyze(x.clone().into(), env.clone());
                    // TODO: check that branches return the same value
                    self.analyze(y.clone().into(), env.clone());
                    self.analyze(z.clone().into(), env.clone());
                    env
                }
                _ => {
                    push(LangErrorKind::NotImplemented);
                    env
                }
            },

            Expr::Chain(_, _) => {
                push(LangErrorKind::NotImplemented);
                env
            }

            Expr::Invalid(_) => {
                push(LangErrorKind::Invalid);
                env
            }
        }
    }

    fn analyze_binary(
        &mut self,
        op: &Operator,
        left: &ExprNode,
        right: &ExprNode,
        mut env: Environment,
    ) -> Environment {
        match op {
            Operator::Declare => {
                return if let Some(ident) = left.try_ident() {
                    // add the RHS to env
                    //log::debug!("Declare {:?} to {}", &right, &ident.name);
                    log::debug!("Declare {:?} to {}", &right.unlex(), &ident.name);
                    env.define(ident, right.clone().into())
                } else {
                    self.results
                        .push(left.context.error(LangErrorKind::Error(format!(
                            "Invalid Assignment, LHS must be identifier"
                        ))));
                    env
                };
            }

            Operator::Assign => {
                return if let Some(ident) = left.try_ident() {
                    match env.resolve_value(&ident.name) {
                        Some(access) => {
                            if access.modifier != VarModifier::Mutable {
                                self.results
                                    .push(left.context.error(LangErrorKind::Error(format!(
                                        "Invalid Assignment, '{}' Not mutable",
                                        &ident.name
                                    ))));
                            }

                            self.analyze(right.clone().into(), env)
                        }
                        None => {
                            left.context.error(LangErrorKind::NotFound);
                            env
                        }
                    }
                } else {
                    self.results
                        .push(left.context.error(LangErrorKind::Error(format!(
                            "Invalid Assignment, LHS must be identifier"
                        ))));
                    env
                };
            }
            _ => {
                env = self.analyze(left.clone().into(), env);
                env = self.analyze(right.clone().into(), env);

                match op {
                    Operator::Plus => (),
                    Operator::Minus => (),
                    Operator::Exp => (),
                    Operator::Multiply => (),
                    Operator::Divide => (),
                    Operator::GreaterThanEqual => (),
                    Operator::LessThanEqual => (),
                    Operator::GreaterThan => (),
                    Operator::LessThan => (),
                    Operator::Equal => (),
                    Operator::NotEqual => (),
                    _ => {
                        self.results
                            .push(left.context.error(LangErrorKind::Error(format!(
                                "Unimplemented expression op: Operator::{:?}",
                                op
                            ))));
                    }
                }
                env
            }
        }
    }

    fn check_bool(expr: &ExprNode) -> Result<bool, LangError> {
        match &expr.value {
            Expr::Literal(t) => match t {
                Tok::BoolLiteral(b) => Ok(*b),
                _ => Err(expr.context.error(LangErrorKind::ExpectBool)),
            },
            _ => Err(expr.context.error(LangErrorKind::ExpectBool)),
        }
    }

    fn check_number(&mut self, expr: &ExprNode) {
        match &expr.value {
            Expr::Literal(t) => match t {
                Tok::IntLiteral(_) => (),
                Tok::FloatLiteral(_) => (),
                _ => {
                    self.results
                        .push(expr.context.error(LangErrorKind::Error(format!(
                            "Expecting a number: {:?}",
                            expr
                        ))));
                }
            },
            _ => {
                self.results
                    .push(expr.context.error(LangErrorKind::Error(format!(
                        "Expecting a callable: {:?}",
                        expr
                    ))));
            }
        }
    }

    pub fn prefix_expr(&mut self, expr: &ExprNode, op: &Operator) {
        //self.check_number(expr);
        match op {
            Operator::Plus => (),
            Operator::Minus => (),
            _ => {
                self.results
                    .push(expr.context.error(LangErrorKind::NotImplemented));
            }
        }
    }

    pub fn postfix_expr(&mut self, expr: &ExprNode, op: &Operator) {
        //self.check_number(expr);
        match op {
            Operator::Bang => (),
            _ => {
                self.results
                    .push(expr.context.error(LangErrorKind::NotImplemented));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::program::Program;
    use test_log::test;

    #[test]
    fn analyze() {
        let mut program = Program::new();
        let mut env = Environment::default();
        env = program.analyze(
            "
        # asdf should not be visible outside the block
        {
                let mut asdf1 = 1;
                (asdf1 + 1);
                asdf1 = 2;
                assert(asdf1 == 2);
        }
        ",
            env,
        );
        program.print();
        assert!(env.resolve_value("asdf1").is_none());
    }
}
