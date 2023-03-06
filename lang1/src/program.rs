use crate::ast::*;
use crate::eval::*;
//use crate::ir::{self, TypeChecker, IR};
use crate::results::*;
use crate::tokens::{FileId, Tok, TokensList};
use nom::InputIter;

use log::*;

#[derive(Default)]
pub struct Results {
    results: Compiler,
}

impl Results {
    pub fn print(&self) {
        self.results.print();
    }

    pub fn clear(&mut self) {
        self.results.clear();
    }

    pub fn push_error(&mut self, r: LangError) {
        self.results.push(r);
    }

    pub fn add_source(&mut self, filename: String, source: String) -> FileId {
        self.results.add_source(filename, source)
    }

    pub fn has_errors(&self) -> bool {
        self.results.has_errors
    }
}

pub struct Program {
    checker: Results,
    pub value: ExprRefWithEnv,
}

impl Program {
    pub fn new() -> Self {
        Self {
            checker: Results::default(),
            value: ExprRefWithEnv::new(Expr::Void.into(), Environment::default()),
        }
    }

    pub fn print(&self) {
        self.checker.print();
    }

    pub fn clear(&mut self) {
        self.checker.clear();
    }

    pub fn add_result(&mut self, value: ExprRefWithEnv) {
        self.value = value;
    }

    pub fn parse_str(&mut self, v: &str) -> anyhow::Result<ExprNode> {
        let file_id = self.checker.add_source("<repl>".into(), v.to_string());
        self.parse(v, file_id)
    }

    pub fn parse(&mut self, s: &str, file_id: FileId) -> anyhow::Result<ExprNode> {
        let mut lexer = crate::lexer::LexerState::default().set_file_id(file_id);
        match lexer.lex(s) {
            Ok((_, tokens)) => {
                tokens.iter_elements().for_each(|t| {
                    if let Tok::Invalid(s) = &t.tok {
                        let error = t
                            .to_context()
                            .error(LangErrorKind::Warning(format!("Invalid Token: {}", s)));
                        self.checker.push_error(error);
                    }
                });

                match crate::parser::parse_program(tokens) {
                    Ok((_, expr)) => Ok(expr),
                    Err(nom::Err::Error(e)) => {
                        for (tokens, err) in e.errors {
                            error!("error {:?}", (&err, tokens.toks()));
                        }
                        let error = LangError::runtime("Error parsing");
                        self.checker.push_error(error.clone());
                        Err(error.into())
                    }
                    Err(e) => Err(LangError::runtime(&format!("Error parsing: {:?}", e)).into()),
                }
            }
            Err(e) => {
                return Err(LangError::runtime(&format!("Error lexing: {:?}", e)).into());
            }
        }
    }

    pub fn parse_file(&mut self, filename: &str) -> anyhow::Result<ExprNode> {
        let contents = std::fs::read_to_string(filename.clone())?.to_string();
        let file_id = self.checker.add_source(filename.into(), contents.clone());
        self.parse(&contents, file_id)
    }

    pub fn analyze_file(&mut self, filename: &str, env: Environment) -> Environment {
        let contents = std::fs::read_to_string(filename.clone())
            .unwrap()
            .to_string();
        self._analyze(filename, &contents, env)
    }

    pub fn analyze(&mut self, v: &str, env: Environment) -> Environment {
        self._analyze("<repl>", v, env)
    }

    fn _analyze(&mut self, filename: &str, v: &str, mut env: Environment) -> Environment {
        let file_id = self.checker.add_source(filename.into(), v.to_string());
        match self.parse(v, file_id) {
            Ok(expr) => {
                //use crate::sexpr::SExpr;
                //debug!("SEXPR: {}", expr.sexpr().unwrap());

                // Analyze it
                let mut a = Analysis::new();
                env = a.analyze(expr.clone().into(), env);
                a.results.iter().for_each(|r| {
                    self.checker.push_error(r.clone());
                });
                if self.checker.has_errors() {
                    self.checker
                        .push_error(expr.context.error(LangErrorKind::AnalysisFailed));
                }
            }
            Err(e) => {
                let error = LangError::runtime(&format!("Error analyzing: {:?}", e));
                self.checker.push_error(error);
            }
        }
        env
    }

    pub fn eval(&mut self, v: &str, env: Environment) -> Result<ExprRefWithEnv, LangError> {
        self._eval_file("<repl>", v, env)
    }

    pub fn eval_file(
        &mut self,
        filename: &str,
        env: Environment,
    ) -> Result<ExprRefWithEnv, LangError> {
        let contents = std::fs::read_to_string(filename.clone())
            .unwrap()
            .to_string();
        self._eval_file(filename, &contents, env)
    }

    fn _eval_file(
        &mut self,
        filename: &str,
        v: &str,
        env: Environment,
    ) -> Result<ExprRefWithEnv, LangError> {
        let file_id = self.checker.add_source(filename.into(), v.to_string());
        match self.parse(v, file_id) {
            Ok(expr) => {
                //debug!("SEXPR: {}", expr.sexpr().unwrap());
                // Analyze it
                //let check_env = crate::ir::base_env();
                //let ir = self.checker.parse_ast(&expr, check_env);
                //debug!("ir {:?}", &ir);

                let mut a = Analysis::new();
                let env = a.analyze(expr.clone().into(), env);
                a.results.iter().for_each(|r| {
                    self.checker.push_error(r.clone());
                });
                if self.checker.has_errors() {
                    self.checker.print();
                    return Err(expr.context.error(LangErrorKind::AnalysisFailed));
                }

                match Interpreter::evaluate(expr.into(), env) {
                    Ok(v) => Ok(v),
                    Err(LangError { loc, kind }) => {
                        let error = LangError { loc, kind };
                        self.checker.push_error(error.clone());
                        Err(error)
                    }
                }
            }
            Err(e) => {
                let error = LangError::runtime(&format!("Eval Error: {:?}", e));
                self.checker.push_error(error.clone());
                Err(error.into())
            }
        }
    }

    //pub fn check_str(&mut self, s: &str, env: ir::Environment) -> anyhow::Result<IR> {
    //self.checker.check_str(s, env)
    //}

    //pub fn check_file(&mut self, filename: &str, env: ir::Environment) -> anyhow::Result<IR> {
    //self.checker.check_file(filename, env)
    //}
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;
    #[test]
    fn parse() {
        let mut p = Program::new();
        let _ = p.parse_file("examples/test.p").unwrap();
    }

    #[test]
    fn eval_example() {
        let mut program = Program::new();
        let env = Environment::default();
        let r = program.eval_file("examples/test.p", env);
        program.checker.print();
        r.unwrap();
    }

    #[test]
    fn eval() {
        let mut program = Program::new();
        let env = Environment::default();
        let r = program.eval("let a=1", env).unwrap();
        r.env.debug();
        assert!(r.env.resolve_value("a").is_some());
    }
}
