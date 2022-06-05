use crate::ast::*;
use crate::eval::*;
use crate::results::*;
use crate::sexpr::SExpr;
use crate::ir::TypeChecker;

use log::debug;

pub struct Program {
    checker: TypeChecker,
    pub value: ExprRefWithEnv,
}

impl Program {
    pub fn new() -> Self {
        Self {
            checker: TypeChecker::default(),
            value: ExprRefWithEnv::new(Expr::Void.into(), Environment::default()),
        }
    }

    pub fn add_result(&mut self, value: ExprRefWithEnv) {
        self.value = value;
    }

    pub fn parse(s: &str) -> anyhow::Result<ExprNode> {
        let mut lexer = crate::lexer::LexerState::default();
        let (_, _) = lexer.lex(s).unwrap();
        let (_, expr) = crate::parser::parse_program(lexer.tokens().clone()).unwrap();
        Ok(expr)
    }

    pub fn parse_file(filename: &str) -> anyhow::Result<ExprNode> {
        let contents = std::fs::read_to_string(filename.clone())
            .unwrap()
            .to_string();
        let mut lexer = crate::lexer::LexerState::default();
        let (_, _) = lexer.lex(contents.as_str()).unwrap();
        let (_, expr) = crate::parser::parse_program(lexer.tokens().clone()).unwrap();
        Ok(expr)
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
        use crate::lexer::LexerState;
        use crate::tokens::*;
        let file_id = self.checker.results.add_source(filename.into(), v.to_string());
        let mut lexer = LexerState::from_str_eof(v).unwrap().set_file_id(file_id);
        let tokens = lexer.tokens();
        use nom::InputIter;

        tokens.iter_elements().for_each(|t| {
            if let Tok::Invalid(s) = &t.tok {
                let error = t
                    .to_context()
                    .error(LangErrorKind::Warning(format!("Invalid Token: {}", s)));
                self.checker.results.push(error);
            }
        });

        match crate::parser::parse_program(tokens) {
            Ok((end, expr)) => {
                debug!("SEXPR: {}", expr.sexpr().unwrap());
                //debug!("program has {} expressions", exprs.len());
                use nom::InputLength;
                if end.input_len() > 0 {
                    debug!("program rest {:?}", end);
                }

                // Analyze it
                let mut a = Analysis::new();
                env = a.analyze(expr.clone().into(), env);
                a.results.iter().for_each(|r| {
                    self.checker.results.push(r.clone());
                });
                let has_errors = self.checker.results.has_errors;
            }
            Err(e) => {
                let error = LangError::runtime(&format!("Error parsing: {:?}", e));
                self.checker.results.push(error);
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
        use crate::lexer::LexerState;
        use crate::tokens::*;
        let file_id = self.checker.results.add_source(filename.into(), v.to_string());
        let mut lexer = LexerState::from_str_eof(v).unwrap().set_file_id(file_id);

        let tokens = lexer.tokens();
        use nom::InputIter;

        tokens.iter_elements().for_each(|t| {
            if let Tok::Invalid(s) = &t.tok {
                let error = t
                    .to_context()
                    .error(LangErrorKind::Warning(format!("Invalid Token: {}", s)));
                self.checker.results.push(error);
            }
        });

        match crate::parser::parse_program(tokens) {
            Ok((end, expr)) => {
                debug!("SEXPR: {}", expr.sexpr().unwrap());
                //debug!("program has {} expressions", exprs.len());
                use nom::InputLength;
                if end.input_len() > 0 {
                    debug!("program rest {:?}", end);
                }

                //let check_env = crate::ir::base_env();
                //let ir = self.checker.parse_ast(&expr, check_env);

                // Analyze it
                let mut a = Analysis::new();
                let env = a.analyze(expr.clone().into(), env);
                a.results.iter().for_each(|r| {
                    self.checker.results.push(r.clone());
                });

                if self.checker.results.has_errors {
                    return Err(expr.context.error(LangErrorKind::AnalysisFailed));
                }

                match Interpreter::evaluate(expr.into(), env) {
                    Ok(v) => Ok(v),
                    Err(LangError { context, kind }) => {
                        let error = LangError { context, kind };
                        self.checker.results.push(error.clone());
                        Err(error)
                    }
                }
            }
            Err(nom::Err::Error(e)) => {
                for (tokens, err) in e.errors {
                    debug!("error {:?}", (&err, tokens.toks()));
                }
                let error = LangError::runtime("Error parsing");
                self.checker.results.push(error.clone());
                Err(error)
            }
            Err(e) => {
                let error = LangError::runtime(&format!("Error parsing: {:?}", e));
                self.checker.results.push(error.clone());
                Err(error)
            }
        }
    }

    pub fn print(&self) {
        self.checker.results.print();
    }

    pub fn clear(&mut self) {
        self.checker.results.clear();
    }
}

/*
impl std::ops::Deref for Program {
    type Target = Vec<Diagnostic<FileId>>;

    fn deref(&self) -> &Self::Target {
        &self.diagnostics
    }
}

impl std::ops::DerefMut for Program {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.diagnostics
    }
}
*/

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;
    #[test]
    fn parse() {
        let _ = Program::parse_file("examples/test.p").unwrap();
    }

    #[test]
    fn eval_example() {
        let mut program = Program::new();
        let env = Environment::default();
        program.eval_file("examples/test.p", env).unwrap();
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
