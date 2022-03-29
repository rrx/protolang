use crate::ast::*;
use crate::eval::*;
use crate::results::*;
use crate::sexpr::SExpr;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use log::debug;

type FileId = usize;

impl LangError {
    pub fn diagnostic(&self, file_id: FileId) -> Diagnostic<FileId> {
        match &self.kind {
            LangErrorKind::Warning(msg) | LangErrorKind::Error(msg) => Diagnostic::warning()
                .with_message(msg)
                .with_labels(vec![Label::primary(file_id, self.context.range())]),
            _ => Diagnostic::warning()
                .with_message(&format!("{}", &self))
                .with_labels(vec![Label::primary(file_id, self.context.range())]),
        }
    }
}

impl InterpretError {
    pub fn diagnostic(&self, file_id: FileId) -> Diagnostic<FileId> {
        match &self.kind {
            InterpretErrorKind::Runtime(message) => Diagnostic::error()
                .with_message(message)
                .with_labels(vec![Label::primary(file_id, self.context.range())]),
            _ => Diagnostic::error()
                .with_message(format!("{}", self))
                .with_labels(vec![Label::primary(file_id, self.context.range())]),
        }
    }
}

pub struct Program {
    pub diagnostics: Vec<Diagnostic<FileId>>,
    pub files: SimpleFiles<String, String>,
    pub value: ExprRefWithEnv,
}

impl Program {
    pub fn new() -> Self {
        Self {
            diagnostics: vec![],
            files: SimpleFiles::new(),
            value: ExprRefWithEnv::new(Expr::Void.into(), Environment::default()),
        }
    }

    pub fn add_result(&mut self, value: ExprRefWithEnv) {
        self.value = value;
    }

    pub fn add_source(&mut self, filename: String, source: String) -> FileId {
        self.files.add(filename, source)
    }

    pub fn print(&self) {
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();
        for diagnostic in self.diagnostics.iter() {
            let _ = term::emit(&mut writer.lock(), &config, &self.files, &diagnostic);
        }
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
        let mut lexer = LexerState::from_str_eof(v).unwrap();
        let tokens = lexer.tokens();
        use nom::InputIter;

        let file_id = self.add_source(filename.into(), v.to_string());
        tokens.iter_elements().for_each(|t| {
            if let Tok::Invalid(s) = &t.tok {
                let error = t
                    .to_context()
                    .lang_error(LangErrorKind::Warning(format!("Invalid Token: {}", s)));
                let diagnostic = error.diagnostic(file_id);
                self.push(diagnostic);
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
                let mut has_errors = false;
                a.results.iter().for_each(|r| {
                    if let LangErrorKind::Warning(_) = r.kind {
                    } else {
                        has_errors = true;
                    }
                    self.push(r.diagnostic(file_id));
                });
            }
            Err(e) => {
                let error = InterpretError::runtime(&format!("Error parsing: {:?}", e));
                let diagnostic = error.diagnostic(file_id);
                self.push(diagnostic);
            }
        }
        env
    }

    pub fn eval(&mut self, v: &str, env: Environment) -> Result<ExprRefWithEnv, InterpretError> {
        self._eval_file("<repl>", v, env)
    }

    pub fn eval_file(
        &mut self,
        filename: &str,
        env: Environment,
    ) -> Result<ExprRefWithEnv, InterpretError> {
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
    ) -> Result<ExprRefWithEnv, InterpretError> {
        use crate::lexer::LexerState;
        use crate::tokens::*;
        let mut lexer = LexerState::from_str_eof(v).unwrap();
        let tokens = lexer.tokens();
        use nom::InputIter;

        let file_id = self.add_source(filename.into(), v.to_string());
        tokens.iter_elements().for_each(|t| {
            if let Tok::Invalid(s) = &t.tok {
                let error = t
                    .to_context()
                    .lang_error(LangErrorKind::Warning(format!("Invalid Token: {}", s)));
                let diagnostic = error.diagnostic(file_id);
                self.push(diagnostic);
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
                let env = a.analyze(expr.clone().into(), env);
                let mut has_errors = false;
                a.results.iter().for_each(|r| {
                    if let LangErrorKind::Warning(_) = r.kind {
                    } else {
                        has_errors = true;
                    }
                    self.push(r.diagnostic(file_id));
                });

                if has_errors {
                    return Err(expr.context.error(InterpretErrorKind::AnalysisFailed));
                }

                match Interpreter::evaluate(expr.into(), env) {
                    Ok(v) => Ok(v),
                    Err(InterpretError { context, kind }) => {
                        let error = InterpretError { context, kind };
                        let diagnostic = error.diagnostic(file_id);
                        self.push(diagnostic);
                        Err(error)
                    }
                }
            }
            Err(nom::Err::Error(e)) => {
                for (tokens, err) in e.errors {
                    debug!("error {:?}", (&err, tokens.toks()));
                }
                let error = InterpretError::runtime("Error parsing");
                let diagnostic = error.diagnostic(file_id);
                self.push(diagnostic);
                Err(error)
            }
            Err(e) => {
                let error = InterpretError::runtime(&format!("Error parsing: {:?}", e));
                let diagnostic = error.diagnostic(file_id);
                self.push(diagnostic);
                Err(error)
            }
        }
    }
}

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
