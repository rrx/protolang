/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//! Utilities to test Starlark code execution.

// `if_then_panic` is only in newer clippy, delete this in future.
#![allow(unknown_lints)]
// We want to carefully control the panic message.
#![allow(clippy::if_then_panic)]

use gazebo::prelude::*;

use crate::codemap::CodeMap;
use crate::codemap::Pos;
use crate::codemap::Span;
use crate::errors::Diagnostic;
use crate::syntax::lexer::Lexer;
use crate::syntax::lexer::Token;
use crate::syntax::AstModule;
use crate::syntax::Dialect;

/// Environment in which to run assertion tests.
pub struct Assert {
    dialect: Dialect,
}

/// Construction and state management.
impl Assert {
    /// Create a new assert object, which will by default use
    /// [`Dialect::Extended`] and [`Globals::extended()`],
    /// plus some additional global functions like `assert_eq`.
    /// The usual pattern is to create a `mut` `Assert`, modify some properties
    /// and then execute some tests.
    pub fn new() -> Self {
        Self {
            dialect: Dialect::Extended,
        }
    }

    /// Set the [`Dialect`] that future tests will use.
    pub fn dialect(&mut self, x: &Dialect) {
        self.dialect = x.clone();
    }

    /// Set specific fields in the [`Dialect`] that future tests will use.
    pub fn dialect_set(&mut self, f: impl FnOnce(&mut Dialect)) {
        f(&mut self.dialect)
    }
}

/// Execution tests.
impl Assert {
    /// Parse some text and return the AST. Fails if the program does not parse.
    pub fn parse_ast(&self, program: &str) -> AstModule {
        match AstModule::parse("assert.bzl", program.to_owned(), &self.dialect) {
            Ok(x) => x,
            Err(e) => {
                panic!(
                    "starlark::assert::parse_ast, expected parse success but failed\nCode: {}\nError: {}",
                    program, e
                );
            }
        }
    }

    /// Parse some text and pretty-print it using enough brackets etc. to avoid
    /// ambiguity. Fails if the program does not parse.
    /// The precise form of the pretty-printed output is not stable over time.
    pub fn parse(&self, program: &str) -> String {
        self.parse_ast(program).statement.to_string()
    }

    /// Restricted to crate because 'Lexeme' isn't a public type.
    pub(crate) fn lex_tokens(&self, program: &str) -> Vec<(usize, Token, usize)> {
        fn tokens(dialect: &Dialect, program: &str) -> Vec<(usize, Token, usize)> {
            let codemap = CodeMap::new("assert.bzl".to_owned(), program.to_owned());
            Lexer::new(program, dialect, codemap.dupe())
                .collect::<Result<Vec<_>, _>>()
                .unwrap_or_else(|e|
                    panic!(
                        "starlark::assert::lex_tokens, expected lex sucess but failed\nCode: {}\nError: {}",
                        program, e
                    )
                )
        }

        // Check the invariant that each token position can't be before the previous one
        fn check_spans(tokens: &[(usize, Token, usize)]) {
            let mut pos = 0;
            for (i, t, j) in tokens {
                let span_incorrect = format!("Span of {:?} incorrect", t);
                assert!(pos <= *i, "{}: {} > {}", span_incorrect, pos, i);
                assert!(i <= j, "{}: {} > {}", span_incorrect, i, j);
                pos = *j;
            }
        }

        let orig = tokens(&self.dialect, program);
        check_spans(&orig);

        // In Starlark Windows newline characters shouldn't change the lex tokens (only the positions), so run that test too.
        // First convert \r\n to \n, in case we started with Windows newlines, so we don't get \r\r\n.
        let with_r = tokens(
            &self.dialect,
            &program.replace("\r\n", "\n").replace('\n', "\r\n"),
        );
        check_spans(&with_r);
        assert_eq!(
            orig.map(|x| &x.1),
            with_r.map(|x| &x.1),
            "starlark::assert::lex_tokens, difference using CRLF newlines\nCode: {}",
            program,
        );

        orig
    }

    /// Lex some text and pretty-print it using enough whitespace etc. to avoid
    /// ambiguity. Fails if the program does not lex.
    /// The precise form of the pretty-printed output is not stable over time.
    pub fn lex(&self, program: &str) -> String {
        self.lex_tokens(program).map(|x| x.1.unlex()).join(" ")
    }

    /// Parse some text which must fail to parse. Two exclamation marks should be
    /// placed around the span which is reported by the error.
    pub fn parse_fail(&self, contents: &str) -> anyhow::Error {
        let rest = contents.replace('!', "");
        assert!(
            rest.len() + 2 == contents.len(),
            "Must be exactly 2 ! marks around the parse error location"
        );

        let begin = contents.find('!').unwrap();
        let end = contents[begin + 1..].find('!').unwrap() + begin;

        match AstModule::parse("assert.bzl", rest, &self.dialect) {
            Ok(ast) => panic!(
                "Expected parse failure, but succeeded:\nContents: {}\nGot: {:?}",
                contents, ast
            ),
            Err(e) => {
                if let Some(d) = e.downcast_ref::<Diagnostic>() {
                    if let Some(span) = &d.span {
                        let want_span = Span::new(Pos::new(begin as u32), Pos::new(end as u32));
                        if span.span == want_span {
                            return e; // Success
                        }
                    }
                }
                panic!(
                    "Expected diagnostic with span information, but didn't get a good span:\nContents: {}\nGot: {:?}\nWanted: {:?}",
                    contents,
                    e,
                    (begin, end)
                )
            }
        }
    }
}

/// See [`Assert::parse`].
pub fn parse(program: &str) -> String {
    Assert::new().parse(program)
}

/// See [`Assert::parse_ast`].
pub fn parse_ast(program: &str) -> AstModule {
    Assert::new().parse_ast(program)
}

/// Lex some text and return the tokens. Fails if the program does not parse.
/// Only available inside the crate because the Token type is not exported.
#[cfg(test)]
pub(crate) fn lex_tokens(program: &str) -> Vec<(usize, Token, usize)> {
    Assert::new().lex_tokens(program)
}

/// See [`Assert::lex`].
pub fn lex(program: &str) -> String {
    Assert::new().lex(program)
}

/// See [`Assert::parse_fail`].
pub fn parse_fail(program: &str) -> anyhow::Error {
    Assert::new().parse_fail(program)
}
