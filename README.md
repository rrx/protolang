# Overview

DEX - an interpreter/compiler focused on Developer Experience

This is a toy compiler used for experiments in programming language design.  It has the following features:

- lossless parser


A lossless compiler preserves whitespace and structure, allowing you to parse the code to an AST, and then covert it back to it's original form.

The goal is to create a simple compiler that can also be used for:
- syntax highlighting, linting, test runner
- code refactoring, and evaluation
- live coding
- self-hosting embedded language similar to Emacs Lisp 
- highly configurable parser that can be dynamically configured
- code transformation and transpilation
- more flexible type inference than Rust
- more granular ownership checking for concurrent code than Rust
- eliminate the need for Rust style macros, by being able to modify code at compile time, using the same language
- a flexible solution for adhoc DSLs, similar to Racket
- experiments in hetergeneous computing (Multiple CPU types + GPUs + FPGAs + Network)
- experiments in multi-paradigm programming (functional, logic, mathematical, reactive, declarative, symbolic)
- numerical calculation performance comparable to C++/Rust

Ideas
- null pointer exceptions should be impossible, unless the option is explicit (like Rust unwrap)


TODO

- [x] Lossless Basic Lexer
- [x] Pratt parser for operation precedence
- [x] Lossless parser 
- [x] sexpr transpiler
- [x] interpreter
- [x] REPL
- [ ] closures
- [ ] branching and control flow
- [ ] recursion
- [ ] integrated unit testing
- [ ] syntax highlighter
- [ ] code formatter
- [ ] basic type inference
- [ ] f-strings
- [ ] module system
- [ ] basic runtime
- [ ] basic polymorphism
- [ ] compile to machine code using LLVM
- [ ] basic build and test tooling
- [ ] parser combinator library
- [ ] self-hosting language parser
- [ ] self-hosting language interpreter
- [ ] embed in an editor
- [ ] syntax highlighting for the editor
- [ ] autoformat for the editor
- [ ] code graph API for query
- [ ] code completion
- [ ] code navigation
- [ ] LSP implementation

Future

- [ ] friendly compiler errors
- [ ] c-library interop
- [ ] rust interop (using stable_abi?)
- [ ] use logos for lexing (https://docs.rs/logos/latest/logos/)

References

- full-moon: Lossless Lua Parser: https://github.com/Kampfkarren/full-moon
- minipratt: https://github.com/matklad/minipratt/blob/master/src/bin/pratt.rs
- RSLint: https://github.com/rslint/rslint/blob/master/crates/rslint_lexer/src/highlight.rs
