# TLDR

This is a personal repo for learning about programming languages.  Nothing usable yet.

`protolang` means "Language Prototyping"

# Why

Rust has a lot of potential for building programming languages, and I want to explore that here.  There are a few projects here with different experiements, which include:

- Parsing starlark code and then lowering to LLVM
- A simple toy language with functions, lowered to LLVM
- A simple interpreter for the toy language

What I'm hoping to achieve here is a proof of concept programming language that can easily lower to LLVM.  It turns out that building a language that compiles down to LLVM isn't trivial, however every language that does compile to LLVM does almost the exact same thing, just slightly different, and with it's own set of bugs.  I'm hoping to reduce the barrier to entry for language experimentation by making an expressive intermediate representation in Rust which lowers to LLVM or MLIR.  Then all the language designer needs to do is convert their AST to this IR, and the library will take care of the rest.  This is one option that I've explored.

This repo mostly explores building on LLVM directly.  In the future, it might be possible to instead build on MLIR, which might hopefully reduce the amount of work.  This is actually the goal of MLIR, though it's just as overwealming as LLVM.

Another option is to do AST-AST transformation, where you convert your AST to the C-AST which is part of LLVM.  This AST could be used to generate C code, or LLVM code directly.  This might be more helpful for language designers, since it allows you to easily write much of your code in C, and makes C-interop very easy.  C-Interop is the bare minimum a language needs to be interesting without having to build everything from scratch that we expect from a language.

# Setup

Install LLVM v13, see: https://apt.llvm.org/

Set environment variable for building:

```
export LLVM_SYS_130_PREFIX=/usr/lib/llvm-13/
```

Only LLVM-13 is currently supported

A few modules are needed as well.

See:
- https://sr.ht/~jplatte/cargo-depgraph/
- https://github.com/regexident/cargo-modules

```
cargo install cargo-modules cargo-depgraph
```

# References

- https://mlir.llvm.org/
- https://github.com/facebookexperimental/starlark-rust

