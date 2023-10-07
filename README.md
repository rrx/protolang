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

