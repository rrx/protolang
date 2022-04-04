use protolang::compiler::test;
use std::env;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    test(args);
}

