use lang1::compiler::direct;
use std::env;

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    direct::compile_and_execute(args);
}
