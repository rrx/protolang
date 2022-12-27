use link::*;
use std::env;
use std::error::Error;
use std::fs;
use std::path::Path;

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();
    let mut reader = Reader::new();
    for path in env::args().skip(1) {
        reader.add(&Path::new(&path));
    }
    let mut block = reader.build();
    block.dump();
    Ok(())
}
