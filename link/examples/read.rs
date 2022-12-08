use link::*;
use std::env;
use std::error::Error;
use std::fs;
use std::path::Path;

fn main() -> Result<(), Box<dyn Error>> {
    let path = env::args().nth(1).unwrap();
    let buf = fs::read(path)?;
    elf_read(&buf)?;
    Ok(())
}
