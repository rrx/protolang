use link::*;
use std::env;
use std::error::Error;
use std::path::Path;

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();
    let mut reader = Reader::new();
    for path in env::args().skip(1) {
        reader.add(&Path::new(&path))?;
    }
    let block = reader.build();
    block.dump();
    block.write::<object::elf::FileHeader64<object::Endianness>>(Path::new("tmp/out.exe"))?;
    Ok(())
}
