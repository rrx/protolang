use link::*;
use std::env;
use std::error::Error;
use std::fs;
use std::path::Path;

fn main() -> Result<(), Box<dyn Error>> {
    env_logger::init();
    let mut reader = Reader::new();
    for path in env::args().skip(1) {
        let buf = std::fs::read(path.clone())?;
        let block = reader.read(&path, &buf)?;
        block.dump();
    }
    //let mut block = reader.build();
    //let mut data = link::Data::new(block.libs.iter().cloned().collect());
    //let mut out_data = Vec::new();
    //let endian = object::Endianness::Little;
    //let mut writer = object::write::elf::Writer::new(endian, data.is_64, &mut out_data);
    //block.build_strings(&mut data, &mut writer);
    //block.dump();
    Ok(())
}
