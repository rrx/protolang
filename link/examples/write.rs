use link::*;
use std::path::Path;

fn main() {
    let mut b = Link::new();
    b.add_obj_file("test", Path::new("./tmp/empty_main.o"))
        .unwrap();
    b.write(Path::new("./tmp/out.exe")).unwrap();
}

