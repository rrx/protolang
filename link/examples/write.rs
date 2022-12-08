use link::*;
use std::path::Path;

fn main() {
    let mut b = Link::new();
    //b.add_obj_file("test", Path::new("./tmp/start.o")).unwrap();
    let path = Path::new("/usr/lib/x86_64-linux-gnu/crt1.o");
    b.add_obj_file("crt1", path).unwrap();
    b.write(Path::new("./tmp/out.exe")).unwrap();
}
