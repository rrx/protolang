use link::*;
use std::path::Path;

fn main() {
    let mut b = Link::new();
    b.add_library("test", Path::new("libsigsegv.so"));
    b.add_obj_file("test", Path::new("tmp/segfault.o"))
        .unwrap();
    let version = b.link().unwrap();
    version.debug();
    let ret: i64 = version.invoke("handlers_init", ()).unwrap();
    //let ret: i64 = collection.invoke("segfault_me", ()).unwrap();
    //println!("ret: {:#08x}", ret);
    //assert_eq!(13, ret);
}
