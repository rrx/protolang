use link::*;
use std::path::Path;

fn main() {
    let mut b = Link::new();
    //b.add_library("t1", Path::new("tmp/live.so"));
    b.add_library("t2", Path::new("libsigsegv.so"));
    //b.add_obj_file("t3", Path::new("tmp/segfault.o"))
    //.unwrap();
    b.add_obj_file("t4", Path::new("tmp/live.o")).unwrap();

    let version = b.link().unwrap();
    //version.debug();

    //let ret: i64 = version.invoke("handlers_init", ()).unwrap();
    let ret: i64 = version.invoke("call_live", (3,)).unwrap();
    println!("ret: {:#08x}", ret);
    assert_eq!(17, ret);
    //let ret: i64 = collection.invoke("segfault_me", ()).unwrap();
}
