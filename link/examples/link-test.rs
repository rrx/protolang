use link::*;
use std::path::Path;

fn main() {
    let mut b = Link::new();
    b.add_library("t1", Path::new("tmp/live.so"));
    b.add_library("t2", Path::new("libsigsegv.so")).unwrap();
    //b.add_library("dl", Path::new("libdl.so.2")).unwrap();
    b.add_obj_file("t3", Path::new("tmp/segfault.o")).unwrap();
    //b.add_obj_file("t4", Path::new("tmp/live.o")).unwrap();
    b.add_obj_file("main", Path::new("tmp/empty_main.o"))
        .unwrap();
    //b.add_obj_file("crt", Path::new("/usr/lib/x86_64-linux-gnu/crt1.o")).unwrap();
    //b.add_library("libc", Path::new("/lib/x86_64-linux-gnu/libc.so.6")).unwrap();
    //b.add_archive_file("libc", Path::new("/lib/x86_64-linux-gnu/libc.a")).unwrap();
    //
    //musl
    //b.add_library("libc", Path::new("/usr/lib/x86_64-linux-musl/libc.so")).unwrap();

    // embedded artistry
    b.add_archive_file(
        "libc",
        Path::new("/home/rrx/src/libc/buildresults/src/libc.a"),
    )
    .unwrap();

    let version = b.link().unwrap();
    //version.debug();
    //
    let ptr = version.lookup("__libc_start_main").unwrap();
    println!("ptr: {:#08x}", ptr as usize);

    let ret: i64 = version
        .invoke(
            "strlen",
            (std::ffi::CString::new("asdf").unwrap().as_ptr(),),
        )
        .unwrap();
    assert_eq!(4, ret);

    let ret: i64 = version.invoke("call_live", (3,)).unwrap();
    println!("ret: {:#08x}", ret);

    let ret: i64 = version.invoke("handlers_init", ()).unwrap();
    //let ret: i64 = version.invoke("segfault_me", ()).unwrap();

    /*
    let ret: i64 = version.invoke("_start", (0,0,0,0,0)).unwrap();
    assert_eq!(17, ret);
    */
}
