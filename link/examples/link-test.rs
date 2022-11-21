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
    b.add_obj_file("crt1", Path::new("/usr/lib/x86_64-linux-musl/crt1.o")).unwrap();
    b.add_obj_file("crti", Path::new("/usr/lib/x86_64-linux-musl/crti.o")).unwrap();
    b.add_obj_file("crtn", Path::new("/usr/lib/x86_64-linux-musl/crtn.o")).unwrap();
    b.add_library("libc", Path::new("/usr/lib/x86_64-linux-musl/libc.so")).unwrap();

    // embedded artistry
    //b.add_archive_file(
        //"libc",
        //Path::new("/home/rrx/src/libc/buildresults/src/libc.a"),
    //)
    //.unwrap();

    let version = b.link().unwrap();
    //version.debug();
    //

    let main_ptr = version.lookup("main").unwrap();
    println!("ptr: {:#08x}", main_ptr as usize);

    let init_ptr = version.lookup("_init").unwrap();
    println!("init_ptr: {:#08x}", init_ptr as usize);

    let (base, size) = b.get_mem_ptr();
    let ret: i64 = version.invoke("initialize", (base, main_ptr)).unwrap();
    assert_eq!(0, ret);

    let ret: i64 = version.invoke("call_live", (3,)).unwrap();
    println!("ret: {:#08x}", ret);

    let ret: i64 = version.invoke("handlers_init", ()).unwrap();
    //let ret: i64 = version.invoke("segfault_me", ()).unwrap();
    //
    let ret: i64 = version.invoke("main", ()).unwrap();
    assert_eq!(0, ret);


    /*
    let ptr = version.lookup("_start").unwrap();
    println!("ptr: {:#08x}", ptr as usize);

    */
    let ret: i64 = version.invoke("_start", (main_ptr,0,0,0,0)).unwrap();
    assert_eq!(0, ret);

    let ret: i64 = version
        .invoke(
            "strlen",
            (std::ffi::CString::new("asdf").unwrap().as_ptr(),),
        )
        .unwrap();
    assert_eq!(4, ret);

}
