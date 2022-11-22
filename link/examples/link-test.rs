use link::*;
use std::path::Path;

fn test_start() {
    let mut b = Link::new();
    b.add_obj_file("start", Path::new("tmp/start.o")).unwrap();
    let version = b.link().unwrap();
    let ret: i64 = version.invoke("_start", ()).unwrap();
    assert_eq!(0, ret);
}

fn test_live_shared() {
    let mut b = Link::new();
    b.add_library("t1", Path::new("tmp/live.so")).unwrap();
    let version = b.link().unwrap();
    let ret: i64 = version.invoke("call_live", (3,)).unwrap();
    println!("ret: {:#08x}", ret);
    assert_eq!(0x11, ret);
}

fn test_live_static() {
    let mut b = Link::new();
    b.add_obj_file("t1", Path::new("tmp/live.o")).unwrap();
    let version = b.link().unwrap();
    let ret: i64 = version.invoke("call_live", (3,)).unwrap();
    println!("ret: {:#08x}", ret);
    assert_eq!(0x11, ret);
}

fn test_empty_main() {
    let mut b = Link::new();
    b.add_obj_file("main", Path::new("tmp/empty_main.o"))
        .unwrap();
    b.add_library("libc", Path::new("/usr/lib/x86_64-linux-musl/libc.so")).unwrap();
    let version = b.link().unwrap();

    // just call the mepty main
    let ret: i64 = version.invoke("main", ()).unwrap();
    assert_eq!(0, ret);

    let main_ptr = version.lookup("main").unwrap();
    println!("ptr: {:#08x}", main_ptr as usize);
    let main_ptr = 0;

    let init_ptr = version.lookup("_init").unwrap();
    println!("init_ptr: {:#08x}", init_ptr as usize);
    let init_ptr = 0;

    let (base, size) = b.get_mem_ptr();
    let ret: i64 = version.invoke("initialize", (base, main_ptr, init_ptr)).unwrap();
    assert_eq!(0, ret);
}

fn test_segfault() {
    let mut b = Link::new();
    b.add_library("t2", Path::new("libsigsegv.so")).unwrap();
    b.add_obj_file("t3", Path::new("tmp/segfault.o")).unwrap();
    let version = b.link().unwrap();
    let ret: i64 = version.invoke("handlers_init", ()).unwrap();
}

fn test_libuv() {
    let mut b = Link::new();
    b.add_library("libc", Path::new("/lib/x86_64-linux-gnu/libc.so.6")).unwrap();
    //b.add_library("libc", Path::new("/usr/lib/x86_64-linux-musl/libc.so")).unwrap();
    b.add_library("libuv", Path::new("libuv.so")).unwrap();
    b.add_obj_file("test", Path::new("tmp/uvtest.o")).unwrap();
    let version = b.link().unwrap();
    let ret: i64 = version.invoke("uvtest", ()).unwrap();
    println!("ret: {:#08x}", ret);
    assert_eq!(0x0, ret);

}

fn test_libc() {
    let mut b = Link::new();
    b.add_library("libc", Path::new("/lib/x86_64-linux-gnu/libc.so.6")).unwrap();
    let version = b.link().unwrap();
    unsafe {
        let stdout = *(version.lookup("stdout").unwrap() as *const usize);
        println!("stdout: {:#08x}", stdout);
        let ret: i64 = version.invoke("putc", (0x31u32, stdout)).unwrap();
        println!("ret: {:#08x}", ret);
        assert_eq!(0x31, ret);
        let ret: i64 = version.invoke("fflush", (stdout, )).unwrap();
        println!("ret: {:#08x}", ret);
        assert_eq!(0x0, ret);
    }
}

fn test_libc_musl() {
    let mut b = Link::new();
    b.add_library("libc", Path::new("/usr/lib/x86_64-linux-musl/libc.so")).unwrap();
    let version = b.link().unwrap();

    // call strlen
    let ret: i64 = version
        .invoke(
            "strlen",
            (std::ffi::CString::new("asdf").unwrap().as_ptr(),),
        )
        .unwrap();
    assert_eq!(4, ret);

    unsafe {
        let stdout = *(version.lookup("stdout").unwrap() as *const usize);
        println!("stdout: {:#08x}", stdout);

        let ret: i64 = version.invoke("fputc", (0x30u32, stdout)).unwrap();
        println!("ret: {:#08x}", ret);
        assert_eq!(0x30, ret);

        let ret: i64 = version.invoke("putc", (0x31u32, stdout)).unwrap();
        println!("ret: {:#08x}", ret);
        assert_eq!(0x31, ret);

        let c_str = std::ffi::CString::new("asdf\n").unwrap();
        let c_str_ptr = c_str.as_ptr();
        let ret: i64 = version.invoke("fputs", (c_str_ptr, stdout)).unwrap();
        println!("ret: {:#08x}", ret);
        assert_eq!(0x0, ret);

        let ret: i64 = version.invoke("fflush", (stdout)).unwrap();
        println!("ret: {:#08x}", ret);
        //assert_eq!(0x30, ret);
    }
}


fn main() {
    //test_start();
    test_live_shared();
    test_live_static();
    test_libc();
    test_libuv();
    //test_segfault();
    //test_empty_main();
    //let mut b = Link::new();
    //b.add_library("t1", Path::new("tmp/live.so"));
    //b.add_library("t2", Path::new("libsigsegv.so")).unwrap();
    //b.add_library("dl", Path::new("libdl.so.2")).unwrap();
    //b.add_obj_file("t3", Path::new("tmp/segfault.o")).unwrap();
    //b.add_obj_file("t4", Path::new("tmp/live.o")).unwrap();
    //b.add_obj_file("main", Path::new("tmp/empty_main.o"))
        //.unwrap();
    //b.add_obj_file("crt", Path::new("/usr/lib/x86_64-linux-gnu/crt1.o")).unwrap();
    //b.add_library("libc", Path::new("/lib/x86_64-linux-gnu/libc.so.6")).unwrap();
    //b.add_archive_file("libc", Path::new("/lib/x86_64-linux-gnu/libc.a")).unwrap();
    //
    //musl
    //b.add_obj_file("crt1", Path::new("/usr/lib/x86_64-linux-musl/crt1.o")).unwrap();
    //b.add_obj_file("crti", Path::new("/usr/lib/x86_64-linux-musl/crti.o")).unwrap();
    //b.add_obj_file("crtn", Path::new("/usr/lib/x86_64-linux-musl/crtn.o")).unwrap();
    //b.add_library("libc", Path::new("/usr/lib/x86_64-linux-musl/libc.so")).unwrap();

    // embedded artistry
    //b.add_archive_file(
        //"libc",
        //Path::new("/home/rrx/src/libc/buildresults/src/libc.a"),
    //)
    //.unwrap();

    //let version = b.link().unwrap();
    //version.debug();

    //let ret: i64 = version.invoke("handlers_init", ()).unwrap();
    //let ret: i64 = version.invoke("segfault_me", ()).unwrap();
    //
    //let ret: i64 = version.invoke("main", ()).unwrap();
    //assert_eq!(0, ret);


    /*
    let ptr = version.lookup("_start").unwrap();
    println!("ptr: {:#08x}", ptr as usize);

    */
    //let ret: i64 = version.invoke("_start", (main_ptr,0,0,0,0)).unwrap();
    //assert_eq!(0, ret);
}
