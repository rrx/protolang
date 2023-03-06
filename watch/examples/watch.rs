use codegen_llvm::{Context, OptimizationLevel};
use link::*;
use notify::event::AccessKind;
use notify::event::AccessMode;
use notify::EventKind::*;
use notify::{Config, RecommendedWatcher, RecursiveMode, Watcher};
use std::error::Error;
use std::fs;
use std::path::Path;
use std::sync::mpsc;
use std::{thread, time};
use watch::*;

fn main() -> Result<(), Box<dyn Error>> {
    let (tx, rx) = mpsc::channel::<LinkVersionSync>();

    // path hardwired for now, eventually this will be configurable
    let dir = Path::new("watch/examples");

    thread::scope(|s| {
        s.spawn(move || {
            let mut maybe_version = None;
            loop {
                let result = rx.try_recv();
                if let Ok(version) = result {
                    maybe_version = Some(version.clone());
                    invoke(&version);
                } else {
                    thread::sleep(time::Duration::from_secs(1));
                    if let Some(version) = &maybe_version {
                        invoke(version)
                    } else {
                        eprintln!("waiting for code");
                    }
                    //unimplemented!();
                }
            }
        });
        s.spawn(move || {
            run(&dir, tx).unwrap();
        });
    });
    Ok(())
}

fn invoke(version: &LinkVersionSync) {
    let result: Result<i64, Box<dyn Error>> = version.invoke("main", ());
    match result {
        Ok(ret) => {
            println!("ret: {}", ret);
        }
        Err(e) => {
            println!("Error: {}", e);
        }
    }
}

fn run(dir: &Path, version_tx: mpsc::Sender<LinkVersionSync>) -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let mut e = Runner::create(&context, OptimizationLevel::None, 0)?;
    let (tx, rx) = std::sync::mpsc::channel();
    // Automatically select the best implementation for your platform.
    // You can also access each implementation directly e.g. INotifyWatcher.
    let mut watcher = RecommendedWatcher::new(tx, Config::default())?;
    let mut load_paths = vec![];

    let linker_path = "./target/debug/liblink.so";
    load_paths.push(Path::new(linker_path).into());
    //load_paths.push(Path::new("libsigsegv.so").into());

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        load_paths.push(path);
    }
    e.load_paths(&load_paths)?;

    match e.link() {
        Ok(version) => {
            version_tx.send(LinkVersionSync::new(version)).unwrap();
        }
        Err(e) => {
            println!("Error: {}", e);
        }
    }

    // Add a path to be watched. All files and directories at that path and
    // below will be monitored for changes.
    watcher.watch(dir, RecursiveMode::Recursive)?;

    for res in rx {
        match res {
            Ok(notify::Event {
                ref paths,
                ref kind,
                ..
            }) => {
                if kind == &Access(AccessKind::Close(AccessMode::Write)) {
                    e.load_paths(&paths)?;
                    match e.link() {
                        Ok(version) => {
                            version_tx.send(LinkVersionSync::new(version)).unwrap();
                        }
                        Err(e) => {
                            println!("Error: {}", e);
                        }
                    }
                }
            }
            Err(e) => println!("watch error: {:?}", e),
        };
    }
    Ok(())
}
