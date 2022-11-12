use codegen_ir::hir;
use codegen_llvm::{Context, LiveLink, OptimizationLevel};
use frontend::syntax::AstModule;
use frontend::syntax::Dialect;
use lang3::{AstBuilder, Environment};
use notify::event::AccessKind;
use notify::event::AccessMode;
use notify::EventKind::*;
use notify::{Config, RecommendedWatcher, RecursiveMode, Watcher};
use std::error::Error;
use std::fs;
use std::path::Path;

// eventually we want to be able to handle multiple input types
// for now we only handle input from frontend, lowering to hir
fn compile<'a>(path: &Path) -> Result<hir::Ast, Box<dyn Error>> {
    let dialect = Dialect::Extended;
    let module = AstModule::parse_file(&path, &dialect)?;
    let mut builder = AstBuilder::default();
    let ast = module.lower(&mut builder)?;
    let env = Environment::default();
    let (ast, _env, _subst) = builder.resolve(&ast, env).unwrap();
    let hir = builder.lower(&ast).unwrap();
    Ok(hir)
}

fn main() -> Result<(), Box<dyn Error>> {
    let (tx, rx) = std::sync::mpsc::channel();
    // Automatically select the best implementation for your platform.
    // You can also access each implementation directly e.g. INotifyWatcher.
    let mut watcher = RecommendedWatcher::new(tx, Config::default())?;

    let context = Context::create();

    // path hardwired for now, eventually this will be configurable
    let dir = Path::new("./tmp");

    let mut e = LiveLink::create(&context, OptimizationLevel::None, 0)?;

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.extension().unwrap().to_string_lossy() == "py" {
            let hir = compile(&path)?;
            let name = "test";
            //let name  = format!("m{}", count),
            let _ = e.compile(name, &hir)?;
            let ret: u64 = e.invoke("asdf", (10,))?;
            println!("ret: {}", ret);
        }
    }

    // Add a path to be watched. All files and directories at that path and
    // below will be monitored for changes.
    watcher.watch(dir, RecursiveMode::Recursive)?;

    for res in rx {
        let mut changed_paths = vec![];
        match res {
            Ok(notify::Event {
                ref paths,
                ref kind,
                ..
            }) => {
                if kind == &Access(AccessKind::Close(AccessMode::Write)) {
                    for path in paths {
                        match path.extension() {
                            Some(ext) => {
                                if ext.to_string_lossy() == "py" {
                                    println!("changed: {:?}", (path, kind));
                                    changed_paths.push(path);
                                }
                            }
                            None => (),
                        }
                    }
                }
            }
            Err(e) => println!("watch error: {:?}", e),
        };

        for path in &changed_paths {
            let hir = compile(&path)?;
            let name = "test";
            //let name  = format!("m{}", count),
            let _ = e.compile(name, &hir)?;
            let ret: u64 = e.invoke("main", (100, 1))?;
            println!("ret: {}", ret);
        }
    }
    Ok(())
}
