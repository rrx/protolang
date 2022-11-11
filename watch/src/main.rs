use notify::{Watcher, RecommendedWatcher, RecursiveMode, Config};
use std::path::Path;
use inkwell::context::Context;
use inkwell::module::{Module};
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{InitializationConfig, FileType, Target, TargetMachine};
use inkwell::execution_engine::ExecutionEngine;
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::error::Error;
use codegen_llvm::Executor;
use std::fs;
use codegen_ir::hir;
use frontend::syntax::AstModule;
use frontend::syntax::Dialect;
use lang3::{AstBuilder, Environment};

fn compile<'a>(path: &Path) -> Result<hir::Ast, Box<dyn Error>> {
    let dialect = Dialect::Extended;
    let module = AstModule::parse_file(&path, &dialect)?;
    //module.print();

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

    let dir = Path::new("./tmp");


    let mut count = 0;
    let mut e = Executor::new(OptimizationLevel::None, 0);

    let mut h = HashMap::new();
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.extension().unwrap().to_string_lossy() == "py" { 
            let hir = compile(&path)?;
            let name = "test";
            //let name  = format!("m{}", count),
            let module = e.compile(name, &hir, &context).unwrap();
            //let module = e.compile(&format!("m{}", count), &hir, &context).unwrap();
            count += 1;
            //println!("{}", module.to_string());
            h.insert(path, module);
        }
    }

    // first execution
    for (k, v) in &h {
        e.add(&v).unwrap();
        let ret = e.run::<i64>().unwrap();
        println!("ret: {}", ret);
        e.remove(&v).unwrap();

    }

    // Add a path to be watched. All files and directories at that path and
    // below will be monitored for changes.
    watcher.watch(dir, RecursiveMode::Recursive)?;
    use notify::EventKind::*;
    use notify::event::AccessKind;
    use notify::event::AccessMode;

    for res in rx {
        let mut has_changed = false;
        let mut e = Executor::new(OptimizationLevel::None, 0);
        let mut changed_paths = vec![];
        match res {
            Ok(notify::Event { ref paths, ref kind, .. }) => {
                if kind == &Access(AccessKind::Close(AccessMode::Write)) {
                    //e.remove(&module).unwrap();
                    for path in paths {
                        match path.extension() {
                            Some(ext) => if ext.to_string_lossy() == "py" { 
                                println!("changed: {:?}", (path, kind));
                                changed_paths.push(path);
                                //let hir = compile(&path)?;
                                //let module = e.compile(&format!("m{}", count), &hir, &context).unwrap();
                                //count += 1;
                                has_changed = true;
                                //if h.contains_key(path) {
                                    //e.remove(h.get(path).unwrap())?;
                                //}
                                //e.add(&module).unwrap();
                                //h.insert(path.clone(), module);
                                //let ret = e.run::<i64>().unwrap();
                                //println!("ret: {}", ret)
                            }
                            None => ()
                        }
                    }
                }
            }
            Err(e) => println!("watch error: {:?}", e),
        };

        //h.clear();

        for path in &changed_paths {
            let hir = compile(&path)?;
            let name = "test";
            //let name  = format!("m{}", count),
            let module = e.compile(name, &hir, &context).unwrap();
            count += 1;
            has_changed = true;
            //if h.contains_key(path) {
            //e.remove(h.get(path).unwrap())?;
            //}
            //e.add(&module).unwrap();
            h.insert(path.to_path_buf(), module);
        }

        if has_changed {
            for (k, v) in &h {
                e.add(&v).unwrap();
                let ret = e.run::<i64>().unwrap();
                println!("ret: {}", ret);
                e.remove(&v).unwrap();
            }
        }

    }
    Ok(())
}
