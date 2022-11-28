use codegen_ir::hir;
use codegen_llvm::{Context, Executor, FileType, OptimizationLevel};
use frontend::syntax::AstModule;
use frontend::syntax::Dialect;
use lang3::{AstBuilder, Environment};
use link::*;
use notify::event::AccessKind;
use notify::event::AccessMode;
use notify::EventKind::*;
use notify::{Config, RecommendedWatcher, RecursiveMode, Watcher};
use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::mpsc;
use std::{thread, time};

pub struct Runner<'a> {
    context: &'a Context,
    linker: Link,
    execute: Executor<'a>,
}
impl<'a> Runner<'a> {
    pub fn create(
        context: &'a Context,
        optimization_level: OptimizationLevel,
        size_level: u32,
    ) -> Result<Self, Box<dyn Error>> {
        Ok(Self {
            context,
            linker: Link::new(),
            execute: Executor::create(optimization_level, size_level)?,
        })
    }

    pub fn compile_lang3(&mut self, name: &str, path: &Path) -> Result<(), Box<dyn Error>> {
        let ast = parse_lang3(path)?;
        self.compile_ast(name, &ast)
    }

    pub fn compile_ast(&mut self, name: &str, ast: &hir::Ast) -> Result<(), Box<dyn Error>> {
        let module = self.execute.compile(name, ast, self.context)?;
        let asm_buf = self
            .execute
            .target_machine
            .write_to_memory_buffer(&module, FileType::Assembly)
            .unwrap();
        let obj_buf = self
            .execute
            .target_machine
            .write_to_memory_buffer(&module, FileType::Object)
            .unwrap();
        self.execute
            .target_machine
            .write_to_file(&module, FileType::Object, &Path::new("out.o"))
            .unwrap();

        println!("asm {}", std::str::from_utf8(asm_buf.as_slice()).unwrap());

        self.linker.add_obj_buf(name, obj_buf.as_slice())
    }

    pub fn load_paths(&mut self, paths: &Vec<PathBuf>) -> Result<(), Box<dyn Error>> {
        for path in paths {
            self.load_path(path)?;
        }
        Ok(())
    }

    pub fn load_path(&mut self, path: &Path) -> Result<(), Box<dyn Error>> {
        println!("load: {}", &path.to_string_lossy());
        let stem = path.file_stem().unwrap().to_string_lossy();
        if let Some(ext) = path.extension() {
            match ext.to_string_lossy().as_ref() {
                "py" => {
                    self.compile_lang3(stem.as_ref(), &path)?;
                }
                "so" => {
                    println!("loading object file {}", &path.to_string_lossy());
                    let _ = self.linker.add_library(stem.as_ref(), &path)?;
                }
                "o" => {
                    println!("loading object file {}", &path.to_string_lossy());
                    let _ = self.linker.add_obj_file(stem.as_ref(), &path)?;
                }
                _ => {
                    println!("skipping {}", &path.to_string_lossy());
                }
            }
        }
        Ok(())
    }

    pub fn link(&mut self) -> Result<LinkVersion, Box<dyn Error>> {
        self.linker.link()
    }
}

// eventually we want to be able to handle multiple input types
// for now we only handle input from frontend, lowering to hir
fn parse_lang3<'a>(path: &Path) -> Result<hir::Ast, Box<dyn Error>> {
    let dialect = Dialect::Extended;
    let module = AstModule::parse_file(&path, &dialect)?;
    let mut builder = AstBuilder::default();
    let ast = module.lower(&mut builder)?;
    let env = Environment::default();
    let (ast, _env, _subst) = builder.resolve(&ast, env).unwrap();
    let hir = builder.lower(&ast).unwrap();
    Ok(hir)
}

fn invoke(version: &LinkVersionSync) {
    let result: Result<i64, Box<_>> = version.0.as_ref().lock().unwrap().invoke("main", ());
    match result {
        Ok(ret) => {
            println!("ret: {}", ret);
        }
        Err(e) => {
            println!("Error: {}", e);
        }
    }
}

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
