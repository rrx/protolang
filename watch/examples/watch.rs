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

pub struct Runner<'a> {
    context: &'a Context,
    linker: LinkBuilder,
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
            linker: LinkBuilder::new(),
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

        self.linker.add_buf(name, obj_buf.as_slice())
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
                    let _ = self.linker.add(stem.as_ref(), &path)?;
                }
                _ => {
                    println!("skipping {}", &path.to_string_lossy());
                }
            }
        }
        Ok(())
    }

    pub fn link(&mut self) -> Result<LinkCollection, Box<dyn Error>> {
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

fn main() -> Result<(), Box<dyn Error>> {
    let (tx, rx) = std::sync::mpsc::channel();
    // Automatically select the best implementation for your platform.
    // You can also access each implementation directly e.g. INotifyWatcher.
    let mut watcher = RecommendedWatcher::new(tx, Config::default())?;

    let context = Context::create();

    // path hardwired for now, eventually this will be configurable
    let dir = Path::new("./tmp");

    let mut e = Runner::create(&context, OptimizationLevel::None, 0)?;

    let mut load_paths = vec![];
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        load_paths.push(path);
    }
    e.load_paths(&load_paths)?;

    match e.link() {
        Ok(collection) => {
            let result: Result<i64, Box<_>> = collection.invoke("call_live", (10,));
            match result {
                Ok(ret) => {
                    println!("ret: {}", ret);
                }
                Err(e) => {
                    println!("Error: {}", e);
                }
            }
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
                        Ok(collection) => {
                            let result: Result<i64, Box<_>> = collection.invoke("call_live", (10,));
                            match result {
                                Ok(ret) => {
                                    println!("ret: {}", ret);
                                }
                                Err(e) => {
                                    println!("Error: {}", e);
                                }
                            }
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
