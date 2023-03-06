use codegen_ir::hir;
use codegen_llvm::{Context, Executor, FileType, OptimizationLevel};
use frontend::syntax::AstModule;
use frontend::syntax::Dialect;
use lang3::{AstBuilder, Environment};
use link::*;
use std::error::Error;
use std::path::{Path, PathBuf};

pub struct Runner<'a> {
    context: &'a Context,
    linker: DynamicLink,
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
            linker: DynamicLink::new(),
            execute: Executor::create(optimization_level, size_level)?,
        })
    }

    pub fn debug(&self) {
        self.linker.debug();
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

        log::debug!("asm {}", std::str::from_utf8(asm_buf.as_slice()).unwrap());

        self.linker.add_obj_buf(name, obj_buf.as_slice())
    }

    pub fn load_paths(&mut self, paths: &Vec<PathBuf>) -> Result<(), Box<dyn Error>> {
        for path in paths {
            self.load_path(path)?;
        }
        Ok(())
    }

    pub fn load_path(&mut self, path: &Path) -> Result<(), Box<dyn Error>> {
        log::debug!("load: {}", &path.to_string_lossy());
        let stem = path.file_stem().unwrap().to_string_lossy();
        if let Some(ext) = path.extension() {
            match ext.to_string_lossy().as_ref() {
                "py" => {
                    self.compile_lang3(stem.as_ref(), &path)?;
                }
                "so" => {
                    log::debug!("loading object file {}", &path.to_string_lossy());
                    let _ = self.linker.add_library(stem.as_ref(), &path)?;
                }
                "o" => {
                    log::debug!("loading object file {}", &path.to_string_lossy());
                    let _ = self.linker.add_obj_file(stem.as_ref(), &path)?;
                }
                _ => {
                    log::debug!("skipping {}", &path.to_string_lossy());
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
pub fn parse_lang3<'a>(path: &Path) -> Result<hir::Ast, Box<dyn Error>> {
    let dialect = Dialect::Extended;
    let module = AstModule::parse_file(&path, &dialect)?;
    let mut builder = AstBuilder::default();
    let ast = module.lower(&mut builder)?;
    let env = Environment::default();
    let (ast, _env, _subst) = builder.resolve(&ast, env).unwrap();
    let hir = builder.lower(&ast).unwrap();
    Ok(hir)
}
