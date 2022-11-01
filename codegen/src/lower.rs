use crate::{hir::*, llvm::generator::{CodeGen, Generator}};
use std::error::Error;
use inkwell::module::{Linkage, Module};
pub use inkwell::context::Context;
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::OptimizationLevel;

pub type ModuleMap<'a> = HashMap<String, Module<'a>>;

pub struct Definitions {
    next_id: usize
}
impl Definitions {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }
    pub fn new_definition(&mut self, name: &str, ast: Ast) -> Definition { 
        let d = Definition {
            variable: DefinitionId(self.next_id),
            name: Some(name.to_string()),
            expr: ast.into()
        };
        self.next_id += 1;
        d
    }

    pub fn named_variable(&mut self, name: &str) -> Variable {
        let v = Variable {
            definition: None,
            definition_id: DefinitionId(self.next_id),
            name: Some(name.to_string()),
        };
        self.next_id += 1;
        v
    }

    pub fn new_variable(&mut self) -> Variable {
        let v = Variable {
            definition: None,
            definition_id: DefinitionId(self.next_id),
            name: None,
        };
        self.next_id += 1;
        v
    }
}

pub fn add(a: Ast, b: Ast) -> Ast {
    Builtin::AddInt(a.into(), b.into()).into()
}

pub fn lt(a: Ast, b: Ast) -> Ast {
    Builtin::LessSigned(a.into(), b.into()).into()
}


pub struct Lower<'a> {
    modules: ModuleMap<'a>
}

impl<'a> Lower<'a> {
    pub fn new() -> Self {
        Self { modules: ModuleMap::new() }
    }
    pub fn module(&mut self, context: &'a Context, name: &str, ast: Ast) -> Result<(), Box<dyn Error>> {
        let mut module = context.create_module(name);

        let mut codegen = Generator {
            module,
            context: context,
            builder: context.create_builder(),
            definitions: HashMap::new(),
            auto_derefs: HashSet::new(),
            current_function_info: None,
            current_definition_name: None,
        };

        ast.codegen(&mut codegen);

        match codegen.module.verify() {
            Ok(_) => {
                codegen.module.print_to_stderr();
                self.modules.insert(name.to_string(), codegen.module);
                Ok(())
            }
            Err(error) => {
                codegen.module.print_to_stderr();
                //eprintln!("{}", &error);
                Err(error.into())
            }
        }
    }

    pub fn run(&self, context: &'a Context) -> Result<i64, Box<dyn Error>> {
        run_jit(context, &self.modules)
    }
}

pub fn run_jit<'a>(context: &'a Context, modules: &ModuleMap<'a>) -> Result<i64, Box<dyn Error>> {
    let config = InitializationConfig::default();
    Target::initialize_native(&config)?;
    eprintln!("Default: {:?}", TargetMachine::get_default_triple().as_str());
    eprintln!("Host: {}", TargetMachine::get_host_cpu_name().to_str()?);

    let module = context.create_module("__xmain__");

    let ee = module.create_jit_execution_engine(OptimizationLevel::None)?;
    for (name, module) in modules {
        ee.add_module(&module).unwrap();
    }

    unsafe {
        let f = ee.get_function::<unsafe extern "C" fn() -> i64>("main").unwrap();
        let ret = f.call();
        println!("ret: {}", ret);
        Ok(ret)
    }
}


