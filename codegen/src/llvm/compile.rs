use crate::hir;
use crate::util;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::targets::{CodeModel, FileType, RelocMode, TargetTriple};
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::types::{BasicType, BasicTypeEnum, PointerType};
use inkwell::values::{AggregateValue, BasicValue, BasicValueEnum, CallableValue, FunctionValue, InstructionOpcode};
use inkwell::AddressSpace;
use inkwell::OptimizationLevel;

use super::{path_to_module_name, CodeGen, Generator};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug, Clone, PartialEq)]
pub enum EmitOption {
    ASM,
    BC,
    IR,
    OBJ,
}

#[derive(Debug)]
pub struct CompileArgs {
    /// Sets the current optimization level from 0 (no optimization) to 3 (aggressive optimization).
    /// Set to s or z to optimize for size.
    pub opt_level: char,

    // optionally link
    pub link: bool,

    // output result to stdout
    pub stdout: bool,
    // target triple
    pub target: Option<String>,

    // output directory
    pub output_dir: Option<String>,

    // output filename
    pub output_filename: Option<String>,

    // emit
    pub emit: Vec<EmitOption>,

    // verbose
    pub verbose: bool,
}

impl Default for CompileArgs {
    fn default() -> Self {
        Self {
            opt_level: '0',
            link: false,
            stdout: false,
            target: None,
            output_dir: None,
            output_filename: None,
            emit: vec![],
            verbose: false,
        }
    }
}

pub fn list_targets() -> Result<(), Box<dyn Error>> {
    let config = InitializationConfig::default();
    Target::initialize_all(&config); //.unwrap();
    eprintln!("Default: {:?}", TargetMachine::get_default_triple().as_str());
    eprintln!("Host: {}", TargetMachine::get_host_cpu_name().to_str()?);

    eprintln!("Registered Targets");
    let mut maybe_target = Target::get_first();
    loop {
        match maybe_target {
            Some(ref target) => {
                eprintln!("  {:16} - {}", target.get_name().to_str()?, target.get_description().to_str()?);
                maybe_target = target.get_next();
            },
            None => break,
        }
    }
    Ok(())
}

fn to_optimization_level(opt_level: char) -> OptimizationLevel {
    match opt_level {
        '1' => OptimizationLevel::Less,
        '2' => OptimizationLevel::Default,
        '3' => OptimizationLevel::Aggressive,
        _ => OptimizationLevel::None,
    }
}

fn to_size_level(optimization_argument: char) -> u32 {
    match optimization_argument {
        's' => 1,
        'z' => 2,
        _ => 0,
    }
}

pub fn compile(module_name: &String, ast: hir::Ast, args: &CompileArgs) -> Result<(), Box<dyn Error>> {
    list_targets();

    let context = Context::create();

    let module = context.create_module(&module_name);

    let target_triple = if let Some(target) = &args.target {
        TargetTriple::create(&target)
    } else {
        TargetMachine::get_default_triple()
    };

    eprintln!("target: {}", &target_triple);

    module.set_triple(&target_triple);
    let mut codegen = Generator {
        context: &context,
        module,
        builder: context.create_builder(),
        definitions: HashMap::new(),
        auto_derefs: HashSet::new(),
        current_function_info: None,
        current_definition_name: None,
    };

    ast.codegen(&mut codegen);

    codegen
        .module
        .verify()
        .map_err(|error| {
            codegen.module.print_to_stderr();
            eprintln!("{}", error);
        })
        .unwrap();

    // print to stdout
    if args.stdout {
        println!("{}", codegen.module.print_to_string().to_string());
    }

    let config = InitializationConfig::default();
    Target::initialize_native(&config).unwrap();
    let pass_manager_builder = PassManagerBuilder::create();

    let optimization_argument = args.opt_level;
    let optimization_level = to_optimization_level(optimization_argument);
    let size_level = to_size_level(optimization_argument);
    pass_manager_builder.set_optimization_level(optimization_level);
    pass_manager_builder.set_size_level(size_level);

    let pass_manager = PassManager::create(());
    pass_manager_builder.populate_module_pass_manager(&pass_manager);
    pass_manager.run_on(&codegen.module);

    // Do LTO optimizations afterward mosty for function inlining
    let link_time_optimizations = PassManager::create(());
    pass_manager_builder.populate_lto_pass_manager(&link_time_optimizations, false, true);
    link_time_optimizations.run_on(&codegen.module);

    codegen.optimize(args.opt_level);
    let binary_name = util::binary_name(&module_name);
    output(module_name.clone(), &binary_name, &target_triple, &codegen.module, &args);

    unsafe {
        let mut ee = codegen.module.create_jit_execution_engine(OptimizationLevel::None)?;
        let f = ee.get_function::<unsafe extern "C" fn() -> u32>("main")?;
        let ret = f.call();
        eprintln!("ret: {}", ret);
    }

    /*
    unsafe {
        let mut ie = codegen.module.create_interpreter_execution_engine()?;
        let f = ie.get_function::<unsafe extern "C" fn() -> u32>("main")?;
        let ret = f.call();
        eprintln!("ret: {}", ret);
    }
    */

    //let program_command = PathBuf::from("./".to_string() + &binary_name);
    //Command::new(&program_command).spawn().unwrap().wait().unwrap();
    Ok(())
}

/// Output the current module to a file and link with gcc.
pub fn output(
    module_name: String, binary_name: &str, target_triple: &TargetTriple, module: &Module, args: &CompileArgs,
) -> Result<(), Box<dyn Error>> {
    let target = Target::from_triple(target_triple).unwrap();
    let target_machine = target
        .create_target_machine(target_triple, "", "", OptimizationLevel::Aggressive, RelocMode::PIC, CodeModel::Default)
        .unwrap();

    let mut path = PathBuf::new();
    match &args.output_filename {
        Some(filename) => path.push(filename),
        None => {
            path.push(args.output_dir.clone().unwrap_or("target".into()));
            path.push(&module_name);
            path.set_extension("o");
        },
    }

    let directory = path.parent().unwrap();
    if !directory.exists() {
        std::fs::create_dir_all(directory)?;
    }

    let mut emit = vec![EmitOption::OBJ];
    if args.emit.len() > 0 {
        emit = args.emit.clone();
    }

    if emit.contains(&EmitOption::IR) {
        // write target independent llvm code to a .ll file
        let ll_path = path.with_extension("ll");
        eprintln!("output: {:?}", &ll_path);
        module.print_to_file(&ll_path)?;
    }

    if emit.contains(&EmitOption::BC) {
        // generate the bitcode to a .bc file
        let bc_path = path.with_extension("bc");
        eprintln!("output: {:?}", &bc_path);
        module.write_bitcode_to_path(&bc_path);
    }

    if emit.contains(&EmitOption::ASM) {
        // write target specific code to an object file
        path.set_extension("s");
        eprintln!("output: {:?}", &path);
        target_machine.write_to_file(module, FileType::Assembly, &path).unwrap();
    }

    if emit.contains(&EmitOption::OBJ) {
        // write target specific code as assembly
        path.set_extension("o");
        eprintln!("output: {:?}", &path);
        target_machine.set_asm_verbosity(args.verbose);
        target_machine.write_to_file(module, FileType::Object, &path).unwrap();
    }

    Ok(())
}
