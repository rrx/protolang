use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{CodeModel, FileType, RelocMode, TargetTriple};
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::OptimizationLevel;

use super::{CodeGen, Generator};
use std::error::Error;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, PartialEq)]
pub enum EmitOption {
    ASM,
    BC,
    IR,
    OBJ,
}

#[derive(Debug, Clone)]
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

pub struct Compiler {
    context: Context,
    args: CompileArgs,
}

impl Compiler {
    pub fn new(args: CompileArgs) -> Self {
        Self { context: Context::create(), args }
    }
}

pub fn list_targets() -> Result<(), Box<dyn Error>> {
    let config = InitializationConfig::default();
    Target::initialize_all(&config);
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

pub fn path_to_module_name(path: &Path) -> String {
    path.with_extension("").to_string_lossy().into()
}

pub fn to_optimization_level(opt_level: char) -> OptimizationLevel {
    match opt_level {
        '1' => OptimizationLevel::Less,
        '2' => OptimizationLevel::Default,
        '3' => OptimizationLevel::Aggressive,
        _ => OptimizationLevel::None,
    }
}

pub fn to_size_level(optimization_argument: char) -> u32 {
    match optimization_argument {
        's' => 1,
        'z' => 2,
        _ => 0,
    }
}

/// Output the current module to a file
pub fn output(
    module_name: String, target_triple: &TargetTriple, module: &Module, args: &CompileArgs,
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
