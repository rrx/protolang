//use std::borrow::Borrow;
use std::collections::HashMap;
use std::io::Write;
//use std::iter::Peekable;
//use std::ops::DerefMut;
use std::path::PathBuf;
//use std::str::Chars;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, FloatValue, FunctionValue, PointerValue,
};
use inkwell::{FloatPredicate, OptimizationLevel};

use crate::ast::{Expr, Operator};
//use crate::eval::Environment;
//use crate::parser::parse_file;
use crate::program::Program;
use crate::tokens::Tok;

/// Defines the prototype (name and parameters) of a function.
#[derive(Debug)]
pub struct Prototype {
    pub name: String,
    pub args: Vec<String>,
    pub is_op: bool,
    pub prec: usize,
}

/// Defines a user-defined or external function.
#[derive(Debug)]
pub struct Function {
    pub prototype: Prototype,
    pub body: Option<Expr>,
    pub is_anon: bool,
}

/// Defines the `Expr` compiler.
pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub module: &'a Module<'ctx>,
    pub function: &'a Function,

    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    /// Gets a defined function given its name.
    #[inline]
    fn get_function(&self, name: &str) -> Option<FunctionValue<'ctx>> {
        self.module.get_function(name)
    }

    /// Returns the `FunctionValue` representing the function being compiled.
    #[inline]
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }

    /// Creates a new stack allocation instruction in the entry block of the function.
    fn create_entry_block_alloca(&self, name: &str) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();

        let entry = self.fn_value().get_first_basic_block().unwrap();

        match entry.get_first_instruction() {
            Some(first_instr) => builder.position_before(&first_instr),
            None => builder.position_at_end(entry),
        }

        builder.build_alloca(self.context.f64_type(), name)
    }

    /// Compiles the specified `Expr` into an LLVM `FloatValue`.
    fn compile_expr(&mut self, expr: &Expr) -> Result<FloatValue<'ctx>, &'static str> {
        //println!("expr: {:?}", expr);
        match expr {
            Expr::Program(exprs) => {
                let mut results = vec![];
                for e in exprs {
                    let v = self.compile_expr(&e)?;
                    results.push(v);
                }
                if results.len() == 0 {
                    Err("Empty program")
                } else {
                    Ok(*results.last().unwrap())
                }
            }

            Expr::Block(exprs) => {
                let mut results = vec![];
                for e in exprs {
                    let v = self.compile_expr(&e)?;
                    results.push(v);
                }
                if results.len() == 0 {
                    Ok(self.context.f64_type().const_float(0.0))
                } else {
                    Ok(*results.last().unwrap())
                }
            }

            Expr::Literal(Tok::FloatLiteral(nb)) => Ok(self.context.f64_type().const_float(*nb)),
            //Expr::Literal(Tok::IntLiteral(nb)) => Ok(self.context.i64_type().const_int(*nb, false)),
            Expr::Ident(ident) => match self.variables.get(ident.name.as_str()) {
                Some(var) => Ok(self
                    .builder
                    .build_load(*var, ident.name.as_str())
                    .into_float_value()),
                None => Err("Could not find a matching variable."),
            },

            /*
            Expr::VarIn { ref variables, ref body } => {
                let mut old_bindings = Vec::new();

                for &(ref var_name, ref initializer) in variables {
                    let var_name = var_name.as_str();

                    let initial_val = match *initializer {
                        Some(ref init) => self.compile_expr(init)?,
                        None => self.context.f64_type().const_float(0.)
                    };

                    let alloca = self.create_entry_block_alloca(var_name);

                    self.builder.build_store(alloca, initial_val);

                    if let Some(old_binding) = self.variables.remove(var_name) {
                        old_bindings.push(old_binding);
                    }

                    self.variables.insert(var_name.to_string(), alloca);
                }

                let body = self.compile_expr(body)?;

                for binding in old_bindings {
                    self.variables.insert(binding.get_name().to_str().unwrap().to_string(), binding);
                }

                Ok(body)
            },
            */
            Expr::Binary(op, left, right) => {
                if op.value == Operator::Declare {
                    // handle declaration
                    let var_name = match &left.value {
                        Expr::Ident(i) => &i.name,
                        _ => {
                            return Err("Expected variable as left-hand operator of assignment.");
                        }
                    };

                    let alloca = self.create_entry_block_alloca(var_name);

                    if let Ok(var_val) = self.compile_expr(right) {
                        self.builder.build_store(alloca, var_val);
                        self.variables.insert(var_name.into(), alloca);
                        Ok(var_val)
                    } else {
                        Err("Unable to compile RHS")
                    }
                } else if op.value == Operator::Assign {
                    // handle assignment
                    let var_name = match &left.value {
                        Expr::Ident(i) => &i.name,
                        _ => {
                            return Err("Expected variable as left-hand operator of assignment.");
                        }
                    };

                    let var_val = self.compile_expr(right)?;
                    let var = self
                        .variables
                        .get(var_name.as_str())
                        .ok_or("Undefined variable.")?;

                    self.builder.build_store(*var, var_val);

                    Ok(var_val)
                } else {
                    let lhs = self.compile_expr(left)?;
                    let rhs = self.compile_expr(right)?;

                    match &op.value {
                        Operator::Plus => Ok(self.builder.build_float_add(lhs, rhs, "tmpadd")),
                        Operator::Minus => Ok(self.builder.build_float_sub(lhs, rhs, "tmpsub")),
                        Operator::Multiply => Ok(self.builder.build_float_mul(lhs, rhs, "tmpmul")),
                        Operator::Divide => Ok(self.builder.build_float_div(lhs, rhs, "tmpdiv")),
                        Operator::LessThan => Ok({
                            let cmp = self.builder.build_float_compare(
                                FloatPredicate::ULT,
                                lhs,
                                rhs,
                                "tmpcmp",
                            );

                            self.builder.build_unsigned_int_to_float(
                                cmp,
                                self.context.f64_type(),
                                "tmpbool",
                            )
                        }),
                        Operator::GreaterThan => Ok({
                            let cmp = self.builder.build_float_compare(
                                FloatPredicate::ULT,
                                rhs,
                                lhs,
                                "tmpcmp",
                            );

                            self.builder.build_unsigned_int_to_float(
                                cmp,
                                self.context.f64_type(),
                                "tmpbool",
                            )
                        }),
                        /*
                        custom => {
                            let mut name = String::from("binary");

                            name.push(custom);

                            match self.get_function(name.as_str()) {
                                Some(fun) => {
                                    match self.builder.build_call(fun, &[lhs.into(), rhs.into()], "tmpbin").try_as_basic_value().left() {
                                        Some(value) => Ok(value.into_float_value()),
                                        None => Err("Invalid call produced.")
                                    }
                                },

                                None => Err("Undefined binary operator.")
                            }
                        }
                        */
                        _ => unimplemented!(),
                    }
                }
            }

            Expr::Apply(f, args) => match &f.value {
                Expr::Ident(ident) => {
                    let fn_name = &ident.name;
                    match self.module.get_function(fn_name.as_str()) {
                        Some(fun) => {
                            let mut compiled_args = Vec::with_capacity(args.len());

                            for arg in args {
                                compiled_args.push(self.compile_expr(arg)?);
                            }

                            let argsv: Vec<BasicMetadataValueEnum> = compiled_args
                                .iter()
                                .by_ref()
                                .map(|&val| val.into())
                                .collect();

                            match self
                                .builder
                                .build_call(fun, argsv.as_slice(), "tmp")
                                .try_as_basic_value()
                                .left()
                            {
                                Some(value) => Ok(value.into_float_value()),
                                None => Err("Invalid call produced."),
                            }
                        }
                        None => {
                            println!("unknown function: {}", fn_name);
                            Err("Unknown function.")
                        }
                    }
                }
                _ => unimplemented!(),
            },

            Expr::Ternary(op, cond, consequence, alternative) => {
                if &op.value == &Operator::Conditional {
                    let parent = self.fn_value();
                    let zero_const = self.context.f64_type().const_float(0.0);

                    // create condition by comparing without 0.0 and returning an int
                    let cond = self.compile_expr(cond)?;
                    let cond = self.builder.build_float_compare(
                        FloatPredicate::ONE,
                        cond,
                        zero_const,
                        "ifcond",
                    );

                    // build branch
                    let then_bb = self.context.append_basic_block(parent, "then");
                    let else_bb = self.context.append_basic_block(parent, "else");
                    let cont_bb = self.context.append_basic_block(parent, "ifcont");

                    self.builder
                        .build_conditional_branch(cond, then_bb, else_bb);

                    // build then block
                    self.builder.position_at_end(then_bb);
                    let then_val = self.compile_expr(consequence)?;
                    self.builder.build_unconditional_branch(cont_bb);

                    let then_bb = self.builder.get_insert_block().unwrap();

                    // build else block
                    self.builder.position_at_end(else_bb);
                    let else_val = self.compile_expr(alternative)?;
                    self.builder.build_unconditional_branch(cont_bb);

                    let else_bb = self.builder.get_insert_block().unwrap();

                    // emit merge block
                    self.builder.position_at_end(cont_bb);

                    let phi = self.builder.build_phi(self.context.f64_type(), "iftmp");

                    phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                    Ok(phi.as_basic_value().into_float_value())
                } else {
                    println!("unimplemented op: {:?}", op);
                    unimplemented!()
                }
            }

            /*
                Expr::For { ref var_name, ref start, ref end, ref step, ref body } => {
                    let parent = self.fn_value();

                    let start_alloca = self.create_entry_block_alloca(var_name);
                    let start = self.compile_expr(start)?;

                    self.builder.build_store(start_alloca, start);

                    // go from current block to loop block
                    let loop_bb = self.context.append_basic_block(parent, "loop");

                    self.builder.build_unconditional_branch(loop_bb);
                    self.builder.position_at_end(loop_bb);

                    let old_val = self.variables.remove(var_name.as_str());

                    self.variables.insert(var_name.to_owned(), start_alloca);

                    // emit body
                    self.compile_expr(body)?;

                    // emit step
                    let step = match *step {
                        Some(ref step) => self.compile_expr(step)?,
                        None => self.context.f64_type().const_float(1.0)
                    };

                    // compile end condition
                    let end_cond = self.compile_expr(end)?;

                    let curr_var = self.builder.build_load(start_alloca, var_name);
                    let next_var = self.builder.build_float_add(curr_var.into_float_value(), step, "nextvar");

                    self.builder.build_store(start_alloca, next_var);

                    let end_cond = self.builder.build_float_compare(FloatPredicate::ONE, end_cond, self.context.f64_type().const_float(0.0), "loopcond");
                    let after_bb = self.context.append_basic_block(parent, "afterloop");

                    self.builder.build_conditional_branch(end_cond, loop_bb, after_bb);
                    self.builder.position_at_end(after_bb);

                    self.variables.remove(var_name);

                    if let Some(val) = old_val {
                        self.variables.insert(var_name.to_owned(), val);
                    }

                    Ok(self.context.f64_type().const_float(0.0))
                }
            */
            _ => {
                //println!("unimplemented {:?}", expr);
                Err("unimplemented") //&format!("Unimplemented: {:?}", expr).to_string())
                                     //unimplemented!()
            }
        }
    }

    /// Compiles the specified `Prototype` into an extern LLVM `FunctionValue`.
    fn compile_prototype(&self, proto: &Prototype) -> Result<FunctionValue<'ctx>, &'static str> {
        let ret_type = self.context.f64_type();
        let args_types = std::iter::repeat(ret_type)
            .take(proto.args.len())
            .map(|f| f.into())
            .collect::<Vec<BasicMetadataTypeEnum>>();
        let args_types = args_types.as_slice();

        let fn_type = self.context.f64_type().fn_type(args_types, false);
        let fn_val = self.module.add_function(proto.name.as_str(), fn_type, None); //Some(Linkage::Private));

        // set arguments names
        for (i, arg) in fn_val.get_param_iter().enumerate() {
            arg.into_float_value().set_name(proto.args[i].as_str());
        }

        // finally return built prototype
        Ok(fn_val)
    }

    /// Compiles the specified `Function` into an LLVM `FunctionValue`.
    fn compile_fn(&mut self) -> Result<FunctionValue<'ctx>, &'static str> {
        let proto = &self.function.prototype;
        let function = self.compile_prototype(proto)?;

        // got external function, returning only compiled prototype
        if self.function.body.is_none() {
            return Ok(function);
        }

        let entry = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(entry);

        // update fn field
        self.fn_value_opt = Some(function);

        // build variables map
        self.variables.reserve(proto.args.len());

        for (i, arg) in function.get_param_iter().enumerate() {
            let arg_name = proto.args[i].as_str();
            let alloca = self.create_entry_block_alloca(arg_name);

            self.builder.build_store(alloca, arg);

            self.variables.insert(proto.args[i].clone(), alloca);
        }

        // compile body
        let body = self.compile_expr(self.function.body.as_ref().unwrap())?;

        self.builder.build_return(Some(&body));

        Ok(function)
    }

    /// Compiles the specified `Function` in the given `Context` and using the specified `Builder`, `PassManager`, and `Module`.
    pub fn compile(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        module: &'a Module<'ctx>,
        function: &Function,
    ) -> Result<FunctionValue<'ctx>, &'static str> {
        let mut compiler = Compiler {
            context: context,
            builder: builder,
            module: module,
            function: function,
            fn_value_opt: None,
            variables: HashMap::new(),
        };

        compiler.compile_fn()
    }
}

// macro used to print & flush without printing a new line
macro_rules! print_flush {
    ( $( $x:expr ),* ) => {
        print!( $($x, )* );

        std::io::stdout().flush().expect("Could not flush to standard output.");
    };
}

#[no_mangle]
pub extern "C" fn putchard(x: f64) -> f64 {
    print_flush!("{}", x as u8 as char);
    x
}

#[no_mangle]
pub extern "C" fn printd(x: f64) -> f64 {
    println!("{}", x);
    x
}

// Adding the functions above to a global array,
// so Rust compiler won't remove them.
#[used]
static EXTERNAL_FNS: [extern "C" fn(f64) -> f64; 2] = [putchard, printd];

fn build_extern<'a, 'ctx>(
    context: &'ctx Context,
    module: &'a Module<'ctx>,
    name: &str,
) -> FunctionValue<'ctx> {
    let proto = Prototype {
        name: name.into(),
        args: vec!["one".into()],
        is_op: false,
        prec: 0,
    };
    let ret_type = context.f64_type();

    let args_types = std::iter::repeat(ret_type)
        .take(proto.args.len())
        .map(|f| f.into())
        .collect::<Vec<BasicMetadataTypeEnum>>();
    let args_types = args_types.as_slice();

    let fn_type = context.f64_type().fn_type(args_types, false);
    let fn_val = module.add_function(proto.name.as_str(), fn_type, None);

    // set arguments names
    for (i, arg) in fn_val.get_param_iter().enumerate() {
        arg.into_float_value().set_name(proto.args[i].as_str());
    }

    // finally return built prototype
    fn_val
}

fn handle_declare(expr: &Expr) -> Result<Option<Function>, &'static str> {
    match expr {
        Expr::Binary(op, left, right) => {
            if &op.value == &Operator::Declare {
                // handle declaration
                let var_name = match &left.value {
                    Expr::Ident(i) => &i.name,
                    _ => {
                        return Err("Expected variable as left-hand operator of assignment.");
                    }
                };

                match &right.value {
                    Expr::Lambda(f) => {
                        let params = f
                            .params
                            .value
                            .iter()
                            .map(|param| match param.value.try_ident() {
                                Some(ident) => ident.name,
                                None => panic!("Invalid Param"),
                            })
                            .collect::<Vec<_>>();

                        let prototype = Prototype {
                            name: var_name.into(),
                            args: params,
                            is_op: false,
                            prec: 0,
                        };
                        let f2 = Function {
                            prototype,
                            body: Some(f.expr.value.clone()),
                            is_anon: false,
                        };
                        //println!("asdf:{:?}", &f2);
                        return Ok(Some(f2));
                    }
                    _ => unimplemented!(),
                }
            }
        }
        _ => unimplemented!(),
    }
    Ok(None)
}

fn handle_program(expr: &Expr) -> Result<Vec<Function>, &'static str> {
    let mut out = vec![];
    match expr {
        Expr::Program(exprs) => {
            for e in exprs {
                match handle_declare(e)? {
                    Some(f) => {
                        out.push(f);
                    }
                    None => (),
                }
            }
        }
        _ => unimplemented!(),
    }
    Ok(out)
}

pub fn compile_and_execute(filenames: Vec<String>) -> anyhow::Result<()> {
    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();
    let mut program = Program::new();

    // Create FPM
    let fpm = PassManager::create(&module);
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.initialize();

    // make sure they aren't removed from here
    printd(0.0);
    putchard(0.0);

    // initialize JIT execution engine
    let ee = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .unwrap();

    // create a module for each filename, and add it to the EE
    for filename in filenames {
        let mut output = PathBuf::from("target");
        output.push(filename.clone());
        output.set_extension("ll");

        let module_name = output.file_stem().unwrap().to_str().unwrap();
        let module = context.create_module(module_name);

        // add runtime functions
        build_extern(&context, &module, "putchard");
        build_extern(&context, &module, "printd");

        let expr = program.parse_file(&filename)?;
        let functions = handle_program(&expr).unwrap();
        let fns = functions
            .iter()
            .map(|f| Compiler::compile(&context, &builder, &module, &f).unwrap())
            .collect::<Vec<_>>();

        println!("BEFORE");
        module.print_to_stderr();

        if let Err(e) = module.print_to_file(output) {
            println!("Unable to save file");
        }

        for f in fns {
            if f.verify(true) {
                fpm.run_on(&f);
            } else {
                println!("Verify failed");
                return Ok(());
            }
        }

        println!("AFTER");
        module.print_to_stderr();
        let _ = ee.add_module(&module);
    }

    // get the main function and execute it
    let maybe_fn = unsafe { ee.get_function::<unsafe extern "C" fn() -> f64>("main") };

    match maybe_fn {
        Ok(f) => unsafe {
            println!("=> {}", f.call());
        },
        Err(err) => {
            println!("!> Error during execution: {:?}", err);
        }
    }

    Ok(())
}
