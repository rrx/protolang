use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{BasicType, BasicTypeEnum, PointerType};
use inkwell::values::{
    BasicValue, BasicValueEnum, CallableValue, FunctionValue, InstructionOpcode, IntValue,
};
use inkwell::{AddressSpace, IntPredicate};

use codegen_ir::util::fmap;
use codegen_ir::{
    hir::{self, *},
    *,
};
use std::error::Error;

#[derive(Debug)]
pub enum MapValue<'a> {
    FunctionDeclaration(FunctionValue<'a>),
    Value(BasicValueEnum<'a>),
    AstFragment(hir::Ast),
}

use MapValue::*;

pub type DefinitionMap<'a> = std::collections::HashMap<DefinitionId, MapValue<'a>>;

pub fn convert_function_type<'g>(context: &'g Context, f: &hir::FunctionType) -> PointerType<'g> {
    let parameters = fmap(&f.parameters, |param| convert_type(context, param).into());
    let ret = convert_type(context, &f.return_type);
    ret.fn_type(&parameters, false)
        .ptr_type(AddressSpace::Generic)
}

pub fn convert_type<'g>(context: &'g Context, typ: &hir::Type) -> BasicTypeEnum<'g> {
    match typ {
        hir::Type::Primitive(p) => match p {
            PrimitiveType::Integer(kind) => context
                .custom_width_int_type(integer_bit_count(*kind))
                .into(),
            PrimitiveType::Float(FloatKind::F32) => context.f32_type().into(),
            PrimitiveType::Float(FloatKind::F64) => context.f64_type().into(),
            PrimitiveType::Char => context.i8_type().into(),
            PrimitiveType::Boolean => context.bool_type().into(),
            PrimitiveType::Unit => context.bool_type().into(),
            PrimitiveType::Pointer => context.i8_type().ptr_type(AddressSpace::Generic).into(),
        },
        hir::Type::Function(f) => convert_function_type(context, f).into(),
        hir::Type::Tuple(tuple) => {
            let fields = fmap(tuple, |typ| convert_type(context, typ));
            context.struct_type(&fields, true).into()
        }
    }
}

fn ptr_size() -> usize {
    std::mem::size_of::<*const i8>()
}

fn undef_value<'a>(typ: BasicTypeEnum<'a>) -> BasicValueEnum<'a> {
    match typ {
        BasicTypeEnum::ArrayType(array) => array.get_undef().into(),
        BasicTypeEnum::FloatType(float) => float.get_undef().into(),
        BasicTypeEnum::IntType(int) => int.get_undef().into(),
        BasicTypeEnum::PointerType(pointer) => pointer.get_undef().into(),
        BasicTypeEnum::StructType(tuple) => tuple.get_undef().into(),
        BasicTypeEnum::VectorType(vector) => vector.get_undef().into(),
    }
}

/// Returns the size in bits of this integer.
pub fn integer_bit_count(int_kind: hir::IntegerKind) -> u32 {
    use hir::IntegerKind::*;
    match int_kind {
        I8 | U8 => 8,
        I16 | U16 => 16,
        I32 | U32 => 32,
        I64 | U64 => 64,
        Isz | Usz => ptr_size() as u32 * 8,
    }
}

fn lookup_var<'a>(var: &Variable, defmap: &DefinitionMap<'a>) -> BasicValueEnum<'a> {
    let value = defmap.get(&var.definition_id).expect("Variable not found");
    match value {
        FunctionDeclaration(fn_val) => fn_val.as_global_value().as_pointer_value().into(),
        Value(v) => *v,
        _ => unreachable!("{:?}", (&var, value)),
    }
}

fn lookup_function_declaration_by_id<'a>(
    definition_id: &DefinitionId,
    defmap: &DefinitionMap<'a>,
) -> Option<FunctionValue<'a>> {
    let r = defmap.get(&definition_id);
    match r {
        Some(value) => match value {
            FunctionDeclaration(fn_val) => Some(*fn_val),
            _ => unimplemented!(),
        },
        None => unreachable!("Definition not found: {}", definition_id),
    }
}

fn lookup_function_declaration<'a>(
    ast: &Ast,
    defmap: &DefinitionMap<'a>,
) -> Option<FunctionValue<'a>> {
    match ast {
        Ast::Variable(Variable {
            definition_id,
            ref name,
        }) => lookup_function_declaration_by_id(definition_id, defmap),
        _ => unimplemented!(),
    }
}

fn scan<'a>(ast: &Ast, context: &'a Context, module: &Module<'a>, defmap: &mut DefinitionMap<'a>) {
    for def in codegen_ir::scan::scan_definitions(ast) {
        let ast: Ast = def.clone().into();
        let name = def.get_name();

        match &*def.expr {
            Ast::Lambda(lambda) => {
                let raw_function_type = convert_function_type(context, &lambda.typ)
                    .get_element_type()
                    .into_function_type();

                let mut linkage = Linkage::Internal;
                if lambda.typ.export {
                    linkage = Linkage::External;
                }

                let fn_val = module.add_function(&name, raw_function_type, Some(linkage));
                defmap.insert(def.variable, FunctionDeclaration(fn_val));
            }
            _ => unimplemented!(),
        }
    }
}

struct ModuleGenerator<'a> {
    context: &'a Context,
    module: Module<'a>,
    builder: Builder<'a>,
    current_definition: Vec<Definition>,
    current_function: Vec<FunctionValue<'a>>,
    current_args: Vec<Ast>,
}

impl<'a> ModuleGenerator<'a> {
    fn new(context: &'a Context, name: &str) -> Self {
        let module = context.create_module(name);
        Self {
            context,
            module,
            builder: context.create_builder(),
            current_definition: vec![],
            current_function: vec![],
            current_args: vec![],
        }
    }

    fn scan(&mut self, ast: &Ast, defmap: &mut DefinitionMap<'a>) {
        for def in codegen_ir::scan::scan_definitions(ast) {
            //let ast: Ast = def.clone().into();
            let name = def.get_name();

            match &*def.expr {
                Ast::Lambda(lambda) => {
                    let raw_function_type = convert_function_type(self.context, &lambda.typ)
                        .get_element_type()
                        .into_function_type();

                    let mut linkage = Linkage::Internal;
                    if lambda.typ.export {
                        linkage = Linkage::External;
                    }

                    let fn_val = self
                        .module
                        .add_function(&name, raw_function_type, Some(linkage));
                    defmap.insert(def.variable, FunctionDeclaration(fn_val));
                }
                _ => unimplemented!(),
            }
        }
    }

    fn push_function(&mut self, f: FunctionValue<'a>) {
        self.current_function.push(f);
    }

    fn pop_function(&mut self) -> Option<FunctionValue<'a>> {
        self.current_function.pop()
    }

    fn get_current_function(&self) -> Option<FunctionValue<'a>> {
        self.current_function.last().cloned()
    }

    fn print(&self) {
        self.module.print_to_stderr()
    }

    fn add_int(&mut self, a: IntValue<'a>, b: IntValue<'a>) -> BasicValueEnum<'a> {
        self.builder
            .build_int_add(a, b, "add")
            .as_basic_value_enum()
    }

    fn sub_int(&mut self, a: IntValue<'a>, b: IntValue<'a>) -> BasicValueEnum<'a> {
        self.builder
            .build_int_sub(a, b, "sub")
            .as_basic_value_enum()
    }

    fn eq_int(&mut self, a: IntValue<'a>, b: IntValue<'a>) -> BasicValueEnum<'a> {
        self.builder
            .build_int_compare(IntPredicate::EQ, a, b, "eq")
            .as_basic_value_enum()
    }

    fn unit_value(&self) -> BasicValueEnum<'a> {
        // TODO: compile () to void, mainly higher-order functions, struct/tuple
        // indexing, and pattern matching need to be addressed for this.
        let i1 = self.context.bool_type();
        i1.const_int(0, false).into()
    }

    /// Return the inkwell function we're currently inserting into
    pub fn current_function(&self) -> FunctionValue<'a> {
        self.current_block().get_parent().unwrap()
    }

    /// Return the llvm block we're currently inserting into
    pub fn current_block(&self) -> BasicBlock<'a> {
        self.builder.get_insert_block().unwrap()
    }

    /// Does the given llvm instruction terminate its BasicBlock?
    /// This currently only checks for cases that can actually occur
    /// while codegening an arbitrary Ast node.
    fn current_instruction_is_block_terminator(&self) -> bool {
        let instruction = self.current_block().get_last_instruction();
        matches!(
            instruction.map(|instruction| instruction.get_opcode()),
            Some(InstructionOpcode::Return | InstructionOpcode::Unreachable)
        )
    }

    pub fn integer_value(&mut self, value: u64, kind: hir::IntegerKind) -> BasicValueEnum<'a> {
        let bits = integer_bit_count(kind);
        let unsigned = kind.is_unsigned_integer();
        self.context
            .custom_width_int_type(bits)
            .const_int(value, unsigned)
            .as_basic_value_enum()
    }

    fn char_value(&self, value: u64) -> BasicValueEnum<'a> {
        self.context.i8_type().const_int(value, true).into()
    }

    fn bool_value(&self, value: bool) -> BasicValueEnum<'a> {
        self.context
            .bool_type()
            .const_int(value as u64, true)
            .into()
    }

    fn float_value(&self, value: f64, kind: FloatKind) -> BasicValueEnum<'a> {
        match kind {
            FloatKind::F32 => self.context.f32_type().const_float(value).into(),
            FloatKind::F64 => self.context.f64_type().const_float(value).into(),
        }
    }

    /// Perform codegen for a string literal. This will create a global
    /// value for the string itself
    fn cstring_value(&mut self, contents: &str) -> BasicValueEnum<'a> {
        let literal = self.context.const_string(contents.as_bytes(), true);

        let global = self
            .module
            .add_global(literal.get_type(), None, "string_literal");

        global.set_initializer(&literal);

        let value = global.as_pointer_value();

        let cstring_type = self.context.i8_type().ptr_type(AddressSpace::Generic);

        let cast = self
            .builder
            .build_pointer_cast(value, cstring_type, "string_cast");

        cast.as_basic_value_enum()
    }
}

fn codegen_if<'a>(
    gen: &mut ModuleGenerator<'a>,
    v: &If,
    defmap: &mut DefinitionMap<'a>,
) -> BasicValueEnum<'a> {
    let condition = codegen(gen, &v.condition, defmap);

    let current_function = gen.current_function(); //.unwrap();
    let then_block = gen.context.append_basic_block(current_function, "then");
    let end_block = gen.context.append_basic_block(current_function, "end_if");

    if let Some(otherwise) = &v.otherwise {
        // Setup conditional jump
        let else_block = gen.context.append_basic_block(current_function, "else");
        gen.builder
            .build_conditional_branch(condition.into_int_value(), then_block, else_block);

        gen.builder.position_at_end(then_block);
        let (if_type, then_option) = codegen_branch(gen, &v.then, end_block, defmap);

        gen.builder.position_at_end(else_block);
        let (_, else_option) = codegen_branch(gen, otherwise, end_block, defmap);

        // Create phi at the end of the if beforehand
        gen.builder.position_at_end(end_block);

        // Some of the branches may have terminated early. We need to check each case to
        // determine which we should add to the phi or if we should even create a phi at all.
        match (then_option, else_option) {
            (Some((then_value, then_branch)), Some((else_value, else_branch))) => {
                let phi = gen.builder.build_phi(then_value.get_type(), "if_result");
                phi.add_incoming(&[(&then_value, then_branch), (&else_value, else_branch)]);
                phi.as_basic_value()
            }
            (Some((then_value, _)), None) => then_value,
            (None, Some((else_value, _))) => else_value,
            (None, None) => {
                gen.builder.build_unreachable();

                // Block is unreachable but we still need to return an undef value.
                // If we return None the compiler would crash while compiling
                // `2 + if true return "uh" else return "oh"`
                undef_value(if_type)
            }
        }
    } else {
        gen.builder
            .build_conditional_branch(condition.into_int_value(), then_block, end_block);

        gen.builder.position_at_end(then_block);
        codegen_branch(gen, &v.then, end_block, defmap);

        gen.builder.position_at_end(end_block);
        gen.unit_value()
    }
}

fn codegen_int<'a>(
    gen: &mut ModuleGenerator<'a>,
    ast: &Ast,
    defmap: &mut DefinitionMap<'a>,
) -> IntValue<'a> {
    codegen(gen, ast, defmap).into_int_value()
}

fn codegen_builtin<'a>(
    gen: &mut ModuleGenerator<'a>,
    builtin: &Builtin,
    defmap: &mut DefinitionMap<'a>,
) -> BasicValueEnum<'a> {
    let current_function = gen.current_function();
    let always_inline = Attribute::get_named_enum_kind_id("alwaysinline");
    assert_ne!(always_inline, 0);
    let attribute = gen.context.create_enum_attribute(always_inline, 1);
    current_function.add_attribute(AttributeLoc::Function, attribute);

    //let mut int = |ast: &Ast| gen.codegen(ast, defmap).into_int_value();

    match builtin {
        Builtin::AddInt(a, b) => {
            let a = codegen_int(gen, a, defmap);
            let b = codegen_int(gen, b, defmap);
            gen.add_int(a, b)
        }
        Builtin::SubInt(a, b) => {
            let a = codegen_int(gen, a, defmap);
            let b = codegen_int(gen, b, defmap);
            gen.sub_int(a, b)
        }
        Builtin::EqInt(a, b) => {
            let a = codegen_int(gen, a, defmap);
            let b = codegen_int(gen, b, defmap);
            gen.eq_int(a, b)
        }
        _ => unimplemented!("{:?}", builtin),
    }
}

fn codegen_literal<'a>(gen: &mut ModuleGenerator<'a>, ast: &Literal) -> BasicValueEnum<'a> {
    match ast {
        hir::Literal::Char(c) => gen.char_value(*c as u64),
        hir::Literal::Bool(b) => gen.bool_value(*b),
        hir::Literal::Float(f, kind) => gen.float_value(f64::from_bits(*f), *kind),
        hir::Literal::Integer(i, kind) => gen.integer_value(*i, *kind),
        hir::Literal::CString(s) => gen.cstring_value(s),
        hir::Literal::Unit => gen.unit_value(),
    }
}

fn codegen_return<'a>(
    gen: &mut ModuleGenerator<'a>,
    v: &Return,
    defmap: &mut DefinitionMap<'a>,
) -> BasicValueEnum<'a> {
    let value = codegen(gen, &v.expression, defmap);
    gen.builder.build_return(Some(&value));
    value
}

fn codegen_call<'a>(
    gen: &mut ModuleGenerator<'a>,
    v: &FunctionCall,
    defmap: &mut DefinitionMap<'a>,
) -> BasicValueEnum<'a> {
    let function = codegen(gen, &v.function, defmap).into_pointer_value();
    let args = fmap(&v.args, |arg| codegen(gen, arg, defmap).into());

    let function = CallableValue::try_from(function).unwrap();
    gen.builder
        .build_call(function, &args, "")
        .try_as_basic_value()
        .left()
        .unwrap()
}

fn codegen_definition<'a>(
    gen: &mut ModuleGenerator<'a>,
    def: &hir::Definition,
    defmap: &mut DefinitionMap<'a>,
) -> BasicValueEnum<'a> {
    let name = match &def.name {
        Some(name) => name.clone(),
        None => format!("v{}", &def.variable),
    };

    gen.current_definition.push(def.clone());

    //gen.current_function_info = Some(gen.variable);
    //generator.current_definition_name = gen.name.clone();
    let value = codegen(gen, &def.expr, defmap);
    defmap.insert(def.variable, Value(value));

    gen.unit_value()
}

fn codegen_extern<'a>(
    gen: &mut ModuleGenerator<'a>,
    v: &hir::Extern,
    defmap: &mut DefinitionMap<'a>,
) -> BasicValueEnum<'a> {
    let name = &v.name;
    let llvm_type = convert_type(&gen.context, &v.typ);

    if matches!(&v.typ, hir::Type::Function(_)) {
        let function_type = llvm_type
            .into_pointer_type()
            .get_element_type()
            .into_function_type();

        gen.module
            .add_function(name, function_type, Some(Linkage::External))
            .as_global_value()
            .as_basic_value_enum()
    } else {
        gen.module
            .add_global(llvm_type, None, name)
            .as_basic_value_enum()
    }
}

/// It is an error in llvm to insert a block terminator (like a br) after
/// the block has already ended from another block terminator (like a return).
///
/// Since returns can happen within a branch, this function should be used to
/// check that the branch hasn't yet terminated before inserting a br after
/// a then/else branch, pattern match, or looping construct.
fn codegen_branch<'a>(
    gen: &mut ModuleGenerator<'a>,
    branch: &hir::Ast,
    end_block: BasicBlock<'a>,
    defmap: &mut DefinitionMap<'a>,
) -> (
    BasicTypeEnum<'a>,
    Option<(BasicValueEnum<'a>, BasicBlock<'a>)>,
) {
    let branch_value = codegen(gen, branch, defmap);

    if gen.current_instruction_is_block_terminator() {
        (branch_value.get_type(), None)
    } else {
        let branch_block = gen.current_block();
        gen.builder.build_unconditional_branch(end_block);
        (branch_value.get_type(), Some((branch_value, branch_block)))
    }
}

fn codegen_lambda<'a>(
    gen: &mut ModuleGenerator<'a>,
    lambda: &Lambda,
    defmap: &mut DefinitionMap<'a>,
) -> BasicValueEnum<'a> {
    //let caller_block = generator.current_block();
    let def = gen.current_definition.pop().unwrap();

    let fn_val =
        lookup_function_declaration_by_id(&def.variable, defmap).expect("Missing declaration");
    //let name = def.name.unwrap_or("lambda".into());
    //let name = gen.current_definition._name.take().unwrap_or_else(|| "lambda".into());

    // start writing a new function
    let entry = gen.context.append_basic_block(fn_val, "entry");
    gen.builder.position_at_end(entry);

    for (i, parameter) in lambda.args.iter().enumerate() {
        let value = expect_opt!(
            fn_val.get_nth_param(i as u32),
            "Could not get parameter {} of function {}",
            i,
            lambda
        );
        defmap.insert(parameter.definition_id, Value(value));
    }

    let return_value = codegen(gen, &lambda.body, defmap);

    gen.builder.build_return(Some(&return_value));
    //generator.builder.position_at_end(caller_block);
    //
    let function_pointer = fn_val.as_global_value().as_pointer_value().into();

    function_pointer
}

fn codegen<'a>(
    gen: &mut ModuleGenerator<'a>,
    ast: &Ast,
    defmap: &mut DefinitionMap<'a>,
) -> BasicValueEnum<'a> {
    match ast {
        Ast::Variable(var) => lookup_var(var, defmap),
        Ast::Sequence(Sequence { statements }) => {
            assert!(!statements.is_empty());

            for statement in statements.iter().take(statements.len() - 1) {
                codegen(gen, statement, defmap);
            }

            codegen(gen, statements.last().unwrap(), defmap)
        }
        Ast::If(v) => codegen_if(gen, v, defmap),
        Ast::Builtin(builtin) => codegen_builtin(gen, builtin, defmap),
        Ast::Literal(literal) => codegen_literal(gen, literal),
        Ast::Return(ret) => codegen_return(gen, ret, defmap),
        Ast::FunctionCall(ret) => codegen_call(gen, ret, defmap),
        Ast::Lambda(lambda) => codegen_lambda(gen, lambda, defmap),
        Ast::Definition(def) => codegen_definition(gen, def, defmap),

        Ast::Extern(v) => codegen_extern(gen, v, defmap),
        _ => unimplemented!("{:?}", ast),
    }
}

pub fn generate<'a>(
    context: &'a Context,
    name: &str,
    ast: &Ast,
    defmap: &mut DefinitionMap<'a>,
) -> Result<Module<'a>, Box<dyn Error>> {
    let mut gen = ModuleGenerator::new(&context, name);
    gen.scan(ast, defmap);
    let v = codegen(&mut gen, &ast, defmap);
    println!("v: {:?}", v);
    println!("def: {:?}", defmap);
    gen.print();
    Ok(gen.module)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Executor;
    use codegen_ir::testing::*;
    use inkwell::OptimizationLevel;

    #[test]
    fn test_fib() {
        let mut defs = Definitions::new();
        let ast = gen_fib(&mut defs);
        println!("AST: {}", &ast.to_ron());

        let context = Context::create();
        let mut defmap = DefinitionMap::default();

        let module = generate(&context, "test", &ast, &mut defmap).unwrap();

        let mut exec = Executor::new(OptimizationLevel::None, 0);
        exec.add(&module);
        let ret = exec.run::<i64>().unwrap();
        println!("ret: {:?}", ret);
        assert_eq!(ret, 55);
    }

    #[test]
    fn test_selfref() {
        let mut defs = Definitions::new();
        let ast = gen_self_reference(&mut defs);
        println!("AST: {}", &ast.to_ron());

        let context = Context::create();
        let mut defmap = DefinitionMap::default();
        let module = generate(&context, "test", &ast, &mut defmap).unwrap();
    }

    #[test]
    fn codegen_extern() {
        let mut defs = Definitions::new();
        let x1_module = gen_x1_module(&mut defs);
        let x1_main = gen_x1_main(&mut defs);

        let context = Context::create();
        let mut exec = Executor::new(OptimizationLevel::None, 0);
        let module = exec.compile("test", &x1_module, &context).unwrap();
        exec.add(&module).unwrap();
        let module = exec.compile("main", &x1_main, &context).unwrap();
        exec.add(&module).unwrap();

        let ret = exec.run::<i64>().unwrap();
        println!("ret: {:?}", ret);
        assert_eq!(11, ret);
    }

    #[test]
    fn hotreload() {
    }
}
