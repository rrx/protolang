//! llvm/codegen.rs - Defines the LLVM baekend for ante's codegen pass.
//! Currently, there are no other backends, but in the future the codegen
//! pass may have the choice between several backends for e.g. faster debug builds.
//!
//! The codegen pass follows the lifetime inference pass, and is the final pass of
//! the compiler. The goal of this pass is to produce native code that is executable
//! by a computer. The majority of this pass is implemented via the CodeGen trait
//! which walks the Ast with a Generator for context. This walk starts in the main
//! function and lazily codegens each Definition that is used so that only what is
//! used is actually compiled into the resulting binary. Once this walk is finished
//! the resulting inkwell::Module is optimized then linked with gcc.
use crate::hir::{self, Ast, DefinitionId, FloatKind};
use crate::util::fmap;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{BasicType, BasicTypeEnum, PointerType};
use inkwell::values::{AggregateValue, BasicValue, BasicValueEnum, CallableValue, FunctionValue, InstructionOpcode};
use inkwell::AddressSpace;

use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;

use super::builtin;
use std::rc::Rc;
use crate::visit;

#[derive(Debug)]
pub enum DefinitionValue<'context> {
    Compiled(BasicValueEnum<'context>),
    Value(Rc<Ast>)
}

pub type DefinitionsMap<'context> = HashMap<DefinitionId, DefinitionValue<'context>>;

struct DefinitionVisitor {}
impl<'context> visit::Visitor<DefinitionsMap<'context>> for DefinitionVisitor {
    fn definition(&mut self, d: &hir::Definition, defmap: &mut DefinitionsMap<'context>) -> visit::VResult {
        //Ast::Definition(d)
        let ast: Ast = d.clone().into();
        println!("def: {}", ast.to_ron());
        defmap.insert(d.variable, DefinitionValue::Value(Rc::new(ast)));
        Ok(())
    }
}

pub fn generate_definitions(defmap: &mut DefinitionsMap, ast: &Ast) {
    let mut f = DefinitionVisitor {};
    visit::visit(&ast, &mut f, defmap).unwrap();
}

/// The (code) Generator provides all the needed context for generating LLVM IR
/// while walking the Ast.
#[derive(Debug)]
pub struct Generator<'context> {
    pub context: &'context Context,
    pub module: Module<'context>,
    pub builder: Builder<'context>,

    /// Cache of already compiled definitions
    pub definitions: DefinitionsMap<'context>,

    /// Contains all the definition ids that should be automatically dereferenced because they're
    /// either stored locally in an alloca or in a global.
    pub auto_derefs: HashSet<DefinitionId>,

    pub current_function_info: Option<DefinitionId>,
    pub current_definition_name: Option<String>,
}

impl<'context> DefinitionValue<'context> {
    fn get_or_codegen(&self, generator: &mut Generator<'context>) -> BasicValueEnum<'context> {
        match self {
            Self::Compiled(value) => *value,
            Self::Value(astrc) => {
                astrc.as_ref().codegen(generator)
            }
        }
    }
}

impl<'g> Generator<'g> {
    fn codegen_main(&mut self, ast: &Ast) {
        let i32_type = self.context.i32_type();
        let main_type = i32_type.fn_type(&[], false);
        let function = self.module.add_function("main", main_type, Some(Linkage::External));
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        ast.codegen(self);

        let success = i32_type.const_int(0, true);
        self.build_return(success.into());
    }

    /// Return the inkwell function we're currently inserting into
    pub fn current_function(&self) -> FunctionValue<'g> {
        self.current_block().get_parent().unwrap()
    }

    /// Return the llvm block we're currently inserting into
    pub fn current_block(&self) -> BasicBlock<'g> {
        self.builder.get_insert_block().unwrap()
    }

    /// Append a new BasicBlock into the current function and set it
    /// as the current insert point.
    pub fn insert_into_new_block(&self, block_name: &str) -> BasicBlock<'g> {
        let current_function = self.current_function();
        let block = self.context.append_basic_block(current_function, block_name);
        self.builder.position_at_end(block);
        block
    }

    /// Create a new function with the given name and type and set
    /// its entry block as the current insert point. Returns the
    /// pointer to the function.
    fn function(&mut self, name: &str, typ: &hir::FunctionType) -> (FunctionValue<'g>, BasicValueEnum<'g>) {
        let raw_function_type = Self::convert_function_type(self.context, typ).get_element_type().into_function_type();

        let mut linkage = Linkage::Internal;
        if typ.export {
            linkage = Linkage::External;
        }

        let function = self.module.add_function(name, raw_function_type, Some(linkage));

        let function_pointer = function.as_global_value().as_pointer_value().into();

        if let Some(id) = self.current_function_info.take() {
            self.definitions.insert(id, DefinitionValue::Compiled(function_pointer));
        }

        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);
        (function, function_pointer)
    }

    fn unit_value(&self) -> BasicValueEnum<'g> {
        // TODO: compile () to void, mainly higher-order functions, struct/tuple
        // indexing, and pattern matching need to be addressed for this.
        let i1 = self.context.bool_type();
        i1.const_int(0, false).into()
    }

    pub fn convert_function_type(context: &'g Context, f: &hir::FunctionType) -> PointerType<'g> {
        let parameters = fmap(&f.parameters, |param| Self::convert_type(context, param).into());
        let ret = Self::convert_type(context, &f.return_type);
        ret.fn_type(&parameters, false).ptr_type(AddressSpace::Generic)
    }

    pub fn convert_type(context: &'g Context, typ: &hir::Type) -> BasicTypeEnum<'g> {
        match typ {
            hir::Type::Primitive(p) => {
                use hir::PrimitiveType;
                match p {
                    PrimitiveType::Integer(kind) => {
                        context.custom_width_int_type(Self::integer_bit_count(*kind)).into()
                    },
                    PrimitiveType::Float(FloatKind::F32) => context.f32_type().into(),
                    PrimitiveType::Float(FloatKind::F64) => context.f64_type().into(),
                    PrimitiveType::Char => context.i8_type().into(),
                    PrimitiveType::Boolean => context.bool_type().into(),
                    PrimitiveType::Unit => context.bool_type().into(),
                    PrimitiveType::Pointer => context.i8_type().ptr_type(AddressSpace::Generic).into(),
                }
            },
            hir::Type::Function(f) => Self::convert_function_type(context, f).into(),
            hir::Type::Tuple(tuple) => {
                let fields = fmap(tuple, |typ| Self::convert_type(context, typ));
                context.struct_type(&fields, true).into()
            },
        }
    }

    fn ptr_size() -> usize {
        std::mem::size_of::<*const i8>()
    }

    /// Returns the size in bits of this integer.
    pub fn integer_bit_count(int_kind: hir::IntegerKind) -> u32 {
        use hir::IntegerKind::*;
        match int_kind {
            I8 | U8 => 8,
            I16 | U16 => 16,
            I32 | U32 => 32,
            I64 | U64 => 64,
            Isz | Usz => Self::ptr_size() as u32 * 8,
        }
    }

    /// Returns whether this type is unsigned (and therefore whether it should be sign-extended).
    ///
    /// Will bind the integer to an i32 if this integer is an IntegerKind::Inferred
    /// that has not already been bound to a concrete type.
    fn is_unsigned_integer(&mut self, int_kind: hir::IntegerKind) -> bool {
        use hir::IntegerKind::*;
        match int_kind {
            I8 | I16 | I32 | I64 | Isz => false,
            U8 | U16 | U32 | U64 | Usz => true,
        }
    }

    pub fn integer_value(&mut self, value: u64, kind: hir::IntegerKind) -> BasicValueEnum<'g> {
        let bits = Self::integer_bit_count(kind);
        let unsigned = self.is_unsigned_integer(kind);
        self.context.custom_width_int_type(bits).const_int(value, unsigned).as_basic_value_enum()
    }

    fn char_value(&self, value: u64) -> BasicValueEnum<'g> {
        self.context.i8_type().const_int(value, true).into()
    }

    fn bool_value(&self, value: bool) -> BasicValueEnum<'g> {
        self.context.bool_type().const_int(value as u64, true).into()
    }

    fn float_value(&self, value: f64, kind: FloatKind) -> BasicValueEnum<'g> {
        match kind {
            FloatKind::F32 => self.context.f32_type().const_float(value).into(),
            FloatKind::F64 => self.context.f64_type().const_float(value).into(),
        }
    }

    /// Perform codegen for a string literal. This will create a global
    /// value for the string itself
    fn cstring_value(&mut self, contents: &str) -> BasicValueEnum<'g> {
        let literal = self.context.const_string(contents.as_bytes(), true);

        let global = self.module.add_global(literal.get_type(), None, "string_literal");

        global.set_initializer(&literal);

        let value = global.as_pointer_value();

        let cstring_type = self.context.i8_type().ptr_type(AddressSpace::Generic);

        let cast = self.builder.build_pointer_cast(value, cstring_type, "string_cast");

        cast.as_basic_value_enum()
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

    fn build_return(&mut self, return_value: BasicValueEnum<'g>) {
        if !self.current_instruction_is_block_terminator() {
            self.builder.build_return(Some(&return_value));
        }
    }

    /// It is an error in llvm to insert a block terminator (like a br) after
    /// the block has already ended from another block terminator (like a return).
    ///
    /// Since returns can happen within a branch, this function should be used to
    /// check that the branch hasn't yet terminated before inserting a br after
    /// a then/else branch, pattern match, or looping construct.
    pub fn codegen_branch(
        &mut self, branch: &hir::Ast, end_block: BasicBlock<'g>,
    ) -> (BasicTypeEnum<'g>, Option<(BasicValueEnum<'g>, BasicBlock<'g>)>) {
        let branch_value = branch.codegen(self);

        if self.current_instruction_is_block_terminator() {
            (branch_value.get_type(), None)
        } else {
            let branch_block = self.current_block();
            self.builder.build_unconditional_branch(end_block);
            (branch_value.get_type(), Some((branch_value, branch_block)))
        }
    }

    fn reinterpret_cast(&mut self, value: BasicValueEnum<'g>, target_type: BasicTypeEnum<'g>) -> BasicValueEnum<'g> {
        let source_type = value.get_type();
        let alloca = self.builder.build_alloca(source_type, "alloca");
        self.builder.build_store(alloca, value);

        let target_type = target_type.ptr_type(AddressSpace::Generic);
        let cast = self.builder.build_pointer_cast(alloca, target_type, "cast");
        self.builder.build_load(cast, "union_cast")
    }

    fn tuple(
        &mut self, elements: Vec<BasicValueEnum<'g>>, element_types: Vec<BasicTypeEnum<'g>>,
    ) -> BasicValueEnum<'g> {
        let tuple_type = self.context.struct_type(&element_types, true);

        // LLVM wants the const elements to be included in the struct literal itself.
        // Attempting to do build_insert_value would a const value will return the struct as-is
        // without mutating the existing struct.
        let const_elements =
            fmap(
                &elements,
                |element| {
                    if Self::is_const(*element) {
                        *element
                    } else {
                        Self::undef_value(element.get_type())
                    }
                },
            );

        let mut tuple = tuple_type.const_named_struct(&const_elements).as_aggregate_value_enum();

        // Now insert all the non-const values
        for (i, element) in elements.into_iter().enumerate() {
            if !Self::is_const(element) {
                tuple = self.builder.build_insert_value(tuple, element, i as u32, "insert").unwrap();
            }
        }

        tuple.as_basic_value_enum()
    }

    fn is_const(value: BasicValueEnum<'g>) -> bool {
        match value {
            BasicValueEnum::ArrayValue(array) => array.is_const(),
            BasicValueEnum::FloatValue(float) => float.is_const(),
            BasicValueEnum::IntValue(int) => int.is_const(),
            BasicValueEnum::PointerValue(pointer) => pointer.is_const(),
            BasicValueEnum::StructValue(_) => false,
            BasicValueEnum::VectorValue(vector) => vector.is_const(),
        }
    }

    fn undef_value(typ: BasicTypeEnum<'g>) -> BasicValueEnum<'g> {
        match typ {
            BasicTypeEnum::ArrayType(array) => array.get_undef().into(),
            BasicTypeEnum::FloatType(float) => float.get_undef().into(),
            BasicTypeEnum::IntType(int) => int.get_undef().into(),
            BasicTypeEnum::PointerType(pointer) => pointer.get_undef().into(),
            BasicTypeEnum::StructType(tuple) => tuple.get_undef().into(),
            BasicTypeEnum::VectorType(vector) => vector.get_undef().into(),
        }
    }

    /// Creates a GEP instruction and Load which emulate a single Extract instruction but
    /// delays the Load as long as possible to make assigning to this as an l-value easier later on.
    fn gep_at_index(&mut self, load: BasicValueEnum<'g>, field_index: u32, field_name: &str) -> BasicValueEnum<'g> {
        let instruction = load.as_instruction_value().unwrap();
        assert_eq!(instruction.get_opcode(), InstructionOpcode::Load);

        let pointer = instruction.get_operand(0).unwrap().left().unwrap().into_pointer_value();

        let gep = self.builder.build_struct_gep(pointer, field_index, field_name).unwrap();
        self.builder.build_load(gep, field_name)
    }
}

pub trait CodeGen<'g> {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g>;
}

impl<'g> CodeGen<'g> for hir::Ast {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g> {
        dispatch_on_hir!(self, CodeGen::codegen, generator)
    }
}

impl<'g> CodeGen<'g> for hir::Literal {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g> {
        match self {
            hir::Literal::Char(c) => generator.char_value(*c as u64),
            hir::Literal::Bool(b) => generator.bool_value(*b),
            hir::Literal::Float(f, kind) => generator.float_value(f64::from_bits(*f), *kind),
            hir::Literal::Integer(i, kind) => generator.integer_value(*i, *kind),
            hir::Literal::CString(s) => generator.cstring_value(s),
            hir::Literal::Unit => generator.unit_value(),
        }
    }
}

impl<'g> CodeGen<'g> for hir::Variable {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g> {
        // check if the definition that the variable refers to has been compiled or not
        let mut value = match generator.definitions.get(&self.definition_id) {
            Some(DefinitionValue::Compiled(d)) => *d,
            _ => unreachable!("Definition for {} not yet compiled", self.definition_id),
        };

        if generator.auto_derefs.contains(&self.definition_id) {
            value = generator.builder.build_load(value.into_pointer_value(), "");
        }

        value
    }
}

impl<'g> CodeGen<'g> for hir::Lambda {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g> {
        //let caller_block = generator.current_block();
        let name = generator.current_definition_name.take().unwrap_or_else(|| "lambda".into());

        let (function, function_value) = generator.function(&name, &self.typ);

        // Bind each parameter node to the nth parameter of `function`
        for (i, parameter) in self.args.iter().enumerate() {
            let value =
                expect_opt!(function.get_nth_param(i as u32), "Could not get parameter {} of function {}", i, self);
            generator.definitions.insert(parameter.definition_id, DefinitionValue::Compiled(value));
        }

        let return_value = self.body.codegen(generator);

        generator.build_return(return_value);
        //generator.builder.position_at_end(caller_block);

        function_value
    }
}

impl<'g> CodeGen<'g> for hir::FunctionCall {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g> {
        let function = self.function.codegen(generator).into_pointer_value();
        let args = fmap(&self.args, |arg| arg.codegen(generator).into());

        let function = CallableValue::try_from(function).unwrap();
        generator.builder.build_call(function, &args, "").try_as_basic_value().left().unwrap()
    }
}

fn should_auto_deref(definition: &hir::Definition) -> bool {
    if let hir::Ast::Extern(ext) = definition.expr.as_ref() {
        return !matches!(&ext.typ, hir::Type::Function(_));
    }

    false
}

impl<'g> CodeGen<'g> for hir::Definition {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g> {
        // Cannot use HashMap::entry here, generator is borrowed mutably in self.expr.codegen
        #[allow(clippy::map_entry)]
        if !generator.definitions.contains_key(&self.variable) {
            if should_auto_deref(self) {
                generator.auto_derefs.insert(self.variable);
            }

            generator.current_function_info = Some(self.variable);
            generator.current_definition_name = self.name.clone();
            let value = self.expr.codegen(generator);
            generator.definitions.insert(self.variable, DefinitionValue::Compiled(value));
        }

        generator.unit_value()
    }
}

impl<'g> CodeGen<'g> for hir::If {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g> {
        let condition = self.condition.codegen(generator);

        let current_function = generator.current_function();
        let then_block = generator.context.append_basic_block(current_function, "then");
        let end_block = generator.context.append_basic_block(current_function, "end_if");

        if let Some(otherwise) = &self.otherwise {
            // Setup conditional jump
            let else_block = generator.context.append_basic_block(current_function, "else");
            generator.builder.build_conditional_branch(condition.into_int_value(), then_block, else_block);

            generator.builder.position_at_end(then_block);
            let (if_type, then_option) = generator.codegen_branch(&self.then, end_block);

            generator.builder.position_at_end(else_block);
            let (_, else_option) = generator.codegen_branch(otherwise, end_block);

            // Create phi at the end of the if beforehand
            generator.builder.position_at_end(end_block);

            // Some of the branches may have terminated early. We need to check each case to
            // determine which we should add to the phi or if we should even create a phi at all.
            match (then_option, else_option) {
                (Some((then_value, then_branch)), Some((else_value, else_branch))) => {
                    let phi = generator.builder.build_phi(then_value.get_type(), "if_result");
                    phi.add_incoming(&[(&then_value, then_branch), (&else_value, else_branch)]);
                    phi.as_basic_value()
                },
                (Some((then_value, _)), None) => then_value,
                (None, Some((else_value, _))) => else_value,
                (None, None) => {
                    generator.builder.build_unreachable();

                    // Block is unreachable but we still need to return an undef value.
                    // If we return None the compiler would crash while compiling
                    // `2 + if true return "uh" else return "oh"`
                    Generator::undef_value(if_type)
                },
            }
        } else {
            generator.builder.build_conditional_branch(condition.into_int_value(), then_block, end_block);

            generator.builder.position_at_end(then_block);
            generator.codegen_branch(&self.then, end_block);

            generator.builder.position_at_end(end_block);
            generator.unit_value()
        }
    }
}

impl<'g> CodeGen<'g> for hir::Match {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g> {
        generator.codegen_tree(self)
    }
}

impl<'g> CodeGen<'g> for hir::Return {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g> {
        let value = self.expression.codegen(generator);
        generator.builder.build_return(Some(&value));
        value
    }
}

impl<'g> CodeGen<'g> for hir::Sequence {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g> {
        assert!(!self.statements.is_empty());

        for statement in self.statements.iter().take(self.statements.len() - 1) {
            statement.codegen(generator);
        }

        self.statements.last().unwrap().codegen(generator)
    }
}

impl<'g> CodeGen<'g> for hir::Extern {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g> {
        let name = &self.name;
        let llvm_type = Generator::convert_type(&generator.context, &self.typ);

        if matches!(&self.typ, hir::Type::Function(_)) {
            let function_type = llvm_type.into_pointer_type().get_element_type().into_function_type();

            generator
                .module
                .add_function(name, function_type, Some(Linkage::External))
                .as_global_value()
                .as_basic_value_enum()
        } else {
            generator.module.add_global(llvm_type, None, name).as_basic_value_enum()
        }
    }
}

impl<'g> CodeGen<'g> for hir::MemberAccess {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g> {
        let lhs = self.lhs.codegen(generator);
        let index = self.member_index;

        // If our lhs is a load from an alloca, create a GEP instead of extracting directly.
        // This will delay the load as long as possible which makes this easier to detect
        // as a valid l-value in hir::Assignment::codegen.
        match lhs.as_instruction_value().map(|instr| instr.get_opcode()) {
            Some(InstructionOpcode::Load) => generator.gep_at_index(lhs, index, ""),
            _ => {
                let collection = lhs.into_struct_value();
                generator.builder.build_extract_value(collection, index, "").unwrap()
            },
        }
    }
}

impl<'g> CodeGen<'g> for hir::Assignment {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g> {
        let lhs = self.lhs.codegen(generator);

        let lhs = match lhs.as_instruction_value() {
            Some(instruction) if instruction.get_opcode() == InstructionOpcode::Load => {
                instruction.get_operand(0).unwrap().left().unwrap().into_pointer_value()
            },
            // TODO: This can result in silent failures. Need better mutability semantics.
            _ => lhs.into_pointer_value(),
        };

        let rhs = self.rhs.codegen(generator);

        let rhs_ptr = rhs.get_type().ptr_type(AddressSpace::Generic);
        let lhs = generator.builder.build_pointer_cast(lhs, rhs_ptr, "bitcast");

        generator.builder.build_store(lhs, rhs);
        generator.unit_value()
    }
}

impl<'g> CodeGen<'g> for hir::Tuple {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g> {
        let (values, types) = self
            .fields
            .iter()
            .map(|field| {
                let value = field.codegen(generator);
                (value, value.get_type())
            })
            .unzip();

        generator.tuple(values, types)
    }
}

impl<'g> CodeGen<'g> for hir::ReinterpretCast {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g> {
        let value = self.lhs.codegen(generator);
        let target_type = Generator::convert_type(&generator.context, &self.target_type);
        generator.reinterpret_cast(value, target_type)
    }
}

impl<'g> CodeGen<'g> for hir::Builtin {
    fn codegen(&self, generator: &mut Generator<'g>) -> BasicValueEnum<'g> {
        builtin::call_builtin(self, generator)
    }
}
