//! This module defines the High-level Intermediate Representation's AST.
//!
//! The goal of this Ast is to function as a simpler Ast for the backends
//! to consume. In comparison to the main Ast, this one:
//! - Has no reliance on the ModuleCache
//! - Has all generic types removed either through monomorphisation or boxing
//! - All trait function calls are replaced with references to the exact
//!   function to call statically (monomorphisation) or are passed in as
//!   arguments to calling functions (boxing).
pub mod constructors;
mod module;
mod printer;
mod types;
pub use constructors::*;
use data::env::*;

pub use module::ModuleBuilder;

use serde::Serialize;
pub use types::{FloatKind, FunctionType, IntegerKind, PrimitiveType, Type};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct DefinitionId(pub usize);

impl LayerKey for DefinitionId {}
impl LayerValue for Ast {}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub enum Literal {
    Integer(u64, IntegerKind),
    Float(u64, FloatKind),
    CString(String),
    Char(char),
    Bool(bool),
    Unit,
}

#[derive(Debug, Clone, Serialize)]
pub struct Variable {
    pub definition_id: DefinitionId,
    pub name: Option<String>,
}
impl Variable {
    pub fn get_name(&self) -> String {
        match &self.name {
            Some(name) => name.clone(),
            None => format!("v{}", &self.definition_id),
        }
    }
}

/*
impl From<DefinitionId> for Variable {
    fn from(definition_id: DefinitionId) -> Variable {
        Variable {
            definition_id,
            name: None,
        }
    }
}
*/

impl From<Definition> for Variable {
    fn from(definition: Definition) -> Variable {
        Variable {
            definition_id: definition.variable,
            name: definition.name,
        }
    }
}


/*
impl DefinitionId {
    pub fn to_variable(self) -> Ast {
        Ast::Variable(self.into())
    }
}
*/

/// \a b. expr
/// Function definitions are also desugared to a ast::Definition with a ast::Lambda as its body
#[derive(Debug, Clone, Serialize)]
pub struct Lambda {
    pub args: Vec<Variable>,
    pub body: Box<Ast>,
    pub typ: FunctionType,
}
impl Lambda {
    pub fn new(args: Vec<Variable>, body: Ast, typ: FunctionType) -> Self {
        Self {
            args,
            body: Box::new(body),
            typ: typ.clone(),
        }
    }
}

/// foo a b c
#[derive(Debug, Clone, Serialize)]
pub struct FunctionCall {
    pub function: Box<Ast>,
    pub args: Vec<Ast>,
    pub function_type: FunctionType,
}
impl FunctionCall {
    pub fn new(f: Ast, args: Vec<Ast>, typ: FunctionType) -> Self {
        // params should match the type, anything else isn't supported
        if args.len() != typ.parameters.len() {
            unimplemented!("Parameters must match the type signature")
        }

        match f {
            Ast::Extern(_) | Ast::Variable(_) => Self {
                function: Box::new(f.into()),
                args,
                function_type: typ,
            },
            _ => unimplemented!(),
        }
    }
}

/// Unlike ast::Definition, hir::Definition
/// is desugared of any patterns, its lhs must
/// be a single variable to simplify backends.
#[derive(Debug, Clone, Serialize)]
pub struct Definition {
    pub variable: DefinitionId,
    pub name: Option<String>,
    pub expr: Box<Ast>,
}
impl Definition {
    pub fn named(definition_id: DefinitionId, name: &str, expr: Ast) -> Self {
        Self {
            variable: definition_id,
            name: Some(name.to_string()),
            expr: expr.into(),
        }
    }

    pub fn unamed(definition_id: DefinitionId, expr: Ast) -> Self {
        Self {
            variable: definition_id,
            name: None,
            expr: expr.into(),
        }
    }

    /// define a variable
    pub fn variable(var: Variable, expr: Ast) -> Self {
        Self {
            variable: var.definition_id,
            name: var.name.clone(),
            expr: expr.into(),
        }
    }

    pub fn to_variable(&self) -> Variable {
        Variable {
            definition_id: self.variable,
            name: self.name.clone(),
        }
    }

    pub fn get_name(&self) -> String {
        match &self.name {
            Some(name) => name.clone(),
            None => format!("v{}", &self.variable),
        }
    }
}

/// if condition then expression else expression
#[derive(Debug, Clone, Serialize)]
pub struct If {
    pub condition: Box<Ast>,
    pub then: Box<Ast>,
    pub otherwise: Option<Box<Ast>>,
    pub result_type: Type,
}

#[derive(Debug, Clone, Serialize)]
pub struct Match {
    // Unlike ast::Match this only contains the parts of the
    // branch after the ->.
    pub branches: Vec<Ast>,
    pub decision_tree: DecisionTree,
    pub result_type: Type,
}

// This cannot be desugared into Ast::If due to the sharing
// of Leafs across separate branches. E.g. a match on:
// ```
// match foo
// | None, None -> ...
// | _ -> ...
// ```
// Compiles to the tree:
// ```
// Switch value1 {
//     Some -> Leaf(1)
//     None -> {
//         switch value2 {
//             Some -> Leaf(1)
//             None -> Leaf(0)
//         }
//     }
// }
// ```
// Where two different paths need to share the same leaf branch.
#[derive(Debug, Clone, Serialize)]
pub enum DecisionTree {
    Leaf(usize),
    Definition(Definition, Box<DecisionTree>),
    Switch {
        int_to_switch_on: Box<Ast>,
        cases: Vec<(u32, DecisionTree)>,
        else_case: Option<Box<DecisionTree>>,
    },
}

/// return expression
#[derive(Debug, Clone, Serialize)]
pub struct Return {
    pub expression: Box<Ast>,
}
impl Return {
    pub fn new(ast: Ast) -> Self {
        Self {
            expression: ast.into(),
        }
    }
}

/// statement1
/// statement2
/// ...
/// statementN
#[derive(Debug, Clone, Serialize)]
pub struct Sequence {
    pub statements: Vec<Ast>,
}
impl Sequence {
    pub fn new(statements: Vec<Ast>) -> Self {
        Self { statements }
    }
}

/// extern declaration
/// // or
/// extern
///     declaration1
///     declaration2
///     ...
///     declarationN
#[derive(Debug, Clone, Serialize)]
pub struct Extern {
    pub name: String,
    pub typ: Type,
}
impl Extern {
    pub fn new(name: String, typ: Type) -> Self {
        Self { name, typ }
    }
}

/// lhs := rhs
#[derive(Debug, Clone, Serialize)]
pub struct Assignment {
    pub lhs: Box<Ast>,
    pub rhs: Box<Ast>,
}

#[derive(Debug, Clone, Serialize)]
pub struct MemberAccess {
    pub lhs: Box<Ast>,
    pub member_index: u32,
}

#[derive(Debug, Clone, Serialize)]
pub struct Tuple {
    pub fields: Vec<Ast>,
}

/// Essentially the same as Builtin::Transmute.
/// Enum variants are padded with extra bytes
/// then lowered to this. lhs's type should be the same
/// size as the target type, though there may be
/// padding differences currently.
#[derive(Debug, Clone, Serialize)]
pub struct ReinterpretCast {
    pub lhs: Box<Ast>,
    pub target_type: Type,
}

#[derive(Debug, Clone, Serialize)]
pub enum Builtin {
    AddInt(Box<Ast>, Box<Ast>),
    AddFloat(Box<Ast>, Box<Ast>),

    SubInt(Box<Ast>, Box<Ast>),
    SubFloat(Box<Ast>, Box<Ast>),

    MulInt(Box<Ast>, Box<Ast>),
    MulFloat(Box<Ast>, Box<Ast>),

    DivSigned(Box<Ast>, Box<Ast>),
    DivUnsigned(Box<Ast>, Box<Ast>),
    DivFloat(Box<Ast>, Box<Ast>),

    ModSigned(Box<Ast>, Box<Ast>),
    ModUnsigned(Box<Ast>, Box<Ast>),
    ModFloat(Box<Ast>, Box<Ast>),

    LessSigned(Box<Ast>, Box<Ast>),
    LessUnsigned(Box<Ast>, Box<Ast>),
    LessFloat(Box<Ast>, Box<Ast>),

    EqInt(Box<Ast>, Box<Ast>),
    EqFloat(Box<Ast>, Box<Ast>),
    EqChar(Box<Ast>, Box<Ast>),
    EqBool(Box<Ast>, Box<Ast>),

    SignExtend(Box<Ast>, Type),
    ZeroExtend(Box<Ast>, Type),

    SignedToFloat(Box<Ast>, Type),
    UnsignedToFloat(Box<Ast>, Type),
    FloatToSigned(Box<Ast>, Type),
    FloatToUnsigned(Box<Ast>, Type),
    FloatPromote(Box<Ast>),
    FloatDemote(Box<Ast>),

    BitwiseAnd(Box<Ast>, Box<Ast>),
    BitwiseOr(Box<Ast>, Box<Ast>),
    BitwiseXor(Box<Ast>, Box<Ast>),
    BitwiseNot(Box<Ast>),

    Truncate(Box<Ast>, Type),
    Deref(Box<Ast>, Type),
    Offset(Box<Ast>, Box<Ast>, u32), // u32 is the pointer element size in bytes
    Transmute(Box<Ast>, Type),

    /// Allocate space for the given value on the stack, and store it there. Return the stack address
    StackAlloc(Box<Ast>),
}

#[derive(Debug, Clone, Serialize)]
pub enum Ast {
    Literal(Literal),
    Variable(Variable),
    Lambda(Lambda),
    FunctionCall(FunctionCall),
    Definition(Definition),
    If(If),
    Match(Match),
    Return(Return),
    Sequence(Sequence),
    Extern(Extern),
    Assignment(Assignment),
    MemberAccess(MemberAccess),
    Tuple(Tuple),
    ReinterpretCast(ReinterpretCast),
    Builtin(Builtin),
}

impl Ast {
    pub fn try_i64(&self) -> Option<i64> {
        if let Self::Literal(Literal::Integer(u, IntegerKind::I64)) = self {
            Some(*u as i64)
        } else {
            None
        }
    }

    pub fn to_ron(&self) -> String {
        //Result<String, ron::Error> {
        use ron::ser::{to_string_pretty, PrettyConfig};
        let pretty = PrettyConfig::new()
            //.depth_limit(3)
            .compact_arrays(true);
        to_string_pretty(&self, pretty).unwrap()
    }
}

impl From<Builtin> for Ast {
    fn from(item: Builtin) -> Self {
        Ast::Builtin(item)
    }
}

impl From<Literal> for Ast {
    fn from(item: Literal) -> Self {
        Ast::Literal(item)
    }
}

impl From<Variable> for Ast {
    fn from(v: Variable) -> Ast {
        Ast::Variable(v)
    }
}

impl From<Definition> for Ast {
    fn from(definition: Definition) -> Ast {
        Ast::Definition(definition)
    }
}

impl From<Lambda> for Ast {
    fn from(f: Lambda) -> Ast {
        Ast::Lambda(f)
    }
}

impl From<FunctionCall> for Ast {
    fn from(f: FunctionCall) -> Ast {
        Ast::FunctionCall(f)
    }
}

impl From<Sequence> for Ast {
    fn from(v: Sequence) -> Ast {
        Ast::Sequence(v)
    }
}

impl From<Extern> for Ast {
    fn from(v: Extern) -> Ast {
        Ast::Extern(v)
    }
}

impl From<If> for Ast {
    fn from(item: If) -> Ast {
        Ast::If(item)
    }
}

impl From<Return> for Ast {
    fn from(item: Return) -> Ast {
        Ast::Return(item)
    }
}

impl std::fmt::Display for DefinitionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "v{}", self.0)
    }
}

#[macro_export]
macro_rules! dispatch_on_hir {
    ( $expr_name:expr, $function:expr $(, $($args:expr),* )? ) => ({
        match $expr_name {
            $crate::hir::Ast::Literal(inner) =>         $function(inner $(, $($args),* )? ),
            $crate::hir::Ast::Variable(inner) =>        $function(inner $(, $($args),* )? ),
            $crate::hir::Ast::Lambda(inner) =>          $function(inner $(, $($args),* )? ),
            $crate::hir::Ast::FunctionCall(inner) =>    $function(inner $(, $($args),* )? ),
            $crate::hir::Ast::Definition(inner) =>      $function(inner $(, $($args),* )? ),
            $crate::hir::Ast::If(inner) =>              $function(inner $(, $($args),* )? ),
            $crate::hir::Ast::Match(inner) =>           $function(inner $(, $($args),* )? ),
            $crate::hir::Ast::Return(inner) =>          $function(inner $(, $($args),* )? ),
            $crate::hir::Ast::Sequence(inner) =>        $function(inner $(, $($args),* )? ),
            $crate::hir::Ast::Extern(inner) =>          $function(inner $(, $($args),* )? ),
            $crate::hir::Ast::Assignment(inner) =>      $function(inner $(, $($args),* )? ),
            $crate::hir::Ast::MemberAccess(inner) =>    $function(inner $(, $($args),* )? ),
            $crate::hir::Ast::Tuple(inner) =>           $function(inner $(, $($args),* )? ),
            $crate::hir::Ast::ReinterpretCast(inner) => $function(inner $(, $($args),* )? ),
            $crate::hir::Ast::Builtin(inner) =>         $function(inner $(, $($args),* )? ),
        }
    });
}

// Rust won't let us impl<T: FmtAst> Display for T
macro_rules! impl_display {
    ($typ:ty) => {
        impl std::fmt::Display for $typ {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                printer::AstPrinter::default().start(self, f)
            }
        }
    };
}

impl_display!(Ast);
impl_display!(Literal);
impl_display!(Variable);
impl_display!(Lambda);
impl_display!(FunctionCall);
impl_display!(Definition);
impl_display!(If);
impl_display!(Match);
impl_display!(Return);
impl_display!(Sequence);
impl_display!(Extern);
impl_display!(Assignment);
impl_display!(MemberAccess);
impl_display!(Tuple);
impl_display!(ReinterpretCast);
impl_display!(Builtin);
