use super::*;
use crate::lexer::Location;
use crate::results::*;
use crate::tokens::{FileId, Tok};
use log::*;
use nom::InputIter;
use std::fmt;

pub type SymbolTable = HashTrieMap<usize, Type>;

#[derive(Clone, Debug, PartialEq)]
pub struct TypeEquation {
    left: Type,
    // ordered set - should be ordered from general to specific
    right: Vec<Type>,
    node: IR,
    env: Environment,
}

impl TypeEquation {
    fn new(left: Type, right: Vec<Type>, node: IR, env: Environment) -> Self {
        Self {
            left,
            right,
            node,
            env,
        }
    }
}

impl fmt::Display for TypeEquation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?} :: {:?}, from: {:?}",
            self.left, self.right, &self.node.loc
        )
    }
}

#[derive(Default)]
pub struct TypeChecker {
    results: CompileResults,
    pub type_equations: Vec<TypeEquation>,
    type_counter: usize,
}

impl TypeChecker {
    pub fn new_env() -> Environment {
        base_env()
    }

    pub fn print(&self) {
        self.results.print();
    }

    pub fn clear(&mut self) {
        self.results.clear();
    }

    pub fn push_error(&mut self, r: LangError) {
        self.results.push(r);
    }

    pub fn add_source(&mut self, filename: String, source: String) -> FileId {
        self.results.add_source(filename, source)
    }

    pub fn has_errors(&self) -> bool {
        self.results.has_errors
    }

    fn next_type_counter(&mut self) -> usize {
        let x = self.type_counter;
        self.type_counter += 1;
        x
    }

    fn get_fresh_typename(&mut self) -> usize {
        self.next_type_counter()
        //format!("t{}", self.next_type_counter())
    }

    fn subs_if_exists<'a>(mut ty: &'a Type, subst: &'a SymbolTable) -> &'a Type {
        if let Type::Unknown(name) = ty {
            if subst.contains_key(name) {
                ty = subst.get(name).unwrap();
            }
        }
        ty
    }

    // Does ty1 occur in ty2?
    fn occurs_check(ty1: &Type, ty2: &Type, subst: &SymbolTable) -> bool {
        if ty1 == ty2 {
            return true;
        }

        assert!(!ty1.is_unknown());

        let ty2 = Self::subs_if_exists(ty2, &subst);

        if ty1 == ty2 {
            return true;
        }

        // if ty1 occurs in any of a functions parameters
        if let Type::Func(sig) = ty2 {
            return sig.iter().any(|s| Self::occurs_check(ty1, s, subst));
        }
        false
    }

    fn unify_eq(&self, ty1: &Type, ty2: &Type, subst: Option<SymbolTable>) -> Option<SymbolTable> {
        debug!("unify_eq: {:?} :: {:?}", ty1, ty2);

        if ty1 == ty2 {
            return subst;
        }

        let subst = subst.unwrap();

        // make substitutions
        let ty1 = Self::subs_if_exists(ty1, &subst);
        let ty2 = Self::subs_if_exists(ty2, &subst);

        if let Type::Unknown(name) = ty1 {
            return Some(subst.insert(name.clone(), ty2.clone()));
        } else if let Type::Unknown(name) = ty2 {
            return Some(subst.insert(name.clone(), ty1.clone()));
        }

        if ty1 == ty2 {
            return Some(subst);
        }

        if Self::occurs_check(ty1, ty2, &subst) {
            return None;
        }

        if let Type::Func(sig1) = ty1 {
            if let Type::Func(sig2) = ty2 {
                return self.unify_fn(sig1, sig2, subst.clone());
            } else {
                return None;
            }
        }

        None
    }

    fn unify_fn(
        &self,
        sig1: &Vec<Type>,
        sig2: &Vec<Type>,
        mut subst: SymbolTable,
    ) -> Option<SymbolTable> {
        debug!("unify_fn: {:?} :: {:?}", sig1, sig2);

        if sig1.len() != sig2.len() {
            return None;
        }

        let mut local_subst = SymbolTable::default();
        for (a1, a2) in sig1.iter().zip(sig2.iter()) {
            match self.unify(a1, &Vec::from([a2.clone()]), Some(local_subst)) {
                Some(s) => {
                    local_subst = s;
                }
                None => {
                    return None;
                }
            }
        }

        for (k, v) in local_subst.iter() {
            subst = subst.insert(k.clone(), v.clone());
        }

        debug!("unify_fn: {:?}", &subst);
        Some(subst)
    }

    pub fn unify(
        &self,
        left: &Type,
        right: &Vec<Type>,
        subst: Option<SymbolTable>,
    ) -> Option<SymbolTable> {
        debug!("unify: {:?} :: {:?}", left, right);
        if subst == None {
            return None;
        }

        let mut local_subst = None;
        for r in right {
            if let Some(s) = self.unify_eq(left, r, subst.clone()) {
                local_subst = Some(s);
                break;
            }
        }

        // return the solution if it was found
        local_subst
    }

    pub fn unify_all(&mut self) -> Option<SymbolTable> {
        let mut subst = Some(SymbolTable::default());
        let mut errors = vec![];
        for eq in &self.type_equations {
            match self.unify(&eq.left, &eq.right, subst.clone()) {
                Some(s) => {
                    subst = Some(s);
                }
                None => {
                    errors.push(eq);
                }
            }
        }
        for eq in errors {
            let msg = format!("Unable to unify: {:?} :: {:?}", &eq.left, &eq.right);
            self.results
                .push(LangError::error(msg.clone(), eq.node.loc.clone()));
        }
        subst
    }

    fn new_unknown_type(&mut self) -> Type {
        Type::new_unknown(self.get_fresh_typename())
    }

    pub fn parse_ast(&mut self, node: &ExprNode, env: Environment) -> IR {
        match &node.value {
            Expr::Literal(Tok::FloatLiteral(f)) => IR::new_with_location(
                IRValue::Literal(Literal::Float(*f)),
                Type::Float,
                node.context.to_location(),
                env,
            ),
            Expr::Literal(Tok::IntLiteral(i)) => IR::new_with_location(
                IRValue::Literal(Literal::Int(*i)),
                Type::Int,
                node.context.to_location(),
                env,
            ),
            Expr::Literal(Tok::BoolLiteral(b)) => IR::new_with_location(
                IRValue::Literal(Literal::Bool(*b)),
                Type::Bool,
                node.context.to_location(),
                env,
            ),
            Expr::Literal(Tok::StringLiteral(s)) => IR::new_with_location(
                IRValue::Literal(Literal::String(s.clone())),
                Type::String,
                node.context.to_location(),
                env,
            ),
            Expr::Literal(_) => unimplemented!(),

            Expr::Ident(s) => {
                let ty = self.new_unknown_type();
                self.make_ident_from_name(s.name.clone(), ty, node.context.to_location(), env)
            }

            Expr::Apply(f, args) => {
                let ir_func = self.parse_ast(f, env.clone());

                let mut local_env = env.clone();
                let mut ir_args = vec![];
                for arg in args {
                    let node = self.parse_ast(arg, local_env);
                    local_env = node.env.clone();
                    ir_args.push(node);
                }

                self.make_apply(ir_func, ir_args, local_env)
            }

            Expr::Lambda(f) => {
                let mut local_env = env.clone();
                let mut ir_args = vec![];
                for p in &f.params.value {
                    let node = self.parse_ast(p, local_env);
                    local_env = node.env.clone();
                    ir_args.push(node);
                }

                // use local environment when parsing body
                let body = self.parse_ast(&f.expr, local_env);

                // use original context for the node
                IR::new_with_location(
                    IRValue::Function(Box::new(body), ir_args),
                    self.new_unknown_type(),
                    node.context.to_location(),
                    env,
                )
            }

            Expr::Block(exprs) | Expr::Program(exprs) => {
                let mut local_env = env.clone();
                let mut ir_exprs = vec![];
                for p in exprs {
                    let node = self.parse_ast(p, local_env);
                    local_env = node.env.clone();
                    ir_exprs.push(node);
                }

                self.make_block(ir_exprs, node.context.to_location(), env)
            }

            Expr::Prefix(op, right) => {
                let name = op_name(op);
                let ir_right = self.parse_ast(right, env.clone());
                self.make_apply_by_name(name, vec![ir_right], node.context.to_location(), env)
            }

            Expr::Binary(op, left, right) => match &op.value {
                Operator::Assign => {
                    let name = left.try_ident().unwrap().name;
                    let ir_right = self.parse_ast(right, env.clone());
                    self.make_assign(name, ir_right, node.context.to_location(), env.clone())
                }

                Operator::Declare => {
                    let name = left.try_ident().unwrap().name;
                    let ir_right = self.parse_ast(right, env.clone());
                    self.make_declare(name, ir_right, node.context.to_location(), env)
                }

                _ => {
                    let name = op_name(op);
                    let ir_left = self.parse_ast(left, env.clone());
                    let ir_right = self.parse_ast(right, env.clone());
                    self.make_apply_by_name(
                        name,
                        vec![ir_left, ir_right],
                        op.context.to_location(),
                        env,
                    )
                }
            },

            Expr::Ternary(op, a, b, c) => match op.value {
                Operator::Conditional => {
                    let ir_a = self.parse_ast(a, env.clone());
                    let ir_b = self.parse_ast(b, env.clone());
                    let ir_c = self.parse_ast(c, env.clone());
                    self.make_apply_by_name(
                        "cond".into(),
                        vec![ir_a, ir_b, ir_c],
                        node.context.to_location(),
                        env,
                    )
                }
                _ => unimplemented!(),
            },

            _ => {
                debug!("Unimplemented: {:?}", &node);
                unimplemented!()
            }
        }
    }

    fn make_block(&mut self, nodes: Vec<IR>, loc: Location, env: Environment) -> IR {
        let block_ty = nodes.last().unwrap().ty.clone();
        let block = IR::new_with_location(IRValue::Block(nodes), block_ty.clone(), loc, env);
        block
    }

    fn make_assign(&mut self, name: String, node: IR, loc: Location, env: Environment) -> IR {
        if let Some(v) = env.resolve(&name) {
            let left_ty = v.ty.clone();
            let right_ty = node.ty.clone();
            let result_ty = self.new_unknown_type();
            let result = IR::new_with_location(
                IRValue::Assign(name, Box::new(node.clone())),
                result_ty.clone(),
                node.loc.clone(),
                env.clone(),
            );

            self.type_equations.push(TypeEquation::new(
                left_ty,
                Vec::from([right_ty.clone()]),
                node.clone(),
                env.clone(),
            ));

            self.type_equations.push(TypeEquation::new(
                result_ty,
                Vec::from([right_ty]),
                result.clone(),
                env,
            ));
            result
        } else {
            self.make_error(format!("Not found: {}", name), loc, env)
        }
    }

    fn make_declare(&mut self, name: String, node: IR, loc: Location, env: Environment) -> IR {
        let mut local_env = env.clone();
        let right_ty = node.ty.clone();
        local_env.define(name.clone(), node.clone());
        let expr = IR::new_with_location(
            IRValue::Declare(name, Box::new(node)),
            right_ty.clone(),
            loc,
            local_env,
        );
        expr
    }

    fn make_error(&mut self, msg: String, loc: Location, env: Environment) -> IR {
        self.results
            .push(LangError::error(msg.clone(), loc.clone()));
        IR::new_with_location(IRValue::Error(msg), Type::Error, loc, env)
    }

    fn make_func_from_name(
        &mut self,
        name: String,
        ty: Type,
        loc: Location,
        env: Environment,
    ) -> IR {
        // get all of the types that match the name
        let mut fn_types = Vec::new();
        for f in env.resolve_all(&name) {
            match &f.value {
                IRValue::Function(_, _) | IRValue::Extern(_) => {
                    fn_types.push(f.ty.clone());
                }
                _ => (),
            }
        }
        if fn_types.len() == 0 {
            let msg = format!("Function Not found: {}", name);
            self.make_error(msg, loc, env)
        } else {
            let result = IR::new_with_location(IRValue::Ident(name), ty.clone(), loc, env.clone());
            self.type_equations.push(TypeEquation::new(
                ty.clone(),
                fn_types,
                result.clone(),
                env.clone(),
            ));
            result
        }
    }

    fn make_ident_from_name(
        &mut self,
        name: String,
        ty: Type,
        loc: Location,
        env: Environment,
    ) -> IR {
        if let Some(v) = env.resolve(&name) {
            let v_ty = v.ty.clone();
            let result =
                IR::new_with_location(IRValue::Ident(name), v_ty.clone(), loc, env.clone());

            self.type_equations.push(TypeEquation::new(
                ty.clone(),
                Vec::from([v_ty.clone()]),
                result.clone(),
                env.clone(),
            ));
            result
        } else {
            self.make_error(format!("Not found: {}", name), loc, env)
        }
    }

    fn make_ident(&mut self, node: IR, env: Environment) -> IR {
        match &node.value {
            IRValue::Ident(name) => {
                self.make_ident_from_name(name.clone(), node.ty.clone(), node.loc, env)
            }
            _ => {
                debug!("Unimplemented: {:?}", &node);
                unimplemented!()
            }
        }
    }

    fn make_apply_by_name(
        &mut self,
        name: String,
        ir_args: Vec<IR>,
        loc: Location,
        env: Environment,
    ) -> IR {
        let ret_ty = self.new_unknown_type();
        let mut f_types = ir_args.iter().map(|v| v.ty.clone()).collect::<Vec<_>>();
        f_types.push(ret_ty.clone());
        let f_ty = Type::Func(f_types);
        let f = self.make_func_from_name(name.clone(), f_ty, loc.clone(), env.clone());
        IR::new_with_location(
            IRValue::Apply(Box::new(f), ir_args),
            ret_ty,
            loc,
            env.clone(),
        )
    }

    fn make_apply(&mut self, ir_func: IR, ir_args: Vec<IR>, env: Environment) -> IR {
        let ret_ty = self.new_unknown_type();
        let ir_func = match &ir_func.value {
            IRValue::Ident(name) => {
                // make function signature
                let mut f_types = ir_args.iter().map(|v| v.ty.clone()).collect::<Vec<_>>();
                f_types.push(ret_ty.clone());
                let f_ty = Type::Func(f_types);

                // make a node with that signature
                self.make_func_from_name(name.clone(), f_ty, ir_func.loc, env.clone())
            }
            IRValue::Extern(_) | IRValue::Function(_, _) => ir_func,
            IRValue::Error(_) => ir_func,
            _ => {
                debug!("Unimplemented: {:?}", &ir_func);
                unimplemented!()
            }
        };

        let loc = ir_func.loc.clone();
        IR::new_with_location(
            IRValue::Apply(Box::new(ir_func), ir_args),
            ret_ty,
            loc,
            env.clone(),
        )
    }

    pub fn parse_str(&mut self, s: &str, env: Environment) -> anyhow::Result<IR> {
        self._parse("<repl>", s, env)
    }

    pub fn parse_file(&mut self, filename: &str, env: Environment) -> anyhow::Result<IR> {
        let contents = std::fs::read_to_string(filename.clone())
            .unwrap()
            .to_string();
        self._parse(filename, &contents, env)
    }

    fn _parse(&mut self, filename: &str, contents: &str, env: Environment) -> anyhow::Result<IR> {
        let file_id = self
            .results
            .add_source(filename.into(), contents.to_string());
        let mut lexer = crate::lexer::LexerState::from_str_eof(&contents)
            .unwrap()
            .set_file_id(file_id);
        let tokens = lexer.tokens();

        for t in tokens.iter_elements() {
            debug!("T: {:?}", t);
        }
        let (_, node) = crate::parser::parse_program(tokens).unwrap();
        debug!("Node: {:?}", node);
        let ir = self.parse_ast(&node, env);
        Ok(ir)
    }

    pub fn check_str(&mut self, s: &str, env: Environment) -> anyhow::Result<IR> {
        self._check("<repl>", s, env)
    }

    pub fn check_file(&mut self, filename: &str, env: Environment) -> anyhow::Result<IR> {
        let contents = std::fs::read_to_string(filename.clone())
            .unwrap()
            .to_string();
        self._check(filename, &contents, env)
    }

    fn _check(&mut self, filename: &str, contents: &str, env: Environment) -> anyhow::Result<IR> {
        let ir = self._parse(filename, contents, env.clone()).unwrap();
        debug!("{}", ir);

        for e in &self.type_equations {
            debug!("E: {}", e);
        }
        let s = self.unify_all().unwrap();
        for x in &s {
            debug!("subst: {:?}", x);
        }
        debug!("has_errors: {}", self.has_errors());
        self.print();
        Ok(ir)
    }
}
