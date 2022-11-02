use super::*;

pub fn make_binary_function(name: String, args: Vec<Type>, env: &mut Environment) {
    assert_eq!(args.len(), 3);
    let left_ty = args.get(0).unwrap().clone();
    let right_ty = args.get(1).unwrap().clone();
    let ret_ty = args.get(2).unwrap().clone();

    let left = AstNode::new(
        Variable::new("left".into(), DefinitionId(0), left_ty.clone()).into(),
        left_ty.clone());

    let right = AstNode::new(
        Variable::new("right".into(), DefinitionId(0), right_ty.clone()).into(),
        right_ty.clone());

    let ret = AstNode::new(
        Variable::new("ret".into(), DefinitionId(0), ret_ty.clone()).into(),
        ret_ty.clone());

    let args = vec![left.into(), right.into(), ret.into()];

    let node = AstNodeInner {
        value: Ast::Builtin(name.clone(), args),
        ty: Type::Func(vec![left_ty, right_ty, ret_ty]),
    };

    env.define(name, node.into());
}
pub fn base_env() -> Environment {
    let mut env = Environment::default();
    make_binary_function("*".into(), vec![Type::Int, Type::Int, Type::Int], &mut env);
    make_binary_function(
        "*".into(),
        vec![Type::Float, Type::Float, Type::Float],
        &mut env,
    );
    make_binary_function(
        "*".into(),
        vec![Type::Int, Type::Float, Type::Float],
        &mut env,
    );
    make_binary_function(
        "*".into(),
        vec![Type::Float, Type::Int, Type::Float],
        &mut env,
    );

    make_binary_function("+".into(), vec![Type::Int, Type::Int, Type::Int], &mut env);
    make_binary_function(
        "+".into(),
        vec![Type::Float, Type::Float, Type::Float],
        &mut env,
    );
    make_binary_function(
        "+".into(),
        vec![Type::Int, Type::Float, Type::Float],
        &mut env,
    );
    make_binary_function(
        "+".into(),
        vec![Type::Float, Type::Int, Type::Float],
        &mut env,
    );

    make_binary_function("-".into(), vec![Type::Int, Type::Int, Type::Int], &mut env);
    make_binary_function(
        "-".into(),
        vec![Type::Float, Type::Float, Type::Float],
        &mut env,
    );
    make_binary_function(
        "-".into(),
        vec![Type::Int, Type::Float, Type::Float],
        &mut env,
    );
    make_binary_function(
        "-".into(),
        vec![Type::Float, Type::Int, Type::Float],
        &mut env,
    );

    make_binary_function("^".into(), vec![Type::Int, Type::Int, Type::Int], &mut env);
    make_binary_function(
        "^".into(),
        vec![Type::Float, Type::Int, Type::Float],
        &mut env,
    );
    make_binary_function(">".into(), vec![Type::Int, Type::Int, Type::Bool], &mut env);
    make_binary_function(
        ">".into(),
        vec![Type::Float, Type::Float, Type::Bool],
        &mut env,
    );
    env
}

