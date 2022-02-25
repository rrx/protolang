pub type Program = Vec<Expr>;

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    LitExpr(Literal),
    PrefixExpr(Prefix, Box<Expr>)
}

#[derive(PartialEq, Debug, Clone)]
pub enum Prefix {
    PrefixPlus,
    PrefixMinus,
    PrefixNot
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    IntLiteral(u64),
    BoolLiteral(bool),
    StringLiteral(String),
}

#[derive(PartialEq, Debug, Eq, Clone)]
pub struct Ident(pub String);

