pub type Program = Vec<Literal>;

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    IntLiteral(u64),
    BoolLiteral(bool),
    StringLiteral(String),
}

#[derive(PartialEq, Debug, Eq, Clone)]
pub struct Ident(pub String);

