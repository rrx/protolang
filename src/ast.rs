use crate::tokens::{Tok, Token};
use crate::sexpr::*;

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    Program(Program),
    Stmt(Stmt),
    Expr(Expr),
    Lit(Literal),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Stmt {
    Expr(Expr),
    Lit(Literal),
}

impl Stmt {
    pub fn unparse(&mut self) -> Vec<Tok> {
        let mut out = vec![];
        out.append(&mut match self {
            Stmt::Expr(expr) => expr.unparse(),
            Stmt::Lit(lit) => lit.unparse(),
        });
        out
    }
}
impl SExpr for Stmt {
    fn sexpr(&self) -> SResult<S> {
        match self {
            Stmt::Lit(x) => x.sexpr(),
            Stmt::Expr(x) => x.sexpr(),
        }
    }
}

impl Value {
    pub fn unparse(&mut self) -> Vec<Tok> {
        let mut out = vec![];
        out.append(&mut match self {
            Value::Program(prog) => prog.unparse(),
            Value::Expr(expr) => expr.unparse(),
            Value::Lit(lit) => lit.unparse(),
            Value::Stmt(stmt) => stmt.unparse(),
        });
        out
    }
}

impl SExpr for Value {
    fn sexpr(&self) -> SResult<S> {
        use Value::*;
        match self {
            Expr(expr) => expr.sexpr(),
            Lit(lit) => lit.sexpr(),
            _ => Err(SError::Invalid)
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Node {
    pub pre: Vec<Tok>,
    pub post: Vec<Tok>,
    //pub tokens: Vec<Tok>,
    pub value: Value
}

impl Node {
    pub fn unparse(&mut self) -> Vec<Tok> {
        let mut out = vec![];
        out.append(&mut self.pre);
        out.append(&mut self.value.unparse());
        out.append(&mut self.post);
        out
    }

    pub fn sexpr(&self) -> SResult<S> {
        self.value.sexpr()
    }
}


#[derive(PartialEq, Debug, Clone)]
pub struct Program(pub Vec<Stmt>);

impl Program {
    pub fn unparse(&mut self) -> Vec<Tok> {
        let mut out = vec![];
        for expr in self.0.iter_mut() {
            out.append(&mut expr.unparse());
        }
        out
    }

    pub fn sexpr(&self) -> SResult<SProgram> {
        let results = self.0.iter().filter_map(|v| v.sexpr().ok()).collect();
        Ok(results)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    IdentExpr(Ident),
    LitExpr(Literal),
    PrefixExpr(Prefix, Box<Value>),
    InfixExpr(Infix, Box<Value>, Box<Value>)
}
impl Expr {
    pub fn unparse(&mut self) -> Vec<Tok> {
        use Literal::*;
        use Expr::*;
        let mut out = vec![];
        match self {
            IdentExpr(x) => {
                out.append(&mut x.unparse());
            }
            LitExpr(x) => {
                out.append(&mut x.unparse());
            }
            PrefixExpr(prefix, expr) => {
                out.append(&mut prefix.unparse());
                out.append(&mut expr.unparse());
            }
            InfixExpr(op, left, right) => {
                out.append(&mut op.unparse());
                out.append(&mut left.unparse());
                out.append(&mut right.unparse());
            }
            _ => ()
        };
        out
    }
    pub fn sexpr(&self) -> SResult<S> {
        use Literal::*;
        use Expr::*;
        match self {
            LitExpr(x) => x.sexpr(),
            IdentExpr(x) => x.sexpr(),
            InfixExpr(op, left, right) => {
                let sleft = left.sexpr()?;
                let sright = right.sexpr()?;
                Ok(S::Cons(op.to_string(), vec![sleft, sright]))
            }
            PrefixExpr(prefix, expr) => {
                let s = expr.sexpr()?;
                Ok(S::Cons(prefix.to_string(), vec![s]))
            }
            _ => Err(SError::Invalid)
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Prefix {
    PrefixPlus,
    PrefixMinus,
    PrefixNot
}
impl Prefix {
    pub fn to_string(&self) -> String {
        self.to_token().unlex()
    }

    pub fn to_token(&self) -> Tok {
        match self {
            Prefix::PrefixPlus => Tok::Plus,
            Prefix::PrefixMinus => Tok::Minus,
            Prefix::PrefixNot => Tok::Not,
        }
    }

    pub fn unparse(&mut self) -> Vec<Tok> {
        vec![self.to_token()]
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Infix {
    Plus,
    Minus,
    Divide,
    Multiply,
    Equal,
    NotEqual,
    GreaterThanEqual,
    LessThanEqual,
    GreaterThan,
    LessThan,
}

impl Infix {
    pub fn to_string(&self) -> String {
        self.to_token().unlex()
    }

    pub fn to_token(&self) -> Tok {
        match self {
            Infix::Plus => Tok::Plus,
            Infix::Minus => Tok::Minus,
            Infix::Multiply => Tok::Mul,
            Infix::Divide => Tok::Div,
            _ => Tok::Not,
        }
    }

    pub fn unparse(&mut self) -> Vec<Tok> {
        vec![self.to_token()]
    }

}


pub fn infix_op(t: &Tok) -> (Precedence, Option<Infix>) {
    match *t {
        Tok::Equals => (Precedence::PEquals, Some(Infix::Equal)),
        Tok::NotEquals => (Precedence::PEquals, Some(Infix::NotEqual)),
        Tok::LTE => (Precedence::PLessGreater, Some(Infix::LessThanEqual)),
        Tok::GTE => (Precedence::PLessGreater, Some(Infix::GreaterThanEqual)),
        Tok::LT => (Precedence::PLessGreater, Some(Infix::LessThan)),
        Tok::GT => (Precedence::PLessGreater, Some(Infix::GreaterThan)),
        Tok::Plus => (Precedence::PSum, Some(Infix::Plus)),
        Tok::Minus => (Precedence::PSum, Some(Infix::Minus)),
        Tok::Mul => (Precedence::PProduct, Some(Infix::Multiply)),
        Tok::Div => (Precedence::PProduct, Some(Infix::Divide)),
        Tok::LParen => (Precedence::PCall, None),
        Tok::LBracket => (Precedence::PIndex, None),
        _ => (Precedence::PLowest, None),
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    IntLiteral(u64),
    FloatLiteral(f64),
    BoolLiteral(bool),
    StringLiteral(String),
    Invalid(String)
}
impl Literal {
    pub fn unparse(&mut self) -> Vec<Tok> {
        use Literal::*;
        let token = match self {
            IntLiteral(x) => Tok::IntLiteral(*x),
            FloatLiteral(x) => Tok::FloatLiteral(*x),
            BoolLiteral(x) => Tok::BoolLiteral(*x),
            StringLiteral(x) => Tok::StringLiteral(x.clone()),
            Invalid(x) => Tok::Invalid(x.clone()),
        };
        vec![token]
    }

}

impl SExpr for Literal {
    fn sexpr(&self) -> SResult<S> {
        use Literal::*;
        match self {
            IntLiteral(x) => Ok(S::Atom(x.to_string())),
            FloatLiteral(x) => Ok(S::Atom(x.to_string())),
            BoolLiteral(x) => Ok(S::Atom(x.to_string())),
            StringLiteral(x) => Ok(S::Atom(x.to_string())),
            _ => Err(SError::Invalid)
        }
    }
}

#[derive(PartialEq, Debug, Eq, Clone)]
pub struct Ident(pub String);
impl Ident {
    pub fn unparse(&self) -> Vec<Tok> {
        vec![Tok::Ident(self.0.clone())]
    }
}

impl SExpr for Ident {
    fn sexpr(&self) -> SResult<S> {
        Ok(S::Atom(self.0.clone()))
    }
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    PLowest,
    PEquals,
    PLessGreater,
    PSum,
    PProduct,
    PCall,
    PIndex,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokens::*;
    use crate::lexer::*;

    #[test]
    fn prefix() {
        let p = Prefix::PrefixMinus;
        assert_eq!(p.to_string(), "-");
    }

    #[test]
    fn infix() {
        let p = Infix::Minus;
        println!("{:?}", (&p, &p.to_token(), &p.to_string()));
        assert_eq!(p.to_string(), "-");
    }
}


