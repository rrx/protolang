#[derive(PartialEq, Debug, Clone)]
pub enum PrattValue {
    Empty,
    Prefix(Tok, Box<ASTNode>),
    Binary(Tok, Box<ASTNode>, Box<ASTNode>),
    Ternary(Tok, Box<ASTNode>, Box<ASTNode>, Box<ASTNode>),
    Postfix(Tok, Box<ASTNode>),
    Literal(Tok),
    Ident(Tok),
    Error(String),
    List(Vec<ASTNode>),
    Chain(Tok, Vec<ASTNode>),
    Expr(Box<ASTNode>),
    Null,
    //Callable(CallableNode),
    Invalid(String),
}

pub trait Context {
    fn from_location(loc: &Location) -> Self;
}

#[derive(PartialEq, Debug, Clone)]
pub struct NodeContextWithLocation {
    pub s: Surround,
    pub loc: Location,
}

impl Context for NodeContextWithLocation {
    fn from_location(loc: &Location) -> Self {
        Self { s: Surround::default(), loc: loc.clone() }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct NodeContextNull {
}

impl Context for NodeContextNull {
    fn from_location(_: &Location) -> Self {
        Self {}
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ASTNodeWithLocationImpl<C: Context> {
    pub context: C,
    pub value: PrattValue,
}


impl Unparse for ASTNode {
    fn unparse(&self) -> Vec<Tok> {
        let mut out = vec![];
        match &self.value {
            PrattValue::Empty => (),
            PrattValue::Null => (),
            PrattValue::Invalid(_) => (),
            PrattValue::Error(_) => (),
            PrattValue::Chain(_,_) => (),
            PrattValue::Ternary(_,_,_,_) => (),
            PrattValue::Ident(x) => {
                out.push(x.clone());
                //out.append(&mut x.unparse());
            }
            PrattValue::Literal(x) => {
                out.push(x.clone());
            }
            PrattValue::Expr(x) => {
                out.append(&mut x.unparse());
            }
            PrattValue::Prefix(tok, expr) => {
                out.push(tok.clone());
                out.append(&mut expr.unparse());
            }
            PrattValue::Postfix(tok, expr) => {
                out.append(&mut expr.unparse());
                out.push(tok.clone());
            }
            PrattValue::Binary(op, left, right) => {
                out.append(&mut left.unparse());
                out.push(op.clone());
                //out.append(&mut op.unparse());
                out.append(&mut right.unparse());
            }
            PrattValue::List(elements) => {
                for e in elements {
                    out.append(&mut e.unparse());
                }
            }
        };
        self.context.s.unparse(out)
    }
}
impl SExpr for ASTNode {
    fn sexpr(&self) -> SResult<S> {
        match &self.value {
            PrattValue::Expr(x) => x.sexpr(),
            //PrattValue::Ident(x) => x.sexpr(),
            PrattValue::Binary(op, left, right) => {
                let sleft = left.sexpr()?;
                let sright = right.sexpr()?;
                Ok(S::Cons(op.unlex(), vec![sleft, sright]))
            }
            PrattValue::Prefix(prefix, expr) => {
                let s = expr.sexpr()?;
                Ok(S::Cons(prefix.unlex(), vec![s]))
            }
            PrattValue::Postfix(postfix, expr) => {
                let s = expr.sexpr()?;
                Ok(S::Cons(postfix.unlex(), vec![s]))
            }
            PrattValue::List(elements) => {
                let s_args = elements
                    .iter()
                    .filter_map(|a| a.sexpr().ok())
                    .collect::<Vec<_>>();
                Ok(S::Cons("list".into(), s_args))
            }
            _ => unreachable!()
        }
    }
}

pub type ASTNode = ASTNodeWithLocationImpl<NodeContextWithLocation>;

impl ASTNode {
    pub fn new(value: PrattValue, loc: &Location) -> Self {
        Self {
            context:  NodeContextWithLocation::from_location(loc),
            value,
        }
    }
}

