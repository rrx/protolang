use crate::parser::Unparse;
use crate::tokens::{Tok, Token};

use super::{FromContext, MaybeNodeContext};

#[derive(PartialEq, Debug, Clone)]
pub struct OperatorNode {
    pub context: MaybeNodeContext,
    pub value: Operator,
}

impl std::ops::Deref for OperatorNode {
    type Target = Operator;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl std::ops::DerefMut for OperatorNode {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl OperatorNode {
    pub fn prefix_token(&self) -> Option<Tok> {
        match self.value {
            Operator::Plus => Some(Tok::Plus),
            Operator::Minus => Some(Tok::Minus),
            Operator::Not => Some(Tok::Exclamation),
            _ => None,
        }
    }

    pub fn postfix_token(&self) -> Option<Tok> {
        match self.value {
            Operator::Bang => Some(Tok::Exclamation),
            _ => None,
        }
    }

    pub fn from_postfix_tok(token: &Tok) -> Option<Operator> {
        match token {
            //Tok::Percent => Some(Operator::Bang),
            Tok::Exclamation => Some(Operator::Bang),
            _ => None,
        }
    }

    pub fn from_postfix_token(token: Token) -> Option<Self> {
        match Self::from_postfix_tok(&token.tok) {
            Some(postfix) => Some(Self {
                context: MaybeNodeContext::move_token(token),
                value: postfix,
            }),
            None => None,
        }
    }

    pub fn from_prefix_tok(token: &Tok) -> Option<Operator> {
        match token {
            Tok::Plus => Some(Operator::Plus),
            Tok::Minus => Some(Operator::Minus),
            Tok::Exclamation => Some(Operator::Not),
            _ => None,
        }
    }
    pub fn from_prefix_token(token: &Token) -> Option<Self> {
        match Self::from_prefix_tok(&token.tok) {
            Some(prefix) => Some(Self {
                context: MaybeNodeContext::from_token(&token),
                value: prefix,
            }),
            None => None,
        }
    }

    pub fn from_token(token: &Token) -> Option<Self> {
        match Operator::from_tok(&token.tok) {
            Some(prefix) => Some(Self {
                context: MaybeNodeContext::from_token(&token),
                value: prefix,
            }),
            None => None,
        }
    }

    pub fn from_tokens(prefix: Operator, pre: Vec<Tok>, post: Vec<Tok>) -> Self {
        let mut context = MaybeNodeContext::default();
        context.prepend(pre);
        context.append(post);
        Self {
            value: prefix,
            context,
        }
    }

    pub fn new(prefix: Operator) -> Self {
        Self {
            context: MaybeNodeContext::default(),
            value: prefix,
        }
    }

    pub fn token(&self) -> Tok {
        self.value.token()
    }
}

impl Unparse for OperatorNode {
    fn unparse(&self) -> Vec<Tok> {
        self.context.unparse(vec![self.value.token()])
    }

    fn unlex(&self) -> String {
        self.value.token().unlex()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Not,
    Divide,
    Multiply,
    Exp,
    Equal,
    NotEqual,
    And,
    GreaterThanEqual,
    LessThanEqual,
    GreaterThan,
    LessThan,
    Assign,
    Declare,
    Bang,
    Modulus,
    Index,
    Call,
    Elvis,
    Conditional,
    ConditionalElse,
    End,
    Comma, //Map,
}
impl Operator {
    pub fn token(&self) -> Tok {
        match self {
            Operator::And => Tok::And,
            Operator::Declare => Tok::Let,
            Operator::Plus => Tok::Plus,
            Operator::Minus => Tok::Minus,
            Operator::Not => Tok::Exclamation,
            Operator::Multiply => Tok::Mul,
            Operator::Divide => Tok::Div,
            Operator::Exp => Tok::Caret,
            Operator::Equal => Tok::Equals,
            Operator::NotEqual => Tok::NotEquals,
            Operator::LessThanEqual => Tok::LTE,
            Operator::GreaterThanEqual => Tok::GTE,
            Operator::LessThan => Tok::LT,
            Operator::GreaterThan => Tok::GT,
            Operator::Assign => Tok::Assign,
            Operator::Modulus => Tok::Percent,
            Operator::Bang => Tok::Exclamation,
            Operator::Index => Tok::LBracket,
            Operator::Call => Tok::LParen,
            Operator::Elvis => Tok::Elvis,
            Operator::ConditionalElse => Tok::Colon,
            Operator::Conditional => Tok::Question,
            Operator::End => Tok::SemiColon,
            Operator::Comma => Tok::Comma,
            //Operator::Map => Tok::LeftArrow,
        }
    }

    pub fn from_tok(token: &Tok) -> Option<Self> {
        match token {
            Tok::And => Some(Operator::And),
            Tok::Equals => Some(Operator::Equal),
            Tok::NotEquals => Some(Operator::NotEqual),
            Tok::LTE => Some(Operator::LessThanEqual),
            Tok::GTE => Some(Operator::GreaterThanEqual),
            Tok::LT => Some(Operator::LessThan),
            Tok::GT => Some(Operator::GreaterThan),
            Tok::Plus => Some(Operator::Plus),
            Tok::Minus => Some(Operator::Minus),
            Tok::Mul => Some(Operator::Multiply),
            Tok::Div => Some(Operator::Divide),
            Tok::Caret => Some(Operator::Exp),
            Tok::LParen => Some(Operator::Call),
            Tok::LBracket => Some(Operator::Index),
            Tok::Assign => Some(Operator::Assign),
            Tok::Percent => Some(Operator::Modulus),
            Tok::Comma => Some(Operator::Comma),
            Tok::Elvis => Some(Operator::Elvis),
            _ => None,
        }
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn prefix() {
        let p = OperatorNode::new(Operator::Minus);
        assert_eq!(p.unlex(), "-");
    }
}
