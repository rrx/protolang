use nom::*;

use crate::tokens::TokensList;
use log::debug;
use nom::error::{ContextError, ErrorKind, VerboseErrorKind};
use std::result::Result::*;

#[derive(Debug)]
pub struct DebugError<I> {
    pub message: String,
    pub errors: Vec<(I, VerboseErrorKind)>,
}

impl<I: std::fmt::Debug + TokensList> nom::error::ParseError<I> for DebugError<I> {
    // on one line, we show the error code and the input that caused it
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        let message = format!("kind {:?}:\t{:?}\n", kind, input.toks());
        //debug!("T:from:{}", message);
        DebugError {
            message,
            errors: vec![(input, VerboseErrorKind::Nom(kind))],
        }
    }

    // if combining multiple errors, we show them one after the other
    fn append(input: I, kind: ErrorKind, mut other: Self) -> Self {
        other.message = format!("{}kind {:?}:\t{:?}\n", other.message, kind, input.toks());
        //debug!("T:append:{}", other.message);
        //DebugError {
        //message,
        other.errors.push((input, VerboseErrorKind::Nom(kind)));
        //}
        other
    }

    fn from_char(input: I, c: char) -> Self {
        let message = format!("'{}':\t{:?}\n", c, input.toks());
        //debug!("T:char:{}", message);
        DebugError {
            message,
            errors: vec![(input, VerboseErrorKind::Char(c))],
        }
    }

    fn or(self, other: Self) -> Self {
        let message = format!("{}\tOR\n{}\n", self.message, other.message);
        //debug!("T {}", message);
        DebugError {
            message,
            errors: vec![],
        }
    }
}

impl<I: std::fmt::Debug + TokensList> ContextError<I> for DebugError<I> {
    fn add_context(input: I, ctx: &'static str, other: Self) -> Self {
        let message = format!(
            "{}\"context-{}\":\t{:?}\n",
            other.message,
            ctx,
            input.toks()
        );
        //debug!("context {}", message);
        DebugError {
            message,
            errors: vec![],
        }
    }
}

pub(crate) type PResult<I, O> = IResult<I, O, DebugError<I>>;

pub(crate) fn print_result<
    I: std::fmt::Debug + crate::tokens::TokensList,
    O: std::fmt::Debug + crate::sexpr::SExpr,
>(
    r: &PResult<I, O>,
) {
    match r {
        Ok((_, expr)) => {
            //debug!("Ok({:?})", (&expr));
            match expr.sexpr() {
                Ok(sexpr) => {
                    debug!("sexpr {}", &sexpr);
                    let rendered = format!("{}", &sexpr);
                    debug!("sexpr {:?}", (&sexpr, &rendered));
                }
                Err(e) => {
                    debug!("Error: {:?}", e);
                }
            }
        }
        Err(nom::Err::Error(e)) => {
            debug!("err: {:?}", e);
            for (tokens, err) in &e.errors {
                debug!("error {:?}", (&err, tokens.toks()));
            }
        }
        Err(e) => {
            debug!("err: {}", e);
        }
    }
}


