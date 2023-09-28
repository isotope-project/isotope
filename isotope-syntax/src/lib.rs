/*!
Syntax and parsing for `isotope` IR
*/
#![forbid(missing_docs, missing_debug_implementations, unsafe_code)]
use ecow::EcoString;
use std::collections::BTreeMap;
use std::fmt::{Debug, Display};
use std::sync::Arc;
use winnow::prelude::*;
use winnow::{
    ascii::{dec_uint, hex_digit1, multispace1},
    combinator::{alt, dispatch, fail, opt, preceded, success},
    token::any,
};

pub mod primitive;
use primitive::*;

mod ident;
pub use ident::*;

/// A statement
#[derive(Debug)]
pub enum Statement {}

/// An expression
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum Expr {
    /// An identifier
    Ident(Ident),
    /// An application
    App(App),
    /// A tuple
    Tuple(Tuple),
    /// A let-binding
    Let(LetExpr),
    /// A bitvector
    Bitvector(Bitvector),
}

impl Expr {
    /// Parse an expression
    pub fn expr(input: &mut &str) -> PResult<Expr> {
        dispatch! { Expr::atom;
            Expr::Ident(first) => opt(preceded(multispace1, Expr::atom)).map(|arg| {
                let first = first.clone();
                match arg {
                    Some(arg) => Expr::App(App { func: first, arg: Arc::new(arg) }),
                    None => Expr::Ident(first)
                }
            }),
            expr => success(expr)
        }
        .parse_next(input)
    }

    /// Parse an atomic expression
    pub fn atom(input: &mut &str) -> PResult<Expr> {
        //TODO: parse tuples
        alt((
            Bitvector::parser.map(Expr::Bitvector),
            Ident::parser.map(Expr::Ident),
        ))
        .parse_next(input)
    }
}

/// A function application
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct App {
    /// The function being applied
    pub func: Ident,
    /// The argument the function is being applied to
    pub arg: Arc<Expr>,
}

/// A tuple
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Tuple(pub Vec<Arc<Expr>>);

/// A let-binding expression
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct LetExpr {
    /// The name of the variables being bound
    pub name: Vec<Ident>,
    /// The value the variable are being given
    pub value: Arc<Expr>,
    /// The expression in which the variable is used
    pub expr: Arc<Expr>,
}

/// A block
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum Block {
    /// A let-binding
    Let(LetBlock),
    /// A where-binding
    Where(WhereBlock),
    /// A switch-statement
    Switch(Switch),
    /// An unconditional jump
    Jump(Jump),
}

/// A let-binding block
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct LetBlock {
    /// The name of the variable being bound
    pub name: Ident,
    /// The value the variable is being given
    pub value: Arc<Expr>,
    /// The block in which the variable is used
    pub expr: Arc<Block>,
}

/// A mutually-recursive control-flow block
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct WhereBlock {
    /// The labels being bound
    pub labels: BTreeMap<Label, PBlock>,
    /// The block in which the labels are being used
    pub block: Arc<Block>,
}

/// A block parametrized with an argument vector
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct PBlock {
    /// The arguments of this block
    pub args: Vec<(Ident, Ident)>,
    /// The block itself
    pub block: Arc<Block>,
}

/// A switch statement
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Switch {
    /// The expression being used as discriminant
    pub disc: Arc<Expr>,
    /// The potential target blocks
    pub targets: BTreeMap<Bitvector, Arc<Block>>,
    /// The default target block
    pub default: Option<Arc<Block>>,
}

/// An unconditional jump
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Jump {
    /// The target label
    pub target: Label,
    /// The argument vector
    pub args: Vec<Arc<Expr>>,
}
