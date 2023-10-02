/*!
Syntax and parsing for `isotope` IR
*/
#![forbid(missing_docs, missing_debug_implementations, unsafe_code)]
use ecow::EcoString;
use std::collections::BTreeMap;
use std::fmt::{Debug, Display};
use winnow::ascii::multispace0;
use winnow::combinator::delimited;
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
    Let(Let),
    /// A bitvector
    Bitvector(Bitvector),
}

impl Expr {
    /// Construct an identifier expression
    pub fn ident(s: impl Into<EcoString>) -> Expr {
        Expr::Ident(Ident(s.into()))
    }
}

impl Default for Expr {
    #[inline]
    fn default() -> Self {
        Expr::Tuple(Tuple::default())
    }
}

impl From<Ident> for Expr {
    #[inline]
    fn from(value: Ident) -> Self {
        Expr::Ident(value)
    }
}

impl Expr {
    /// Parse an expression
    pub fn expr(input: &mut &str) -> PResult<Expr> {
        dispatch! { Expr::atom;
            Expr::Ident(first) => opt(preceded(multispace1, Expr::atom)).map(|arg| {
                let first = first.clone();
                match arg {
                    Some(arg) => Expr::App(App { func: first, arg: Box::new(arg) }),
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
            delimited(("(", multispace0), Expr::expr, (multispace0, ")")),
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
    pub arg: Box<Expr>,
}

/// A let-binding
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Let {
    /// The variables being defined
    pub def: Def,
    /// The expression in which the variables are used
    pub expr: Box<Expr>,
}

/// A tuple
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Default)]
pub struct Tuple(pub Vec<Expr>);

/// A target, which is a basic block equipped with an argument vector
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Target {
    /// The arguments of this basic block
    pub args: Vec<(Ident, Ident)>,
    /// The basic block itself
    pub block: Block,
}

/// A basic block
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Block {
    /// This block's definitions
    pub defs: Vec<Def>,
    /// This block's terminator
    pub terminator: Terminator,
    /// This block's sub-blocks
    pub sub_blocks: BTreeMap<Label, Target>,
}

/// A terminator
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum Terminator {
    /// A switch
    Switch(Switch),
    /// An unconditional jump
    Jump(Jump),
}

/// A definition, which binds a pattern to a (potentially effectful) expression
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Def {
    /// The pattern being bound
    pub pattern: Pattern,
    /// The value the pattern is being given
    pub value: Box<Expr>,
}

/// A pattern for a variable binding
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum Pattern {
    /// A name
    Name(Ident),
    /// A tuple of patterns
    Tuple(Vec<Pattern>),
}

impl Default for Pattern {
    #[inline]
    fn default() -> Self {
        Pattern::Tuple(vec![])
    }
}

/// A switch statement
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Switch {
    /// The expression being used as discriminant
    pub disc: Expr,
    /// The potential target blocks
    ///
    /// Note that this is exactly equivalent to a jump to an anonymous label with a unit argument
    pub targets: BTreeMap<Bitvector, Block>,
    /// The default target block
    pub default: Option<Box<Block>>,
}

/// An unconditional jump
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Jump {
    /// The target label
    pub target: Label,
    /// The argument vector
    pub args: Vec<Expr>,
}

#[cfg(test)]
mod test {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn ident_parsing(i in "[[:alpha:]][[:alnum:]]*") {
            assert_eq!(
                Expr::expr.parse(&i).unwrap(),
                Expr::ident(&*i)
            );
            assert_eq!(
                Expr::expr.parse(&format!("({i})")).unwrap(),
                Expr::ident(&*i)
            );
            assert_eq!(
                Expr::expr.parse(&format!("(({i}))")).unwrap(),
                Expr::ident(&*i)
            );
        }

        #[test]
        fn ident_app_parsing(f in "[[:alpha:]][[:alnum:]]*", x in "[[:alpha:]][[:alnum:]]*") {
            let es = [
                format!("{f} {x}"),
                format!("({f} {x})"),
                format!("(({f} {x}))"),
            ];
            for e in es {
                assert_eq!(
                    Expr::expr.parse(&e).unwrap(),
                    Expr::App(App { func: Ident((&*f).into()), arg: Expr::ident(&*x).into() })
                )
            }
        }
    }
}
