/*!
Syntax and parsing for `isotope` IR
*/
#![forbid(missing_docs, missing_debug_implementations, unsafe_code)]
use ecow::EcoString;
use std::collections::BTreeMap;
use std::fmt::{Debug, Display};
use winnow::ascii::multispace0;
use winnow::combinator::{delimited, separated0};
use winnow::prelude::*;
use winnow::{
    ascii::{dec_uint, hex_digit1},
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

    /// Whether an expression is a valid let-pattern
    pub fn is_pattern(&self) -> bool {
        match self {
            Expr::Ident(_) => true,
            Expr::Tuple(t) => t.0.iter().all(Expr::is_pattern),
            _ => false,
        }
    }

    /// Convert an expression to a pattern, if possible
    pub fn to_pattern(&self) -> Result<Pattern, ()> {
        match self {
            Expr::Ident(i) => Ok(Pattern::Ident(i.clone())),
            Expr::Tuple(t) => {
                t.0.iter()
                    .map(Expr::to_pattern)
                    .collect::<Result<_, _>>()
                    .map(Pattern::Tuple)
            }
            _ => Err(()),
        }
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
            Expr::Ident(first) => dispatch! { preceded(multispace0, opt("="));
                Some(_) => (
                    multispace0,
                    Expr::expr,
                    multispace0, ";", multispace0,
                    Expr::expr).map(|(_, a, _, _, _, e)| Expr::Let(Let {
                        def: Def {
                            pattern: Pattern::Ident(first.clone()),
                            value: Box::new(a)
                        },
                        expr: Box::new(e)
                    })),
                None => opt(Expr::atom).map(|arg| {
                    let first = first.clone();
                    match arg {
                        Some(arg) => Expr::App(App { func: first, arg: Box::new(arg) }),
                        None => Expr::Ident(first)
                    }
                })
            },
            Expr::Tuple(tuple)
                if tuple.0.iter().all(Expr::is_pattern)
                => dispatch! { preceded(multispace0, opt("="));
                Some(_) => (
                    multispace0,
                    Expr::expr,
                    multispace0, ";", multispace0,
                    Expr::expr).map(|(_, a, _, _, _, e)| Expr::Let(Let {
                        def: Def {
                            pattern: Pattern::Tuple(tuple.0.iter().map(|e| e.to_pattern().unwrap()).collect()),
                            value: Box::new(a)
                        },
                        expr: Box::new(e)
                    })),
                None => success(Expr::Tuple(tuple.clone()))
            },
            expr => success(expr)
        }
        .parse_next(input)
    }

    /// Parse an atomic expression
    pub fn atom(input: &mut &str) -> PResult<Expr> {
        alt((
            delimited(
                ("(", multispace0),
                (
                    separated0(Expr::expr, (multispace0, ",", multispace0)),
                    opt((multispace0, ",")),
                )
                    .map(|(mut v, t): (Vec<_>, _)| {
                        if v.len() == 1 && t.is_none() {
                            v.remove(0)
                        } else {
                            Expr::Tuple(Tuple(v))
                        }
                    }),
                (multispace0, ")"),
            ),
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
    Ident(Ident),
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

    #[test]
    fn empty_tuple_parses() {
        assert_eq!(Expr::default(), Expr::Tuple(Tuple(vec![])));
        assert_eq!(Tuple::default(), Tuple(vec![]));
        assert_eq!(Pattern::default(), Pattern::Tuple(vec![]));
        assert_eq!(Expr::default().to_pattern().unwrap(), Pattern::default());
        assert_eq!(Expr::expr.parse("()").unwrap(), Expr::default());
        assert_eq!(Expr::expr.parse("(,)").unwrap(), Expr::default());
    }

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
            assert_eq!(
                Expr::ident(&*i),
                Expr::from(Ident(i.into()))
            )
        }

        #[test]
        fn bitvector_parsing(b in "[-]??[1-9][0-9]{0,3}['][bdoh][[:xdigit:]]+") {
            let bv = Bitvector::parser.parse(&b).unwrap();
            let e = Expr::expr.parse(&b).unwrap();
            assert_eq!(e, Expr::Bitvector(bv));
            assert_eq!(e.to_pattern(), Err(()));
        }

        #[test]
        fn bin_ident_parsing(f in "[[:alpha:]][[:alnum:]]*", x in "[[:alpha:]][[:alnum:]]*") {
            let es = [
                format!("{f} {x}"),
                format!("{f} ({x})"),
                format!("({f} {x})"),
                format!("(({f} {x}))"),
                format!("(({f} ({x})))"),
            ];
            for e in es {
                assert_eq!(
                    Expr::expr.parse(&e).unwrap(),
                    Expr::App(App { func: Ident((&*f).into()), arg: Expr::ident(&*x).into() })
                )
            }
            let es = [
                format!("({f} {x},)"),
                format!("(({f} {x},))"),
                format!("(({f}({x}),))"),
            ];
            for e in es {
                assert_eq!(
                    Expr::expr.parse(&e).unwrap(),
                    Expr::Tuple(Tuple(vec![
                        Expr::App(App { func: Ident((&*f).into()), arg: Expr::ident(&*x).into() })]))
                )
            }
            let es = [
                format!("({f}, {x})"),
                format!("({f}, ({x}),)"),
                format!("(({f}, {x}))"),
                format!("(({f}, {x},))"),
                format!("({f},{x})"),
                format!("({f},({x}),)"),
                format!("(({f},{x}))"),
                format!("(({f},{x},))"),
                format!("({f},{x}, )"),
                format!("({f} ,{x} , )"),
            ];
            for e in es {
                assert_eq!(
                    Expr::expr.parse(&e).unwrap(),
                    Expr::Tuple(Tuple(vec![Expr::ident(&*f), Expr::ident(&*x)]))
                )
            }
            let es = [
                format!("{x} = {f}; {x}"),
                format!("{x} = ({f}); {x}"),
                format!("({x})={f}; ({x})"),
            ];
            for e in es {
                assert_eq!(
                    Expr::expr.parse(&e).unwrap(),
                    Expr::Let(Let {
                        def: Def {
                            pattern: Pattern::Ident(Ident((&*x).into())),
                            value: Box::new(Expr::ident(&*f))
                        },
                        expr: Box::new(Expr::ident(&*x))
                    })
                )
            }
            let es = [
                format!("{x} = {x} = {f}; {x}; {x}"),
                format!("{x} = ({x} = {f}; {x}); {x}"),
                format!("{x} = {x} = {f}; ({x}); {x}"),
            ];
            for e in es {
                assert_eq!(
                    Expr::expr.parse(&e).unwrap(),
                    Expr::Let(Let {
                        def: Def {
                            pattern: Pattern::Ident(Ident((&*x).into())),
                            value: Box::new(Expr::Let(Let {
                                def: Def {
                                    pattern: Pattern::Ident(Ident((&*x).into())),
                                    value: Box::new(Expr::ident(&*f))
                                },
                                expr: Box::new(Expr::ident(&*x))
                            }))
                        },
                        expr: Box::new(Expr::ident(&*x))
                    })
                )
            }
            let es = [
                format!("({x},)={f}; ({x})"),
                format!("({x},)=({f});{x}"),
            ];
            for e in es {
                assert_eq!(
                    Expr::expr.parse(&e).unwrap(),
                    Expr::Let(Let {
                        def: Def {
                            pattern: Pattern::Tuple(vec![Pattern::Ident(Ident((&*x).into()))]),
                            value: Box::new(Expr::ident(&*f))
                        },
                        expr: Box::new(Expr::ident(&*x))
                    })
                )
            }
            let es = [
                format!("(({x},),)={f}; ({x})"),
            ];
            for e in es {
                assert_eq!(
                    Expr::expr.parse(&e).unwrap(),
                    Expr::Let(Let {
                        def: Def {
                            pattern: Pattern::Tuple(vec![Pattern::Tuple(vec![Pattern::Ident(Ident((&*x).into()))])]),
                            value: Box::new(Expr::ident(&*f))
                        },
                        expr: Box::new(Expr::ident(&*x))
                    })
                )
            }
        }
    }
}
