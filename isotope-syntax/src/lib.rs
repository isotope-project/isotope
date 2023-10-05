/*!
Syntax and parsing for `isotope` IR
*/
#![forbid(missing_docs, missing_debug_implementations, unsafe_code)]
use ecow::EcoString;
use std::fmt::{Debug, Display};
use winnow::ascii::{multispace0, multispace1};
use winnow::combinator::{delimited, separated0, separated_pair};
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
    pub fn parser(input: &mut &str) -> PResult<Expr> {
        dispatch! { Expr::atom;
            Expr::Ident(first) => dispatch! { opt((multispace0, "="));
                Some(_) => (
                    multispace0,
                    Expr::parser,
                    multispace0, ";", multispace0,
                    Expr::parser).map(|(_, a, _, _, _, e)| Expr::Let(Let {
                        def: Def {
                            pattern: Pattern::Ident(first.clone()),
                            value: Box::new(a)
                        },
                        expr: Box::new(e)
                    })),
                None => opt(preceded(multispace0, Expr::atom)).map(|arg| {
                    let first = first.clone();
                    match arg {
                        Some(arg) => Expr::App(App { func: first, arg: Box::new(arg) }),
                        None => Expr::Ident(first)
                    }
                })
            },
            Expr::Tuple(tuple)
                if tuple.0.iter().all(Expr::is_pattern)
                => dispatch! { opt((multispace0, "="));
                Some(_) => (
                    multispace0,
                    Expr::parser,
                    multispace0, ";", multispace0,
                    Expr::parser).map(|(_, a, _, _, _, e)| Expr::Let(Let {
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
                    separated0(Expr::parser, (multispace0, ",", multispace0)),
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
    /// The argument of this basic block
    pub arg: Pattern,
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
    pub sub_blocks: Vec<(Label, Target)>,
}

impl From<Expr> for Block {
    fn from(value: Expr) -> Self {
        Block {
            defs: vec![],
            terminator: Terminator::Return(value),
            sub_blocks: vec![],
        }
    }
}

impl From<Branch> for Block {
    fn from(value: Branch) -> Self {
        Block {
            defs: vec![],
            terminator: Terminator::Branch(value),
            sub_blocks: vec![],
        }
    }
}

impl From<Switch> for Block {
    fn from(value: Switch) -> Self {
        Block {
            defs: vec![],
            terminator: Terminator::Switch(value),
            sub_blocks: vec![],
        }
    }
}

impl Block {
    /// Parse a block
    pub fn parser(input: &mut &str) -> PResult<Block> {
        (
            separated0(Def::parser, multispace0),
            multispace0,
            Terminator::parser,
            opt(delimited(
                (multispace1, "#where", multispace0, "{", multispace0),
                separated0(
                    (
                        Label::parser,
                        (
                            multispace1,
                            Pattern::parser,
                            multispace0,
                            "=>",
                            multispace0,
                            Block::parser,
                        )
                            .map(|(_, arg, _, _, _, block)| Target { arg, block }),
                    ),
                    (multispace0, ",", multispace0),
                ),
                (multispace0, opt(","), multispace0, "}"),
            )),
        )
            .map(
                |(defs, _, terminator, sub_blocks): (_, _, _, Option<Vec<(Label, Target)>>)| {
                    Block {
                        defs,
                        terminator,
                        sub_blocks: sub_blocks.unwrap_or_default(),
                    }
                },
            )
            .parse_next(input)
    }
}

/// A terminator
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum Terminator {
    /// An unconditional branch
    Branch(Branch),
    /// A switch
    Switch(Switch),
    /// A return value
    Return(Expr),
}

impl Terminator {
    /// Parse a terminator
    pub fn parser(input: &mut &str) -> PResult<Terminator> {
        alt((
            Branch::parser.map(Terminator::Branch),
            Switch::parser.map(Terminator::Switch),
            Expr::parser.map(Terminator::Return),
        ))
        .parse_next(input)
    }
}

/// A definition, which binds a pattern to a (potentially effectful) expression
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Def {
    /// The pattern being bound
    pub pattern: Pattern,
    /// The value the pattern is being given
    pub value: Box<Expr>,
}

impl Def {
    /// Parse a definition
    pub fn parser(input: &mut &str) -> PResult<Def> {
        (
            Pattern::parser,
            multispace0,
            "=",
            multispace0,
            Expr::parser,
            multispace0,
            ";",
        )
            .map(|(pattern, _, _, _, value, _, _)| Def {
                pattern,
                value: Box::new(value),
            })
            .parse_next(input)
    }
}

/// A pattern for a variable binding
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum Pattern {
    /// A name
    Ident(Ident),
    /// A tuple of patterns
    Tuple(Vec<Pattern>),
    /// An annotated pattern
    Annotated(Box<Pattern>, Type),
}

impl Pattern {
    /// Parse a pattern
    pub fn parser(input: &mut &str) -> PResult<Pattern> {
        (
            separated0(
                (
                    Pattern::atom,
                    opt(preceded((multispace0, ":", multispace0), Type::parser)),
                )
                    .map(|(pattern, annot)| {
                        if let Some(annot) = annot {
                            Pattern::Annotated(Box::new(pattern), annot)
                        } else {
                            pattern
                        }
                    }),
                (multispace0, ",", multispace0),
            ),
            opt((multispace0, ",")),
        )
            .map(|(mut components, trailing): (Vec<_>, _)| {
                if components.len() != 1 || trailing.is_some() {
                    Pattern::Tuple(components)
                } else {
                    components.remove(0)
                }
            })
            .parse_next(input)
    }

    /// Parse an atomic pattern
    pub fn atom(input: &mut &str) -> PResult<Pattern> {
        alt((
            delimited(
                ("(", multispace0),
                opt(Pattern::parser).map(Option::unwrap_or_default),
                (multispace0, ")"),
            ),
            Ident::parser.map(Pattern::Ident),
        ))
        .parse_next(input)
    }
}

impl Default for Pattern {
    #[inline]
    fn default() -> Self {
        Pattern::Tuple(vec![])
    }
}

/// A type
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum Type {
    /// A name
    Ident(Ident),
    /// A tuple type
    Tuple(Vec<Type>),
}

impl Type {
    /// Parse a type
    pub fn parser(input: &mut &str) -> PResult<Type> {
        alt((
            delimited(
                ("(", multispace0),
                separated_pair(
                    separated0(Type::parser, (multispace0, ",", multispace0)),
                    multispace0,
                    opt(","),
                )
                .map(|(mut components, trailing): (Vec<_>, _)| {
                    if components.len() != 1 || trailing.is_some() {
                        Type::Tuple(components)
                    } else {
                        components.remove(0)
                    }
                }),
                (multispace0, ")"),
            ),
            Ident::parser.map(Type::Ident),
        ))
        .parse_next(input)
    }
}

impl Default for Type {
    #[inline]
    fn default() -> Self {
        Type::Tuple(vec![])
    }
}

/// An unconditional branch
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Branch {
    /// The target label, or `None` for a return
    pub target: Label,
    /// The argument of this branch
    pub arg: Expr,
}

impl Branch {
    /// Parse a branch
    pub fn parser(input: &mut &str) -> PResult<Branch> {
        preceded(
            ("#br", multispace1),
            separated_pair(Label::parser, multispace1, Expr::parser),
        )
        .map(|(target, arg)| Branch { target, arg })
        .parse_next(input)
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
    pub targets: Vec<(Bitvector, Block)>,
    /// The default target block
    pub default: Option<Box<Block>>,
}

impl Switch {
    /// Parse a switch statement
    pub fn parser(input: &mut &str) -> PResult<Switch> {
        (
            "#switch",
            multispace1,
            Expr::atom,
            multispace1,
            "{",
            multispace0,
            separated0(
                separated_pair(
                    Bitvector::parser,
                    (multispace0, "=>", multispace0),
                    Block::parser,
                ),
                (multispace0, ",", multispace0),
            ),
            multispace0,
            opt((multispace0, ",", multispace0)),
            opt(preceded(("_", multispace0, "=>", multispace0), Block::parser).map(Box::new)),
            opt((multispace0, ",", multispace0)),
            multispace0,
            "}",
        )
            .map(
                |(_, _, disc, _, _, _, targets, _, _, default, _, _, _)| Switch {
                    disc,
                    targets,
                    default,
                },
            )
            .parse_next(input)
    }
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
        assert_eq!(Type::default(), Type::Tuple(vec![]));
        assert_eq!(Expr::default().to_pattern().unwrap(), Pattern::default());
        assert_eq!(Expr::parser.parse("()").unwrap(), Default::default());
        assert_eq!(Expr::parser.parse("(,)").unwrap(), Default::default());
        assert_eq!(Expr::atom.parse("()").unwrap(), Default::default());
        assert_eq!(Expr::atom.parse("(,)").unwrap(), Default::default());
        assert_eq!(Pattern::parser.parse("()").unwrap(), Default::default());
        assert_eq!(Pattern::parser.parse("(,)").unwrap(), Default::default());
        assert_eq!(Pattern::atom.parse("()").unwrap(), Default::default());
        assert_eq!(Pattern::atom.parse("(,)").unwrap(), Default::default());
        assert_eq!(
            Pattern::parser.parse("(),").unwrap(),
            Pattern::Tuple(vec![Default::default()])
        );
        assert_eq!(
            Pattern::parser.parse("(),()").unwrap(),
            Pattern::Tuple(vec![Default::default(), Default::default()])
        );
        Pattern::atom.parse("(),").unwrap_err();
        Pattern::atom.parse("(),()").unwrap_err();
        assert_eq!(Type::parser.parse("()").unwrap(), Default::default());
        assert_eq!(Type::parser.parse("(,)").unwrap(), Default::default());
        Type::parser.parse("(),").unwrap_err();
        assert_eq!(
            Pattern::parser.parse("(): ()").unwrap(),
            Pattern::Annotated(Default::default(), Default::default())
        );
        assert_eq!(
            Expr::parser.parse("() = (); ()").unwrap(),
            Expr::Let(Let {
                def: Def {
                    pattern: Pattern::default(),
                    value: Default::default()
                },
                expr: Default::default()
            })
        );
        assert_eq!(Block::parser.parse("()").unwrap(), Expr::default().into());
        assert_eq!(
            Switch::parser.parse("#switch () {,,}").unwrap(),
            Switch {
                disc: Default::default(),
                targets: vec![],
                default: None
            }
        );
        //TODO: disallow these behaviours? For consistency with while, allow arbitrary commas?
        assert_eq!(
            Switch::parser.parse("#switch () {,}").unwrap(),
            Switch {
                disc: Default::default(),
                targets: vec![],
                default: None
            }
        );
        assert_eq!(
            Switch::parser.parse("#switch () {,,}").unwrap(),
            Switch {
                disc: Default::default(),
                targets: vec![],
                default: None
            }
        );
    }

    proptest! {
        #[test]
        fn ident_parsing(i in "[[:alpha:]][[:alnum:]]*") {
            assert_eq!(
                Expr::parser.parse(&i).unwrap(),
                Expr::ident(&*i)
            );
            assert_eq!(
                Expr::parser.parse(&format!("({i})")).unwrap(),
                Expr::ident(&*i)
            );
            assert_eq!(
                Expr::parser.parse(&format!("(({i}))")).unwrap(),
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
            let e = Expr::parser.parse(&b).unwrap();
            assert_eq!(e, Expr::Bitvector(bv));
            assert_eq!(e.to_pattern(), Err(()));
        }

        #[test]
        fn switch_parsing(
            d in "[[:alpha:]][[:alnum:]]*",
            l in "[-]??[1-9][0-9]{0,3}['][bdoh][[:xdigit:]]+",
            r in "[-]??[1-9][0-9]{0,3}['][bdoh][[:xdigit:]]+"
        ) {
            let es = [
                format!("#switch {d} {{ {l} => {r}, {r} => {l}, _ => {d} }}"),
                format!("#switch {d} {{ {l} => {r}, {r} => {l}, _ => {d}, }}"),
            ];
            let bl = Bitvector::parser.parse(&*l).unwrap();
            let br = Bitvector::parser.parse(&*r).unwrap();

            for e in es {
                let sp = Switch::parser.parse(&e).unwrap();
                let bp = Block::parser.parse(&e).unwrap();
                assert_eq!(
                    sp,
                    Switch {
                        disc: Expr::ident(&*d),
                        targets: vec![
                            (bl.clone(), Expr::Bitvector(br.clone()).into()),
                            (br.clone(), Expr::Bitvector(bl.clone()).into()),
                        ],
                        default: Some(Box::new(Expr::ident(&*d).into()))
                    }
                );
                assert_eq!(bp, sp.into());
            }


            let es = [
                format!("#switch {d} {{ {l} => {r}, {r} => {l}, _ => #br 'l {d} }} #where {{ 'l x => x }}"),
                format!("#switch {d} {{ {l} => {r}, {r} => {l}, _ => #br 'l {d} }} #where {{ 'l x => x, }}"),
                format!("#switch {d} {{ {l} => {r}, {r} => {l}, _ => #br 'l {d}, }} #where {{ 'l x => x }}"),
                format!("#switch {d} {{ {l} => {r}, {r} => {l}, _ => #br 'l {d}, }} #where {{ 'l x => x, }}"),
            ];
            for e in es {
                let bp = Block::parser.parse(&e).unwrap();
                assert_eq!(
                    bp,
                    Block {
                        defs: vec![],
                        terminator: Terminator::Switch(Switch {
                            disc: Expr::ident(&*d),
                            targets: vec![
                                (bl.clone(), Expr::Bitvector(br.clone()).into()),
                                (br.clone(), Expr::Bitvector(bl.clone()).into()),
                            ],
                            default: Some(Box::new(Branch { target: Label(Ident("l".into())), arg: Expr::ident(&*d).into() }.into()))
                        }),
                        sub_blocks: vec![
                            (
                                Label(Ident("l".into())),
                                Target {
                                    arg: Pattern::Ident(Ident("x".into())),
                                    block: Expr::ident("x").into()
                                }
                            )
                        ]
                    }
                );
            }
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
                    Expr::parser.parse(&e).unwrap(),
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
                    Expr::parser.parse(&e).unwrap(),
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
                let exp = Expr::parser.parse(&e).unwrap();
                let ptp = Pattern::parser.parse(&e).unwrap();
                assert_eq!(
                    exp,
                    Expr::Tuple(Tuple(vec![Expr::ident(&*f), Expr::ident(&*x)]))
                );
                assert_eq!(
                    ptp,
                    Pattern::Tuple(vec![Pattern::Ident(Ident((&*f).into())), Pattern::Ident(Ident((&*x).into()))])
                );
                assert!(exp.is_pattern());
                assert_eq!(
                    exp.to_pattern(),
                    Ok(ptp)
                );
                assert_eq!(
                    Type::parser.parse(&e).unwrap(),
                    Type::Tuple(vec![Type::Ident(Ident((&*f).into())), Type::Ident(Ident((&*x).into()))])
                )
            }
            let es = [
                format!("{x} = {f}; {x}"),
                format!("{x} = ({f}); {x}"),
                format!("({x})={f}; ({x})"),
            ];
            for e in es {
                assert_eq!(
                    Expr::parser.parse(&e).unwrap(),
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
                format!("{x} = {f}; {x}"),
                format!("{x} = ({f}); {x}"),
                format!("({x})={f}; ({x})"),
            ];
            for e in es {
                assert_eq!(
                    Block::parser.parse(&e).unwrap(),
                    Block {
                        defs: vec![
                            Def {
                                pattern: Pattern::Ident(Ident((&*x).into())),
                                value: Box::new(Expr::ident(&*f))
                            }
                        ],
                        terminator: Terminator::Return(Expr::ident(&*x)),
                        sub_blocks: vec![]
                    }
                )
            }
            let es = [
                format!("{x} = {f}; #br '{f} {x}"),
                format!("{x} = ({f}); #br '{f} {x}"),
                format!("({x})={f}; #br '{f} ({x})"),
            ];
            for e in es {
                assert_eq!(
                    Block::parser.parse(&e).unwrap(),
                    Block {
                        defs: vec![
                            Def {
                                pattern: Pattern::Ident(Ident((&*x).into())),
                                value: Box::new(Expr::ident(&*f))
                            }
                        ],
                        terminator: Terminator::Branch(Branch { target: Label(Ident((&*f).into())), arg: Expr::ident(&*x) }),
                        sub_blocks: vec![]
                    }
                )
            }
            let es = [
                format!("{x} = {x} = {f}; {x}; {x}"),
                format!("{x} = ({x} = {f}; {x}); {x}"),
                format!("{x} = {x} = {f}; ({x}); {x}"),
            ];
            for e in es {
                assert_eq!(
                    Expr::parser.parse(&e).unwrap(),
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
                    Expr::parser.parse(&e).unwrap(),
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
                format!("{x},={f};"),
                format!("({x},)={f};"),
                format!("({x},)=({f});"),
                format!("(({x},))=({f});"),
                format!("(({x},))={f};"),
                format!("(({x}),)={f};"),
            ];
            for e in es {
                assert_eq!(
                    Def::parser.parse(&e).unwrap(),
                    Def {
                        pattern: Pattern::Tuple(vec![Pattern::Ident(Ident((&*x).into()))]),
                        value: Box::new(Expr::ident(&*f))
                    }
                )
            }
            let es = [
                format!("(({x},),)={f}; ({x})"),
            ];
            for e in es {
                assert_eq!(
                    Expr::parser.parse(&e).unwrap(),
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
