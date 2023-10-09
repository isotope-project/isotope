/*!
In-memory representation for `isotope` IR in SSA form
*/
use egg_isotope as egg;

use egg::*;
use smallvec::SmallVec;

/// An `isotope` function in SSA form
#[derive(Debug, Clone, Default)]
pub struct Function {
    /// The basic blocks in this function
    blocks: Vec<Block>,
    /// The instructions in this function; blocks will refer to these by instruction ID
    instructions: Vec<Instruction>,
    /// The terms defined in this function
    terms: EGraph<IsotopeLanguage, IsotopeAnalysis>,
}

impl Function {
    /// Construct a new block
    pub fn insert_empty_block(&mut self) -> BlockId {
        let ix = self.instructions.len();
        BlockId(ix as u32)
    }

    /// Construct a new instruction in a basic block, without unpacking its arguments
    ///
    /// This will execute *after* the last current instruction, but *before* the terminator!
    pub fn push_instruction(&mut self, block: BlockId, expr: ExprId) -> ValId {
        let ix = ValId(self.terms.analysis.values.len() as u32);
        let iix = InstId(self.instructions.len() as u32);
        let value = Value {
            source: iix,
            ty: self.terms[expr.0].data.ty,
        };
        let instruction = Instruction {
            value: expr,
            begin: ix,
            end: ix,
            block,
        };
        self.terms.analysis.values.push(value);
        self.instructions.push(instruction);
        ix
    }

    // /// Construct a new instruction in a basic block, unpacking its arguments
    // pub fn push_unpacked_instruction(
    //     &mut self,
    //     block: BlockId,
    //     expr: ExprId,
    // ) -> Result<ValRange, ()> {
    //     Err(())
    // }

    /// Set the terminator of a block
    pub fn set_terminator(&mut self, block: BlockId, terminator: TerminatorId) -> Result<(), ()> {
        self.blocks.get_mut(block.0 as usize).ok_or(())?.terminator = terminator;
        Ok(())
    }
}

define_language! {
    enum IsotopeLanguage {
        // == Values ==
        // A local value
        ValId(ValId),
        // A tuple literal
        "tup" = Tup(Tuple),
        // An index into a tuple
        "ix" = Ix([Id; 2]),
        // An unresolved function call
        Call(GlobalId, Id),
        // A global value
        GlobalId(GlobalId),
        // An integer
        Integer(i64),

        // == Terminators ==
        // An unconditional branch
        Branch(BlockId, Id),
        // An if-then-else terminator
        "ite" = Ite([Id; 3]),
    }
}

define_language! {
    enum IsotopeKind {
        // A tuple
        "tup" = Tup(Tuple),
        // A bitvector of length n
        Bitvector(u32),
        // An integer
        "int" = Integer,
        // A type error
        "error" = TypeError,
    }
}

/// The analysis for expressions in an `isotope` function
#[derive(Debug, Clone, Default)]
struct IsotopeAnalysis {
    /// The values defined in this function
    values: Vec<Value>,
    /// The types used in this function
    kinds: EGraph<IsotopeKind, ()>,
}

impl Analysis<IsotopeLanguage> for IsotopeAnalysis {
    type Data = IsotopeMetadata;

    #[inline]
    fn make(egraph: &mut EGraph<IsotopeLanguage, Self>, enode: &IsotopeLanguage) -> Self::Data {
        match enode {
            IsotopeLanguage::ValId(v) => IsotopeMetadata {
                ty: egraph.analysis.values[v.0 as usize].ty,
            },
            IsotopeLanguage::Tup(t) => {
                let mapped =
                    IsotopeKind::Tup(Tuple(t.0.iter().map(|t| egraph[*t].data.ty.0).collect()));
                IsotopeMetadata {
                    ty: TypeId(egraph.analysis.kinds.add(mapped)),
                }
            }
            IsotopeLanguage::Ix(_) => IsotopeMetadata::default(), //TODO
            IsotopeLanguage::Call(_, _) => IsotopeMetadata::default(), //TODO
            IsotopeLanguage::GlobalId(_) => IsotopeMetadata::default(), //TODO
            IsotopeLanguage::Integer(_) => IsotopeMetadata {
                ty: TypeId(egraph.analysis.kinds.add(IsotopeKind::Integer)),
            },
            _ => IsotopeMetadata::default(),
        }
    }

    #[inline]
    fn merge(&mut self, a: &mut Self::Data, b: Self::Data) -> DidMerge {
        self.kinds.union(a.ty.0, b.ty.0);
        DidMerge(false, false)
    }
}

#[derive(Debug, Clone, Default, Eq, PartialEq, Hash, Ord, PartialOrd)]
struct IsotopeMetadata {
    // This value's type
    ty: TypeId,
}

/// A basic block in an `isotope` function
#[derive(Debug, Clone, Eq, PartialEq, Default)]
struct Block {
    /// This block's instructions
    instructions: Vec<InstId>,
    /// This block's terminator
    terminator: TerminatorId,
}

/// An instruction in an `isotope` function
#[derive(Debug, Clone, Eq, PartialEq)]
struct Instruction {
    /// The value of the instruction
    value: ExprId,
    /// The first child of the instruction
    begin: ValId,
    /// One-past the last child of the instruction, *or* the only child of the instruction if equal to `begin`
    end: ValId,
    /// The basic block of the instruction
    block: BlockId,
}

/// A value in an `isotope` function, which is one of the results of an instruction
#[derive(Debug, Clone, Eq, PartialEq)]
struct Value {
    /// The instruction this is a child of
    source: InstId,
    /// This value's type
    ty: TypeId,
}

/// A tuple of isotope expressions
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
struct Tuple(pub SmallVec<[Id; 4]>);

impl LanguageChildren for Tuple {
    #[inline]
    fn len(&self) -> usize {
        self.0.len()
    }

    #[inline]
    fn can_be_length(_: usize) -> bool {
        true
    }

    #[inline]
    fn from_vec(v: Vec<Id>) -> Self {
        Tuple(v.into())
    }

    #[inline]
    fn as_slice(&self) -> &[Id] {
        &self.0
    }

    #[inline]
    fn as_mut_slice(&mut self) -> &mut [Id] {
        &mut self.0
    }
}

/// The ID of an `isotope` expression
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct ExprId(Id);

/// The ID of an `isotope` terminator
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Default)]
pub struct TerminatorId(Id);

/// The ID of an `isotope` type
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Default)]
pub struct TypeId(Id);

/// The ID of an `isotope` value generated by an instruction
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct ValId(u32);

/// The ID of an `isotope` instruction
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
struct InstId(u32);

/// The ID of an `isotope` block
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct BlockId(u32);

/// The ID of an `isotope` global
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
struct GlobalId(u32);

/// The ID of an `isotope` type var
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
struct TypeVarId(u32);

macro_rules! prefixed_id {
    ($prefix:literal $name:ident) => {
        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}{}", $prefix, self.0)
            }
        }

        impl std::fmt::Debug for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}{}", $prefix, self.0)
            }
        }

        impl std::str::FromStr for $name {
            type Err = ();

            fn from_str(input: &str) -> Result<Self, Self::Err> {
                input
                    .strip_prefix($prefix)
                    .ok_or(())?
                    .parse()
                    .map_err(|_| ())
                    .map($name)
            }
        }
    };
}

prefixed_id!("%" ValId);
prefixed_id!("#" InstId);
prefixed_id!("'" BlockId);
prefixed_id!("@" GlobalId);
prefixed_id!("!" TypeVarId);

/// A range of `ValId`s
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Default)]
pub struct ValRange(u32, u32);

impl IntoIterator for ValRange {
    type Item = ValId;

    type IntoIter = ValRangeIter;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        ValRangeIter(self)
    }
}

/// An iterator over a range of `ValId`s
#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Default)]
pub struct ValRangeIter(pub ValRange);

impl Iterator for ValRangeIter {
    type Item = ValId;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.0 .0 >= self.0 .1 {
            None
        } else {
            self.0 .0 += 1;
            Some(ValId(self.0 .0))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn id_parsing(n: u32) {
            let nv = ValId(n);
            let ni = InstId(n);
            let nb = BlockId(n);
            let ng = GlobalId(n);
            let nt = TypeVarId(n);
            let fv = format!("%{n}");
            let fi = format!("#{n}");
            let fb = format!("'{n}");
            let fg = format!("@{n}");
            let ft = format!("!{n}");
            assert_eq!(format!("{nv}"), fv);
            assert_eq!(format!("{ni}"), fi);
            assert_eq!(format!("{nb}"), fb);
            assert_eq!(format!("{ng}"), fg);
            assert_eq!(format!("{nt}"), ft);
            assert_eq!(format!("{nv:?}"), fv);
            assert_eq!(format!("{ni:?}"), fi);
            assert_eq!(format!("{nb:?}"), fb);
            assert_eq!(format!("{ng:?}"), fg);
            assert_eq!(format!("{nt:?}"), ft);
            assert_eq!(fv.parse(), Ok(nv));
            assert_eq!(fi.parse(), Ok(ni));
            assert_eq!(fb.parse(), Ok(nb));
            assert_eq!(fg.parse(), Ok(ng));
            assert_eq!(ft.parse(), Ok(nt));
            assert_eq!("%".parse::<ValId>(), Err(()));
            assert_eq!(fi.parse::<ValId>(), Err(()));
        }
    }
}
