/*!
In-memory representation for `isotope` IR in SSA form
*/
use egg::*;
use smallvec::SmallVec;

pub mod primitive;
use primitive::*;

pub mod types;
use types::*;

pub mod builder;

/// An `isotope` function in SSA form
#[derive(Debug, Clone, Default)]
pub struct Function {
    /// The basic blocks in this function
    blocks: Vec<Block>,
    /// The terms defined in this function
    terms: EGraph<IsotopeLanguage, IsotopeAnalysis>,
}

impl Function {
    /// Construct a new block
    pub fn insert_empty_block(&mut self) -> BlockId {
        let ix = self.blocks.len();
        self.blocks.push(Block::default());
        BlockId(ix as u32)
    }

    /// Construct a bitvector constant
    pub fn insert_bitvector(&mut self, bits: Bitvector) -> ValId {
        ValId(self.terms.add(IsotopeLanguage::Bitvector(bits)))
    }

    /// Get the type of bitvectors of a given bitwidth
    pub fn bitvector_ty(&mut self, width: u32) -> TypeId {
        self.terms.analysis.types.bitvector(width)
    }

    /// Get a tuple type
    pub fn tuple_ty(&mut self, types: impl IntoIterator<Item = TypeId>) -> TypeId {
        self.terms.analysis.types.tuple(types)
    }

    /// Get the type of a value
    pub fn val_ty(&self, expr: ValId) -> TypeId {
        self.terms[expr.0].data.ty
    }

    /// Get the type of an instruction
    pub fn inst_ty(&self, inst: InstId) -> TypeId {
        self.terms[self.terms.analysis.instructions[inst.0 as usize].value.0]
            .data
            .ty
    }

    /// Get the result of an instruction
    pub fn res(&mut self, inst: InstId) -> ValId {
        ValId(self.terms.add(IsotopeLanguage::Res(inst)))
    }

    /// Get the projection of a value
    ///
    /// Return an error if this is ill-typed
    pub fn proj(&mut self, value: ValId, ix: u32) -> Result<ValId, ()> {
        let data = self.terms[value.0].data;
        if !data.relevant {
            // Cannot project from a non-relevant value
            return Err(());
        }
        let ty = self.terms.analysis.types.proj(data.ty, ix)?;
        let ix = self.terms.add(IsotopeLanguage::Integer(ix as i64));
        let proj = self.terms.add(IsotopeLanguage::Ix([value.0, ix]));
        debug_assert_eq!(self.terms[proj].data.ty, ty);
        Ok(ValId(proj))
    }

    /// Construct a new instruction in a basic block
    ///
    /// This will execute *after* the last current instruction, but *before* the terminator!
    pub fn push_instruction(&mut self, block: BlockId, expr: ValId) -> InstId {
        let ix = InstId(self.terms.analysis.instructions.len() as u32);
        let instruction = Instruction { value: expr, block };
        self.terms.analysis.instructions.push(instruction);
        ix
    }

    /// Set the terminator of a block
    pub fn set_terminator(&mut self, block: BlockId, terminator: TerminatorId) -> Result<(), ()> {
        self.blocks.get_mut(block.0 as usize).ok_or(())?.terminator = terminator;
        Ok(())
    }
}

define_language! {
    enum IsotopeLanguage {
        // == Values ==
        // === Variables and tuples ===
        // The result of an instruction
        Res(InstId),
        // The parameter of a block
        Param(BlockId),
        // A tuple literal
        "tup" = Tup(Tuple),
        // An index into a tuple
        "ix" = Ix([Id; 2]),

        // === Functions and globals ===
        // A function call
        Call(GlobalId, Id),
        // A global value
        GlobalId(GlobalId),

        // === Constants ===
        // A bitvector
        Bitvector(Bitvector),
        // An integer
        Integer(i64),

        // == Terminators ==
        // An unconditional branch with a given parameter
        Branch(BlockId, Id),
        // An if-then-else terminator
        "ite" = Ite([Id; 3]),
    }
}

/// The analysis for expressions in an `isotope` function
#[derive(Debug, Clone, Default)]
struct IsotopeAnalysis {
    /// The instructions in this function
    instructions: Vec<Instruction>,
    /// The types used in this function
    types: Types,
}

impl IsotopeAnalysis {}

impl Analysis<IsotopeLanguage> for IsotopeAnalysis {
    type Data = IsotopeMetadata;

    #[inline]
    fn make(egraph: &mut EGraph<IsotopeLanguage, Self>, enode: &IsotopeLanguage) -> Self::Data {
        match enode {
            IsotopeLanguage::Res(v) => IsotopeMetadata {
                ty: egraph[egraph.analysis.instructions[v.0 as usize].value.0]
                    .data
                    .ty,
                affine: true,
                relevant: true,
                central: true,
            },
            IsotopeLanguage::Tup(t) => IsotopeMetadata {
                ty: egraph.analysis.types.tuple(
                    t.0.iter()
                        .map(|t| egraph[*t].data.ty)
                        .collect::<SmallVec<[TypeId; 64]>>(),
                ),
                affine: t.0.iter().all(|t| egraph[*t].data.affine),
                relevant: t.0.iter().all(|t| egraph[*t].data.relevant),
                // Note: a valid tuple should always be central!
                central: t.0.iter().all(|t| egraph[*t].data.central),
            },
            IsotopeLanguage::Ix([val, ix]) => {
                //TODO: clean this up...
                let IsotopeLanguage::Integer(ix) = egraph[*ix].nodes[0] else {
                    panic!("invalid index")
                };
                if ix > u32::MAX as i64 {
                    panic!("index out of bounds")
                }
                let ty = egraph
                    .analysis
                    .types
                    .proj(egraph[*val].data.ty, ix as u32)
                    .expect("invalid projection");
                IsotopeMetadata {
                    ty,
                    affine: egraph[*val].data.affine,
                    // Note: a valid projection should always be both central and relevant!
                    relevant: egraph[*val].data.relevant,
                    central: egraph[*val].data.central,
                }
            }
            IsotopeLanguage::Call(_, _) => IsotopeMetadata::default(), //TODO
            IsotopeLanguage::GlobalId(_) => IsotopeMetadata::default(), //TODO
            IsotopeLanguage::Bitvector(b) => IsotopeMetadata {
                ty: egraph.analysis.types.bitvector(b.bitwidth()),
                affine: true,
                relevant: true,
                central: true,
            },
            IsotopeLanguage::Integer(_) => IsotopeMetadata {
                ty: egraph.analysis.types.integer(),
                affine: true,
                relevant: true,
                central: true,
            },
            _ => IsotopeMetadata::default(),
        }
    }

    #[inline]
    fn merge(&mut self, l: &mut Self::Data, r: Self::Data) -> DidMerge {
        let affine = l.affine || r.affine;
        let relevant = l.relevant || r.relevant;
        let central = l.central || r.central;
        let lc = affine != l.affine || relevant != l.relevant || central != l.central;
        let rc = affine != r.affine || relevant != r.relevant || central != r.central;
        DidMerge(lc, rc)
    }
}

#[derive(Debug, Copy, Clone, Default, Eq, PartialEq, Hash, Ord, PartialOrd)]
struct IsotopeMetadata {
    // This value's type
    ty: TypeId,
    //TODO: pack these bits
    // Whether this value is affine
    affine: bool,
    // Whether this value is relevant
    relevant: bool,
    // Whether this value is central
    central: bool,
}

/// A basic block in an `isotope` function
#[derive(Debug, Clone, Eq, PartialEq, Default)]
struct Block {
    /// This block's parameters
    params: Vec<InstId>,
    /// This block's instructions
    instructions: Vec<InstId>,
    /// This block's terminator
    terminator: TerminatorId,
}

/// An instruction in an `isotope` function
///
/// Having this as a separate struct, rather than storing [`ValId`]s in
/// [`Block`], allows us to move, insert, and delete instructions without
/// invalidating the E-graph.
///
/// TODO: find out how to delete instructions from the E-graph as well,
/// reclaiming memory Can use this as a way to catch errors which come from
/// unused instructions, as well
#[derive(Debug, Clone, Eq, PartialEq)]
struct Instruction {
    /// The value of the instruction
    value: ValId,
    /// The basic block of the instruction
    block: BlockId,
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

/// The ID of an `isotope` value
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct ValId(Id);

/// The ID of an `isotope` terminator
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Default)]
pub struct TerminatorId(Id);

/// The ID of an `isotope` value instruction
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct InstId(u32);

/// The ID of an `isotope` block
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct BlockId(u32);

/// The ID of an `isotope` global
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
struct GlobalId(u32);

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

prefixed_id!("%" InstId);
prefixed_id!("'" BlockId);
prefixed_id!("@" GlobalId);

#[cfg(test)]
mod test {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn id_parsing(n: u32) {
            let nv = InstId(n);
            let nb = BlockId(n);
            let ng = GlobalId(n);
            let nt = TypeId(Id::from(n as usize));
            let fv = format!("%{n}");
            let fi = format!("#{n}");
            let ft = format!("!{n}");
            let fb = format!("'{n}");
            let fg = format!("@{n}");
            assert_eq!(format!("{nv}"), fv);
            assert_eq!(format!("{nb}"), fb);
            assert_eq!(format!("{ng}"), fg);
            assert_eq!(format!("{nt}"), ft);
            assert_eq!(format!("{nv:?}"), fv);
            assert_eq!(format!("{nb:?}"), fb);
            assert_eq!(format!("{ng:?}"), fg);
            assert_eq!(format!("{nt:?}"), ft);
            assert_eq!(fv.parse(), Ok(nv));
            assert_eq!(fb.parse(), Ok(nb));
            assert_eq!(fg.parse(), Ok(ng));
            assert_eq!(ft.parse(), Ok(nt));
            assert_eq!("%".parse::<InstId>(), Err(()));
            assert_eq!(fi.parse::<InstId>(), Err(()));
            assert_eq!("%".parse::<TypeId>(), Err(()));
            assert_eq!(fi.parse::<TypeId>(), Err(()));
        }
    }
}
