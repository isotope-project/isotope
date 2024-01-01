use super::*;
use indexmap::IndexSet;

/// A set of `isotope` types
#[derive(Debug, Clone, Default)]
pub struct Types {
    inner: IndexSet<IsotopeType>,
}

impl Types {
    /// Get the arity of a type
    pub fn arity(&self, ty: TypeId) -> u32 {
        match self.inner.get_index(ty.0.into()) {
            Some(IsotopeType::Tup(t)) => t.0.len() as u32,
            _ => 0,
        }
    }

    /// Construct a tuple from an iterator over type IDs
    pub fn tuple(&mut self, types: impl IntoIterator<Item = TypeId>) -> TypeId {
        TypeId(
            self.inner
                .insert_full(IsotopeType::Tup(Tuple(
                    types.into_iter().map(|ty| ty.0).collect(),
                )))
                .0
                .into(),
        )
    }

    /// Construct a bitvector type of a given width
    pub fn bitvector(&mut self, width: u32) -> TypeId {
        TypeId(
            self.inner
                .insert_full(IsotopeType::Bitvector(width))
                .0
                .into(),
        )
    }

    /// Get the type of integers
    pub fn integer(&mut self) -> TypeId {
        TypeId(self.inner.insert_full(IsotopeType::Integer).0.into())
    }

    /// Get the nth projection of a tuple type
    ///
    /// Return an error if the input type is not a tuple
    pub fn proj(&self, ty: TypeId, ix: u32) -> Result<TypeId, ()> {
        match self.inner.get_index(ty.0.into()) {
            Some(IsotopeType::Tup(t)) => t.0.get(ix as usize).copied().map(TypeId).ok_or(()),
            _ => Err(()),
        }
    }

    // /// Get an error type
    // pub fn error(&mut self) -> TypeId {
    //     TypeId(self.inner.insert_full(IsotopeType::TypeError).0.into())
    // }
}

define_language! {
    enum IsotopeType {
        // A tuple
        "tup" = Tup(Tuple),
        // A bitvector of length n
        Bitvector(u32),
        // An integer
        "int" = Integer,
        // // A type error
        // "error" = TypeError,
    }
}

/// The ID of an `isotope` type
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Default)]
pub struct TypeId(pub(crate) Id);

impl std::fmt::Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "!{}", usize::from(self.0))
    }
}

impl std::fmt::Debug for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "!{}", usize::from(self.0))
    }
}

impl std::str::FromStr for TypeId {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        input
            .strip_prefix("!")
            .ok_or(())?
            .parse::<usize>()
            .map_err(|_| ())
            .map(Id::from)
            .map(TypeId)
    }
}

// /// The ID of a typed de-Bruijn index
// #[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
// pub struct VarId(pub u32, pub TypeId);

// impl std::fmt::Display for VarId {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "#{}:{}", self.0, usize::from(self.1 .0))
//     }
// }

// impl std::fmt::Debug for VarId {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "#{}:{}", self.0, usize::from(self.1 .0))
//     }
// }

// impl std::str::FromStr for VarId {
//     type Err = ();

//     fn from_str(input: &str) -> Result<Self, Self::Err> {
//         let (ix, ty) = input
//             .strip_prefix("#")
//             .ok_or(())?
//             .split_once(':')
//             .ok_or(())?;
//         let ix = ix.parse().map_err(|_| ())?;
//         let ty = TypeId(Id::from(ty.parse::<usize>().map_err(|_| ())?));
//         Ok(VarId(ix, ty))
//     }
// }
