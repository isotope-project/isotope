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

    /// Get an error type
    pub fn error(&mut self) -> TypeId {
        TypeId(self.inner.insert_full(IsotopeType::TypeError).0.into())
    }
}

define_language! {
    enum IsotopeType {
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

/// The ID of an `isotope` type
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd, Default)]
pub struct TypeId(Id);
