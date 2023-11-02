/*!
In-memory representation for primitive data types
*/

use std::{
    fmt::{Debug, Display},
    str::FromStr,
};

use winnow::Parser;

use crate::builder::SyntaxError;

/// A short bitvector (up to 128 bits long)
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Bitvector {
    bitwidth: u8,
    value: u128,
}

impl Debug for Bitvector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for Bitvector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}'d{}", self.bitwidth, self.value)
    }
}

impl Bitvector {
    /// Attempt to construct a bitvector
    pub fn try_new(bitwidth: u8, value: u128) -> Result<Bitvector, ()> {
        if bitwidth > 128 {
            return Err(());
        }
        let mask = 1u128
            .checked_shl(bitwidth as u32)
            .unwrap_or(0)
            .wrapping_sub(1);
        if value & mask == value {
            Ok(Bitvector { bitwidth, value })
        } else {
            Err(())
        }
    }

    /// Get the bitwidth of this bitvector
    pub fn bitwidth(&self) -> u32 {
        self.bitwidth as u32
    }

    /// Get this bitvector's value as a 128-bit unsigned integer
    pub fn value(&self) -> u128 {
        self.value
    }

    /// Whether this bitvector has a power-of-two bitwidth
    pub fn bitwidth_power_of_two(&self) -> bool {
        self.bitwidth.is_power_of_two()
    }

    /// Zero-extend this bitvector to the given length
    pub fn zext(&self, bitwidth: u32) -> Result<Bitvector, ()> {
        if bitwidth > 128 {
            return Err(());
        }
        let mask = 1u128.checked_shl(bitwidth).unwrap_or(0).wrapping_sub(1);
        Ok(Bitvector {
            bitwidth: bitwidth as u8,
            value: self.value & mask,
        })
    }

    /// Zero extend this bitvector to the nearest power-of-two length
    pub fn zext_to_power_of_two(&self) -> Bitvector {
        Bitvector {
            bitwidth: self.bitwidth.next_power_of_two(),
            value: self.value,
        }
    }
}

impl TryFrom<&'_ isotope_syntax::primitive::Bitvector> for Bitvector {
    type Error = SyntaxError;

    fn try_from(value: &isotope_syntax::primitive::Bitvector) -> Result<Self, Self::Error> {
        if value.bitwidth > 128 {
            return Err(SyntaxError::BitvectorError);
        }
        let mut data = u128::from_str_radix(&value.digits, value.radix as u32)
            .map_err(|_| SyntaxError::BitvectorError)?;
        let mask = 1u128
            .checked_shl(value.bitwidth)
            .unwrap_or(0)
            .wrapping_sub(1);
        if data & !mask != 0 {
            return Err(SyntaxError::BitvectorError);
        }
        if value.negate {
            data = data.wrapping_neg() & mask;
        }
        Ok(Bitvector {
            bitwidth: value.bitwidth as u8,
            value: data,
        })
    }
}

impl TryFrom<isotope_syntax::primitive::Bitvector> for Bitvector {
    type Error = SyntaxError;

    fn try_from(value: isotope_syntax::primitive::Bitvector) -> Result<Self, Self::Error> {
        TryFrom::try_from(&value)
    }
}

impl From<Bitvector> for isotope_syntax::primitive::Bitvector {
    fn from(value: Bitvector) -> Self {
        isotope_syntax::primitive::Bitvector {
            negate: false,
            bitwidth: value.bitwidth(),
            radix: isotope_syntax::primitive::Radix::Dec,
            digits: format!("{}", value.value()).into(),
        }
    }
}

impl FromStr for Bitvector {
    type Err = SyntaxError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        isotope_syntax::primitive::Bitvector::parser
            .parse(s)
            .map_err(|_| SyntaxError::BitvectorError)?
            .try_into()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use proptest::*;

    proptest! {
        #[test]
        fn bitvector128_parsing(u: u128) {
            let bv = Bitvector { bitwidth: 128, value: u };
            assert_eq!(format!("128'd{u}").parse::<Bitvector>().unwrap(), bv);
            assert_eq!(format!("128'h{u:x}").parse::<Bitvector>().unwrap(), bv);
            assert_eq!(format!("128'o{u:o}").parse::<Bitvector>().unwrap(), bv);
            assert_eq!(format!("128'b{u:b}").parse::<Bitvector>().unwrap(), bv);
            assert_eq!(format!("128'd{u}"), format!("{bv}"));
            assert_eq!(format!("128'd{u}"), format!("{bv}"));
        }
    }
}
