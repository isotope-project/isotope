/*!
AST nodes for primitive data types
*/

use super::*;

/// A bitvector literal
#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Bitvector {
    /// Whether this literal should be negated or not
    pub negate: bool,
    /// This literal's bitwidth
    pub bitwidth: u32,
    /// This literal's data
    pub radix: Radix,
    /// This literal's digits. Note that this _may_ be invalid!
    pub digits: EcoString,
}

impl Debug for Bitvector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for Bitvector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}{}",
            if self.negate { "-" } else { "" },
            self.bitwidth,
            self.radix,
            self.digits
        )
    }
}

impl Bitvector {
    /// Parse a bitvector literal
    ///
    /// This has format `<-?><width><radix><digits>`, where `<width>` is a 32-bit decimal integer
    pub fn parser(input: &mut &str) -> PResult<Bitvector> {
        (opt('-'), dec_uint, Radix::parser, hex_digit1)
            .parse_next(input)
            .map(|(negate, bitwidth, radix, digits)| Bitvector {
                negate: negate.is_some(),
                bitwidth,
                radix,
                digits: digits.into(),
            })
    }
}

/// Different radices for bitvector literals
#[derive(Copy, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum Radix {
    /// Binary
    Bin = 2,
    /// Octal
    Oct = 8,
    /// Decimal
    Dec = 10,
    /// Hexadecimal
    Hex = 16,
}

impl Debug for Radix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for Radix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bin => write!(f, "'b"),
            Self::Oct => write!(f, "'o"),
            Self::Dec => write!(f, "'d"),
            Self::Hex => write!(f, "'h"),
        }
    }
}

impl Radix {
    /// Parse a radix
    ///
    /// ```rust
    /// # use winnow::prelude::*;
    /// # use isotope_syntax::primitive::*;
    /// assert_eq!(Radix::parser.parse("'b").unwrap(), Radix::Bin);
    /// assert_eq!(Radix::parser.parse("'o").unwrap(), Radix::Oct);
    /// assert_eq!(Radix::parser.parse("'d").unwrap(), Radix::Dec);
    /// assert_eq!(Radix::parser.parse("'h").unwrap(), Radix::Hex);
    /// assert!(Radix::parser.parse("b").is_err());
    /// assert!(Radix::parser.parse("'w").is_err());
    /// ```
    pub fn parser(input: &mut &str) -> PResult<Radix> {
        use Radix::*;
        preceded(
            '\'',
            dispatch! { any;
                'b' => success(Bin),
                'o' => success(Oct),
                'd' => success(Dec),
                'h' => success(Hex),
                _ => fail
            },
        )
        .parse_next(input)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn bitvector_roundtrip(b in "[-]??[1-9][0-9]{0,3}['][bdoh][[:xdigit:]]+") {
            let p = Bitvector::parser.parse(&b).unwrap();
            assert_eq!(format!("{p}"), b);
            assert_eq!(format!("{p:?}"), b);
        }

        #[test]
        fn radix_roundtrip(r in "['][bdoh]") {
            let p = Radix::parser.parse(&r).unwrap();
            assert_eq!(format!("{p}"), r);
            assert_eq!(format!("{p:?}"), r);
        }
    }
}
