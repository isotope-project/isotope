use winnow::{
    error::ParseError,
    token::{one_of, take_while},
};

use super::*;

/// An identifier
#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Ident(pub EcoString);

impl Ident {
    //TODO: add support for raw identifiers, etc

    /// Parse an identifier
    pub fn parser(input: &mut &str) -> PResult<Ident> {
        (
            one_of(|c: char| c.is_alphabetic() || c == '_'),
            take_while(0.., |c: char| c.is_alphanumeric() || c == '_'),
        )
            .recognize()
            .parse_next(input)
            .map(EcoString::from)
            .map(Ident)
    }
}

impl Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let pr: Result<&str, ParseError<&str, ()>> = (
            one_of(|c: char| c.is_alphabetic() || c == '_'),
            take_while(0.., |c: char| c.is_alphanumeric() || c == '_'),
        )
            .recognize()
            .parse(&*self.0);
        if pr.is_ok() {
            write!(f, "{}", self.0)
        } else {
            write!(f, "#r{:?}", self.0)
        }
    }
}

/// A label
#[derive(Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub struct Label(pub Ident);

impl Label {
    /// Parse a label
    pub fn parser(input: &mut &str) -> PResult<Label> {
        preceded('\'', Ident::parser).parse_next(input).map(Label)
    }
}

impl Debug for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
    }
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}", self.0)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn valid_ident_roundtrip(i in "[[:alpha:]_][[:alnum:]_]*") {
            let p = Ident::parser.parse(&i).unwrap();
            assert_eq!(p.0, i);
            assert_eq!(format!("{p}"), i);
            assert_eq!(format!("{p:?}"), i);
        }

        #[test]
        fn valid_label_roundtrip(i in "['][[:alpha:]_][[:alnum:]_]*") {
            let p = Label::parser.parse(&i).unwrap();
            assert_eq!(p.0.0, i[1..]);
            assert_eq!(format!("{p}"), i);
            assert_eq!(format!("{p:?}"), i);
        }
    }

    #[test]
    fn number_invalid_ident() {
        Ident::parser.parse(&"5").unwrap_err();
        assert_eq!(format!("{}", Ident("5".into())), "#r\"5\"")
    }
}
