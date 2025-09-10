use serde::{Deserialize, Serialize};

/// Contains only builtin items which can be redefined (are not keywords).
/// This includes most builtin types except for sized-integer and float types `I8`, `I16`, `U32`, etc.
#[derive(Copy, Clone, Serialize, Deserialize, Debug, PartialEq, Eq)]
pub enum Builtin {
    Unit,
    Int,
    Char,
    Float,
    String,
    PairType,
    PairConstructor,
}

impl Builtin {
    /// Return the builtin of the same name, if there is one.
    /// An `is_type` disambiguator is required to distinguish between
    /// the pair type `,` and the value-level pair constructor `,`.
    pub fn from_name(name: &str, is_type: bool) -> Option<Builtin> {
        use Builtin::*;
        match name {
            "Unit" => Some(Unit),
            "Int" => Some(Int),
            "Char" => Some(Char),
            "Float" => Some(Float),
            "String" => Some(String),
            "," if is_type => Some(PairType),
            "," => Some(PairConstructor),
            _ => None,
        }
    }
}
