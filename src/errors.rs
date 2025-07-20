use std::{path::PathBuf, sync::Arc};

use serde::{Deserialize, Serialize};

use crate::lexer::token::Token;

pub type Location = Arc<LocationData>;
pub type Errors = Vec<Diagnostic>;

/// Any diagnostic that the compiler can issue
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum Diagnostic {
    // TODO: `message` could be an enum to save allocation costs
    ParserExpected { message: String, actual: Token, location: Location },

    NameAlreadyInScope { name: Arc<String>, first_location: Location, second_location: Location },
    ImportedNameAlreadyInScope { name: Arc<String>, first_location: Location, second_location: Location },
    UnknownImportFile { file_name: Arc<PathBuf>, location: Location },
    NameNotInScope { name: Arc<String>, location: Location },
    ExpectedType { actual: String, expected: String, location: Location },
    RecursiveType { typ: String, location: Location },
}

impl Diagnostic {
    pub fn message(&self) -> String {
        match self {
            Diagnostic::ParserExpected { message, actual, location } => {
                format!("{location}: Expected {message} but found `{actual}`")
            },
            Diagnostic::NameAlreadyInScope { name, first_location, second_location } => {
                format!("{second_location}: `{name}` was already defined at {first_location}")
            },
            Diagnostic::ImportedNameAlreadyInScope { name, first_location, second_location } => {
                format!(
                    "{second_location}: This imports `{name}`, which has already been defined here: {first_location}"
                )
            },
            Diagnostic::UnknownImportFile { file_name, location } => {
                format!("{location}: Cannot read source file `{}`, does it exist?", file_name.display())
            },
            Diagnostic::NameNotInScope { name, location } => {
                format!("{location}: `{name}` is not defined, was it a typo?")
            },
            Diagnostic::ExpectedType { actual, expected, location } => {
                format!("{location}: Expected type `{expected}` but found `{actual}`")
            },
            Diagnostic::RecursiveType { typ, location } => {
                format!("{location}: Binding here would create an infinitely recursive type with `{typ}`")
            },
        }
    }
}

impl std::fmt::Display for LocationData {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}", self.file_name.display(), self.span.start.line_number)
    }
}
impl std::fmt::Debug for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message())
    }
}

/// A default value to provide when something has errored
pub trait ErrorDefault {
    fn error_default() -> Self;
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Hash)]
pub struct LocationData {
    pub file_name: Arc<PathBuf>,
    pub span: Span,
}

impl LocationData {
    /// Merge two locations
    pub fn to(&self, end: &LocationData) -> Location {
        assert_eq!(self.file_name, end.file_name);
        Arc::new(LocationData { file_name: self.file_name.clone(), span: self.span.to(&end.span) })
    }

    /// An invalid location used only as a temporary placeholder
    pub fn placeholder(file_name: Arc<PathBuf>) -> Location {
        let position = Position { byte_index: 0, line_number: 0, column_number: 0 };
        Arc::new(LocationData { file_name, span: Span { start: position, end: position } })
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Hash)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    /// Merge two spans
    pub fn to(&self, end: &Span) -> Span {
        assert!(self.start.byte_index <= end.end.byte_index);
        Span { start: self.start, end: end.end }
    }

    /// Construct a Location from this Span
    pub fn in_file(self, file_name: Arc<PathBuf>) -> Location {
        Arc::new(LocationData { file_name, span: self })
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Hash)]
pub struct Position {
    pub byte_index: usize,
    pub line_number: u32,
    pub column_number: u32,
}

impl Position {
    pub fn start() -> Position {
        Position { byte_index: 0, line_number: 1, column_number: 1 }
    }
}
