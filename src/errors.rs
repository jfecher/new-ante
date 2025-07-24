use std::{path::PathBuf, sync::Arc};

use serde::{Deserialize, Serialize};

use crate::{lexer::token::Token, name_resolution::namespace::SourceFileId};

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
    NamespaceNotFound { name: String, location: Location },
    NameNotFound { name: String, location: Location },
}

impl Diagnostic {
    pub fn message(&self) -> String {
        match self {
            Diagnostic::ParserExpected { message, actual, location: _ } => {
                format!("Expected {message} but found `{actual}`")
            },
            Diagnostic::NameAlreadyInScope { name, first_location: _, second_location: _ } => {
                format!("`{name}` was already defined")
            },
            Diagnostic::ImportedNameAlreadyInScope { name, first_location: _, second_location: _ } => {
                format!("This imports `{name}`, which has already been defined")
            },
            Diagnostic::UnknownImportFile { file_name, location: _ } => {
                format!("Cannot read source file `{}`, does it exist?", file_name.display())
            },
            Diagnostic::NameNotInScope { name, location: _ } => {
                format!("`{name}` is not defined, was it a typo?")
            },
            Diagnostic::ExpectedType { actual, expected, location: _ } => {
                format!("Expected type `{expected}` but found `{actual}`")
            },
            Diagnostic::RecursiveType { typ, location: _ } => {
                format!("Binding here would create an infinitely recursive type with `{typ}`")
            },
            Diagnostic::NamespaceNotFound { name, location: _ } => {
                format!("Namespace `{name}` not found in path")
            },
            Diagnostic::NameNotFound { name, location: _ } => {
                format!("`{name}` not found in scope")
            },
        }
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
    pub file_id: SourceFileId,
    pub span: Span,
}

impl LocationData {
    /// Merge two locations
    pub fn to(&self, end: &LocationData) -> Location {
        assert_eq!(self.file_id, end.file_id);
        Arc::new(LocationData { file_id: self.file_id.clone(), span: self.span.to(&end.span) })
    }

    /// An invalid location used only as a temporary placeholder
    pub fn placeholder(file_id: SourceFileId) -> Location {
        let position = Position { byte_index: 0, line_number: 0, column_number: 0 };
        Arc::new(LocationData { file_id, span: Span { start: position, end: position } })
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
    pub fn in_file(self, file_id: SourceFileId) -> Location {
        Arc::new(LocationData { file_id, span: self })
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
