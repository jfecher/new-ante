use std::sync::Arc;

use serde::{Deserialize, Serialize};

use crate::name_resolution::namespace::SourceFileId;

/// A default value to provide when something has errored
pub trait ErrorDefault {
    fn error_default() -> Self;
}

pub type Location = Arc<LocationData>;

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
