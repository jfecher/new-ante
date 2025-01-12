use crate::lexer::token::Token;

#[derive(Debug)]
pub struct CompileResult<T> {
    pub item: T,
    pub warnings: Vec<Diagnostic>,
    pub errors: Vec<Diagnostic>,
}

#[derive(Debug)]
pub enum Diagnostic {
    ExpectedToken { message: &'static str, actual: Token },
}

/// A Span is a byte index into a file used for reporting errors.
#[derive(Debug, Copy, Clone)]
pub struct Span {
    pub start: Position,
    pub end: usize,
}

#[derive(Debug)]
pub struct Spanned<T> {
    pub item: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(item: T, span: Span) -> Self {
        Self { item, span }
    }
}

/// A given Position in a file. These are usually used as
/// start positions for a Location struct.
///
/// TODO: remove line and column fields to make Position smaller
/// and faster. These can be computed on demand while issuing
/// error messages. Since Locations are used pervasively in the
/// lexer and parser, this would likely speed up compilation.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position {
    pub index: usize,
    pub line: u32,
    pub column: u16,
}

impl Position {
    /// The first position in a file
    pub fn begin() -> Position {
        Position { index: 0, line: 1, column: 1 }
    }

    /// Increment the position 1 character forward
    pub fn advance(&mut self, char_len: usize, passed_newline: bool) {
        if passed_newline {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        self.index += char_len;
    }
}

impl std::fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Diagnostic::ExpectedToken { message, actual } => {
                write!(f, "Parser expected {message}, but found {actual}")
            },
        }
    }
}
