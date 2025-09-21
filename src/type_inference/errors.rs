use colored::Colorize;
use serde::{Deserialize, Serialize};

use crate::{diagnostics::Location, parser::ids::{ExprId, PathId, PatternId}, type_inference::TypeChecker};

/// Different kinds of type errors.
/// All of these boil down to "expected {expected}, but found {actual}" but each
/// variant carries more contextual information on the location of this error.
/// E.g. "then-clause of type {expected} does not match the else-clause's type of {actual}"
#[derive(Debug, Copy, Clone, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum TypeErrorKind {
    /// A general type error with no specific verbage
    General,
    /// `expr : Type` where `Type` does not match the inferred type of `expr`
    TypeAnnotationMismatch,
    /// if's `else` type (actual) does not match its `then` type (expected)
    Else,
    /// match branch type (actual) does not match the type of the first branch (expected)
    MatchBranch,
    /// `if` is used without an `else` so it always returns Unit, but a non-Unit return was expected
    IfStatement,
}

impl TypeErrorKind {
    pub fn message(self, actual: &str, expected: &str) -> String {
        let actual = actual.blue();
        let expected = expected.blue();
        match self {
            TypeErrorKind::General => format!("Expected {expected} but found {actual}"),
            TypeErrorKind::TypeAnnotationMismatch => format!("Type annotation {expected} does not match the inferred type {actual}"),
            TypeErrorKind::Else => format!("Then branch's type of {expected} does not match the else branch's type {actual}"),
            TypeErrorKind::MatchBranch => format!("This match branch has type {actual} which does not match the first branch's type of {expected}"),
            TypeErrorKind::IfStatement => format!("This `if` has no `else` so it always returns {expected}, but {actual} was expected instead"),
        }
    }
}

pub(super) trait Locateable {
    fn locate(self, context: &TypeChecker) -> Location;
}

impl Locateable for ExprId {
    fn locate(self, context: &TypeChecker) -> Location {
        context.context.expr_locations[self].clone()
    }
}

impl Locateable for PatternId {
    fn locate(self, context: &TypeChecker) -> Location {
        context.context.pattern_locations[self].clone()
    }
}

impl Locateable for PathId {
    fn locate(self, context: &TypeChecker) -> Location {
        context.context.path_locations[self].clone()
    }
}
