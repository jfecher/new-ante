use crate::{error::{Spanned, ErrorDefault}, lexer::token::IntegerKind};

/// The Concrete Syntax Tree (CST) is the output of parsing a source file.
/// Unlike the Abstract Syntax Tree (AST), the CST is expected to mirror
/// the source file without any constructs being desugared or removed.
/// This makes the CST the ideal candidate for pretty-printing back to a file
/// since no constructs should be removed in the process - including comments.
#[derive(Debug)]
pub struct Cst {
    pub imports: Vec<Import>,
    pub top_level_items: Vec<TopLevelItem>,
}

pub type Ident = Spanned<String>;

#[derive(Debug)]
pub struct Path {
    pub components: Vec<Ident>,
}

#[derive(Debug)]
pub struct Import {
    pub path: Path,
}

#[derive(Debug)]
pub struct TopLevelItem {
    pub comments: Vec<String>,
    pub kind: TopLevelItemKind,
}

#[derive(Debug)]
pub enum TopLevelItemKind {
    FunctionGroup(Vec<Function>),
    TypeDefinition(TypeDefinition),
    Methods(Methods),
}

#[derive(Debug)]
pub struct Function {
    pub name: Ident,
    pub parameters: Vec<(Ident, Option<Type>)>,
    pub return_type: Option<Type>,
    pub body: Option<Expr>,
}

#[derive(Debug)]
pub enum Type {
    Error,
    Named(Path),
    Unit
}

#[derive(Debug)]
pub struct TypeDefinition {
    pub name: Ident,
    pub body: TypeDefinitionBody,
}

#[derive(Debug)]
pub enum TypeDefinitionBody {
    Error,
    Struct(Vec<(Ident, Type)>),
    Enum(Vec<(Ident, Vec<Type>)>),
}

impl ErrorDefault for TypeDefinitionBody {
    fn error_default() -> Self {
        Self::Error
    }
}

#[derive(Debug)]
pub struct Methods {
    pub typ: Type,
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub enum Expr {
    Error,
    Literal(Literal),
    Variable(Path),
    Sequence(Vec<SequenceItem>),
    Definition(Definition),
    Call(Call),
}

impl ErrorDefault for Expr {
    fn error_default() -> Self {
        Self::Error
    }
}

#[derive(Debug)]
pub struct SequenceItem {
    pub comments: Vec<String>,
    pub expr: Expr,
}

#[derive(Debug)]
pub enum Literal {
    Integer(u64, Option<IntegerKind>),
    String(String),
}

#[derive(Debug)]
pub struct Definition {
    pub name: Ident,
    pub typ: Option<Type>,
    pub rhs: Box<Expr>,
}

impl ErrorDefault for Type {
    fn error_default() -> Type {
        Type::Error
    }
}

#[derive(Debug)]
pub struct Call {
    pub function: Box<Expr>,
    pub arguments: Vec<Expr>,
}
