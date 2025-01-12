use crate::error::Spanned;

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
    Struct(Vec<(Ident, Type)>),
    Enum(Vec<(Ident, Vec<Type>)>),
}

#[derive(Debug)]
pub struct Methods {
    pub typ: Type,
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Variable(Ident),
    Sequence(Vec<SequenceItem>),
    Definition(Definition),
}

#[derive(Debug)]
pub struct SequenceItem {
    pub comments: Vec<String>,
    pub expr: Expr,
}

#[derive(Debug)]
pub enum Literal {
    Integer(String),
    String(String),
}

#[derive(Debug)]
pub struct Definition {
    pub name: Ident,
    pub typ: Option<Type>,
    pub rhs: Box<Expr>,
}
