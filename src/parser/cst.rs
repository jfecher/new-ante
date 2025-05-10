use crate::{error::{ErrorDefault, Spanned}, lexer::token::{IntegerKind, Token}};

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

#[derive(Debug)]
pub enum Expr {
    Error,
    Literal(Literal),
    Variable(Path),
    Sequence(Vec<SequenceItem>),
    Definition(Definition),
    MemberAccess(MemberAccess),
    Index(Index),
    Call(Call),
}

impl ErrorDefault for Expr {
    fn error_default() -> Self {
        Self::Error
    }
}

impl Expr {
    pub fn is_atom(&self) -> bool {
        match self {
            Expr::Error => true,
            Expr::Literal(_) => true,
            Expr::Variable(_) => true,
            Expr::Sequence(_) => false,
            Expr::Definition(_) => false,
            Expr::Call(_) => false,
            Expr::MemberAccess(_) => true,
            Expr::Index(_) => true,
        }
    }
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
}

#[derive(Debug)]
pub struct Function {
    pub path: Path,
    pub parameters: Vec<(Ident, Option<Type>)>,
    pub return_type: Option<Type>,
    pub body: Option<Expr>,
}

#[derive(Debug)]
pub enum Type {
    Error,
    Named(Path),
    Unit,
    I32,
    U32,
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

#[derive(Debug)]
pub struct MemberAccess {
    pub object: Box<Expr>,
    pub member: Spanned<String>,
    pub ownership: OwnershipMode,
}

#[derive(Debug)]
pub struct Index {
    pub object: Box<Expr>,
    pub index: Box<Expr>,
    pub ownership: OwnershipMode,
}

#[derive(Debug)]
pub enum OwnershipMode {
    Owned,
    Borrow,
    BorrowMut,
}

impl OwnershipMode {
    pub fn from_token(token: &Token) -> Option<Self> {
        match token {
            Token::MemberAccess | Token::Index => Some(Self::Owned),
            Token::MemberRef | Token::IndexRef => Some(Self::Borrow),
            Token::MemberMut | Token::IndexMut => Some(Self::BorrowMut),
            _ => None,
        }
    }
}
