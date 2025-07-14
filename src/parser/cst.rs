use crate::{errors::ErrorDefault, lexer::token::{IntegerKind, Token}};

use super::ids::{ExprId, PatternId, TopLevelId};

/// The Concrete Syntax Tree (CST) is the output of parsing a source file.
/// This is expected to mirror the source file without removing too much information.
/// This isn't a perfect mirroring - we keep only enough information for pretty-printing
/// the CST back into a file. So while things like comments are kept, certain syntax
/// constructs like `foo = fn a -> expr` may be sugared into `foo x = expr`.
pub struct Cst {
    pub imports: Vec<Import>,
    pub top_level_items: Vec<TopLevelItem>,
}

pub struct TopLevelItem {
    pub comments: Vec<String>,
    pub kind: TopLevelItemKind,
    pub id: TopLevelId,
}

pub enum TopLevelItemKind {
    Definition(Definition),
    TypeDefinition(TypeDefinition),
    TraitDefinition(TraitDefinition),
    TraitImpl(TraitImpl),
    EffectDefinition(EffectDefinition),
    Extern(Extern),
}

pub struct TopLevelDefinition {
    pub path: Path,
}

pub enum Type {
    Error,
    Unit,
    Named(Path),
    Integer(IntegerKind),
    Function(FunctionType),
    TypeApplication(Box<Type>, Vec<Type>),
}

impl ErrorDefault for Type {
    fn error_default() -> Self {
        Self::Error
    }
}

pub struct FunctionType {
    pub parameters: Vec<Type>,
    pub return_type: Box<Type>,

    /// Any effects that were specified on this function.
    /// - `None` means none were specified
    /// - `Some(Vec::new())` means it was specified to be `pure`
    pub effects: Option<Vec<EffectType>>,
}

pub enum EffectType {
    Known(Path, Vec<Type>),
    Variable(String),
}

pub struct TypeDefinition {
    pub name: String,
    pub generics: Vec<String>,
    pub body: TypeDefinitionBody,
}

pub enum TypeDefinitionBody {
    Error,
    Struct(Vec<(String, Type)>),
    Enum(Vec<(String, Vec<Type>)>),
}

impl ErrorDefault for TypeDefinitionBody {
    fn error_default() -> Self {
        Self::Error
    }
}


pub enum Expr {
    Error,
    Literal(Literal),
    Variable(Path),
    Sequence(Vec<SequenceItem>),
    Definition(Definition),
    MemberAccess(MemberAccess),
    Index(Index),
    Call(Call),
    Lambda(Lambda),
    If(If),
    Match(Match),
    Reference(Reference),
    TypeAnnotation(TypeAnnotation),
}

impl ErrorDefault for Expr {
    fn error_default() -> Self {
        Self::Error
    }
}

impl Expr {
    /// Are parenthesis not required when printing this Expr within another?
    pub fn is_atom(&self) -> bool {
        match self {
            Expr::Error => true,
            Expr::Literal(_) => true,
            Expr::Variable(_) => true,
            Expr::MemberAccess(_) => true,
            Expr::Index(_) => true,
            Expr::Reference(_) => true,
            _ => false,
        }
    }
}

/// Path Can't contain any ExprIds since it is used for hashing top-level definition names
///
/// A path is always guaranteed to have at least 1 component
#[derive(Hash)]
pub struct Path {
    pub components: Vec<String>,
}

impl Path {
    pub fn last(&self) -> &String {
        self.components.last().unwrap()
    }
}

pub struct Import {
    pub path: Path,
}

pub struct SequenceItem {
    pub comments: Vec<String>,
    pub expr: ExprId,
}

pub enum Literal {
    Integer(u64, Option<IntegerKind>),
    String(String),
}

pub struct Definition {
    pub mutable: bool,
    pub path: Path,
    pub typ: Option<Type>,
    pub rhs: ExprId,
}

pub struct Call {
    pub function: ExprId,
    pub arguments: Vec<ExprId>,
}

pub struct MemberAccess {
    pub object: ExprId,
    pub member: String,
    pub ownership: OwnershipMode,
}

pub struct Index {
    pub object: ExprId,
    pub index: ExprId,
    pub ownership: OwnershipMode,
}

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

pub struct Lambda {
    pub parameters: Vec<(String, Option<Type>)>,
    pub return_type: Option<Type>,
    pub body: ExprId,
}

pub struct If {
    pub condition: ExprId,
    pub then: ExprId,
    pub else_: Option<ExprId>,
}

pub struct Match {
    /// The expression being matched
    pub expression: ExprId,
    pub cases: Vec<(PatternId, ExprId)>,
}

/// `&rhs`, `!rhs`
pub struct Reference {
    pub mode: BorrowMode,
    pub rhs: ExprId,
}

#[derive(Copy, Clone)]
pub enum BorrowMode {
    Immutable(SharedMode),
    Mutable(SharedMode),
}

impl BorrowMode {
    pub fn shared_mode(self) -> SharedMode {
        match self {
            BorrowMode::Immutable(shared_mode) => shared_mode,
            BorrowMode::Mutable(shared_mode) => shared_mode,
        }
    }

    pub fn is_shared(self) -> bool {
        matches!(self.shared_mode(), SharedMode::Shared)
    }
}

#[derive(Copy, Clone)]
pub enum SharedMode {
    Shared,
    Owned,
}

pub enum Pattern {
    Variable(Path),
    Literal(Literal),
    Constructor(Path, Vec<PatternId>),
}

pub struct TypeAnnotation {
    pub lhs: ExprId,
    pub rhs: Type,
}

pub struct Declaration {
    pub name: String,
    pub typ: Type,
}

pub struct TraitDefinition {
    pub name: String,
    pub generics: Vec<String>,
    pub functional_dependencies: Vec<String>,
    pub body: Vec<Declaration>,
}

pub struct TraitImpl {
    pub trait_name: String,
    pub arguments: Vec<Type>,
    pub body: Vec<Definition>,
}

pub struct EffectDefinition {
    pub name: String,
    pub generics: Vec<String>,
    pub body: Vec<Declaration>,
}

pub struct Extern {
    pub declarations: Vec<Declaration>,
}
