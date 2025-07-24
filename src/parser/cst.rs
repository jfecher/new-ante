use std::{path::PathBuf, sync::Arc};

use serde::{Deserialize, Serialize};

use crate::{errors::{ErrorDefault, Location}, lexer::token::{IntegerKind, Token}};

use super::ids::{ExprId, PatternId, TopLevelId};

/// The Concrete Syntax Tree (CST) is the output of parsing a source file.
/// This is expected to mirror the source file without removing too much information.
/// This isn't a perfect mirroring - we keep only enough information for pretty-printing
/// the CST back into a file. So while things like comments are kept, certain syntax
/// constructs like `foo = fn a -> expr` may be sugared into `foo x = expr`.
#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Cst {
    pub imports: Vec<Import>,
    pub top_level_items: Vec<Arc<TopLevelItem>>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct TopLevelItem {
    pub comments: Vec<String>,
    pub kind: TopLevelItemKind,
    pub id: TopLevelId,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum TopLevelItemKind {
    Definition(Definition),
    TypeDefinition(TypeDefinition),
    TraitDefinition(TraitDefinition),
    TraitImpl(TraitImpl),
    EffectDefinition(EffectDefinition),
    Extern(Extern),
    Comptime(Comptime),
}

impl TopLevelItemKind {
    pub(crate) fn name(&self) -> ItemName {
        match self {
            TopLevelItemKind::Definition(definition) => ItemName::Path(&definition.path),
            TopLevelItemKind::TypeDefinition(type_definition) => ItemName::Single(&type_definition.name),
            TopLevelItemKind::TraitDefinition(trait_definition) => ItemName::Single(&trait_definition.name),
            TopLevelItemKind::TraitImpl(_) => ItemName::None,
            TopLevelItemKind::EffectDefinition(effect_definition) => ItemName::Single(&effect_definition.name),
            TopLevelItemKind::Extern(extern_) => ItemName::Single(&extern_.declaration.name),
            TopLevelItemKind::Comptime(_) => ItemName::None,
        }
    }
}

pub enum ItemName<'a> {
    Single(&'a Arc<String>),
    Path(&'a Path),
    None,
}

impl<'a> std::fmt::Display for ItemName<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ItemName::Single(name) => write!(f, "{name}"),
            ItemName::Path(path) => write!(f, "{path}"),
            ItemName::None => write!(f, "impl"),
        }
    }
}

pub struct TopLevelDefinition {
    pub path: Path,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
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

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct FunctionType {
    pub parameters: Vec<Type>,
    pub return_type: Box<Type>,

    /// Any effects that were specified on this function.
    /// - `None` means none were specified
    /// - `Some(Vec::new())` means it was specified to be `pure`
    pub effects: Option<Vec<EffectType>>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum EffectType {
    Known(Path, Vec<Type>),
    Variable(String),
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct TypeDefinition {
    pub name: Arc<String>,
    pub generics: Vec<String>,
    pub body: TypeDefinitionBody,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
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

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
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
    Quoted(Quoted),
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
#[derive(Serialize, Deserialize, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Path {
    pub components: Vec<(String, Location)>,
}

impl Path {
    pub fn last(&self) -> &String {
        &self.components.last().unwrap().0
    }

    pub fn into_file_path(self) -> Arc<PathBuf> {
        let mut path = PathBuf::new();
        for (component, _) in self.components {
            path.push(component);
        }
        Arc::new(path)
    }
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Import {
    pub comments: Vec<String>,
    /// TODO: Investigate whether this breaks serialization stability across Windows <-> Unix
    pub path: Arc<PathBuf>,
    pub location: Location,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct SequenceItem {
    pub comments: Vec<String>,
    pub expr: ExprId,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Literal {
    Integer(u64, Option<IntegerKind>),
    String(String),
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Definition {
    pub mutable: bool,
    pub path: Path,
    pub typ: Option<Type>,
    pub rhs: ExprId,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Call {
    pub function: ExprId,
    pub arguments: Vec<ExprId>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct MemberAccess {
    pub object: ExprId,
    pub member: String,
    pub ownership: OwnershipMode,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Index {
    pub object: ExprId,
    pub index: ExprId,
    pub ownership: OwnershipMode,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
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

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Lambda {
    pub parameters: Vec<PatternId>,
    pub return_type: Option<Type>,
    pub body: ExprId,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct If {
    pub condition: ExprId,
    pub then: ExprId,
    pub else_: Option<ExprId>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Match {
    /// The expression being matched
    pub expression: ExprId,
    pub cases: Vec<(PatternId, ExprId)>,
}

/// `&rhs`, `!rhs`
#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Reference {
    pub mode: BorrowMode,
    pub rhs: ExprId,
}

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
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

#[derive(Serialize, Deserialize, Copy, Clone, PartialEq, Eq, Hash)]
pub enum SharedMode {
    Shared,
    Owned,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Pattern {
    Variable(Path),
    Literal(Literal),
    Constructor(PatternId, Vec<PatternId>),
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct TypeAnnotation {
    pub lhs: ExprId,
    pub rhs: Type,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Quoted {
    pub tokens: Vec<Token>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Declaration {
    pub name: Arc<String>,
    pub typ: Type,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct TraitDefinition {
    pub name: Arc<String>,
    pub generics: Vec<String>,
    pub functional_dependencies: Vec<String>,
    pub body: Vec<Declaration>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct TraitImpl {
    pub trait_name: Arc<String>,
    pub arguments: Vec<Type>,
    pub body: Vec<Definition>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct EffectDefinition {
    pub name: Arc<String>,
    pub generics: Vec<String>,
    pub body: Vec<Declaration>,
}

#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Extern {
    pub declaration: Declaration,
}

/// A top-level item evaluated at compile-time, e.g:
/// ```ante
/// #if foo then
///     function () = 3
///
/// // or
/// #modify
/// foo bar = ()
///
/// // or
/// derive Foo Bar
/// type MyType = x: I32
/// ```
#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Comptime {
    Expr(ExprId),
    Derive(Vec<Path>),
    Definition(Definition),
}
