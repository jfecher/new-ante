use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};

use serde::{Deserialize, Serialize};

use crate::{
    iterator_extensions::vecmap,
    lexer::token::{FloatKind, IntegerKind},
    name_resolution::ResolutionResult,
    parser::{
        cst::{self, Mutability, Sharedness},
        ids::NameId,
    },
    type_inference::type_id::TypeId,
};

/// A top-level type is a type which may be in a top-level signature.
/// This notably excludes unbound type variables. Unlike `Type`, top-level
/// types must also be thread-safe.
#[derive(Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum TopLevelType {
    /// Any primitive type which can be compared for unification via primitive equality
    Primitive(PrimitiveType),
    /// A user-supplied generic type. We don't want to bind over these like we do with type variables.
    Generic(NameId),
    Function {
        parameters: Vec<TopLevelType>,
        return_type: Box<TopLevelType>,
    },
    TypeApplication(Box<TopLevelType>, Vec<TopLevelType>),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// Any primitive type which can be compared for unification via primitive equality
    Primitive(PrimitiveType),

    /// A user-supplied generic type. We don't want to bind over these like we do with type variables.
    Generic(NameId),

    /// We represent type variables with unique ids and an external bindings map instead of a
    /// `Arc<RwLock<..>>` or similar because these need to be compared for equality, serialized, and
    /// be performant. We want the faster insertion of a local BTreeMap compared to a thread-safe
    /// version so we use a BTreeMap internally then freeze it in an Arc when finished to be
    /// able to access it from other threads.
    Variable(TypeVariableId),
    Function(FunctionType),
    Application(TypeId, Vec<TypeId>),
    Reference(Mutability, Sharedness),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct FunctionType {
    pub parameters: Vec<TypeId>,
    pub return_type: TypeId,
    pub effects: TypeId,
}

#[derive(Copy, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Error,
    Unit,
    Bool,
    Pointer,
    Char,
    /// TODO: This should be a struct type
    String,
    Int(IntegerKind),
    Float(FloatKind),
}

/// Maps type variables to their bindings
pub type TypeBindings = BTreeMap<TypeVariableId, Type>;

/// Maps generics to new types to instantiate them with
#[allow(unused)]
pub type Substitutions = BTreeMap<Arc<String>, Type>;

#[allow(unused)]
impl TopLevelType {
    pub fn error() -> Self {
        Self::Primitive(PrimitiveType::Error)
    }

    pub fn unit() -> Self {
        Self::Primitive(PrimitiveType::Unit)
    }

    pub fn from_ast_type(typ: &cst::Type, resolve: &ResolutionResult) -> TopLevelType {
        match typ {
            cst::Type::Error => TopLevelType::error(),
            cst::Type::Unit => TopLevelType::unit(),
            cst::Type::Char => TopLevelType::Primitive(PrimitiveType::Char),
            cst::Type::String => TopLevelType::Primitive(PrimitiveType::String),
            cst::Type::Named(path_id) => {
                let _origin = resolve.path_origins[path_id];
                todo!("resolve named type")
            },
            cst::Type::Variable(name_id) => TopLevelType::Generic(*name_id),
            cst::Type::Integer(kind) => TopLevelType::Primitive(PrimitiveType::Int(*kind)),
            cst::Type::Float(kind) => TopLevelType::Primitive(PrimitiveType::Float(*kind)),
            cst::Type::Function(function_type) => {
                // TODO: Effects
                let parameters = vecmap(&function_type.parameters, |typ| Self::from_ast_type(typ, resolve));
                let return_type = Box::new(Self::from_ast_type(&function_type.return_type, resolve));
                Self::Function { parameters, return_type }
            },
            cst::Type::Application(constructor, args) => {
                let constructor = Box::new(Self::from_ast_type(constructor, resolve));
                let args = vecmap(args, |arg| Self::from_ast_type(arg, resolve));
                Self::TypeApplication(constructor, args)
            },
            cst::Type::Reference(mutability, sharedness) => todo!("Reference types"),
        }
    }

    fn find_generics(&self) -> Vec<NameId> {
        match self {
            TopLevelType::Primitive(_) => Vec::new(),
            TopLevelType::Generic(generic) => vec![*generic],
            TopLevelType::Function { parameters, return_type } => parameters
                .iter()
                .chain(std::iter::once(return_type.as_ref()))
                .flat_map(|typ| typ.find_generics())
                .collect(),
            TopLevelType::TypeApplication(constructor, args) => {
                std::iter::once(constructor.as_ref()).chain(args).flat_map(|typ| typ.find_generics()).collect()
            },
        }
    }
}

#[allow(unused)]
impl Type {
    /// Substitutes any generics with the given names with the corresponding type in the map
    pub fn substitute(&self, _substitutions: &Substitutions, _bindings: &TypeBindings) -> Type {
        todo!()
    }

    pub fn display<'a, 'b>(&'a self, bindings: &'b TypeBindings) -> TypePrinter<'a, 'b> {
        TypePrinter { typ: self, bindings }
    }

    pub fn find_all_generics(&self) -> Vec<Arc<String>> {
        let mut found = BTreeSet::new();
        self.find_all_generics_helper(&mut found);
        found.into_iter().collect()
    }

    fn find_all_generics_helper(&self, _found: &mut BTreeSet<Arc<String>>) {
        todo!()
    }

    pub fn unit() -> Self {
        Self::Primitive(PrimitiveType::Unit)
    }

    pub fn error() -> Self {
        Self::Primitive(PrimitiveType::Error)
    }
}

pub struct TypePrinter<'typ, 'bindings> {
    typ: &'typ Type,
    bindings: &'bindings TypeBindings,
}

impl std::fmt::Display for TypePrinter<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.fmt_type(self.typ, f)
    }
}

impl TypePrinter<'_, '_> {
    fn fmt_type(&self, typ: &Type, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match typ {
            Type::Primitive(primitive_type) => write!(f, "{primitive_type}"),
            Type::Generic(_) => todo!("format generic type"),
            Type::Variable(id) => {
                if let Some(binding) = self.bindings.get(id) {
                    self.fmt_type(binding, f)
                } else {
                    write!(f, "{id}")
                }
            },
            Type::Function(_function) => {
                todo!("format function type")
            },
            Type::Application(_, _) => todo!("format type application"),
            Type::Reference(mutability, sharedness) => write!(f, "{mutability}{sharedness}"),
        }
    }
}

impl std::fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimitiveType::Error => write!(f, "(error)"),
            PrimitiveType::Unit => write!(f, "Unit"),
            PrimitiveType::Bool => write!(f, "Bool"),
            PrimitiveType::Pointer => write!(f, "Ptr"),
            PrimitiveType::Int(kind) => write!(f, "{kind}"),
            PrimitiveType::Float(kind) => write!(f, "{kind}"),
            PrimitiveType::String => write!(f, "String"),
            PrimitiveType::Char => write!(f, "Char"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize, Deserialize)]
pub struct TypeVariableId(pub u32);

impl TypeVariableId {
    #[allow(unused)]
    pub(crate) fn occurs_in(self, _other: &Type, _bindings: &TypeBindings) -> bool {
        todo!()
    }
}

impl std::fmt::Display for TypeVariableId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "_{}", self.0)
    }
}

/// A top level definition's type may be generalized (made generic).
/// Other definitions like parameters are never generic.
#[derive(Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct GeneralizedType {
    pub generics: Vec<NameId>,
    pub typ: TopLevelType,
}

impl GeneralizedType {
    fn new(generics: Vec<NameId>, typ: TopLevelType) -> Self {
        Self { typ, generics }
    }

    pub fn unit() -> GeneralizedType {
        Self::new(Vec::new(), TopLevelType::Primitive(PrimitiveType::Unit))
    }

    pub fn display<'a, 'b>(&'a self, bindings: &'b TypeBindings) -> TopLevelTypePrinter<'a, 'b> {
        TopLevelTypePrinter { typ: self, bindings }
    }

    pub fn from_ast_type(typ: &cst::Type, resolve: &ResolutionResult) -> Self {
        let typ = TopLevelType::from_ast_type(typ, resolve);
        Self::from_top_level_type(typ)
    }

    /// Convert a TopLevelType into a GeneralizedType. TopLevelTypes never contain
    /// unbound type variables so this operation cannot fail.
    pub fn from_top_level_type(typ: TopLevelType) -> GeneralizedType {
        let generics = typ.find_generics();
        GeneralizedType { generics, typ }
    }
}

pub struct TopLevelTypePrinter<'typ, 'bindings> {
    typ: &'typ GeneralizedType,
    bindings: &'bindings TypeBindings,
}

impl std::fmt::Display for TopLevelTypePrinter<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if !self.typ.generics.is_empty() {
            write!(f, "forall")?;
            for id in self.typ.generics.iter() {
                write!(f, " {}", id)?;
            }
            write!(f, ". ")?;
        }
        write!(f, "{}", self.typ.display(self.bindings))
    }
}
