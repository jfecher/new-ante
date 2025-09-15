use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};

use serde::{Deserialize, Serialize};

use crate::{
    lexer::token::{FloatKind, IntegerKind},
    parser::{
        cst::{Mutability, Sharedness},
        ids::{ExprId, NameId},
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
    Generic(ExprId),
    /// We represent type variables with unique ids and an external bindings map instead of a
    /// `Arc<RwLock<..>>` or similar because these need to be compared for equality, serialized, and
    /// be performant. We want the faster insertion of a local BTreeMap compared to a thread-safe
    /// version so we use a BTreeMap internally then freeze it in an Arc when finished to be
    /// able to access it from other threads.
    TypeVariable(TypeVariableId),
    Function {
        parameter: Arc<TopLevelType>,
        return_type: Arc<TopLevelType>,
    },
    TypeApplication(Arc<TopLevelType>, Arc<Vec<TopLevelType>>),
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
    TypeVariable(TypeVariableId),
    Function(FunctionType),
    TypeApplication(TypeId, Vec<TypeId>),
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
        self.fmt_type(&self.typ, f)
    }
}

impl TypePrinter<'_, '_> {
    fn fmt_type(&self, typ: &Type, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match typ {
            Type::Primitive(primitive_type) => write!(f, "{primitive_type}"),
            Type::Generic(_) => todo!("format generic type"),
            Type::TypeVariable(id) => {
                if let Some(binding) = self.bindings.get(&id) {
                    self.fmt_type(binding, f)
                } else {
                    write!(f, "{id}")
                }
            },
            Type::Function(_function) => {
                todo!("format function type")
            },
            Type::TypeApplication(_, _) => todo!("format type application"),
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
    pub generics: Vec<ExprId>,
    pub typ: TopLevelType,
}

impl GeneralizedType {
    pub fn new(generics: Vec<ExprId>, typ: TopLevelType) -> Self {
        Self { typ, generics }
    }

    pub fn unit() -> GeneralizedType {
        Self::new(Vec::new(), TopLevelType::Primitive(PrimitiveType::Unit))
    }

    pub fn display<'a, 'b>(&'a self, bindings: &'b TypeBindings) -> TopLevelTypePrinter<'a, 'b> {
        TopLevelTypePrinter { typ: self, bindings }
    }

    #[allow(unused)]
    pub fn from_ast_type(_typ: &crate::parser::cst::Type) -> Self {
        todo!("resolve generalized type")
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
