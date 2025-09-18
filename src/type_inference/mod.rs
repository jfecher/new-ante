use std::{collections::BTreeMap, sync::Arc};

use serde::{Deserialize, Serialize};
use types::{PrimitiveType, Type, TypeBindings};

use crate::{
    incremental::{self, DbHandle, GetItem, GetType, Resolve, TypeCheck},
    name_resolution::{Origin, ResolutionResult},
    parser::{
        cst::{Expr, Literal, TopLevelItemKind},
        ids::{ExprId, NameId, PathId, PatternId, TopLevelId},
        TopLevelContext,
    },
    type_inference::types::{GeneralizedType, TopLevelType, TypeVariableId},
};

pub mod types;

/// Get the type of the name defined by this TopLevelId.
/// If this doesn't define a name we return the Unit type by default.
///
/// This is very similar to but separate from `type_check_impl`. We separate these
/// because `type_check_impl` will always type check the contents of a definition,
/// and we don't want other definitions to depend on the contents of another definition
/// if the other definition provides a type annotation. Without type annotations the two
/// functions should be mostly equivalent.
pub fn get_type_impl(context: &GetType, compiler: &DbHandle) -> GeneralizedType {
    incremental::enter_query();
    let (item, _item_context) = compiler.get(GetItem(context.0.clone()));
    incremental::println(format!("Get type of {:?}", item.id));

    let typ = match &item.kind {
        TopLevelItemKind::Definition(_) => todo!(),
        TopLevelItemKind::TypeDefinition(_) => GeneralizedType::unit(),
        TopLevelItemKind::TraitDefinition(_) => GeneralizedType::unit(),
        TopLevelItemKind::TraitImpl(_) => todo!(),
        TopLevelItemKind::EffectDefinition(_) => GeneralizedType::unit(),
        TopLevelItemKind::Extern(_) => todo!(),
        TopLevelItemKind::Comptime(_) => todo!(),
    };
    incremental::exit_query();
    typ
}

/// Actually type check a statement and its contents.
/// Unlike `get_type_impl`, this always type checks the expressions inside a statement
/// to ensure they type check correctly.
pub fn type_check_impl(context: &TypeCheck, compiler: &DbHandle) -> TypeCheckResult {
    incremental::enter_query();
    let (item, item_context) = GetItem(context.0.clone()).get(compiler);
    incremental::println(format!("Type checking {:?}", item.id));

    let resolve = Resolve(context.0.clone()).get(compiler);
    let checker = TypeChecker::new(context.0.clone(), resolve, &item_context, compiler);

    let typ = match &item.kind {
        TopLevelItemKind::Definition(_) => todo!(),
        TopLevelItemKind::TypeDefinition(_) => GeneralizedType::unit(),
        TopLevelItemKind::TraitDefinition(_) => GeneralizedType::unit(),
        TopLevelItemKind::TraitImpl(_) => todo!(),
        TopLevelItemKind::EffectDefinition(_) => GeneralizedType::unit(),
        TopLevelItemKind::Extern(_) => todo!(),
        TopLevelItemKind::Comptime(_) => todo!(),
    };

    incremental::exit_query();
    checker.finish(typ)
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeCheckResult {
    pub typ: GeneralizedType,
    pub expr_types: BTreeMap<PatternId, TopLevelType>,
}

#[allow(unused)]
struct TypeChecker<'local, 'inner> {
    compiler: &'local DbHandle<'inner>,
    context: &'local TopLevelContext,
    path_origins: BTreeMap<PathId, Origin>,
    name_origins: BTreeMap<NameId, Origin>,
    pattern_types: BTreeMap<PatternId, TopLevelType>,
    bindings: TypeBindings,
    item: TopLevelId,
    next_id: u32,
}

#[allow(unused)]
impl<'local, 'inner> TypeChecker<'local, 'inner> {
    fn new(
        item: TopLevelId, resolve: ResolutionResult, context: &'local TopLevelContext,
        compiler: &'local DbHandle<'inner>,
    ) -> Self {
        Self {
            compiler,
            item,
            context,
            bindings: Default::default(),
            path_origins: resolve.path_origins,
            name_origins: resolve.name_origins,
            next_id: 0,
            pattern_types: Default::default(),
        }
    }

    fn finish(self, typ: GeneralizedType) -> TypeCheckResult {
        TypeCheckResult { typ, expr_types: self.pattern_types }
    }

    fn next_type_variable(&mut self) -> TopLevelType {
        let id = TypeVariableId(self.next_id);
        self.next_id += 1;
        TopLevelType::TypeVariable(id)
    }

    fn store_and_return_type(&mut self, _id: ExprId, _typ: Type) -> Type {
        todo!()
    }

    fn try_get_type_for_builtin(&self, _name: &str) -> Option<TopLevelType> {
        todo!()
    }

    /// Generalize a type, making it generic. Any holes in the type become generic types.
    fn generalize(&mut self, _typ: &TopLevelType) -> GeneralizedType {
        // The `generics` list in `TopLevelDefinitionType` can only map `Type::Generic` so
        // we have to manually replace type variables here with generics with somewhat arbitrary
        // names.
        todo!("Generalize")
    }

    /// Replaces any `Type::TypeVariable`s with `Type::Generic`s.
    /// Their names are simply their integer ids converted to a string to ensure they do not
    /// collide with the names of any existing generics in the same type.
    fn replace_type_variables_with_named_generics(&mut self, typ: &TopLevelType) -> TopLevelType {
        match typ {
            TopLevelType::Generic(name) => TopLevelType::Generic(name.clone()),
            TopLevelType::TypeVariable(_) => todo!(),
            TopLevelType::Function { parameter, return_type } => {
                let parameter = Arc::new(self.replace_type_variables_with_named_generics(parameter));
                let return_type = Arc::new(self.replace_type_variables_with_named_generics(return_type));
                TopLevelType::Function { parameter, return_type }
            },
            TopLevelType::Primitive(_) => todo!(),
            TopLevelType::TypeApplication(..) => todo!(),
        }
    }

    fn check_expr(&mut self, expr: ExprId, _expected: &Type) -> Type {
        let typ = match &self.context.exprs[expr] {
            Expr::Literal(Literal::Unit) => Type::unit(),
            Expr::Literal(Literal::Integer(_, Some(kind))) => Type::Primitive(PrimitiveType::Int(*kind)),
            Expr::Literal(Literal::Float(_, Some(kind))) => Type::Primitive(PrimitiveType::Float(*kind)),
            Expr::Literal(Literal::Bool(_)) => Type::Primitive(PrimitiveType::Bool),
            Expr::Literal(Literal::Integer(_, None)) => todo!(),
            Expr::Literal(Literal::Float(_, None)) => todo!(),
            Expr::Literal(Literal::String(_)) => todo!(),
            Expr::Literal(Literal::Char(_)) => todo!(),
            Expr::Variable(_identifier) => {
                // If this is a built-in, get that type. Otherwise, lookup or query its type.
                todo!()
            },
            Expr::Call(_call) => todo!(),
            Expr::Lambda(_lambda) => todo!(),
            Expr::Sequence(_items) => todo!(),
            Expr::Definition(_definition) => todo!(),
            Expr::MemberAccess(_member_access) => todo!(),
            Expr::Index(_index) => todo!(),
            Expr::If(_if) => todo!(),
            Expr::Match(_match) => todo!(),
            Expr::Handle(_handle) => todo!(),
            Expr::Reference(_reference) => todo!(),
            Expr::TypeAnnotation(_type_annotation) => todo!(),
            Expr::Quoted(_quoted) => todo!(),
            Expr::Error => Type::error(),
        };
        self.store_and_return_type(expr, typ)
    }

    fn instantiate(&mut self, _typ: &GeneralizedType) -> TopLevelType {
        todo!()
    }

    fn unify(&mut self, _actual: &Type, _expected: &Type, _id: ExprId) {
        todo!()
    }
}
