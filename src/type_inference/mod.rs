use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};
use types::{Type, TypeBindings};

use crate::{
    incremental::{self, DbHandle, GetItem, Resolve, TypeCheck},
    name_resolution::{Origin, ResolutionResult},
    parser::{
        cst::{Expr, Literal, TopLevelItemKind},
        ids::{ExprId, NameId, PathId, PatternId, TopLevelId},
        TopLevelContext,
    },
    type_inference::{
        type_id::TypeId,
        types::{GeneralizedType, TopLevelType, TypeVariableId},
    },
};

mod get_type;
pub mod type_context;
pub mod type_id;
pub mod types;

pub use get_type::get_type_impl;

/// Actually type check a statement and its contents.
/// Unlike `get_type_impl`, this always type checks the expressions inside a statement
/// to ensure they type check correctly.
pub fn type_check_impl(context: &TypeCheck, compiler: &DbHandle) -> TypeCheckResult {
    incremental::enter_query();
    let (item, item_context) = GetItem(context.0).get(compiler);
    incremental::println(format!("Type checking {:?}", item.id));

    let resolve = Resolve(context.0).get(compiler);
    let checker = TypeChecker::new(context.0, resolve, &item_context, compiler);

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

    fn next_type_variable(&mut self) -> TypeId {
        let _id = TypeVariableId(self.next_id);
        self.next_id += 1;
        todo!()
    }

    fn store_and_return_type(&mut self, _expr: ExprId, typ: TypeId) -> TypeId {
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

    fn check_expr(&mut self, expr: ExprId, _expected: &Type) -> TypeId {
        let typ = match &self.context.exprs[expr] {
            Expr::Literal(literal) => self.check_literal(literal),
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
            Expr::Error => TypeId::ERROR,
        };
        self.store_and_return_type(expr, typ)
    }

    fn check_literal(&mut self, literal: &Literal) -> TypeId {
        match literal {
            Literal::Unit => TypeId::UNIT,
            Literal::Integer(_, Some(kind)) => TypeId::integer(*kind),
            Literal::Float(_, Some(kind)) => TypeId::float(*kind),
            Literal::Bool(_) => TypeId::BOOL,
            Literal::Integer(_, None) => todo!(),
            Literal::Float(_, None) => todo!(),
            Literal::String(_) => TypeId::STRING,
            Literal::Char(_) => TypeId::CHAR,
        }
    }

    fn instantiate(&mut self, _typ: &GeneralizedType) -> TopLevelType {
        todo!()
    }

    fn unify(&mut self, _actual: &Type, _expected: &Type, _id: ExprId) {
        todo!()
    }
}
