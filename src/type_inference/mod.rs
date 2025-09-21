use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};
use types::{Type, TypeBindings};

use crate::{
    diagnostics::Diagnostic, incremental::{self, DbHandle, GetItem, Resolve, TypeCheck}, name_resolution::ResolutionResult, parser::{
        cst::TopLevelItemKind,
        ids::{ExprId, NameId, TopLevelId},
        TopLevelContext,
    }, type_inference::{errors::{Locateable, TypeErrorKind}, type_context::TypeContext, type_id::TypeId, types::{GeneralizedType, TypeVariableId}}
};

pub mod errors;
pub mod type_context;
pub mod type_id;
pub mod types;
mod get_type;
mod cst_traversal;

pub use get_type::get_type_impl;

/// Actually type check a statement and its contents.
/// Unlike `get_type_impl`, this always type checks the expressions inside a statement
/// to ensure they type check correctly.
pub fn type_check_impl(context: &TypeCheck, compiler: &DbHandle) -> TypeCheckResult {
    incremental::enter_query();
    let (item, item_context) = GetItem(context.0).get(compiler);
    incremental::println(format!("Type checking {:?}", item.id));

    let resolve = Resolve(context.0).get(compiler);
    let mut checker = TypeChecker::new(context.0, resolve, &item_context, compiler);

    let typ = match &item.kind {
        TopLevelItemKind::Definition(definition) => checker.check_definition(definition),
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
    pub name_types: BTreeMap<NameId, TypeId>,
    pub expr_types: BTreeMap<ExprId, TypeId>,
    pub types: TypeContext,
}

#[allow(unused)]
struct TypeChecker<'local, 'inner> {
    compiler: &'local DbHandle<'inner>,
    context: &'local TopLevelContext,
    types: TypeContext,
    resolve: ResolutionResult,
    name_types: BTreeMap<NameId, TypeId>,
    expr_types: BTreeMap<ExprId, TypeId>,
    bindings: TypeBindings,
    item: TopLevelId,
    next_id: u32,
}

impl<'local, 'inner> TypeChecker<'local, 'inner> {
    fn new(
        item: TopLevelId, resolve: ResolutionResult, context: &'local TopLevelContext,
        compiler: &'local DbHandle<'inner>,
    ) -> Self {
        Self {
            compiler,
            item,
            context,
            types: TypeContext::new(),
            bindings: Default::default(),
            resolve,
            next_id: 0,
            name_types: Default::default(),
            expr_types: Default::default(),
        }
    }

    fn finish(self, typ: GeneralizedType) -> TypeCheckResult {
        TypeCheckResult {
            expr_types: self.expr_types,
            name_types: self.name_types,
            types: self.types,
            typ,
        }
    }

    fn next_type_variable(&mut self) -> TypeId {
        let id = TypeVariableId(self.next_id);
        self.next_id += 1;
        self.types.get_or_insert_type(Type::Variable(id))
    }

    /// Generalize a type, making it generic. Any holes in the type become generic types.
    fn generalize(&mut self, _typ: TypeId) -> GeneralizedType {
        // The `generics` list in `TopLevelDefinitionType` can only map `Type::Generic` so
        // we have to manually replace type variables here with generics with somewhat arbitrary
        // names.
        todo!("Generalize")
    }

    fn instantiate(&mut self, typ: &GeneralizedType) -> TypeId {
        let substitutions = typ.generics.iter().map(|generic| {
            (*generic, self.next_type_variable())
        }).collect();

        typ.typ.substitute(&mut self.types, &substitutions)
    }

    fn unify(&mut self, actual_id: TypeId, expected_id: TypeId, kind: TypeErrorKind, locator: impl Locateable) {
        if self.try_unify(actual_id, expected_id).is_err() {
            let actual = self.type_to_string(actual_id);
            let expected = self.type_to_string(expected_id);
            let location = locator.locate(self);
            self.compiler.accumulate(Diagnostic::TypeError { actual, expected, kind, location });
        }
    }

    fn type_to_string(&self, typ: TypeId) -> String {
        self.types.get_type(typ).display(&self.bindings, &self.types, &self.context.names).to_string()
    }

    /// Try to unify the given types, returning `Err(())` on error without pushing a Diagnostic.
    ///
    /// Note that any type variable bindings will remain bound.
    fn try_unify(&mut self, actual_id: TypeId, expected_id: TypeId) -> Result<(), ()> {
        if actual_id == expected_id {
            return Ok(());
        }

        match (self.types.get_type(actual_id), self.types.get_type(expected_id)) {
            (Type::Variable(actual), _) => {
                if let Some(actual) = self.bindings.get(actual) {
                    self.try_unify(*actual, expected_id)
                } else {
                    self.try_bind_type_variable(*actual, actual_id, expected_id)
                }
            },
            (_, Type::Variable(expected)) => {
                if let Some(expected) = self.bindings.get(expected) {
                    self.try_unify(actual_id, *expected)
                } else {
                    self.try_bind_type_variable(*expected, expected_id, actual_id)
                }
            }
            (Type::Function(actual), Type::Function(expected)) => {
                if actual.parameters.len() != expected.parameters.len() {
                    return Err(());
                }
                let actual = actual.clone();
                let expected = expected.clone();
                for (actual, expected) in actual.parameters.into_iter().zip(expected.parameters) {
                    self.try_unify(actual, expected)?;
                }
                self.try_unify(actual.effects, expected.effects)?;
                self.try_unify(actual.return_type, expected.return_type)
            },
            (Type::Application(actual_constructor, actual_args), Type::Application(expected_constructor, expected_args)) => {
                if actual_args.len() != expected_args.len() {
                    return Err(());
                }
                let actual_args = actual_args.clone();
                let expected_args = expected_args.clone();
                self.try_unify(*actual_constructor, *expected_constructor)?;
                for (actual, expected) in actual_args.into_iter().zip(expected_args) {
                    self.try_unify(actual, expected)?;
                }
                Ok(())
            },
            (Type::Reference(actual_mutability, actual_sharedness), Type::Reference(expected_mutability, expected_sharedness)) => {
                if actual_mutability == expected_mutability && actual_sharedness == expected_sharedness {
                    Ok(())
                } else {
                    Err(())
                }
            },
            (actual, other) if actual == other => Ok(()),
            (_, _) => Err(()),
        }
    }

    /// Try to bind a type variable, possibly erroring instead if the binding would lead
    /// to a recursive type.
    fn try_bind_type_variable(&mut self, id: TypeVariableId, type_variable_type_id: TypeId, binding: TypeId) -> Result<(), ()> {
        // This should be prevented by the `actual_id == expected_id` check in `unify`
        // Otherwise we need to ensure this case would not issue an `occurs` error.
        assert_ne!(type_variable_type_id, binding);

        if self.occurs(binding, id) {
            // Recursive type error
            Err(())
        } else {
            self.bindings.insert(id, binding);
            Ok(())
        }
    }

    /// True if `variable` occurs within `typ`.
    /// Used to prevent the creation of infinitely recursive types when binding type variables.
    fn occurs(&self, typ: TypeId, variable: TypeVariableId) -> bool {
        match self.types.get_type(typ) {
            Type::Primitive(_) | Type::Reference(..) | Type::Generic(_) => false,
            Type::Variable(candidate_id) => {
                if let Some(binding) = self.bindings.get(candidate_id) {
                    self.occurs(*binding, variable)
                } else {
                    *candidate_id == variable
                }
            },
            Type::Function(function_type) => {
                function_type.parameters.iter().any(|param| self.occurs(*param, variable))
                    || self.occurs(function_type.return_type, variable)
                    || self.occurs(function_type.effects, variable)
            },
            Type::Application(constructor, args) => {
                self.occurs(*constructor, variable)
                    || args.iter().any(|arg| self.occurs(*arg, variable))
            },
        }
    }
}
