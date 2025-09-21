use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};
use types::{Type, TypeBindings};

use crate::{
    diagnostics::Diagnostic, incremental::{self, DbHandle, GetItem, Resolve, TypeCheck}, iterator_extensions::vecmap, name_resolution::ResolutionResult, parser::{
        cst::TopLevelItemKind,
        ids::{ExprId, NameId, PathId},
        TopLevelContext,
    }, type_inference::{errors::{Locateable, TypeErrorKind}, generics::Generic, type_context::TypeContext, type_id::TypeId, types::{GeneralizedType, TopLevelType, TypeVariableId}}
};

pub mod errors;
pub mod type_context;
pub mod type_id;
pub mod types;
mod get_type;
mod generics;
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
    let mut checker = TypeChecker::new(resolve, &item_context, compiler);

    let typ = match &item.kind {
        TopLevelItemKind::Definition(definition) => checker.check_definition(definition),
        TopLevelItemKind::TypeDefinition(_) => GeneralizedType::unit(),
        TopLevelItemKind::TraitDefinition(_) => GeneralizedType::unit(),
        TopLevelItemKind::TraitImpl(trait_impl) => checker.check_impl(trait_impl),
        TopLevelItemKind::EffectDefinition(_) => GeneralizedType::unit(),
        TopLevelItemKind::Extern(extern_) => checker.check_extern(extern_),
        TopLevelItemKind::Comptime(comptime) => checker.check_comptime(comptime),
    };

    incremental::exit_query();
    checker.finish(typ)
}

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeCheckResult {
    pub typ: GeneralizedType,
    pub name_types: BTreeMap<NameId, TypeId>,
    pub path_types: BTreeMap<PathId, TypeId>,
    pub expr_types: BTreeMap<ExprId, TypeId>,
    pub types: TypeContext,
    pub bindings: TypeBindings,
}

#[allow(unused)]
struct TypeChecker<'local, 'inner> {
    compiler: &'local DbHandle<'inner>,
    context: &'local TopLevelContext,
    types: TypeContext,
    resolve: ResolutionResult,
    name_types: BTreeMap<NameId, TypeId>,
    path_types: BTreeMap<PathId, TypeId>,
    expr_types: BTreeMap<ExprId, TypeId>,
    bindings: TypeBindings,
    next_id: u32,
}

impl<'local, 'inner> TypeChecker<'local, 'inner> {
    fn new(
        resolve: ResolutionResult, context: &'local TopLevelContext,
        compiler: &'local DbHandle<'inner>,
    ) -> Self {
        Self {
            compiler,
            context,
            types: TypeContext::new(),
            bindings: Default::default(),
            resolve,
            next_id: 0,
            name_types: Default::default(),
            path_types: Default::default(),
            expr_types: Default::default(),
        }
    }

    fn finish(self, typ: GeneralizedType) -> TypeCheckResult {
        TypeCheckResult {
            expr_types: self.expr_types,
            name_types: self.name_types,
            path_types: self.path_types,
            types: self.types,
            typ,
            bindings: self.bindings,
        }
    }

    fn next_type_variable(&mut self) -> TypeId {
        let id = TypeVariableId(self.next_id);
        self.next_id += 1;
        self.types.get_or_insert_type(Type::Variable(id))
    }

    /// Generalize a type, making it generic. Any holes in the type become generic types.
    fn generalize(&mut self, typ: TypeId) -> GeneralizedType {
        let free_vars = self.free_vars(typ); 
        let substitutions = free_vars.into_iter().map(|var| {
            (var, self.types.get_or_insert_type(Type::Generic(Generic::Inferred(var))))
        }).collect();

        let typ = self.substitute(typ, &substitutions);
        self.promote_to_top_level_type(typ).generalize()
    }

    fn substitute(&mut self, typ: TypeId, bindings: &TypeBindings) -> TypeId {
        match self.follow_type(typ) {
            Type::Primitive(_) | Type::Generic(_) | Type::Reference(..) => typ,
            Type::Variable(id) => {
                match bindings.get(id) {
                    Some(binding) => *binding,
                    None => typ,
                }
            },
            Type::Function(function) => {
                let function = function.clone();
                let parameters = vecmap(&function.parameters, |param| self.substitute(*param, bindings));
                let return_type = self.substitute(function.return_type, bindings);
                let effects = self.substitute(function.effects, bindings);
                let function = Type::Function(types::FunctionType { parameters, return_type, effects });
                self.types.get_or_insert_type(function)
            },
            Type::Application(constructor, args) => {
                let (constructor, args) = (*constructor, args.clone());
                let constructor = self.substitute(constructor, bindings);
                let args = vecmap(args, |arg| self.substitute(arg, bindings));
                self.types.get_or_insert_type(Type::Application(constructor, args))
            },
        }
    }

    /// Promotes a type to a top-level type.
    /// Panics if the typ contains an unbound type variable.
    fn promote_to_top_level_type(&self, typ: TypeId) -> TopLevelType {
        match self.follow_type(typ) {
            Type::Primitive(primitive) => TopLevelType::Primitive(*primitive),
            Type::Generic(name) => TopLevelType::Generic(*name),
            Type::Variable(_) => panic!("promote_to_top_level_type called with type containing an unbound type variable"),
            Type::Function(function_type) => {
                let parameters = vecmap(&function_type.parameters, |typ| self.promote_to_top_level_type(*typ));
                let return_type = Box::new(self.promote_to_top_level_type(function_type.return_type));
                TopLevelType::Function { parameters, return_type }
            },
            Type::Application(constructor, args) => {
                let constructor = Box::new(self.promote_to_top_level_type(*constructor));
                let args = vecmap(args, |arg| self.promote_to_top_level_type(*arg));
                TopLevelType::TypeApplication(constructor, args)
            },
            Type::Reference(..) => {
                todo!("convert Type::Reference to TopLevelType")
            },
        }
    }

    /// Return the list of unbound type variables within this type
    fn free_vars(&self, typ: TypeId) -> Vec<TypeVariableId> {
        fn free_vars_helper(this: &TypeChecker, typ: TypeId, free_vars: &mut Vec<TypeVariableId>) {
            match this.follow_type(typ) {
                Type::Primitive(_) | Type::Reference(..) => (),
                Type::Generic(_) => (),
                Type::Variable(id) => {
                    // The number of free vars is expected to remain too small so we're
                    // not too worried about asymptotic behavior. It is more important we
                    // maintain the ordering of insertion.
                    if !free_vars.contains(id) {
                        free_vars.push(*id);
                    }
                },
                Type::Function(function) => {
                    for parameter in &function.parameters {
                        free_vars_helper(this, *parameter, free_vars);
                    }
                    free_vars_helper(this, function.return_type, free_vars);
                    free_vars_helper(this, function.effects, free_vars);
                },
                Type::Application(constructor, args) => {
                    free_vars_helper(this, *constructor, free_vars);
                    for arg in args {
                        free_vars_helper(this, *arg, free_vars);
                    }
                },
            }
        }

        let mut free_vars = Vec::new();
        free_vars_helper(self, typ, &mut free_vars);
        free_vars
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
        typ.to_string(&self.types, &self.bindings, &self.context.names)
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

    /// Retrieve a Type then follow all its type variable bindings so that we only return
    /// `Type::Variable` if the type variable is unbound. Note that this may still return
    /// a composite type such as `Type::Application` with bound type variables within.
    fn follow_type(&self, id: TypeId) -> &Type {
        match self.types.get_type(id) {
            typ @ Type::Variable(id) => {
                match self.bindings.get(&id) {
                    Some(binding) => self.follow_type(*binding),
                    None => typ,
                }
            },
            other => other,
        }
    }
}
