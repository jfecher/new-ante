use std::{collections::BTreeMap, sync::Arc};

use namespace::{Namespace, SourceFileId, LOCAL_CRATE};
use serde::{Deserialize, Serialize};

pub mod namespace;

use crate::{
    diagnostics::{Diagnostic, Errors, Location},
    incremental::{self, DbHandle, ExportedTypes, GetCrateGraph, GetItem, Resolve, VisibleDefinitions},
    parser::{
        cst::{
            Comptime, Declaration, Definition, EffectDefinition, EffectType, Expr, Extern, Generics, Path, Pattern,
            TopLevelItemKind, TraitDefinition, TraitImpl, Type, TypeDefinition, TypeDefinitionBody,
        },
        ids::{ExprId, NameId, PathId, PatternId, TopLevelId},
        TopLevelContext,
    },
};

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct ResolutionResult {
    /// This resolution is for a single top level id so all expressions within are in the
    /// context of that id.
    pub path_origins: BTreeMap<PathId, Origin>,
    pub name_origins: BTreeMap<NameId, Origin>,
    pub errors: Errors,
}

struct Resolver<'local, 'inner> {
    item: TopLevelId,
    path_links: BTreeMap<PathId, Origin>,
    name_links: BTreeMap<NameId, Origin>,
    errors: Errors,
    names_in_global_scope: &'local BTreeMap<Arc<String>, TopLevelId>,
    names_in_local_scope: Vec<BTreeMap<Arc<String>, NameId>>,
    context: &'local TopLevelContext,
    compiler: &'local DbHandle<'inner>,
}

/// Where was this variable defined?
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Origin {
    /// This name comes from this top level definition
    TopLevelDefinition(TopLevelId),
    /// This name comes from a local binding (parameter, let-binding, match-binding, etc)
    Local(NameId),
    /// This name did not resolve, try to perform type based resolution on it during type inference
    TypeResolution,
}

pub fn resolve_impl(context: &Resolve, compiler: &DbHandle) -> ResolutionResult {
    incremental::enter_query();
    let (statement, statement_ctx) = GetItem(context.0.clone()).get(compiler);
    incremental::println(format!("Resolving {}", statement.kind.name_string(&statement_ctx)));

    // Note that we discord errors here because they're errors for the entire file and we are
    // resolving just one statement in it. This does mean that `CompileFile` will later need to
    // manually query `VisibleDefinition` to pick these errors back up.
    let visible = VisibleDefinitions(context.0.source_file).get(compiler);
    let names_in_scope = &visible.definitions;

    let mut resolver = Resolver::new(compiler, context, names_in_scope, &statement_ctx);

    match &statement.kind {
        TopLevelItemKind::Definition(definition) => {
            resolver.resolve_expr(definition.rhs);
        },
        TopLevelItemKind::TypeDefinition(type_definition) => resolver.resolve_type_definition(type_definition),
        TopLevelItemKind::TraitDefinition(trait_definition) => resolver.resolve_trait_definition(trait_definition),
        TopLevelItemKind::TraitImpl(trait_impl) => resolver.resolve_trait_impl(trait_impl),
        TopLevelItemKind::EffectDefinition(effect_definition) => resolver.resolve_effect_definition(effect_definition),
        TopLevelItemKind::Extern(extern_) => resolver.resolve_extern(extern_),
        TopLevelItemKind::Comptime(comptime_) => resolver.resolve_comptime(comptime_),
    }

    incremental::exit_query();
    resolver.result()
}

impl<'local, 'inner> Resolver<'local, 'inner> {
    fn new(
        compiler: &'local DbHandle<'inner>, resolve: &Resolve,
        names_in_scope: &'local BTreeMap<Arc<String>, TopLevelId>, context: &'local TopLevelContext,
    ) -> Self {
        Self {
            compiler,
            item: resolve.0.clone(),
            names_in_global_scope: names_in_scope,
            path_links: Default::default(),
            name_links: Default::default(),
            errors: Vec::new(),
            names_in_local_scope: vec![Default::default()],
            context,
        }
    }

    fn result(self) -> ResolutionResult {
        ResolutionResult { path_origins: self.path_links, name_origins: self.name_links, errors: self.errors }
    }

    #[allow(unused)]
    fn namespace(&self) -> Namespace {
        Namespace::Module(self.item.source_file)
    }

    fn push_local_scope(&mut self) {
        self.names_in_local_scope.push(Default::default());
    }

    /// TODO: Check for unused names
    fn pop_local_scope(&mut self) {
        self.names_in_local_scope.pop();
    }

    /// Declares a name in local scope.
    fn declare_name(&mut self, id: NameId) {
        let scope = self.names_in_local_scope.last_mut().unwrap();
        let name = self.context.names[id].clone();
        scope.insert(name, id);
        self.name_links.insert(id, Origin::Local(id));
    }

    /// Retrieve each visible namespace in the given namespace, restricting the namespace
    /// to only items visible from `self.namespace()`
    fn get_child_namespace(&self, name: &String, namespace: Namespace) -> Option<Namespace> {
        match namespace {
            Namespace::Local => {
                if let Some(submodule) = self.get_item_in_submodule(self.item.source_file, name) {
                    return Some(submodule);
                }

                let type_id = self.names_in_global_scope.get(name)?;
                let (item, _) = GetItem(*type_id).get(self.compiler);
                if matches!(&item.kind, TopLevelItemKind::TypeDefinition(_)) {
                    Some(Namespace::Type(*type_id))
                } else {
                    None
                }
            },
            Namespace::Type(_) => return None,
            Namespace::Module(id) => {
                if let Some(submodule) = self.get_item_in_submodule(id, name) {
                    return Some(submodule);
                }

                let exported = ExportedTypes(id).get(self.compiler).0;
                exported.get(name).copied().map(Namespace::Type)
            },
        }
    }

    fn get_item_in_submodule(&self, parent_module: SourceFileId, name: &str) -> Option<Namespace> {
        parent_module.get(self.compiler).submodules.get(name).copied().map(Namespace::Module)
    }

    /// Retrieve each visible item in the given namespace, restricting the namespace
    /// to only items visible from `self.namespace()`
    fn get_item_in_namespace(&self, name: &String, namespace: Namespace) -> Option<Origin> {
        match namespace {
            this if this == self.namespace() => self.lookup_local_name(name),
            Namespace::Local => self.lookup_local_name(name),
            Namespace::Module(file_id) => {
                let visible = &VisibleDefinitions(file_id).get(self.compiler);
                visible.definitions.get(name).copied().map(Origin::TopLevelDefinition)
            },
            Namespace::Type(top_level_id) => {
                let visible = &VisibleDefinitions(top_level_id.source_file).get(self.compiler);
                let methods = visible.methods.get(&top_level_id)?;
                methods.get(name).copied().map(Origin::TopLevelDefinition)
            },
        }
    }

    /// Lookup the given path in the given namespace
    fn lookup_in<'a, Iter>(&mut self, mut path: Iter, mut namespace: Namespace) -> Option<Origin>
    where
        Iter: ExactSizeIterator<Item = &'a (String, Location)>,
    {
        while path.len() > 1 {
            let (item_name, item_location) = path.next().unwrap();

            if let Some(next_namespace) = self.get_child_namespace(item_name, namespace) {
                namespace = next_namespace;
            } else {
                let name = item_name.clone();
                let location = item_location.clone();
                self.errors.push(Diagnostic::NamespaceNotFound { name, location });
                return None;
            }
        }

        let (name, location) = path.next().unwrap();
        assert_eq!(path.len(), 0);

        if matches!(namespace, Namespace::Local) {
            if let Some(origin) = self.lookup_local_name(name) {
                return Some(origin);
            }
        }

        if let Some(origin) = self.get_item_in_namespace(name, namespace) {
            return Some(origin);
        }

        // No known origin.
        // If the name is capitalized we delay until type inference to auto-import variants
        let first_char = name.chars().next().unwrap();
        if first_char.is_ascii_uppercase() {
            Some(Origin::TypeResolution)
        } else {
            let location = location.clone();
            let name = Arc::new(name.clone());
            self.errors.push(Diagnostic::NameNotInScope { name, location });
            None
        }
    }

    /// Lookup a single name (not a full path) in local scope
    fn lookup_local_name(&self, name: &String) -> Option<Origin> {
        for scope in self.names_in_local_scope.iter().rev() {
            if let Some(expr) = scope.get(name) {
                return Some(Origin::Local(*expr));
            }
        }

        if let Some(item) = self.names_in_global_scope.get(name) {
            return Some(Origin::TopLevelDefinition(*item));
        }
        None
    }

    fn lookup(&mut self, path: &Path) -> Option<Origin> {
        let mut components = path.components.iter().peekable();

        if components.len() > 1 {
            let (first, _) = components.peek().unwrap();

            // Check if it is an absolute path
            let crates = GetCrateGraph.get(self.compiler);
            let local_crate = &crates[&LOCAL_CRATE];

            for dependency_id in &local_crate.dependencies {
                let dependency = &crates[dependency_id];

                if **first == dependency.name {
                    // Discard the crate name
                    components.next();
                    return self.lookup_in(components, Namespace::crate_(*dependency_id));
                }
            }
        }

        // Not an absolute path
        self.lookup_in(components, Namespace::Local)
    }

    /// Links a path to its definition or errors if it does not exist
    fn link(&mut self, path: PathId) {
        if let Some(origin) = self.lookup(&self.context.paths[path]) {
            self.path_links.insert(path, origin);
        }
    }

    fn resolve_expr(&mut self, expr: ExprId) {
        match &self.context.exprs[expr] {
            Expr::Literal(_literal) => (),
            Expr::Variable(path) => self.link(*path),
            Expr::Call(call) => {
                self.resolve_expr(call.function);
                for arg in &call.arguments {
                    self.resolve_expr(*arg);
                }
            },
            Expr::Lambda(lambda) => {
                // Resolve body with the parameter name in scope
                self.push_local_scope();
                for parameter in &lambda.parameters {
                    self.declare_names_in_pattern(*parameter, true);
                }

                self.resolve_expr(lambda.body);
                self.pop_local_scope();
            },
            Expr::Sequence(sequence) => {
                self.push_local_scope();
                for item in sequence {
                    self.resolve_expr(item.expr);
                }
                self.pop_local_scope();
            },
            Expr::Definition(_) => (),
            Expr::MemberAccess(access) => {
                self.resolve_expr(access.object);
            },
            Expr::Index(index) => {
                self.resolve_expr(index.object);
                self.resolve_expr(index.index);
            },
            Expr::If(if_) => {
                self.resolve_expr(if_.condition);

                self.push_local_scope();
                self.resolve_expr(if_.then);
                self.pop_local_scope();

                if let Some(else_) = if_.else_ {
                    self.push_local_scope();
                    self.resolve_expr(else_);
                    self.pop_local_scope();
                }
            },
            Expr::Match(match_) => {
                self.resolve_expr(match_.expression);
                for (pattern, branch) in &match_.cases {
                    self.push_local_scope();
                    self.declare_names_in_pattern(*pattern, false);
                    self.resolve_expr(*branch);
                    self.pop_local_scope();
                }
            },
            Expr::Reference(reference) => {
                self.resolve_expr(reference.rhs);
            },
            Expr::TypeAnnotation(type_annotation) => {
                self.resolve_expr(type_annotation.lhs);
            },
            Expr::Quoted(_) => (),
            Expr::Error => (),
        }
    }

    /// Declare each name in a pattern position in the given pattern, pushing the old names
    /// if any existed in the declared list.
    ///
    /// If `declare_type_vars` is true, any type variables used that are not in scope will
    /// automatically be declared. Otherwise an error will be issued.
    fn declare_names_in_pattern(&mut self, pattern: PatternId, declare_type_vars: bool) {
        match &self.context.patterns[pattern] {
            Pattern::Variable(name) => {
                self.declare_name(*name);
            },
            Pattern::Literal(_) => (),
            // In a constructor pattern such as `Struct foo bar baz` or `(a, b)` the arguments
            // should be declared but the function itself should never be.
            Pattern::Constructor(_, args) => {
                for arg in args {
                    self.declare_names_in_pattern(*arg, declare_type_vars);
                }
            },
            Pattern::Error => (),
            Pattern::TypeAnnotation(pattern, typ) => {
                self.declare_names_in_pattern(*pattern, declare_type_vars);
                self.resolve_type(typ, declare_type_vars);
            },
        }
    }

    /// Resolves a type ensuring all names used are in scope and issuing errors
    /// for any that are not. If `declare_type_vars` is set then any type variables
    /// not already in scope will be declared in the current local scope. Otherwise,
    /// an error will be issued.
    fn resolve_type(&mut self, typ: &Type, declare_type_vars: bool) {
        match typ {
            Type::Error | Type::Unit | Type::Integer(_) | Type::Float(_) | Type::String | Type::Char => (),
            Type::Named(path) => self.link(*path),
            Type::Variable(name) => self.resolve_type_variable(*name, declare_type_vars),
            Type::Function(function) => {
                for parameter in &function.parameters {
                    self.resolve_type(parameter, declare_type_vars);
                }
                self.resolve_type(&function.return_type, declare_type_vars);

                if let Some(effects) = function.effects.as_ref() {
                    for effect in effects {
                        self.resolve_effect_type(effect, declare_type_vars);
                    }
                }
            },
            Type::TypeApplication(f, args) => {
                self.resolve_type(f, declare_type_vars);
                for arg in args {
                    self.resolve_type(arg, declare_type_vars);
                }
            },
        }
    }

    /// Resolve an effect type, ensuring all names used are in scope
    fn resolve_effect_type(&mut self, effect: &EffectType, declare_type_vars: bool) {
        match effect {
            EffectType::Known(path, args) => {
                self.link(*path);

                for arg in args {
                    self.resolve_type(arg, declare_type_vars);
                }
            },
            EffectType::Variable(name_id) => self.resolve_type_variable(*name_id, declare_type_vars),
        }
    }

    fn resolve_type_variable(&mut self, name_id: NameId, declare_type_vars: bool) {
        let name = &self.context.names[name_id];

        if let Some(origin) = self.lookup_local_name(name) {
            self.name_links.insert(name_id, origin);
        } else if declare_type_vars {
            self.declare_name(name_id);
        } else {
            let location = self.context.name_locations[name_id].clone();
            let name = self.context.names[name_id].clone();
            self.errors.push(Diagnostic::NameNotFound { name, location });
        }
    }

    fn resolve_type_definition(&mut self, type_definition: &TypeDefinition) {
        self.declare_generics(&type_definition.generics);

        match &type_definition.body {
            TypeDefinitionBody::Error => (),
            TypeDefinitionBody::Struct(fields) => {
                for (_name, field_type) in fields {
                    self.resolve_type(field_type, false);
                }
            },
            TypeDefinitionBody::Enum(variants) => {
                for (_name, variant_args) in variants {
                    for arg in variant_args {
                        self.resolve_type(arg, false);
                    }
                }
            },
            TypeDefinitionBody::Alias(typ) => {
                self.resolve_type(typ, false);
            },
        }
    }

    fn declare_generics(&mut self, generics: &Generics) {
        for generic in generics {
            self.declare_name(*generic);
        }
    }

    fn declare(&mut self, declaration: &Declaration) {
        self.declare_name(declaration.name);
        self.resolve_type(&declaration.typ, true);
    }

    fn resolve_definition(&mut self, definition: &Definition) {
        if let Some(typ) = definition.typ.as_ref() {
            // TODO: We should only set declare_type_vars to true here
            // if we're resolving a top-level definition, not a local one.
            self.resolve_type(typ, true);
        }
        self.resolve_expr(definition.rhs);
    }

    fn resolve_trait_definition(&mut self, trait_definition: &TraitDefinition) {
        self.declare_generics(&trait_definition.generics);
        self.declare_generics(&trait_definition.functional_dependencies);
        for declaration in &trait_definition.body {
            self.declare(declaration);
        }
    }

    fn resolve_trait_impl(&mut self, trait_impl: &TraitImpl) {
        self.link(trait_impl.trait_path);

        for arg in &trait_impl.arguments {
            self.resolve_type(arg, true);
        }

        for definition in &trait_impl.body {
            self.resolve_definition(definition);
        }
    }

    fn resolve_effect_definition(&mut self, effect_definition: &EffectDefinition) {
        self.declare_generics(&effect_definition.generics);
        for declaration in &effect_definition.body {
            self.declare(declaration);
        }
    }

    fn resolve_extern(&mut self, extern_: &Extern) {
        self.declare(&extern_.declaration);
    }

    /// Does this require special handling? This should be resolved before runtime
    /// definitions are resolved.
    fn resolve_comptime(&mut self, comptime: &Comptime) {
        match comptime {
            Comptime::Expr(expr_id) => self.resolve_expr(*expr_id),
            Comptime::Derive(paths) => {
                for path in paths {
                    self.link(*path);
                }
            },
            Comptime::Definition(definition) => self.resolve_definition(definition),
        }
    }
}
