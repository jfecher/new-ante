use std::{collections::{BTreeMap, HashMap}, sync::Arc};

use namespace::{CrateId, Namespace};
use serde::{Deserialize, Serialize};

pub mod namespace;

use crate::{
    errors::{Diagnostic, Errors, Location},
    incremental::{self, CrateData, DbHandle, GetStatement, Resolve, VisibleDefinitions},
    parser::{cst::{Expr, Path, TopLevelItemKind}, ids::{ExprId, TopLevelId}, TopLevelContext},
};

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct ResolutionResult {
    /// This resolution is for a single top level id so all expressions within are in the
    /// context of that id.
    pub origins: BTreeMap<ExprId, Origin>,
    pub errors: Errors,
}

struct Resolver<'local, 'inner> {
    item: TopLevelId,
    links: BTreeMap<ExprId, Origin>,
    errors: Errors,
    names_in_global_scope: BTreeMap<Arc<String>, TopLevelId>,
    names_in_local_scope: BTreeMap<Arc<String>, ExprId>,
    context: &'local TopLevelContext,
    compiler: &'local DbHandle<'inner>,
}

/// Where was this variable defined?
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Origin {
    /// This name comes from this top level definition
    TopLevelDefinition(TopLevelId),
    /// This name is the parameter of a lambda expression.
    Parameter(ExprId),
    /// This name comes from a local binding
    Local(ExprId),
    /// This name did not resolve, try to perform type based resolution on it during type inference
    TypeResolution,
}

/// Retrieves the crate dependencies for the current crate
pub fn dependencies<'db>(compiler: &'db DbHandle) -> &'db BTreeMap<CrateId, CrateData> {
    &compiler.storage().crates
}

pub fn resolve_impl(context: &Resolve, compiler: &DbHandle) -> ResolutionResult {
    incremental::enter_query();
    let (statement, statement_ctx) = GetStatement(context.0.clone()).get(compiler);
    incremental::println(format!("Resolving {}", statement.kind.name()));

    // Note that we discord errors here because they're errors for the entire file and we are
    // resolving just one statement in it. This does mean that `CompileFile` will later need to
    // manually query `VisibleDefinition` to pick these errors back up.
    let (names_in_scope, _errors) = VisibleDefinitions(context.0.source_file).get(compiler);

    let mut resolver = Resolver::new(compiler, context, names_in_scope, &statement_ctx);

    match &statement.kind {
        TopLevelItemKind::Definition(definition) => {
            resolver.resolve_expr(definition.rhs);
        },
        TopLevelItemKind::TypeDefinition(_type_definition) => todo!(),
        TopLevelItemKind::TraitDefinition(_trait_definition) => todo!(),
        TopLevelItemKind::TraitImpl(_trait_impl) => todo!(),
        TopLevelItemKind::EffectDefinition(_effect_definition) => todo!(),
        TopLevelItemKind::Extern(_) => todo!(),
        TopLevelItemKind::Comptime(_comptime) => todo!(),
    }

    incremental::exit_query();
    resolver.result()
}

impl<'local, 'inner> Resolver<'local, 'inner> {
    fn new(
        compiler: &'local DbHandle<'inner>,
        resolve: &Resolve,
        names_in_scope: BTreeMap<Arc<String>, TopLevelId>,
        context: &'local TopLevelContext,
    ) -> Self {
        Self {
            compiler,
            item: resolve.0.clone(),
            names_in_global_scope: names_in_scope,
            links: Default::default(),
            errors: Vec::new(),
            names_in_local_scope: Default::default(),
            context,
        }
    }

    fn result(self) -> ResolutionResult {
        ResolutionResult { origins: self.links, errors: self.errors }
    }

    fn namespace(&self) -> Namespace {
        Namespace::Module(self.item.source_file.crate_id, self.item.source_file.local_module_id)
    }

    /// Retrieve each visible namespace in the given namespace, restricting the namespace
    /// to only items visible from `self.namespace()`
    fn visible_namespaces_in(&self, _namespace: Namespace) -> &HashMap<String, Namespace> {
        todo!("visible_namespaces_in")
    }

    /// Retrieve each visible item in the given namespace, restricting the namespace
    /// to only items visible from `self.namespace()`
    fn visible_items_in(&self, _namespace: Namespace) -> &HashMap<String, Origin> {
        todo!("visible_items_in")
    }

    /// Lookup the given path in the given namespace
    fn lookup_in<'a, Iter>(&mut self, mut path: Iter, mut namespace: Namespace) -> Option<Origin> 
        where Iter: ExactSizeIterator<Item = &'a (String, Location)>
    {
        while path.len() > 1 {
            let (item_name, item_location) = path.next().unwrap();
            let visible_namespaces = self.visible_namespaces_in(namespace);

            if let Some(next_namespace) = visible_namespaces.get(item_name) {
                namespace = next_namespace.clone();
            } else {
                let name = item_name.clone();
                let location = item_location.clone();
                self.errors.push(Diagnostic::NamespaceNotFound { name, location });
                todo!("Namespace not found")
            }
        }

        let (name, location) = path.next().unwrap();
        assert_eq!(path.len(), 0);

        if matches!(namespace, Namespace::Local) {
            if let Some(expr) = self.names_in_local_scope.get(name) {
                return Some(Origin::Parameter(*expr));
            }
        }

        let items = self.visible_items_in(namespace);
        if let Some(origin) = items.get(name) {
            Some(origin.clone());
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

    fn lookup(&mut self, path: &Path) -> Option<Origin> {
        let mut components = path.components.iter().peekable();

        if components.len() > 1 {
            let (first, _) = components.peek().unwrap();

            // Check if it is an absolute path
            let crates = dependencies(self.compiler);
            for (crate_id, crate_data) in crates {
                if **first == crate_data.name {
                    // Discard the crate name
                    components.next();
                    return self.lookup_in(components, Namespace::crate_(*crate_id));
                }
            }
        }

        // Not an absolute path
        self.lookup_in(components, Namespace::Local)
    }

    fn link(&mut self, path: &Path, expr: ExprId) {
        if let Some(origin) = self.lookup(path) {
            self.links.insert(expr, origin);
        }
    }

    fn resolve_expr(&mut self, expr: ExprId) {
        match &self.context.exprs[expr] {
            Expr::Literal(_literal) => (),
            Expr::Variable(identifier) => self.link(&identifier, expr),
            Expr::Call(call) => {
                self.resolve_expr(call.function);
                for arg in &call.arguments {
                    self.resolve_expr(*arg);
                }
            },
            Expr::Lambda(lambda) => {
                // Resolve body with the parameter name in scope
                for (parameter, _parameter_type) in &lambda.parameters {
                    let parameter = Arc::new(parameter.clone());
                    // TODO: Need a unique id for each parameter
                    self.names_in_local_scope.insert(parameter, todo!());
                }

                self.resolve_expr(lambda.body);

                // Then remember to either remove the parameter name from scope, or if we shadowed
                // an existing name, then re-insert that one.
                if let Some(old_name) = old_name {
                    self.names_in_local_scope.insert(parameter_name.name.clone(), old_name);
                } else {
                    self.names_in_local_scope.remove(&parameter_name.name);
                }
            },
            Expr::Sequence(sequence) => {
                for item in sequence {
                    self.resolve_expr(item.expr);
                }
            }
            Expr::Definition(_) => (),
            Expr::MemberAccess(access) => {
                self.resolve_expr(access.object);
            }
            Expr::Index(index) => {
                self.resolve_expr(index.object);
                self.resolve_expr(index.index);
            },
            Expr::If(if_) => {
                self.resolve_expr(if_.condition);
                self.resolve_expr(if_.then);
                if let Some(else_) = if_.else_ {
                    self.resolve_expr(else_);
                }
            },
            Expr::Match(match_) => {
                self.resolve_expr(match_.expression);
                for (pattern, branch) in &match_.cases {
                    self.resolve_expr(*branch);
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
}
