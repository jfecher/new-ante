use std::{collections::{BTreeMap, HashMap}, sync::Arc};

use namespace::{CrateId, Namespace};
use serde::{Deserialize, Serialize};

pub mod namespace;

use crate::{
    errors::{Diagnostic, Errors, Location},
    incremental::{self, CrateData, DbHandle, GetStatement, Resolve, VisibleDefinitions},
    parser::{cst::{Expr, Path, Pattern, TopLevelItemKind}, ids::{ExprId, PatternId, TopLevelId}, TopLevelContext},
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
    names_in_local_scope: Vec<BTreeMap<Arc<String>, PatternId>>,
    context: &'local TopLevelContext,
    compiler: &'local DbHandle<'inner>,
}

/// Where was this variable defined?
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum Origin {
    /// This name comes from this top level definition
    TopLevelDefinition(TopLevelId),
    /// This name is the parameter of a lambda expression.
    Parameter(PatternId),
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
            names_in_local_scope: vec![Default::default()],
            context,
        }
    }

    fn result(self) -> ResolutionResult {
        ResolutionResult { origins: self.links, errors: self.errors }
    }

    fn namespace(&self) -> Namespace {
        Namespace::Module(self.item.source_file.crate_id, self.item.source_file.local_module_id)
    }

    fn push_local_scope(&mut self) {
        self.names_in_local_scope.push(Default::default());
    }

    /// TODO: Check for unused names
    fn pop_local_scope(&mut self) {
        self.names_in_local_scope.pop();
    }

    /// Declares a name in local scope.
    fn declare_name(&mut self, name: Arc<String>, id: PatternId) {
        let scope = self.names_in_local_scope.last_mut().unwrap();
        scope.insert(name, id);
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
            for scope in self.names_in_local_scope.iter().rev() {
                if let Some(expr) = scope.get(name) {
                    return Some(Origin::Parameter(*expr));
                }
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
                self.push_local_scope();
                for parameter in &lambda.parameters {
                    self.declare_names_in_pattern(*parameter);
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
                    self.declare_names_in_pattern(*pattern);
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
    fn declare_names_in_pattern(&mut self, pattern: PatternId) {
        match &self.context.patterns[pattern] {
            Pattern::Variable(path) => {
                if let Some((name, _)) = path.ident() {
                    let name = Arc::new(name.clone());
                    self.declare_name(name.clone(), pattern);
                }
            },
            Pattern::Literal(_) => (),
            // In a constructor pattern such as `Struct foo bar baz` or `(a, b)` the arguments
            // should be declared but the function itself should never be.
            Pattern::Constructor(_, args) => {
                for arg in args {
                    self.declare_names_in_pattern(*arg);
                }
            },
        }
    }
}
