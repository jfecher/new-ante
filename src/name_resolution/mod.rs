use std::{collections::{BTreeMap, HashMap}, sync::Arc};

use namespace::{CrateId, ModuleId, Namespace};
use serde::{Deserialize, Serialize};

pub mod namespace;

use crate::{
    errors::{Diagnostic, Errors},
    incremental::{self, CrateName, DbHandle, Dependencies, GetStatement, Resolve, VisibleDefinitions, VisibleTypes},
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
    crate_id: CrateId,
    module_id: ModuleId,
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
}

pub fn dependencies_impl(_context: &Dependencies, _compiler: &DbHandle) -> Vec<CrateId> {
    todo!("dependencies")
}

pub fn crate_name_impl(_context: &CrateName, _compiler: &DbHandle) -> String {
    todo!("crate_name")
}

pub fn resolve_impl(context: &Resolve, compiler: &DbHandle) -> ResolutionResult {
    incremental::enter_query();
    let (statement, statement_ctx) = GetStatement(context.0.clone()).get(compiler);
    incremental::println(format!("Resolving {}", statement.kind.name()));

    // Note that we discord errors here because they're errors for the entire file and we are
    // resolving just one statement in it. This does mean that `CompileFile` will later need to
    // manually query `VisibleDefinition` to pick these errors back up.
    let (names_in_scope, _errors) = VisibleDefinitions { file_name: context.0.file_path.clone() }.get(compiler);

    let mut resolver = Resolver::new(compiler, context.0.clone(), names_in_scope, &statement_ctx);

    match &statement.kind {
        TopLevelItemKind::Definition(definition) => {
            resolver.resolve_expr(definition.rhs);
        },
        TopLevelItemKind::TypeDefinition(_type_definition) => todo!(),
        TopLevelItemKind::TraitDefinition(_trait_definition) => todo!(),
        TopLevelItemKind::TraitImpl(_trait_impl) => todo!(),
        TopLevelItemKind::EffectDefinition(_effect_definition) => todo!(),
        TopLevelItemKind::Extern(_) => todo!(),
    }

    incremental::exit_query();
    resolver.result()
}

impl<'local, 'inner> Resolver<'local, 'inner> {
    fn new(
        compiler: &'local DbHandle<'inner>, item: TopLevelId,
        names_in_scope: BTreeMap<Arc<String>, TopLevelId>,
        context: &'local TopLevelContext,
    ) -> Self {
        Self {
            compiler,
            item,
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
        Namespace::Module(self.crate_id, self.module_id)
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

    fn lookup_in<'a, Iter>(&self, mut path: Iter, mut namespace: Namespace) -> Option<Origin> 
        where Iter: ExactSizeIterator<Item = &'a String>
    {
        while path.len() > 1 {
            let item_name = path.next().unwrap();
            let visible_namespaces = self.visible_namespaces_in(namespace);

            if let Some(next_namespace) = visible_namespaces.get(item_name) {
                namespace = next_namespace.clone();
            } else {
                todo!("Namespace not found")
            }
        }

        let name = path.next().unwrap();
        assert_eq!(path.len(), 0);

        let items = self.visible_items_in(namespace);
        if let Some(origin) = items.get(name) {
            Some(origin.clone())
        } else {
            todo!("Name not found in path")
        }
        // Check local parameters first. They shadow global definitions
        // if let Some(expr) = self.names_in_local_scope.get(name) {
        //     return Some(Origin::Parameter(*expr));
        // }
        // if let Some(statement) = self.names_in_global_scope.get(name) {
        //     return Some(Origin::TopLevelDefinition(statement.clone()));
        // }
        // None
    }

    fn lookup(&self, path: &Path) -> Option<Origin> {
        let mut components = path.components.iter().peekable();

        if components.len() > 1 {
            let first = components.peek().unwrap();

            // Check if it is an absolute path
            let crates = Dependencies(self.crate_id).get(self.compiler);
            for crate_id in crates {
                if **first == CrateName(crate_id).get(self.compiler) {
                    // Discard the crate name
                    components.next();
                    return self.lookup_in(components, Namespace::crate_(crate_id));
                }
            }
        }

        // Not an absolute path
        self.lookup_in(components, Namespace::Module(self.crate_id, self.module_id))
    }

    fn link(&mut self, path: &Path, expr: ExprId) {
        if let Some(origin) = self.lookup(path) {
            self.links.insert(expr, origin);
        } else {
            let location = expr.location(&self.item, self.compiler);
            self.errors.push(Diagnostic::NameNotInScope { name: name.clone(), location });
        }
    }

    fn resolve_expr(&mut self, expr: ExprId) {
        match &self.context.exprs[expr] {
            Expr::Literal(_literal) => (),
            Expr::Variable(identifier) => self.link(&identifier.name, expr),
            Expr::Call(call) => {
                self.resolve_expr(call.function);
                for arg in &call.arguments {
                    self.resolve_expr(*arg);
                }
            },
            Expr::Lambda(lambda) => {
                // Resolve body with the parameter name in scope
                let old_name = self.names_in_local_scope.insert(parameter_name.name.clone(), parameter_name.id);
                self.resolve_expr(&body);

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
            Expr::Reference(reference) => todo!(),
            Expr::TypeAnnotation(type_annotation) => todo!(),
            Expr::Error => (),
        }
    }
}
