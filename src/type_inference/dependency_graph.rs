use petgraph::graph::DiGraph;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

use crate::{incremental::{DbHandle, GetItem, Resolve, TypeCheck, TypeCheckSCC}, iterator_extensions::vecmap, parser::{cst::TopLevelItemKind, ids::TopLevelId}, type_inference::{get_type::try_get_type, type_context::TypeContext, types::TypeBindings, IndividualTypeCheckResult, TypeCheckSCCResult}};

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeCheckResult {
    pub result: IndividualTypeCheckResult,
    pub types: TypeContext,
    pub bindings: TypeBindings,
}

/// Type check the given item by building a type inference dependency graph, finding the
/// SCCs in the graph, and deferring to TypeCheckSCC.
pub fn type_check_impl(context: &TypeCheck, db: &DbHandle) -> TypeCheckResult {
    let mut graph = DiGraph::new();
    let mut item_to_index = FxHashMap::default();
    let mut index_to_item = FxHashMap::default();

    let mut add_node = |graph: &mut DiGraph<_, _>, item| {
        if let Some(index) = item_to_index.get(&item) {
            *index
        } else {
            let index = graph.add_node(());
            item_to_index.insert(item, index);
            index_to_item.insert(index, item);
            index
        }
    };

    let mut queue = vec![context.0];

    while let Some(item) = queue.pop() {
        let resolution = Resolve(context.0).get(db);
        let item_index = add_node(&mut graph, item);

        for dependency_id in resolution.referenced_items {
            let dependency_index = add_node(&mut graph, dependency_id);

            if item_lacks_known_type(dependency_id, db) {
                queue.push(dependency_id);
                graph.update_edge(item_index, dependency_index, ());
            }
        }
    }

    let mut result = None;

    // tarjan_scc returns SCCs in post_order, which is the order we want to analyze in.
    for scc in petgraph::algo::tarjan_scc(&graph) {
        let scc = vecmap(scc, |index| index_to_item[&index]);
        let contains_target = scc.contains(&context.0);
        let scc_result = TypeCheckSCC(scc).get(db);

        if contains_target {
            assert!(result.is_none());
            result = Some(TypeCheckResult::from_scc_result(scc_result, context.0));
        }
    }

    result.expect("Target id is not in SCC")
}

/// If the dependency in question lacks a known type it means we must infer its
/// type before we infer the type of the item that refers to it. These edges
/// are used to build a dependency tree for type inference where cycles represent
/// mutually recursive functions without type annotations.
fn item_lacks_known_type(dependency_id: TopLevelId, db: &DbHandle) -> bool {
    let (item, context) = GetItem(dependency_id).get(db);

    // Only Definitions matter here but the full match is written in case more
    // TopLevelItemKinds are added in the future which do matter for this analysis.
    match &item.kind {
        TopLevelItemKind::Definition(definition) => {
            let resolve = Resolve(dependency_id).get(db);
            try_get_type(definition, &context, &resolve).is_none()
        }
        TopLevelItemKind::TypeDefinition(_) => false,
        TopLevelItemKind::TraitDefinition(_) => false,
        TopLevelItemKind::TraitImpl(_) => false,
        TopLevelItemKind::EffectDefinition(_) => false,
        TopLevelItemKind::Extern(_) => false,
        // Comptime items shouldn't be possible to be referred to in this way
        TopLevelItemKind::Comptime(_) => false,
    }
}

impl TypeCheckResult {
    fn from_scc_result(mut result: TypeCheckSCCResult, target_id: TopLevelId) -> Self {
        Self {
            result: result.items.remove(&target_id).expect("from_scc_result: expected target item"),
            types: result.types,
            bindings: result.bindings,
        }
    }
}
