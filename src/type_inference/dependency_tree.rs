use petgraph::graph::DiGraph;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};

use crate::{incremental::{DbHandle, GetItem, Resolve, TypeCheckSCC, TypeInferenceDependencyGraph}, iterator_extensions::vecmap, parser::{cst::TopLevelItemKind, ids::TopLevelId}, type_inference::get_type::try_get_type};

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeCheckOrder {}

pub fn dependency_tree_impl(context: &TypeInferenceDependencyGraph, db: &DbHandle) -> TypeCheckOrder {
    let mut graph = DiGraph::new();
    let mut item_to_index = FxHashMap::default();
    let mut index_to_item = FxHashMap::default();

    let mut add_node = |graph: &mut DiGraph<_, _>, item| {
        let index = graph.add_node(());
        item_to_index.insert(item, index);
        index_to_item.insert(index, item);
        index
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

    // tarjan_scc returns SCCs in post_order, which is the order we want to analyze in.
    for scc in petgraph::algo::tarjan_scc(&graph) {
        let scc = vecmap(scc, |index| index_to_item[&index]);
        TypeCheckSCC(scc).get(db);
    }

    TypeCheckOrder {}
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
