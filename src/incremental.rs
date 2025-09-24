use std::{cell::Cell, collections::BTreeMap, path::PathBuf, sync::Arc};

use inc_complete::{
    accumulate::Accumulator,
    define_input, define_intermediate,
    storage::{HashMapStorage, SingletonStorage},
    Storage,
};
use serde::{Deserialize, Serialize};

use crate::{
    backend, definition_collection,
    diagnostics::{Diagnostic, Location},
    find_files::CrateGraph,
    name_resolution::{
        self,
        namespace::{CrateId, SourceFileId},
        ResolutionResult,
    },
    parser::{self, cst::TopLevelItem, ids::TopLevelId, ParseResult, TopLevelContext},
    type_inference::{self, dependency_tree::TypeCheckOrder, types::GeneralizedType, TypeCheckResult},
};

/// A wrapper over inc-complete's database with our specific storage type to hold
/// all the results of our incremental computations. See docs on `Storage` for a
/// list of all the computations we cache in this way.
pub type Db = inc_complete::Db<DbStorage>;

/// Although we have a `Db` object in `main`, each incremental computation
/// only has access to a `DbHandle` which still allows them to retrieve other
/// queries but prevents them from updating inputs since that would break
/// incremental computation. These two types are specific to inc-complete but
/// any reasonable library should either prevent updating inputs while incremental
/// computations are running or cancel the running computations.
pub type DbHandle<'db> = inc_complete::DbHandle<'db, DbStorage>;

/// Here we define which functions we want to cache (through wrapper structs defined below)
/// as well as what storage we want to use for each. We don't really care for specifics so
/// `HashMapStorage` is a good default for all of them. See where each type is defined
/// for more, including which function it actually maps to (e.g. `Parse` maps to `parser::parse_impl`).
/// Generally speaking, each type here maps to a function ending with `_impl`, so when you
/// see that suffix elsewhere you know that function is incremental and is meant to be called
/// through the `Compiler` object.
#[derive(Default, Serialize, Deserialize, Storage)]
pub struct DbStorage {
    files: HashMapStorage<SourceFileId>,
    crate_graph: SingletonStorage<GetCrateGraph>,
    parse_results: HashMapStorage<Parse>,
    visible_definitions: HashMapStorage<VisibleDefinitions>,
    visible_types: HashMapStorage<VisibleTypes>,
    exported_definitions: HashMapStorage<ExportedDefinitions>,
    exported_types: HashMapStorage<ExportedTypes>,
    get_imports: HashMapStorage<GetImports>,
    resolves: HashMapStorage<Resolve>,
    top_level_items: HashMapStorage<GetItem>,
    get_types: HashMapStorage<GetType>,
    type_checks: HashMapStorage<TypeCheckSCC>,
    type_dependency_tree: SingletonStorage<TypeInferenceDependencyGraph>,
    compiled_files: HashMapStorage<CompileFile>,

    #[inc_complete(accumulate)]
    diagnostics: Accumulator<Diagnostic>,
}

std::thread_local! {
    // This is a helper to show us how many queries deep we are for our print outs
    static QUERY_NESTING: Cell<usize> = const { Cell::new(0) };
}

pub fn enter_query() {
    QUERY_NESTING.with(|cell| {
        cell.set(cell.get() + 1);
    });
}

pub fn exit_query() {
    QUERY_NESTING.with(|cell| {
        cell.set(cell.get() - 1);
    });
}

/// Currently disabled until a Cli argument is added to opt into these traces
pub fn println(_msg: String) {
    // let level = QUERY_NESTING.with(|cell| cell.get());
    // let spaces = "  ".repeat(level);

    // Thread ids are usually in the form `ThreadId(X)` or `ThreadId(XX)`.
    // Add some padding to keep output aligned for both cases.
    // println!("{:02?}: {spaces}- {msg}", std::thread::current().id());
}

#[derive(PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SourceFile {
    pub path: Arc<PathBuf>,
    pub contents: String,
    pub submodules: BTreeMap<String, SourceFileId>,
}

impl SourceFile {
    pub fn new(path: Arc<PathBuf>, contents: String) -> SourceFile {
        SourceFile { path, contents, submodules: BTreeMap::new() }
    }
}

// SourceFileIds along with CrateIds are the main inputs to the compiler.
// SourceFileIds map to a FileData which contains, among other things, the full
// source text of the file.
define_input!(10, SourceFileId -> Arc<SourceFile>, DbStorage);

#[derive(PartialEq, Eq, Serialize, Deserialize)]
pub struct Crate {
    pub name: String,

    /// Path to the folder containing this crates's files
    pub path: PathBuf,

    /// Direct dependencies of this crate
    pub dependencies: Vec<CrateId>,

    /// All source files within this crate. This excludes any files
    /// owned by dependencies.
    ///
    /// The path buf these are keyed on does not include the `src` directory.
    /// For example, a module `MyCrate.Foo.Bar` would correspond to the path `foo/bar.an`
    pub source_files: BTreeMap<Arc<PathBuf>, SourceFileId>,
}

impl Crate {
    pub fn new(name: String, path: PathBuf) -> Crate {
        Crate { name, path, dependencies: Vec::new(), source_files: BTreeMap::new() }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct GetCrateGraph;

// SourceFileIds along with CrateIds are the main inputs to the compiler.
// A CrateId maps to a Crate which is used for organizing dependencies.
define_input!(20, GetCrateGraph -> Arc<CrateGraph>, DbStorage);

/// Any full path from a crate to module in name resolution must query the
/// crate names of all dependencies. To avoid all of name resolution changing
/// if anything changes in these crate inputs we refine the query more here
/// making it depend only on the name of the crate.
#[derive(Copy, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct CrateName(pub CrateId);
define_intermediate!(30, CrateName -> Arc<String>, DbStorage, |ctx, db| {
    match GetCrateGraph.get(db).get(&ctx.0) {
        Some(crate_) => Arc::new(crate_.name.clone()),
        None => Arc::new("(none)".to_string()),
    }
});

///////////////////////////////////////////////////////////////////////////////////////////////////
/// For each file name, we cache the parse result of that file. This includes not only
/// the `Ast`, but also parse errors and some metadata tracked by the parser. Note that the
/// `ParserResult` is rather large. Checking if this has changed on each parse can be expensive.
/// If we were more concerned about this, we may want to tell inc-complete to not check it changed
/// at all, and simply assume it has since it is likely to if the input SourceFile was modified to
/// begin with (since Parse is incremental we only re-run if the input source file changed). Note
/// that because we have a `GetItem` step later to check if individual items have
/// changed, we won't re-check everything in a file even if we assume the Ast as a whole always
/// changes.
#[derive(Copy, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct Parse(pub SourceFileId);
define_intermediate!(40, assume_changed Parse -> Arc<ParseResult>, DbStorage, parser::parse_impl);

///////////////////////////////////////////////////////////////////////////////////////////////////
/// Collect all the visible definitions within a file. These are the definitions that can be
/// referred to in any expression in the file.
#[derive(Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct VisibleDefinitions(pub SourceFileId);
define_intermediate!(50, VisibleDefinitions -> Arc<VisibleDefinitionsResult>, DbStorage, definition_collection::visible_definitions_impl);

#[derive(Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct VisibleDefinitionsResult {
    pub definitions: Definitions,
    pub methods: BTreeMap<TopLevelId, Definitions>,
}

#[derive(Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct VisibleTypes(pub SourceFileId);
define_intermediate!(60, VisibleTypes -> Definitions, DbStorage, definition_collection::visible_types_impl);

/// We iterate over collected definitions within `visible_definitions_impl`. Since
/// collecting these can error, we need a stable iteration order, otherwise the order
/// we issue errors would be nondeterministic. This is why we use a BTreeMap over a
/// HashMap, since hashmap iteration in rust has a nondeterministic ordering.
pub type Definitions = BTreeMap<Arc<String>, TopLevelId>;

/// A map from each top-level type in a file to the methods defined on it.
/// If a type in a file does not have any methods defined on it, it may not be in the map.
pub type Methods = BTreeMap<TopLevelId, Definitions>;

///////////////////////////////////////////////////////////////////////////////////////////////////
/// Collect all exported definitions in a file. This separate step is important because we don't
/// want ordinary name resolution of another file to depend upon private definitions in an import.
/// Instead, it only depends on the `ExportedDefinitions` of that import.
#[derive(Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExportedDefinitions(pub SourceFileId);
define_intermediate!(70, ExportedDefinitions -> Arc<VisibleDefinitionsResult>, DbStorage, definition_collection::exported_definitions_impl);

#[derive(Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExportedTypes(pub SourceFileId);
define_intermediate!(80, ExportedTypes -> Definitions, DbStorage, definition_collection::exported_types_impl);

///////////////////////////////////////////////////////////////////////////////////////////////////
/// Retrieves the imports used by a file. This step is the first done by the compiler to collect
/// all the files used by the program. It is important this step is separate because the compiler
/// needs this to check if any inputs (source files) have changed. If they have, it needs to
/// perform some IO and call `set_soure_file` which need to be done outside of any incremental
/// compilations. So we provide this top-level utility to collect these then return without doing
/// anything else.
#[derive(Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct GetImports(pub SourceFileId);
define_intermediate!(90, GetImports -> Vec<(Arc<PathBuf>, Location)>, DbStorage, definition_collection::get_imports_impl);

///////////////////////////////////////////////////////////////////////////////////////////////////
/// Resolves a single top-level item. Note that since the granularity of this is per-item
/// this means we cache the results of this for every top-level item separately. This kind of
/// granularity helps us repeat as little work as possible but does come with the tradeoff of
/// requiring we query the `Compiler` cache more often. In a real compiler we may want to do
/// performance testing to determine if this tradeoff is worth it. An alternative to hit the cache
/// less often would resolving entire files at a time instead. In general, less granularity is
/// better for faster operations like resolution or type checking, and more granularity may be
/// better for slower operations like backend codegen. Nevertheless, this relatively fast pass is
/// defined in this granular way to provide an example of how you would do so. Plus, it is cool
/// to look at the output after changing something and see only exactly that one definition is
/// re-resolved!
#[derive(Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct Resolve(pub TopLevelId);
define_intermediate!(100, Resolve -> ResolutionResult, DbStorage, name_resolution::resolve_impl);

///////////////////////////////////////////////////////////////////////////////////////////////////
/// To go from queries which resolve entire files like `Parse` to queries that resolve only a
/// single top-level item like `Resolve` we need a way to split a large `Ast` result into smaller items,
/// in this case individual top-level items. This being cached means we check if the resulting
/// `TopLevelItem` has changed, and if not, we don't need to re-run any computations that
/// depend on that item.
#[derive(Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct GetItem(pub TopLevelId);

// This one is quick and simple, let's just define it here.
define_intermediate!(110, GetItem -> (Arc<TopLevelItem>, Arc<TopLevelContext>), DbStorage, |context, db| {
    let target_id = &context.0;
    let ast = Parse(context.0.source_file).get(db);

    for item in ast.cst.top_level_items.iter() {
        if item.id == *target_id {
            let ctx = ast.top_level_data[target_id].clone();
            return (item.clone(), ctx);
        }
    }

    // Note that panics are not cached (so avoid `catch_unwind` within incremental computations!)
    unreachable!("No TopLevelItem for id {target_id:?}")
});

///////////////////////////////////////////////////////////////////////////////////////////////////
/// Retrieves the type of a top-level item. Like `Resolve`, this is done per-item.
/// `GetType` interacts with type-inference: if a variable's type is specified then we know the
/// type from only parsing the file (and `GetItem` to find the item in question). If
/// the variable's type is inferred however, we need to  call `TypeCheck` to get the type which
/// will in turn depend on not just the types of any names used in any expressions but also the
/// name resolution results of those names.
#[derive(Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct GetType(pub TopLevelId);
define_intermediate!(120, GetType -> GeneralizedType, DbStorage, type_inference::get_type_impl);

///////////////////////////////////////////////////////////////////////////////////////////////////
/// Type check the contents of one or more top-level items. This isn't always necessary just to get
/// the type of a top-level item but for compiling we also want to ensure the contents of all
/// expresions are free from type errors. The "SCC" in the name refers to a strongly-connected
/// component specifically in regard to the type inference dependency graph which only includes
/// edges leading to definitions without type annotations. A cycle in the graph represents mutually
/// recursive definitions without type annotations, which must be inferred and generalized in one
/// group. Hence the rare case where `TypeCheckSCC` may be given more than one `TopLevelId`.
#[derive(Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeCheckSCC(pub Vec<TopLevelId>);
define_intermediate!(130, TypeCheckSCC -> TypeCheckResult, DbStorage, type_inference::type_check_impl);

///////////////////////////////////////////////////////////////////////////////////////////////////
/// Construct a dependency graph used to determine the order of type inference. This is somewhat
/// different from a general dependency tree in that the only dependencies we consider are those
/// which require type inference beforehand. Dependent definitions which are already annotated can
/// be checked in any order.
#[derive(Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeInferenceDependencyGraph(pub TopLevelId);
define_intermediate!(140, assume_changed TypeInferenceDependencyGraph -> TypeCheckOrder, DbStorage, type_inference::dependency_tree::dependency_tree_impl);

///////////////////////////////////////////////////////////////////////////////////////////////////
/// Compile a single file to a string representing python source code of that file.
/// This will also return any errors originating in that file.
#[derive(Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct CompileFile(pub SourceFileId);
define_intermediate!(150, CompileFile -> String, DbStorage, backend::compile_file_impl);
