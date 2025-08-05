use std::{collections::{BTreeMap, BTreeSet}, path::PathBuf, sync::Arc};

use rustc_hash::FxHashSet;
use serde::{Deserialize, Serialize};

use crate::{diagnostics::{Diagnostic, Errors, Location}, incremental::{CrateData, Db, FileData, FileId, GetImports, SourceFile}, name_resolution::namespace::{CrateId, SourceFileId, LOCAL_CRATE, STDLIB_CRATE}, read_file};

const STDLIB_PATH: &str = "stdlib/Std";

/// Before we start incremental compilation we have to collect all the inputs. This means finding
/// all the source files used. To do this we find all the crates used first then search through all
/// the files used by those crates.
pub fn collect_all_files(start_file: Arc<PathBuf>, compiler: &mut Db) -> (BTreeSet<SourceFileId>, Errors) {
    let mut finder = Finder::new();
    let mut remaining_files = BTreeSet::new();
    let start_file = FileId(start_file).get(compiler);
    remaining_files.insert(start_file);

    while !remaining_files.is_empty() {
        remaining_files = finder.find_files_step(remaining_files, compiler);
    }

    (finder.done, finder.errors)
}

type FileName = Arc<PathBuf>;

struct Finder {
    queue: scc::Queue<(FileName, Location)>,
    done: BTreeSet<SourceFileId>,
    thread_pool: rayon::ThreadPool,
    errors: Errors,
}

#[derive(Default, Serialize, Deserialize)]
#[serde(transparent)]
pub struct CrateGraph {
    pub crates: BTreeMap<CrateId, CrateData>,
}

// TODO:
// - Error for cyclic dependencies
// - Handle crate versions
pub fn find_all_crates() -> CrateGraph {
    let mut graph = CrateGraph::default();
    graph.crates.insert(STDLIB_CRATE, CrateData {
        name: "Std".to_string(),
        path: PathBuf::from(STDLIB_PATH),
        dependencies: Vec::new(),
    });
    // TODO: track name for local crate. Currently we only compile single source files
    graph.crates.insert(LOCAL_CRATE, CrateData {
        name: "Local".to_string(),
        path: PathBuf::from("."),
        dependencies: Vec::new(),
    });

    let mut stack = vec![LOCAL_CRATE];
    let mut finished = FxHashSet::default();
    finished.insert(STDLIB_CRATE);

    while let Some(crate_id) = stack.pop() {
        let dependencies = find_crate_dependencies(&mut graph, crate_id);
        for dependency in &dependencies {
            if finished.insert(*dependency) {
                stack.push(*dependency);
            }
        }

        graph.crates.get_mut(&crate_id).unwrap().dependencies = dependencies;
    }
    graph
}

fn find_crate_dependencies(graph: &mut CrateGraph, crate_id: CrateId) -> Vec<CrateId> {
    // Every crate currently depends on the stdlib
    graph.crates.insert(STDLIB_CRATE, CrateData {
        name: "Std".to_string(),
        path: PathBuf::from(STDLIB_PATH),
        dependencies: Vec::new(),
    });

    let mut deps_folder = graph.crates[&crate_id].path.clone();
    deps_folder.push("deps");

    let mut remaining = vec![deps_folder];
    let mut dependencies = Vec::new();

    // Push every crate in the `deps` folder as a new crate
    while let Some(deps_folder) = remaining.pop() {
        for dependency in deps_folder.read_dir().expect("Failed to read `deps` directory to find crate dependencies") {
            if let Ok(dependency) = dependency {
                let path = dependency.path();
                let name = path.with_extension("").file_name().unwrap().to_string_lossy().into_owned();
                let id = new_crate_id(&graph, &name, 0);
                dependencies.push(id);

                graph.crates.insert(id, CrateData {
                    name,
                    path,
                    dependencies: Vec::new(),
                });
            }
        }
    }

    dependencies
}

/// Create a new unique CrateId from the crate's name and version
fn new_crate_id(graph: &CrateGraph, name: &String, version: u32) -> CrateId {
    for collisions in 0.. {
        let hash = crate::parser::ids::hash((name, version, collisions));
        let id = CrateId(hash as u32);

        if !graph.crates.contains_key(&id) {
            return id;
        }
    }
    unreachable!()
}

impl Finder {
    fn new() -> Self {
        let thread_pool = rayon::ThreadPoolBuilder::new().build().unwrap();
        Self { thread_pool, queue: Default::default(), done: Default::default(), errors: Vec::new() }
    }

    /// Search through all files in the queue, parse them, and wait until they finish.
    /// Afterward, update all the new inputs found. We must wait for them to finish before
    /// updating any new inputs, even though this limits concurrency, because it is not possible
    /// to update inputs while incremental computations are running in general (inc-complete
    /// forbids this, salsa cancels ongoing computations, etc). 
    fn find_files_step(&mut self, files: BTreeSet<SourceFileId>, compiler: &mut Db) -> BTreeSet<SourceFileId> {
        let compiler_ref: &Db = &compiler;
        let queue = &self.queue;
        self.thread_pool.scope(|scope| {
            for file in files {
                if self.done.contains(&file) {
                    continue;
                }
                self.done.insert(file);

                // Parse and collect imports of the file in a separate thread. This can be helpful
                // when files contain many imports, so we can parse many of them simultaneously.
                scope.spawn(move |_| {
                    for import in GetImports(file).get(compiler_ref) {
                        queue.push(import);
                    }
                });
            }
        });

        // Wait for all threads to complete before updating new files because we need exclusive
        // access to Compiler
        let mut new_files = BTreeSet::new();
        while let Some(file_and_location) = self.queue.pop() {
            let file = file_and_location.0.clone();
            let location = file_and_location.1.clone();

            let file_id = super::path_to_id(&file);
            if self.done.contains(&file_id) {
                continue;
            }

            let text = read_file(&file).unwrap_or_else(|_| {
                self.errors.push(Diagnostic::UnknownImportFile { file_name: file.clone(), location });

                // Treat file as an empty string. This will probably just lead to more errors but does
                // let us continue to collect name/type errors for other files
                String::new()
            });

            let file_data = FileData::new(file, text);
            SourceFile(file_id).set(compiler, Arc::new(file_data));
            new_files.insert(file_id);
        }
        new_files
    }
}
