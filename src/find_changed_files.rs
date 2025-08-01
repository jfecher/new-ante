use std::{collections::BTreeSet, path::PathBuf, sync::Arc};

use crate::{diagnostics::{Diagnostic, Errors, Location}, incremental::{Db, FileData, FileId, GetImports, SourceFile}, name_resolution::namespace::SourceFileId, read_file};

/// One limitation of query systems is that you cannot change inputs during an incremental
/// computation. For a compiler, this means dynamically discovering new files (inputs) to parse is
/// a bit more difficult. Instead of doing it within an incremental computation like the parser or
/// during name resolution, we have a separate step here to collect all the files that are used and
/// check if any have changed. Some frameworks may let you iterate through known inputs where you
/// may be able to test if a file has changed there, but inc-complete doesn't support this (yet) as
/// of version 0.5.0.
///
/// In `collect_all_changed_files`, we start by parsing the first INPUT_FILE which imports all
/// other files. We collect each import and for each import we read the new file, set the source
/// file input (which requires an exclusive &mut reference), and spawn a thread to parse that file
/// and collect the imports. Spawning multiple threads here is advantageous when a file imports
/// many source files - we can distribute work to parse many of them at once. The implementation
/// for this could be more efficient though. For example, the parser could accept the shared `queue`
/// of files to parse as an argument, and push to this queue immediately when it finds an import.
pub fn collect_all_changed_files(start_file: Arc<PathBuf>, compiler: &mut Db) -> (BTreeSet<SourceFileId>, Errors) {
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
