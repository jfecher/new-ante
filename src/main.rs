//! Welcome to this repository! You're in the entry point to the program where we handle
//! command-line arguments and invoke the rest of the compiler.
//!
//! Compared to a traditional pipeline-style compiler, the main difference in architecture
//! of this compiler comes from it being pull-based rather than push-based. So instead of
//! starting by lexing everything, then parsing, name resolution, type inference, etc.,
//! we start by saying "I want a compiled program!" Then the function to get us a compiled
//! program says "well, I need a type-checked Ast for that." Then our type inference pass
//! says "I need a name-resolved ast," and so on. So this compiler still has the same
//! passes you know and love (and listed further down), they're just composed together a
//! bit differently.
//!
//! List of compiler passes and the source file to find more about them in:
//! - Lexing `src/lexer/mod.rs`:
//! - Parsing `src/parser/mod.rs`:
//! - Name Resolution `src/name_resolution/mod.rs`:
//! - Type Inference `src/type_inference/mod.rs`:
//!
//! Non-passes:
//! - `src/errors.rs`: Defines each error used in the program as well as the `Location` struct
//! - `src/incremental.rs`: Some plumbing for the inc-complete library which also defines
//!   which functions we're caching the result of.
use clap::{CommandFactory, Parser};
use cli::{Cli, Completions};
use diagnostics::Diagnostic;
use incremental::{CompileFile, Db, FileData, FileId, Parse, Resolve, SourceFile};
use name_resolution::namespace::{CrateId, LocalModuleId, SourceFileId};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::{
    collections::BTreeSet, fs::File, io::{Read, Write}, path::{Path, PathBuf}, sync::Arc
};

use crate::diagnostics::Errors;

// All the compiler passes:
// (listed out of order because `cargo fmt` alphabetizes them)
mod find_files;
mod definition_collection;
mod lexer;
mod name_resolution;
mod parser;
mod type_inference;
mod backend;

// Util modules:
mod cli;
mod diagnostics;
mod incremental;
mod vecmap;
mod iterator_extensions;

// Deserialize the compiler from our metadata file.
// If we fail, just default to a fresh compiler with no cached compilations.
fn make_compiler(metadata_file: &Path, incremental: bool) -> Db {
    let crates = find_files::find_all_crates();
    if incremental {
        if let Ok(text) = read_file(metadata_file) {
            let mut db: Db = ron::from_str(&text).unwrap_or_default();
            db.storage_mut().crates = crates;
            return db;
        }
    }
    let mut db = Db::default();
    db.storage_mut().crates = crates;
    db
}

fn main() {
    if let Ok(Completions { shell_completion }) = Completions::try_parse() {
        let mut cmd = Cli::command();
        let name = cmd.get_name().to_string();
        clap_complete::generate(shell_completion, &mut cmd, name, &mut std::io::stdout());
    } else {
        compile(Cli::parse())
    }
}

fn compile(args: Cli) {
    let filename = Path::new(&args.file);
    let metadata_file = filename.with_extension("inc");

    let mut compiler = make_compiler(&metadata_file, args.incremental);

    let source = read_file(filename).unwrap_or_else(|error| {
        eprintln!("error: {error}");
        std::process::exit(1);
    });

    let file_name = Arc::new(PathBuf::from(filename));
    let input_id = path_to_id(&file_name);
    compiler.update_input(FileId(file_name.clone()), input_id);

    let file = FileData::new(file_name.clone(), source);
    compiler.update_input(SourceFile(input_id), Arc::new(file));

    // First, run through our input file and any imports recursively to find any
    // files which have changed. These are the inputs to our incremental compilation
    // and we can't dynamically update our inputs within another query. Instead, we
    // can query to collect them all and update them here at top-level.
    let (_files, mut errors) = find_files::collect_all_files(file_name, &mut compiler);

    let more_errors = if args.parse {
        display_parse_tree(&compiler, input_id)
    } else {
        // Only name resolution is implemented currently
        display_name_resolution(&compiler, input_id)
    };

    errors.extend(more_errors);

    for error in errors {
        eprintln!("{}", error.display(true, &compiler));
    }

    if args.incremental {
        if let Err(error) = write_metadata(compiler, &metadata_file) {
            eprintln!("\n{error}");
        }
    }
}

fn display_parse_tree(compiler: &Db, input: SourceFileId) -> Vec<Diagnostic> {
    let result = Parse(input).get(compiler);
    println!("{}", result.cst.display(&result.top_level_data));
    result.diagnostics.clone()
}

fn display_name_resolution(compiler: &Db, input: SourceFileId) -> Vec<Diagnostic> {
    let parse = Parse(input).get(compiler);
    let mut diagnostics = Vec::new();

    for item in &parse.cst.top_level_items {
        let resolve = Resolve(item.id).get(compiler);
        diagnostics.extend(resolve.errors);
    }

    diagnostics
}

pub fn path_to_id(path: &Path) -> SourceFileId {
    let local_module_id = LocalModuleId(parser::ids::hash(path) as u32);
    // Temporarily assign crate id while crates are unimplemented
    SourceFileId { crate_id: CrateId(1), local_module_id }
}

/// Compile all the files in the set to python files. In a real compiler we may want
/// to compile each as an independent llvm or cranelift module then link them all
/// together at the end.
#[allow(unused)]
fn compile_all(files: BTreeSet<SourceFileId>, compiler: &mut Db) -> Errors {
    files.into_par_iter().flat_map(|file| {
        let (_text, errors) = CompileFile(file).get(compiler);
        errors
    }).collect()
}

fn write_file(file_name: &Path, text: &str) -> Result<(), String> {
    let mut metadata_file =
        File::create(file_name).map_err(|error| format!("Failed to create file `{}`:\n{error}", file_name.display()))?;

    let text = text.as_bytes();
    metadata_file.write_all(text).map_err(|error| format!("Failed to write to file `{}`:\n{error}", file_name.display()))
}

/// This could be changed so that we only write if the metadata actually
/// changed but to simplify things we just always write.
fn write_metadata(compiler: Db, metadata_file: &Path) -> Result<(), String> {
    // Using `to_writer` here would avoid the intermediate step of creating the string
    let serialized = ron::to_string(&compiler).map_err(|error| format!("Failed to serialize database:\n{error}"))?;
    write_file(metadata_file, &serialized)
}

fn read_file(file_name: &std::path::Path) -> Result<String, String> {
    let mut file = File::open(file_name).map_err(|error| format!("Failed to open `{}`:\n{error}", file_name.display()))?;

    let mut text = String::new();
    file.read_to_string(&mut text).map_err(|error| format!("Failed to read from file `{}`:\n{error}", file_name.display()))?;

    Ok(text)
}
