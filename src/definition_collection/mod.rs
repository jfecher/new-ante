use std::sync::Arc;

use crate::{
    errors::{Diagnostic, Errors, Location},
    incremental::{
        self, parse, DbHandle, Definitions, ExportedDefinitions, GetImports, Parse, VisibleDefinitions
    }, parser::cst::{TopLevelItem, TopLevelItemKind},
};

/// Collect all definitions which should be visible to expressions within this file.
/// This includes all top-level definitions within this file, as well as any imported ones.
pub fn visible_definitions_impl(context: &VisibleDefinitions, db: &DbHandle) -> (Definitions, Errors) {
    incremental::enter_query();
    incremental::println(format!("Collecting visible definitions in {}", context.file_name));

    let (mut definitions, mut errors) = ExportedDefinitions { file_name: context.file_name.clone() }.get(db);

    // This should always be cached. Ignoring errors here since they should already be
    // included in ExportedDefinitions' errors
    let ast = Parse { file_name: context.file_name.clone() }.get(db);

    for import in &ast.cst.imports {
        // Ignore errors from imported files. We want to only collect errors
        // from this file. Otherwise we'll duplicate errors.
        let (exports, _errors) = ExportedDefinitions { file_name: import.path.clone() }.get(db);

        for (exported_name, exported_id) in exports {
            if let Some(existing) = definitions.get(&exported_name) {
                // This reports the location the item was defined in, not the location it was imported at.
                // I could improve this but instead I'll leave it as an exercise for the reader!
                let first_location = existing.location(db);
                let second_location = import_id.location(db);
                let name = exported_name;
                errors.push(Diagnostic::ImportedNameAlreadyInScope { name, first_location, second_location });
            } else {
                definitions.insert(exported_name, exported_id);
            }
        }
    }

    incremental::exit_query();
    (definitions, errors)
}

/// Collect only the exported definitions within a file.
/// For this small example language, this is all top-level definitions in a file, except for imported ones.
pub fn exported_definitions_impl(context: &ExportedDefinitions, db: &DbHandle) -> (Definitions, Errors) {
    incremental::enter_query();
    incremental::println(format!("Collecting exported definitions in {}", context.file_name));

    let result = Parse { file_name: context.file_name.clone() }.get(db);
    let mut definitions = Definitions::default();
    let mut errors = result.diagnostics.clone();

    // Collect each definition, issuing an error if there is a duplicate name (imports are not counted)
    for item in result.cst.top_level_items.iter() {
        if let TopLevelItemKind::Definition(definition) = &item.kind {
            if let Some(existing) = definitions.get(&definition.path) {
                let first_location = existing.location(db);
                let second_location = definition.path.id.location(&definition.id, db);
                let name = definition.name.name.clone();
                errors.push(Diagnostic::NameAlreadyInScope { name, first_location, second_location });
            } else {
                definitions.insert(definition.name.name.clone(), definition.id.clone());
            }
        }
    }

    incremental::exit_query();
    (definitions, errors)
}

/// Collects the file names of all imports within this file.
pub fn get_imports_impl(context: &GetImports, db: &DbHandle) -> Vec<(Arc<String>, Location)> {
    incremental::enter_query();
    incremental::println(format!("Collecting imports of {}", context.file_name));

    // Ignore parse errors for now, we can report them later
    let result = Parse { file_name: context.file_name.clone() }.get(db);
    let mut imports = Vec::new();

    // Collect each definition, issuing an error if there is a duplicate name (imports are not counted)
    for import in result.cst.imports.iter() {
        // We don't care about duplicate imports.
        // This method is only used for finding input files and the top-level
        // will filter out any repeats.
        let location = id.location(db);
        imports.push((import.path.clone(), location));
    }

    incremental::exit_query();
    imports
}
