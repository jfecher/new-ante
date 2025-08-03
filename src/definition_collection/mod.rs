use std::{path::PathBuf, sync::Arc};

use crate::{
    diagnostics::{Diagnostic, Errors, Location}, incremental::{
        self, DbHandle, Definitions, ExportedDefinitions, ExportedTypes, FileId, GetImports, Methods, Parse, VisibleDefinitions, VisibleDefinitionsResult, VisibleTypes
    }, parser::{cst::{ItemName, Name, TopLevelItem, TopLevelItemKind}, ids::NameId, ParseResult}
};

/// Collect all definitions which should be visible to expressions within this file.
/// This includes all top-level definitions within this file, as well as any imported ones.
pub fn visible_definitions_impl(context: &VisibleDefinitions, db: &DbHandle) -> Arc<VisibleDefinitionsResult> {
    incremental::enter_query();
    incremental::println(format!("Collecting visible definitions in {:?}", context.0));

    let mut visible = ExportedDefinitions(context.0).get(db).as_ref().clone();

    // This should always be cached. Ignoring errors here since they should already be
    // included in ExportedDefinitions' errors
    let ast = Parse(context.0).get(db);

    for import in &ast.cst.imports {
        // Ignore errors from imported files. We want to only collect errors
        // from this file. Otherwise we'll duplicate errors.
        // TODO: This id should be optional in case the path doesn't exist
        // TODO: `import.path` includes the imported item too - we want just the module path
        let import_file_id = FileId(import.module_path.clone()).get(db);
        let exported = ExportedDefinitions(import_file_id).get(db);

        for (exported_name, exported_id) in &exported.definitions {
            if let Some(existing) = visible.definitions.get(exported_name) {
                // This reports the location the item was defined in, not the location it was imported at.
                // I could improve this but instead I'll leave it as an exercise for the reader!
                let first_location = existing.location(db);
                let second_location = import.location.clone();
                let name = exported_name.clone();
                visible.diagnostics.push(Diagnostic::ImportedNameAlreadyInScope { name, first_location, second_location });
            } else {
                visible.definitions.insert(exported_name.clone(), *exported_id);
            }
        }
    }

    incremental::exit_query();
    Arc::new(visible)
}

pub fn visible_types_impl(context: &VisibleTypes, db: &DbHandle) -> (Definitions, Errors) {
    incremental::enter_query();
    incremental::println(format!("Collecting visible types in {:?}", context.0));

    let (mut definitions, mut errors) = ExportedTypes(context.0).get(db);

    // This should always be cached. Ignoring errors here since they should already be
    // included in ExportedTypes' errors
    let ast = Parse(context.0).get(db);

    for import in &ast.cst.imports {
        // Ignore errors from imported files. We want to only collect errors
        // from this file. Otherwise we'll duplicate errors.
        let import_file_id = FileId(import.module_path.clone()).get(db);
        let (exports, _errors) = ExportedTypes(import_file_id).get(db);

        for (exported_name, exported_id) in exports {
            if let Some(existing) = definitions.get(&exported_name) {
                // This reports the location the item was defined in, not the location it was imported at.
                // I could improve this but instead I'll leave it as an exercise for the reader!
                let first_location = existing.location(db);
                let second_location = import.location.clone();
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

/// Collect only the exported types within a file.
pub fn exported_types_impl(context: &ExportedTypes, db: &DbHandle) -> (Definitions, Errors) {
    incremental::enter_query();
    incremental::println(format!("Collecting exported definitions in {:?}", context.0));

    let result = Parse(context.0).get(db);
    let mut definitions = Definitions::default();
    let mut errors = result.diagnostics.clone();

    // Collect each definition, issuing an error if there is a duplicate name (imports are not counted)
    for item in result.cst.top_level_items.iter() {
        if let TopLevelItemKind::TypeDefinition(definition) = &item.kind {
            let name = &result.top_level_data[&item.id].names[definition.name];

            if let Some(existing) = definitions.get(name) {
                let first_location = existing.location(db);
                let second_location = item.id.location(db);
                let name = name.clone();
                errors.push(Diagnostic::NameAlreadyInScope { name, first_location, second_location });
            } else {
                definitions.insert(name.clone(), item.id.clone());
            }
        }
    }

    incremental::exit_query();
    (definitions, errors)
}

/// Collect only the exported definitions within a file.
pub fn exported_definitions_impl(context: &ExportedDefinitions, db: &DbHandle) -> Arc<VisibleDefinitionsResult> {
    incremental::enter_query();
    incremental::println(format!("Collecting exported definitions in {:?}", context.0));

    let result = Parse(context.0).get(db);
    let mut definitions = Definitions::default();
    let mut methods = Methods::default();
    let mut diagnostics = result.diagnostics.clone();

    // Collect each definition, issuing an error if there is a duplicate name (imports are not counted)
    for item in result.cst.top_level_items.iter() {
        match item.kind.name() {
            ItemName::Single(name) => {
                let name = &result.top_level_data[&item.id].names[name];
                resolve_single(name, item, &mut definitions, &mut diagnostics, db);
            }
            ItemName::Method { type_name, item_name } => {
                resolve_method(type_name, item_name, item, &definitions, &mut methods, &result, &mut diagnostics, db);
            },
            ItemName::None => (),
        }
    }

    incremental::exit_query();
    Arc::new(VisibleDefinitionsResult { definitions, methods, diagnostics })
}

fn resolve_single(name: &Name, item: &TopLevelItem, definitions: &mut Definitions, errors: &mut Vec<Diagnostic>, db: &DbHandle) {
    if let Some(existing) = definitions.get(name) {
        let first_location = existing.location(db);
        let second_location = item.id.location(db);
        let name = name.clone();
        errors.push(Diagnostic::NameAlreadyInScope { name, first_location, second_location });
    } else {
        definitions.insert(name.clone(), item.id.clone());
    }
}

fn resolve_method(type_name: NameId, item_name: NameId, item: &TopLevelItem, definitions: &Definitions, methods: &mut Methods, parse: &ParseResult, errors: &mut Vec<Diagnostic>, db: &DbHandle) {
    let context = &parse.top_level_data[&item.id];
    let type_name = &context.names[type_name];
    let item_name = &context.names[item_name];

    // Methods can only be declared on a type declared in the same file, so look in the same file
    // for the type.
    if let Some(object_type) = definitions.get(type_name) {
        let object_methods = methods.entry(*object_type).or_default();
        resolve_single(item_name, item, object_methods, errors, db);
    } else {
        todo!("error: method defined for unknown or external type")
    }
}

/// Collects the file names of all imports within this file.
pub fn get_imports_impl(context: &GetImports, db: &DbHandle) -> Vec<(Arc<PathBuf>, Location)> {
    incremental::enter_query();
    incremental::println(format!("Collecting imports of {:?}", context.0));

    // Ignore parse errors for now, we can report them later
    let result = Parse(context.0).get(db);
    let mut imports = Vec::new();

    // Collect each definition, issuing an error if there is a duplicate name (imports are not counted)
    for import in result.cst.imports.iter() {
        // We don't care about duplicate imports.
        // This method is only used for finding input files and the top-level
        // will filter out any repeats.
        imports.push((import.module_path.clone(), import.location.clone()));
    }

    incremental::exit_query();
    imports
}
