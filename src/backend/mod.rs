use crate::{
    diagnostics::Errors,
    incremental::{CompileFile, DbHandle},
};

/// Compile a given source file to python, returning any errors in the file.
pub fn compile_file_impl(_context: &CompileFile, _compiler: &DbHandle) -> (String, Errors) {
    todo!()
}
