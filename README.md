
A work in progress rewrite of Ante's compiler.

Goals for this new compiler:
- Better parse error messages & better recovery after parse errors
- Keep enough source code information to be able to write the AST back into the file for formatting and for writing in inferred types
- Incremental, doesn't repeat work across multiple compilations for unchanged source code
- Multi-threaded
- Less buggy trait dispatch
