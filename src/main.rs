use std::io::Read;

use clap::{CommandFactory, Parser};
use cli::{Completions, Cli};
use error::Diagnostic;

mod cli;
mod lexer;
mod parser;
mod error;
mod memoizer;

fn main() {
    if let Ok(Completions { shell_completion }) = Completions::try_parse() {
        let mut cmd = Cli::command();
        let  name = cmd.get_name().to_string();
        clap_complete::generate(shell_completion, &mut cmd, name, &mut std::io::stdout());
    } else {
        compile(Cli::parse());
    }
}

fn compile(args: Cli) {
    let mut file = match std::fs::File::open(&args.file) {
        Ok(file) => file,
        Err(error) => {
            eprintln!("Unable to open file '{}'\n\nOriginal error: {error}", args.file);
            return;
        }
    };

    let mut file_contents = String::new();
    if let Err(error) = file.read_to_string(&mut file_contents) {
        eprintln!("Unable to read '{}' to a String\n\nOriginal error: {error}", args.file);
        return;
    }

    let result = parser::parse_file(&file_contents);
    println!("{}", result.item);

    if !result.errors.is_empty() {
        display_errors(&result.errors, &args.file);
    }
}

fn display_errors(errors: &[Diagnostic], file: &str) {
    for error in errors {
        println!();
        let span = error.span();
        println!("{file}:{}:{}: {error}", span.start.line, span.start.column);
    }
}
