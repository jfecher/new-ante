use clap::{Parser, ValueHint};
use clap_complete::Shell;

#[derive(Parser, Debug)]
pub struct Completions {
    #[arg(long)]
    pub shell_completion: Shell,
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Cli {
    /// Path to the source file
    #[arg(value_hint=ValueHint::FilePath)]
    pub file: String,

    /// Lex the file and output the resulting list of tokens
    #[arg(long, short, group = "compile_mode")]
    pub lex: bool,

    /// Parse the file and output the resulting Ast
    #[arg(long, short, group = "compile_mode")]
    pub parse: bool,

    /// Use plaintext and an indicator line instead of color for pointing out error locations
    #[arg(long)]
    pub no_color: bool,
}

