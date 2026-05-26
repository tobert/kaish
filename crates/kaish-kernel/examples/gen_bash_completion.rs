//! Generate a bash completion script for kaish's `echo` builtin from its
//! clap_derive struct. Proof-of-life for the clap-migration sweep — every
//! builtin's argv layer is a clap::Command, so clap_complete can generate
//! shell completions for them automatically.
//!
//! Run with:
//! ```bash
//! cargo run --example gen_bash_completion -p kaish-kernel > contrib/echo.bash
//! ```
//!
//! The struct below mirrors `crates/kaish-kernel/src/tools/builtin/echo.rs`.
//! Builtins' clap structs are module-private; this example duplicates the
//! shape so a third-party project can regenerate completions without depending
//! on internal kernel modules.

use clap::{CommandFactory, Parser};
use clap_complete::{generate, Shell};

#[derive(Parser, Debug)]
#[command(name = "echo", about = "Print arguments to standard output")]
struct EchoArgs {
    /// Do not output trailing newline.
    #[arg(short = 'n', long = "no_newline")]
    _no_newline: bool,

    /// Render structured output as JSON.
    #[arg(long)]
    _json: bool,

    /// Values to print.
    #[arg(hide = true)]
    _words: Vec<String>,
}

fn main() {
    let mut cmd = EchoArgs::command();
    generate(Shell::Bash, &mut cmd, "echo", &mut std::io::stdout());
}
