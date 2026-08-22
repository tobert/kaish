//! Rendering a parsed call into the child's argv.
//!
//! ```text
//! argv = command.lead…
//!      , verb.name            (omitted for the root verb, and for omit_name)
//!      , verb.lead…
//!      , flags…               (in declaration order)
//!      , positionals…         (in source order; a `--` the agent wrote stays
//!                              where it was)
//!      , "--", tail…          (only when the tail is non-empty)
//! ```
//!
//! The executable itself is not in the rendered argv — the spawn site owns
//! argv[0].
//!
//! The wrapper never inserts a `--` the agent did not write, except to
//! introduce a non-empty tail. `git log -- main` means "paths named main",
//! not "revision main"; inserting `--` would change the program's meaning.

use super::declaration::{Flag, Style, Verb, WrappedCommand};
use super::parse::{Call, Item};

/// The rendered argv, plus where each of `call.items` landed in it.
///
/// The index map is what lets a deferred path check rewrite the word it
/// constrains once the real cwd is known.
pub(crate) struct Rendered {
    /// The child's argv, without the executable.
    pub(crate) argv: Vec<String>,
    /// Parallel to `call.items`: the argv index each item rendered at.
    pub(crate) item_argv_index: Vec<usize>,
}

/// Render `call` against its declaration.
pub(crate) fn render(declaration: &WrappedCommand, verb: &Verb, call: &Call) -> Rendered {
    let mut argv: Vec<String> = Vec::new();
    argv.extend(declaration.lead.iter().cloned());
    if !verb.omit_name
        && let Some(name) = &verb.name
    {
        argv.push(name.clone());
    }
    argv.extend(verb.lead.iter().cloned());

    // Declaration order for the flag block; source order within one
    // repeatable flag's occurrences.
    for (flag_index, flag) in verb.flags.iter().enumerate() {
        for use_ in call.flags.iter().filter(|u| u.flag_index == flag_index) {
            render_flag(flag, use_.value.as_deref(), &mut argv);
        }
    }

    let mut item_argv_index = Vec::with_capacity(call.items.len());
    let mut ends_with_dash_dash = false;
    for item in &call.items {
        item_argv_index.push(argv.len());
        match item {
            Item::Positional { value, .. } => {
                argv.push(value.clone());
                ends_with_dash_dash = false;
            }
            Item::DashDash => {
                argv.push("--".to_string());
                ends_with_dash_dash = true;
            }
            Item::Forwarded(word) => {
                argv.push(word.clone());
                ends_with_dash_dash = false;
            }
        }
    }

    if !call.tail.is_empty() {
        // The agent's own `--` already introduces the tail when it sits last;
        // a second one would be a word the agent did not write.
        if !ends_with_dash_dash {
            argv.push("--".to_string());
        }
        argv.extend(call.tail.iter().cloned());
    }

    Rendered {
        argv,
        item_argv_index,
    }
}

/// Render one flag occurrence under its declared name, whichever alias the
/// agent wrote.
fn render_flag(flag: &Flag, value: Option<&str>, argv: &mut Vec<String>) {
    let name = flag.written_name();
    match value {
        None => argv.push(name),
        Some(value) => match flag.effective_style() {
            Style::Equals => argv.push(format!("{name}={value}")),
            Style::Separate => {
                argv.push(name);
                argv.push(value.to_string());
            }
        },
    }
}
