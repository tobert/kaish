//! Rendering a parsed call into the child's argv.
//!
//! ```text
//! argv = command.lead…
//!      , verb.name            (omitted for the root verb, and for omit_name)
//!      , verb.lead…           , repeated for every node on the path, then the leaf
//!      , every word the agent wrote, in source order
//! ```
//!
//! A node's name and lead render exactly like a leaf's, in path order: `git
//! worktree list` renders `worktree`, then `list`, with each level's `lead`
//! spliced in right after its own name.
//!
//! The executable itself is not in the rendered argv — the spawn site owns
//! argv[0].
//!
//! **Source order, canonical spelling.** The declaration decides how a word is
//! spelled — `-n 5` renders as `--max-count=5` — and never where it sits.
//! `cargo clippy --message-format json` rendered as
//! `clippy --message-format -- json`: `json` filled no declared slot, so it
//! became a tail behind an inserted `--`, split from the flag whose value it
//! was. A block of declared flags collected ahead of the positionals reordered
//! argv the same way, one step earlier.
//!
//! The wrapper never inserts a `--` the agent did not write. `git log -- main`
//! means "paths named main", not "revision main"; inserting `--` would change
//! the program's meaning.

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
    for step in verb_chain(declaration, &call.verb_path) {
        if !step.omit_name
            && let Some(name) = &step.name
        {
            argv.push(name.clone());
        }
        argv.extend(step.lead.iter().cloned());
    }

    let mut item_argv_index = Vec::with_capacity(call.items.len());
    for item in &call.items {
        item_argv_index.push(argv.len());
        match item {
            Item::Positional { value, .. } => argv.push(value.clone()),
            Item::Flag(use_index) => {
                // Both lookups are in-range for any `Call` the parser built:
                // it pushes the `Item` and the `FlagUse` together, from an
                // index it just found in this verb's flags.
                if let Some(use_) = call.flags.get(*use_index)
                    && let Some(flag) = verb.flags.get(use_.flag_index)
                {
                    render_flag(flag, use_.value.as_deref(), &mut argv);
                }
            }
            Item::DashDash => argv.push("--".to_string()),
            Item::Undeclared(word) => argv.push(word.clone()),
        }
    }

    Rendered {
        argv,
        item_argv_index,
    }
}

/// The verb at every step from the top down to the leaf `path` selects.
/// Empty for the root (`render` then renders nothing but `declaration.lead`
/// before the agent's own words). Every index in `path` was produced by
/// `parse::select_verb`'s own walk over these same lists, so it is always
/// in range.
fn verb_chain<'d>(declaration: &'d WrappedCommand, path: &[usize]) -> Vec<&'d Verb> {
    let Some((&first, rest)) = path.split_first() else {
        return declaration.root.iter().collect();
    };
    let mut chain = Vec::with_capacity(rest.len() + 1);
    let mut verb = &declaration.verbs[first];
    chain.push(verb);
    for &index in rest {
        verb = &verb.verbs[index];
        chain.push(verb);
    }
    chain
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
