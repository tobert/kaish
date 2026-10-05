//! Tool system for kaish.
//!
//! Tools are the primary way to perform actions in kaish. Every command
//! is a tool — builtins and user-defined tools all implement
//! the same `Tool` trait.
//!
//! # Architecture
//!
//! ```text
//! ToolRegistry
//! ├── Builtins (echo, ls, cat, ...)
//! └── User Tools (defined via `tool` statements)
//! ```

mod builtin;
mod clap_schema;
mod context;
pub(crate) use context::StdinState;
mod global_flags;
mod registry;
mod traits;
// Wrapped commands run external programs, so the module only exists on the
// `subprocess` axis; a sandbox build has no `wrapped` module at all.
#[cfg(feature = "subprocess")]
pub mod wrapped;

pub use builtin::register_builtins;
#[cfg(feature = "subprocess")]
pub use builtin::{resolve_in_path, virtual_cwd_error};
pub use clap_schema::{params_from_clap, schema_from_clap, schema_tree_from_clap};
pub use context::{
    external_commands_unavailable_error, ExecContext, ExternalCommandsUnavailable,
    GateExpectations, OutputContext, OverwriteExpectation, ScanOutcome, DEFAULT_KILL_GRACE,
};
pub(crate) use context::{cas_overwrite, cas_replace, exec_context, read_for_replace, is_trash_excluded, note_skipped_mounts, ExternalCommandOutcome};
pub use global_flags::GlobalFlags;
pub use registry::ToolRegistry;
pub use traits::{ArgBinding, global_flag_value_is_truthy, is_global_output_flag, validate_against_schema, RefusedFlag, Tool, ToolArgs, ToolCtx, ToolSchema, ParamSchema};

/// Commands that consume bareword `key=value` argv (Arg::WordAssign) as
/// shell-assignment pairs and route them through `tool_args.named`. For every
/// other command, `key=value` lands as a positional `"key=value"` string —
/// matching bash (`cat foo=bar` opens a file named `foo=bar`).
///
/// Add to this list only for builtins that have a documented shell-assignment
/// argv contract (`export FOO=bar`, `alias greet='echo hi'`). Long-flag
/// `--key=value` is a separate AST node (`Arg::Named`) and routes through
/// `tool_args.named` regardless — except past `--`, where both spellings
/// become literal positionals.
pub const WORD_ASSIGN_BUILTINS: &[&str] = &["export", "alias", "unalias"];

pub fn accepts_word_assign(name: &str) -> bool {
    WORD_ASSIGN_BUILTINS.contains(&name)
}

/// Recognize generic help without consuming a tool-owned flag or an option value.
pub(crate) fn requests_builtin_help(args: &ToolArgs, schema: &ToolSchema) -> bool {
    if schema.owns_output {
        return false;
    }
    let claims = |flag: &str| schema.params.iter().any(|parameter| {
        !parameter.positional && (parameter.name == flag
            || parameter.aliases.iter().any(|alias| alias.trim_start_matches('-') == flag))
    });
    if (args.flags.contains("help") && !claims("help"))
        || (args.flags.contains("h") && !claims("h"))
    {
        return true;
    }
    let is_help = |value: &crate::ast::Value| {
        matches!(value, crate::ast::Value::String(word) if word == "--help")
    };
    if schema.raw_argv {
        return !claims("help") && args.positional.first().is_some_and(is_help);
    }
    if !matches!(schema.arg_binding, ArgBinding::Verbatim) || claims("help") {
        return false;
    }
    let Some(words) = args.words.as_deref() else { return false; };
    let mut state = VerbatimArgumentState::default();
    for value in words {
        // Help keeps the end-marker rule even when the tool consumes that word.
        if matches!(value, crate::ast::Value::String(word) if word == "--") { return false; }
        if !state.expects_value() && !state.past_end_marker() && is_help(value) {
            return true;
        }
        state.consume(value, schema);
    }
    false
}

/// Track schema-declared value words before interpreting generic flags.
#[derive(Default)]
pub(crate) struct VerbatimArgumentState {
    remaining_values: usize,
    past_end_marker: bool,
}

impl VerbatimArgumentState {
    pub(crate) fn expects_value(&self) -> bool { self.remaining_values > 0 }
    pub(crate) fn past_end_marker(&self) -> bool { self.past_end_marker }
    pub(crate) fn mark_end_marker(&mut self) { self.past_end_marker = true; }

    pub(crate) fn consume(&mut self, value: &crate::ast::Value, schema: &ToolSchema) {
        if self.remaining_values > 0 {
            self.remaining_values -= 1;
            return;
        }
        if self.past_end_marker { return; }
        let crate::ast::Value::String(word) = value else { return; };
        if word == "--" {
            self.past_end_marker = true;
            return;
        }
        let (flag, attached) = match word.split_once('=') {
            Some((flag, _)) => (flag, true),
            None => (word.as_str(), false),
        };
        if !flag.starts_with('-') { return; }
        let bare = flag.trim_start_matches('-');
        if let Some(parameter) = schema.params.iter().find(|parameter| {
            !parameter.positional && (parameter.name == bare
                || parameter.aliases.iter().any(|alias| alias.trim_start_matches('-') == bare))
        }) && !crate::scheduler::is_bool_type(&parameter.param_type) {
            self.remaining_values = parameter.consumes.saturating_sub(usize::from(attached));
        }
    }
}
