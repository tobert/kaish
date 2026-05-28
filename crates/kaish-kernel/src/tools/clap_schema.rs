//! Bridge from `clap::Command` reflection → `ToolSchema`.
//!
//! Each migrated builtin's `schema()` delegates to [`params_from_clap`] so the
//! clap-derived struct in `execute()` is the single source of truth for the
//! argv surface. Description and examples remain hand-written — clap doesn't
//! own those concepts.
//!
//! See `docs/clap-migration.md` for the full recipe.

use clap::{Arg, ArgAction, Command};

use crate::ast::Value;
use crate::tools::{ParamSchema, ToolSchema};

/// Build a `ToolSchema` for a builtin from its clap [`Command`] reflection plus
/// hand-written description and examples.
///
/// Bool flags are recognised via [`ArgAction::SetTrue`] / [`ArgAction::SetFalse`].
/// Aliases include the short flag (as a single-char string) and all visible
/// long aliases. `--json` is **excluded** — it's the global output flag and
/// handled by the kernel after `apply_output_format`, not declared as a per-tool
/// param.
pub fn schema_from_clap(
    cmd: &Command,
    name: &str,
    description: &str,
    examples: impl IntoIterator<Item = (&'static str, &'static str)>,
) -> ToolSchema {
    let mut schema = ToolSchema::new(name, description);
    for param in params_from_clap(cmd) {
        schema = schema.param(param);
    }
    for (desc, code) in examples {
        schema = schema.example(desc, code);
    }
    schema
}

/// Reflect each [`Arg`] in a clap [`Command`] into a [`ParamSchema`].
///
/// Skips:
/// - `--help` / `-h` (auto-injected by clap)
/// - `--version` / `-V` (auto-injected by clap when version is set)
/// - `--json` (kernel-owned global flag)
/// - Hidden *flag* args (`#[arg(hide = true)]` without an index) — true
///   internal helpers.
///
/// Hidden *positional* args are kept: many migrated builtins use
/// `#[arg(hide = true)] paths: Vec<String>` purely so clap accepts the
/// `--`-terminated positional tail emitted by `ToolArgs::to_argv()`. Those
/// positionals ARE the tool's public surface (`cat paths…`, `mkdir paths…`),
/// so they belong in the schema with a `positional: true` marker and ride
/// the field's doc-comment for the description.
pub fn params_from_clap(cmd: &Command) -> Vec<ParamSchema> {
    cmd.get_arguments()
        .filter(|arg| !is_skipped(arg))
        .map(arg_to_param)
        .collect()
}

fn is_skipped(arg: &Arg) -> bool {
    let id = arg.get_id().as_str();
    if matches!(id, "help" | "version" | "json") {
        return true;
    }
    // Keep hidden positionals (real user surface, just marked hidden to keep
    // clap's --help tidy); drop hidden flag args (internal clap helpers).
    // Use `is_positional()` — `get_index()` returns None for derived
    // `Vec<String>` positionals even though they ARE positional.
    arg.is_hide_set() && !arg.is_positional()
}

fn arg_to_param(arg: &Arg) -> ParamSchema {
    let name = arg.get_id().as_str().to_string();
    let action = arg.get_action();
    let is_bool = matches!(action, ArgAction::SetTrue | ArgAction::SetFalse);

    let param_type = if is_bool {
        "bool"
    } else if matches!(action, ArgAction::Count) {
        "int"
    } else {
        // clap erases the field type at runtime — we report "string" for all
        // value-taking flags. The kernel's type check is a hint, not a gate.
        "string"
    };

    let description = arg
        .get_help()
        .map(|s| s.to_string())
        .or_else(|| arg.get_long_help().map(|s| s.to_string()))
        .unwrap_or_default();

    let required = arg.is_required_set();

    let mut aliases: Vec<String> = Vec::new();
    if let Some(short) = arg.get_short() {
        aliases.push(short.to_string());
    }
    // Expose the long-flag form whenever it differs from the canonical id
    // (Rust field name, which is always snake_case). Without this, the
    // validator and kernel-side `param_lookup` only know the snake form and
    // would mis-classify a kebab `--ignore-case` as an unknown bool flag.
    if let Some(long) = arg.get_long()
        && long != arg.get_id().as_str()
    {
        aliases.push(long.to_string());
    }
    if let Some(visible) = arg.get_visible_aliases() {
        for alias in visible {
            aliases.push(alias.to_string());
        }
    }

    let consumes = match arg.get_num_args() {
        Some(range) => {
            let lo = range.min_values();
            if lo == 0 { 1 } else { lo }
        }
        None => 1,
    };

    let default = if is_bool {
        Some(Value::Bool(false))
    } else {
        None
    };

    // `arg.is_positional()` is the right oracle: clap returns true for any
    // positional arg, whereas `get_index()` returns None for derived
    // `Vec<String>` positionals. Positional slot ordering is by appearance
    // in `schema.params`, not by clap's internal index.
    let positional = arg.is_positional();

    ParamSchema {
        name,
        param_type: param_type.to_string(),
        required,
        default,
        description,
        aliases,
        consumes,
        positional,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap::{CommandFactory, Parser};

    #[derive(Parser, Debug)]
    #[command(name = "demo", about = "demo tool")]
    struct DemoArgs {
        /// Number output lines.
        #[arg(short = 'n', long = "number")]
        number: bool,

        /// Number of lines.
        #[arg(short = 'l', long = "lines", default_value_t = 10)]
        lines: i64,

        /// Files to read.
        #[arg(hide = true)]
        paths: Vec<String>,
    }

    /// A demo struct with a hidden internal flag (not positional) — those
    /// should still be skipped from the schema.
    #[derive(Parser, Debug)]
    #[command(name = "demo-internal", about = "demo with internal flag")]
    struct DemoInternalArgs {
        #[arg(hide = true, long = "internal-only")]
        internal: bool,

        /// Files to read.
        paths: Vec<String>,
    }

    #[test]
    fn bool_flag_becomes_bool_param() {
        let cmd = DemoArgs::command();
        let params = params_from_clap(&cmd);
        let p = params.iter().find(|p| p.name == "number").expect("number param");
        assert_eq!(p.param_type, "bool");
        assert!(!p.required);
        assert_eq!(p.aliases, vec!["n".to_string()]);
        assert!(p.description.contains("Number output lines"));
    }

    #[test]
    fn value_flag_reports_short_alias_and_string_type() {
        let cmd = DemoArgs::command();
        let params = params_from_clap(&cmd);
        let p = params.iter().find(|p| p.name == "lines").expect("lines param");
        assert_eq!(p.param_type, "string");
        assert!(!p.required);
        assert_eq!(p.aliases, vec!["l".to_string()]);
    }

    #[test]
    fn hidden_positional_is_kept_and_marked_positional() {
        let cmd = DemoArgs::command();
        let params = params_from_clap(&cmd);
        let p = params.iter().find(|p| p.name == "paths").expect("paths param");
        assert!(p.positional, "hidden positional sink should be exposed as positional");
        assert_eq!(p.param_type, "string");
        assert!(p.description.contains("Files to read"));
    }

    #[test]
    fn hidden_flag_is_dropped() {
        let cmd = DemoInternalArgs::command();
        let params = params_from_clap(&cmd);
        assert!(
            params.iter().all(|p| p.name != "internal"),
            "hidden non-positional flag should be skipped: {:?}",
            params.iter().map(|p| &p.name).collect::<Vec<_>>()
        );
        // Non-hidden positional still appears.
        assert!(params.iter().any(|p| p.name == "paths" && p.positional));
    }

    /// A field named `_foo` (Rust convention for unused) used to leak its
    /// leading underscore into the schema `id`. The kernel's flag
    /// canonicalization would then route `--foo VALUE` through as
    /// `--_foo=VALUE`, which clap rejects. Callers must override with
    /// `#[arg(id = "foo", ...)]` to keep the schema name clean. This test
    /// pins both pre-fix and post-fix behavior on a small sample so the
    /// convention stays load-bearing.
    #[derive(Parser, Debug)]
    #[command(name = "demo-id-override")]
    struct DemoIdOverrideArgs {
        /// Without an id override, the leading `_` leaks through.
        #[arg(short = 'b', long = "bare")]
        _bare: Option<String>,

        /// With `id = "..."`, the schema name is clean.
        #[arg(id = "clean", short = 'c', long = "clean")]
        _clean: Option<String>,
    }

    #[test]
    fn id_override_strips_leading_underscore_from_schema_name() {
        let cmd = DemoIdOverrideArgs::command();
        let params = params_from_clap(&cmd);

        // Without override: schema name keeps the `_` prefix — this is the
        // regression to guard against. If we ever auto-strip, flip this
        // assertion + update the builtins to drop the explicit `id =`.
        let bare = params.iter().find(|p| p.name == "_bare")
            .expect("_bare still leaks without an id override");
        assert_eq!(bare.aliases, vec!["b".to_string(), "bare".to_string()]);

        // With override: schema name is the override.
        let clean = params.iter().find(|p| p.name == "clean")
            .expect("id override should set the schema name to `clean`");
        assert_eq!(clean.aliases, vec!["c".to_string()]);
        // No `clean` alias because the long matches the id now.
    }

    #[test]
    fn flag_params_are_not_marked_positional() {
        let cmd = DemoArgs::command();
        let params = params_from_clap(&cmd);
        let p = params.iter().find(|p| p.name == "number").unwrap();
        assert!(!p.positional);
        let p = params.iter().find(|p| p.name == "lines").unwrap();
        assert!(!p.positional);
    }

    #[test]
    fn help_version_json_filtered() {
        let cmd = DemoArgs::command();
        let params = params_from_clap(&cmd);
        assert!(params.iter().all(|p| !matches!(p.name.as_str(), "help" | "version" | "json")));
    }
}
