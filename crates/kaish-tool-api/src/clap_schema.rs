//! Bridge from `clap::Command` reflection → `ToolSchema`.
//!
//! Each migrated builtin's `schema()` delegates to [`params_from_clap`] so the
//! clap-derived struct in `execute()` is the single source of truth for the
//! argv surface. Description and examples remain hand-written — clap doesn't
//! own those concepts.
//!
//! See `docs/clap-migration.md` for the full recipe.

use clap::{Arg, ArgAction, Command};

use kaish_types::{ParamSchema, ToolSchema, Value};

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

/// Build a recursive [`ToolSchema`] from a composed clap [`Command`] tree.
///
/// Like [`schema_from_clap`] for the top level, but also descends into
/// `cmd.get_subcommands()`, so a subcommand-aware tool (`kj context list …`)
/// reflects as a tree the kernel can walk with `select_leaf` to bind flags
/// against the active leaf.
///
/// Each child's `name`/`description` come from the clap subcommand itself
/// (`get_name`/`get_about`) and its command-level aliases from
/// `get_all_aliases()`. Examples belong to the top level only — clap doesn't
/// model per-subcommand usage examples. Flat tools (no `get_subcommands()`)
/// produce a schema with empty `subcommands`, identical to [`schema_from_clap`].
pub fn schema_tree_from_clap(
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
    for sub in cmd.get_subcommands() {
        schema = schema.subcommand(child_schema_from_clap(sub));
    }
    schema
}

/// Reflect a clap subcommand (and its descendants) into a child [`ToolSchema`].
///
/// Name and description are taken from the clap command; command-level aliases
/// from `get_all_aliases()` (visible *and* hidden, so every routable name is
/// known to `select_leaf`).
fn child_schema_from_clap(cmd: &Command) -> ToolSchema {
    let name = cmd.get_name().to_string();
    let description = cmd
        .get_about()
        .map(|s| s.to_string())
        .unwrap_or_default();
    let mut schema = ToolSchema::new(name, description);
    for param in params_from_clap(cmd) {
        schema = schema.param(param);
    }
    let aliases: Vec<String> = cmd.get_all_aliases().map(|s| s.to_string()).collect();
    if !aliases.is_empty() {
        schema = schema.with_command_aliases(aliases);
    }
    for sub in cmd.get_subcommands() {
        schema = schema.subcommand(child_schema_from_clap(sub));
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
    let id = arg.get_id().as_str();
    // Canonical name: prefer the long flag (the user-facing, kebab-cased form).
    // Every tool that consumes a bound `ToolArgs` reconstructs argv via
    // `ToolArgs::to_argv()` and re-parses it with clap (kj, cat, tail, …). That
    // reconstruction emits `--<name>`, so the param name MUST equal the clap
    // long, or the rebuilt flag is unknown to clap: a snake field id like
    // `system_prompt` would render as `--system_prompt`, but clap only accepts
    // the derived long `--system-prompt`. Positionals have no long and keep the
    // field id. (Previously the id was canonical and the long an alias, which
    // worked only because no builtin had a field-name ≠ long-name flag.)
    let name = arg.get_long().unwrap_or(id).to_string();
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
    // Keep the snake-case field id reachable as an alias when it differs from
    // the canonical long, so a consumer (or script) that addresses the arg by
    // its Rust field name still matches. The long is now the canonical `name`.
    if id != name {
        aliases.push(id.to_string());
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

    ParamSchema::new(name, param_type.to_string())
        .with_required(required)
        .with_default(default)
        .with_description(description)
        .with_aliases(aliases)
        .consumes(consumes)
        .with_positional(positional)
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

        // The canonical name is the LONG flag (`bare`), NOT the snake field id
        // (`_bare`). Tools reconstruct argv as `--<name>` and re-parse with clap,
        // so the name must equal the clap long. The field id rides as an alias so
        // it stays addressable. (Previously the id was canonical and the leading
        // `_` leaked into the schema name — that's the bug this now guards.)
        let bare = params.iter().find(|p| p.name == "bare")
            .expect("name should be the long flag `bare`, not the field id `_bare`");
        assert_eq!(bare.aliases, vec!["b".to_string(), "_bare".to_string()]);
        assert!(
            !params.iter().any(|p| p.name == "_bare"),
            "the snake field id must not be the canonical name"
        );

        // With an explicit `id = "clean"` matching the long: name is `clean` and
        // there's no redundant id alias (id == long == name).
        let clean = params.iter().find(|p| p.name == "clean")
            .expect("name should be the long flag `clean`");
        assert_eq!(clean.aliases, vec!["c".to_string()]);
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

    /// A composed two-level command tree reflects into a recursive schema:
    /// child names, command aliases, and leaf params all land on the right node.
    #[test]
    fn schema_tree_reflects_subcommands_and_aliases() {
        // Build a `kj`-shaped tree by hand: kj → context (alias ctx) → list.
        let list = Command::new("list").about("list contexts").visible_alias("ls");
        let context = Command::new("context")
            .about("context ops")
            .visible_alias("ctx")
            .arg(Arg::new("type").long("type").short('t').action(ArgAction::Set))
            .subcommand(list);
        let kj = Command::new("kj").about("kaijutsu").subcommand(context);

        let schema = schema_tree_from_clap(&kj, "kj", "kaijutsu", []);

        assert_eq!(schema.subcommands.len(), 1, "kj should have one child");
        let context = &schema.subcommands[0];
        assert!(context.matches_command("context"));
        assert!(context.matches_command("ctx"), "command alias should route");
        // The `--type`/`-t` value flag lives on the context leaf, not root.
        let type_param = context.params.iter().find(|p| p.name == "type").expect("type on context");
        assert_eq!(type_param.param_type, "string");
        assert_eq!(type_param.aliases, vec!["t".to_string()]);
        assert!(schema.params.iter().all(|p| p.name != "type"), "leaf flag must not leak to root");

        assert_eq!(context.subcommands.len(), 1);
        let list = &context.subcommands[0];
        assert!(list.matches_command("list"));
        assert!(list.matches_command("ls"));
    }

    /// A flat command (no subcommands) reflects with empty `subcommands`,
    /// identical to `schema_from_clap`.
    #[test]
    fn schema_tree_of_flat_command_has_no_subcommands() {
        let cmd = DemoArgs::command();
        let schema = schema_tree_from_clap(&cmd, "demo", "demo tool", []);
        assert!(schema.subcommands.is_empty());
        assert!(schema.aliases.is_empty());
        // Same params as the flat reflection.
        assert_eq!(schema.params.len(), params_from_clap(&cmd).len());
    }
}
