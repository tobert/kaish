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
/// - Hidden args (`#[arg(hide = true)]`) — these are sink positionals used to
///   absorb args after `--`; they're an internal clap detail, not part of the
///   tool's public schema.
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
    arg.is_hide_set()
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

    ParamSchema {
        name,
        param_type: param_type.to_string(),
        required,
        default,
        description,
        aliases,
        consumes,
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

        /// Output sink — clap parses but tool ignores.
        #[arg(hide = true)]
        rest: Vec<String>,
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
    fn hidden_args_excluded_from_schema() {
        let cmd = DemoArgs::command();
        let params = params_from_clap(&cmd);
        assert!(params.iter().all(|p| p.name != "rest"));
    }

    #[test]
    fn help_version_json_filtered() {
        let cmd = DemoArgs::command();
        let params = params_from_clap(&cmd);
        assert!(params.iter().all(|p| !matches!(p.name.as_str(), "help" | "version" | "json")));
    }
}
