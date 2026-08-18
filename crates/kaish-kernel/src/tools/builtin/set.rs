//! set — Set shell options (like set -e, set -o trash).

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Set tool: configure shell options.
///
/// Supports:
/// - `-e` / `+e`: Enable/disable error-exit mode (exit on command failure)
/// - `-o trash` / `+o trash`: Enable/disable trash-on-delete for rm
/// - `-o glob` / `+o glob`: Enable/disable bare glob expansion
/// - `-o output-limit[=SIZE]` / `+o output-limit`: Cap or uncap output size
///
/// An unrecognized bare short flag (`set -q`, `set -v`) is silently ignored
/// for bash compatibility — kaish implements one small, enumerable subset of
/// bash's option surface, not the rest. `-o NAME` is different: NAME names
/// one specific thing out of that small set, so a typo or a bash option
/// kaish doesn't have (`pipefail`) fails loudly instead of no-opping — see
/// `apply_set_o`.
pub struct Set;

/// The `-o`/`+o` names kaish implements.
const VALID_SET_O_NAMES: &[&str] = &["glob", "output-limit[=SIZE]", "trash"];

/// Applies one `-o NAME` (`enable = true`) or `+o NAME` (`enable = false`).
///
/// Shared by both parse shapes `set` can end up with for `-o NAME`: the
/// ordinary `positional = ["-o", "NAME"]` shape, and the flags-split shape
/// (`flags = {"o"}, positional = ["NAME"]`) the binder produces when `-o`
/// arrives as the only flag on its token. Keeping one function means the two
/// shapes can't drift into accepting different names.
fn apply_set_o(ctx: &mut ExecContext, name: &str, enable: bool) -> Result<(), String> {
    match name {
        "trash" => ctx.scope.set_trash_enabled(enable),
        "glob" => ctx.scope.set_glob_enabled(enable),
        "output-limit" => {
            if enable {
                if ctx.output_limit.max_bytes().is_none() {
                    ctx.output_limit.set_limit(Some(
                        crate::output_limit::OutputLimitConfig::default_limit(),
                    ));
                }
            } else {
                ctx.output_limit.set_limit(None);
            }
        }
        _ if enable && name.starts_with("output-limit=") => {
            let size_str = &name["output-limit=".len()..];
            let bytes = crate::output_limit::parse_size(size_str)
                .map_err(|e| format!("set: -o output-limit={size_str}: {e}"))?;
            ctx.output_limit.set_limit(Some(bytes));
        }
        "pipefail" => {
            return Err(
                "set: -o pipefail: not implemented — kaish has no pipefail; \
                 see `help limits` for the deliberate omission and its workaround"
                    .to_string(),
            );
        }
        _ => {
            return Err(format!(
                "set: -o {name}: unknown option — valid names are {}",
                VALID_SET_O_NAMES.join(", ")
            ));
        }
    }
    Ok(())
}

/// clap-derived argv layer for set.
///
/// `set` has bespoke argv handling — it reads `-e`/`+e`/`-o NAME` from
/// args.flags and args.positional directly. clap is only used here as a
/// schema sink and to honor the global `--json` flag.
#[derive(Parser, Debug)]
#[command(name = "set", about = "Set shell options")]
struct SetArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Shell option arguments (`-e`, `+e`, `-o NAME`, `+o NAME`).
    options: Vec<String>,
}

#[async_trait]
impl Tool for Set {
    fn name(&self) -> &str {
        "set"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &SetArgs::command(),
            "set",
            "Set shell options",
            [
                ("Exit on error", "set -e"),
                ("Disable exit on error", "set +e"),
                ("Enable trash-on-delete", "set -o trash"),
                ("Disable glob expansion", "set +o glob"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        // set has bespoke argv handling — strip the user-provided -e / -o etc.
        // tokens from the argv before handing to clap, otherwise clap would
        // reject unknown flags. Only `--json` (global) needs to clap-parse.
        let mut clap_argv: Vec<String> = Vec::new();
        if args.flags.contains("json") {
            clap_argv.push("--json".to_string());
        }
        let parsed = match SetArgs::try_parse_from(
            std::iter::once("set".to_string()).chain(clap_argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("set: {e}")),
        };
        parsed.global.apply(ctx);

        // No arguments: show current settings
        if args.positional.is_empty() && args.flags.is_empty() {
            let mut output = String::new();
            if ctx.scope.error_exit_enabled() {
                output.push_str("set -e\n");
            }
            if ctx.scope.trash_enabled() {
                output.push_str("set -o trash\n");
            }
            if !ctx.scope.glob_enabled() {
                output.push_str("set +o glob\n");
            }
            if let Some(bytes) = ctx.output_limit.max_bytes() {
                output.push_str(&format!("set -o output-limit={}\n", format_size_for_set(bytes)));
            }
            return ExecResult::with_output(OutputData::text(output.trim_end()));
        }

        // Process flags (from parser: ShortFlag("e") -> args.flags contains "e")
        for flag in &args.flags {
            match flag.as_str() {
                "e" => ctx.scope.set_error_exit(true),
                "o" => {} // handled below with positional args
                // Deliberately left silent: unlike `-o NAME`, a bare short
                // flag here (`-u`, `-x`, `-v`, a stray "q" from `set -q`,
                // ...) isn't drawn from one small named set we can check
                // against and report — bash has dozens, kaish implements
                // only `-e`/`-o`, and scripts written for bash routinely
                // carry the rest (`set -x` for tracing, `-u`/`-v` and more).
                // Rejecting them would break real scripts that already
                // tolerate kaish ignoring what it doesn't implement, for no
                // gain: there's no fixed list to name in the error the way
                // `apply_set_o` names its valid `-o` set.
                _ => {}
            }
        }

        // Process positional args.
        // From parser: PlusFlag("e") -> String("+e"), String("-o") followed by String("trash"), etc.
        let positionals: Vec<&str> = args
            .positional
            .iter()
            .filter_map(|v| match v {
                Value::String(s) => Some(s.as_str()),
                _ => None,
            })
            .collect();

        let mut i = 0;
        while i < positionals.len() {
            let opt = positionals[i];
            match opt {
                "-e" => ctx.scope.set_error_exit(true),
                "+e" => ctx.scope.set_error_exit(false),
                "-o" => {
                    // Consume next positional as option name
                    if let Some(&name) = positionals.get(i + 1) {
                        if let Err(msg) = apply_set_o(ctx, name, true) {
                            return ExecResult::failure(1, msg);
                        }
                        i += 1; // skip the option name
                    }
                }
                "+o" => {
                    if let Some(&name) = positionals.get(i + 1) {
                        if let Err(msg) = apply_set_o(ctx, name, false) {
                            return ExecResult::failure(1, msg);
                        }
                        i += 1;
                    }
                }
                // Same reasoning as the flags loop above: a bare token here
                // is a bash short flag/word kaish doesn't implement (e.g.
                // "-u" arriving as a positional in some parses), not a `-o`
                // name — no fixed list to check it against.
                _ => {}
            }
            i += 1;
        }

        // Handle case where parser split `-o` into flags and the option name
        // ended up as a bare positional (flags=["o"], positional=["trash"]).
        // Only fire if no "-o" or "+o" appeared in positionals (which would have
        // already consumed the option name above). Only "-o" reaches here as a
        // bare flag: `+o` always arrives as a literal "+o" positional (see the
        // `PlusFlag` handling in parser.rs), so this path never needs to
        // disable — it always calls `apply_set_o` with `enable = true`, same
        // as the ordinary `"-o"` branch above, so the two shapes agree on
        // which names are valid (`apply_set_o` is the single source of truth).
        if args.flags.contains("o")
            && !positionals.iter().any(|p| *p == "-o" || *p == "+o")
        {
            if let Some(&name) = positionals.first() {
                if let Err(msg) = apply_set_o(ctx, name, true) {
                    return ExecResult::failure(1, msg);
                }
            }
        }

        ExecResult::success("")
    }
}

fn format_size_for_set(bytes: usize) -> String {
    if bytes % (1024 * 1024) == 0 {
        format!("{}M", bytes / (1024 * 1024))
    } else if bytes % 1024 == 0 {
        format!("{}K", bytes / 1024)
    } else {
        bytes.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_set_e_enables_error_exit() {
        let mut ctx = make_ctx();
        assert!(!ctx.scope.error_exit_enabled());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-e".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(ctx.scope.error_exit_enabled());
    }

    #[tokio::test]
    async fn test_set_plus_e_disables_error_exit() {
        let mut ctx = make_ctx();
        ctx.scope.set_error_exit(true);
        assert!(ctx.scope.error_exit_enabled());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("+e".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(!ctx.scope.error_exit_enabled());
    }

    #[tokio::test]
    async fn test_set_ignores_unknown_bare_flags() {
        // Bare short flags kaish doesn't implement (-u, -x) are still
        // silently ignored for bash compatibility — see the comment on the
        // flags-loop `_ => {}` arm. `-o pipefail` is different: it now
        // fails loudly (test_set_o_pipefail_fails below), so it's dropped
        // from this case.
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-u".into()));
        args.positional.push(Value::String("-x".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
    }

    #[tokio::test]
    async fn test_set_o_pipefail_fails() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-o".into()));
        args.positional.push(Value::String("pipefail".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("pipefail"));
    }

    #[tokio::test]
    async fn test_set_no_args_shows_settings() {
        let mut ctx = make_ctx();
        ctx.scope.set_error_exit(true);

        let args = ToolArgs::new();
        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("set -e"));
    }

    #[tokio::test]
    async fn test_set_euo_pipefail_fails_on_pipefail() {
        // Common bash idiom: set -euo pipefail. kaish has no pipefail (see
        // limits.md), so this now fails loudly instead of silently
        // no-opping — but -e, processed earlier in the same positional
        // loop, has already taken effect by the time -o pipefail errors.
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-e".into()));
        args.positional.push(Value::String("-u".into()));
        args.positional.push(Value::String("-o".into()));
        args.positional.push(Value::String("pipefail".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("pipefail"));
        assert!(ctx.scope.error_exit_enabled());
    }

    #[tokio::test]
    async fn the_flag_split_parse_path_still_enables() {
        // When the parser produces `flags=["o"] positional=["trash"]` the
        // option never reaches the `-o` branch, so the fallback below it has
        // to catch the name. Pinned to a surviving option after the approval
        // policy it originally covered was removed — the parse quirk is the
        // thing under test, not the option.
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.flags.insert("o".to_string());
        args.positional.push(Value::String("trash".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(ctx.scope.trash_enabled());
    }

    #[tokio::test]
    async fn test_set_o_trash_enables() {
        let mut ctx = make_ctx();
        assert!(!ctx.scope.trash_enabled());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-o".into()));
        args.positional.push(Value::String("trash".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(ctx.scope.trash_enabled());
    }

    #[tokio::test]
    async fn test_set_plus_o_trash_disables() {
        let mut ctx = make_ctx();
        ctx.scope.set_trash_enabled(true);

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("+o".into()));
        args.positional.push(Value::String("trash".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(!ctx.scope.trash_enabled());
    }

    #[tokio::test]
    async fn test_set_no_args_shows_all_options() {
        let mut ctx = make_ctx();
        ctx.scope.set_trash_enabled(true);

        let args = ToolArgs::new();
        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("set -o trash"));
    }

    #[tokio::test]
    async fn test_set_o_unknown_name_fails() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-o".into()));
        args.positional.push(Value::String("bogusname".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("bogusname"));
        assert!(
            result.err.contains("glob") && result.err.contains("trash") && result.err.contains("output-limit"),
            "error should name the valid set: {:?}",
            result.err
        );
    }

    #[tokio::test]
    async fn test_set_o_output_limit_enables_default() {
        let mut ctx = make_ctx();
        assert!(!ctx.output_limit.is_enabled());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-o".into()));
        args.positional.push(Value::String("output-limit".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(ctx.output_limit.is_enabled());
        assert_eq!(ctx.output_limit.max_bytes(), Some(crate::output_limit::OutputLimitConfig::default_limit()));
    }

    #[tokio::test]
    async fn test_set_o_output_limit_with_size() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-o".into()));
        args.positional.push(Value::String("output-limit=16K".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(ctx.output_limit.max_bytes(), Some(16 * 1024));
    }

    #[tokio::test]
    async fn test_set_plus_o_output_limit_disables() {
        let mut ctx = make_ctx();
        ctx.output_limit.set_limit(Some(8 * 1024));
        assert!(ctx.output_limit.is_enabled());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("+o".into()));
        args.positional.push(Value::String("output-limit".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(!ctx.output_limit.is_enabled());
    }

    #[tokio::test]
    async fn test_set_no_args_shows_output_limit() {
        let mut ctx = make_ctx();
        ctx.output_limit.set_limit(Some(4 * 1024));

        let args = ToolArgs::new();
        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("set -o output-limit=4K"));
    }

    #[tokio::test]
    async fn test_set_no_args_hides_output_limit_when_disabled() {
        let mut ctx = make_ctx();
        // output_limit disabled by default in test ctx

        let args = ToolArgs::new();
        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(!result.text_out().contains("output-limit"));
    }

    #[test]
    fn test_format_size_for_set() {
        assert_eq!(format_size_for_set(1024), "1K");
        assert_eq!(format_size_for_set(8 * 1024), "8K");
        assert_eq!(format_size_for_set(1024 * 1024), "1M");
        assert_eq!(format_size_for_set(512), "512");
    }
}
