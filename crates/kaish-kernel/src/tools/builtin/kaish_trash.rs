//! kaish-trash — Manage the freedesktop.org Trash.
//!
//! Subcommands: list, restore, empty, config.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::trash::TrashBackend;
use crate::operation::KernelOperation;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// KaishTrash tool: manage the system trash.
pub struct KaishTrash;

/// clap-derived argv layer for kaish-trash.
#[derive(Parser, Debug)]
#[command(name = "kaish-trash", about = "Manage the freedesktop.org Trash")]
struct KaishTrashArgs {
    /// Acknowledge that `empty` is irreversible. Takes no value: emptying
    /// the trash discards the recovery net every other operation depends on,
    /// so it always asks.
    #[arg(long = "confirm")]
    confirm: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Subcommand (`list`, `restore`, `empty`, `config`) and its arguments.
    args: Vec<String>,
}

#[async_trait]
impl Tool for KaishTrash {
    fn name(&self) -> &str {
        "kaish-trash"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &KaishTrashArgs::command(),
            "kaish-trash",
            "Manage the freedesktop.org Trash",
            [
                ("List trashed items", "kaish-trash list"),
                ("List with filter", "kaish-trash list '*.log'"),
                ("Restore a file", "kaish-trash restore myfile.txt"),
                ("Show trash settings", "kaish-trash config"),
                ("Set max size to 50MB", "kaish-trash config max-size 52428800"),
                ("Empty trash", "kaish-trash empty"),
            ],
        )
        // Flat tool, one schema for `list`/`restore`/`config`/`empty` alike
        // (no clap subcommand tree — see the module doc); only `empty` gates,
        // so this names what the tool *can* post, not what every invocation
        // reaches.
        .with_operations([KernelOperation::TrashEmpty.as_str()])
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("kaish-trash: {e}")),
        };
        let parsed = match KaishTrashArgs::try_parse_from(
            std::iter::once("kaish-trash".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("kaish-trash: {e}")),
        };
        parsed.global.apply(ctx);

        let subcmd = match args.get_string("subcommand", 0) {
            Some(s) => s,
            None => return ExecResult::failure(1, "kaish-trash: missing subcommand (list, restore, empty, config)"),
        };

        match subcmd.as_str() {
            "list" => cmd_list(&args, ctx).await,
            "restore" => cmd_restore(&args, ctx).await,
            "empty" => cmd_empty(parsed.confirm, ctx).await,
            "config" => cmd_config(&args, ctx).await,
            other => ExecResult::failure(1, format!("kaish-trash: unknown subcommand: {}", other)),
        }
    }
}

/// Get the trash backend from context, or return an error result.
// Same call as `ExecContext::snapshot_overwrites`: the `Err` IS the result the
// caller returns, so boxing it would buy an allocation and cost a deref.
#[allow(clippy::result_large_err)]
fn get_backend<'a>(ctx: &'a ExecContext, subcmd: &str) -> Result<&'a dyn TrashBackend, ExecResult> {
    ctx.trash_backend
        .as_deref()
        .ok_or_else(|| ExecResult::failure(1, format!("kaish-trash {}: trash backend not available", subcmd)))
}

async fn cmd_list(args: &ToolArgs, ctx: &mut ExecContext) -> ExecResult {
    let trash = match get_backend(ctx, "list") {
        Ok(t) => t,
        Err(e) => return e,
    };
    let filter = args.get_string("arg", 1);

    let entries = match trash.list(filter.as_deref()).await {
        Ok(entries) => entries,
        Err(e) => return ExecResult::failure(1, format!("kaish-trash list: {}", e)),
    };

    if entries.is_empty() {
        let msg = if filter.is_some() { "no matching items in trash" } else { "trash is empty" };
        return ExecResult::with_output(OutputData::text(msg));
    }

    let nodes: Vec<OutputNode> = entries
        .iter()
        .map(|entry| {
            let original = entry.original_path.to_string_lossy().to_string();
            let deleted = format!("{}", entry.deleted_at);
            OutputNode::new(&entry.name).with_cells(vec![original, deleted])
        })
        .collect();

    ExecResult::with_output(OutputData::table(
        vec!["NAME".to_string(), "ORIGINAL_PATH".to_string(), "DELETED".to_string()],
        nodes,
    ))
}

async fn cmd_restore(args: &ToolArgs, ctx: &mut ExecContext) -> ExecResult {
    let name = match args.get_string("arg", 1) {
        Some(n) => n,
        None => return ExecResult::failure(1, "kaish-trash restore: specify a path/name to restore"),
    };
    let trash = match get_backend(ctx, "restore") {
        Ok(t) => t,
        Err(e) => return e,
    };

    let matches = match trash.find_by_name(&name).await {
        Ok(m) => m,
        Err(e) => return ExecResult::failure(1, format!("kaish-trash restore: {}", e)),
    };

    match trash.restore(matches).await {
        Ok(()) => ExecResult::with_output(OutputData::text(format!("restored: {}", name))),
        Err(e) => ExecResult::failure(1, format!("kaish-trash restore: {}", e)),
    }
}

async fn cmd_empty(confirmed: bool, ctx: &mut ExecContext) -> ExecResult {
    // Emptying the trash discards the recovery net every other fs.* operation
    // depends on, and it is the one destructive act with nothing behind it —
    // so it always asks. This is a flag check, not a policy: `--confirm`
    // takes no value, nothing is recorded, and no session setting turns it
    // off. An embedder that wants a richer decision reads the plan first.
    if !confirmed {
        return ExecResult::failure(
            2,
            "kaish-trash empty: emptying the trash is irreversible; re-run as \
             `kaish-trash empty --confirm` to proceed",
        );
    }

    let trash = match get_backend(ctx, "empty") {
        Ok(t) => t,
        Err(e) => return e,
    };
    match trash.purge_all().await {
        Ok(0) => ExecResult::with_output(OutputData::text("trash is already empty")),
        Ok(_) => ExecResult::with_output(OutputData::text("trash emptied")),
        Err(e) => ExecResult::failure(1, format!("kaish-trash empty: {}", e)),
    }
}

async fn cmd_config(args: &ToolArgs, ctx: &mut ExecContext) -> ExecResult {
    // Check for "config max-size <bytes>" subcommand
    if let Some(arg) = args.get_string("arg", 1) {
        if arg == "max-size" {
            // Look for the size value in positional[2]
            if let Some(size_str) = args.positional.get(2) {
                let size = match size_str {
                    Value::Int(n) => *n as u64,
                    Value::String(s) => match s.parse::<u64>() {
                        Ok(n) => n,
                        Err(_) => return ExecResult::failure(1, format!("kaish-trash config: invalid size: {}", s)),
                    },
                    _ => return ExecResult::failure(1, "kaish-trash config: max-size requires a numeric value"),
                };
                ctx.scope.set_trash_max_size(size);
                return ExecResult::with_output(OutputData::text(format!(
                    "trash max-size set to {}",
                    format_size(size),
                )));
            }
            return ExecResult::failure(1, "kaish-trash config: max-size requires a value in bytes");
        }
    }

    // Show current config
    let enabled = ctx.scope.trash_enabled();
    let max_size = ctx.scope.trash_max_size();

    let nodes = vec![
        OutputNode::new("enabled").with_cells(vec![enabled.to_string()]),
        OutputNode::new("max_size").with_cells(vec![format_size(max_size)]),
    ];

    ExecResult::with_output(OutputData::table(
        vec!["SETTING".to_string(), "VALUE".to_string()],
        nodes,
    ))
}

/// Format a byte size for human display.
fn format_size(bytes: u64) -> String {
    const MB: u64 = 1024 * 1024;
    const GB: u64 = 1024 * 1024 * 1024;

    if bytes >= GB {
        format!("{:.1}GB", bytes as f64 / GB as f64)
    } else if bytes >= MB {
        format!("{:.1}MB", bytes as f64 / MB as f64)
    } else {
        format!("{}B", bytes)
    }
}

#[cfg(all(test, feature = "os-integration"))]
mod tests {
    use super::*;
    use crate::trash::find_restore_match;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_config_shows_defaults() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("config".into()));

        let result = KaishTrash.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("false")); // enabled=false
        assert!(result.text_out().contains("10.0MB")); // max_size
    }

    #[tokio::test]
    async fn test_config_reflects_changes() {
        let mut ctx = make_ctx();
        ctx.scope.set_trash_enabled(true);

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("config".into()));

        let result = KaishTrash.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should show enabled=true
        assert!(result.text_out().contains("true"));
    }

    #[tokio::test]
    async fn test_config_max_size_update() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("config".into()));
        args.positional.push(Value::String("max-size".into()));
        args.positional.push(Value::Int(52_428_800)); // 50MB

        let result = KaishTrash.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("50.0MB"));
        assert_eq!(ctx.scope.trash_max_size(), 52_428_800);
    }

    #[tokio::test]
    async fn test_restore_no_arg_fails() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("restore".into()));

        let result = KaishTrash.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1);
        assert!(result.err.contains("specify a path/name"));
    }

    #[tokio::test]
    async fn test_unknown_subcommand_fails() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("frobnicate".into()));

        let result = KaishTrash.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1);
        assert!(result.err.contains("unknown subcommand"));
    }

    #[ignore] // calls real OS trash — flaky in CI
    #[tokio::test]
    async fn test_list_empty_trash() {
        let mut ctx = make_ctx();
        ctx.trash_backend = Some(Arc::new(crate::trash_system::SystemTrash));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("list".into()));

        let result = KaishTrash.execute(args, &mut ctx).await;
        assert!(result.ok());
        // May show "trash is empty" or actual items depending on system state.
        // In CI/test, trash should typically be empty.
    }

    // ── find_restore_match tests (pure logic, no OS trash dependency) ──

    #[test]
    fn test_find_restore_match_single_exact() {
        let items = vec![
            ("foo.txt".to_string(), 1),
            ("bar.txt".to_string(), 2),
        ];
        let result = find_restore_match(items, "foo.txt");
        assert_eq!(result.unwrap(), vec![1]);
    }

    #[test]
    fn test_find_restore_match_multiple_exact_uses_all() {
        let items = vec![
            ("foo.txt".to_string(), 1),
            ("foo.txt".to_string(), 2),
            ("bar.txt".to_string(), 3),
        ];
        let result = find_restore_match(items, "foo.txt");
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.contains("multiple matches"));
    }

    #[test]
    fn test_find_restore_match_substring_fallback() {
        let items = vec![
            ("my_foo.txt".to_string(), 1),
            ("bar.txt".to_string(), 2),
        ];
        let result = find_restore_match(items, "foo");
        assert_eq!(result.unwrap(), vec![1]);
    }

    #[test]
    fn test_find_restore_match_no_match() {
        let items: Vec<(String, i32)> = vec![
            ("foo.txt".to_string(), 1),
            ("bar.txt".to_string(), 2),
        ];
        let result = find_restore_match(items, "baz");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("not found"));
    }

    #[test]
    fn test_find_restore_match_multiple_ambiguous() {
        let items = vec![
            ("foo_a.txt".to_string(), 1),
            ("foo_b.txt".to_string(), 2),
        ];
        let result = find_restore_match(items, "foo");
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.contains("multiple matches"));
        assert!(err.contains("foo_a.txt"));
        assert!(err.contains("foo_b.txt"));
    }
}
