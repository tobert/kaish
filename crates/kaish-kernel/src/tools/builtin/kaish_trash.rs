//! kaish-trash — Manage the freedesktop.org Trash.
//!
//! Subcommands: list, restore, empty, config.

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// KaishTrash tool: manage the system trash.
pub struct KaishTrash;

/// Run a blocking trash operation, flattening the JoinError/trash::Error into a single Result.
async fn trash_op<F, T>(op: F) -> Result<T, String>
where
    F: FnOnce() -> Result<T, trash::Error> + Send + 'static,
    T: Send + 'static,
{
    match tokio::task::spawn_blocking(op).await {
        Ok(Ok(v)) => Ok(v),
        Ok(Err(e)) => Err(e.to_string()),
        Err(e) => Err(e.to_string()),
    }
}

#[async_trait]
impl Tool for KaishTrash {
    fn name(&self) -> &str {
        "kaish-trash"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("kaish-trash", "Manage the freedesktop.org Trash")
            .param(ParamSchema::required(
                "subcommand",
                "string",
                "Subcommand: list, restore, empty, config",
            ))
            .param(ParamSchema::optional(
                "arg",
                "string",
                Value::Null,
                "Argument for the subcommand (filter for list, name for restore, max-size for config)",
            ))
            .param(ParamSchema::optional(
                "confirm",
                "string",
                Value::Null,
                "Confirmation nonce for empty (--confirm=NONCE)",
            ))
            .example("List trashed items", "kaish-trash list")
            .example("List with filter", "kaish-trash list '*.log'")
            .example("Restore a file", "kaish-trash restore myfile.txt")
            .example("Show trash settings", "kaish-trash config")
            .example("Set max size to 50MB", "kaish-trash config max-size 52428800")
            .example("Empty trash", "kaish-trash empty")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let subcmd = match args.get_string("subcommand", 0) {
            Some(s) => s,
            None => return ExecResult::failure(1, "kaish-trash: missing subcommand (list, restore, empty, config)"),
        };

        match subcmd.as_str() {
            "list" => cmd_list(&args, ctx).await,
            "restore" => cmd_restore(&args, ctx).await,
            "empty" => cmd_empty(&args, ctx).await,
            "config" => cmd_config(&args, ctx).await,
            other => ExecResult::failure(1, format!("kaish-trash: unknown subcommand: {}", other)),
        }
    }
}

async fn cmd_list(args: &ToolArgs, _ctx: &mut ExecContext) -> ExecResult {
    let filter = args.get_string("arg", 1);

    let items = match trash_op(trash::os_limited::list).await {
        Ok(items) => items,
        Err(e) => return ExecResult::failure(1, format!("kaish-trash list: {}", e)),
    };

    if items.is_empty() {
        return ExecResult::with_output(OutputData::text("trash is empty"));
    }

    let mut nodes = Vec::new();
    for item in &items {
        let name = item.name.to_string_lossy().to_string();

        // Apply filter if provided
        if let Some(ref f) = filter {
            if !name.contains(f.as_str()) {
                continue;
            }
        }

        let original = item.original_parent.join(&item.name).to_string_lossy().to_string();
        let deleted = format!("{}", item.time_deleted);

        nodes.push(OutputNode::new(&name).with_cells(vec![original, deleted]));
    }

    if nodes.is_empty() {
        return ExecResult::with_output(OutputData::text("no matching items in trash"));
    }

    ExecResult::with_output(OutputData::table(
        vec!["NAME".to_string(), "ORIGINAL_PATH".to_string(), "DELETED".to_string()],
        nodes,
    ))
}

async fn cmd_restore(args: &ToolArgs, _ctx: &mut ExecContext) -> ExecResult {
    let name = match args.get_string("arg", 1) {
        Some(n) => n,
        None => return ExecResult::failure(1, "kaish-trash restore: specify a path/name to restore"),
    };

    let items = match trash_op(trash::os_limited::list).await {
        Ok(items) => items,
        Err(e) => return ExecResult::failure(1, format!("kaish-trash restore: {}", e)),
    };

    // Exact match first, then fall back to substring
    let exact_count = items.iter()
        .filter(|item| item.name.to_string_lossy() == name)
        .count();

    let matches: Vec<_> = if exact_count == 1 {
        items.into_iter()
            .filter(|item| item.name.to_string_lossy() == name)
            .collect()
    } else {
        items.into_iter()
            .filter(|item| item.name.to_string_lossy().contains(name.as_str()))
            .collect()
    };

    if matches.is_empty() {
        return ExecResult::failure(1, format!("kaish-trash restore: '{}' not found in trash", name));
    }

    if matches.len() > 1 {
        let names: Vec<String> = matches.iter().map(|m| m.name.to_string_lossy().to_string()).collect();
        return ExecResult::failure(1, format!(
            "kaish-trash restore: multiple matches for '{}': {}. Be more specific.",
            name,
            names.join(", ")
        ));
    }

    match trash_op(move || trash::os_limited::restore_all(matches)).await {
        Ok(()) => ExecResult::with_output(OutputData::text(format!("restored: {}", name))),
        Err(e) => ExecResult::failure(1, format!("kaish-trash restore: {}", e)),
    }
}

async fn cmd_empty(args: &ToolArgs, ctx: &mut ExecContext) -> ExecResult {
    let confirm = args.get_named("confirm").and_then(|v| match v {
        Value::String(s) => Some(s.clone()),
        _ => None,
    });

    // Empty always requires nonce confirmation (inherently destructive)
    if let Some(nonce) = &confirm {
        match ctx.nonce_store.validate(nonce, "kaish-trash empty", &[]) {
            Ok(()) => {
                // Check if trash is actually empty first
                let items = match trash_op(trash::os_limited::list).await {
                    Ok(items) => items,
                    Err(e) => return ExecResult::failure(1, format!("kaish-trash empty: {}", e)),
                };

                if items.is_empty() {
                    return ExecResult::with_output(OutputData::text("trash is already empty"));
                }

                match trash_op(move || trash::os_limited::purge_all(items)).await {
                    Ok(()) => ExecResult::with_output(OutputData::text("trash emptied")),
                    Err(e) => ExecResult::failure(1, format!("kaish-trash empty: {}", e)),
                }
            }
            Err(e) => ExecResult::failure(1, format!("kaish-trash empty: {}", e)),
        }
    } else {
        // Issue a nonce
        let nonce = ctx.nonce_store.issue("kaish-trash empty", &[]);
        let ttl = ctx.nonce_store.ttl().as_secs();
        ExecResult::failure(2, format!(
            "kaish-trash empty: confirmation required (emptying trash is destructive)\nTo confirm, run: kaish-trash empty --confirm={}\nNonce expires in {} seconds.",
            nonce, ttl
        ))
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
    let latch = ctx.scope.latch_enabled();

    let nodes = vec![
        OutputNode::new("enabled").with_cells(vec![enabled.to_string()]),
        OutputNode::new("max_size").with_cells(vec![format_size(max_size)]),
        OutputNode::new("latch").with_cells(vec![latch.to_string()]),
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
    async fn test_config_shows_defaults() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("config".into()));

        let result = KaishTrash.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("false")); // enabled=false
        assert!(result.out.contains("10.0MB")); // max_size
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
        assert!(result.out.contains("true"));
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
        assert!(result.out.contains("50.0MB"));
        assert_eq!(ctx.scope.trash_max_size(), 52_428_800);
    }

    #[tokio::test]
    async fn test_empty_without_nonce_returns_code_2() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("empty".into()));

        let result = KaishTrash.execute(args, &mut ctx).await;
        assert_eq!(result.code, 2);
        assert!(result.err.contains("confirmation required"));
        assert!(result.err.contains("--confirm="));
    }

    #[tokio::test]
    async fn test_empty_with_valid_nonce_on_empty_trash() {
        let mut ctx = make_ctx();

        let nonce = ctx.nonce_store.issue("kaish-trash empty", &[]);

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("empty".into()));
        args.named.insert("confirm".to_string(), Value::String(nonce));

        let result = KaishTrash.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("already empty"));
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

    #[tokio::test]
    async fn test_list_empty_trash() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("list".into()));

        let result = KaishTrash.execute(args, &mut ctx).await;
        assert!(result.ok());
        // May show "trash is empty" or actual items depending on system state.
        // In CI/test, trash should typically be empty.
    }
}
