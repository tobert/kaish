//! ignore — Inspect and modify the ignore file configuration.

use async_trait::async_trait;

use crate::ignore_config::IgnoreScope;
use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{ExecContext, Tool, ToolArgs, ToolSchema};

/// Ignore tool: inspect and modify ignore file configuration.
pub struct KaishIgnore;

#[async_trait]
impl Tool for KaishIgnore {
    fn name(&self) -> &str {
        "kaish-ignore"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("kaish-ignore", "Inspect or modify ignore file configuration")
            .example("Show current config", "kaish-ignore")
            .example("Add a file", "kaish-ignore add .dockerignore")
            .example("Remove a file", "kaish-ignore remove .gitignore")
            .example("Clear all filtering", "kaish-ignore clear")
            .example("Toggle defaults", "kaish-ignore defaults on")
            .example("Toggle auto-gitignore", "kaish-ignore auto on")
            .example("Change scope", "kaish-ignore scope enforced")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let subcommand = args.get_string("", 0);

        match subcommand.as_deref() {
            None | Some("") => show_config(ctx),
            Some("add") => {
                let file = match args.get_string("", 1) {
                    Some(f) => f,
                    None => return ExecResult::failure(1, "kaish-ignore add: missing filename"),
                };
                ctx.ignore_config.add_file(&file);
                show_config(ctx)
            }
            Some("remove") => {
                let file = match args.get_string("", 1) {
                    Some(f) => f,
                    None => return ExecResult::failure(1, "kaish-ignore remove: missing filename"),
                };
                ctx.ignore_config.remove_file(&file);
                show_config(ctx)
            }
            Some("clear") => {
                ctx.ignore_config.clear();
                show_config(ctx)
            }
            Some("defaults") => {
                let on = match args.get_string("", 1).as_deref() {
                    Some("on") | Some("true") | Some("1") => true,
                    Some("off") | Some("false") | Some("0") => false,
                    _ => return ExecResult::failure(1, "kaish-ignore defaults: expected 'on' or 'off'"),
                };
                ctx.ignore_config.set_defaults(on);
                show_config(ctx)
            }
            Some("auto") => {
                let on = match args.get_string("", 1).as_deref() {
                    Some("on") | Some("true") | Some("1") => true,
                    Some("off") | Some("false") | Some("0") => false,
                    _ => return ExecResult::failure(1, "kaish-ignore auto: expected 'on' or 'off'"),
                };
                ctx.ignore_config.set_auto_gitignore(on);
                show_config(ctx)
            }
            Some("scope") => {
                let scope = match args.get_string("", 1).as_deref() {
                    Some("advisory") => IgnoreScope::Advisory,
                    Some("enforced") => IgnoreScope::Enforced,
                    _ => return ExecResult::failure(1, "kaish-ignore scope: expected 'advisory' or 'enforced'"),
                };
                ctx.ignore_config.set_scope(scope);
                show_config(ctx)
            }
            Some(other) => ExecResult::failure(1, format!(
                "kaish-ignore: unknown subcommand '{}' (try: add, remove, clear, defaults, auto, scope)",
                other
            )),
        }
    }
}

fn show_config(ctx: &ExecContext) -> ExecResult {
    let cfg = &ctx.ignore_config;
    let scope_str = match cfg.scope() {
        IgnoreScope::Advisory => "advisory",
        IgnoreScope::Enforced => "enforced",
    };
    let files_str = if cfg.files().is_empty() {
        "(none)".to_string()
    } else {
        cfg.files().join(", ")
    };

    let headers = vec!["KEY".to_string(), "VALUE".to_string()];
    let rows = vec![
        OutputNode::new("scope").with_cells(vec![scope_str.to_string()]),
        OutputNode::new("files").with_cells(vec![files_str]),
        OutputNode::new("defaults").with_cells(vec![on_off(cfg.use_defaults())]),
        OutputNode::new("auto-gitignore").with_cells(vec![on_off(cfg.auto_gitignore())]),
        OutputNode::new("active").with_cells(vec![on_off(cfg.is_active())]),
    ];

    ExecResult::with_output(OutputData::table(headers, rows))
}

fn on_off(v: bool) -> String {
    if v { "on".to_string() } else { "off".to_string() }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_ignore_show_default() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();
        let result = KaishIgnore.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("advisory"));
        assert!(result.out.contains("off"));
    }

    #[tokio::test]
    async fn test_ignore_add_remove() {
        let mut ctx = make_ctx();

        // Add a file
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("add".into()));
        args.positional.push(Value::String(".dockerignore".into()));
        let result = KaishIgnore.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains(".dockerignore"));
        assert!(ctx.ignore_config.files().contains(&".dockerignore".to_string()));

        // Remove it
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("remove".into()));
        args.positional.push(Value::String(".dockerignore".into()));
        let result = KaishIgnore.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(ctx.ignore_config.files().is_empty());
    }

    #[tokio::test]
    async fn test_ignore_clear() {
        let mut ctx = make_ctx();
        ctx.ignore_config = crate::ignore_config::IgnoreConfig::mcp();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("clear".into()));
        let result = KaishIgnore.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(!ctx.ignore_config.is_active());
    }

    #[tokio::test]
    async fn test_ignore_scope() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("scope".into()));
        args.positional.push(Value::String("enforced".into()));
        let result = KaishIgnore.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(ctx.ignore_config.scope(), IgnoreScope::Enforced);
    }

    #[tokio::test]
    async fn test_ignore_defaults_toggle() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("defaults".into()));
        args.positional.push(Value::String("on".into()));
        let result = KaishIgnore.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(ctx.ignore_config.use_defaults());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("defaults".into()));
        args.positional.push(Value::String("off".into()));
        let result = KaishIgnore.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(!ctx.ignore_config.use_defaults());
    }

    #[tokio::test]
    async fn test_ignore_auto_toggle() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("auto".into()));
        args.positional.push(Value::String("on".into()));
        let result = KaishIgnore.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(ctx.ignore_config.auto_gitignore());
    }

    #[tokio::test]
    async fn test_ignore_unknown_subcommand() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("bogus".into()));
        let result = KaishIgnore.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("unknown subcommand"));
    }
}
