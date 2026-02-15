//! alias / unalias — Manage command aliases.

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Alias tool: define, list, or show command aliases.
///
/// - `alias` — list all aliases
/// - `alias name='command args'` — define alias
/// - `alias name` — show one alias
pub struct Alias;

#[async_trait]
impl Tool for Alias {
    fn name(&self) -> &str {
        "alias"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("alias", "Define or display command aliases")
            .param(ParamSchema::optional(
                "args",
                "any",
                Value::Null,
                "Alias definitions (name=value) or names to display",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        if args.positional.is_empty() && args.named.is_empty() {
            // List all aliases
            return list_aliases(ctx);
        }

        // Check named args first (name=value form, parsed by kernel as named args)
        for (name, value) in &args.named {
            let val = match value {
                Value::String(s) => s.clone(),
                other => crate::interpreter::value_to_string(other),
            };
            ctx.aliases.insert(name.clone(), val);
        }

        // Check positional args: either "name=value" strings or bare names to show
        for arg in &args.positional {
            let s = match arg {
                Value::String(s) => s.clone(),
                other => crate::interpreter::value_to_string(other),
            };

            if let Some((name, value)) = s.split_once('=') {
                ctx.aliases.insert(name.to_string(), value.to_string());
            } else {
                // Show one alias
                if let Some(value) = ctx.aliases.get(&s) {
                    let output = format!("alias {}='{}'\n", s, value);
                    return ExecResult::with_output(OutputData::text(output));
                } else {
                    return ExecResult::failure(1, format!("alias: {}: not found", s));
                }
            }
        }

        ExecResult::success("")
    }
}

fn list_aliases(ctx: &ExecContext) -> ExecResult {
    if ctx.aliases.is_empty() {
        return ExecResult::with_output(OutputData::new());
    }

    let headers = vec!["NAME".to_string(), "VALUE".to_string()];

    let mut entries: Vec<_> = ctx.aliases.iter().collect();
    entries.sort_by(|(a, _), (b, _)| a.cmp(b));

    let nodes: Vec<OutputNode> = entries
        .iter()
        .map(|(name, value)| OutputNode::new(name.as_str()).with_cells(vec![value.to_string()]))
        .collect();

    ExecResult::with_output(OutputData::table(headers, nodes))
}

/// Unalias tool: remove command aliases.
pub struct Unalias;

#[async_trait]
impl Tool for Unalias {
    fn name(&self) -> &str {
        "unalias"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("unalias", "Remove command aliases")
            .param(ParamSchema::required("names", "array", "Alias names to remove"))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        if args.positional.is_empty() {
            return ExecResult::failure(1, "unalias: missing alias name");
        }

        for arg in &args.positional {
            let name = match arg {
                Value::String(s) => s.clone(),
                other => crate::interpreter::value_to_string(other),
            };
            ctx.aliases.remove(&name);
        }

        ExecResult::success("")
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
    async fn test_alias_list_empty() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();
        let result = Alias.execute(args, &mut ctx).await;
        assert!(result.ok());
    }

    #[tokio::test]
    async fn test_alias_define_and_list() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("ll=ls -la".into()));
        let result = Alias.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(ctx.aliases.get("ll").unwrap(), "ls -la");
    }

    #[tokio::test]
    async fn test_alias_show_one() {
        let mut ctx = make_ctx();
        ctx.aliases.insert("ll".to_string(), "ls -la".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("ll".into()));
        let result = Alias.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("ls -la"));
    }

    #[tokio::test]
    async fn test_alias_not_found() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("nope".into()));
        let result = Alias.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_unalias() {
        let mut ctx = make_ctx();
        ctx.aliases.insert("ll".to_string(), "ls -la".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("ll".into()));
        let result = Unalias.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(ctx.aliases.is_empty());
    }
}
