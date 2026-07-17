//! alias / unalias — Manage command aliases.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Alias tool: define, list, or show command aliases.
///
/// - `alias` — list all aliases
/// - `alias name='command args'` — define alias
/// - `alias name` — show one alias
pub struct Alias;

/// clap-derived argv layer for alias.
#[derive(Parser, Debug)]
#[command(name = "alias", about = "Define or display command aliases")]
struct AliasArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Alias name to display, or `name=value` to define a new alias.
    args: Vec<String>,
}

#[async_trait]
impl Tool for Alias {
    fn name(&self) -> &str {
        "alias"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &AliasArgs::command(),
            "alias",
            "Define or display command aliases",
            [
                ("List all aliases", "alias"),
                ("Define an alias", "alias ll='ls -la'"),
                ("Show one alias", "alias ll"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        // For alias, args.named carries user-defined name=value pairs that
        // clap can't know about. Synthesise an argv with just flags so clap
        // sees only the global --json; we read name=value off args.named below.
        let mut clap_argv: Vec<String> = Vec::new();
        let mut sorted_flags: Vec<&String> = args.flags.iter().collect();
        sorted_flags.sort();
        for flag in sorted_flags {
            clap_argv.push(if flag.chars().count() == 1 {
                format!("-{flag}")
            } else {
                format!("--{flag}")
            });
        }
        let parsed = match AliasArgs::try_parse_from(
            std::iter::once("alias".to_string()).chain(clap_argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("alias: {e}")),
        };
        parsed.global.apply(ctx);


        if args.positional.is_empty() && args.named.is_empty() {
            // List all aliases
            return list_aliases(ctx);
        }

        // Check named args first (name=value form, parsed by kernel as named args)
        for (name, value) in &args.named {
            // Loud on binary (GH #116): `alias name=$BIN` must not silently
            // define an alias body of `[binary: N bytes]`.
            let val = match value {
                Value::String(s) => s.clone(),
                other => match crate::interpreter::value_to_text_sink_named(other, "an alias value") {
                    Ok(s) => s,
                    Err(e) => return ExecResult::failure(1, format!("alias: {e}")),
                },
            };
            ctx.aliases.insert(name.clone(), val);
        }

        // Check positional args: either "name=value" strings or bare names to show
        for arg in &args.positional {
            // Loud on binary (GH #116): `alias $BIN` must not silently treat the
            // placeholder as an alias name/definition.
            let s = match arg {
                Value::String(s) => s.clone(),
                other => match crate::interpreter::value_to_text_sink_named(
                    other,
                    "an alias name or definition",
                ) {
                    Ok(s) => s,
                    Err(e) => return ExecResult::failure(1, format!("alias: {e}")),
                },
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
    // sort_by_key would force cloning the borrowed &String key; the comparator
    // form sorts by reference with no allocation.
    #[allow(clippy::unnecessary_sort_by)]
    entries.sort_by(|(a, _), (b, _)| a.cmp(b));

    let nodes: Vec<OutputNode> = entries
        .iter()
        .map(|(name, value)| OutputNode::new(name.as_str()).with_cells(vec![value.to_string()]))
        .collect();

    ExecResult::with_output(OutputData::table(headers, nodes))
}

/// Unalias tool: remove command aliases.
pub struct Unalias;

/// clap-derived argv layer for unalias.
#[derive(Parser, Debug)]
#[command(name = "unalias", about = "Remove command aliases")]
struct UnaliasArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Alias names to remove.
    names: Vec<String>,
}

#[async_trait]
impl Tool for Unalias {
    fn name(&self) -> &str {
        "unalias"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &UnaliasArgs::command(),
            "unalias",
            "Remove command aliases",
            [("Remove an alias", "unalias ll")],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("unalias: {e}")),
        };
        let parsed = match UnaliasArgs::try_parse_from(
            std::iter::once("unalias".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("unalias: {e}")),
        };
        parsed.global.apply(ctx);

        if args.positional.is_empty() {
            return ExecResult::failure(1, "unalias: missing alias name");
        }

        for arg in &args.positional {
            // Loud on binary (GH #116): `unalias $BIN` must not silently try to
            // remove an alias literally named `[binary: N bytes]`.
            let name = match arg {
                Value::String(s) => s.clone(),
                other => match crate::interpreter::value_to_text_sink_named(other, "an alias name") {
                    Ok(s) => s,
                    Err(e) => return ExecResult::failure(1, format!("unalias: {e}")),
                },
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
        assert!(result.text_out().contains("ls -la"));
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
