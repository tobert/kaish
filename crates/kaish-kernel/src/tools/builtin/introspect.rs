//! Introspection builtins for kaish.
//!
//! These builtins provide visibility into kernel state for context generation.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

// ============================================================================
// kaish-tools — List available tools
// ============================================================================

/// Tools builtin: lists all available tools and their schemas.
pub struct Tools;

/// clap-derived argv layer for kaish-tools.
#[derive(Parser, Debug)]
#[command(name = "kaish-tools", about = "List available tools and their schemas")]
struct ToolsArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Tool name to introspect; lists all tools when empty.
    tool: Vec<String>,
}

#[async_trait]
impl Tool for Tools {
    fn name(&self) -> &str {
        "kaish-tools"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &ToolsArgs::command(),
            "kaish-tools",
            "List available tools and their schemas",
            [
                ("List all tools", "kaish-tools"),
                ("Show tool detail", "kaish-tools cat"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("kaish-tools: {e}")),
        };
        let parsed = match ToolsArgs::try_parse_from(
            std::iter::once("kaish-tools".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("kaish-tools: {e}")),
        };
        parsed.global.apply(ctx);

        let tool_name = args.get_string("name", 0);

        if let Some(name) = tool_name {
            format_tool_detail(&ctx.tool_schemas, &name)
        } else {
            format_tool_list(&ctx.tool_schemas)
        }
    }
}

fn format_tool_list(schemas: &[ToolSchema]) -> ExecResult {
    let headers = vec![
        "NAME".to_string(),
        "DESCRIPTION".to_string(),
        "PARAMS".to_string(),
    ];

    let nodes: Vec<OutputNode> = schemas
        .iter()
        .map(|s| {
            let param_count = s.params.len().to_string();
            OutputNode::new(&s.name).with_cells(vec![s.description.clone(), param_count])
        })
        .collect();

    ExecResult::with_output(OutputData::table(headers, nodes))
}

fn format_tool_detail(schemas: &[ToolSchema], name: &str) -> ExecResult {
    let schema = schemas.iter().find(|s| s.name == name);

    match schema {
        Some(s) => {
            let mut output = format!("{}\n{}\n\n", s.name, s.description);

            if !s.params.is_empty() {
                output.push_str("Parameters:\n");
                for p in &s.params {
                    let required = if p.required { "(required)" } else { "(optional)" };
                    output.push_str(&format!(
                        "  {} : {} {}\n    {}\n",
                        p.name, p.param_type, required, p.description
                    ));
                }
            }

            ExecResult::with_output(OutputData::text(output))
        }
        None => ExecResult::failure(1, format!("tool not found: {}", name)),
    }
}

// ============================================================================
// kaish-mounts — List VFS mount points
// ============================================================================

/// Mounts builtin: lists all VFS mount points.
pub struct Mounts;

/// clap-derived argv layer for kaish-mounts.
#[derive(Parser, Debug)]
#[command(name = "kaish-mounts", about = "List VFS mount points")]
struct MountsArgs {
    #[command(flatten)]
    global: GlobalFlags,
}

/// Format a byte count in a human-readable short form (right-aligned for columns).
/// Returns "-" for None (disk-backed mount — disk residency is the host's concern).
fn format_resident(bytes: Option<u64>) -> String {
    match bytes {
        None => "-".to_string(),
        Some(b) => {
            const UNITS: &[&str] = &["", "K", "M", "G", "T"];
            let mut size = b as f64;
            let mut idx = 0;
            while size >= 1024.0 && idx < UNITS.len() - 1 {
                size /= 1024.0;
                idx += 1;
            }
            if idx == 0 {
                b.to_string()
            } else if size >= 10.0 {
                format!("{:.0}{}", size, UNITS[idx])
            } else {
                format!("{:.1}{}", size, UNITS[idx])
            }
        }
    }
}

#[async_trait]
impl Tool for Mounts {
    fn name(&self) -> &str {
        "kaish-mounts"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &MountsArgs::command(),
            "kaish-mounts",
            "List VFS mount points",
            [("Show mount points", "kaish-mounts")],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("kaish-mounts: {e}")),
        };
        let parsed = match MountsArgs::try_parse_from(
            std::iter::once("kaish-mounts".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("kaish-mounts: {e}")),
        };
        parsed.global.apply(ctx);

        let mounts = ctx.backend.mounts();
        let budget = ctx.vfs_budget.as_ref();

        let headers = vec![
            "PATH".to_string(),
            "MODE".to_string(),
            "RESIDENT".to_string(),
        ];

        let nodes: Vec<OutputNode> = mounts
            .iter()
            .map(|m| {
                let mode = if m.read_only { "ro" } else { "rw" };
                OutputNode::new(m.path.to_string_lossy())
                    .with_cells(vec![mode.to_string(), format_resident(m.resident_bytes)])
            })
            .collect();

        // Build the text-mode budget summary line (appended after the table).
        let budget_summary = budget.map(|b| {
            format!(
                "\nvfs-memory budget: {} used / {} limit / {} remaining",
                format_resident(Some(b.used())),
                format_resident(Some(b.limit())),
                format_resident(Some(b.remaining())),
            )
        });

        // Build the --json override: {mounts: [...], budget: {...}} so the
        // budget object is not a fake mount row. OutputData::with_rich_json
        // makes to_json() return this verbatim.
        let rich = {
            let mount_array: Vec<serde_json::Value> = mounts
                .iter()
                .map(|m| {
                    let mut obj = serde_json::Map::new();
                    obj.insert("path".to_string(), serde_json::Value::String(m.path.to_string_lossy().into_owned()));
                    obj.insert("read_only".to_string(), serde_json::Value::Bool(m.read_only));
                    match m.resident_bytes {
                        Some(n) => obj.insert("resident_bytes".to_string(), serde_json::Value::Number(n.into())),
                        None => obj.insert("resident_bytes".to_string(), serde_json::Value::Null),
                    };
                    serde_json::Value::Object(obj)
                })
                .collect();

            let mut top = serde_json::Map::new();
            top.insert("mounts".to_string(), serde_json::Value::Array(mount_array));
            if let Some(b) = budget {
                let mut bobj = serde_json::Map::new();
                bobj.insert("label".to_string(), serde_json::Value::String(b.label().to_string()));
                bobj.insert("used".to_string(), serde_json::Value::Number(b.used().into()));
                bobj.insert("limit".to_string(), serde_json::Value::Number(b.limit().into()));
                bobj.insert("remaining".to_string(), serde_json::Value::Number(b.remaining().into()));
                top.insert("budget".to_string(), serde_json::Value::Object(bobj));
            }
            serde_json::Value::Object(top)
        };

        let output = OutputData::table(headers, nodes).with_rich_json(rich);

        // For text mode: if there's a budget summary, materialize the table
        // to its canonical string first, then append the summary line.
        // `with_output_and_text` stores both: `out` (text rendering) and
        // `output` (structured, used by --json via rich_json override).
        if let Some(summary) = budget_summary {
            let text = format!("{}{}", output.to_canonical_string(), summary);
            ExecResult::with_output_and_text(output, text)
        } else {
            ExecResult::with_output(output)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::interpreter::{apply_output_format, OutputFormat};
    use crate::tools::ToolSchema as TS;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        vfs.mount("/tmp", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));
        ctx.set_tool_schemas(vec![
            TS::new("echo", "Print arguments"),
            TS::new("cat", "Concatenate files"),
        ]);
        ctx
    }

    // ============================
    // tools tests
    // ============================

    #[tokio::test]
    async fn test_tools_list_plain() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Tools.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("echo"));
        assert!(result.text_out().contains("cat"));
        // Should have structured OutputData with table headers
        let output = result.output().expect("should have OutputData");
        assert!(output.headers.is_some());
    }

    #[tokio::test]
    async fn test_tools_list_json_via_global_flag() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Tools.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Simulate global --json (handled by kernel)
        let result = apply_output_format(result, OutputFormat::Json);
        let data: Vec<serde_json::Value> = serde_json::from_str(&result.text_out()).expect("valid JSON");
        assert_eq!(data.len(), 2);
        assert!(data[0].get("NAME").is_some());
    }

    #[tokio::test]
    async fn test_tools_detail() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("echo".into()));

        let result = Tools.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("echo"));
        assert!(result.text_out().contains("Print arguments"));
    }

    #[tokio::test]
    async fn test_tools_detail_not_found() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("nonexistent".into()));

        let result = Tools.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("tool not found"));
    }

    // ============================
    // mounts tests
    // ============================

    #[tokio::test]
    async fn test_mounts_list_plain() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Mounts.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("/"));
        assert!(result.text_out().contains("/tmp"));
        assert!(result.text_out().contains("rw"));
    }

    #[tokio::test]
    async fn test_mounts_list_json_via_global_flag() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Mounts.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Simulate global --json (handled by kernel).
        // Shape is now {mounts: [{path, read_only, resident_bytes}, ...]}
        // (and optionally a "budget" key when a budget is set — not present here).
        let result = apply_output_format(result, OutputFormat::Json);
        let top: serde_json::Value =
            serde_json::from_str(&result.text_out()).expect("valid JSON");

        let data = top.get("mounts").expect("must have 'mounts' key");
        assert!(data.is_array(), "'mounts' must be an array");
        let data = data.as_array().unwrap();
        assert!(!data.is_empty());

        let paths: Vec<&str> = data
            .iter()
            .filter_map(|v| v.get("path").and_then(|p| p.as_str()))
            .collect();
        assert!(paths.contains(&"/"));
        assert!(paths.contains(&"/tmp"));

        // No budget key for an unbudgeted context.
        assert!(top.get("budget").is_none(), "no budget key for unbudgeted ctx");
    }
}
