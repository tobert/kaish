//! Introspection builtins for kaish.
//!
//! These builtins provide visibility into kernel state for context generation.

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::{value_to_json, ExecResult};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

// ============================================================================
// tools — List available tools
// ============================================================================

/// Tools builtin: lists all available tools and their schemas.
pub struct Tools;

#[async_trait]
impl Tool for Tools {
    fn name(&self) -> &str {
        "tools"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("tools", "List available tools and their schemas")
            .param(ParamSchema::optional(
                "name",
                "string",
                Value::Null,
                "Show detailed schema for a specific tool",
            ))
            .param(ParamSchema::optional(
                "json",
                "bool",
                Value::Bool(false),
                "Output as JSON",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let json_output = args.has_flag("json");
        let tool_name = args.get_string("name", 0);

        if let Some(name) = tool_name {
            format_tool_detail(&ctx.tool_schemas, &name, json_output)
        } else {
            format_tool_list(&ctx.tool_schemas, json_output)
        }
    }
}

fn format_tool_list(schemas: &[ToolSchema], json_output: bool) -> ExecResult {
    if json_output {
        let json_tools: Vec<serde_json::Value> = schemas
            .iter()
            .map(|s| {
                let params: Vec<serde_json::Value> = s
                    .params
                    .iter()
                    .map(|p| {
                        serde_json::json!({
                            "name": p.name,
                            "type": p.param_type,
                            "required": p.required,
                            "description": p.description
                        })
                    })
                    .collect();

                serde_json::json!({
                    "name": s.name,
                    "description": s.description,
                    "params": params
                })
            })
            .collect();

        match serde_json::to_string_pretty(&json_tools) {
            Ok(json_str) => ExecResult::success(json_str),
            Err(e) => ExecResult::failure(1, format!("failed to serialize tools: {}", e)),
        }
    } else {
        let mut output = String::new();
        for schema in schemas {
            output.push_str(&format!("{:<16} {}\n", schema.name, schema.description));
        }
        ExecResult::success(output)
    }
}

fn format_tool_detail(schemas: &[ToolSchema], name: &str, json_output: bool) -> ExecResult {
    let schema = schemas.iter().find(|s| s.name == name);

    match schema {
        Some(s) => {
            if json_output {
                let params: Vec<serde_json::Value> = s
                    .params
                    .iter()
                    .map(|p| {
                        let mut obj = serde_json::json!({
                            "name": p.name,
                            "type": p.param_type,
                            "required": p.required,
                            "description": p.description
                        });
                        if let Some(ref default) = p.default {
                            obj["default"] = value_to_json(default);
                        }
                        obj
                    })
                    .collect();

                let json_obj = serde_json::json!({
                    "name": s.name,
                    "description": s.description,
                    "params": params
                });

                match serde_json::to_string_pretty(&json_obj) {
                    Ok(json_str) => ExecResult::success(json_str),
                    Err(e) => ExecResult::failure(1, format!("failed to serialize tool: {}", e)),
                }
            } else {
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

                ExecResult::success(output)
            }
        }
        None => ExecResult::failure(1, format!("tool not found: {}", name)),
    }
}

// ============================================================================
// mounts — List VFS mount points
// ============================================================================

/// Mounts builtin: lists all VFS mount points.
pub struct Mounts;

#[async_trait]
impl Tool for Mounts {
    fn name(&self) -> &str {
        "mounts"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("mounts", "List VFS mount points")
            .param(ParamSchema::optional(
                "json",
                "bool",
                Value::Bool(false),
                "Output as JSON",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let json_output = args.has_flag("json");
        let mounts = ctx.backend.mounts();

        if json_output {
            let json_mounts: Vec<serde_json::Value> = mounts
                .iter()
                .map(|m| {
                    serde_json::json!({
                        "path": m.path.to_string_lossy(),
                        "read_only": m.read_only
                    })
                })
                .collect();

            match serde_json::to_string_pretty(&json_mounts) {
                Ok(json_str) => ExecResult::success(json_str),
                Err(e) => ExecResult::failure(1, format!("failed to serialize mounts: {}", e)),
            }
        } else {
            let mut output = String::new();
            for mount in mounts {
                let mode = if mount.read_only { "ro" } else { "rw" };
                output.push_str(&format!("{} ({})\n", mount.path.display(), mode));
            }
            ExecResult::success(output)
        }
    }
}

// ============================================================================
// history — Show execution history
// ============================================================================

/// History builtin: shows execution history from the state store.
pub struct History;

#[async_trait]
impl Tool for History {
    fn name(&self) -> &str {
        "history"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("history", "Show execution history")
            .param(ParamSchema::optional(
                "limit",
                "int",
                Value::Int(20),
                "Maximum number of entries to show",
            ))
            .param(ParamSchema::optional(
                "since_checkpoint",
                "bool",
                Value::Bool(false),
                "Only show history since the last checkpoint",
            ))
            .param(ParamSchema::optional(
                "json",
                "bool",
                Value::Bool(false),
                "Output as JSON",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let json_output = args.has_flag("json");
        let since_checkpoint = args.has_flag("since_checkpoint");
        let limit = args
            .get("limit", 0)
            .and_then(|v| match v {
                Value::Int(i) => Some(*i as usize),
                _ => None,
            })
            .unwrap_or(20);

        let state_store = match &ctx.state_store {
            Some(store) => store,
            None => {
                return ExecResult::failure(1, "history requires a persistent kernel (no state store)")
            }
        };

        let entries = {
            let store = match state_store.lock() {
                Ok(s) => s,
                Err(e) => return ExecResult::failure(1, format!("failed to lock state store: {}", e)),
            };

            if since_checkpoint {
                match store.history_since_checkpoint() {
                    Ok(e) => e,
                    Err(e) => return ExecResult::failure(1, format!("failed to get history: {}", e)),
                }
            } else {
                match store.get_history(limit) {
                    Ok(e) => e,
                    Err(e) => return ExecResult::failure(1, format!("failed to get history: {}", e)),
                }
            }
        };

        if json_output {
            format_history_json(&entries)
        } else {
            format_history_plain(&entries)
        }
    }
}

fn format_history_plain(entries: &[crate::state::HistoryEntry]) -> ExecResult {
    let mut output = String::new();

    for entry in entries {
        let id = entry.id.unwrap_or(0);
        let status = if entry.result_ok { "✓" } else { "✗" };
        let code = format!("[{}]", entry.code.trim());
        let timestamp = entry.created_at.as_deref().unwrap_or("");

        output.push_str(&format!(
            "{:4} {} {:50} {}\n",
            id,
            status,
            code,
            timestamp
        ));
    }

    ExecResult::success(output)
}

fn format_history_json(entries: &[crate::state::HistoryEntry]) -> ExecResult {
    let json_entries: Vec<serde_json::Value> = entries
        .iter()
        .map(|e| {
            serde_json::json!({
                "id": e.id,
                "code": e.code,
                "result_code": e.result_code,
                "result_ok": e.result_ok,
                "result_out": e.result_out,
                "result_err": e.result_err,
                "duration_ms": e.duration_ms,
                "created_at": e.created_at
            })
        })
        .collect();

    match serde_json::to_string_pretty(&json_entries) {
        Ok(json_str) => ExecResult::success(json_str),
        Err(e) => ExecResult::failure(1, format!("failed to serialize history: {}", e)),
    }
}

// ============================================================================
// checkpoints — List checkpoints
// ============================================================================

/// Checkpoints builtin: lists all checkpoints from the state store.
pub struct Checkpoints;

#[async_trait]
impl Tool for Checkpoints {
    fn name(&self) -> &str {
        "checkpoints"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("checkpoints", "List checkpoints")
            .param(ParamSchema::optional(
                "json",
                "bool",
                Value::Bool(false),
                "Output as JSON",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let json_output = args.has_flag("json");

        let state_store = match &ctx.state_store {
            Some(store) => store,
            None => {
                return ExecResult::failure(1, "checkpoints requires a persistent kernel (no state store)")
            }
        };

        let checkpoints = {
            let store = match state_store.lock() {
                Ok(s) => s,
                Err(e) => return ExecResult::failure(1, format!("failed to lock state store: {}", e)),
            };

            match store.list_checkpoints() {
                Ok(c) => c,
                Err(e) => return ExecResult::failure(1, format!("failed to list checkpoints: {}", e)),
            }
        };

        if json_output {
            format_checkpoints_json(&checkpoints)
        } else {
            format_checkpoints_plain(&checkpoints)
        }
    }
}

fn format_checkpoints_plain(checkpoints: &[crate::state::Checkpoint]) -> ExecResult {
    if checkpoints.is_empty() {
        return ExecResult::success("No checkpoints.\n");
    }

    let mut output = String::new();

    for checkpoint in checkpoints {
        let id = checkpoint.id.unwrap_or(0);
        let name = checkpoint.name.as_deref().unwrap_or("-");
        let timestamp = checkpoint.created_at.as_deref().unwrap_or("");

        output.push_str(&format!(
            "{:4} {:20} {}\n     {}\n\n",
            id, name, timestamp, checkpoint.summary
        ));
    }

    ExecResult::success(output)
}

fn format_checkpoints_json(checkpoints: &[crate::state::Checkpoint]) -> ExecResult {
    let json_checkpoints: Vec<serde_json::Value> = checkpoints
        .iter()
        .map(|c| {
            serde_json::json!({
                "id": c.id,
                "name": c.name,
                "summary": c.summary,
                "up_to_history_id": c.up_to_history_id,
                "created_at": c.created_at
            })
        })
        .collect();

    match serde_json::to_string_pretty(&json_checkpoints) {
        Ok(json_str) => ExecResult::success(json_str),
        Err(e) => ExecResult::failure(1, format!("failed to serialize checkpoints: {}", e)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
        assert!(result.out.contains("echo"));
        assert!(result.out.contains("cat"));
    }

    #[tokio::test]
    async fn test_tools_list_json() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("json".to_string());

        let result = Tools.execute(args, &mut ctx).await;
        assert!(result.ok());

        let data: Vec<serde_json::Value> = serde_json::from_str(&result.out).expect("valid JSON");
        assert_eq!(data.len(), 2);
    }

    #[tokio::test]
    async fn test_tools_detail() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("echo".into()));

        let result = Tools.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("echo"));
        assert!(result.out.contains("Print arguments"));
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
        assert!(result.out.contains("/"));
        assert!(result.out.contains("/tmp"));
        assert!(result.out.contains("rw"));
    }

    #[tokio::test]
    async fn test_mounts_list_json() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("json".to_string());

        let result = Mounts.execute(args, &mut ctx).await;
        assert!(result.ok());

        let data: Vec<serde_json::Value> = serde_json::from_str(&result.out).expect("valid JSON");
        assert!(!data.is_empty());

        let paths: Vec<&str> = data
            .iter()
            .filter_map(|v| v.get("path").and_then(|p| p.as_str()))
            .collect();
        assert!(paths.contains(&"/"));
        assert!(paths.contains(&"/tmp"));
    }

    // ============================
    // history tests (without state store)
    // ============================

    #[tokio::test]
    async fn test_history_no_state_store() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = History.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("no state store"));
    }

    // ============================
    // checkpoints tests (without state store)
    // ============================

    #[tokio::test]
    async fn test_checkpoints_no_state_store() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Checkpoints.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("no state store"));
    }
}
