//! jq — JSON query tool wrapping /usr/bin/jq.
//!
//! # Examples
//!
//! ```kaish
//! echo '{"name": "Alice"}' | jq ".name"
//! echo '{"name": "Alice"}' | jq ".name" -r
//! jq ".items[]" path=/data/items.json
//! jq ".[] | select(.active)" -c
//! ```

use std::path::Path;

use async_trait::async_trait;
use tokio::io::AsyncWriteExt;
use tokio::process::Command;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};
use crate::vfs::Filesystem;

/// Jq tool: JSON query processor wrapping /usr/bin/jq.
pub struct Jq;

#[async_trait]
impl Tool for Jq {
    fn name(&self) -> &str {
        "jq"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("jq", "JSON query processor (wraps /usr/bin/jq)")
            .param(ParamSchema::required(
                "filter",
                "string",
                "jq filter expression",
            ))
            .param(ParamSchema::optional(
                "raw",
                "bool",
                Value::Bool(false),
                "Raw output mode (-r): output strings without quotes",
            ))
            .param(ParamSchema::optional(
                "compact",
                "bool",
                Value::Bool(false),
                "Compact output mode (-c): no pretty-printing",
            ))
            .param(ParamSchema::optional(
                "path",
                "string",
                Value::String("".into()),
                "Read from VFS file instead of stdin",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // Get filter (required, positional 0)
        let filter = match args.get_string("filter", 0) {
            Some(f) => f,
            None => return ExecResult::failure(1, "jq: filter expression required"),
        };

        // Build jq arguments
        let mut jq_args = vec![filter];

        if args.has_flag("raw") || args.has_flag("r") {
            jq_args.insert(0, "-r".to_string());
        }

        if args.has_flag("compact") || args.has_flag("c") {
            jq_args.insert(0, "-c".to_string());
        }

        // Get input: from path or stdin
        let input = if let Some(path) = args.get_string("path", 999) {
            if !path.is_empty() {
                // Read from VFS
                let resolved = ctx.resolve_path(&path);
                match ctx.vfs.read(Path::new(&resolved)).await {
                    Ok(bytes) => String::from_utf8_lossy(&bytes).into_owned(),
                    Err(e) => {
                        return ExecResult::failure(1, format!("jq: failed to read {}: {}", path, e))
                    }
                }
            } else {
                ctx.take_stdin().unwrap_or_default()
            }
        } else {
            ctx.take_stdin().unwrap_or_default()
        };

        if input.is_empty() {
            return ExecResult::failure(1, "jq: no input provided");
        }

        // Execute jq (search PATH)
        let mut cmd = Command::new("jq");
        cmd.args(&jq_args);
        cmd.stdin(std::process::Stdio::piped());
        cmd.stdout(std::process::Stdio::piped());
        cmd.stderr(std::process::Stdio::piped());

        let mut child = match cmd.spawn() {
            Ok(child) => child,
            Err(e) => return ExecResult::failure(127, format!("jq: failed to spawn: {}", e)),
        };

        // Write input to stdin
        if let Some(mut stdin) = child.stdin.take()
            && let Err(e) = stdin.write_all(input.as_bytes()).await {
                return ExecResult::failure(1, format!("jq: failed to write stdin: {}", e));
            }

        // Collect output
        match child.wait_with_output().await {
            Ok(output) => {
                let code = output.status.code().unwrap_or(-1) as i64;
                let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
                let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
                ExecResult::from_output(code, stdout, stderr)
            }
            Err(e) => ExecResult::failure(1, format!("jq: failed to wait: {}", e)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_jq_simple_filter() {
        let mut ctx = make_ctx();
        ctx.set_stdin(r#"{"name": "Alice"}"#.to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String(".name".into()));

        let result = Jq.execute(args, &mut ctx).await;
        assert!(result.ok(), "jq failed: {}", result.err);
        assert_eq!(result.out.trim(), "\"Alice\"");
    }

    #[tokio::test]
    async fn test_jq_raw_output() {
        let mut ctx = make_ctx();
        ctx.set_stdin(r#"{"name": "Alice"}"#.to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String(".name".into()));
        args.flags.insert("r".to_string());

        let result = Jq.execute(args, &mut ctx).await;
        assert!(result.ok(), "jq failed: {}", result.err);
        assert_eq!(result.out.trim(), "Alice");
    }

    #[tokio::test]
    async fn test_jq_array_iteration() {
        let mut ctx = make_ctx();
        ctx.set_stdin(r#"[1, 2, 3]"#.to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String(".[]".into()));

        let result = Jq.execute(args, &mut ctx).await;
        assert!(result.ok(), "jq failed: {}", result.err);
        assert_eq!(result.out.trim(), "1\n2\n3");
    }

    #[tokio::test]
    async fn test_jq_compact_output() {
        let mut ctx = make_ctx();
        ctx.set_stdin(r#"{"a": 1, "b": 2}"#.to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String(".".into()));
        args.flags.insert("c".to_string());

        let result = Jq.execute(args, &mut ctx).await;
        assert!(result.ok(), "jq failed: {}", result.err);
        // Compact output has no extra whitespace
        assert!(
            !result.out.contains("  "),
            "expected compact output, got: {}",
            result.out
        );
    }

    #[tokio::test]
    async fn test_jq_invalid_json() {
        let mut ctx = make_ctx();
        ctx.set_stdin("not valid json".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String(".".into()));

        let result = Jq.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_jq_invalid_filter() {
        let mut ctx = make_ctx();
        ctx.set_stdin(r#"{"a": 1}"#.to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String(".[[[invalid".into()));

        let result = Jq.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_jq_no_input() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String(".".into()));

        let result = Jq.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("no input"));
    }

    #[tokio::test]
    async fn test_jq_from_vfs_file() {
        let mut ctx = make_ctx();

        // Write test data to VFS
        ctx.vfs
            .write(Path::new("/test.json"), br#"{"value": 42}"#)
            .await
            .expect("failed to write test file");

        let mut args = ToolArgs::new();
        args.positional.push(Value::String(".value".into()));
        args.named
            .insert("path".to_string(), Value::String("/test.json".into()));

        let result = Jq.execute(args, &mut ctx).await;
        assert!(result.ok(), "jq failed: {}", result.err);
        assert_eq!(result.out.trim(), "42");
    }
}
