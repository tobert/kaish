//! write — Write content to a file.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::ast::Value;
use crate::backend::WriteMode;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Write tool: write content to a file.
pub struct Write;

/// clap-derived argv layer for write. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "write", about = "Write content to a file")]
struct WriteArgs {
    /// File path to write to (positional or --path).
    #[arg(long)]
    path: Option<String>,

    /// Content to write (positional or --content). Falls back to stdin when absent.
    #[arg(long)]
    content: Option<String>,

    #[command(flatten)]
    global: GlobalFlags,

    /// Destination path followed by the content to write.
    args: Vec<String>,
}

#[async_trait]
impl Tool for Write {
    fn name(&self) -> &str {
        "write"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &WriteArgs::command(),
            "write",
            "Write content to a file",
            [
                ("Write to a file", "write output.txt \"hello world\""),
                ("Pipe into write", "echo content | write file.txt"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let parsed = match WriteArgs::try_parse_from(
            std::iter::once("write".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("write: {e}")),
        };
        parsed.global.apply(ctx);

        let path = match args.get_string("path", 0) {
            Some(p) => p,
            None => return ExecResult::failure(1, "write: missing path argument"),
        };

        // Content can be positional[1], named "content", or stdin (pipe or
        // buffered — `read_stdin_to_text` prefers the streaming pipe).
        let content = if let Some(v) = args.named.get("content") {
            value_to_string(v)
        } else if let Some(v) = args.positional.get(1) {
            value_to_string(v)
        } else {
            match ctx.read_stdin_to_text().await {
                Ok(Some(s)) => s,
                Ok(None) => return ExecResult::failure(1, "write: missing content argument"),
                Err(e) => return ExecResult::failure(2, format!("write: {e}")),
            }
        };

        let resolved = ctx.resolve_path(&path);

        match ctx.backend.write(Path::new(&resolved), content.as_bytes(), WriteMode::Overwrite).await {
            Ok(()) => ExecResult::with_output(OutputData::text(format!("Wrote {} bytes to {}", content.len(), path))),
            Err(e) => ExecResult::failure(1, format!("write: {}: {}", path, e)),
        }
    }
}

fn value_to_string(value: &Value) -> String {
    match value {
        Value::String(s) => s.clone(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Null => "null".to_string(),
        Value::Json(json) => json.to_string(),
        Value::Bytes(b) => format!("[binary: {} bytes]", b.len()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_write_simple() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/test.txt".into()));
        args.positional.push(Value::String("hello world".into()));

        let result = Write.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Verify content via backend
        let data = ctx.backend.read(Path::new("/test.txt"), None).await.unwrap();
        assert_eq!(String::from_utf8(data).unwrap(), "hello world");
    }

    #[tokio::test]
    async fn test_write_named() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/test.txt".into()));
        args.named.insert("content".to_string(), Value::String("named content".into()));

        let result = Write.execute(args, &mut ctx).await;
        assert!(result.ok());

        let data = ctx.backend.read(Path::new("/test.txt"), None).await.unwrap();
        assert_eq!(String::from_utf8(data).unwrap(), "named content");
    }

    #[tokio::test]
    async fn test_write_nested() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/a/b/c.txt".into()));
        args.positional.push(Value::String("nested".into()));

        let result = Write.execute(args, &mut ctx).await;
        assert!(result.ok());

        let data = ctx.backend.read(Path::new("/a/b/c.txt"), None).await.unwrap();
        assert_eq!(String::from_utf8(data).unwrap(), "nested");
    }

    #[tokio::test]
    async fn test_write_no_path() {
        let mut ctx = make_ctx().await;
        let args = ToolArgs::new();

        let result = Write.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("missing path"));
    }
}
