//! write — Write content to a file.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::builtin::get_path_string;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Write tool: write content to a file.
pub struct Write;

/// clap-derived argv layer for write.
#[derive(Parser, Debug)]
#[command(name = "write", about = "Write content to a file")]
struct WriteArgs {
    /// File path to write to (positional or --path).
    #[arg(long)]
    path: Option<String>,

    /// Content to write (positional or --content). Falls back to stdin when absent.
    #[arg(long)]
    content: Option<String>,

    /// Confirmation nonce for a latch-gated overwrite.
    #[arg(long = "confirm")]
    confirm: Option<String>,

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
        // `--content` is never read off `parsed.content` — see below, it's
        // always read as a raw typed `Value` off `args.named`/`args.positional`
        // specifically so a `Value::Bytes` payload survives untouched. That
        // makes it the same "clap sink nobody reads" case as `push`'s hidden
        // positional (CLAUDE.md's clap-builtin convention), just on a named
        // flag instead: `to_argv()`'s loud named-Bytes guard (GH #164) exists
        // for keys a builtin *does* read via the clap-parsed field, so drop
        // `content` before computing argv for clap — every other flag
        // (`path`/`confirm`/global) still gets the full guard, and
        // `parsed.content` simply stays `None`, which is fine since nothing
        // reads it.
        let mut argv_source = args.clone();
        argv_source.named.remove("content");
        let argv = match argv_source.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("write: {e}")),
        };
        let parsed = match WriteArgs::try_parse_from(
            std::iter::once("write".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("write: {e}")),
        };
        parsed.global.apply(ctx);

        let path = match get_path_string(&args, "path", 0) {
            Ok(Some(p)) => p,
            Ok(None) => return ExecResult::failure(1, "write: missing path argument"),
            Err(e) => return ExecResult::failure(1, format!("write: {e}")),
        };

        let resolved = ctx.resolve_path(&path);

        // Gate the truncating overwrite through latch + trash (no-op when both
        // are off). On latch this returns an exit-2 nonce result; under trash
        // the prior content is snapshotted and returned for the CAS below.
        let snapshots = match ctx
            .gate_overwrites("write", &[(path.clone(), false)], parsed.confirm.as_deref(), |nonce, joined| {
                format!("write --confirm=\"{nonce}\" {joined}")
            })
            .await
        {
            Ok(s) => s,
            Err(blocked) => return blocked,
        };

        // Content can be positional[1], named "content", or stdin. Read it as
        // raw bytes so a `Value::Bytes` (e.g. from `$(producer)`) is written
        // verbatim instead of stringified to the `[binary: N bytes]` marker —
        // that placeholder reaching a file is silent corruption. Stdin uses the
        // byte reader (pipe-preferred) for the same reason.
        let content: Vec<u8> = if let Some(v) = args.named.get("content") {
            value_to_bytes(v)
        } else if let Some(v) = args.positional.get(1) {
            value_to_bytes(v)
        } else {
            match ctx.read_stdin_to_bytes().await {
                Some(bytes) => bytes,
                None => return ExecResult::failure(1, "write: missing content argument"),
            }
        };

        // CAS against the gate snapshot: a concurrent change between the gate
        // and this write is a loud conflict, not a silent clobber.
        let expected = snapshots.get(&resolved).map(|v| v.as_slice());
        match ctx.overwrite_checked(Path::new(&resolved), &content, expected).await {
            Ok(()) => ExecResult::with_output(OutputData::text(format!("Wrote {} bytes to {}", content.len(), path))),
            Err(e) => ExecResult::failure(1, format!("write: {}: {}", path, e)),
        }
    }
}

/// Render a value as the raw bytes to write. `Bytes` is written verbatim;
/// every other variant uses its lossless string form.
fn value_to_bytes(value: &Value) -> Vec<u8> {
    match value {
        Value::Bytes(b) => b.clone(),
        Value::String(s) => s.clone().into_bytes(),
        Value::Int(i) => i.to_string().into_bytes(),
        Value::Float(f) => f.to_string().into_bytes(),
        Value::Bool(b) => b.to_string().into_bytes(),
        Value::Null => b"null".to_vec(),
        Value::Json(json) => json.to_string().into_bytes(),
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
