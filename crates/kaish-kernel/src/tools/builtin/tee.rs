//! tee — Read from stdin and write to both stdout and files.

use async_trait::async_trait;
use std::path::Path;

use crate::ast::Value;
use crate::backend::WriteMode;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Tee tool: duplicate stdin to stdout and files.
pub struct Tee;

#[async_trait]
impl Tool for Tee {
    fn name(&self) -> &str {
        "tee"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("tee", "Read from stdin and write to stdout and files")
            .param(ParamSchema::required(
                "path",
                "string",
                "File to write to",
            ))
            .param(ParamSchema::optional(
                "append",
                "bool",
                Value::Bool(false),
                "Append to file instead of overwriting (-a)",
            ))
            .example("Save and display", "echo hello | tee output.txt")
            .example("Append to log", "echo entry | tee -a log.txt")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let path_str = match args.get_string("path", 0) {
            Some(p) => p,
            None => return ExecResult::failure(1, "tee: missing file argument"),
        };

        let append = args.has_flag("append") || args.has_flag("a");
        let input = ctx.read_stdin_to_string().await.unwrap_or_default();

        let resolved = ctx.resolve_path(&path_str);
        // Note: kaish VFS doesn't support append mode directly
        // For append, we need to read existing content and concatenate
        let final_content = if append {
            match ctx.backend.read(Path::new(&resolved), None).await {
                Ok(existing) => {
                    let mut combined = existing;
                    combined.extend_from_slice(input.as_bytes());
                    combined
                }
                Err(_) => input.as_bytes().to_vec(),
            }
        } else {
            input.as_bytes().to_vec()
        };

        match ctx
            .backend
            .write(Path::new(&resolved), &final_content, WriteMode::Overwrite)
            .await
        {
            Ok(()) => ExecResult::with_output(OutputData::text(input)),
            Err(e) => ExecResult::failure(1, format!("tee: {}: {}", path_str, e)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("existing.txt"), b"original content\n")
            .await
            .unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_tee_new_file() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hello world\n".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/output.txt".into()));

        let result = Tee.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "hello world\n");

        let written = ctx
            .backend
            .read(Path::new("/output.txt"), None)
            .await
            .unwrap();
        assert_eq!(written, b"hello world\n");
    }

    #[tokio::test]
    async fn test_tee_overwrite() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("new content\n".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/existing.txt".into()));

        let result = Tee.execute(args, &mut ctx).await;
        assert!(result.ok());

        let written = ctx
            .backend
            .read(Path::new("/existing.txt"), None)
            .await
            .unwrap();
        assert_eq!(written, b"new content\n");
    }

    #[tokio::test]
    async fn test_tee_append() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("appended\n".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/existing.txt".into()));
        args.flags.insert("a".to_string());

        let result = Tee.execute(args, &mut ctx).await;
        assert!(result.ok());

        let written = ctx
            .backend
            .read(Path::new("/existing.txt"), None)
            .await
            .unwrap();
        assert_eq!(written, b"original content\nappended\n");
    }

    #[tokio::test]
    async fn test_tee_empty_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/empty.txt".into()));

        let result = Tee.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "");

        let written = ctx
            .backend
            .read(Path::new("/empty.txt"), None)
            .await
            .unwrap();
        assert!(written.is_empty());
    }

    #[tokio::test]
    async fn test_tee_missing_file() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("data\n".to_string());

        let result = Tee.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
    }
}
