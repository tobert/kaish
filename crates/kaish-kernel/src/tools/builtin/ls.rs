//! ls — List directory contents.

use async_trait::async_trait;
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, Tool, ToolArgs, ToolSchema, ParamSchema};

/// Ls tool: list directory contents.
pub struct Ls;

#[async_trait]
impl Tool for Ls {
    fn name(&self) -> &str {
        "ls"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("ls", "List directory contents")
            .param(ParamSchema::optional(
                "path",
                "string",
                Value::String(".".into()),
                "Directory path to list",
            ))
            .param(ParamSchema::optional(
                "long",
                "bool",
                Value::Bool(false),
                "Use long format with details (-l)",
            ))
            .param(ParamSchema::optional(
                "all",
                "bool",
                Value::Bool(false),
                "Show hidden files starting with . (-a)",
            ))
            .param(ParamSchema::optional(
                "one",
                "bool",
                Value::Bool(false),
                "One entry per line (-1)",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let path = args
            .get_string("path", 0)
            .unwrap_or_else(|| ".".to_string());

        let resolved = ctx.resolve_path(&path);
        let long_format = args.has_flag("long") || args.has_flag("l");
        let show_all = args.has_flag("all") || args.has_flag("a");
        // -1 is always true for now (we output one per line), but flag is recognized

        match ctx.backend.list(Path::new(&resolved)).await {
            Ok(entries) => {
                // Filter hidden files (starting with .) unless -a is set
                let filtered: Vec<_> = entries
                    .into_iter()
                    .filter(|e| show_all || !e.name.starts_with('.'))
                    .collect();

                if filtered.is_empty() {
                    return ExecResult::success("");
                }

                let lines: Vec<String> = if long_format {
                    filtered
                        .iter()
                        .map(|e| {
                            let type_char = if e.is_dir { 'd' } else { '-' };
                            format!("{}  {}", type_char, e.name)
                        })
                        .collect()
                } else {
                    filtered.iter().map(|e| e.name.clone()).collect()
                };

                ExecResult::success(lines.join("\n"))
            }
            Err(e) => ExecResult::failure(1, format!("ls: {}: {}", path, e)),
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
        mem.write(Path::new("file1.txt"), b"a").await.unwrap();
        mem.write(Path::new("file2.txt"), b"b").await.unwrap();
        mem.mkdir(Path::new("subdir")).await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_ls_root() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("file1.txt"));
        assert!(result.out.contains("file2.txt"));
        assert!(result.out.contains("subdir"));
    }

    #[tokio::test]
    async fn test_ls_long() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));
        args.named.insert("long".to_string(), Value::Bool(true));

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("d  subdir"));
        assert!(result.out.contains("-  file1.txt"));
    }

    #[tokio::test]
    async fn test_ls_cwd() {
        let mut ctx = make_ctx().await;
        let args = ToolArgs::new();

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        // cwd is /, should list root
        assert!(result.out.contains("file1.txt"));
    }

    #[tokio::test]
    async fn test_ls_not_found() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));

        let result = Ls.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    async fn make_ctx_with_hidden() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("visible.txt"), b"a").await.unwrap();
        mem.write(Path::new(".hidden"), b"b").await.unwrap();
        mem.mkdir(Path::new(".config")).await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_ls_hides_dotfiles_by_default() {
        let mut ctx = make_ctx_with_hidden().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("visible.txt"));
        assert!(!result.out.contains(".hidden"));
        assert!(!result.out.contains(".config"));
    }

    #[tokio::test]
    async fn test_ls_a_shows_hidden() {
        let mut ctx = make_ctx_with_hidden().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));
        args.flags.insert("a".to_string());

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("visible.txt"));
        assert!(result.out.contains(".hidden"));
        assert!(result.out.contains(".config"));
    }

    #[tokio::test]
    async fn test_ls_all_shows_hidden() {
        let mut ctx = make_ctx_with_hidden().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));
        args.flags.insert("all".to_string());

        let result = Ls.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains(".hidden"));
    }
}
