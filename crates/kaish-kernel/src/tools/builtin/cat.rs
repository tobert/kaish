//! cat — Read and output file contents.

use async_trait::async_trait;
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, Tool, ToolArgs, ToolSchema, ParamSchema};

/// Cat tool: read and output file contents.
pub struct Cat;

#[async_trait]
impl Tool for Cat {
    fn name(&self) -> &str {
        "cat"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("cat", "Read and output file contents")
            .param(ParamSchema::required("path", "string", "File path(s) to read"))
            .param(ParamSchema::optional(
                "number",
                "bool",
                Value::Bool(false),
                "Number output lines (-n)",
            ).with_aliases(["-n"]))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let number_lines = args.has_flag("number") || args.has_flag("n");

        // If no files specified, read from stdin (like POSIX cat)
        if args.positional.is_empty() {
            if let Some(stdin) = ctx.take_stdin() {
                if number_lines {
                    let numbered = stdin
                        .lines()
                        .enumerate()
                        .map(|(i, line)| format!("{:6}\t{}", i + 1, line))
                        .collect::<Vec<_>>()
                        .join("\n");
                    return ExecResult::with_output(OutputData::text(numbered));
                }
                return ExecResult::with_output(OutputData::text(stdin));
            }
            return ExecResult::failure(1, "cat: missing path argument");
        }
        let mut all_content = String::new();
        let mut line_num = 1;

        for (i, arg) in args.positional.iter().enumerate() {
            let path = match arg {
                Value::String(s) => s.clone(),
                Value::Int(n) => n.to_string(),
                _ => continue,
            };

            let resolved = ctx.resolve_path(&path);

            match ctx.backend.read(Path::new(&resolved), None).await {
                Ok(data) => match String::from_utf8(data) {
                    Ok(content) => {
                        if number_lines {
                            for line in content.lines() {
                                if !all_content.is_empty() {
                                    all_content.push('\n');
                                }
                                all_content.push_str(&format!("{:6}\t{}", line_num, line));
                                line_num += 1;
                            }
                        } else {
                            if i > 0 && !all_content.is_empty() {
                                all_content.push('\n');
                            }
                            all_content.push_str(&content);
                        }
                    }
                    Err(_) => return ExecResult::failure(1, format!("cat: {}: invalid UTF-8", path)),
                },
                Err(e) => return ExecResult::failure(1, format!("cat: {}: {}", path, e)),
            }
        }

        ExecResult::with_output(OutputData::text(all_content))
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
        mem.write(Path::new("test.txt"), b"hello world").await.unwrap();
        mem.write(Path::new("dir/nested.txt"), b"nested content").await.unwrap();
        mem.write(Path::new("lines.txt"), b"line1\nline2\nline3").await.unwrap();
        mem.write(Path::new("other.txt"), b"other content").await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_cat_file() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/test.txt".into()));

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "hello world");
    }

    #[tokio::test]
    async fn test_cat_nested() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/dir/nested.txt".into()));

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "nested content");
    }

    #[tokio::test]
    async fn test_cat_not_found() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent.txt".into()));

        let result = Cat.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("not found") || result.err.contains("nonexistent"));
    }

    #[tokio::test]
    async fn test_cat_no_arg_no_stdin() {
        let mut ctx = make_ctx().await;
        let args = ToolArgs::new();

        let result = Cat.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("missing"));
    }

    #[tokio::test]
    async fn test_cat_from_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hello from stdin".to_string());
        let args = ToolArgs::new();

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "hello from stdin");
    }

    #[tokio::test]
    async fn test_cat_from_stdin_with_line_numbers() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("line1\nline2\nline3".to_string());
        let mut args = ToolArgs::new();
        args.flags.insert("n".to_string());

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("1\tline1"));
        assert!(result.out.contains("2\tline2"));
        assert!(result.out.contains("3\tline3"));
    }

    #[tokio::test]
    async fn test_cat_multiple_files() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/test.txt".into()));
        args.positional.push(Value::String("/other.txt".into()));

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("hello world"));
        assert!(result.out.contains("other content"));
    }

    #[tokio::test]
    async fn test_cat_n_line_numbers() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/lines.txt".into()));
        args.flags.insert("n".to_string());

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("1\tline1"));
        assert!(result.out.contains("2\tline2"));
        assert!(result.out.contains("3\tline3"));
    }

    #[tokio::test]
    async fn test_cat_number_line_numbers() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/lines.txt".into()));
        args.flags.insert("number".to_string());

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("1\tline1"));
    }

    #[tokio::test]
    async fn test_cat_n_multiple_files_continuous_numbering() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/lines.txt".into()));
        args.positional.push(Value::String("/test.txt".into()));
        args.flags.insert("n".to_string());

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        // lines.txt has 3 lines, so test.txt should start at line 4
        assert!(result.out.contains("4\thello world"));
    }
}
