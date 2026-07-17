//! uniq — Report or filter out repeated lines.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::interpreter::{ExecResult, OutputData};
use crate::tools::builtin::get_path_string;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Uniq tool: report or filter out repeated adjacent lines.
pub struct Uniq;

/// clap-derived argv layer for uniq.
#[derive(Parser, Debug)]
#[command(name = "uniq", about = "Report or filter out repeated lines")]
struct UniqArgs {
    /// Prefix lines with occurrence count (-c)
    #[arg(short = 'c', long = "count")]
    count: bool,

    /// Only print duplicate lines (-d)
    #[arg(short = 'd', long = "repeated")]
    repeated: bool,

    /// Only print unique lines (-u)
    #[arg(short = 'u', long = "unique")]
    unique: bool,

    /// Ignore case when comparing (-i)
    #[arg(short = 'i', long = "ignore-case", visible_alias = "ignore_case")]
    ignore_case: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Input file; reads stdin when omitted.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Uniq {
    fn name(&self) -> &str {
        "uniq"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &UniqArgs::command(),
            "uniq",
            "Report or filter out repeated lines",
            [
                ("Remove adjacent duplicates", "sort data.txt | uniq"),
                ("Count occurrences", "sort data.txt | uniq -c"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("uniq: {e}")),
        };
        let parsed = match UniqArgs::try_parse_from(
            std::iter::once("uniq".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("uniq: {e}")),
        };
        parsed.global.apply(ctx);

        // Get input: from file or stdin. A binary `path` operand goes loud
        // rather than silently falling through to the stdin branch below.
        let input = match get_path_string(&args, "path", 0) {
            Ok(Some(path)) => {
                let resolved = ctx.resolve_path(&path);
                match ctx.backend.read(Path::new(&resolved), None).await {
                    Ok(data) => match String::from_utf8(data) {
                        Ok(s) => s,
                        Err(_) => {
                            return ExecResult::failure(
                                1,
                                format!("uniq: {}: invalid UTF-8", path),
                            )
                        }
                    },
                    Err(e) => return ExecResult::failure(1, format!("uniq: {}: {}", path, e)),
                }
            }
            Ok(None) => match ctx.read_stdin_to_text().await {
                Ok(s) => s.unwrap_or_default(),
                Err(e) => return ExecResult::failure(2, format!("uniq: {e}")),
            },
            Err(e) => return ExecResult::failure(1, format!("uniq: {e}")),
        };

        let show_count = parsed.count;
        let only_repeated = parsed.repeated;
        let only_unique = parsed.unique;
        let ignore_case = parsed.ignore_case;

        let lines: Vec<&str> = input.lines().collect();
        if lines.is_empty() {
            return ExecResult::success("");
        }

        // Group consecutive identical lines
        let mut groups: Vec<(usize, &str)> = Vec::new();
        let mut current_line = lines[0];
        let mut count = 1;

        for line in lines.iter().skip(1) {
            let matches = if ignore_case {
                line.eq_ignore_ascii_case(current_line)
            } else {
                *line == current_line
            };

            if matches {
                count += 1;
            } else {
                groups.push((count, current_line));
                current_line = line;
                count = 1;
            }
        }
        groups.push((count, current_line));

        // Filter based on flags
        let filtered: Vec<(usize, &str)> = groups
            .into_iter()
            .filter(|(c, _)| {
                if only_repeated {
                    *c > 1
                } else if only_unique {
                    *c == 1
                } else {
                    true
                }
            })
            .collect();

        // Format output
        let output: Vec<String> = if show_count {
            filtered
                .iter()
                .map(|(c, line)| format!("{:>7} {}", c, line))
                .collect()
        } else {
            filtered.iter().map(|(_, line)| (*line).to_string()).collect()
        };

        if output.is_empty() {
            ExecResult::success("")
        } else {
            ExecResult::with_output(OutputData::text(format!("{}\n", output.join("\n"))))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(
            Path::new("dup.txt"),
            b"apple\napple\nbanana\ncherry\ncherry\ncherry",
        )
        .await
        .unwrap();
        mem.write(Path::new("case.txt"), b"Hello\nhello\nHELLO\nworld")
            .await
            .unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_uniq_basic() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/dup.txt".into()));

        let result = Uniq.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["apple", "banana", "cherry"]);
    }

    #[tokio::test]
    async fn test_uniq_count() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/dup.txt".into()));
        args.flags.insert("c".to_string());

        let result = Uniq.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("2 apple"));
        assert!(result.text_out().contains("1 banana"));
        assert!(result.text_out().contains("3 cherry"));
    }

    #[tokio::test]
    async fn test_uniq_only_repeated() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/dup.txt".into()));
        args.flags.insert("d".to_string());

        let result = Uniq.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["apple", "cherry"]);
        assert!(!result.text_out().contains("banana"));
    }

    #[tokio::test]
    async fn test_uniq_only_unique() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/dup.txt".into()));
        args.flags.insert("u".to_string());

        let result = Uniq.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["banana"]);
    }

    #[tokio::test]
    async fn test_uniq_ignore_case() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/case.txt".into()));
        args.flags.insert("i".to_string());

        let result = Uniq.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        // First occurrence is kept when ignoring case
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0], "Hello");
        assert_eq!(lines[1], "world");
    }

    #[tokio::test]
    async fn test_uniq_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("one\none\ntwo\nthree\nthree\n".to_string());

        let args = ToolArgs::new();
        let result = Uniq.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["one", "two", "three"]);
    }

    // --- Flag combinations and edge cases ---

    #[tokio::test]
    async fn test_uniq_count_repeated() {
        // uniq -cd (count + only repeated)
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/dup.txt".into()));
        args.flags.insert("c".to_string());
        args.flags.insert("d".to_string());

        let result = Uniq.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Only lines that appear more than once, with counts
        assert!(result.text_out().contains("2 apple"));
        assert!(result.text_out().contains("3 cherry"));
        assert!(!result.text_out().contains("banana"));
    }

    #[tokio::test]
    async fn test_uniq_count_ignore_case() {
        // uniq -ci (count + ignore case)
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/case.txt".into()));
        args.flags.insert("c".to_string());
        args.flags.insert("i".to_string());

        let result = Uniq.execute(args, &mut ctx).await;
        assert!(result.ok());
        // "Hello", "hello", "HELLO" should be counted as 3
        assert!(result.text_out().contains("3 Hello"));
    }

    #[tokio::test]
    async fn test_uniq_unicode() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("日本\n日本\n中国\n日本\n".to_string());

        let args = ToolArgs::new();
        let result = Uniq.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        // Adjacent duplicates collapsed, but non-adjacent remain
        assert_eq!(lines, vec!["日本", "中国", "日本"]);
    }

    #[tokio::test]
    async fn test_uniq_empty_input() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let args = ToolArgs::new();
        let result = Uniq.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().is_empty());
    }

    #[tokio::test]
    async fn test_uniq_single_line() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("only one".to_string());

        let args = ToolArgs::new();
        let result = Uniq.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "only one");
    }

    #[tokio::test]
    async fn test_uniq_all_same() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("same\nsame\nsame\nsame\n".to_string());

        let mut args = ToolArgs::new();
        args.flags.insert("c".to_string());

        let result = Uniq.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("4 same"));
    }

    #[tokio::test]
    async fn test_uniq_all_different() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("a\nb\nc\nd\n".to_string());

        let mut args = ToolArgs::new();
        args.flags.insert("d".to_string());

        let result = Uniq.execute(args, &mut ctx).await;
        assert!(result.ok());
        // No repeated lines, so output should be empty
        assert!(result.text_out().is_empty());
    }

    #[tokio::test]
    async fn test_uniq_empty_lines() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("\n\na\n\n\nb\n".to_string());

        let args = ToolArgs::new();
        let result = Uniq.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        // Adjacent empty lines collapsed
        assert_eq!(lines, vec!["", "a", "", "b"]);
    }
}
