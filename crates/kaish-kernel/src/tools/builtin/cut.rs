//! cut — Remove sections from each line of files.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Cut tool: select portions of each line.
pub struct Cut;

/// clap-derived argv layer for cut. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "cut", about = "Remove sections from each line")]
struct CutArgs {
    /// Field delimiter (-d)
    #[arg(short = 'd', long = "delimiter")]
    delimiter: Option<String>,

    /// Select fields by number, e.g. '1,3' or '1-3' (-f)
    #[arg(short = 'f', long = "fields")]
    fields: Option<String>,

    /// Select characters by position (-c)
    #[arg(short = 'c', long = "characters")]
    characters: Option<String>,

    /// Suppress lines with no delimiter character (-s)
    #[arg(short = 's', long = "only-delimited")]
    only_delimited: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Files to read; reads stdin when none are given.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Cut {
    fn name(&self) -> &str {
        "cut"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &CutArgs::command(),
            "cut",
            "Remove sections from each line",
            [
                ("Extract first field (CSV)", "cut -d ',' -f 1 data.csv"),
                // A comma-separated field list must be quoted — an unquoted
                // comma is reserved and splits the word.
                ("Extract fields 1 and 3", "cut -d: -f \"1,3\" /etc/passwd"),
                ("Extract characters 1-10", "cut -c 1-10 file.txt"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let parsed = match CutArgs::try_parse_from(
            std::iter::once("cut".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("cut: {e}")),
        };
        parsed.global.apply(ctx);

        // POSIX: cut accepts multiple files and concatenates their content.
        // When no file is given, read stdin.
        let input = if args.positional.is_empty() {
            match ctx.read_stdin_to_text().await {
                Ok(s) => s.unwrap_or_default(),
                Err(e) => return ExecResult::failure(2, format!("cut: {e}")),
            }
        } else {
            let mut acc = String::new();
            for value in &args.positional {
                let path = crate::interpreter::value_to_string(value);
                let resolved = ctx.resolve_path(&path);
                match ctx.backend.read(Path::new(&resolved), None).await {
                    Ok(data) => match String::from_utf8(data) {
                        Ok(s) => acc.push_str(&s),
                        Err(_) => return ExecResult::failure(1, format!("cut: {}: invalid UTF-8", path)),
                    },
                    Err(e) => return ExecResult::failure(1, format!("cut: {}: {}", path, e)),
                }
            }
            acc
        };

        let delimiter = parsed.delimiter.clone()
            .or_else(|| args.get_string("delimiter", usize::MAX))
            .or_else(|| args.get_string("d", usize::MAX))
            .unwrap_or_else(|| "\t".to_string());

        let fields = parsed.fields.clone()
            .or_else(|| args.get_string("fields", usize::MAX))
            .or_else(|| args.get_string("f", usize::MAX));

        let characters = parsed.characters.clone()
            .or_else(|| args.get_string("characters", usize::MAX))
            .or_else(|| args.get_string("c", usize::MAX));

        if delimiter.chars().count() > 1 {
            return ExecResult::failure(1, "cut: delimiter must be a single character");
        }

        if fields.is_none() && characters.is_none() {
            return ExecResult::failure(1, "cut: must specify either -f or -c");
        }

        let mut output = Vec::new();

        for line in input.lines() {
            if let Some(ref char_spec) = characters {
                // Character mode
                let chars: Vec<char> = line.chars().collect();
                let selected = select_indices(char_spec, chars.len());
                let result: String = selected
                    .iter()
                    .filter_map(|&i| chars.get(i).copied())
                    .collect();
                output.push(result);
            } else if let Some(ref field_spec) = fields {
                // Field mode
                let delim_char = delimiter.chars().next().unwrap_or('\t');
                // A line without the delimiter has no fields to cut. GNU passes
                // it through unchanged unless `-s` (only-delimited) is set, which
                // suppresses it. Emitting an empty line (the old behavior) was
                // silent data loss.
                if !line.contains(delim_char) {
                    if !parsed.only_delimited {
                        output.push(line.to_string());
                    }
                    continue;
                }
                let parts: Vec<&str> = line.split(delim_char).collect();
                let selected = select_indices(field_spec, parts.len());
                let result: Vec<&str> = selected
                    .iter()
                    .filter_map(|&i| parts.get(i).copied())
                    .collect();
                output.push(result.join(&delimiter));
            }
        }

        if output.is_empty() {
            ExecResult::success("")
        } else {
            // Populate .data so `for v in $(cut …)` iterates per output line.
            // cut reads input via `.lines()` so each line is newline-clean —
            // safe to treat as a true N-item list without word-splitting risk.
            let text = format!("{}\n", output.join("\n"));
            let data = Value::Json(serde_json::Value::Array(
                output.into_iter().map(serde_json::Value::String).collect(),
            ));
            ExecResult::success_with_data(text, data)
        }
    }
}

/// Parse a field/character specification like "1,3,5" or "1-3" or "2-"
/// Returns 0-indexed positions.
fn select_indices(spec: &str, max_len: usize) -> Vec<usize> {
    let mut indices = Vec::new();

    for part in spec.split(',') {
        let part = part.trim();
        if part.contains('-') {
            // Range specification
            let parts: Vec<&str> = part.split('-').collect();
            let start = parts
                .first()
                .and_then(|s| s.parse::<usize>().ok())
                .unwrap_or(1);
            let end = parts
                .get(1)
                .and_then(|s| {
                    if s.is_empty() {
                        Some(max_len)
                    } else {
                        s.parse::<usize>().ok()
                    }
                })
                .unwrap_or(max_len);

            for i in start..=end {
                if i > 0 && i <= max_len {
                    indices.push(i - 1); // Convert to 0-indexed
                }
            }
        } else if let Ok(n) = part.parse::<usize>()
            && n > 0 && n <= max_len {
                indices.push(n - 1); // Convert to 0-indexed
            }
    }

    indices
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("csv.txt"), b"alice,25,engineer\nbob,30,manager")
            .await
            .unwrap();
        mem.write(Path::new("tsv.txt"), b"one\ttwo\tthree\nfour\tfive\tsix")
            .await
            .unwrap();
        mem.write(Path::new("text.txt"), b"hello world\nfoo bar")
            .await
            .unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_cut_single_field() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/csv.txt".into()));
        args.named
            .insert("delimiter".to_string(), Value::String(",".into()));
        args.named
            .insert("fields".to_string(), Value::String("1".into()));

        let result = Cut.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["alice", "bob"]);
    }

    #[tokio::test]
    async fn test_cut_multiple_fields() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/csv.txt".into()));
        args.named
            .insert("delimiter".to_string(), Value::String(",".into()));
        args.named
            .insert("fields".to_string(), Value::String("1,3".into()));

        let result = Cut.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["alice,engineer", "bob,manager"]);
    }

    #[tokio::test]
    async fn test_cut_field_range() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/csv.txt".into()));
        args.named
            .insert("delimiter".to_string(), Value::String(",".into()));
        args.named
            .insert("fields".to_string(), Value::String("2-3".into()));

        let result = Cut.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["25,engineer", "30,manager"]);
    }

    #[tokio::test]
    async fn test_cut_tab_delimiter() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/tsv.txt".into()));
        args.named
            .insert("fields".to_string(), Value::String("2".into()));

        let result = Cut.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["two", "five"]);
    }

    #[tokio::test]
    async fn test_cut_characters() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/text.txt".into()));
        args.named
            .insert("characters".to_string(), Value::String("1-5".into()));

        let result = Cut.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["hello", "foo b"]);
    }

    #[tokio::test]
    async fn test_cut_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("a:b:c\n1:2:3\n".to_string());

        let mut args = ToolArgs::new();
        args.named
            .insert("delimiter".to_string(), Value::String(":".into()));
        args.named
            .insert("fields".to_string(), Value::String("2".into()));

        let result = Cut.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["b", "2"]);
    }

    #[tokio::test]
    async fn test_cut_missing_field_spec() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/csv.txt".into()));

        let result = Cut.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("-f or -c"));
    }

    #[test]
    fn test_select_indices() {
        assert_eq!(select_indices("1", 5), vec![0]);
        assert_eq!(select_indices("1,3", 5), vec![0, 2]);
        assert_eq!(select_indices("1-3", 5), vec![0, 1, 2]);
        assert_eq!(select_indices("2-", 5), vec![1, 2, 3, 4]);
        assert_eq!(select_indices("1,3-5", 5), vec![0, 2, 3, 4]);
    }

    // --- Additional tests for edge cases and common patterns ---

    #[tokio::test]
    async fn test_cut_open_range_from_start() {
        // cut -f -3 (fields 1-3)
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/csv.txt".into()));
        args.named
            .insert("delimiter".to_string(), Value::String(",".into()));
        args.named
            .insert("fields".to_string(), Value::String("-2".into()));

        let result = Cut.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["alice,25", "bob,30"]);
    }

    #[tokio::test]
    async fn test_cut_out_of_range_field() {
        // Requesting field beyond available columns should return empty for that field
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/csv.txt".into()));
        args.named
            .insert("delimiter".to_string(), Value::String(",".into()));
        args.named
            .insert("fields".to_string(), Value::String("5".into()));

        let result = Cut.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Fields beyond range are silently ignored
        assert!(result.text_out().trim().is_empty() || result.text_out().lines().all(|l| l.is_empty()));
    }

    #[tokio::test]
    async fn test_cut_unicode_characters() {
        // Unicode character cutting
        let mut ctx = make_ctx().await;
        ctx.set_stdin("日本語テスト\n".to_string());

        let mut args = ToolArgs::new();
        args.named
            .insert("characters".to_string(), Value::String("1-3".into()));

        let result = Cut.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "日本語");
    }

    #[tokio::test]
    async fn test_cut_empty_input() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let mut args = ToolArgs::new();
        args.named
            .insert("fields".to_string(), Value::String("1".into()));

        let result = Cut.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().is_empty());
    }

    #[tokio::test]
    async fn test_cut_single_column_csv() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("value1\nvalue2\nvalue3\n".to_string());

        let mut args = ToolArgs::new();
        args.named
            .insert("delimiter".to_string(), Value::String(",".into()));
        args.named
            .insert("fields".to_string(), Value::String("1".into()));

        let result = Cut.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["value1", "value2", "value3"]);
    }

    #[tokio::test]
    async fn test_cut_mixed_ranges_and_single() {
        // Common pattern: cut -f 1,3-5,7
        let mut ctx = make_ctx().await;
        ctx.set_stdin("a,b,c,d,e,f,g,h\n".to_string());

        let mut args = ToolArgs::new();
        args.named
            .insert("delimiter".to_string(), Value::String(",".into()));
        args.named
            .insert("fields".to_string(), Value::String("1,3-5,7".into()));

        let result = Cut.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "a,c,d,e,g");
    }

    #[tokio::test]
    async fn test_cut_multi_char_delimiter_error() {
        // Bug L: multi-character delimiter should error, not silently truncate
        let mut ctx = make_ctx().await;
        ctx.set_stdin("a::b::c\n".to_string());

        let mut args = ToolArgs::new();
        args.named
            .insert("delimiter".to_string(), Value::String("::".into()));
        args.named
            .insert("fields".to_string(), Value::String("1".into()));

        let result = Cut.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("single character"));
    }

    /// POSIX: `cut FILE1 FILE2` reads from both files in order and applies
    /// the same field/character selection to the concatenated input.
    #[tokio::test]
    async fn test_cut_multiple_files_concatenate() {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("a.csv"), b"alice,25\nbob,30\n").await.unwrap();
        mem.write(Path::new("b.csv"), b"carol,40\n").await.unwrap();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/a.csv".into()));
        args.positional.push(Value::String("/b.csv".into()));
        args.named
            .insert("delimiter".to_string(), Value::String(",".into()));
        args.named
            .insert("fields".to_string(), Value::String("1".into()));

        let result = Cut.execute(args, &mut ctx).await;
        assert!(result.ok(), "cut multi-file should succeed: {}", result.err);
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["alice", "bob", "carol"], "expected concat of both files");
    }
}
