//! cut — Remove sections from each line of files.

use async_trait::async_trait;
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Cut tool: select portions of each line.
pub struct Cut;

#[async_trait]
impl Tool for Cut {
    fn name(&self) -> &str {
        "cut"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("cut", "Remove sections from each line")
            .param(ParamSchema::optional(
                "path",
                "string",
                Value::Null,
                "File to process (reads stdin if not provided)",
            ))
            .param(ParamSchema::optional(
                "delimiter",
                "string",
                Value::String("\t".into()),
                "Field delimiter (-d)",
            ).with_aliases(["-d"]))
            .param(ParamSchema::optional(
                "fields",
                "string",
                Value::Null,
                "Select fields by number, e.g. '1,3' or '1-3' (-f)",
            ).with_aliases(["-f"]))
            .param(ParamSchema::optional(
                "characters",
                "string",
                Value::Null,
                "Select characters by position (-c)",
            ).with_aliases(["-c"]))
            .example("Extract first field (CSV)", "cut -d ',' -f 1 data.csv")
            .example("Extract fields 1 and 3", "cut -d ':' -f 1,3 /etc/passwd")
            .example("Extract characters 1-10", "cut -c 1-10 file.txt")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // Get input: from file or stdin
        let input = match args.get_string("path", 0) {
            Some(path) => {
                let resolved = ctx.resolve_path(&path);
                match ctx.backend.read(Path::new(&resolved), None).await {
                    Ok(data) => match String::from_utf8(data) {
                        Ok(s) => s,
                        Err(_) => {
                            return ExecResult::failure(1, format!("cut: {}: invalid UTF-8", path))
                        }
                    },
                    Err(e) => return ExecResult::failure(1, format!("cut: {}: {}", path, e)),
                }
            }
            None => ctx.read_stdin_to_string().await.unwrap_or_default(),
        };

        let delimiter = args
            .get_string("delimiter", usize::MAX)
            .or_else(|| args.get_string("d", usize::MAX))
            .unwrap_or_else(|| "\t".to_string());

        let fields = args
            .get_string("fields", usize::MAX)
            .or_else(|| args.get_string("f", usize::MAX));

        let characters = args
            .get_string("characters", usize::MAX)
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
            ExecResult::with_output(OutputData::text(""))
        } else {
            ExecResult::with_output(OutputData::text(format!("{}\n", output.join("\n"))))
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
        let lines: Vec<&str> = result.out.lines().collect();
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
        let lines: Vec<&str> = result.out.lines().collect();
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
        let lines: Vec<&str> = result.out.lines().collect();
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
        let lines: Vec<&str> = result.out.lines().collect();
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
        let lines: Vec<&str> = result.out.lines().collect();
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
        let lines: Vec<&str> = result.out.lines().collect();
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
        let lines: Vec<&str> = result.out.lines().collect();
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
        assert!(result.out.trim().is_empty() || result.out.lines().all(|l| l.is_empty()));
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
        assert_eq!(result.out.trim(), "日本語");
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
        assert!(result.out.is_empty());
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
        let lines: Vec<&str> = result.out.lines().collect();
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
        assert_eq!(result.out.trim(), "a,c,d,e,g");
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

    #[tokio::test]
    async fn test_cut_single_file_only() {
        // 80/20 design: only first positional argument is used.
        // Extra file arguments are silently ignored.
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/csv.txt".into()));
        args.positional.push(Value::String("/tsv.txt".into())); // ignored
        args.named
            .insert("delimiter".to_string(), Value::String(",".into()));
        args.named
            .insert("fields".to_string(), Value::String("1".into()));

        let result = Cut.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should only read from first file (csv.txt has "alice,25,engineer" etc.)
        let lines: Vec<&str> = result.out.lines().collect();
        assert_eq!(lines, vec!["alice", "bob"]);
    }
}
