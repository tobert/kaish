//! grep — Search for patterns in files or stdin.

use async_trait::async_trait;
use regex::RegexBuilder;
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Grep tool: search for patterns in text.
pub struct Grep;

#[async_trait]
impl Tool for Grep {
    fn name(&self) -> &str {
        "grep"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("grep", "Search for patterns in files or stdin")
            .param(ParamSchema::required(
                "pattern",
                "string",
                "Regular expression pattern to search for",
            ))
            .param(ParamSchema::optional(
                "path",
                "string",
                Value::Null,
                "File to search (reads stdin if not provided)",
            ))
            .param(ParamSchema::optional(
                "ignore_case",
                "bool",
                Value::Bool(false),
                "Case-insensitive matching (-i)",
            ))
            .param(ParamSchema::optional(
                "line_number",
                "bool",
                Value::Bool(false),
                "Prefix output with line numbers (-n)",
            ))
            .param(ParamSchema::optional(
                "invert",
                "bool",
                Value::Bool(false),
                "Select non-matching lines (-v)",
            ))
            .param(ParamSchema::optional(
                "count",
                "bool",
                Value::Bool(false),
                "Only print count of matching lines (-c)",
            ))
            .param(ParamSchema::optional(
                "only_matching",
                "bool",
                Value::Bool(false),
                "Print only the matched parts (-o)",
            ))
            .param(ParamSchema::optional(
                "after_context",
                "int",
                Value::Null,
                "Print NUM lines after match (-A)",
            ))
            .param(ParamSchema::optional(
                "before_context",
                "int",
                Value::Null,
                "Print NUM lines before match (-B)",
            ))
            .param(ParamSchema::optional(
                "context",
                "int",
                Value::Null,
                "Print NUM lines before and after match (-C)",
            ))
            .param(ParamSchema::optional(
                "quiet",
                "bool",
                Value::Bool(false),
                "Quiet mode, only return exit code (-q)",
            ))
            .param(ParamSchema::optional(
                "files_with_matches",
                "bool",
                Value::Bool(false),
                "Print only filenames with matches (-l)",
            ))
            .param(ParamSchema::optional(
                "word_regexp",
                "bool",
                Value::Bool(false),
                "Match whole words only (-w)",
            ))
            .example("Search for pattern in file", "grep pattern file.txt")
            .example("Case-insensitive search", "grep -i ERROR log.txt")
            .example("Show line numbers", "grep -n TODO *.rs")
            .example("Extract matched text only", "grep -o 'https://[^\"]*' file.html")
            .example("Context around matches", "grep -C 2 error log.txt")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let pattern = match args.get_string("pattern", 0) {
            Some(p) => p,
            None => return ExecResult::failure(1, "grep: missing pattern argument"),
        };

        let ignore_case = args.has_flag("ignore_case") || args.has_flag("i");
        let line_number = args.has_flag("line_number") || args.has_flag("n");
        let invert = args.has_flag("invert") || args.has_flag("v");
        let count_only = args.has_flag("count") || args.has_flag("c");
        let only_matching = args.has_flag("only_matching") || args.has_flag("o");
        let quiet = args.has_flag("quiet") || args.has_flag("q");
        let files_only = args.has_flag("files_with_matches") || args.has_flag("l");
        let word_regexp = args.has_flag("word_regexp") || args.has_flag("w");

        // Get context values
        let context = args.get("context", usize::MAX).and_then(|v| match v {
            Value::Int(i) => Some(*i as usize),
            Value::String(s) => s.parse().ok(),
            _ => None,
        });

        let after_context = args
            .get("after_context", usize::MAX)
            .and_then(|v| match v {
                Value::Int(i) => Some(*i as usize),
                Value::String(s) => s.parse().ok(),
                _ => None,
            })
            .or(context);

        let before_context = args
            .get("before_context", usize::MAX)
            .and_then(|v| match v {
                Value::Int(i) => Some(*i as usize),
                Value::String(s) => s.parse().ok(),
                _ => None,
            })
            .or(context);

        // Modify pattern for word boundary matching
        let final_pattern = if word_regexp {
            format!(r"\b{}\b", pattern)
        } else {
            pattern
        };

        // Build regex
        let regex = match RegexBuilder::new(&final_pattern)
            .case_insensitive(ignore_case)
            .build()
        {
            Ok(r) => r,
            Err(e) => return ExecResult::failure(1, format!("grep: invalid pattern: {}", e)),
        };

        // Get input: from file or stdin
        let (input, filename) = match args.get_string("path", 1) {
            Some(path) => {
                let resolved = ctx.resolve_path(&path);
                match ctx.backend.read(Path::new(&resolved), None).await {
                    Ok(data) => match String::from_utf8(data) {
                        Ok(s) => (s, Some(path)),
                        Err(_) => {
                            return ExecResult::failure(1, format!("grep: {}: invalid UTF-8", path))
                        }
                    },
                    Err(e) => return ExecResult::failure(1, format!("grep: {}: {}", path, e)),
                }
            }
            None => (ctx.take_stdin().unwrap_or_default(), None),
        };

        let opts = GrepOptions {
            show_line_numbers: line_number,
            invert,
            only_matching,
            before_context,
            after_context,
        };

        let (output, match_count) = grep_lines(&input, &regex, &opts);

        // Quiet mode: just return exit code
        if quiet {
            return if match_count > 0 {
                ExecResult::success("")
            } else {
                ExecResult::from_output(1, "", "")
            };
        }

        // Files with matches mode
        if files_only {
            return if match_count > 0 {
                if let Some(name) = filename {
                    ExecResult::success(format!("{}\n", name))
                } else {
                    ExecResult::success("(stdin)\n".to_string())
                }
            } else {
                ExecResult::from_output(1, "", "")
            };
        }

        if count_only {
            ExecResult::success(format!("{}\n", match_count))
        } else if match_count == 0 {
            ExecResult::from_output(1, output, "")
        } else {
            ExecResult::success(output)
        }
    }
}

struct GrepOptions {
    show_line_numbers: bool,
    invert: bool,
    only_matching: bool,
    before_context: Option<usize>,
    after_context: Option<usize>,
}

/// Search lines and return matching output and count.
fn grep_lines(input: &str, regex: &regex::Regex, opts: &GrepOptions) -> (String, usize) {
    let lines: Vec<&str> = input.lines().collect();
    let mut output = String::new();
    let mut match_count = 0;
    let mut printed = vec![false; lines.len()];

    for (line_num, line) in lines.iter().enumerate() {
        let matches = regex.is_match(line);
        let should_match = if opts.invert { !matches } else { matches };

        if should_match {
            match_count += 1;

            // Handle context lines
            if let Some(before) = opts.before_context {
                let start = line_num.saturating_sub(before);
                for ctx_line in start..line_num {
                    if !printed[ctx_line] {
                        if opts.show_line_numbers {
                            output.push_str(&format!("{}-{}\n", ctx_line + 1, lines[ctx_line]));
                        } else {
                            output.push_str(lines[ctx_line]);
                            output.push('\n');
                        }
                        printed[ctx_line] = true;
                    }
                }
            }

            // Print the matching line
            if !printed[line_num] {
                if opts.only_matching && !opts.invert {
                    // Print only matched parts
                    for m in regex.find_iter(line) {
                        if opts.show_line_numbers {
                            output.push_str(&format!("{}:{}\n", line_num + 1, m.as_str()));
                        } else {
                            output.push_str(m.as_str());
                            output.push('\n');
                        }
                    }
                } else if opts.show_line_numbers {
                    output.push_str(&format!("{}:{}\n", line_num + 1, line));
                } else {
                    output.push_str(line);
                    output.push('\n');
                }
                printed[line_num] = true;
            }

            // Handle after context
            if let Some(after) = opts.after_context {
                let end = (line_num + after + 1).min(lines.len());
                for ctx_line in (line_num + 1)..end {
                    if !printed[ctx_line] {
                        if opts.show_line_numbers {
                            output.push_str(&format!("{}-{}\n", ctx_line + 1, lines[ctx_line]));
                        } else {
                            output.push_str(lines[ctx_line]);
                            output.push('\n');
                        }
                        printed[ctx_line] = true;
                    }
                }
            }
        }
    }

    (output, match_count)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("test.txt"), b"hello world\nHELLO WORLD\nfoo bar\nbaz")
            .await
            .unwrap();
        mem.write(Path::new("lines.txt"), b"line one\nline two\nline three\nfour")
            .await
            .unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_grep_file() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello".into()));
        args.positional.push(Value::String("/test.txt".into()));

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "hello world\n");
    }

    #[tokio::test]
    async fn test_grep_case_insensitive() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello".into()));
        args.positional.push(Value::String("/test.txt".into()));
        args.flags.insert("i".to_string());

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("hello world"));
        assert!(result.out.contains("HELLO WORLD"));
    }

    #[tokio::test]
    async fn test_grep_line_numbers() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("line".into()));
        args.positional.push(Value::String("/lines.txt".into()));
        args.flags.insert("n".to_string());

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("1:line one"));
        assert!(result.out.contains("2:line two"));
        assert!(result.out.contains("3:line three"));
    }

    #[tokio::test]
    async fn test_grep_invert() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("line".into()));
        args.positional.push(Value::String("/lines.txt".into()));
        args.flags.insert("v".to_string());

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "four\n");
    }

    #[tokio::test]
    async fn test_grep_count() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("line".into()));
        args.positional.push(Value::String("/lines.txt".into()));
        args.flags.insert("c".to_string());

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "3\n");
    }

    #[tokio::test]
    async fn test_grep_no_match() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("xyz".into()));
        args.positional.push(Value::String("/test.txt".into()));

        let result = Grep.execute(args, &mut ctx).await;
        // Exit code 1 for no matches, but not an error
        assert!(!result.ok());
        assert!(result.err.is_empty());
        assert_eq!(result.code, 1);
    }

    #[tokio::test]
    async fn test_grep_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("apple\nbanana\napricot\n".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("ap".into()));

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("apple"));
        assert!(result.out.contains("apricot"));
        assert!(!result.out.contains("banana"));
    }

    #[tokio::test]
    async fn test_grep_regex() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("^line".into())); // Start of line
        args.positional.push(Value::String("/lines.txt".into()));

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("line one"));
        assert!(!result.out.contains("four")); // "four" doesn't start with "line"
    }

    #[tokio::test]
    async fn test_grep_invalid_regex() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("[invalid".into()));
        args.positional.push(Value::String("/test.txt".into()));

        let result = Grep.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("invalid pattern"));
    }

    #[tokio::test]
    async fn test_grep_missing_pattern() {
        let mut ctx = make_ctx().await;
        let result = Grep.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("pattern"));
    }

    #[tokio::test]
    async fn test_grep_file_not_found() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("pattern".into()));
        args.positional.push(Value::String("/nonexistent".into()));

        let result = Grep.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_grep_only_matching() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hello world hello\nfoo bar\n".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello".into()));
        args.flags.insert("o".to_string());

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should output "hello" twice (two matches on first line)
        let lines: Vec<&str> = result.out.lines().collect();
        assert_eq!(lines.len(), 2);
        assert_eq!(lines[0], "hello");
        assert_eq!(lines[1], "hello");
    }

    #[tokio::test]
    async fn test_grep_quiet_match() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello".into()));
        args.positional.push(Value::String("/test.txt".into()));
        args.flags.insert("q".to_string());

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.is_empty());
    }

    #[tokio::test]
    async fn test_grep_quiet_no_match() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("xyz".into()));
        args.positional.push(Value::String("/test.txt".into()));
        args.flags.insert("q".to_string());

        let result = Grep.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert_eq!(result.code, 1);
    }

    #[tokio::test]
    async fn test_grep_word_regexp() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("foobar\nfoo bar\nbarfoo\n".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("foo".into()));
        args.flags.insert("w".to_string());

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Only "foo bar" matches (foo as whole word)
        assert_eq!(result.out, "foo bar\n");
    }

    #[tokio::test]
    async fn test_grep_files_with_matches() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello".into()));
        args.positional.push(Value::String("/test.txt".into()));
        args.flags.insert("l".to_string());

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "/test.txt");
    }

    #[tokio::test]
    async fn test_grep_context() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("two".into()));
        args.positional.push(Value::String("/lines.txt".into()));
        args.named.insert("context".to_string(), Value::Int(1));

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should include lines before and after "line two"
        assert!(result.out.contains("line one"));
        assert!(result.out.contains("line two"));
        assert!(result.out.contains("line three"));
    }
}
