//! grep — Search for patterns in files or stdin.

use async_trait::async_trait;
use regex::RegexBuilder;
use std::path::{Path, PathBuf};

use crate::ast::Value;
use crate::backend_walker_fs::BackendWalkerFs;
use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema, validate_against_schema};
use crate::validator::{IssueCode, ValidationIssue};
use crate::walker::{FileWalker, GlobPath, IncludeExclude, WalkOptions};

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
            ).with_aliases(["-i"]))
            .param(ParamSchema::optional(
                "line_number",
                "bool",
                Value::Bool(false),
                "Prefix output with line numbers (-n)",
            ).with_aliases(["-n"]))
            .param(ParamSchema::optional(
                "invert",
                "bool",
                Value::Bool(false),
                "Select non-matching lines (-v)",
            ).with_aliases(["-v"]))
            .param(ParamSchema::optional(
                "count",
                "bool",
                Value::Bool(false),
                "Only print count of matching lines (-c)",
            ).with_aliases(["-c"]))
            .param(ParamSchema::optional(
                "only_matching",
                "bool",
                Value::Bool(false),
                "Print only the matched parts (-o)",
            ).with_aliases(["-o"]))
            .param(ParamSchema::optional(
                "after_context",
                "int",
                Value::Null,
                "Print NUM lines after match (-A)",
            ).with_aliases(["-A"]))
            .param(ParamSchema::optional(
                "before_context",
                "int",
                Value::Null,
                "Print NUM lines before match (-B)",
            ).with_aliases(["-B"]))
            .param(ParamSchema::optional(
                "context",
                "int",
                Value::Null,
                "Print NUM lines before and after match (-C)",
            ).with_aliases(["-C"]))
            .param(ParamSchema::optional(
                "quiet",
                "bool",
                Value::Bool(false),
                "Quiet mode, only return exit code (-q)",
            ).with_aliases(["-q"]))
            .param(ParamSchema::optional(
                "files_with_matches",
                "bool",
                Value::Bool(false),
                "Print only filenames with matches (-l)",
            ).with_aliases(["-l"]))
            .param(ParamSchema::optional(
                "word_regexp",
                "bool",
                Value::Bool(false),
                "Match whole words only (-w)",
            ).with_aliases(["-w"]))
            .param(ParamSchema::optional(
                "recursive",
                "bool",
                Value::Bool(false),
                "Search directories recursively (-r)",
            ).with_aliases(["-r", "-R"]))
            .param(ParamSchema::optional(
                "include",
                "string",
                Value::Null,
                "Include only files matching pattern (--include)",
            ).with_aliases(["--include"]))
            .param(ParamSchema::optional(
                "exclude",
                "string",
                Value::Null,
                "Exclude files matching pattern (--exclude)",
            ).with_aliases(["--exclude"]))
            .example("Search for pattern in file", "grep pattern file.txt")
            .example("Case-insensitive search", "grep -i ERROR log.txt")
            .example("Show line numbers", "grep -n TODO *.rs")
            .example("Extract matched text only", "grep -o 'https://[^\"]*' file.html")
            .example("Context around matches", "grep -C 2 error log.txt")
            .example("Recursive search", "grep -r TODO src/")
            .example("With file filter", "grep -rn TODO . --include='*.rs'")
    }

    fn validate(&self, args: &ToolArgs) -> Vec<ValidationIssue> {
        let mut issues = validate_against_schema(args, &self.schema());

        // Check if the regex pattern is valid
        if let Some(pattern) = args.get_string("pattern", 0) {
            // Don't validate if pattern looks dynamic (contains shell expansion markers)
            if !pattern.contains("<dynamic>")
                && let Err(e) = regex::Regex::new(&pattern) {
                    issues.push(ValidationIssue::error(
                        IssueCode::InvalidRegex,
                        format!("grep: invalid regex pattern: {}", e),
                    ).with_suggestion("check regex syntax at https://docs.rs/regex"));
                }
        }

        issues
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
        let recursive = args.has_flag("recursive") || args.has_flag("r") || args.has_flag("R");

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

        let grep_opts = GrepOptions {
            show_line_numbers: line_number,
            invert,
            only_matching,
            before_context,
            after_context,
            show_filename: false, // Will be set for multi-file
        };

        // Handle recursive search
        if recursive {
            let path = args
                .get_string("path", 1)
                .unwrap_or_else(|| ".".to_string());
            let root = ctx.resolve_path(&path);

            // Build include/exclude filter
            let mut filter = IncludeExclude::new();
            if let Some(Value::String(inc)) = args.get("include", usize::MAX) {
                filter.include(inc);
            }
            if let Some(Value::String(exc)) = args.get("exclude", usize::MAX) {
                filter.exclude(exc);
            }

            // Build glob pattern if include is specified
            let glob = if let Some(Value::String(inc)) = args.get("include", usize::MAX) {
                GlobPath::new(&format!("**/{}", inc)).ok()
            } else {
                GlobPath::new("**/*").ok()
            };

            let options = WalkOptions {
                max_depth: None,
                entry_types: crate::walker::EntryTypes::files_only(),
                respect_gitignore: true,
                include_hidden: false,
                filter,
                ..WalkOptions::default()
            };

            let fs = BackendWalkerFs(ctx.backend.as_ref());
            let walker = if let Some(g) = glob {
                FileWalker::new(&fs, &root)
                    .with_pattern(g)
                    .with_options(options)
            } else {
                FileWalker::new(&fs, &root).with_options(options)
            };

            let files = match walker.collect().await {
                Ok(f) => f,
                Err(e) => return ExecResult::failure(1, format!("grep: {}", e)),
            };

            return self
                .grep_multiple_files(ctx, &files, &root, &regex, &grep_opts, quiet, files_only, count_only)
                .await;
        }

        // Streaming path: pipe_stdin → pipe_stdout, process line by line
        // Only for simple stdin grep (no context, no count, no quiet, no files-only, no only-matching)
        if args.get_string("path", 1).is_none()
            && ctx.pipe_stdin.is_some()
            && ctx.pipe_stdout.is_some()
            && !count_only && !quiet && !files_only && !only_matching
            && before_context.is_none() && after_context.is_none()
        {
            return self.stream_grep(ctx, &regex, invert, line_number).await;
        }

        // Single file or stdin search
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
            None => (ctx.read_stdin_to_string().await.unwrap_or_default(), None),
        };

        let (text_output, nodes, match_count) = grep_lines_structured(&input, &regex, &grep_opts, filename.as_deref());

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
            ExecResult::from_output(1, text_output, "")
        } else {
            // Return structured output with nodes
            let headers = if grep_opts.show_line_numbers {
                vec!["Match".to_string(), "Line".to_string()]
            } else {
                vec!["Match".to_string()]
            };
            let output = OutputData::table(headers, nodes);
            let mut result = ExecResult::with_output(output);
            // Override the canonical output with traditional grep format
            result.out = text_output;
            result
        }
    }
}

impl Grep {
    /// Stream grep: read lines from pipe_stdin, write matching lines to pipe_stdout.
    async fn stream_grep(
        &self,
        ctx: &mut ExecContext,
        regex: &regex::Regex,
        invert: bool,
        show_line_numbers: bool,
    ) -> ExecResult {
        use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};

        let pipe_in = ctx.pipe_stdin.take().unwrap();
        let mut reader = BufReader::new(pipe_in);
        let mut pipe_out = ctx.pipe_stdout.take().unwrap();
        let mut match_count = 0usize;
        let mut line_num = 0usize;

        let mut line_buf = String::new();
        loop {
            line_buf.clear();
            match reader.read_line(&mut line_buf).await {
                Ok(0) => break,
                Ok(_) => {
                    line_num += 1;
                    let matches = regex.is_match(line_buf.trim_end_matches('\n'));
                    let should_output = if invert { !matches } else { matches };
                    if should_output {
                        match_count += 1;
                        let output = if show_line_numbers {
                            format!("{}:{}", line_num, line_buf)
                        } else {
                            line_buf.clone()
                        };
                        if pipe_out.write_all(output.as_bytes()).await.is_err() {
                            break;
                        }
                        if !output.ends_with('\n') && pipe_out.write_all(b"\n").await.is_err() {
                            break;
                        }
                    }
                }
                Err(_) => break,
            }
        }

        drop(reader);
        let _ = pipe_out.shutdown().await;

        if match_count > 0 {
            ExecResult::success("")
        } else {
            ExecResult::from_output(1, String::new(), String::new())
        }
    }

    #[allow(clippy::too_many_arguments)]
    async fn grep_multiple_files(
        &self,
        ctx: &mut ExecContext,
        files: &[PathBuf],
        root: &Path,
        regex: &regex::Regex,
        base_opts: &GrepOptions,
        quiet: bool,
        files_only: bool,
        count_only: bool,
    ) -> ExecResult {
        let mut total_output = String::new();
        let mut total_nodes: Vec<OutputNode> = Vec::new();
        let mut total_matches = 0;
        let mut files_with_matches = Vec::new();

        let opts = GrepOptions {
            show_filename: true,
            ..*base_opts
        };

        for file_path in files {
            let content = match ctx.backend.read(file_path, None).await {
                Ok(data) => match String::from_utf8(data) {
                    Ok(s) => s,
                    Err(_) => continue, // Skip binary files
                },
                Err(_) => continue,
            };

            // Create relative filename for display
            let display_name = file_path
                .strip_prefix(root)
                .unwrap_or(file_path)
                .to_string_lossy()
                .to_string();

            let (output, nodes, match_count) = grep_lines_structured(&content, regex, &opts, Some(&display_name));

            if match_count > 0 {
                total_matches += match_count;
                files_with_matches.push(display_name.clone());

                if !quiet && !files_only && !count_only {
                    total_output.push_str(&output);
                    total_nodes.extend(nodes);
                }
            }
        }

        if quiet {
            return if total_matches > 0 {
                ExecResult::success("")
            } else {
                ExecResult::from_output(1, "", "")
            };
        }

        if files_only {
            return if files_with_matches.is_empty() {
                ExecResult::from_output(1, "", "")
            } else {
                ExecResult::success(files_with_matches.join("\n") + "\n")
            };
        }

        if count_only {
            ExecResult::success(format!("{}\n", total_matches))
        } else if total_matches == 0 {
            ExecResult::from_output(1, total_output, "")
        } else {
            // Return structured output
            let headers = if opts.show_line_numbers {
                vec!["Match".to_string(), "File".to_string(), "Line".to_string()]
            } else {
                vec!["Match".to_string(), "File".to_string()]
            };
            let output = OutputData::table(headers, total_nodes);
            let mut result = ExecResult::with_output(output);
            result.out = total_output;
            result
        }
    }
}

#[derive(Clone, Copy)]
struct GrepOptions {
    show_line_numbers: bool,
    invert: bool,
    show_filename: bool,
    only_matching: bool,
    before_context: Option<usize>,
    after_context: Option<usize>,
}

/// Search lines and return matching output, nodes, and count.
fn grep_lines_structured(
    input: &str,
    regex: &regex::Regex,
    opts: &GrepOptions,
    filename: Option<&str>,
) -> (String, Vec<OutputNode>, usize) {
    let lines: Vec<&str> = input.lines().collect();
    let mut output = String::new();
    let mut nodes: Vec<OutputNode> = Vec::new();
    let mut match_count = 0;
    let mut printed = vec![false; lines.len()];

    // Helper to format prefix for text output
    let prefix = |line_num: usize, sep: char| -> String {
        let mut p = String::new();
        if opts.show_filename
            && let Some(f) = filename {
                p.push_str(f);
                p.push(sep);
            }
        if opts.show_line_numbers {
            p.push_str(&format!("{}{}", line_num + 1, sep));
        }
        p
    };

    for (line_num, line) in lines.iter().enumerate() {
        let matches = regex.is_match(line);
        let should_match = if opts.invert { !matches } else { matches };

        if should_match {
            match_count += 1;

            // Handle context lines (text output only, not as nodes)
            if let Some(before) = opts.before_context {
                let start = line_num.saturating_sub(before);
                for ctx_line in start..line_num {
                    if !printed[ctx_line] {
                        output.push_str(&prefix(ctx_line, '-'));
                        output.push_str(lines[ctx_line]);
                        output.push('\n');
                        printed[ctx_line] = true;
                    }
                }
            }

            // Print the matching line
            if !printed[line_num] {
                if opts.only_matching && !opts.invert {
                    // Print only matched parts
                    for m in regex.find_iter(line) {
                        output.push_str(&prefix(line_num, ':'));
                        output.push_str(m.as_str());
                        output.push('\n');

                        // Create node for each match
                        let mut cells = Vec::new();
                        if opts.show_filename
                            && let Some(f) = filename {
                                cells.push(f.to_string());
                            }
                        if opts.show_line_numbers {
                            cells.push((line_num + 1).to_string());
                        }
                        nodes.push(OutputNode::new(m.as_str()).with_cells(cells));
                    }
                } else {
                    output.push_str(&prefix(line_num, ':'));
                    output.push_str(line);
                    output.push('\n');

                    // Create node for the match
                    let mut cells = Vec::new();
                    if opts.show_filename
                        && let Some(f) = filename {
                            cells.push(f.to_string());
                        }
                    if opts.show_line_numbers {
                        cells.push((line_num + 1).to_string());
                    }
                    nodes.push(OutputNode::new(*line).with_cells(cells));
                }
                printed[line_num] = true;
            }

            // Handle after context (text output only)
            if let Some(after) = opts.after_context {
                let end = (line_num + after + 1).min(lines.len());
                for ctx_line in (line_num + 1)..end {
                    if !printed[ctx_line] {
                        output.push_str(&prefix(ctx_line, '-'));
                        output.push_str(lines[ctx_line]);
                        output.push('\n');
                        printed[ctx_line] = true;
                    }
                }
            }
        }
    }

    (output, nodes, match_count)
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

    async fn make_recursive_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();

        mem.mkdir(Path::new("src")).await.unwrap();
        mem.mkdir(Path::new("src/lib")).await.unwrap();

        mem.write(Path::new("src/main.rs"), b"fn main() {\n    // TODO: implement\n}")
            .await
            .unwrap();
        mem.write(Path::new("src/lib.rs"), b"// TODO: add modules\npub mod lib;")
            .await
            .unwrap();
        mem.write(Path::new("src/lib/utils.rs"), b"pub fn util() {\n    // helper function\n}")
            .await
            .unwrap();
        mem.write(Path::new("README.md"), b"# Project\nTODO: write docs")
            .await
            .unwrap();

        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_grep_recursive() {
        let mut ctx = make_recursive_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("TODO".into()));
        args.positional.push(Value::String("/".into()));
        args.flags.insert("r".to_string());

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should find TODO in multiple files
        assert!(result.out.contains("TODO"));
        assert!(result.out.contains("main.rs"));
        assert!(result.out.contains("lib.rs"));
        assert!(result.out.contains("README.md"));
    }

    #[tokio::test]
    async fn test_grep_recursive_with_line_numbers() {
        let mut ctx = make_recursive_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("TODO".into()));
        args.positional.push(Value::String("/src".into()));
        args.flags.insert("r".to_string());
        args.flags.insert("n".to_string());

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should have filename:linenum:content format
        assert!(result.out.contains(":"));
    }

    #[tokio::test]
    async fn test_grep_recursive_include() {
        let mut ctx = make_recursive_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("TODO".into()));
        args.positional.push(Value::String("/".into()));
        args.flags.insert("r".to_string());
        args.named
            .insert("include".to_string(), Value::String("*.rs".into()));

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should find TODO in .rs files but not README.md
        assert!(result.out.contains("main.rs") || result.out.contains("lib.rs"));
        assert!(!result.out.contains("README.md"));
    }

    #[tokio::test]
    async fn test_grep_recursive_files_only() {
        let mut ctx = make_recursive_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("TODO".into()));
        args.positional.push(Value::String("/".into()));
        args.flags.insert("r".to_string());
        args.flags.insert("l".to_string());

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should only list filenames, not content
        let lines: Vec<&str> = result.out.lines().collect();
        assert!(lines.len() >= 2); // At least 2 files have TODO
        // Each line should be a filename, not contain ":"
        for line in &lines {
            assert!(!line.contains("TODO"), "Output should only contain filenames");
        }
    }

    #[tokio::test]
    async fn test_grep_recursive_uppercase_r() {
        // -R should work the same as -r (muscle memory compatibility)
        let mut ctx = make_recursive_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("TODO".into()));
        args.positional.push(Value::String("/src".into()));
        args.flags.insert("R".to_string());

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("TODO"));
        assert!(result.out.contains("main.rs") || result.out.contains("lib.rs"));
    }
}
