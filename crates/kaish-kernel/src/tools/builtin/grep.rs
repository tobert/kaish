//! grep — Search for patterns in files or stdin.
//!
//! Uses ripgrep's `grep-searcher` + `grep-regex` libraries as the
//! underlying engine: binary detection, encoding sniffing, multiline
//! support, and correct context-break handling all come from there. The
//! kaish builtin keeps its existing schema and exit-code semantics.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use grep_regex::{RegexMatcher, RegexMatcherBuilder};
use grep_searcher::{BinaryDetection, Encoding, SearcherBuilder};
use regex::RegexBuilder;
use std::path::{Path, PathBuf};

use crate::ast::Value;
use crate::backend_walker_fs::BackendWalkerFs;
use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::builtin::grep_engine::{AccumulatorSink, ContextKind, SearchEvent};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema, validate_against_schema};
use crate::validator::{IssueCode, ValidationIssue};
use crate::walker::{FileWalker, GlobPath, IncludeExclude, WalkOptions};

/// Grep tool: search for patterns in text.
pub struct Grep;

/// clap-derived argv layer for grep. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "grep", about = "Search for patterns in files or stdin")]
struct GrepArgs {
    /// Case-insensitive matching.
    #[arg(short = 'i', long = "ignore-case", visible_alias = "ignore_case")]
    ignore_case: bool,

    /// Prefix output with line numbers.
    #[arg(short = 'n', long = "line-number", visible_alias = "line_number")]
    line_number: bool,

    /// Select non-matching lines.
    #[arg(short = 'v', long = "invert")]
    invert: bool,

    /// Only print count of matching lines.
    #[arg(short = 'c', long = "count")]
    count: bool,

    /// Print only the matched parts.
    #[arg(short = 'o', long = "only-matching", visible_alias = "only_matching")]
    only_matching: bool,

    /// Quiet mode, only return exit code.
    #[arg(short = 'q', long = "quiet")]
    quiet: bool,

    /// Print only filenames with matches.
    #[arg(short = 'l', long = "files-with-matches", visible_alias = "files_with_matches")]
    files_with_matches: bool,

    /// Match whole words only.
    #[arg(short = 'w', long = "word-regexp", visible_alias = "word_regexp")]
    word_regexp: bool,

    /// Search directories recursively (lowercase).
    #[arg(short = 'r', long = "recursive")]
    recursive: bool,

    /// Search directories recursively (uppercase, muscle memory alias).
    #[arg(short = 'R')]
    recursive_upper: bool,

    /// Allow patterns to match across line boundaries.
    #[arg(short = 'U', long = "multiline")]
    multiline: bool,

    /// Extended regex (POSIX -E). No-op: Rust's regex crate is always
    /// extended (alternation, grouping, quantifiers without backslash);
    /// accepted for POSIX/muscle-memory compatibility.
    #[arg(id = "extended_regexp", short = 'E', long = "extended-regexp", visible_alias = "extended_regexp")]
    _extended: bool,

    /// Fixed strings (POSIX -F): treat pattern as a literal string, not a
    /// regex. Implemented by escaping every metachar via `regex::escape`.
    #[arg(id = "fixed_strings", short = 'F', long = "fixed-strings", visible_alias = "fixed_strings")]
    _fixed: bool,

    /// Print NUM lines after match.
    #[arg(short = 'A', long = "after-context", visible_alias = "after_context")]
    after_context: Option<String>,

    /// Print NUM lines before match.
    #[arg(short = 'B', long = "before-context", visible_alias = "before_context")]
    before_context: Option<String>,

    /// Print NUM lines before and after match.
    #[arg(short = 'C', long = "context")]
    context: Option<String>,

    /// Include only files matching pattern.
    #[arg(long = "include")]
    include: Option<String>,

    /// Exclude files matching pattern.
    #[arg(long = "exclude")]
    exclude: Option<String>,

    /// Force a specific text encoding.
    #[arg(long = "encoding")]
    encoding: Option<String>,

    /// Binary handling: quit (default), text, without-match.
    #[arg(long = "binary")]
    binary: Option<String>,

    #[command(flatten)]
    global: GlobalFlags,

    /// Pattern to search for, followed by optional file paths.
    pattern: Vec<String>,
}

#[async_trait]
impl Tool for Grep {
    fn name(&self) -> &str {
        "grep"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &GrepArgs::command(),
            "grep",
            "Search for patterns in files or stdin",
            [
                ("Search for pattern in file", "grep pattern file.txt"),
                ("Case-insensitive search", "grep -i ERROR log.txt"),
                ("Show line numbers", "grep -n TODO *.rs"),
                ("Extract matched text only", "grep -o 'https://[^\"]*' file.html"),
                ("Context around matches", "grep -C 2 error log.txt"),
                ("Recursive search", "grep -r TODO src/"),
                ("With file filter", "grep -rn TODO . --include='*.rs'"),
            ],
        )
    }

    fn validate(&self, args: &ToolArgs) -> Vec<ValidationIssue> {
        let mut issues = validate_against_schema(args, &self.schema());

        // Skip regex syntax check when -F is set: pattern will be escaped at runtime.
        let fixed = args.has_flag("F") || args.has_flag("fixed-strings");
        if !fixed && let Some(pattern) = args.get_string("pattern", 0) {
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

    async fn execute(&self, mut args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        args.flagify_bool_named();

        let parsed = match GrepArgs::try_parse_from(
            std::iter::once("grep".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("grep: {e}")),
        };
        parsed.global.apply(ctx);

        let pattern = match args.get_string("pattern", 0) {
            Some(p) => p,
            None => return ExecResult::failure(1, "grep: missing pattern argument"),
        };

        let ignore_case = args.has_flag("ignore-case") || args.has_flag("i");
        let line_number = args.has_flag("line-number") || args.has_flag("n");
        let invert = args.has_flag("invert") || args.has_flag("v");
        let count_only = args.has_flag("count") || args.has_flag("c");
        let only_matching = args.has_flag("only-matching") || args.has_flag("o");
        let quiet = args.has_flag("quiet") || args.has_flag("q");
        let files_only = args.has_flag("files-with-matches") || args.has_flag("l");
        let word_regexp = args.has_flag("word-regexp") || args.has_flag("w");
        let recursive = args.has_flag("recursive") || args.has_flag("r") || args.has_flag("R");
        let fixed_strings = args.has_flag("F") || args.has_flag("fixed-strings");

        // Context values come from the clap layer: the kernel binds `-A 5` /
        // `--after-context=5` under the kebab long-flag name, so re-reading
        // the raw named map by snake_case id silently dropped them (bug the
        // kernel-routed realworld port exposed 2026-06-11). A non-numeric
        // value is a loud error, not a silently ignored flag.
        fn parse_context(name: &str, value: &Option<String>) -> Result<Option<usize>, String> {
            match value {
                None => Ok(None),
                Some(s) => s.parse::<usize>().map(Some).map_err(|_| {
                    format!("grep: invalid {name} value {s:?} (expected a non-negative number)")
                }),
            }
        }
        let context = match parse_context("--context", &parsed.context) {
            Ok(c) => c,
            Err(e) => return ExecResult::failure(2, e),
        };
        let after_context = match parse_context("--after-context", &parsed.after_context) {
            Ok(c) => c.or(context),
            Err(e) => return ExecResult::failure(2, e),
        };
        let before_context = match parse_context("--before-context", &parsed.before_context) {
            Ok(c) => c.or(context),
            Err(e) => return ExecResult::failure(2, e),
        };

        let multiline = args.has_flag("multiline") || args.has_flag("U");
        let encoding = args.get_string("encoding", usize::MAX);
        let binary_mode = args
            .get_string("binary", usize::MAX)
            .unwrap_or_else(|| "quit".into());
        let binary_detection = match binary_mode.as_str() {
            "none" | "text" => BinaryDetection::none(),
            "without-match" => BinaryDetection::convert(b'\x00'),
            // Default: quit on first null byte (skips most binary content
            // gracefully; matches the legacy "skip non-UTF-8" intent).
            _ => BinaryDetection::quit(b'\x00'),
        };

        // -F: escape regex metachars so the pattern matches literally.
        // -w wraps in word boundaries (regex syntax) AFTER escaping so
        // `grep -Fw "192.168.1.1"` still anchors at word boundaries.
        let escaped = if fixed_strings { regex::escape(&pattern) } else { pattern };
        let final_pattern = if word_regexp {
            format!(r"\b{}\b", escaped)
        } else {
            escaped
        };

        // `regex::Regex` is still used by the streaming-stdin fast path
        // (line-by-line writes through pipe_stdout) where the full Searcher
        // machinery is overkill.
        let regex = match RegexBuilder::new(&final_pattern)
            .case_insensitive(ignore_case)
            .multi_line(multiline)
            .build()
        {
            Ok(r) => r,
            Err(e) => return ExecResult::failure(1, format!("grep: invalid pattern: {}", e)),
        };

        // RegexMatcher drives the Searcher; same pattern, same flags.
        let matcher = match RegexMatcherBuilder::new()
            .case_insensitive(ignore_case)
            .multi_line(multiline)
            .build(&final_pattern)
        {
            Ok(m) => m,
            Err(e) => return ExecResult::failure(1, format!("grep: invalid pattern: {}", e)),
        };

        let grep_opts = GrepOptions {
            show_line_numbers: line_number,
            invert,
            only_matching,
            before_context,
            after_context,
            show_filename: false, // Will be set for multi-file
            multiline,
            encoding: encoding.clone(),
            binary_detection,
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
                respect_gitignore: ctx.ignore_config.auto_gitignore(),
                include_hidden: false,
                filter,
                ..WalkOptions::default()
            };

            let fs = BackendWalkerFs(ctx.backend.as_ref());
            let mut walker = if let Some(g) = glob {
                FileWalker::new(&fs, &root)
                    .with_pattern(g)
                    .with_options(options)
            } else {
                FileWalker::new(&fs, &root).with_options(options)
            };

            // Inject ignore filter from config
            if let Some(ignore_filter) = ctx.build_ignore_filter(&root).await {
                walker = walker.with_ignore(ignore_filter);
            }

            let files = match walker.collect().await {
                Ok(f) => f,
                Err(e) => return ExecResult::failure(1, format!("grep: {}", e)),
            };

            return self
                .grep_multiple_files(ctx, &files, &root, &matcher, &grep_opts, quiet, files_only, count_only)
                .await;
        }

        // Explicit multiple file operands: positional[1..]. The kernel
        // pre-expands a bare glob into one positional per match, so
        // `grep p *.rs` and `grep p a b c` both arrive as several file args.
        // Search ALL of them (the old single `get_string("path", 1)` searched
        // only the first and silently ignored the rest), reusing the
        // filename-prefixing multi-file renderer.
        let file_operands: Vec<String> = args
            .positional
            .iter()
            .skip(1)
            .map(crate::interpreter::value_to_string)
            .collect();
        if file_operands.len() > 1 {
            let root = ctx.resolve_path(".");
            let resolved: Vec<PathBuf> = file_operands
                .iter()
                .map(|f| ctx.resolve_path(f))
                .collect();
            return self
                .grep_multiple_files(
                    ctx, &resolved, &root, &matcher, &grep_opts, quiet, files_only, count_only,
                )
                .await;
        }

        // Streaming path: pipe_stdin → pipe_stdout, process line by line
        // Only for simple stdin grep (no context, no count, no quiet, no files-only, no only-matching)
        let can_stream = args.get_string("path", 1).is_none()
            && !count_only && !quiet && !files_only && !only_matching
            && before_context.is_none() && after_context.is_none()
            && ctx.pipe_stdin.is_some() && ctx.pipe_stdout.is_some();
        if can_stream {
            // Both checked with is_some() above — take() cannot return None.
            if let (Some(pipe_stdin), Some(pipe_stdout)) =
                (ctx.pipe_stdin.take(), ctx.pipe_stdout.take())
            {
                return self.stream_grep(ctx, pipe_stdin, pipe_stdout, &regex, invert, line_number).await;
            }
        }

        // Single file or stdin search. The bytes are buffered up-front;
        // the searcher then runs synchronously inside `spawn_blocking`.
        let (bytes, filename) = match args.get_string("path", 1) {
            Some(path) => {
                let resolved = ctx.resolve_path(&path);
                match ctx.backend.read(Path::new(&resolved), None).await {
                    Ok(data) => (data, Some(path)),
                    Err(e) => return ExecResult::failure(1, format!("grep: {}: {}", path, e)),
                }
            }
            None => (
                ctx.read_stdin_to_string()
                    .await
                    .unwrap_or_default()
                    .into_bytes(),
                None,
            ),
        };

        let render = match grep_lines_structured(
            &bytes,
            &matcher,
            &grep_opts,
            filename.as_deref(),
        ) {
            Ok(t) => t,
            Err(e) => return ExecResult::failure(1, format!("grep: {e}")),
        };

        // Quiet mode: just return exit code
        if quiet {
            return if render.match_count > 0 {
                ExecResult::success("")
            } else {
                ExecResult::from_output(1, "", "")
            };
        }

        // Files with matches mode
        if files_only {
            return if render.match_count > 0 {
                if let Some(name) = filename {
                    ExecResult::with_output(OutputData::text(format!("{}\n", name)))
                } else {
                    ExecResult::with_output(OutputData::text("-\n".to_string()))
                }
            } else {
                ExecResult::from_output(1, "", "")
            };
        }

        if count_only {
            ExecResult::with_output(OutputData::text(format!("{}\n", render.match_count)))
        } else if render.match_count == 0 {
            ExecResult::from_output(1, render.text, "")
        } else {
            let headers = if grep_opts.show_line_numbers {
                vec!["MATCH".to_string(), "LINE".to_string()]
            } else {
                vec!["MATCH".to_string()]
            };
            let output = OutputData::table(headers, render.nodes)
                .with_rich_json(serde_json::Value::Array(render.rich));
            ExecResult::with_output_and_text(output, render.text)
        }
    }
}

impl Grep {
    /// Stream grep: read lines from pipe_stdin, write matching lines to pipe_stdout.
    async fn stream_grep(
        &self,
        _ctx: &mut ExecContext,
        pipe_in: crate::scheduler::PipeReader,
        mut pipe_out: crate::scheduler::PipeWriter,
        regex: &regex::Regex,
        invert: bool,
        show_line_numbers: bool,
    ) -> ExecResult {
        use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};

        let mut reader = BufReader::new(pipe_in);
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
        matcher: &RegexMatcher,
        base_opts: &GrepOptions,
        quiet: bool,
        files_only: bool,
        count_only: bool,
    ) -> ExecResult {
        let mut total_output = String::new();
        let mut total_nodes: Vec<OutputNode> = Vec::new();
        let mut total_rich: Vec<serde_json::Value> = Vec::new();
        let mut total_matches: usize = 0;
        let mut files_with_matches = Vec::new();

        let opts = GrepOptions {
            show_filename: true,
            ..base_opts.clone()
        };

        for file_path in files {
            let bytes = match ctx.backend.read(file_path, None).await {
                Ok(data) => data,
                Err(_) => continue,
            };

            // Create relative filename for display
            let display_name = file_path
                .strip_prefix(root)
                .unwrap_or(file_path)
                .to_string_lossy()
                .to_string();

            let render = match grep_lines_structured(&bytes, matcher, &opts, Some(&display_name)) {
                Ok(t) => t,
                Err(_) => continue,
            };

            if render.match_count > 0 {
                total_matches += render.match_count;
                files_with_matches.push(display_name.clone());

                if !quiet && !files_only && !count_only {
                    total_output.push_str(&render.text);
                    total_nodes.extend(render.nodes);
                    total_rich.extend(render.rich);
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
                ExecResult::with_output(OutputData::text(files_with_matches.join("\n") + "\n"))
            };
        }

        if count_only {
            ExecResult::with_output(OutputData::text(format!("{}\n", total_matches)))
        } else if total_matches == 0 {
            ExecResult::from_output(1, total_output, "")
        } else {
            // Return structured output
            let headers = if opts.show_line_numbers {
                vec!["MATCH".to_string(), "FILE".to_string(), "LINE".to_string()]
            } else {
                vec!["MATCH".to_string(), "FILE".to_string()]
            };
            let output = OutputData::table(headers, total_nodes)
                .with_rich_json(serde_json::Value::Array(total_rich));
            ExecResult::with_output_and_text(output, total_output)
        }
    }
}

#[derive(Clone)]
struct GrepOptions {
    show_line_numbers: bool,
    invert: bool,
    show_filename: bool,
    only_matching: bool,
    before_context: Option<usize>,
    after_context: Option<usize>,
    multiline: bool,
    /// Encoding label (`utf-16`, `latin-1`, …). `None` lets the searcher
    /// auto-detect via BOM sniffing.
    encoding: Option<String>,
    binary_detection: BinaryDetection,
}

/// Search bytes via grep-searcher and return the rendered output bundle.
/// Replaces the legacy line-by-line scanner.
///
/// Returns an error string when the searcher can't run (e.g. a bad
/// encoding label). Match-not-found is *not* an error — it returns an
/// empty `RenderResult` with `match_count == 0`.
fn grep_lines_structured(
    input: &[u8],
    matcher: &RegexMatcher,
    opts: &GrepOptions,
    filename: Option<&str>,
) -> Result<RenderResult, String> {
    // Build the searcher. line_number is always on so the renderer can
    // emit `LINE` cells when requested; `show_line_numbers` gates *display*,
    // not the underlying numbering.
    let mut sb = SearcherBuilder::new();
    sb.line_number(true)
        .multi_line(opts.multiline)
        .invert_match(opts.invert)
        .binary_detection(opts.binary_detection.clone());
    if let Some(before) = opts.before_context {
        sb.before_context(before);
    }
    if let Some(after) = opts.after_context {
        sb.after_context(after);
    }
    if let Some(enc_label) = opts.encoding.as_deref() {
        match Encoding::new(enc_label) {
            Ok(enc) => {
                sb.encoding(Some(enc));
            }
            Err(e) => return Err(format!("invalid encoding '{enc_label}': {e}")),
        }
    }
    let mut searcher = sb.build();

    let mut sink = AccumulatorSink::new(matcher, None);
    searcher
        .search_slice(matcher, input, &mut sink)
        .map_err(|e| e.to_string())?;
    let events = sink.into_events();

    Ok(render_events(&events, opts, filename))
}

/// Per-event render output: legacy text + table nodes for text mode, plus
/// a parallel array of rich JSON objects for `--json` consumers.
struct RenderResult {
    text: String,
    nodes: Vec<OutputNode>,
    rich: Vec<serde_json::Value>,
    match_count: usize,
}

/// Render a stream of search events into both the legacy table form and a
/// rich JSON shape (one object per matched line, with submatches and
/// byte offset).
fn render_events(events: &[SearchEvent], opts: &GrepOptions, filename: Option<&str>) -> RenderResult {
    let prefix = |line_num: u64, sep: char| -> String {
        let mut p = String::new();
        if opts.show_filename
            && let Some(f) = filename
        {
            p.push_str(f);
            p.push(sep);
        }
        if opts.show_line_numbers {
            p.push_str(&format!("{line_num}{sep}"));
        }
        p
    };

    let mut output = String::new();
    let mut nodes: Vec<OutputNode> = Vec::new();
    let mut rich: Vec<serde_json::Value> = Vec::new();
    let mut match_count: usize = 0;
    let mut emitted_any = false;

    for event in events {
        match event {
            SearchEvent::Match(m) => {
                let line_num = m.line_number.unwrap_or(0);
                if opts.only_matching && !opts.invert && !m.submatches.is_empty() {
                    for sub in &m.submatches {
                        output.push_str(&prefix(line_num, ':'));
                        output.push_str(&sub.text);
                        output.push('\n');

                        let mut cells = Vec::new();
                        if opts.show_filename
                            && let Some(f) = filename
                        {
                            cells.push(f.to_string());
                        }
                        if opts.show_line_numbers {
                            cells.push(line_num.to_string());
                        }
                        nodes.push(OutputNode::new(&sub.text).with_cells(cells));
                    }
                } else {
                    output.push_str(&prefix(line_num, ':'));
                    output.push_str(&m.line_text);
                    output.push('\n');

                    let mut cells = Vec::new();
                    if opts.show_filename
                        && let Some(f) = filename
                    {
                        cells.push(f.to_string());
                    }
                    if opts.show_line_numbers {
                        cells.push(line_num.to_string());
                    }
                    nodes.push(OutputNode::new(&m.line_text).with_cells(cells));
                }

                rich.push(match_record_to_json(m, filename));
                match_count += 1;
                emitted_any = true;
            }
            SearchEvent::Context(c) => {
                let sep = match c.kind {
                    ContextKind::Before | ContextKind::After | ContextKind::Other => '-',
                };
                let line_num = c.line_number.unwrap_or(0);
                output.push_str(&prefix(line_num, sep));
                output.push_str(&c.line_text);
                output.push('\n');
                emitted_any = true;
            }
            SearchEvent::ContextBreak => {
                if emitted_any {
                    output.push_str("--\n");
                }
            }
        }
    }

    RenderResult {
        text: output,
        nodes,
        rich,
        match_count,
    }
}

/// Build the rich JSON object for one matched line.
///
/// Shape (stable contract; tested in `tests::test_grep_json_rich_schema`):
/// ```json
/// {
///   "path": "src/main.rs",  // null when reading stdin
///   "line_number": 42,       // null when line numbering is off
///   "byte_offset": 1234,
///   "line_text": "...",
///   "submatches": [
///     { "text": "foo", "start": 0, "end": 3 }
///   ]
/// }
/// ```
fn match_record_to_json(
    m: &crate::tools::builtin::grep_engine::MatchRecord,
    fallback_path: Option<&str>,
) -> serde_json::Value {
    use serde_json::{Value, json};
    // Prefer the path the Sink captured; fall back to caller's filename
    // (this matters because the per-file driver currently doesn't tag
    // sinks with paths, so the renderer's filename is the source of truth).
    let path = m
        .path
        .as_ref()
        .map(|p| p.to_string_lossy().to_string())
        .or_else(|| fallback_path.map(|s| s.to_string()));
    let path_v = match path {
        Some(p) => Value::String(p),
        None => Value::Null,
    };
    let line_number_v = match m.line_number {
        Some(n) => Value::Number(n.into()),
        None => Value::Null,
    };
    let submatches: Vec<Value> = m
        .submatches
        .iter()
        .map(|s| {
            json!({
                "text": s.text,
                "start": s.start,
                "end": s.end,
            })
        })
        .collect();
    json!({
        "path": path_v,
        "line_number": line_number_v,
        "byte_offset": m.absolute_byte_offset,
        "line_text": m.line_text,
        "submatches": submatches,
    })
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
        assert_eq!(&*result.text_out(), "hello world\n");
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
        assert!(result.text_out().contains("hello world"));
        assert!(result.text_out().contains("HELLO WORLD"));
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
        assert!(result.text_out().contains("1:line one"));
        assert!(result.text_out().contains("2:line two"));
        assert!(result.text_out().contains("3:line three"));
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
        assert_eq!(&*result.text_out(), "four\n");
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
        assert_eq!(&*result.text_out(), "3\n");
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
        assert!(result.text_out().contains("apple"));
        assert!(result.text_out().contains("apricot"));
        assert!(!result.text_out().contains("banana"));
    }

    #[tokio::test]
    async fn test_grep_regex() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("^line".into())); // Start of line
        args.positional.push(Value::String("/lines.txt".into()));

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("line one"));
        assert!(!result.text_out().contains("four")); // "four" doesn't start with "line"
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
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
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
        assert!(result.text_out().is_empty());
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

    /// Multiline matching with -U: pattern with `(?s).` can span newlines.
    #[tokio::test]
    async fn test_grep_multiline_flag() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("foo line\nmiddle\nbar line\n".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("(?s)foo.*bar".into()));
        args.flags.insert("U".to_string());

        let result = Grep.execute(args, &mut ctx).await;
        assert!(
            result.ok(),
            "multiline grep failed: code={} err={}",
            result.code,
            result.err
        );
        assert!(
            result.text_out().contains("foo line"),
            "expected match crossing lines: {:?}",
            result.text_out().to_string(),
        );
    }

    /// Without -U the same pattern must NOT cross newlines (single-line
    /// regime is the default per existing behavior).
    #[tokio::test]
    async fn test_grep_no_multiline_by_default() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("foo line\nmiddle\nbar line\n".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("(?s)foo.*bar".into()));

        let result = Grep.execute(args, &mut ctx).await;
        // No matches across lines → exit 1.
        assert_eq!(result.code, 1);
    }

    /// Binary detection: a file with embedded NUL is skipped under the
    /// default `--binary=quit` mode (engine quits searching at the first
    /// null byte; subsequent `bar` doesn't match).
    #[tokio::test]
    async fn test_grep_binary_quit_default() {
        use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
        use std::sync::Arc;

        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        let mut bytes = b"foo\x00bar\n".to_vec();
        bytes.extend_from_slice(b"second line foo\n");
        mem.write(Path::new("bin.dat"), &bytes).await.unwrap();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("foo".into()));
        args.positional.push(Value::String("/bin.dat".into()));

        let result = Grep.execute(args, &mut ctx).await;
        // Search quits at the NUL — the second line's "foo" must NOT
        // appear in the rendered output.
        assert!(
            !result.text_out().contains("second line"),
            "binary quit should suppress post-NUL output, got: {:?}",
            result.text_out().to_string(),
        );
    }

    /// `--binary=text` keeps searching past the NUL.
    #[tokio::test]
    async fn test_grep_binary_text_searches_through() {
        use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
        use std::sync::Arc;

        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        let mut bytes = b"foo\x00bar\n".to_vec();
        bytes.extend_from_slice(b"after_null foo bar\n");
        mem.write(Path::new("bin.dat"), &bytes).await.unwrap();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("foo".into()));
        args.positional.push(Value::String("/bin.dat".into()));
        args.named
            .insert("binary".to_string(), Value::String("text".into()));

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(
            result.text_out().contains("after_null"),
            "binary=text should find post-NUL match, got: {:?}",
            result.text_out().to_string(),
        );
    }

    /// `--json` emits a richer per-match shape than the legacy
    /// `MATCH/FILE/LINE` table: each row is an object with `path`,
    /// `line_number`, `byte_offset`, `line_text`, and a `submatches`
    /// array carrying per-submatch text + byte ranges.
    #[tokio::test]
    async fn test_grep_json_rich_schema() {
        use kaish_types::output::{OutputFormat, apply_output_format};

        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello".into()));
        args.positional.push(Value::String("/test.txt".into()));
        args.flags.insert("n".to_string());

        let raw = Grep.execute(args, &mut ctx).await;
        let result = apply_output_format(raw, OutputFormat::Json);
        let parsed: serde_json::Value =
            serde_json::from_str(&result.text_out()).expect("valid JSON");
        let arr = parsed.as_array().expect("array");
        assert!(!arr.is_empty(), "expected at least one match: {parsed:#?}");

        let first = &arr[0];
        // Required keys.
        for key in ["path", "line_number", "byte_offset", "line_text", "submatches"] {
            assert!(
                first.get(key).is_some(),
                "missing key {key:?} in rich JSON: {first:#?}",
            );
        }
        // `submatches` is an array; each entry has text/start/end.
        let subs = first
            .get("submatches")
            .and_then(|v| v.as_array())
            .expect("submatches array");
        assert!(!subs.is_empty(), "expected at least one submatch");
        let first_sub = &subs[0];
        assert!(first_sub.get("text").and_then(|v| v.as_str()).is_some());
        assert!(first_sub.get("start").and_then(|v| v.as_u64()).is_some());
        assert!(first_sub.get("end").and_then(|v| v.as_u64()).is_some());
    }

    /// Engine swap regression: `--` separator emitted between
    /// non-contiguous context groups when -C/-A/-B are used.
    #[tokio::test]
    async fn test_grep_context_break_separator() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin(
            "match1\nbetween1\nbetween2\nbetween3\nbetween4\nmatch2\n".to_string(),
        );

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("match".into()));
        args.named.insert("context".to_string(), Value::Int(1));

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        // The two match groups don't overlap (gap > 2*context). Expect a `--` separator
        // between them.
        let out = result.text_out().to_string();
        assert!(
            out.contains("--\n"),
            "expected context-break separator '--' in output, got:\n{out}",
        );
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
        assert_eq!(&*result.text_out(), "foo bar\n");
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
        assert_eq!(result.text_out().trim(), "/test.txt");
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
        assert!(result.text_out().contains("line one"));
        assert!(result.text_out().contains("line two"));
        assert!(result.text_out().contains("line three"));
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
        assert!(result.text_out().contains("TODO"));
        assert!(result.text_out().contains("main.rs"));
        assert!(result.text_out().contains("lib.rs"));
        assert!(result.text_out().contains("README.md"));
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
        assert!(result.text_out().contains(":"));
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
        assert!(result.text_out().contains("main.rs") || result.text_out().contains("lib.rs"));
        assert!(!result.text_out().contains("README.md"));
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
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
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
        assert!(result.text_out().contains("TODO"));
        assert!(result.text_out().contains("main.rs") || result.text_out().contains("lib.rs"));
    }

    async fn make_ctx_with(files: &[(&str, &[u8])]) -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        for (path, content) in files {
            mem.write(Path::new(path), content).await.unwrap();
        }
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    /// -F (fixed strings): pattern's regex metachars must be treated literally.
    /// Regression guard: previous behavior silently accepted -F and matched
    /// `1.168.1.1` against `1X168Y1Z1` because `.` was the regex any-char.
    #[tokio::test]
    async fn test_grep_fixed_strings_literal_dot() {
        let mut ctx = make_ctx_with(&[(
            "ips.txt",
            b"192.168.1.1\n1X168Y1Z1\n10.0.0.1\n",
        )])
        .await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("192.168.1.1".into()));
        args.positional.push(Value::String("/ips.txt".into()));
        args.flags.insert("F".to_string());

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok(), "grep -F should succeed: {}", result.err);
        let out = result.text_out();
        assert!(out.contains("192.168.1.1"), "literal match missing: {}", out);
        assert!(!out.contains("1X168Y1Z1"), "regex metachar leaked through -F: {}", out);
    }

    /// -F with regex metachars in the pattern must escape them, not error.
    #[tokio::test]
    async fn test_grep_fixed_strings_with_metachars() {
        let mut ctx = make_ctx_with(&[("code.txt", b"foo[bar]\nfoobar\nbaz\n")]).await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("foo[bar]".into()));
        args.positional.push(Value::String("/code.txt".into()));
        args.flags.insert("fixed-strings".to_string());

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok(), "grep --fixed_strings should succeed: {}", result.err);
        assert!(result.text_out().contains("foo[bar]"));
        assert!(!result.text_out().contains("foobar\n"));
    }
}
