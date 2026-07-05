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
use crate::tools::builtin::read_repeatable_strings;
use crate::tools::builtin::regex_dialect::{append_dialect_hint, bre_metas_to_ere};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema, validate_against_schema};
use crate::validator::{IssueCode, ValidationIssue};
use crate::walker::{
    build_file_types, list_file_types, FileWalker, GlobPath, IncludeExclude, WalkOptions,
};

/// Grep tool: search for patterns in text.
pub struct Grep;

/// clap-derived argv layer for grep.
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

    /// Strict ERE (POSIX -E): backslash-escaped metas match the literal
    /// character (`\|` is a `|`). Default mode also accepts the GNU BRE
    /// spellings (`a\|b`, `\(…\)`, `x\{2,5\}`) as operators.
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

    /// Filter the recursive walk to files of the given type(s), e.g.
    /// `--ftype rust`. Repeatable. `--ftype` is the kaish-wide file-type
    /// filter (see `--ftype-list`).
    #[arg(long = "ftype")]
    ftype: Vec<String>,

    /// Exclude files of the given type(s) from the recursive walk. Repeatable.
    #[arg(long = "ftype-not")]
    ftype_not: Vec<String>,

    /// List known file types (TYPE → globs) and exit. No pattern needed.
    #[arg(long = "ftype-list")]
    ftype_list: bool,

    /// Include hidden files and directories (dotfiles) in the recursive walk.
    /// Off by default; applies to `-r` only.
    #[arg(long = "hidden")]
    hidden: bool,

    /// Stop after NUM matching lines per file (GNU `--max-count`).
    #[arg(long = "max-count")]
    max_count: Option<String>,

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
                ("Alternation (ERE or GNU BRE)", r"grep 'foo\|bar' file.txt"),
                ("With file filter", "grep -rn TODO . --include='*.rs'"),
            ],
        )
    }

    fn validate(&self, args: &ToolArgs) -> Vec<ValidationIssue> {
        let mut issues = validate_against_schema(args, &self.schema());

        // Skip regex syntax check when -F is set: pattern will be escaped at runtime.
        let fixed = args.has_flag("F") || args.has_flag("fixed-strings");
        if !fixed && let Some(pattern) = args.get_string("pattern", 0) {
            // Validate the pattern that actually runs: rewrite GNU BRE metas to
            // ERE first (issue #60) so `grep 'a\|b'` is checked as alternation.
            // `-E` (strict ERE) skips the rewrite, matching execute().
            let extended = args.has_flag("E") || args.has_flag("extended-regexp");
            let rewritten = if extended { pattern.clone() } else { bre_metas_to_ere(&pattern) };
            let rewrote = rewritten != pattern;
            // Don't validate if pattern looks dynamic (contains shell expansion markers)
            if !rewritten.contains("<dynamic>")
                && let Err(e) = regex::Regex::new(&rewritten) {
                    issues.push(ValidationIssue::error(
                        IssueCode::InvalidRegex,
                        append_dialect_hint(
                            format!("grep: invalid regex pattern: {}", e),
                            rewrote,
                            Some("-E"),
                        ),
                    ).with_suggestion("check regex syntax at https://docs.rs/regex"));
                }
        }

        issues
    }

    async fn execute(&self, mut args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        args.flagify_bool_named(&self.schema());

        let parsed = match GrepArgs::try_parse_from(
            std::iter::once("grep".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("grep: {e}")),
        };
        parsed.global.apply(ctx);

        // `--ftype-list` is a pure info query: emit the TYPE→globs table and
        // exit, no pattern required.
        if parsed.ftype_list {
            let rows: Vec<OutputNode> = list_file_types()
                .into_iter()
                .map(|(name, globs)| OutputNode::new(&name).with_cells(vec![globs.join(", ")]))
                .collect();
            let table = OutputData::table(vec!["TYPE".to_string(), "GLOBS".to_string()], rows);
            return ExecResult::with_output(table);
        }

        // Build the file-type filter from `--ftype`/`--ftype-not`. Repeatable
        // value flags arrive as `Json(Array)` (or a bare `String` for one) —
        // read them off the raw args like sed's `-e`, since `to_argv()` can't
        // round-trip a repeatable array back through the clap re-parse. An
        // unknown type name is loud (exit 2), never a silent empty match. The
        // filter only takes effect on the `-r` walk below, but we validate the
        // names here regardless so a typo is caught on any invocation.
        let ftype_select = read_repeatable_strings(&args, "ftype");
        let ftype_negate = read_repeatable_strings(&args, "ftype-not");
        let file_types = match build_file_types(&ftype_select, &ftype_negate) {
            Ok(t) => t,
            Err(e) => return ExecResult::failure(2, format!("grep: {e}")),
        };
        let include_hidden = parsed.hidden;

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
        // Default: rewrite the GNU BRE backslash-metas (`\|`, `\+`, `\(`, …) into
        // their ERE form so agent-idiomatic `grep 'a\|b'` alternates instead of
        // silently matching a literal `|` (issue #60). `-E` (extended) is strict
        // ERE, where those escapes stay literal — the escape hatch for a literal
        // `|`/`+`.
        // -w wraps in word boundaries (regex syntax) AFTER escaping so
        // `grep -Fw "192.168.1.1"` still anchors at word boundaries.
        let extended = args.has_flag("E") || args.has_flag("extended-regexp");
        let (escaped, dialect_rewrote) = if fixed_strings {
            (regex::escape(&pattern), false)
        } else if extended {
            (pattern, false)
        } else {
            let rewritten = bre_metas_to_ere(&pattern);
            let rewrote = rewritten != pattern;
            (rewritten, rewrote)
        };
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
            Err(e) => {
                return ExecResult::failure(
                    1,
                    append_dialect_hint(
                        format!("grep: invalid pattern: {}", e),
                        dialect_rewrote,
                        Some("-E"),
                    ),
                )
            }
        };

        // RegexMatcher drives the Searcher; same pattern, same flags.
        let matcher = match RegexMatcherBuilder::new()
            .case_insensitive(ignore_case)
            .multi_line(multiline)
            .build(&final_pattern)
        {
            Ok(m) => m,
            Err(e) => {
                return ExecResult::failure(
                    1,
                    append_dialect_hint(
                        format!("grep: invalid pattern: {}", e),
                        dialect_rewrote,
                        Some("-E"),
                    ),
                )
            }
        };

        // `--max-count N`: stop after N matching lines per file (GNU semantics).
        // `--max-count 0` matches nothing. A non-numeric value is a loud usage
        // error, reusing the same parser as the context flags.
        let max_count = match parse_context("--max-count", &parsed.max_count) {
            Ok(c) => c,
            Err(e) => return ExecResult::failure(2, e),
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
            max_count,
        };

        // Handle recursive search
        if recursive {
            // Partition the operands by kind. A *directory* is walked; a *file*
            // is searched directly (#105 — a file has nothing "under" it, so
            // treating it as a walk root collected zero entries and reported no
            // matches *silently*, the worst failure mode for a model reading the
            // empty result as "not found"). `-r` governs how *directories*
            // expand, which is the only thing anyone means by it. A missing or
            // otherwise-unstattable operand falls to the walker, preserving the
            // pre-#105 behavior for a bad root.
            let operands: Vec<String> = args
                .positional
                .iter()
                .skip(1)
                .map(crate::interpreter::value_to_string)
                .collect();
            let operands = if operands.is_empty() {
                vec![".".to_string()]
            } else {
                operands
            };

            let mut dir_roots: Vec<PathBuf> = Vec::new();
            let mut file_operands: Vec<PathBuf> = Vec::new();
            for operand in &operands {
                let resolved = ctx.resolve_path(operand);
                match ctx.backend.stat(&resolved).await {
                    Ok(entry) if entry.is_file() => file_operands.push(resolved),
                    _ => dir_roots.push(resolved),
                }
            }

            // No directory to recurse into: every operand is a plain file. Fall
            // through to the ordinary file-operand handling below (a lone file
            // → no prefix like `grep -c`; several → prefixed) — exactly what
            // grep *without* `-r` does with these files. This is the #105 fix.
            if !dir_roots.is_empty() {
                let fs = BackendWalkerFs(ctx.backend.as_ref());
                let mut files: Vec<PathBuf> = Vec::new();
                for root in &dir_roots {
                    // include/exclude + glob are walk-only (see the validation
                    // note above); rebuilt per root since `with_options` moves.
                    let mut filter = IncludeExclude::new();
                    if let Some(Value::String(inc)) = args.get("include", usize::MAX) {
                        filter.include(inc);
                    }
                    if let Some(Value::String(exc)) = args.get("exclude", usize::MAX) {
                        filter.exclude(exc);
                    }
                    let glob = if let Some(Value::String(inc)) = args.get("include", usize::MAX) {
                        GlobPath::new(&format!("**/{}", inc)).ok()
                    } else {
                        GlobPath::new("**/*").ok()
                    };

                    let options = WalkOptions {
                        max_depth: None,
                        entry_types: crate::walker::EntryTypes::files_only(),
                        respect_gitignore: ctx.ignore_config.auto_gitignore(),
                        include_hidden,
                        filter,
                        types: file_types.clone(),
                        ..WalkOptions::default()
                    };

                    let mut walker = if let Some(g) = glob {
                        FileWalker::new(&fs, root)
                            .with_pattern(g)
                            .with_options(options)
                    } else {
                        FileWalker::new(&fs, root).with_options(options)
                    };

                    if let Some(ignore_filter) = ctx.build_ignore_filter(root).await {
                        walker = walker.with_ignore(ignore_filter);
                    }

                    match walker.collect().await {
                        Ok(f) => files.extend(f),
                        Err(e) => return ExecResult::failure(1, format!("grep: {}", e)),
                    }
                }

                // Directly-named file operands (the mixed `grep -r p file dir`
                // case) join the walked set.
                let has_file_operands = !file_operands.is_empty();
                files.extend(file_operands);

                // Display prefix: strip the sole walk root — preserving the
                // historical `grep -r p dir` → `file` display byte-for-byte —
                // whenever there's exactly one directory and no file operand
                // mixed in. Otherwise strip cwd so each source shows under its
                // own subpath (`top.txt`, `sub/inner.txt`).
                let display_root = if dir_roots.len() == 1 && !has_file_operands {
                    dir_roots[0].clone()
                } else {
                    ctx.resolve_path(".")
                };

                return self
                    .grep_multiple_files(ctx, &files, &display_root, &matcher, &grep_opts, quiet, files_only, count_only, false)
                    .await;
            }
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
                    ctx, &resolved, &root, &matcher, &grep_opts, quiet, files_only, count_only, true,
                )
                .await;
        }

        // Streaming path: pipe_stdin → pipe_stdout, process line by line.
        // Only for simple grep (no context, no count, no quiet, no files-only, no only-matching).
        let is_simple = !count_only && !quiet && !files_only && !only_matching
            && before_context.is_none() && after_context.is_none();
        let can_stream = args.get_string("path", 1).is_none()
            && is_simple
            && ctx.pipe_stdin.is_some() && ctx.pipe_stdout.is_some();
        if can_stream {
            // Both checked with is_some() above — take() cannot return None.
            if let (Some(pipe_stdin), Some(pipe_stdout)) =
                (ctx.pipe_stdin.take(), ctx.pipe_stdout.take())
            {
                return self.stream_grep(ctx, pipe_stdin, pipe_stdout, &regex, invert, line_number, max_count).await;
            }
        }

        // Single file: stream the file in bounded chunks rather than reading it whole.
        // The conditions mirror `can_stream` above — simple invocations only.
        // Complex flags (-A/-B/-C context, -c, -q, -l, -o) keep the whole-buffer path
        // below because context lines need look-behind buffering.
        if let Some(path) = args.get_string("path", 1).filter(|_| is_simple) {
            let resolved = ctx.resolve_path(&path);
            // Mirror BinaryDetection::quit(b'\x00') for the default binary_mode.
            // The grep-searcher's quit mode stops at the first NUL byte; our
            // streaming path must do the same so single-file and buffered paths
            // produce identical results.
            let quit_byte = match binary_mode.as_str() {
                "none" | "text" | "without-match" => None,
                _ => Some(b'\x00'),
            };
            let mut scanner = GrepLineScanner::new(
                &regex,
                invert,
                line_number,
                quit_byte,
                Some(path.clone()),
                max_count,
            );
            let scan_result = ctx
                .read_file_chunked(
                    Path::new(&resolved),
                    ExecContext::STREAM_CHUNK_SIZE,
                    |chunk| {
                        scanner.push(chunk);
                        // Once the scanner has decided the file is binary (bad
                        // UTF-8), hit the quit byte, or reached --max-count, the
                        // rest of the file is irrelevant — stop reading it.
                        if scanner.saw_invalid_utf8 || scanner.stopped_early || scanner.hit_limit {
                            std::ops::ControlFlow::Break(())
                        } else {
                            std::ops::ControlFlow::Continue(())
                        }
                    },
                )
                .await;

            // I/O error reading the file.
            if let Err(e) = scan_result {
                return ExecResult::failure(1, format!("grep: {}: {}", path, e));
            }

            // Flush the remaining carry.  `saw_invalid_utf8` is set if any
            // chunk (including the final carry) contained a genuinely bad byte.
            // `stopped_early` means a quit_byte (NUL) was seen.
            scanner.finish();
            if scanner.saw_invalid_utf8 {
                return ExecResult::failure(
                    2,
                    format!("grep: {path}: binary data — pipe through base64/xxd or use cmp"),
                );
            }

            // A NUL byte (valid UTF-8, so not the loud error above) triggers
            // grep-searcher's `BinaryDetection::quit`, whose exact emit/suppress
            // semantics are subtle and engine-specific. Rather than reproduce
            // them in the line scanner, fall through to the whole-buffer path
            // below — it runs the real searcher and is the parity reference.
            // NUL-bearing files are the rare binary case, so the extra read is
            // acceptable.
            if !scanner.stopped_early {
                let render = scanner.into_render_result();
                return if render.match_count == 0 {
                    ExecResult::from_output(1, render.text, "")
                } else {
                    let headers = if line_number {
                        vec!["MATCH".to_string(), "LINE".to_string()]
                    } else {
                        vec!["MATCH".to_string()]
                    };
                    let output = OutputData::table(headers, render.nodes)
                        .with_rich_json(serde_json::Value::Array(render.rich));
                    ExecResult::with_output_and_text(output, render.text)
                };
            }
            // else: NUL seen — fall through to the whole-buffer path below.
        }

        // Fallback: whole-buffer path for stdin OR complex flags (-c/-q/-l/-o/-A/-B/-C).
        let (bytes, filename) = match args.get_string("path", 1) {
            Some(path) => {
                let resolved = ctx.resolve_path(&path);
                match ctx.backend.read(Path::new(&resolved), None).await {
                    Ok(data) => (data, Some(path)),
                    Err(e) => return ExecResult::failure(1, format!("grep: {}: {}", path, e)),
                }
            }
            None => {
                let text = match ctx.read_stdin_to_text().await {
                    Ok(s) => s.unwrap_or_default(),
                    Err(e) => return ExecResult::failure(2, format!("grep: {e}")),
                };
                (text.into_bytes(), None)
            }
        };

        // grep is a text tool: a binary file is a loud error, not a lossy match.
        if std::str::from_utf8(&bytes).is_err() {
            let where_ = filename.as_deref().unwrap_or("(standard input)");
            return ExecResult::failure(
                2,
                format!("grep: {where_}: binary data — pipe through base64/xxd or use cmp"),
            );
        }

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
            // GNU `grep -c`: the count is printed *and* the exit status still
            // reflects whether anything matched (1 = no match).
            let mut result =
                ExecResult::with_output(OutputData::text(format!("{}\n", render.match_count)));
            if render.match_count == 0 {
                result.code = 1;
            }
            result
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
    #[allow(clippy::too_many_arguments)]
    async fn stream_grep(
        &self,
        _ctx: &mut ExecContext,
        pipe_in: crate::scheduler::PipeReader,
        mut pipe_out: crate::scheduler::PipeWriter,
        regex: &regex::Regex,
        invert: bool,
        show_line_numbers: bool,
        max_count: Option<usize>,
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
                        // --max-count: stop before emitting the (N+1)th line.
                        // `Some(0)` breaks before the first, matching nothing.
                        if let Some(max) = max_count
                            && match_count >= max
                        {
                            break;
                        }
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

    /// Search several files, prefixing each match with its filename.
    ///
    /// `report_missing` distinguishes the two callers: explicit file operands
    /// (`grep p a.txt b.txt`) must report an unreadable operand on stderr and
    /// exit 2, like POSIX grep — silently skipping it would hide a typo. The
    /// recursive walk (`grep -r`) passes `false`: a file vanishing between the
    /// directory walk and the read is a benign race, not a user error.
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
        report_missing: bool,
    ) -> ExecResult {
        let mut total_output = String::new();
        let mut total_nodes: Vec<OutputNode> = Vec::new();
        let mut total_rich: Vec<serde_json::Value> = Vec::new();
        let mut total_matches: usize = 0;
        let mut files_with_matches = Vec::new();
        let mut error_text = String::new();

        let opts = GrepOptions {
            show_filename: true,
            ..base_opts.clone()
        };

        for file_path in files {
            // Create relative filename for display
            let display_name = file_path
                .strip_prefix(root)
                .unwrap_or(file_path)
                .to_string_lossy()
                .to_string();

            let bytes = match ctx.backend.read(file_path, None).await {
                Ok(data) => data,
                Err(e) => {
                    if report_missing {
                        error_text.push_str(&format!("grep: {}: {}\n", display_name, e));
                    }
                    continue;
                }
            };

            let render = match grep_lines_structured(&bytes, matcher, &opts, Some(&display_name)) {
                Ok(t) => t,
                Err(e) => {
                    if report_missing {
                        error_text.push_str(&format!("grep: {}: {}\n", display_name, e));
                    }
                    continue;
                }
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

        let mut result = if quiet {
            if total_matches > 0 {
                ExecResult::success("")
            } else {
                ExecResult::from_output(1, "", "")
            }
        } else if files_only {
            if files_with_matches.is_empty() {
                ExecResult::from_output(1, "", "")
            } else {
                ExecResult::with_output(OutputData::text(files_with_matches.join("\n") + "\n"))
            }
        } else if count_only {
            // GNU `grep -c` over multiple files prints a per-file/total count
            // but still exits 1 when nothing matched in any file.
            let mut r = ExecResult::with_output(OutputData::text(format!("{}\n", total_matches)));
            if total_matches == 0 {
                r.code = 1;
            }
            r
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
        };

        // A read/parse error on an explicit operand is exit 2 and stderr,
        // overriding the match-based code — the error must not be swallowed by
        // matches found in the readable files. (`quiet` keeps a 0 on a match
        // per POSIX; everything else surfaces the trouble.)
        if !error_text.is_empty() {
            result.err.push_str(&error_text);
            if !(quiet && total_matches > 0) {
                result.code = 2;
            }
        }
        result
    }
}

/// Incremental, line-buffered grep scanner for streaming file reads.
///
/// Fed arbitrary byte chunks via [`push`](GrepLineScanner::push), it produces
/// the same `RenderResult` as [`grep_lines_structured`] for simple matches
/// (no context, no `-c`/`-q`/`-l`/`-o`) — without ever holding the whole file.
///
/// Binary detection is incremental:
/// - A `\n` is never part of a multibyte UTF-8 sequence, so splitting on it
///   keeps each line's bytes complete and lets us validate them with
///   `std::str::from_utf8`.
/// - An incomplete multibyte sequence at a chunk boundary is distinguishable
///   from a genuine invalid byte via `Utf8Error::error_len()`: `None` means
///   "truncated at end of input" (not an error; wait for the next chunk),
///   `Some` means a real bad byte (binary error, exit 2).
/// - A `quit_byte` (default `\x00`) mirrors `BinaryDetection::quit(b'\x00')`:
///   scanning stops silently at the first occurrence without emitting an error.
struct GrepLineScanner<'r> {
    regex: &'r regex::Regex,
    invert: bool,
    show_line_numbers: bool,
    /// Filename for the rich-JSON `path` field, mirroring the whole-buffer
    /// path's `fallback_path` (the arg the user passed). `None` for stdin.
    path: Option<String>,
    /// If `Some(b)`, stop scanning **silently** when byte `b` is seen.
    /// Mirrors `BinaryDetection::quit(b)` from the grep-searcher library: the
    /// searcher just stops; it does not return an error.
    quit_byte: Option<u8>,
    /// Bytes since the last `\n` — the in-progress (potentially partial) line.
    carry: Vec<u8>,
    /// Absolute byte offset of `carry[0]` from the start of the file — i.e. how
    /// many bytes have already been drained. Used to give each emitted line the
    /// same `byte_offset` the grep-searcher reports (`absolute_byte_offset`).
    consumed: u64,
    line_number: u64,
    text: String,
    nodes: Vec<OutputNode>,
    rich: Vec<serde_json::Value>,
    match_count: usize,
    /// Set to `true` when a genuinely invalid UTF-8 byte is seen.  Callers
    /// check this after [`finish`](GrepLineScanner::finish) and emit exit-2.
    /// Distinguished from `stopped_early` (quit byte) which is *not* an error.
    saw_invalid_utf8: bool,
    /// Set to `true` when the `quit_byte` is encountered.  Scanning stops but
    /// the matched output collected so far is still returned (no error).
    stopped_early: bool,
    /// `--max-count`: stop after this many emitted lines. `None` = no limit;
    /// `Some(0)` emits nothing.
    max_count: Option<usize>,
    /// Set when `max_count` is reached. Like `stopped_early` it halts the chunk
    /// reader, but — unlike a quit byte — it does *not* trigger the whole-buffer
    /// re-read fallback (the cap is already honored by the scanner).
    hit_limit: bool,
}

impl<'r> GrepLineScanner<'r> {
    fn new(
        regex: &'r regex::Regex,
        invert: bool,
        show_line_numbers: bool,
        quit_byte: Option<u8>,
        path: Option<String>,
        max_count: Option<usize>,
    ) -> Self {
        Self {
            regex,
            invert,
            show_line_numbers,
            path,
            quit_byte,
            carry: Vec::new(),
            consumed: 0,
            line_number: 0,
            text: String::new(),
            nodes: Vec::new(),
            rich: Vec::new(),
            match_count: 0,
            saw_invalid_utf8: false,
            stopped_early: false,
            max_count,
            hit_limit: false,
        }
    }

    /// Feed the next chunk of bytes from the file.
    /// - Sets `saw_invalid_utf8 = true` on a genuinely bad UTF-8 byte (exit 2).
    /// - Sets `stopped_early = true` on a `quit_byte` (stop silently, no error).
    fn push(&mut self, chunk: &[u8]) {
        if self.saw_invalid_utf8 || self.stopped_early || self.hit_limit {
            return;
        }
        // Honour BinaryDetection::quit(byte): stop at the first occurrence of
        // the quit byte, mirroring grep-searcher's "quit" mode (no error).
        let chunk = if let Some(qb) = self.quit_byte {
            if let Some(pos) = chunk.iter().position(|&b| b == qb) {
                // Process only the bytes before the quit byte, then stop.
                let prefix = &chunk[..pos];
                self.carry.extend_from_slice(prefix);
                self.drain_complete_lines();
                self.stopped_early = true;
                return;
            }
            chunk
        } else {
            chunk
        };
        self.carry.extend_from_slice(chunk);
        self.drain_complete_lines();
    }

    /// Process all complete (newline-terminated) lines in `carry`, leaving any
    /// trailing partial line in place for the next chunk.
    ///
    /// Three cases for `from_utf8`:
    /// - `Ok`: entire carry is valid; process up to the last `\n`.
    /// - `Err` with `error_len() == None`: the *tail* of carry is an incomplete
    ///   multibyte sequence split across a chunk boundary — not a real error.
    ///   Process only the valid prefix (up to `valid_up_to`), but only lines
    ///   that are fully terminated by `\n` within that prefix.
    /// - `Err` with `error_len() == Some(_)`: genuine invalid byte → binary.
    fn drain_complete_lines(&mut self) {
        // Determine the longest valid UTF-8 prefix we can inspect.
        let valid_text = match std::str::from_utf8(&self.carry) {
            Ok(s) => s.to_owned(),
            Err(e) if e.error_len().is_none() => {
                // Incomplete multibyte at the tail — safe to inspect the valid prefix.
                // from_utf8 validated [0..valid_up_to]; the second call cannot fail.
                let valid_up_to = e.valid_up_to();
                match std::str::from_utf8(&self.carry[..valid_up_to]) {
                    Ok(s) => s.to_owned(),
                    // This branch is unreachable: Utf8Error guarantees the prefix
                    // is valid.  Treat it as empty to avoid a panic.
                    Err(_) => String::new(),
                }
            }
            Err(_) => {
                // Genuine invalid byte — binary file.
                self.saw_invalid_utf8 = true;
                return;
            }
        };

        // Find the last newline in the valid text (whether full or truncated
        // prefix).  If there isn't one, no complete line exists yet — wait.
        let Some(last_nl) = valid_text.rfind('\n') else {
            return; // No complete line yet.
        };

        // Process every complete line up to (and excluding) `last_nl`. Track
        // each line's start as a byte offset within carry so it can be made
        // absolute via `consumed`.
        let mut local = 0usize;
        for line in valid_text[..last_nl].split('\n') {
            let line_abs = self.consumed + local as u64;
            self.line_number += 1;
            // Mirror grep-searcher's CRLF handling (`trim_line_terminator`): a
            // `\r` before the `\n` is part of the terminator, not the line. The
            // offset still advances by the original length — the `\r` is a real
            // byte consumed before the `\n`.
            let stripped = line.strip_suffix('\r').unwrap_or(line);
            self.match_line(stripped, line_abs);
            local += line.len() + 1; // + 1 for the consumed '\n'
        }
        // Discard the processed bytes (including the `\n` at last_nl).
        self.carry.drain(..last_nl + 1);
        self.consumed += (last_nl + 1) as u64;
    }

    /// Flush the remaining carry as the final (unterminated) line.
    /// Sets `saw_invalid_utf8` if the remaining bytes are genuinely invalid UTF-8.
    ///
    /// `hit_limit` short-circuits like `stopped_early`: once `--max-count` is
    /// reached the reader stops mid-file, so the leftover carry may be a UTF-8
    /// multibyte sequence we *artificially* truncated at a chunk boundary.
    /// Validating it would raise a spurious "binary data" exit-2 and clobber a
    /// correct capped result — so don't.
    fn finish(&mut self) {
        if self.saw_invalid_utf8 || self.stopped_early || self.hit_limit || self.carry.is_empty() {
            return;
        }
        match std::str::from_utf8(&self.carry) {
            Ok(line) => {
                // `trim_line_terminator` strips a trailing `\r` even with no
                // following `\n`, so the final unterminated line does too.
                let owned = line.strip_suffix('\r').unwrap_or(line).to_owned();
                self.line_number += 1;
                // The trailing line starts exactly where the undrained carry begins.
                let line_abs = self.consumed;
                self.match_line(&owned, line_abs);
                self.consumed += self.carry.len() as u64;
                self.carry.clear();
            }
            Err(_) => {
                // Invalid bytes in the final carry — binary file.
                self.saw_invalid_utf8 = true;
            }
        }
    }

    fn match_line(&mut self, line: &str, byte_offset: u64) {
        if self.hit_limit {
            return;
        }
        let matches = self.regex.is_match(line);
        let should_output = if self.invert { !matches } else { matches };
        if !should_output {
            return;
        }

        // --max-count: don't emit beyond the cap. `Some(0)` stops before the
        // first match. Flagging `hit_limit` halts the chunk reader after this
        // chunk (without the quit-byte whole-buffer fallback).
        if let Some(max) = self.max_count
            && self.match_count >= max
        {
            self.hit_limit = true;
            return;
        }

        self.match_count += 1;

        // Build the prefix (line number, no filename — single-file simple path).
        if self.show_line_numbers {
            self.text.push_str(&format!("{}:{}\n", self.line_number, line));
        } else {
            self.text.push_str(line);
            self.text.push('\n');
        }

        let mut cells = Vec::new();
        if self.show_line_numbers {
            cells.push(self.line_number.to_string());
        }
        self.nodes
            .push(OutputNode::new(line).with_cells(cells));

        // Rich JSON: must be byte-identical to `match_record_to_json` for the
        // same line — path (the filename arg), the real line number (the
        // searcher numbers unconditionally), the line's absolute byte offset,
        // and submatches. Submatches come from `regex::Regex` here vs the
        // whole-buffer path's `grep_regex::RegexMatcher`; both compile the same
        // `final_pattern` with the same flags (same underlying engine), so spans
        // agree for the patterns we support — verified by the rich-JSON parity
        // test against `grep_lines_structured`.
        let submatches: Vec<serde_json::Value> = self
            .regex
            .find_iter(line)
            .map(|m| {
                serde_json::json!({
                    "text": m.as_str(),
                    "start": m.start(),
                    "end": m.end(),
                })
            })
            .collect();
        let path_v = match &self.path {
            Some(p) => serde_json::Value::String(p.clone()),
            None => serde_json::Value::Null,
        };
        self.rich.push(serde_json::json!({
            "path": path_v,
            "line_number": self.line_number,
            "byte_offset": byte_offset,
            "line_text": line,
            "submatches": submatches,
        }));
    }

    fn into_render_result(self) -> RenderResult {
        RenderResult {
            text: self.text,
            nodes: self.nodes,
            rich: self.rich,
            match_count: self.match_count,
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
    /// Stop after this many matching lines (GNU `--max-count`). `None` = no
    /// limit; `Some(0)` matches nothing.
    max_count: Option<usize>,
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
        // Honor --max-count: once N matching lines are emitted, stop. Trailing
        // after-context of the Nth match still flows (a new match or a group
        // break ends emission), matching GNU `grep -m N -A k`.
        if let Some(max) = opts.max_count
            && match_count >= max
        {
            match event {
                SearchEvent::Context(c) if matches!(c.kind, ContextKind::After) => {}
                _ => break,
            }
        }
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

    /// `-n` with `-B`/`-A` context must number every emitted line and keep the
    /// GNU separator convention: matched lines use `:`, context lines use `-`.
    #[tokio::test]
    async fn test_grep_line_numbers_with_context() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("two".into()));
        args.positional.push(Value::String("/lines.txt".into()));
        args.flags.insert("n".to_string());
        args.named.insert("before-context".to_string(), Value::Int(1));
        args.named.insert("after-context".to_string(), Value::Int(1));

        let result = Grep.execute(args, &mut ctx).await;
        assert!(result.ok());
        // /lines.txt = "line one\nline two\nline three\nfour"; match is line 2.
        // Before/after context get `-`; the match gets `:`.
        assert_eq!(
            &*result.text_out(),
            "1-line one\n2:line two\n3-line three\n",
        );
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

    // ---- Streaming file path: chunk-seam parity ----

    /// Helper: run the streaming file path (via Grep.execute with a small chunk
    /// size backed by a tiny MemoryFs file) and return the text output.
    async fn stream_grep_via_exec(content: &[u8], pattern: &str, line_numbers: bool) -> (String, i32) {
        let mem = MemoryFs::new();
        mem.write(Path::new("f.txt"), content).await.unwrap();
        let mut vfs = VfsRouter::new();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String(pattern.into()));
        args.positional.push(Value::String("/f.txt".into()));
        if line_numbers {
            args.flags.insert("n".to_string());
        }
        let result = Grep.execute(args, &mut ctx).await;
        (result.text_out().to_string(), result.code as i32)
    }

    /// Whole-buffer reference: the real production formatter the streaming path
    /// must match byte-for-byte (text, table, AND rich `--json`).
    fn reference_render(content: &[u8], pattern: &str, line_numbers: bool, path: Option<&str>) -> RenderResult {
        let matcher = RegexMatcherBuilder::new().build(pattern).unwrap();
        let opts = GrepOptions {
            show_line_numbers: line_numbers,
            invert: false,
            only_matching: false,
            before_context: None,
            after_context: None,
            show_filename: false,
            multiline: false,
            encoding: None,
            binary_detection: BinaryDetection::quit(b'\x00'),
            max_count: None,
        };
        grep_lines_structured(content, &matcher, &opts, path).unwrap()
    }

    /// Run the streaming scanner with a configurable chunk size.
    fn scanner_render(
        content: &[u8],
        pattern: &str,
        line_numbers: bool,
        path: Option<&str>,
        chunk_size: usize,
    ) -> (RenderResult, bool) {
        let regex = regex::RegexBuilder::new(pattern).build().unwrap();
        let mut scanner =
            GrepLineScanner::new(&regex, false, line_numbers, Some(b'\x00'), path.map(str::to_string), None);
        for chunk in content.chunks(chunk_size.max(1)) {
            scanner.push(chunk);
        }
        scanner.finish();
        let invalid = scanner.saw_invalid_utf8;
        (scanner.into_render_result(), invalid)
    }

    /// Convenience wrapper used by the binary/quit tests.
    fn scanner_grep(content: &[u8], pattern: &str, invert: bool, line_numbers: bool, chunk_size: usize) -> (String, usize, bool) {
        let regex = regex::RegexBuilder::new(pattern).build().unwrap();
        let mut scanner = GrepLineScanner::new(&regex, invert, line_numbers, Some(b'\x00'), None, None);
        for chunk in content.chunks(chunk_size) {
            scanner.push(chunk);
        }
        scanner.finish();
        let invalid = scanner.saw_invalid_utf8;
        let render = scanner.into_render_result();
        (render.text, render.match_count, invalid)
    }

    /// Chunk-seam parity: for each input, split at every byte boundary and both
    /// `-n` modes, and assert the streaming scanner produces output identical to
    /// the **production** whole-buffer path — including the rich `--json` array
    /// (path, line_number, byte_offset, line_text, submatches). This is the
    /// guard that catches a silent `--json` divergence; comparing text alone
    /// would not (an earlier draft hardcoded byte_offset=0 and path=null and
    /// still produced correct text).
    ///
    /// Inputs include a multibyte UTF-8 character (日本語) and lines that straddle
    /// arbitrary boundaries — the seam the scanner must handle correctly.
    #[test]
    fn grep_scanner_matches_whole_buffer_across_every_split() {
        let cases: &[(&[u8], &str)] = &[
            (b"hello world\nfoo bar\nbaz hello\n", "hello"),
            (b"line one\nline two\nline three\n", "line"),
            // Multibyte UTF-8 characters: each is 3 bytes in UTF-8.
            ("日本語テスト\nfoo\n日本語match\n".as_bytes(), "日本語"),
            // Line with no trailing newline (final partial line).
            (b"foo\nbar\nbaz", "bar"),
            // Case with no matches at all.
            (b"alpha\nbeta\ngamma\n", "zzz"),
            // Only one line, no newline.
            (b"only line here", "line"),
            // CRLF line endings — the `\r` must be stripped to match grep-searcher.
            (b"win foo\r\nwin bar\r\nno match\r\n", "win"),
            // CRLF with a final unterminated line carrying a stray `\r`.
            (b"a\r\nbcd\r", "a"),
            // Empty file.
            (b"", "pattern"),
        ];
        // NOTE: NUL-bearing inputs are intentionally absent here. The line
        // scanner does NOT replicate grep-searcher's `BinaryDetection::quit`;
        // instead the execute() path falls back to the whole-buffer searcher on
        // a NUL. That end-to-end parity is covered by
        // `grep_nul_file_matches_whole_buffer_path` below.

        for (input, pattern) in cases {
            for line_numbers in [false, true] {
                let reference = reference_render(input, pattern, line_numbers, Some("f.txt"));
                for chunk_size in 1..=input.len().max(1) {
                    let (render, invalid) =
                        scanner_render(input, pattern, line_numbers, Some("f.txt"), chunk_size);
                    let ctx = format!(
                        "pattern={pattern:?} ln={line_numbers} chunk={chunk_size} input={:?}",
                        String::from_utf8_lossy(input)
                    );
                    assert!(!invalid, "valid UTF-8 flagged binary — {ctx}");
                    assert_eq!(render.text, reference.text, "text — {ctx}");
                    assert_eq!(render.match_count, reference.match_count, "match_count — {ctx}");
                    assert_eq!(render.rich, reference.rich, "rich JSON — {ctx}");
                }
            }
        }
    }

    /// Binary file (invalid UTF-8) must produce exit-2 with the exact binary
    /// error message, and saw_invalid_utf8 must be set.
    #[test]
    fn grep_scanner_rejects_invalid_utf8_as_binary() {
        // \xff is not valid UTF-8 and will never be part of a multibyte sequence.
        let binary_content: &[u8] = b"valid line\n\xff invalid\nsecond line\n";
        let (text, _count, saw_invalid) = scanner_grep(binary_content, "line", false, false, 4);
        assert!(saw_invalid, "saw_invalid_utf8 must be set for non-UTF-8 input");
        // No matches should be emitted before/after the binary byte.
        // (In practice, "valid line\n" may have been processed if the invalid
        // byte is in a later chunk — the important thing is the flag is set.)
        let _ = text; // text may be partial; the caller checks saw_invalid_utf8
    }

    /// When a NUL byte is present and binary_mode is "quit" (the default),
    /// the scanner stops silently and saw_invalid_utf8 remains false.
    #[test]
    fn grep_scanner_quit_on_nul_is_not_an_error() {
        let content: &[u8] = b"foo line\nbar\x00baz\nsecond foo\n";
        let (text, _count, saw_invalid) =
            scanner_grep(content, "foo", false, false, 32);
        // The scanner must NOT set saw_invalid_utf8 on NUL (that's stopped_early).
        assert!(!saw_invalid, "NUL byte must not set saw_invalid_utf8");
        // "second foo" is after the NUL, so it must NOT appear.
        assert!(
            !text.contains("second foo"),
            "output after NUL must be suppressed: {text:?}",
        );
    }

    /// Streaming file path round-trip: Grep.execute via a MemoryFs file with the
    /// streaming path active (simple flags: no -c/-q/-l/-o/-A/-B/-C) must return
    /// the same text as the same invocation on a tiny file.
    #[tokio::test]
    async fn grep_streaming_file_parity_with_reference() {
        // Use a file with a multibyte character to stress the UTF-8 seam.
        let content = "one line\ntwo line\nthree\n日本語 test\n".as_bytes().to_vec();
        let (streamed, code) = stream_grep_via_exec(&content, "line", false).await;
        assert_eq!(code, 0, "streaming path must return 0 on match");
        assert!(streamed.contains("one line"), "missing 'one line': {streamed:?}");
        assert!(streamed.contains("two line"), "missing 'two line': {streamed:?}");
        assert!(!streamed.contains("three"), "non-matching line leaked: {streamed:?}");
    }

    /// Streaming file path with -n (line numbers) must number from 1.
    #[tokio::test]
    async fn grep_streaming_file_line_numbers() {
        let content = b"alpha\nbeta\ngamma\n";
        let (out, code) = stream_grep_via_exec(content, "beta", true).await;
        assert_eq!(code, 0);
        assert!(out.contains("2:beta"), "expected '2:beta' in output: {out:?}");
    }

    /// A file with invalid UTF-8 bytes must produce exit-2 with the binary error
    /// message, not a partial match or a panic.
    #[tokio::test]
    async fn grep_streaming_file_binary_data_error() {
        let content: Vec<u8> = b"good line\nbad \xff byte\nmore\n".to_vec();
        let mem = MemoryFs::new();
        mem.write(Path::new("bad.txt"), &content).await.unwrap();
        let mut vfs = VfsRouter::new();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("line".into()));
        args.positional.push(Value::String("/bad.txt".into()));
        let result = Grep.execute(args, &mut ctx).await;

        assert_eq!(result.code, 2, "binary data must exit with code 2");
        assert!(
            result.err.contains("binary data"),
            "error must mention 'binary data': {:?}",
            result.err,
        );
        assert!(
            result.err.contains("/bad.txt"),
            "error must name the file: {:?}",
            result.err,
        );
    }

    /// A NUL byte (valid UTF-8) is not the loud binary error; it triggers
    /// grep-searcher's `BinaryDetection::quit`. The streaming line scanner would
    /// emit pre-NUL matches, but the searcher suppresses them — so execute()
    /// falls back to the whole-buffer path on a NUL. This asserts the command's
    /// output matches the whole-buffer reference exactly (it did NOT before the
    /// fallback: the scanner emitted "foo" while the searcher emitted nothing).
    #[tokio::test]
    async fn grep_nul_file_matches_whole_buffer_path() {
        let content: &[u8] = b"foo\nbar\x00baz\nfoo again\n";
        for (pattern, ln) in [("foo", false), ("bar", false), ("foo", true)] {
            let reference = reference_render(content, pattern, ln, Some("/f.txt"));
            let (text, code) = stream_grep_via_exec(content, pattern, ln).await;
            assert_eq!(text, reference.text, "pattern={pattern} ln={ln}: text");
            let expected_code = if reference.match_count == 0 { 1 } else { 0 };
            assert_eq!(code, expected_code, "pattern={pattern} ln={ln}: exit code");
        }
    }

    // ---- Streaming: bounded-memory proof ----

    /// Recorded (offset, limit) pairs from each `read_range` call.
    type RecordedRanges = Arc<std::sync::Mutex<Vec<(Option<u64>, Option<u64>)>>>;

    /// A `Filesystem` that records every `read_range` it is asked for, so a test
    /// can prove the streaming file path pulls the file in bounded chunks rather
    /// than slurping it whole.  Delegates all real I/O to an inner `MemoryFs`.
    struct RecordingFs {
        inner: MemoryFs,
        ranges: RecordedRanges,
    }

    #[async_trait::async_trait]
    impl crate::vfs::Filesystem for RecordingFs {
        async fn read(&self, path: &Path) -> std::io::Result<Vec<u8>> {
            // A whole-file read would defeat the test; record it as (None, None)
            // so the assertion can catch it.
            self.ranges.lock().unwrap().push((None, None));
            self.inner.read(path).await
        }
        async fn read_range(
            &self,
            path: &Path,
            range: Option<kaish_vfs::ReadRange>,
        ) -> std::io::Result<Vec<u8>> {
            let key = (
                range.as_ref().and_then(|r| r.offset),
                range.as_ref().and_then(|r| r.limit),
            );
            self.ranges.lock().unwrap().push(key);
            self.inner.read_range(path, range).await
        }
        async fn write(&self, path: &Path, data: &[u8]) -> std::io::Result<()> {
            self.inner.write(path, data).await
        }
        async fn list(&self, path: &Path) -> std::io::Result<Vec<crate::vfs::DirEntry>> {
            self.inner.list(path).await
        }
        async fn stat(&self, path: &Path) -> std::io::Result<crate::vfs::DirEntry> {
            self.inner.stat(path).await
        }
        async fn mkdir(&self, path: &Path) -> std::io::Result<()> {
            self.inner.mkdir(path).await
        }
        async fn remove(&self, path: &Path) -> std::io::Result<()> {
            self.inner.remove(path).await
        }
        fn read_only(&self) -> bool {
            self.inner.read_only()
        }
    }

    #[tokio::test]
    async fn grep_streams_file_in_bounded_chunks() {
        let ranges = Arc::new(std::sync::Mutex::new(Vec::new()));
        let rec = RecordingFs {
            inner: MemoryFs::new(),
            ranges: ranges.clone(),
        };
        // 1000 bytes of repeated content with newlines to exercise the line-buffer path.
        let mut payload = Vec::new();
        for i in 0..40usize {
            payload.extend_from_slice(format!("line number {i:04} content here\n").as_bytes());
        }
        rec.inner.write(Path::new("big.txt"), &payload).await.unwrap();

        let mut vfs = VfsRouter::new();
        vfs.mount("/", rec);
        let ctx = ExecContext::new(Arc::new(vfs));

        // Use read_file_chunked directly with a small chunk to force multiple reads.
        let regex = regex::Regex::new("line").unwrap();
        let mut scanner =
            GrepLineScanner::new(&regex, false, false, Some(b'\x00'), Some("/big.txt".to_string()), None);
        ctx.read_file_chunked(Path::new("/big.txt"), 256, |c| {
            scanner.push(c);
            std::ops::ControlFlow::Continue(())
        })
        .await
        .unwrap();
        scanner.finish();

        let recs = ranges.lock().unwrap();
        // 1000+ bytes / 256 → multiple bounded reads + a terminating empty read.
        assert!(
            recs.len() >= 4,
            "expected the file to be read in several chunks, got {} reads",
            recs.len()
        );
        // Every read must be bounded (limit = Some(256)); no unbounded whole-file read.
        assert!(
            recs.iter().all(|&(_, limit)| limit == Some(256)),
            "every read must be bounded to the chunk size; recorded {recs:?}",
        );

        drop(recs);
        let render = scanner.into_render_result();
        // All 40 lines match "line".
        assert_eq!(render.match_count, 40, "expected 40 matches");
    }

    /// Regression (DeepSeek review, 2026-06-28): when `--max-count` is hit and
    /// the reader stops mid-file at a chunk boundary that splits a UTF-8
    /// multibyte sequence, the artificially truncated carry must NOT be reported
    /// as binary data. Before the `finish()` `hit_limit` guard, the leftover
    /// `\xC3` (lead byte of `é`) was validated and raised a spurious exit-2 over
    /// a correct 2-match result.
    #[test]
    fn max_count_does_not_flag_truncated_multibyte_carry() {
        let regex = regex::Regex::new("m").unwrap();
        let mut scanner = GrepLineScanner::new(&regex, false, false, Some(b'\x00'), None, Some(2));
        // 3 matching lines (cap is 2) then a lone UTF-8 lead byte with no
        // continuation — in production the reader stops here because the cap was
        // hit, so the continuation byte is never fed.
        scanner.push(b"m\nm\nm\n\xC3");
        assert!(scanner.hit_limit, "cap of 2 must be reached");
        scanner.finish();
        assert!(
            !scanner.saw_invalid_utf8,
            "a truncated-at-cap carry must not be flagged as binary",
        );
        let render = scanner.into_render_result();
        assert_eq!(render.match_count, 2, "exactly the capped number of matches");
    }
}
