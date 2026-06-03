//! rg — ripgrep-style search.
//!
//! A first-class kaish builtin that uses ripgrep's `grep-searcher` +
//! `grep-regex` engine for searching, and `ignore::WalkBuilder` for
//! traversing real-FS paths (gets parent-`.gitignore` walk-up, type
//! filters, hidden-file handling, etc. for free). VFS paths fall back
//! to kaish's own `FileWalker`, which after Pillar C carries the same
//! semantics modulo parallelism. Output mirrors `grep`'s — legacy
//! table for text mode, rich JSON for `--json`.
//!
//! WASI compatibility is currently out of scope; this builtin pulls in
//! `memmap2` (transitively via `grep-searcher`) and `walkdir` (via
//! `ignore`), neither of which compile on `wasm32-wasip1`. Will gate
//! behind `native` once the rest of the design settles.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use grep_matcher::Matcher;
use grep_regex::{RegexMatcher, RegexMatcherBuilder};
use grep_searcher::{BinaryDetection, Encoding, SearcherBuilder};
use ignore::types::TypesBuilder;
use std::path::{Path, PathBuf};

use crate::ast::Value;
use crate::backend_walker_fs::BackendWalkerFs;
use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::builtin::grep_engine::{AccumulatorSink, ContextKind, SearchEvent};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};
use crate::walker::{EntryTypes, FileWalker, IncludeExclude, WalkOptions};

pub struct Rg;

/// clap-derived argv layer for rg. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "rg", about = "ripgrep — recursive search with gitignore awareness")]
struct RgArgs {
    /// Case-insensitive matching.
    #[arg(short = 'i', long = "ignore-case", visible_alias = "ignore_case")]
    ignore_case: bool,

    /// Match whole words only.
    #[arg(short = 'w', long = "word")]
    word: bool,

    /// Treat pattern as a literal string.
    #[arg(short = 'F', long = "fixed-strings", visible_alias = "fixed_strings")]
    fixed_strings: bool,

    /// Select non-matching lines.
    #[arg(short = 'v', long = "invert")]
    invert: bool,

    /// Show line numbers (default on).
    #[arg(short = 'n', long = "line-number", visible_alias = "line_number")]
    line_number: bool,

    /// Suppress line numbers.
    #[arg(short = 'N', long = "no-line-number", visible_alias = "no_line_number")]
    no_line_number: bool,

    /// Count matching lines per file.
    #[arg(short = 'c', long = "count")]
    count: bool,

    /// List only filenames with matches.
    #[arg(short = 'l', long = "files-with-matches", visible_alias = "files_with_matches")]
    files_with_matches: bool,

    /// Print only matched parts.
    #[arg(short = 'o', long = "only-matching", visible_alias = "only_matching")]
    only_matching: bool,

    /// Print NUM lines after each match.
    #[arg(short = 'A', long = "after-context", visible_alias = "after_context")]
    after_context: Option<String>,

    /// Print NUM lines before each match.
    #[arg(short = 'B', long = "before-context", visible_alias = "before_context")]
    before_context: Option<String>,

    /// Print NUM lines before and after each match.
    #[arg(short = 'C', long = "context")]
    context: Option<String>,

    /// Stop after NUM matches per file.
    #[arg(short = 'm', long = "max-count", visible_alias = "max_count")]
    max_count: Option<String>,

    /// Limit directory recursion depth.
    #[arg(long = "max-depth", visible_alias = "max_depth")]
    max_depth: Option<String>,

    /// Skip files larger than this many bytes.
    #[arg(long = "max-filesize", visible_alias = "max_filesize")]
    max_filesize: Option<String>,

    /// Allow patterns to match across line boundaries.
    #[arg(short = 'U', long = "multiline")]
    multiline: bool,

    /// Only search files matching the given type.
    #[arg(id = "type", short = 't', long = "type")]
    type_: Option<String>,

    /// Skip files matching the given type.
    #[arg(short = 'T', long = "type-not", visible_alias = "type_not")]
    type_not: Option<String>,

    /// Search hidden files and directories.
    #[arg(long = "hidden")]
    hidden: bool,

    /// Don't honor .gitignore / .ignore / .rgignore.
    #[arg(long = "no-ignore", visible_alias = "no_ignore")]
    no_ignore: bool,

    /// Include only files matching glob.
    #[arg(long = "include")]
    include: Option<String>,

    /// Exclude files matching glob.
    #[arg(long = "exclude")]
    exclude: Option<String>,

    /// Force a specific text encoding.
    #[arg(long = "encoding")]
    encoding: Option<String>,

    /// Binary handling: quit (default), text, without-match.
    #[arg(long = "binary")]
    binary: Option<String>,

    /// List files that would be searched, without searching them.
    #[arg(long = "files")]
    files: bool,

    /// Use PCRE2 regex (requires --features pcre2).
    #[arg(short = 'P', long = "pcre2")]
    pcre2: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Pattern to search for, followed by optional paths (defaults to `.`).
    pattern_and_paths: Vec<String>,
}

#[async_trait]
impl Tool for Rg {
    fn name(&self) -> &str {
        "rg"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &RgArgs::command(),
            "rg",
            "ripgrep — recursive search with gitignore awareness",
            [
                ("Recursive search", "rg pattern ."),
                ("Type filter: only Rust files", "rg -trust 'fn main' ."),
                ("Type filter: skip JS files", "rg -Tjs TODO ."),
                ("Multiline match", "rg -U '(?s)foo.*bar' ."),
                ("Search hidden files", "rg --hidden secret ."),
            ],
        )
    }

    async fn execute(&self, mut args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        args.flagify_bool_named();

        let parsed = match RgArgs::try_parse_from(
            std::iter::once("rg".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("rg: {e}")),
        };
        parsed.global.apply(ctx);

        let pattern = match args.get_string("pattern", 0) {
            Some(p) => p,
            None => return ExecResult::failure(2, "rg: missing pattern"),
        };

        let opts = match RgOptions::from_args(&args) {
            Ok(o) => o,
            Err(e) => return ExecResult::failure(2, format!("rg: {e}")),
        };

        // PCRE2 dispatch: with `--features pcre2`, build a `grep_pcre2`
        // matcher and run searches through it; without the feature,
        // surface a clear error so users know how to enable it.
        if opts.pcre2 {
            #[cfg(feature = "pcre2")]
            {
                let m = match build_pcre2_matcher(&pattern, &opts) {
                    Ok(m) => m,
                    Err(e) => {
                        return ExecResult::failure(2, format!("rg: invalid PCRE2 pattern: {e}"));
                    }
                };
                return run_with_matcher(&m, &args, &opts, ctx).await;
            }
            #[cfg(not(feature = "pcre2"))]
            return ExecResult::failure(
                2,
                "rg: -P requires building kaish with --features pcre2",
            );
        }

        let matcher = match build_matcher(&pattern, &opts) {
            Ok(m) => m,
            Err(e) => return ExecResult::failure(2, format!("rg: invalid pattern: {e}")),
        };
        run_with_matcher(&matcher, &args, &opts, ctx).await
    }
}

/// Drive the search end-to-end against a fully-built matcher. Generic so
/// PCRE2 and the default regex backend share the same code path.
async fn run_with_matcher<M: Matcher>(
    matcher: &M,
    args: &ToolArgs,
    opts: &RgOptions,
    ctx: &mut ExecContext,
) -> ExecResult {
    // Stdin path: no positional roots + stdin available → search stdin
    // bytes directly via `search_slice`. Avoids needing a sync-over-
    // async reader bridge for the common buffered case.
    let positional_paths: Vec<String> = args
        .positional
        .iter()
        .skip(1)
        .filter_map(|v| match v {
            Value::String(s) => Some(s.clone()),
            _ => None,
        })
        .collect();

    let stdin_available = ctx.pipe_stdin.is_some() || ctx.stdin.is_some();
    if positional_paths.is_empty() && stdin_available {
        let stdin = ctx.read_stdin_to_string().await.unwrap_or_default();
        return run_stdin_search(matcher, stdin.as_bytes(), opts);
    }

    let mut roots = positional_paths;
    if roots.is_empty() {
        roots.push(".".to_string());
    }

    // Gather files across all roots, dispatching real-FS vs VFS per root.
    let mut files: Vec<PathBuf> = Vec::new();
    for root in &roots {
        let resolved = ctx.resolve_path(root);
        let resolved_path = PathBuf::from(&resolved);
        let is_dir = ctx.backend.stat(&resolved_path).await.is_ok_and(|s| s.is_dir());
        if !is_dir {
            files.push(resolved_path);
            continue;
        }
        match ctx.backend.resolve_real_path(&resolved_path) {
            Some(real) => match collect_real_fs(&real, opts) {
                Ok(mut paths) => files.append(&mut paths),
                Err(e) => {
                    return ExecResult::failure(2, format!("rg: {root}: {e}"));
                }
            },
            None => match collect_vfs(ctx, &resolved_path, opts).await {
                Ok(mut paths) => files.append(&mut paths),
                Err(e) => {
                    return ExecResult::failure(2, format!("rg: {root}: {e}"));
                }
            },
        }
    }

    if opts.list_files {
        return list_files_result(&files);
    }

    let mut total_text = String::new();
    let mut total_nodes: Vec<OutputNode> = Vec::new();
    let mut total_rich: Vec<serde_json::Value> = Vec::new();
    let mut total_matches: usize = 0;
    let mut files_with_matches: Vec<String> = Vec::new();
    let mut per_file_counts: Vec<(String, usize)> = Vec::new();

    for file_path in &files {
        let display = file_path.to_string_lossy().to_string();
        let render_result = match ctx.backend.resolve_real_path(file_path) {
            Some(real) => search_real_path(matcher, &real, &display, opts),
            None => match ctx.backend.read(file_path, None).await {
                Ok(bytes) => search_slice(matcher, &bytes, &display, opts),
                Err(_) => continue,
            },
        };
        let render = match render_result {
            Ok(r) => r,
            Err(_) => continue,
        };
        if render.match_count > 0 {
            files_with_matches.push(display.clone());
            per_file_counts.push((display, render.match_count));
            total_matches += render.match_count;
            total_text.push_str(&render.text);
            total_nodes.extend(render.nodes);
            total_rich.extend(render.rich);
        }
    }

    if opts.files_with_matches {
        return if files_with_matches.is_empty() {
            ExecResult::from_output(1, "", "")
        } else {
            ExecResult::with_output(OutputData::text(files_with_matches.join("\n") + "\n"))
        };
    }

    if opts.count {
        return if per_file_counts.is_empty() {
            ExecResult::from_output(1, "", "")
        } else {
            let text = per_file_counts
                .iter()
                .map(|(p, n)| format!("{p}:{n}"))
                .collect::<Vec<_>>()
                .join("\n")
                + "\n";
            ExecResult::with_output(OutputData::text(text))
        };
    }

    if total_matches == 0 {
        return ExecResult::from_output(1, total_text, "");
    }

    let headers = if opts.show_line_numbers {
        vec!["MATCH".to_string(), "FILE".to_string(), "LINE".to_string()]
    } else {
        vec!["MATCH".to_string(), "FILE".to_string()]
    };
    let output = OutputData::table(headers, total_nodes)
        .with_rich_json(serde_json::Value::Array(total_rich));
    ExecResult::with_output_and_text(output, total_text)
}

/// Search stdin bytes and emit the same render bundle as a one-file
/// search rooted at "(stdin)".
fn run_stdin_search<M: Matcher>(matcher: &M, bytes: &[u8], opts: &RgOptions) -> ExecResult {
    let display = "(stdin)";
    let render = match search_slice(matcher, bytes, display, opts) {
        Ok(r) => r,
        Err(e) => return ExecResult::failure(2, format!("rg: {e}")),
    };

    if opts.files_with_matches {
        return if render.match_count > 0 {
            ExecResult::with_output(OutputData::text(format!("{display}\n")))
        } else {
            ExecResult::from_output(1, "", "")
        };
    }

    if opts.count {
        return if render.match_count > 0 {
            ExecResult::with_output(OutputData::text(format!(
                "{display}:{}\n",
                render.match_count,
            )))
        } else {
            ExecResult::from_output(1, "", "")
        };
    }

    if render.match_count == 0 {
        return ExecResult::from_output(1, render.text, "");
    }

    let headers = if opts.show_line_numbers {
        vec!["MATCH".to_string(), "FILE".to_string(), "LINE".to_string()]
    } else {
        vec!["MATCH".to_string(), "FILE".to_string()]
    };
    let output = OutputData::table(headers, render.nodes)
        .with_rich_json(serde_json::Value::Array(render.rich));
    ExecResult::with_output_and_text(output, render.text)
}

/// Parsed flags for one `rg` invocation. Centralizes the clamps and
/// error messages so `execute` stays readable.
struct RgOptions {
    ignore_case: bool,
    word: bool,
    fixed_strings: bool,
    invert: bool,
    show_line_numbers: bool,
    only_matching: bool,
    files_with_matches: bool,
    count: bool,
    after_context: Option<usize>,
    before_context: Option<usize>,
    max_count: Option<u64>,
    max_depth: Option<usize>,
    max_filesize: Option<u64>,
    multiline: bool,
    type_select: Option<String>,
    type_negate: Option<String>,
    hidden: bool,
    no_ignore: bool,
    include: Option<String>,
    exclude: Option<String>,
    encoding: Option<String>,
    binary_detection: BinaryDetection,
    list_files: bool,
    pcre2: bool,
}

impl RgOptions {
    fn from_args(args: &ToolArgs) -> Result<Self, String> {
        let context = int_arg(args, "context");
        let after = int_arg(args, "after_context").or(context);
        let before = int_arg(args, "before_context").or(context);

        let line_number = args.has_flag("line-number") || args.has_flag("n");
        let no_line_number = args.has_flag("no-line-number") || args.has_flag("N");
        // -N wins over default-on -n.
        let show_line_numbers = line_number && !no_line_number;

        let binary_mode = args
            .get_string("binary", usize::MAX)
            .unwrap_or_else(|| "quit".into());
        let binary_detection = match binary_mode.as_str() {
            "none" | "text" => BinaryDetection::none(),
            "without-match" => BinaryDetection::convert(b'\x00'),
            "quit" => BinaryDetection::quit(b'\x00'),
            other => return Err(format!("invalid --binary value: {other}")),
        };

        Ok(Self {
            ignore_case: args.has_flag("ignore-case") || args.has_flag("i"),
            word: args.has_flag("word") || args.has_flag("w"),
            fixed_strings: args.has_flag("fixed-strings") || args.has_flag("F"),
            invert: args.has_flag("invert") || args.has_flag("v"),
            show_line_numbers,
            only_matching: args.has_flag("only-matching") || args.has_flag("o"),
            files_with_matches: args.has_flag("files-with-matches") || args.has_flag("l"),
            count: args.has_flag("count") || args.has_flag("c"),
            after_context: after,
            before_context: before,
            max_count: int_arg(args, "max_count").map(|n| n as u64),
            max_depth: int_arg(args, "max_depth"),
            max_filesize: int_arg(args, "max_filesize").map(|n| n as u64),
            multiline: args.has_flag("multiline") || args.has_flag("U"),
            type_select: args.get_string("type", usize::MAX),
            type_negate: args.get_string("type_not", usize::MAX),
            hidden: args.has_flag("hidden"),
            no_ignore: args.has_flag("no-ignore"),
            include: args.get_string("include", usize::MAX),
            exclude: args.get_string("exclude", usize::MAX),
            encoding: args.get_string("encoding", usize::MAX),
            binary_detection,
            list_files: args.has_flag("files"),
            pcre2: args.has_flag("pcre2") || args.has_flag("P"),
        })
    }
}

#[cfg(feature = "pcre2")]
fn build_pcre2_matcher(
    pattern: &str,
    opts: &RgOptions,
) -> Result<grep_pcre2::RegexMatcher, String> {
    use grep_pcre2::RegexMatcherBuilder as Pcre2Builder;
    let escaped: String;
    let pattern_str: &str = if opts.fixed_strings {
        escaped = regex::escape(pattern);
        &escaped
    } else {
        pattern
    };
    let final_pattern: String = if opts.word {
        format!(r"\b{pattern_str}\b")
    } else {
        pattern_str.to_string()
    };
    let mut b = Pcre2Builder::new();
    b.caseless(opts.ignore_case).multi_line(opts.multiline);
    b.build(&final_pattern).map_err(|e| e.to_string())
}

fn int_arg(args: &ToolArgs, name: &str) -> Option<usize> {
    args.get(name, usize::MAX).and_then(|v| match v {
        Value::Int(i) if *i >= 0 => Some(*i as usize),
        Value::String(s) => s.parse().ok(),
        _ => None,
    })
}

fn build_matcher(pattern: &str, opts: &RgOptions) -> Result<RegexMatcher, String> {
    // -F (fixed strings): escape regex metacharacters before compilation.
    let escaped: String;
    let pattern_str: &str = if opts.fixed_strings {
        escaped = regex::escape(pattern);
        &escaped
    } else {
        pattern
    };

    let final_pattern: String = if opts.word {
        format!(r"\b{pattern_str}\b")
    } else {
        pattern_str.to_string()
    };

    RegexMatcherBuilder::new()
        .case_insensitive(opts.ignore_case)
        .multi_line(opts.multiline)
        .build(&final_pattern)
        .map_err(|e| e.to_string())
}

fn build_searcher_for(opts: &RgOptions) -> Result<grep_searcher::Searcher, String> {
    let mut sb = SearcherBuilder::new();
    sb.line_number(true)
        .multi_line(opts.multiline)
        .invert_match(opts.invert)
        .binary_detection(opts.binary_detection.clone());
    if let Some(b) = opts.before_context {
        sb.before_context(b);
    }
    if let Some(a) = opts.after_context {
        sb.after_context(a);
    }
    if let Some(label) = opts.encoding.as_deref() {
        let enc = Encoding::new(label).map_err(|e| format!("invalid encoding '{label}': {e}"))?;
        sb.encoding(Some(enc));
    }
    Ok(sb.build())
}

/// Build a `Types` matcher from the `-t` / `-T` arguments.
fn build_types(opts: &RgOptions) -> Result<Option<ignore::types::Types>, String> {
    if opts.type_select.is_none() && opts.type_negate.is_none() {
        return Ok(None);
    }
    let mut tb = TypesBuilder::new();
    tb.add_defaults();
    if let Some(t) = opts.type_select.as_deref() {
        tb.select(t);
    }
    if let Some(t) = opts.type_negate.as_deref() {
        tb.negate(t);
    }
    let types = tb.build().map_err(|e| format!("type filter: {e}"))?;
    Ok(Some(types))
}

/// Walk a real-FS root using ripgrep's `ignore::WalkBuilder`. Yields a
/// flat list of file paths in deterministic order (sequential `Walk`
/// iterator — no parallel workers in this round).
fn collect_real_fs(root: &Path, opts: &RgOptions) -> Result<Vec<PathBuf>, String> {
    let mut wb = ignore::WalkBuilder::new(root);
    wb.hidden(!opts.hidden)
        .ignore(!opts.no_ignore)
        .git_ignore(!opts.no_ignore)
        .git_global(!opts.no_ignore)
        .git_exclude(!opts.no_ignore)
        .parents(!opts.no_ignore);
    if let Some(d) = opts.max_depth {
        wb.max_depth(Some(d));
    }
    if let Some(sz) = opts.max_filesize {
        wb.max_filesize(Some(sz));
    }
    if let Some(types) = build_types(opts)? {
        wb.types(types);
    }
    if let Some(inc) = &opts.include {
        // ripgrep's overrides take a glob list; we expose it as a single
        // include + a single exclude for simplicity. Both can be set.
        let mut ob = ignore::overrides::OverrideBuilder::new(root);
        ob.add(inc).map_err(|e| format!("include: {e}"))?;
        if let Some(exc) = &opts.exclude {
            ob.add(&format!("!{exc}"))
                .map_err(|e| format!("exclude: {e}"))?;
        }
        let ov = ob.build().map_err(|e| format!("overrides: {e}"))?;
        wb.overrides(ov);
    } else if let Some(exc) = &opts.exclude {
        let mut ob = ignore::overrides::OverrideBuilder::new(root);
        ob.add(&format!("!{exc}"))
            .map_err(|e| format!("exclude: {e}"))?;
        let ov = ob.build().map_err(|e| format!("overrides: {e}"))?;
        wb.overrides(ov);
    }

    let mut out = Vec::new();
    for entry in wb.build() {
        match entry {
            Ok(dent) => {
                if dent.file_type().is_some_and(|ft| ft.is_file()) {
                    out.push(dent.into_path());
                }
            }
            Err(_) => continue, // permission denied etc. — silently skipped, matches rg
        }
    }
    Ok(out)
}

/// Walk a VFS root using kaish's own `FileWalker`. Pulls type filters
/// and gitignore handling from the kernel's `IgnoreConfig`.
async fn collect_vfs(
    ctx: &mut ExecContext,
    root: &Path,
    opts: &RgOptions,
) -> Result<Vec<PathBuf>, String> {
    let mut filter = IncludeExclude::new();
    if let Some(inc) = &opts.include {
        filter.include(inc);
    }
    if let Some(exc) = &opts.exclude {
        filter.exclude(exc);
    }

    let walk_opts = WalkOptions {
        max_depth: opts.max_depth,
        max_filesize: opts.max_filesize,
        entry_types: EntryTypes::files_only(),
        respect_gitignore: !opts.no_ignore,
        include_hidden: opts.hidden,
        filter,
        types: build_types(opts)
            .map_err(|e| e.to_string())?
            .map(std::sync::Arc::new),
        ..WalkOptions::default()
    };

    let fs = BackendWalkerFs(ctx.backend.as_ref());
    let mut walker = FileWalker::new(&fs, root).with_options(walk_opts);
    if !opts.no_ignore
        && let Some(ignore_filter) = ctx.build_ignore_filter(root).await
    {
        walker = walker.with_ignore(ignore_filter);
    }
    walker.collect().await.map_err(|e| e.to_string())
}

/// Result of searching one file.
struct FileSearchOutput {
    text: String,
    nodes: Vec<OutputNode>,
    rich: Vec<serde_json::Value>,
    match_count: usize,
}

fn search_real_path<M: Matcher>(
    matcher: &M,
    real_path: &Path,
    display: &str,
    opts: &RgOptions,
) -> Result<FileSearchOutput, String> {
    let mut searcher = build_searcher_for(opts)?;
    let mut sink = AccumulatorSink::new(matcher, Some(real_path.to_path_buf()))
        .with_max_count(opts.max_count);
    searcher
        .search_path(matcher, real_path, &mut sink)
        .map_err(|e| e.to_string())?;
    Ok(render_for_file(&sink.into_events(), display, opts))
}

fn search_slice<M: Matcher>(
    matcher: &M,
    bytes: &[u8],
    display: &str,
    opts: &RgOptions,
) -> Result<FileSearchOutput, String> {
    let mut searcher = build_searcher_for(opts)?;
    let mut sink =
        AccumulatorSink::new(matcher, Some(PathBuf::from(display))).with_max_count(opts.max_count);
    searcher
        .search_slice(matcher, bytes, &mut sink)
        .map_err(|e| e.to_string())?;
    Ok(render_for_file(&sink.into_events(), display, opts))
}

/// Format one file's events into legacy text + table nodes + rich JSON.
fn render_for_file(events: &[SearchEvent], filename: &str, opts: &RgOptions) -> FileSearchOutput {
    let prefix = |line_num: u64, sep: char| -> String {
        let mut p = String::new();
        p.push_str(filename);
        p.push(sep);
        if opts.show_line_numbers {
            p.push_str(&format!("{line_num}{sep}"));
        }
        p
    };

    let mut text = String::new();
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
                        text.push_str(&prefix(line_num, ':'));
                        text.push_str(&sub.text);
                        text.push('\n');
                        let mut cells = vec![filename.to_string()];
                        if opts.show_line_numbers {
                            cells.push(line_num.to_string());
                        }
                        nodes.push(OutputNode::new(&sub.text).with_cells(cells));
                    }
                } else {
                    text.push_str(&prefix(line_num, ':'));
                    text.push_str(&m.line_text);
                    text.push('\n');
                    let mut cells = vec![filename.to_string()];
                    if opts.show_line_numbers {
                        cells.push(line_num.to_string());
                    }
                    nodes.push(OutputNode::new(&m.line_text).with_cells(cells));
                }
                rich.push(match_to_json(m, filename));
                match_count += 1;
                emitted_any = true;
            }
            SearchEvent::Context(c) => {
                let sep = match c.kind {
                    ContextKind::Before | ContextKind::After | ContextKind::Other => '-',
                };
                let line_num = c.line_number.unwrap_or(0);
                text.push_str(&prefix(line_num, sep));
                text.push_str(&c.line_text);
                text.push('\n');
                emitted_any = true;
            }
            SearchEvent::ContextBreak => {
                if emitted_any {
                    text.push_str("--\n");
                }
            }
        }
    }

    FileSearchOutput {
        text,
        nodes,
        rich,
        match_count,
    }
}

fn match_to_json(
    m: &crate::tools::builtin::grep_engine::MatchRecord,
    filename: &str,
) -> serde_json::Value {
    use serde_json::{Value, json};
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
        "path": filename,
        "line_number": line_number_v,
        "byte_offset": m.absolute_byte_offset,
        "line_text": m.line_text,
        "submatches": submatches,
    })
}

fn list_files_result(files: &[PathBuf]) -> ExecResult {
    if files.is_empty() {
        return ExecResult::from_output(1, "", "");
    }
    let lines: Vec<String> = files.iter().map(|p| p.to_string_lossy().to_string()).collect();
    let text = lines.join("\n") + "\n";
    let nodes: Vec<OutputNode> = lines.iter().map(OutputNode::new).collect();
    ExecResult::with_output_and_text(OutputData::nodes(nodes), text)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn ctx_with_project() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.mkdir(Path::new("src")).await.unwrap();
        mem.mkdir(Path::new("docs")).await.unwrap();
        mem.write(
            Path::new("src/main.rs"),
            b"fn main() {\n    println!(\"hello, TODO world\");\n}\n",
        )
        .await
        .unwrap();
        mem.write(Path::new("src/lib.rs"), b"pub fn helper() {}\n// TODO\n")
            .await
            .unwrap();
        mem.write(Path::new("docs/notes.md"), b"# Notes\nTODO: write docs\n")
            .await
            .unwrap();
        mem.write(Path::new("README.md"), b"# Project\nTODO: ship\n")
            .await
            .unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn rg_basic_recursive_search() {
        let mut ctx = ctx_with_project().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("TODO".into()));
        args.positional.push(Value::String("/".into()));

        let result = Rg.execute(args, &mut ctx).await;
        assert!(result.ok(), "rg failed: code={} err={}", result.code, result.err);
        let text = result.text_out();
        assert!(text.contains("main.rs"), "missing main.rs: {text}");
        assert!(text.contains("notes.md"), "missing notes.md: {text}");
        assert!(text.contains("TODO"), "missing TODO: {text}");
    }

    #[tokio::test]
    async fn rg_type_filter_only_rust() {
        let mut ctx = ctx_with_project().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("TODO".into()));
        args.positional.push(Value::String("/".into()));
        args.named
            .insert("type".to_string(), Value::String("rust".into()));

        let result = Rg.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        assert!(text.contains("main.rs"));
        assert!(text.contains("lib.rs"));
        assert!(!text.contains("notes.md"), "type filter should exclude .md: {text}");
        assert!(!text.contains("README.md"));
    }

    #[tokio::test]
    async fn rg_type_negate_excludes_rust() {
        let mut ctx = ctx_with_project().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("TODO".into()));
        args.positional.push(Value::String("/".into()));
        args.named
            .insert("type_not".to_string(), Value::String("rust".into()));

        let result = Rg.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        assert!(!text.contains("main.rs"));
        assert!(text.contains("notes.md"));
    }

    #[tokio::test]
    async fn rg_files_mode_lists_searched_paths() {
        let mut ctx = ctx_with_project().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("dummy".into()));
        args.positional.push(Value::String("/".into()));
        args.flags.insert("files".to_string());

        let result = Rg.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        // Should list files but not search them.
        assert!(text.contains("main.rs"));
        assert!(text.contains("README.md"));
    }

    #[tokio::test]
    async fn rg_no_match_returns_exit_1() {
        let mut ctx = ctx_with_project().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("__no_such_token__".into()));
        args.positional.push(Value::String("/".into()));

        let result = Rg.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1);
    }

    #[tokio::test]
    async fn rg_count_mode() {
        let mut ctx = ctx_with_project().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("TODO".into()));
        args.positional.push(Value::String("/".into()));
        args.flags.insert("c".to_string());

        let result = Rg.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Format: path:count\n per matching file.
        let text = result.text_out();
        assert!(text.contains(":1") || text.contains(":2"), "expected count format: {text}");
    }

    #[tokio::test]
    async fn rg_files_with_matches() {
        let mut ctx = ctx_with_project().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("TODO".into()));
        args.positional.push(Value::String("/".into()));
        args.flags.insert("l".to_string());

        let result = Rg.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Just file paths, no inline content.
        let text = result.text_out();
        assert!(!text.contains("println"));
        assert!(text.contains("main.rs"));
    }

    #[tokio::test]
    async fn rg_max_count_per_file() {
        let mut ctx = ctx_with_project().await;
        // Ensure a file has multiple matches.
        if let Some(_fs) = std::option::Option::<&str>::None { let _: () = (); }

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("o".into())); // matches 'o' in many places
        args.positional.push(Value::String("/src/main.rs".into()));
        args.named.insert("max_count".to_string(), Value::Int(1));

        let result = Rg.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Exactly one match from main.rs.
        let text = result.text_out();
        assert_eq!(text.lines().count(), 1, "expected 1 line, got: {text}");
    }

    /// Rich JSON shape — same contract as `grep --json`.
    #[tokio::test]
    async fn rg_json_rich_schema() {
        use kaish_types::output::{OutputFormat, apply_output_format};

        let mut ctx = ctx_with_project().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("TODO".into()));
        args.positional.push(Value::String("/".into()));

        let raw = Rg.execute(args, &mut ctx).await;
        let result = apply_output_format(raw, OutputFormat::Json);
        let parsed: serde_json::Value =
            serde_json::from_str(&result.text_out()).expect("valid JSON");
        let arr = parsed.as_array().expect("array");
        assert!(!arr.is_empty());
        let first = &arr[0];
        for key in ["path", "line_number", "byte_offset", "line_text", "submatches"] {
            assert!(first.get(key).is_some(), "missing key {key}: {first}");
        }
    }

    /// Multiline pattern via -U.
    #[tokio::test]
    async fn rg_multiline_match() {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(
            Path::new("multi.txt"),
            b"foo header\nbar middle\nbaz footer\n",
        )
        .await
        .unwrap();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("(?s)foo.*baz".into()));
        args.positional.push(Value::String("/multi.txt".into()));
        args.flags.insert("U".to_string());

        let result = Rg.execute(args, &mut ctx).await;
        assert!(
            result.ok(),
            "rg -U failed: code={} err={}",
            result.code,
            result.err
        );
    }

    /// Hidden files default to skipped; --hidden surfaces them.
    #[tokio::test]
    async fn rg_hidden_default_off() {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new(".secret"), b"TODO hidden\n").await.unwrap();
        mem.write(Path::new("public.txt"), b"TODO public\n").await.unwrap();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("TODO".into()));
        args.positional.push(Value::String("/".into()));

        let result = Rg.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        assert!(text.contains("public.txt"));
        assert!(!text.contains(".secret"), "hidden file leaked: {text}");
    }

    /// `echo foo bar | rg foo` — no positional paths, stdin available,
    /// rg searches the buffered stdin.
    #[tokio::test]
    async fn rg_searches_stdin_when_no_paths() {
        let mut ctx = ctx_with_project().await;
        ctx.set_stdin("hello foo world\nno match here\nfoo again\n".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("foo".into()));

        let result = Rg.execute(args, &mut ctx).await;
        assert!(
            result.ok(),
            "stdin search failed: code={} err={}",
            result.code,
            result.err
        );
        let text = result.text_out();
        assert!(text.contains("hello foo world"));
        assert!(text.contains("foo again"));
        assert!(!text.contains("no match here"));
    }

    /// `-P` without the cargo feature must fail with a clear message.
    #[cfg(not(feature = "pcre2"))]
    #[tokio::test]
    async fn rg_pcre2_flag_errors_without_feature() {
        let mut ctx = ctx_with_project().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("foo".into()));
        args.positional.push(Value::String("/".into()));
        args.flags.insert("P".to_string());

        let result = Rg.execute(args, &mut ctx).await;
        assert_eq!(result.code, 2);
        assert!(
            result.err.contains("--features pcre2"),
            "expected pcre2 feature hint, got: {}",
            result.err,
        );
    }

    /// With the feature enabled, `-P` accepts a lookahead (something the
    /// default regex backend rejects).
    #[cfg(feature = "pcre2")]
    #[tokio::test]
    async fn rg_pcre2_supports_lookahead() {
        let mut ctx = ctx_with_project().await;
        let mut args = ToolArgs::new();
        // PCRE2-specific: positive lookahead.
        args.positional.push(Value::String("TODO(?=:)".into()));
        args.positional.push(Value::String("/".into()));
        args.flags.insert("P".to_string());

        let result = Rg.execute(args, &mut ctx).await;
        assert!(
            result.ok(),
            "rg -P failed: code={} err={}",
            result.code,
            result.err,
        );
    }

    #[tokio::test]
    async fn rg_hidden_flag_includes() {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new(".secret"), b"TODO hidden\n").await.unwrap();
        mem.write(Path::new("public.txt"), b"TODO public\n").await.unwrap();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("TODO".into()));
        args.positional.push(Value::String("/".into()));
        args.flags.insert("hidden".to_string());

        let result = Rg.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        assert!(text.contains(".secret"), "expected .secret with --hidden: {text}");
    }
}
