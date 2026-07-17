//! find — Search for files in directory hierarchy.
//!
//! # Examples
//!
//! ```kaish
//! find /workspace                    # Find all files
//! find . -name "*.rs"                # Find by name pattern
//! find src -type f                   # Find files only
//! find . -type d                     # Find directories only
//! find . -maxdepth 2                 # Limit recursion depth
//! find . -name "*.rs" -type f        # Combine predicates
//! find . -mindepth 1                 # Skip start directory
//! find . -path '*/sub/*'             # Match full path glob
//! find . -ipath '*/SUB/*'            # Case-insensitive path glob
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use kaish_glob::glob_match;
use std::path::Path;
use std::time::SystemTime;
use kaish_types::clock::system_now;

use crate::ast::Value;
use crate::backend_walker_fs::BackendWalkerFs;
use crate::vfs::DirEntry;
use crate::ignore_config::IgnoreScope;
use crate::interpreter::{EntryType, ExecResult, OutputData, OutputNode};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};
use crate::walker::{EntryTypes, FileWalker, GlobPath, WalkOptions};

/// Find tool: searches for files in directory hierarchy.
pub struct Find;

/// clap-derived argv layer for find.
///
/// All GNU find predicates that kaish honours must be declared here so clap
/// does not mis-parse them as short-flag sequences.  For example, `-path`
/// without a declaration is lexed as `-p`, `-a`, `-t`, `-h`; `-h` triggers
/// the help output silently.  The fields are intentionally `Option<String>` /
/// `Option<i64>` — the actual predicate values are still read from
/// `args.named` / `args.positional` after clap has consumed the argv.
#[derive(Parser, Debug)]
#[command(name = "find", about = "Search for files in directory hierarchy")]
struct FindArgs {
    /// Pattern to match filename (glob syntax).
    #[arg(long = "name")]
    name: Option<String>,

    /// Type filter: 'f' for files, 'd' for directories.
    #[arg(id = "type", long = "type")]
    type_: Option<String>,

    /// Maximum depth to descend (GNU find -maxdepth; 0 = start path only).
    #[arg(long = "maxdepth")]
    maxdepth: Option<String>,

    /// Minimum depth to yield (GNU find -mindepth; 1 = skip start path).
    #[arg(long = "mindepth")]
    mindepth: Option<String>,

    /// Modified time filter: +N older than N days, -N newer than N days.
    #[arg(long = "mtime")]
    mtime: Option<String>,

    /// Size filter: +N larger than N bytes, -N smaller than N bytes.
    #[arg(long = "size")]
    size: Option<String>,

    /// Full-path glob predicate (case-sensitive).
    #[arg(long = "path", id = "path_glob")]
    path_glob: Option<String>,

    /// Full-path glob predicate (case-insensitive).
    #[arg(long = "ipath")]
    ipath: Option<String>,

    #[command(flatten)]
    global: GlobalFlags,

    /// Starting paths for the search; defaults to the current directory.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Find {
    fn name(&self) -> &str {
        "find"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &FindArgs::command(),
            "find",
            "Search for files in directory hierarchy",
            [
                ("Find all files", "find ."),
                ("Find by name pattern", "find src -name '*.rs'"),
                ("Find directories only", "find . -type d"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("find: {e}")),
        };
        let parsed = match FindArgs::try_parse_from(
            std::iter::once("find".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("find: {e}")),
        };
        parsed.global.apply(ctx);

        // Read starting paths from positionals (Value-typed, not the clap sink).
        // Fall back to "." when none supplied.
        let start_paths: Vec<String> = if args.positional.is_empty() {
            vec![".".to_string()]
        } else {
            match crate::interpreter::values_to_text_sink_named(&args.positional, "a path") {
                Ok(p) => p,
                Err(e) => return ExecResult::failure(1, format!("find: {e}")),
            }
        };

        // Parse predicates from the parsed clap struct (kebab-case, authoritative).
        let name_pattern = parsed.name.clone();

        let type_filter = parsed.type_.clone();
        let entry_types = match type_filter.as_deref() {
            Some("f") => EntryTypes::files_only(),
            Some("d") => EntryTypes::dirs_only(),
            Some(t) => {
                return ExecResult::failure(1, format!("find: invalid type '{}': use 'f' or 'd'", t));
            }
            None => EntryTypes::all(),
        };

        // -maxdepth: parse from the clap struct. A malformed value is refused
        // rather than silently walking unlimited.
        let max_depth: Option<usize> = match parsed.maxdepth.as_deref() {
            None => None,
            Some(s) => match s.parse() {
                Ok(d) => Some(d),
                Err(_) => {
                    return ExecResult::failure(
                        1,
                        format!("find: invalid -maxdepth '{s}': expected a non-negative integer"),
                    )
                }
            },
        };

        // -mindepth: parse from the clap struct.
        //
        // GNU semantics: -mindepth N excludes entries at depth < N, where the
        // start path itself is depth 0 and its direct children are depth 1.
        // The kaish walker uses "containing-dir depth" where root=0 means
        // "entries inside the root dir" — so a 1-based GNU depth maps to
        // (N-1)-based walker depth.  Translation: GNU N → walker N-1 (clamped
        // at 0 / None for N <= 1, since the start dir is never emitted anyway).
        let gnu_mindepth: Option<usize> = match parsed.mindepth.as_deref() {
            None => None,
            Some(s) => match s.parse() {
                Ok(d) => Some(d),
                Err(_) => {
                    return ExecResult::failure(
                        1,
                        format!("find: invalid -mindepth '{s}': expected a non-negative integer"),
                    )
                }
            },
        };
        let min_depth: Option<usize> = match gnu_mindepth {
            None | Some(0) | Some(1) => None, // walker already skips the start dir
            Some(n) => Some(n - 1),
        };

        // -mtime / -size: still read from args.named (Value-typed).
        let mtime_filter = args.get_named("mtime").and_then(parse_relative_int);

        let size_filter = args
            .get_string("size", usize::MAX)
            .and_then(|s| parse_size_filter(&s));

        // -path / -ipath: read from the clap struct.
        let path_glob = parsed.path_glob.clone();
        let ipath_glob = parsed.ipath.clone();

        // find only respects ignore config in Enforced scope
        let respect_ignore = matches!(ctx.ignore_config.scope(), IgnoreScope::Enforced)
            && ctx.ignore_config.is_active();

        // Accumulate all results across start paths.
        let mut nodes: Vec<OutputNode> = Vec::new();
        let mut json_array: Vec<serde_json::Value> = Vec::new();

        for start_path in &start_paths {
            let resolved_path = ctx.resolve_path(start_path);

            if !ctx.backend.exists(Path::new(&resolved_path)).await {
                return ExecResult::failure(
                    1,
                    format!("find: '{}': No such file or directory", start_path),
                );
            }

            // Detect whether the start path is a regular file (not a directory).
            // GNU find: when given a regular-file operand it prints that path and
            // moves on — it does NOT recurse into it.
            let start_stat = ctx.backend.stat(Path::new(&resolved_path)).await.ok();
            let start_is_file = start_stat
                .as_ref()
                .map(|s| !s.is_dir())
                .unwrap_or(false);

            if start_is_file {
                // Apply type filter: a plain file only matches "f" or no type.
                if matches!(type_filter.as_deref(), Some("d")) {
                    // -type d excludes regular files
                    continue;
                }
                // Apply -name predicate to the filename component.
                if let Some(ref pattern) = name_pattern {
                    let filename = Path::new(&resolved_path)
                        .file_name()
                        .and_then(|n| n.to_str())
                        .unwrap_or("");
                    if !glob_match(pattern, filename) {
                        continue;
                    }
                }
                // Apply -path / -ipath against the full display path.
                if !path_matches(&path_glob, &ipath_glob, start_path) {
                    continue;
                }
                if !passes_mtime_size(start_stat.as_ref(), mtime_filter, size_filter) {
                    continue;
                }
                let entry_type = EntryType::File;
                nodes.push(OutputNode::new(start_path).with_entry_type(entry_type));
                json_array.push(serde_json::Value::String(start_path.clone()));
                continue;
            }

            // -maxdepth 0 means "yield just the start directory, no recursion".
            // The walker lists *children* starting at depth 0, so we handle
            // maxdepth 0 as a special case before building the walker.
            if max_depth == Some(0) {
                // Yield the start directory itself (honoring type / path filters).
                if !matches!(type_filter.as_deref(), Some("f")) {
                    // -type f excludes directories
                    if let Some(ref pattern) = name_pattern {
                        let filename = Path::new(&resolved_path)
                            .file_name()
                            .and_then(|n| n.to_str())
                            .unwrap_or(".");
                        if !glob_match(pattern, filename) {
                            continue;
                        }
                    }
                    if !path_matches(&path_glob, &ipath_glob, start_path) {
                        continue;
                    }
                    if !passes_mtime_size(start_stat.as_ref(), mtime_filter, size_filter) {
                        continue;
                    }
                    nodes.push(
                        OutputNode::new(start_path).with_entry_type(EntryType::Directory),
                    );
                    json_array.push(serde_json::Value::String(start_path.clone()));
                }
                continue;
            }

            // Build walker options for directory recursion.
            let options = WalkOptions {
                max_depth,
                min_depth,
                entry_types,
                include_hidden: true, // find includes hidden by default
                respect_gitignore: if respect_ignore {
                    ctx.ignore_config.auto_gitignore()
                } else {
                    false
                },
                ..WalkOptions::default()
            };

            let fs = BackendWalkerFs(ctx.backend.as_ref());
            let mut walker = FileWalker::new(&fs, &resolved_path).with_options(options);

            if respect_ignore {
                if let Some(ignore_filter) = ctx.build_ignore_filter(&resolved_path).await {
                    walker = walker.with_ignore(ignore_filter);
                }
            }

            // -name: attach a glob pattern that matches the filename component.
            if let Some(ref pattern) = name_pattern {
                let glob_pattern = if pattern.contains('/') {
                    pattern.clone()
                } else {
                    format!("**/{}", pattern)
                };
                match GlobPath::new(&glob_pattern) {
                    Ok(glob) => walker = walker.with_pattern(glob),
                    Err(e) => {
                        return ExecResult::failure(
                            1,
                            format!("find: invalid pattern '{}': {}", pattern, e),
                        );
                    }
                }
            }

            let paths = match walker.collect().await {
                Ok(p) => p,
                Err(e) => return ExecResult::failure(1, format!("find: {}", e)),
            };

            let _now_secs = system_now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .map(|d| d.as_secs())
                .unwrap_or(0);

            for path in paths {
                let info = ctx.backend.stat(&path).await.ok();

                // -mtime filter
                if let Some((sign, days)) = mtime_filter
                    && let Some(ref info) = info
                    && let Some(modified) = info.modified
                {
                    let age_secs = modified.elapsed().map(|d| d.as_secs()).unwrap_or(0);
                    let age_days = age_secs / 86400;
                    let matches = match sign {
                        '+' => age_days > days,
                        '-' => age_days < days,
                        _ => age_days == days,
                    };
                    if !matches {
                        continue;
                    }
                }

                // -size filter
                if let Some((sign, size)) = size_filter
                    && let Some(ref info) = info
                {
                    let matches = match sign {
                        '+' => info.size > size,
                        '-' => info.size < size,
                        _ => info.size == size,
                    };
                    if !matches {
                        continue;
                    }
                }

                // -path / -ipath: match against the display path (relative to cwd).
                // The walker returns absolute paths; convert back to the same form
                // as the start_path for consistency with GNU find output.
                let display_path = relative_display_path(&path, &resolved_path, start_path);
                if !path_matches(&path_glob, &ipath_glob, &display_path) {
                    continue;
                }

                let entry_type = info
                    .map(|i| if i.is_dir() { EntryType::Directory } else { EntryType::File })
                    .unwrap_or(EntryType::File);

                nodes.push(OutputNode::new(&display_path).with_entry_type(entry_type));
                json_array.push(serde_json::Value::String(display_path));
            }
        }

        let output = OutputData::nodes(nodes);
        let mut result = ExecResult::with_output(output);
        result.data = Some(Value::Json(serde_json::Value::Array(json_array)));
        result
    }
}

/// Check whether `display_path` matches the `-path` and/or `-ipath` predicates.
///
/// Both predicates are glob patterns matched against the full path string as
/// emitted by find (e.g. `./sub/file.txt`).  `-path` is case-sensitive;
/// `-ipath` folds both pattern and path to lowercase before matching.
/// Returns `true` when no path predicates are active.
fn path_matches(path_glob: &Option<String>, ipath_glob: &Option<String>, display_path: &str) -> bool {
    if let Some(pat) = path_glob {
        if !glob_match(pat, display_path) {
            return false;
        }
    }
    if let Some(pat) = ipath_glob {
        if !glob_match(&pat.to_lowercase(), &display_path.to_lowercase()) {
            return false;
        }
    }
    true
}

/// Convert an absolute `path` returned by the walker back to the display form
/// that GNU find would print — i.e. relative to the start path.
///
/// If `resolved_path` is the absolute form of `start_path`, and `path` is
/// under `resolved_path`, we strip the absolute prefix and replace it with
/// the user-supplied `start_path` (e.g. ".").
fn relative_display_path(path: &Path, resolved_path: &Path, start_path: &str) -> String {
    let resolved = resolved_path;
    match path.strip_prefix(resolved) {
        Ok(rel) if rel.as_os_str().is_empty() => start_path.to_string(),
        Ok(rel) => format!("{}/{}", start_path.trim_end_matches('/'), rel.display()),
        Err(_) => path.to_string_lossy().into_owned(),
    }
}

/// Parse a relative integer like "+7", "-3", or "5".
fn parse_relative_int(value: &Value) -> Option<(char, u64)> {
    let s = match value {
        Value::Int(i) => return Some(('=', *i as u64)),
        Value::String(s) => s.as_str(),
        _ => return None,
    };

    if s.is_empty() {
        return None;
    }

    let first = s.chars().next()?;
    if first == '+' || first == '-' {
        let num: u64 = s[1..].parse().ok()?;
        Some((first, num))
    } else {
        let num: u64 = s.parse().ok()?;
        Some(('=', num))
    }
}

/// Apply the `-mtime`/`-size` predicates to an already-stat'd entry, matching
/// the walker's (lenient) semantics: a filter whose stat field is unavailable
/// passes. Used by the regular-file-operand and `-maxdepth 0` short-circuits so
/// they don't bypass these filters the way the main walker applies them.
fn passes_mtime_size(
    info: Option<&DirEntry>,
    mtime: Option<(char, u64)>,
    size: Option<(char, u64)>,
) -> bool {
    if let Some((sign, days)) = mtime
        && let Some(info) = info
        && let Some(modified) = info.modified
    {
        let age_days = modified.elapsed().map(|d| d.as_secs()).unwrap_or(0) / 86400;
        let ok = match sign {
            '+' => age_days > days,
            '-' => age_days < days,
            _ => age_days == days,
        };
        if !ok {
            return false;
        }
    }
    if let Some((sign, sz)) = size
        && let Some(info) = info
    {
        let ok = match sign {
            '+' => info.size > sz,
            '-' => info.size < sz,
            _ => info.size == sz,
        };
        if !ok {
            return false;
        }
    }
    true
}

/// Parse a size filter like "+1000", "-500", "1M", "+10K".
fn parse_size_filter(s: &str) -> Option<(char, u64)> {
    if s.is_empty() {
        return None;
    }

    let first = s.chars().next()?;
    let (sign, rest) = if first == '+' || first == '-' {
        (first, &s[1..])
    } else {
        ('=', s)
    };

    // Check for size suffixes
    let last = rest.chars().last()?;
    let (num_str, multiplier) = match last.to_ascii_uppercase() {
        'K' => (&rest[..rest.len()-1], 1024u64),
        'M' => (&rest[..rest.len()-1], 1024 * 1024),
        'G' => (&rest[..rest.len()-1], 1024 * 1024 * 1024),
        _ => (rest, 1),
    };

    let num: u64 = num_str.parse().ok()?;
    Some((sign, num * multiplier))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_test_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();

        // Create test structure
        mem.mkdir(Path::new("src")).await.unwrap();
        mem.mkdir(Path::new("src/lib")).await.unwrap();
        mem.mkdir(Path::new("test")).await.unwrap();
        mem.mkdir(Path::new(".hidden")).await.unwrap();

        mem.write(Path::new("src/main.rs"), b"fn main() {}")
            .await
            .unwrap();
        mem.write(Path::new("src/lib.rs"), b"pub mod lib;")
            .await
            .unwrap();
        mem.write(Path::new("src/lib/utils.rs"), b"pub fn util() {}")
            .await
            .unwrap();
        mem.write(Path::new("test/test_main.rs"), b"#[test]")
            .await
            .unwrap();
        mem.write(Path::new("README.md"), b"# Test").await.unwrap();
        mem.write(Path::new(".hidden/secret.txt"), b"secret")
            .await
            .unwrap();

        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_find_all() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));

        let result = Find.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("main.rs"));
        assert!(result.text_out().contains("lib.rs"));
        assert!(result.text_out().contains("README.md"));
    }

    #[tokio::test]
    async fn test_find_by_name() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));
        args.named.insert("name".to_string(), Value::String("*.rs".into()));

        let result = Find.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("main.rs"));
        assert!(result.text_out().contains("lib.rs"));
        assert!(result.text_out().contains("utils.rs"));
        assert!(!result.text_out().contains("README.md"));
    }

    #[tokio::test]
    async fn test_find_type_file() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));
        args.named.insert("type".to_string(), Value::String("f".into()));

        let result = Find.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("main.rs"));
        assert!(!result.text_out().contains("/src\n")); // src is a directory
    }

    #[tokio::test]
    async fn test_find_type_dir() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));
        args.named.insert("type".to_string(), Value::String("d".into()));

        let result = Find.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("src"));
        assert!(result.text_out().contains("lib"));
        assert!(!result.text_out().contains("main.rs"));
    }

    #[tokio::test]
    async fn test_find_maxdepth() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));
        args.named.insert("maxdepth".to_string(), Value::Int(1));
        args.named.insert("type".to_string(), Value::String("f".into()));

        let result = Find.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Files at depth 1 (directly under /src or /test)
        assert!(result.text_out().contains("main.rs"));
        // Files at depth 2 (under /src/lib) should NOT be present
        assert!(!result.text_out().contains("utils.rs"));
    }

    #[tokio::test]
    async fn test_find_nonexistent_path() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));

        let result = Find.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("No such file or directory"));
    }

    #[tokio::test]
    async fn test_find_hidden() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));

        let result = Find.execute(args, &mut ctx).await;
        assert!(result.ok());
        // find includes hidden files by default
        assert!(result.text_out().contains(".hidden"));
        assert!(result.text_out().contains("secret.txt"));
    }

    /// Create a ctx with build artifact dirs to test ignore filtering.
    async fn make_ctx_with_artifacts() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();

        mem.mkdir(Path::new("src")).await.unwrap();
        mem.mkdir(Path::new("target")).await.unwrap();
        mem.mkdir(Path::new("target/debug")).await.unwrap();
        mem.mkdir(Path::new("node_modules")).await.unwrap();
        mem.mkdir(Path::new("node_modules/foo")).await.unwrap();

        mem.write(Path::new("src/main.rs"), b"fn main() {}")
            .await
            .unwrap();
        mem.write(Path::new("target/debug/binary"), b"\x7fELF")
            .await
            .unwrap();
        mem.write(Path::new("node_modules/foo/index.js"), b"module.exports = {}")
            .await
            .unwrap();
        mem.write(Path::new("README.md"), b"# Test").await.unwrap();

        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_find_advisory_ignores_nothing() {
        // Advisory scope (default): find shows everything, even with config
        let mut ctx = make_ctx_with_artifacts().await;
        ctx.ignore_config = crate::ignore_config::IgnoreConfig::none();
        ctx.ignore_config.set_defaults(true); // defaults on, but scope is Advisory

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));

        let result = Find.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Advisory scope: find does NOT filter, even with defaults on
        assert!(result.text_out().contains("target"), "Advisory find should show target/");
        assert!(result.text_out().contains("node_modules"), "Advisory find should show node_modules/");
        assert!(result.text_out().contains("main.rs"));
    }

    #[tokio::test]
    async fn test_find_enforced_filters_defaults() {
        // Enforced scope (MCP default): find skips default-ignored dirs
        let mut ctx = make_ctx_with_artifacts().await;
        ctx.ignore_config = crate::ignore_config::IgnoreConfig::agent();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));

        let result = Find.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Enforced scope with defaults: target/ and node_modules/ are filtered
        assert!(!result.text_out().contains("target"), "Enforced find should skip target/");
        assert!(!result.text_out().contains("node_modules"), "Enforced find should skip node_modules/");
        // Source files still visible
        assert!(result.text_out().contains("main.rs"));
        assert!(result.text_out().contains("README.md"));
    }

    #[tokio::test]
    async fn test_find_enforced_but_inactive() {
        // Enforced scope but no config active: find shows everything
        let mut ctx = make_ctx_with_artifacts().await;
        let mut config = crate::ignore_config::IgnoreConfig::none();
        config.set_scope(crate::ignore_config::IgnoreScope::Enforced);
        // scope is Enforced but is_active() is false (no defaults, no files)
        ctx.ignore_config = config;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));

        let result = Find.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("target"), "Enforced but inactive should show target/");
        assert!(result.text_out().contains("node_modules"));
    }

    #[test]
    fn test_parse_size_filter() {
        assert_eq!(parse_size_filter("+1000"), Some(('+', 1000)));
        assert_eq!(parse_size_filter("-500"), Some(('-', 500)));
        assert_eq!(parse_size_filter("100"), Some(('=', 100)));
        assert_eq!(parse_size_filter("+1K"), Some(('+', 1024)));
        assert_eq!(parse_size_filter("+1M"), Some(('+', 1024 * 1024)));
        assert_eq!(parse_size_filter("2G"), Some(('=', 2 * 1024 * 1024 * 1024)));
    }

    #[test]
    fn test_parse_relative_int() {
        assert_eq!(parse_relative_int(&Value::Int(5)), Some(('=', 5)));
        assert_eq!(parse_relative_int(&Value::String("+7".into())), Some(('+', 7)));
        assert_eq!(parse_relative_int(&Value::String("-3".into())), Some(('-', 3)));
        assert_eq!(parse_relative_int(&Value::String("10".into())), Some(('=', 10)));
    }
}
