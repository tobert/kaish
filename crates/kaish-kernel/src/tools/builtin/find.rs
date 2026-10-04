//! find — Search for files in directory hierarchy.
//!
//! # Examples
//!
//! ```kaish
//! find /workspace                    # Find all files
//! find . -name "*.rs"                # Find by name pattern
//! find src -type f                   # Find files only
//! find . -type d                     # Find directories only
//! find . -type l                     # Find symlinks only
//! find . -maxdepth 2                 # Limit recursion depth
//! find . -name "*.rs" -type f        # Combine predicates
//! find . -mindepth 1                 # Skip start directory
//! find . -path '*/sub/*'             # Match full path glob
//! find . -ipath '*/SUB/*'            # Case-insensitive path glob
//! find . -type f -o -type d          # -o: either test
//! find . -name '*.rs' -a -size +1K   # -a: both tests (also implied by adjacency)
//! find . ! -name '*.log'             # !: negate the next test
//! find . -type f '(' -name '*.rs' -o -name '*.md' ')'   # group with quoted parens
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;
use std::sync::{Arc, Mutex, PoisonError};

use crate::ast::Value;
use crate::backend_walker_fs::BackendWalkerFs;
use crate::ignore_config::IgnoreScope;
use crate::interpreter::{EntryType, ExecResult, OutputData, OutputNode};
use crate::tools::{exec_context, note_skipped_mounts, schema_from_clap, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema};
use crate::walker::{EntryTypes, FileWalker, WalkOptions};
use kaish_glob::WalkerError;

use super::find_expr::{self, EntryView};

/// Find tool: searches for files in directory hierarchy.
pub struct Find;

/// clap-derived schema for find; it is never used to parse.
///
/// find's expression is ordered (`-o`, `!`, `( )`), so the tool binds its
/// argv verbatim and `find_expr` parses the words. These fields exist so the
/// published schema lists each test; their `///` text ships to models.
#[derive(Parser, Debug)]
#[command(name = "find", about = "Search for files in directory hierarchy")]
struct FindArgs {
    /// Name matches the glob (the last path component).
    #[arg(id = "name", long = "name")]
    _name: Option<String>,

    /// Name matches the glob, ignoring case.
    #[arg(id = "iname", long = "iname")]
    _iname: Option<String>,

    /// Entry kind: 'f' file, 'd' directory, 'l' symlink (a link is never
    /// reported as the kind of what it points to).
    #[arg(id = "type", long = "type")]
    _type: Option<String>,

    /// Maximum depth to descend; 0 is the start path only.
    #[arg(id = "maxdepth", long = "maxdepth")]
    _maxdepth: Option<String>,

    /// Minimum depth to print; 1 skips the start path.
    #[arg(id = "mindepth", long = "mindepth")]
    _mindepth: Option<String>,

    /// Modified time in days: +N older than N, -N newer than N, N exactly.
    #[arg(id = "mtime", long = "mtime")]
    _mtime: Option<String>,

    /// Size in bytes: +N larger than N, -N smaller than N; K, M, G suffixes.
    #[arg(id = "size", long = "size")]
    _size: Option<String>,

    /// Whole path matches the glob.
    #[arg(id = "path", long = "path", visible_alias = "wholename")]
    _path: Option<String>,

    /// Whole path matches the glob, ignoring case.
    #[arg(id = "ipath", long = "ipath")]
    _ipath: Option<String>,

    /// Descend into other mounts. By default the walk stays in the mount
    /// region where it starts and prints a mount point without entering it
    /// (see `set -o crossmounts`).
    #[arg(id = "cross-mounts", long = "cross-mounts")]
    _cross_mounts: bool,

    /// Stay in the mount region where the walk starts, even under
    /// `set -o crossmounts`. Also spelled -mount.
    #[arg(id = "xdev", long = "xdev", visible_alias = "mount")]
    _xdev: bool,

    #[command(flatten)]
    _global: GlobalFlags,

    /// Starting paths for the search; defaults to the current directory.
    #[arg(id = "paths")]
    _paths: Vec<String>,
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
            "Search for files in directory hierarchy. Tests join with -a (also implied), -o, ! and quoted '(' ')'",
            [
                ("Find all files", "find ."),
                ("Find by name pattern", "find src -name '*.rs'"),
                ("Find directories only", "find . -type d"),
                ("Either test", "find . -name '*.rs' -o -name '*.md'"),
                ("Negate a test", "find . -type f ! -name '*.log'"),
                ("Group tests", "find . -type f '(' -name '*.rs' -o -name '*.md' ')'"),
            ],
        )
        .with_verbatim_argv()
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let ctx = exec_context(ctx);
        let Some(raw_words) = args.words.as_ref() else {
            return ExecResult::failure(2, "find: argv was not bound verbatim");
        };
        if raw_words.iter().any(|w| matches!(w, Value::Bytes(_))) {
            return ExecResult::failure(2, "find: a binary value cannot be a path or a test argument");
        }
        let words = args.words_argv();
        let (operands, expression) = find_expr::split_operands(&words);
        let parsed = match find_expr::parse(expression) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("find: {e}")),
        };
        let start_paths: Vec<String> = if operands.is_empty() {
            vec![".".to_string()]
        } else {
            operands.to_vec()
        };

        let max_depth = parsed.depth.max;
        let gnu_mindepth = parsed.depth.min.unwrap_or(0);
        // GNU depth: the start path is 0 and its children are 1. The walker
        // counts the start directory's children as 0 and never emits the
        // start directory, so GNU N maps to walker N-1.
        let min_depth: Option<usize> = match gnu_mindepth {
            0 | 1 => None,
            n => Some(n - 1),
        };

        // find only respects ignore config in Enforced scope
        let respect_ignore = matches!(ctx.ignore_config.scope(), IgnoreScope::Enforced)
            && ctx.ignore_config.is_active();

        let mut nodes: Vec<OutputNode> = Vec::new();
        let mut json_array: Vec<serde_json::Value> = Vec::new();
        // GNU find reports an entry it cannot read or stat, keeps walking,
        // and exits 1 at the end.
        let walk_errors: Arc<Mutex<Vec<String>>> = Arc::new(Mutex::new(Vec::new()));
        let mut emit = |display: &str, entry_type: EntryType| {
            nodes.push(OutputNode::new(display).with_entry_type(entry_type));
            json_array.push(serde_json::Value::String(display.to_string()));
        };
        let mut skipped_mounts: Vec<std::path::PathBuf> = Vec::new();

        for start_path in &start_paths {
            let resolved_path = ctx.resolve_path(start_path);

            // lstat: the operand is classified by its own kind, like every
            // walked entry. A link to a directory is a leaf, not descended.
            let start_stat = match ctx.backend.lstat(Path::new(&resolved_path)).await {
                Ok(info) => info,
                Err(e) => {
                    record_walk_error(&walk_errors, start_path, &e.to_string());
                    continue;
                }
            };

            // A file operand, or a directory at -maxdepth 0, is the only
            // entry; it sits at depth 0, which -mindepth 1 excludes.
            if !start_stat.is_dir() || max_depth == Some(0) {
                if gnu_mindepth == 0 {
                    let entry = EntryView { display: start_path, info: Some(&start_stat) };
                    let print_count = find_expr::print_count(&parsed, &entry);
                    if print_count > 0 {
                        let entry_type = if start_stat.is_symlink() {
                            EntryType::Symlink
                        } else if start_stat.is_dir() {
                            EntryType::Directory
                        } else {
                            EntryType::File
                        };
                        for _ in 0..print_count {
                            emit(start_path, entry_type);
                        }
                    }
                }
                continue;
            }

            // GNU `-maxdepth N` keeps depth N; the walker's limit counts the
            // start directory's children as 0, so it is one lower.
            let options = WalkOptions {
                max_depth: max_depth.map(|n| n.saturating_sub(1)),
                min_depth,
                entry_types: EntryTypes::all(),
                include_hidden: true, // find includes hidden by default
                respect_gitignore: if respect_ignore {
                    ctx.ignore_config.auto_gitignore()
                } else {
                    false
                },
                on_error: Some({
                    let walk_errors = Arc::clone(&walk_errors);
                    let start_path = start_path.clone();
                    let resolved_path = resolved_path.clone();
                    Arc::new(move |path: &Path, err: &WalkerError| {
                        let shown = relative_display_path(path, &resolved_path, &start_path);
                        let reason = match err {
                            WalkerError::Io(message) => message.clone(),
                            other => other.to_string(),
                        };
                        record_walk_error(&walk_errors, &shown, &reason);
                    })
                }),
                cross_mounts: parsed.cross_mounts.unwrap_or_else(|| ctx.walk_crosses_mounts(false)),
                ..WalkOptions::default()
            };

            let fs = BackendWalkerFs(ctx.backend.as_ref());
            let mut walker = FileWalker::new(&fs, &resolved_path).with_options(options);

            if respect_ignore {
                if let Some(ignore_filter) = ctx.build_ignore_filter(&resolved_path).await {
                    walker = walker.with_ignore(ignore_filter);
                }
            }

            let (paths, skipped) = match walker.walk().await {
                Ok(walk) => (walk.paths, walk.skipped_mounts),
                Err(e) => return ExecResult::failure(1, format!("find: {}", e)),
            };
            skipped_mounts.extend(skipped);

            for path in paths {
                // The walker returns absolute paths; print them the way the
                // operand was written, as GNU find does.
                let display_path = relative_display_path(&path, &resolved_path, start_path);

                // lstat, not stat: a symlink is classified by its own kind,
                // never by the kind of what it points to. An entry that
                // cannot be stat'ed is reported and skipped, as GNU find does.
                let info = match ctx.backend.lstat(&path).await {
                    Ok(info) => info,
                    Err(e) => {
                        record_walk_error(&walk_errors, &display_path, &e.to_string());
                        continue;
                    }
                };
                let info = Some(info);
                let entry = EntryView { display: &display_path, info: info.as_ref() };
                let print_count = find_expr::print_count(&parsed, &entry);
                if print_count == 0 {
                    continue;
                }

                let entry_type = info
                    .map(|i| {
                        if i.is_symlink() {
                            EntryType::Symlink
                        } else if i.is_dir() {
                            EntryType::Directory
                        } else {
                            EntryType::File
                        }
                    })
                    .unwrap_or(EntryType::File);
                for _ in 0..print_count {
                    emit(&display_path, entry_type);
                }
            }
        }

        let output = OutputData::nodes(nodes);
        let mut result = ExecResult::with_output(output);
        let walk_errors = walk_errors.lock().unwrap_or_else(PoisonError::into_inner);
        if !walk_errors.is_empty() {
            result.code = 1;
            result.err = walk_errors.join("");
        }
        drop(walk_errors);
        result.data = Some(Value::Json(serde_json::Value::Array(json_array)));
        // Text is the default here; `--json` serializes each name as its own
        // JSON string and never joins them by newline, so it stays the
        // documented lossless way past a newline-bearing name. See
        // `guard_no_newline_names`.
        if ctx.output_format.is_none()
            && let Some(output) = result.output()
            && let Err(e) = super::guard_no_newline_names("find", output)
        {
            return ExecResult::failure(2, e);
        }
        note_skipped_mounts(&mut result, "find", skipped_mounts);
        result
    }
}

/// Record `find: 'path': reason` for an entry that could not be read or stat'ed.
fn record_walk_error(
    errors: &Mutex<Vec<String>>,
    shown: &str,
    reason: &str,
) {
    let mut errors = errors.lock().unwrap_or_else(PoisonError::into_inner);
    errors.push(format!("find: '{shown}': {reason}\n"));
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tools::ExecContext;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn verbatim(words: &[&str]) -> ToolArgs {
        let mut args = ToolArgs::new();
        args.words = Some(words.iter().map(|w| Value::String((*w).into())).collect());
        args
    }

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
        let args = verbatim(&["/"]);

        let result = Find.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("main.rs"));
        assert!(result.text_out().contains("lib.rs"));
        assert!(result.text_out().contains("README.md"));
    }

    #[tokio::test]
    async fn test_find_by_name() {
        let mut ctx = make_test_ctx().await;
        let args = verbatim(&["/", "-name", "*.rs"]);

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
        let args = verbatim(&["/", "-type", "f"]);

        let result = Find.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("main.rs"));
        assert!(!result.text_out().contains("/src\n")); // src is a directory
    }

    #[tokio::test]
    async fn test_find_type_dir() {
        let mut ctx = make_test_ctx().await;
        let args = verbatim(&["/", "-type", "d"]);

        let result = Find.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("src"));
        assert!(result.text_out().contains("lib"));
        assert!(!result.text_out().contains("main.rs"));
    }

    #[tokio::test]
    async fn test_find_maxdepth() {
        let mut ctx = make_test_ctx().await;
        let args = verbatim(&["/", "-maxdepth", "2", "-type", "f"]);

        let result = Find.execute(args, &mut ctx).await;
        assert!(result.ok());
        // GNU depth 2: /src/main.rs
        assert!(result.text_out().contains("main.rs"));
        // GNU depth 3 (under /src/lib) should NOT be present
        assert!(!result.text_out().contains("utils.rs"));
    }

    #[tokio::test]
    async fn test_find_nonexistent_path() {
        let mut ctx = make_test_ctx().await;
        let args = verbatim(&["/nonexistent"]);

        let result = Find.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.starts_with("find: '/nonexistent': not found"), "{}", result.err);
    }

    #[tokio::test]
    async fn test_find_hidden() {
        let mut ctx = make_test_ctx().await;
        let args = verbatim(&["/"]);

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

        let args = verbatim(&["/"]);

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

        let args = verbatim(&["/"]);

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

        let args = verbatim(&["/"]);

        let result = Find.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("target"), "Enforced but inactive should show target/");
        assert!(result.text_out().contains("node_modules"));
    }

    /// Lists `ghost.txt` but refuses to stat it, as an entry removed or
    /// locked after the listing would.
    struct GhostFs(MemoryFs);

    #[async_trait]
    impl Filesystem for GhostFs {
        async fn read(&self, path: &Path) -> std::io::Result<Vec<u8>> {
            self.0.read(path).await
        }
        async fn write(&self, path: &Path, data: &[u8]) -> std::io::Result<()> {
            self.0.write(path, data).await
        }
        async fn replace(&self, path: &Path, data: &[u8]) -> std::io::Result<()> {
            self.0.replace(path, data).await
        }
        async fn list(&self, path: &Path) -> std::io::Result<Vec<kaish_types::DirEntry>> {
            self.0.list(path).await
        }
        async fn stat(&self, path: &Path) -> std::io::Result<kaish_types::DirEntry> {
            self.lstat(path).await
        }
        async fn lstat(&self, path: &Path) -> std::io::Result<kaish_types::DirEntry> {
            if path.ends_with("ghost.txt") {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::PermissionDenied,
                    "locked by test",
                ));
            }
            self.0.lstat(path).await
        }
        async fn mkdir(&self, path: &Path) -> std::io::Result<()> {
            self.0.mkdir(path).await
        }
        async fn remove(&self, path: &Path) -> std::io::Result<()> {
            self.0.remove(path).await
        }
        fn read_only(&self) -> bool {
            false
        }
    }

    #[rstest::rstest]
    #[case::mtime(&["/", "-mtime", "-1"][..])]
    #[case::size(&["/", "-size", "-1M"][..])]
    #[case::no_test(&["/"][..])]
    #[tokio::test]
    async fn test_find_reports_and_skips_unstattable_entry(#[case] words: &[&str]) {
        let mem = MemoryFs::new();
        mem.write(Path::new("ok.txt"), b"x").await.unwrap();
        mem.write(Path::new("ghost.txt"), b"x").await.unwrap();
        let mut vfs = VfsRouter::new();
        vfs.mount("/", GhostFs(mem));
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let result = Find.execute(verbatim(words), &mut ctx).await;
        let out = result.text_out();
        assert!(!out.contains("ghost.txt"), "unstattable entry printed: {out}");
        assert!(out.contains("ok.txt"), "walk must go on: {out}");
        assert_eq!(result.code, 1);
        assert_eq!(result.err, "find: '/ghost.txt': permission denied: locked by test\n");
    }
}
