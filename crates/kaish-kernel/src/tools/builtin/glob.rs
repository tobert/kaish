//! glob — Expand glob patterns to matching file paths.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::backend_walker_fs::BackendWalkerFs;
use crate::interpreter::{EntryType, ExecResult, OutputData, OutputNode};
use crate::tools::builtin::read_repeatable_strings;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};
use crate::walker::{
    build_file_types, list_file_types, EntryTypes, FileWalker, GlobPath, IncludeExclude, WalkOptions,
};

/// Glob tool: expand glob patterns to matching file paths.
pub struct Glob;

/// clap-derived argv layer for glob.
#[derive(Parser, Debug)]
#[command(name = "glob", about = "Expand glob patterns to matching file paths")]
struct GlobArgs {
    /// Maximum depth to recurse.
    #[arg(short = 'd', long = "depth")]
    depth: Option<i64>,

    /// Include ignored files like .git, node_modules.
    #[arg(long = "no-ignore", visible_alias = "no_ignore")]
    no_ignore: bool,

    /// Include hidden files starting with `.`.
    #[arg(short = 'a', long = "hidden")]
    hidden: bool,

    /// Entry type: f=files, d=dirs, a=all.
    #[arg(id = "type", short = 't', long = "type")]
    type_: Option<String>,

    /// Filter to files of the given type(s), e.g. `--ftype rust`. Repeatable.
    /// File-type (rg-style, by extension) — distinct from `-t`/`--type`, which
    /// is *entry kind* (file/dir). See `--ftype-list`.
    #[arg(long = "ftype")]
    ftype: Vec<String>,

    /// Exclude files of the given type(s). Repeatable.
    #[arg(long = "ftype-not")]
    ftype_not: Vec<String>,

    /// List known file types (TYPE → globs) and exit. No pattern needed.
    #[arg(long = "ftype-list")]
    ftype_list: bool,

    /// Null-separated output.
    #[arg(short = '0', long = "null")]
    null: bool,

    /// Include pattern: only matches also matching one of these are kept
    /// (repeatable).
    #[arg(long = "include")]
    include: Vec<String>,

    /// Exclude pattern: matches are dropped, directories pruned (repeatable).
    #[arg(long = "exclude")]
    exclude: Vec<String>,

    #[command(flatten)]
    global: GlobalFlags,

    /// Glob pattern(s) to expand.
    pattern: Vec<String>,
}

#[async_trait]
impl Tool for Glob {
    fn name(&self) -> &str {
        "glob"
    }

    fn schema(&self) -> ToolSchema {
        // glob_passthrough: the argv binder hands bare patterns through as
        // written — without it, `glob **/*.rs` (as every example here spells
        // it) would bind the first *matching path* as the pattern.
        schema_from_clap(
            &GlobArgs::command(),
            "glob",
            "Expand glob patterns to matching file paths",
            [
                ("Find all Rust files", "glob **/*.rs"),
                ("Find in specific directory", "glob src/**/*.rs"),
                ("Multiple patterns", "glob **/*.rs **/*.toml"),
                ("Multiple extensions", "glob **/*.{rs,go,py}"),
                ("With depth limit", "glob **/*.rs -d 3"),
                ("Include hidden files", "glob **/*.rs -a"),
                ("Null-separated for xargs", "glob **/*.rs -0"),
                ("Exclude test files", "glob **/*.rs --exclude='*_test.rs'"),
            ],
        )
        .with_glob_passthrough()
    }

    async fn execute(&self, mut args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        // Tests poke args.named.insert("no-ignore", Value::Bool(true)); promote
        // such bool-typed named entries to flag form so clap accepts them.
        args.flagify_bool_named(&self.schema());

        let parsed = match GlobArgs::try_parse_from(
            std::iter::once("glob".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("glob: {e}")),
        };
        parsed.global.apply(ctx);

        // `--ftype-list`: emit the TYPE→globs table and exit, no pattern needed.
        if parsed.ftype_list {
            let rows: Vec<OutputNode> = list_file_types()
                .into_iter()
                .map(|(name, globs)| OutputNode::new(&name).with_cells(vec![globs.join(", ")]))
                .collect();
            let table = OutputData::table(vec!["TYPE".to_string(), "GLOBS".to_string()], rows);
            return ExecResult::with_output(table);
        }

        // Build the rg-style file-type filter from `--ftype`/`--ftype-not`.
        // Repeatable value flags arrive as `Json(Array)`; read them off the raw
        // args (shared with grep). An unknown type name is loud (exit 2).
        let ftype_select = read_repeatable_strings(&args, "ftype");
        let ftype_negate = read_repeatable_strings(&args, "ftype-not");
        let file_types = match build_file_types(&ftype_select, &ftype_negate) {
            Ok(t) => t,
            Err(e) => return ExecResult::failure(2, format!("glob: {e}")),
        };

        // Every positional is a pattern. The argv binder hands bare patterns
        // through as written (ToolSchema::glob_passthrough), so this is the
        // whole pattern list; Value-typed positionals are read off
        // `args.positional`, not the clap struct (to_argv is lossy).
        let patterns: Vec<String> = args
            .positional
            .iter()
            .map(crate::interpreter::value_to_string)
            .collect();
        if patterns.is_empty() {
            return ExecResult::failure(1, "glob: missing pattern argument");
        }

        // Parse options off the clap struct. A non-numeric `--depth` was
        // already a loud clap error; a negative one is refused here rather
        // than wrapping to "effectively unlimited".
        let max_depth: Option<usize> = match parsed.depth {
            Some(d) if d < 0 => {
                return ExecResult::failure(
                    2,
                    format!("glob: invalid --depth {d}: must be >= 0"),
                )
            }
            other => other.map(|d| d as usize),
        };

        let no_ignore = parsed.no_ignore;
        let include_hidden = parsed.hidden;
        let null_sep = parsed.null;

        // Entry types: f=files (default), d=dirs, a=all. Anything else used
        // to silently mean "files"; now it's a usage error.
        let entry_types = match parsed.type_.as_deref() {
            None | Some("f") => EntryTypes::files_only(),
            Some("d") => EntryTypes::dirs_only(),
            Some("a") => EntryTypes::all(),
            Some(other) => {
                return ExecResult::failure(
                    2,
                    format!("glob: invalid --type '{other}': expected f, d, or a"),
                )
            }
        };

        // Build the include/exclude filter. Repeatable value flags arrive as
        // `Json(Array)`; read them off the raw args like `--ftype` above.
        let mut filter = IncludeExclude::new();
        for pattern in read_repeatable_strings(&args, "include") {
            filter.include(&pattern);
        }
        for pattern in read_repeatable_strings(&args, "exclude") {
            filter.exclude(&pattern);
        }

        let fs = BackendWalkerFs(ctx.backend.as_ref());
        let mut nodes: Vec<OutputNode> = Vec::new();
        let mut seen: std::collections::HashSet<String> = std::collections::HashSet::new();

        for pattern in &patterns {
            let glob = match GlobPath::new(pattern) {
                Ok(g) => g,
                Err(e) => return ExecResult::failure(1, format!("glob: invalid pattern: {}", e)),
            };

            // Names are reported relative to the conventional root — `/` for an
            // anchored pattern, the cwd otherwise — regardless of where the walk
            // actually starts.
            let report_root = if glob.is_anchored() {
                ctx.resolve_path("/")
            } else {
                ctx.resolve_path(".")
            };

            // Walk from the pattern's deepest static directory, not from `/`.
            // Walking from `/` is O(filesystem) and — fatally — the walker skips
            // hidden intermediate directories, so an anchored pattern routed
            // through a hidden dir (e.g. tempfile's `/tmp/.tmpXXXX/*.txt`) matched
            // nothing. `split_static_dir` peels the literal leading components into
            // the walk root and leaves the remainder as the match pattern.
            let (static_dir, glob) = glob.split_static_dir();
            let root = if static_dir.as_os_str().is_empty() {
                report_root.clone()
            } else {
                report_root.join(&static_dir)
            };

            let options = WalkOptions {
                max_depth,
                entry_types,
                respect_gitignore: if no_ignore { false } else { ctx.ignore_config.auto_gitignore() },
                include_hidden,
                filter: filter.clone(),
                types: file_types.clone(),
                ..WalkOptions::default()
            };

            let mut walker = FileWalker::new(&fs, &root)
                .with_pattern(glob)
                .with_options(options);

            // Inject ignore filter from config (unless --no-ignore)
            if !no_ignore {
                if let Some(ignore_filter) = ctx.build_ignore_filter(&root).await {
                    walker = walker.with_ignore(ignore_filter);
                }
            }

            let paths = match walker.collect().await {
                Ok(p) => p,
                Err(e) => return ExecResult::failure(1, format!("glob: {}", e)),
            };

            // Strict-glob guarantee, per pattern: zero matches is an error
            // naming the pattern that missed, not silent success. This is
            // consistent with how the kernel treats a bare glob in argv
            // position (no matches → error). The `glob` builtin must enforce
            // the same contract so agents can rely on a non-zero exit to
            // detect misses.
            if paths.is_empty() {
                return ExecResult::failure(1, format!("glob: no matches for pattern '{pattern}'"));
            }

            // Build OutputNodes for each matched path (relative to root),
            // deduped across patterns by reported name.
            for p in &paths {
                let rel = p.strip_prefix(&report_root).unwrap_or(p);
                let name = rel.to_string_lossy().to_string();
                if !seen.insert(name.clone()) {
                    continue;
                }
                let entry_type = if p.extension().is_none() && name.ends_with('/') {
                    EntryType::Directory
                } else {
                    EntryType::File
                };
                nodes.push(OutputNode::new(name).with_entry_type(entry_type));
            }
        }

        // Build JSON array for structured pipeline flow (same pattern as seq/split/find)
        let json_array: Vec<serde_json::Value> = nodes
            .iter()
            .map(|n| serde_json::Value::String(n.name.clone()))
            .collect();

        // Null-separated mode returns plain text for xargs compatibility
        if null_sep {
            let output: String = nodes.iter()
                .map(|n| n.name.as_str())
                .collect::<Vec<_>>()
                .join("\0");
            return ExecResult::success(output);
        }

        let mut result = ExecResult::with_output(OutputData::nodes(nodes));
        result.data = Some(Value::Json(serde_json::Value::Array(json_array)));
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::path::Path;
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();

        mem.mkdir(Path::new("src")).await.unwrap();
        mem.mkdir(Path::new("src/lib")).await.unwrap();
        mem.mkdir(Path::new("test")).await.unwrap();

        mem.write(Path::new("src/main.rs"), b"fn main() {}")
            .await
            .unwrap();
        mem.write(Path::new("src/lib.rs"), b"pub mod lib;")
            .await
            .unwrap();
        mem.write(Path::new("src/lib/utils.rs"), b"pub fn util() {}")
            .await
            .unwrap();
        mem.write(Path::new("test/main_test.rs"), b"#[test]")
            .await
            .unwrap();
        mem.write(Path::new("README.md"), b"# Test").await.unwrap();
        mem.write(Path::new(".hidden"), b"secret").await.unwrap();

        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_glob_basic() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("**/*.rs".into()));

        let result = Glob.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("main.rs"));
        assert!(result.text_out().contains("lib.rs"));
        assert!(result.text_out().contains("utils.rs"));
        assert!(!result.text_out().contains("README.md"));
    }

    #[tokio::test]
    async fn test_glob_scoped() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("src/**/*.rs".into()));

        let result = Glob.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("main.rs"));
        assert!(result.text_out().contains("lib.rs"));
        assert!(!result.text_out().contains("main_test.rs"));
    }

    #[tokio::test]
    async fn test_glob_json_via_global_flag() {
        use crate::interpreter::{apply_output_format, OutputFormat};

        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("**/*.rs".into()));

        let result = Glob.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should have structured OutputData
        assert!(result.has_output());

        // Simulate global --json (handled by kernel)
        let result = apply_output_format(result, OutputFormat::Json);
        assert!(result.text_out().starts_with('['));
        assert!(result.text_out().ends_with(']'));
    }

    #[tokio::test]
    async fn test_glob_exclude() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("**/*.rs".into()));
        args.named
            .insert("exclude".to_string(), Value::String("*_test.rs".into()));

        let result = Glob.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("main.rs"));
        assert!(!result.text_out().contains("main_test.rs"));
    }

    #[tokio::test]
    async fn test_glob_hidden() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("*".into()));
        args.flags.insert("a".to_string());
        args.named
            .insert("no_ignore".to_string(), Value::Bool(true));

        let result = Glob.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains(".hidden"));
    }

    #[tokio::test]
    async fn test_glob_result_data() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("**/*.rs".into()));

        let result = Glob.execute(args, &mut ctx).await;
        assert!(result.ok());
        // result.data should be a JSON array of path strings
        let data = result.data.expect("glob should set result.data");
        if let Value::Json(serde_json::Value::Array(arr)) = data {
            assert!(!arr.is_empty(), "should have matched .rs files");
            // All entries should be strings
            for entry in &arr {
                assert!(entry.is_string(), "each entry should be a string: {:?}", entry);
            }
            // Should contain our test files
            let paths: Vec<&str> = arr.iter().filter_map(|v| v.as_str()).collect();
            assert!(paths.iter().any(|p| p.contains("main.rs")));
            assert!(paths.iter().any(|p| p.contains("lib.rs")));
        } else {
            panic!("Expected JSON array, got: {:?}", data);
        }
    }

    #[tokio::test]
    async fn test_glob_no_pattern() {
        let mut ctx = make_ctx().await;
        let args = ToolArgs::new();

        let result = Glob.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("missing pattern"));
    }

    #[tokio::test]
    async fn test_glob_anchored_through_hidden_dir() {
        // Regression: an absolute pattern routed through a HIDDEN intermediate
        // directory matched nothing. The walk started at `/` and the walker
        // skips hidden entries, so the `.work` component was never descended —
        // which is exactly how `tempfile`'s `/tmp/.tmpXXXX/*.txt` resolved to
        // zero matches. The fix walks from the pattern's static directory
        // (`/.work/data`) instead of `/`. See docs/issues.md.
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.mkdir(Path::new(".work")).await.unwrap();
        mem.mkdir(Path::new(".work/data")).await.unwrap();
        mem.mkdir(Path::new(".work/data/sub")).await.unwrap();
        mem.write(Path::new(".work/data/a.txt"), b"a").await.unwrap();
        mem.write(Path::new(".work/data/b.txt"), b"b").await.unwrap();
        // A deeper file must NOT be caught by a fixed-depth `*.txt`.
        mem.write(Path::new(".work/data/sub/c.txt"), b"c").await.unwrap();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/.work/data/*.txt".into()));
        let result = Glob.execute(args, &mut ctx).await;
        assert!(result.ok(), "glob failed: {}", result.err);

        let out = result.text_out();
        assert!(out.contains("a.txt"), "should match a.txt through hidden dir: {out:?}");
        assert!(out.contains("b.txt"), "should match b.txt through hidden dir: {out:?}");
        assert!(!out.contains("c.txt"), "*.txt must not match nested sub/c.txt: {out:?}");
    }

    #[tokio::test]
    async fn test_glob_anchored_exact_literal_file() {
        // All-literal anchored pattern: `split_static_dir` must keep the final
        // component as the match target (walk `/proj`, match `only.txt`) rather
        // than trying to descend into the file itself.
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.mkdir(Path::new("proj")).await.unwrap();
        mem.write(Path::new("proj/only.txt"), b"x").await.unwrap();
        mem.write(Path::new("proj/other.txt"), b"y").await.unwrap();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/proj/only.txt".into()));
        let result = Glob.execute(args, &mut ctx).await;
        assert!(result.ok(), "glob failed: {}", result.err);

        let out = result.text_out();
        assert!(out.contains("only.txt"), "should match the exact file: {out:?}");
        assert!(!out.contains("other.txt"), "must not match siblings: {out:?}");
    }

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
        mem.write(Path::new("target/debug/app.rs"), b"compiled")
            .await
            .unwrap();
        mem.write(Path::new("node_modules/foo/index.js"), b"module")
            .await
            .unwrap();

        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_glob_none_config_no_filtering() {
        // IgnoreConfig::none() — glob should return everything
        let mut ctx = make_ctx_with_artifacts().await;
        // Default is IgnoreConfig::none()
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("**/*.rs".into()));

        let result = Glob.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("main.rs"));
        // With none config, target/ files should be included
        assert!(result.text_out().contains("app.rs"), "none config should include target/debug/app.rs");
    }

    #[tokio::test]
    async fn test_glob_agent_config_filters_defaults() {
        // IgnoreConfig::agent() — glob should skip target/ and node_modules/
        let mut ctx = make_ctx_with_artifacts().await;
        ctx.ignore_config = crate::ignore_config::IgnoreConfig::agent();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("**/*.rs".into()));

        let result = Glob.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("main.rs"));
        // MCP config has defaults on — target/ should be filtered
        assert!(!result.text_out().contains("app.rs"), "agent config should filter target/debug/app.rs");
    }

    #[tokio::test]
    async fn test_glob_no_ignore_overrides_config() {
        // --no-ignore should bypass config filtering
        let mut ctx = make_ctx_with_artifacts().await;
        ctx.ignore_config = crate::ignore_config::IgnoreConfig::agent();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("**/*.rs".into()));
        args.flags.insert("no-ignore".to_string());

        let result = Glob.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("main.rs"));
        // --no-ignore overrides: target/ files should be visible
        assert!(result.text_out().contains("app.rs"), "--no-ignore should bypass config");
    }
}
