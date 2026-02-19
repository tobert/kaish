//! glob — Expand glob patterns to matching file paths.

use async_trait::async_trait;

use crate::ast::Value;
use crate::backend_walker_fs::BackendWalkerFs;
use crate::interpreter::{EntryType, ExecResult, OutputData, OutputNode};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};
use crate::walker::{EntryTypes, FileWalker, GlobPath, IncludeExclude, WalkOptions};

/// Glob tool: expand glob patterns to matching file paths.
pub struct Glob;

#[async_trait]
impl Tool for Glob {
    fn name(&self) -> &str {
        "glob"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("glob", "Expand glob patterns to matching file paths")
            .param(ParamSchema::required(
                "pattern",
                "string",
                "Glob pattern (e.g., **/*.rs, src/**/*.go)",
            ))
            .param(ParamSchema::optional(
                "depth",
                "int",
                Value::Null,
                "Maximum depth to recurse (-d)",
            ))
            .param(ParamSchema::optional(
                "no_ignore",
                "bool",
                Value::Bool(false),
                "Include ignored files like .git, node_modules (--no-ignore)",
            ))
            .param(ParamSchema::optional(
                "hidden",
                "bool",
                Value::Bool(false),
                "Include hidden files starting with . (-a)",
            ))
            .param(ParamSchema::optional(
                "type",
                "string",
                Value::String("f".into()),
                "Entry type: f=files, d=dirs, a=all (-t)",
            ))
            .param(ParamSchema::optional(
                "null",
                "bool",
                Value::Bool(false),
                "Null-separated output (-0)",
            ))
            .param(ParamSchema::optional(
                "include",
                "string",
                Value::Null,
                "Include pattern (can be repeated) (--include)",
            ))
            .param(ParamSchema::optional(
                "exclude",
                "string",
                Value::Null,
                "Exclude pattern (can be repeated) (--exclude)",
            ))
            .example("Find all Rust files", "glob **/*.rs")
            .example("Find in specific directory", "glob src/**/*.rs")
            .example("Multiple extensions", "glob **/*.{rs,go,py}")
            .example("With depth limit", "glob **/*.rs -d 3")
            .example("Include hidden files", "glob **/*.rs -a")
            .example("Null-separated for xargs", "glob **/*.rs -0")
            .example(
                "Exclude test files",
                "glob **/*.rs --exclude='*_test.rs'",
            )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // Get the pattern
        let pattern = match args.get_string("pattern", 0) {
            Some(p) => p,
            None => return ExecResult::failure(1, "glob: missing pattern argument"),
        };

        // Parse the glob pattern
        let glob = match GlobPath::new(&pattern) {
            Ok(g) => g,
            Err(e) => return ExecResult::failure(1, format!("glob: invalid pattern: {}", e)),
        };

        // Determine the root directory for walking:
        // If pattern is anchored (/foo/bar), start from /
        // Otherwise, start from current directory
        //
        // Note: We could optimize by using static_prefix, but that requires
        // stripping the prefix from the pattern too. For now, keep it simple.
        let root = if glob.is_anchored() {
            ctx.resolve_path("/")
        } else {
            ctx.resolve_path(".")
        };

        // Parse options
        let max_depth = args.get("depth", usize::MAX).and_then(|v| match v {
            Value::Int(i) => Some(*i as usize),
            Value::String(s) => s.parse().ok(),
            _ => None,
        });

        // Also check short flags
        let max_depth = max_depth.or_else(|| {
            args.get("d", usize::MAX).and_then(|v| match v {
                Value::Int(i) => Some(*i as usize),
                Value::String(s) => s.parse().ok(),
                _ => None,
            })
        });

        let no_ignore = args.has_flag("no_ignore") || args.has_flag("no-ignore");
        let include_hidden = args.has_flag("hidden") || args.has_flag("a");
        let null_sep = args.has_flag("null") || args.has_flag("0");

        // Entry types
        let entry_types = match args.get_string("type", usize::MAX).as_deref() {
            Some("d") => EntryTypes::dirs_only(),
            Some("a") => EntryTypes::all(),
            _ => {
                // Check -t flag
                if args.has_flag("t") {
                    if let Some(Value::String(t)) = args.get("t", usize::MAX) {
                        match t.as_str() {
                            "d" => EntryTypes::dirs_only(),
                            "a" => EntryTypes::all(),
                            _ => EntryTypes::files_only(),
                        }
                    } else {
                        EntryTypes::files_only()
                    }
                } else {
                    EntryTypes::files_only()
                }
            }
        };

        // Build include/exclude filter
        let mut filter = IncludeExclude::new();

        // Handle include patterns
        if let Some(Value::String(inc)) = args.get("include", usize::MAX) {
            filter.include(inc);
        }

        // Handle exclude patterns
        if let Some(Value::String(exc)) = args.get("exclude", usize::MAX) {
            filter.exclude(exc);
        }

        let options = WalkOptions {
            max_depth,
            entry_types,
            respect_gitignore: if no_ignore { false } else { ctx.ignore_config.auto_gitignore() },
            include_hidden,
            filter,
            ..WalkOptions::default()
        };

        // Create walker
        let fs = BackendWalkerFs(ctx.backend.as_ref());
        let mut walker = FileWalker::new(&fs, &root)
            .with_pattern(glob)
            .with_options(options);

        // Inject ignore filter from config (unless --no-ignore)
        if !no_ignore {
            if let Some(ignore_filter) = ctx.build_ignore_filter(&root).await {
                walker = walker.with_ignore(ignore_filter);
            }
        }

        // Collect results
        let paths = match walker.collect().await {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(1, format!("glob: {}", e)),
        };

        // Build OutputNodes for each matched path (relative to root)
        let nodes: Vec<OutputNode> = paths
            .iter()
            .map(|p| {
                let rel = p.strip_prefix(&root).unwrap_or(p);
                let name = rel.to_string_lossy().to_string();
                let entry_type = if p.extension().is_none() && name.ends_with('/') {
                    EntryType::Directory
                } else {
                    EntryType::File
                };
                OutputNode::new(name).with_entry_type(entry_type)
            })
            .collect();

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
        assert!(result.out.contains("main.rs"));
        assert!(result.out.contains("lib.rs"));
        assert!(result.out.contains("utils.rs"));
        assert!(!result.out.contains("README.md"));
    }

    #[tokio::test]
    async fn test_glob_scoped() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("src/**/*.rs".into()));

        let result = Glob.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("main.rs"));
        assert!(result.out.contains("lib.rs"));
        assert!(!result.out.contains("main_test.rs"));
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
        assert!(result.output.is_some());

        // Simulate global --json (handled by kernel)
        let result = apply_output_format(result, OutputFormat::Json);
        assert!(result.out.starts_with('['));
        assert!(result.out.ends_with(']'));
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
        assert!(result.out.contains("main.rs"));
        assert!(!result.out.contains("main_test.rs"));
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
        assert!(result.out.contains(".hidden"));
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
        assert!(result.out.contains("main.rs"));
        // With none config, target/ files should be included
        assert!(result.out.contains("app.rs"), "none config should include target/debug/app.rs");
    }

    #[tokio::test]
    async fn test_glob_mcp_config_filters_defaults() {
        // IgnoreConfig::mcp() — glob should skip target/ and node_modules/
        let mut ctx = make_ctx_with_artifacts().await;
        ctx.ignore_config = crate::ignore_config::IgnoreConfig::mcp();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("**/*.rs".into()));

        let result = Glob.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("main.rs"));
        // MCP config has defaults on — target/ should be filtered
        assert!(!result.out.contains("app.rs"), "mcp config should filter target/debug/app.rs");
    }

    #[tokio::test]
    async fn test_glob_no_ignore_overrides_config() {
        // --no-ignore should bypass config filtering
        let mut ctx = make_ctx_with_artifacts().await;
        ctx.ignore_config = crate::ignore_config::IgnoreConfig::mcp();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("**/*.rs".into()));
        args.flags.insert("no_ignore".to_string());

        let result = Glob.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("main.rs"));
        // --no-ignore overrides: target/ files should be visible
        assert!(result.out.contains("app.rs"), "--no-ignore should bypass config");
    }
}
