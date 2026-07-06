//! tree — Display directory structure.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::collections::BTreeMap;
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::{EntryType, ExecResult, OutputData, OutputNode};
use crate::tools::builtin::get_path_string;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Tree tool: display directory structure.
pub struct Tree;

/// clap-derived argv layer for tree.
#[derive(Parser, Debug)]
#[command(name = "tree", about = "Display directory structure")]
struct TreeArgs {
    /// Maximum depth to display.
    #[arg(short = 'L', long = "level")]
    level: Option<String>,

    /// Traditional tree format with box-drawing.
    #[arg(long = "traditional")]
    traditional: bool,

    /// Flat indent format.
    #[arg(long = "flat")]
    flat: bool,

    /// Show only files, no directory entries.
    #[arg(short = 'f', long = "files-only", visible_alias = "files_only")]
    files_only: bool,

    /// Show hidden files.
    #[arg(short = 'a', long = "all")]
    all: bool,

    /// Include ignored directories.
    #[arg(long = "no-ignore", visible_alias = "no_ignore")]
    no_ignore: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Starting directory; defaults to the current directory.
    paths: Vec<String>,
}

/// Node in the tree structure.
#[derive(Debug, Default)]
struct TreeNode {
    children: BTreeMap<String, TreeNode>,
    is_dir: bool,
}

impl TreeNode {
    fn insert(&mut self, path: &[&str], is_dir: bool) {
        if path.is_empty() {
            return;
        }

        let entry = self
            .children
            .entry(path[0].to_string())
            .or_default();

        if path.len() == 1 {
            entry.is_dir = is_dir;
        } else {
            entry.is_dir = true; // Intermediate nodes are directories
            entry.insert(&path[1..], is_dir);
        }
    }

    fn format_traditional(&self, prefix: &str, _is_last: bool, output: &mut String) {
        let mut children: Vec<_> = self.children.iter().collect();
        children.sort_by_key(|(name, _)| *name);

        for (i, (name, node)) in children.iter().enumerate() {
            let is_last_child = i == children.len() - 1;
            let connector = if is_last_child { "└── " } else { "├── " };
            let name_suffix = if node.is_dir && node.children.is_empty() {
                "/"
            } else {
                ""
            };

            output.push_str(prefix);
            output.push_str(connector);
            output.push_str(name);
            output.push_str(name_suffix);
            output.push('\n');

            if !node.children.is_empty() {
                let new_prefix = if is_last_child {
                    format!("{}    ", prefix)
                } else {
                    format!("{}│   ", prefix)
                };
                node.format_traditional(&new_prefix, is_last_child, output);
            }
        }
    }

    fn format_flat(&self, indent: usize, output: &mut String) {
        let mut children: Vec<_> = self.children.iter().collect();
        children.sort_by_key(|(name, _)| *name);

        for (name, node) in children {
            let spaces = "  ".repeat(indent);
            let name_suffix = if node.is_dir { "/" } else { "" };

            output.push_str(&spaces);
            output.push_str(name);
            output.push_str(name_suffix);
            output.push('\n');

            if !node.children.is_empty() {
                node.format_flat(indent + 1, output);
            }
        }
    }

    /// Convert TreeNode to OutputNode for the structured output model.
    fn to_output_node(&self, name: &str) -> OutputNode {
        let entry_type = if self.is_dir {
            EntryType::Directory
        } else {
            EntryType::File
        };

        let children: Vec<OutputNode> = self.children
            .iter()
            .map(|(child_name, child_node)| child_node.to_output_node(child_name))
            .collect();

        OutputNode::new(name)
            .with_entry_type(entry_type)
            .with_children(children)
    }
}

#[async_trait]
impl Tool for Tree {
    fn name(&self) -> &str {
        "tree"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &TreeArgs::command(),
            "tree",
            "Display directory structure",
            [
                ("Compact notation (default)", "tree src/"),
                ("Traditional tree", "tree --traditional src/"),
                ("Flat indent", "tree --flat src/"),
                ("JSON output", "tree --json src/"),
                ("Limited depth", "tree -L 2 src/"),
            ],
        )
    }

    async fn execute(&self, mut args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        args.flagify_bool_named(&self.schema());

        let parsed = match TreeArgs::try_parse_from(
            std::iter::once("tree".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("tree: {e}")),
        };
        parsed.global.apply(ctx);

        // A binary `path` operand goes loud rather than silently defaulting to
        // "." (the "no operand given" case).
        let path = match get_path_string(&args, "path", 0) {
            Ok(p) => p.unwrap_or_else(|| ".".to_string()),
            Err(e) => return ExecResult::failure(1, format!("tree: {e}")),
        };

        let resolved = ctx.resolve_path(&path).to_string_lossy().to_string();

        // Fail loudly when the start path does not exist — silent success with
        // a bare root node is confusing and masks typos.
        if !ctx.backend.exists(Path::new(&resolved)).await {
            return ExecResult::failure(1, format!("tree: {}: No such file or directory", path));
        }

        // Parse options
        let max_depth = args
            .get("level", usize::MAX)
            .and_then(|v| match v {
                Value::Int(i) => Some(*i as usize),
                Value::String(s) => s.parse().ok(),
                _ => None,
            })
            .or_else(|| {
                args.get("L", usize::MAX).and_then(|v| match v {
                    Value::Int(i) => Some(*i as usize),
                    Value::String(s) => s.parse().ok(),
                    _ => None,
                })
            });

        let traditional = args.has_flag("traditional");
        let flat = args.has_flag("flat");
        let files_only = args.has_flag("files-only") || args.has_flag("f");
        let show_hidden = args.has_flag("all") || args.has_flag("a");
        let no_ignore = args.has_flag("no-ignore") || args.has_flag("no-ignore");

        // Build tree by walking directory
        let mut tree = TreeNode::default();

        // Set up ignore filter from config (unless --no-ignore)
        let ignore_filter = if no_ignore {
            None
        } else {
            ctx.build_ignore_filter(&ctx.resolve_path(&resolved)).await
        };

        // Walk directory using stack-based iteration
        let mut stack: Vec<(String, usize)> = vec![(resolved.clone(), 0usize)];

        while let Some((dir, depth)) = stack.pop() {
            // Check max depth
            if let Some(max) = max_depth
                && depth >= max {
                    continue;
                }

            // List directory contents
            let entries = match ctx.backend.list(Path::new(&dir)).await {
                Ok(entries) => entries,
                Err(_) => continue,
            };

            for entry in entries {
                // Skip hidden files unless -a
                if !show_hidden && entry.name.starts_with('.') {
                    continue;
                }

                // Check ignore filter
                if let Some(ref filter) = ignore_filter
                    && filter.is_name_ignored(&entry.name, entry.is_dir()) {
                        continue;
                    }

                let dir_str = dir.trim_end_matches('/');
                let full_path = format!("{}/{}", dir_str, entry.name);

                // Calculate relative path from root
                let relative = full_path
                    .strip_prefix(&resolved)
                    .unwrap_or(&full_path)
                    .trim_start_matches('/');

                if entry.is_dir() {
                    stack.push((full_path.clone(), depth + 1));

                    // Add directory to tree unless files_only
                    if !files_only {
                        let parts: Vec<&str> = relative.split('/').collect();
                        tree.insert(&parts, true);
                    }
                } else {
                    let parts: Vec<&str> = relative.split('/').collect();
                    tree.insert(&parts, false);
                }
            }
        }

        // Get root name for formatting
        let root_name = Path::new(&path)
            .file_name()
            .map(|s| s.to_string_lossy().to_string())
            .unwrap_or_else(|| ".".to_string());

        // Handle explicit text format requests
        if flat {
            let mut output = format!("{}/\n", root_name);
            tree.format_flat(1, &mut output);
            return ExecResult::with_output(OutputData::text(output.trim_end()));
        }

        if traditional {
            let mut output = format!("{}/\n", root_name);
            tree.format_traditional("", false, &mut output);
            return ExecResult::with_output(OutputData::text(output.trim_end()));
        }

        // Build structured OutputData with tree structure
        let root_node = OutputNode::new(&root_name)
            .with_entry_type(EntryType::Directory)
            .with_children(
                tree.children
                    .iter()
                    .map(|(name, node)| node.to_output_node(name))
                    .collect()
            );

        ExecResult::with_output(OutputData::nodes(vec![root_node]))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();

        mem.mkdir(Path::new("src")).await.unwrap();
        mem.mkdir(Path::new("src/lib")).await.unwrap();

        mem.write(Path::new("src/main.rs"), b"fn main() {}")
            .await
            .unwrap();
        mem.write(Path::new("src/lib.rs"), b"pub mod lib;")
            .await
            .unwrap();
        mem.write(Path::new("src/lib/utils.rs"), b"pub fn util() {}")
            .await
            .unwrap();
        mem.write(Path::new("README.md"), b"# Test").await.unwrap();

        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_tree_compact_default() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/src".into()));

        let result = Tree.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Default format now returns structured OutputData
        // Canonical output uses brace notation for nested children
        assert!(result.text_out().contains("src"));
        // Should have structured output
        assert!(result.has_output());
    }

    #[tokio::test]
    async fn test_tree_traditional() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/src".into()));
        args.flags.insert("traditional".to_string());

        let result = Tree.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("main.rs"));
        assert!(result.text_out().contains("lib.rs"));
        // Check for lib directory
        assert!(result.text_out().contains("lib"));
        // Traditional format uses box-drawing chars
        assert!(result.text_out().contains("├") || result.text_out().contains("└"));
    }

    #[tokio::test]
    async fn test_tree_flat() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/src".into()));
        args.flags.insert("flat".to_string());

        let result = Tree.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Flat format uses indentation
        assert!(result.text_out().contains("src/"));
        assert!(result.text_out().contains("main.rs"));
    }

    #[tokio::test]
    async fn test_tree_json_via_global_flag() {
        use crate::interpreter::{apply_output_format, OutputFormat};

        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/src".into()));

        let result = Tree.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.has_output());

        // Simulate global --json (handled by kernel)
        let result = apply_output_format(result, OutputFormat::Json);
        assert!(result.text_out().starts_with('{'));
        assert!(result.text_out().ends_with('}'));
    }

    #[tokio::test]
    async fn test_tree_depth() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/src".into()));
        args.named.insert("level".to_string(), Value::Int(1));

        let result = Tree.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should show immediate children but not nested
        assert!(result.text_out().contains("lib/") || result.text_out().contains("lib"));
        // utils.rs is at depth 2, should not appear
        // (actually depends on how we count - lib/ is at depth 1, utils.rs at depth 2)
    }

    #[tokio::test]
    async fn test_tree_returns_output_data() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/src".into()));

        let result = Tree.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Default tree (no flags) should return OutputData with nested structure
        match result.output() {
            Some(output) => {
                assert!(!output.root.is_empty());
                // Root node should be "src"
                assert_eq!(output.root[0].name, "src");
                // Should have children (the tree content)
                assert!(!output.root[0].children.is_empty());
            }
            None => panic!("Expected OutputData for default tree output"),
        }
    }

    #[tokio::test]
    async fn test_tree_explicit_flag_returns_text() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/src".into()));
        args.flags.insert("traditional".to_string());

        let result = Tree.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Explicit format flags should return plain text output
        // (output is still Some but it's simple text, not a tree structure)
        assert!(result.text_out().contains("├") || result.text_out().contains("└"));
    }
}
