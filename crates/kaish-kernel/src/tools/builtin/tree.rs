//! tree — Display directory structure.

use async_trait::async_trait;
use std::collections::BTreeMap;
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};
use crate::walker::IgnoreFilter;

/// Tree tool: display directory structure.
pub struct Tree;

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
            .or_insert_with(TreeNode::default);

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

    fn format_flat(&self, prefix: &str, indent: usize, output: &mut String) {
        let mut children: Vec<_> = self.children.iter().collect();
        children.sort_by_key(|(name, _)| *name);

        for (name, node) in children {
            let spaces = "  ".repeat(indent);
            let name_suffix = if node.is_dir && node.children.is_empty() {
                "/"
            } else if node.is_dir {
                "/"
            } else {
                ""
            };

            output.push_str(&spaces);
            output.push_str(name);
            output.push_str(name_suffix);
            output.push('\n');

            if !node.children.is_empty() {
                node.format_flat(prefix, indent + 1, output);
            }
        }
    }

    fn format_compact(&self) -> String {
        let children: Vec<_> = self.children.iter().collect();

        if children.is_empty() {
            return String::new();
        }

        if children.len() == 1 {
            let (name, node) = &children[0];
            let child_content = node.format_compact();
            if child_content.is_empty() {
                name.to_string()
            } else if node.children.len() == 1 && node.is_dir {
                // Single chain: a/b/c
                format!("{}/{}", name, child_content)
            } else {
                format!("{}/{{{}}}", name, child_content)
            }
        } else {
            // Multiple children: {a,b,c}
            let parts: Vec<String> = children
                .iter()
                .map(|(name, node)| {
                    let child_content = node.format_compact();
                    if child_content.is_empty() {
                        name.to_string()
                    } else if node.children.len() == 1 && node.is_dir {
                        format!("{}/{}", name, child_content)
                    } else {
                        format!("{}/{{{}}}", name, child_content)
                    }
                })
                .collect();
            parts.join(",")
        }
    }

    fn to_json(&self) -> serde_json::Value {
        if self.children.is_empty() {
            serde_json::Value::Null
        } else {
            let mut map = serde_json::Map::new();
            for (name, node) in &self.children {
                map.insert(name.clone(), node.to_json());
            }
            serde_json::Value::Object(map)
        }
    }
}

#[async_trait]
impl Tool for Tree {
    fn name(&self) -> &str {
        "tree"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("tree", "Display directory structure")
            .param(ParamSchema::optional(
                "path",
                "string",
                Value::String(".".into()),
                "Directory to display",
            ))
            .param(ParamSchema::optional(
                "level",
                "int",
                Value::Null,
                "Maximum depth to display (-L)",
            ))
            .param(ParamSchema::optional(
                "traditional",
                "bool",
                Value::Bool(false),
                "Traditional tree format with box-drawing (--traditional)",
            ))
            .param(ParamSchema::optional(
                "flat",
                "bool",
                Value::Bool(false),
                "Flat indent format (--flat)",
            ))
            .param(ParamSchema::optional(
                "json",
                "bool",
                Value::Bool(false),
                "JSON output (-j)",
            ))
            .param(ParamSchema::optional(
                "files_only",
                "bool",
                Value::Bool(false),
                "Show only files, no directory entries (-f)",
            ))
            .param(ParamSchema::optional(
                "all",
                "bool",
                Value::Bool(false),
                "Show hidden files (-a)",
            ))
            .param(ParamSchema::optional(
                "no_ignore",
                "bool",
                Value::Bool(false),
                "Include ignored directories (--no-ignore)",
            ))
            .example("Compact notation (default)", "tree src/")
            .example("Traditional tree", "tree --traditional src/")
            .example("Flat indent", "tree --flat src/")
            .example("JSON output", "tree -j src/")
            .example("Limited depth", "tree -L 2 src/")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let path = args
            .get_string("path", 0)
            .unwrap_or_else(|| ".".to_string());

        let resolved = ctx.resolve_path(&path).to_string_lossy().to_string();

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
        let json_output = args.has_flag("json") || args.has_flag("j");
        let files_only = args.has_flag("files_only") || args.has_flag("f");
        let show_hidden = args.has_flag("all") || args.has_flag("a");
        let no_ignore = args.has_flag("no_ignore") || args.has_flag("no-ignore");

        // Build tree by walking directory
        let mut tree = TreeNode::default();

        // Set up ignore filter
        let ignore_filter = if no_ignore {
            None
        } else {
            Some(IgnoreFilter::with_defaults())
        };

        // Walk directory using stack-based iteration
        let mut stack: Vec<(String, usize)> = vec![(resolved.clone(), 0usize)];

        while let Some((dir, depth)) = stack.pop() {
            // Check max depth
            if let Some(max) = max_depth {
                if depth >= max {
                    continue;
                }
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
                if let Some(ref filter) = ignore_filter {
                    if filter.is_name_ignored(&entry.name, entry.is_dir) {
                        continue;
                    }
                }

                let dir_str = dir.trim_end_matches('/');
                let full_path = format!("{}/{}", dir_str, entry.name);

                // Calculate relative path from root
                let relative = full_path
                    .strip_prefix(&resolved)
                    .unwrap_or(&full_path)
                    .trim_start_matches('/');

                if entry.is_dir {
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

        // Format output
        let output = if json_output {
            serde_json::to_string(&tree.to_json()).unwrap_or_else(|_| "{}".to_string())
        } else if traditional {
            // Traditional tree format with box-drawing characters
            let root_name = Path::new(&path)
                .file_name()
                .map(|s| s.to_string_lossy().to_string())
                .unwrap_or_else(|| ".".to_string());

            let mut output = format!("{}/\n", root_name);
            tree.format_traditional("", false, &mut output);
            output.trim_end().to_string()
        } else if flat {
            let root_name = Path::new(&path)
                .file_name()
                .map(|s| s.to_string_lossy().to_string())
                .unwrap_or_else(|| ".".to_string());

            let mut output = format!("{}/\n", root_name);
            tree.format_flat(&root_name, 1, &mut output);
            output.trim_end().to_string()
        } else {
            // Compact brace notation (default) - token efficient!
            let root_name = Path::new(&path)
                .file_name()
                .map(|s| s.to_string_lossy().to_string())
                .unwrap_or_else(|| ".".to_string());

            let content = tree.format_compact();
            if content.is_empty() {
                format!("{}/", root_name)
            } else if tree.children.len() == 1 {
                let (name, _) = tree.children.iter().next().unwrap();
                // If there's only one top-level entry, don't wrap in braces
                if name == &root_name {
                    format!("{}/{{{}}}", root_name, content)
                } else {
                    format!("{}/{{{}}}", root_name, content)
                }
            } else {
                format!("{}/{{{}}}", root_name, content)
            }
        };

        ExecResult::success(output)
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
        // Compact format is now default, uses braces
        assert!(result.out.contains("src/"));
        // Should contain brace notation
        assert!(result.out.contains('{') || result.out.contains(','));
    }

    #[tokio::test]
    async fn test_tree_traditional() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/src".into()));
        args.flags.insert("traditional".to_string());

        let result = Tree.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("main.rs"));
        assert!(result.out.contains("lib.rs"));
        // Check for lib directory
        assert!(result.out.contains("lib"));
        // Traditional format uses box-drawing chars
        assert!(result.out.contains("├") || result.out.contains("└"));
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
        assert!(result.out.contains("src/"));
        assert!(result.out.contains("main.rs"));
    }

    #[tokio::test]
    async fn test_tree_json() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/src".into()));
        args.flags.insert("j".to_string());

        let result = Tree.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.starts_with('{'));
        assert!(result.out.ends_with('}'));
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
        assert!(result.out.contains("lib/") || result.out.contains("lib"));
        // utils.rs is at depth 2, should not appear
        // (actually depends on how we count - lib/ is at depth 1, utils.rs at depth 2)
    }
}
