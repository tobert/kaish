//! Structured output model (Tree-of-Tables) and output format handling.
//!
//! The unified output model uses `OutputData` containing a tree of `OutputNode`s:
//!
//! - **Builtins**: Pure data producers returning `OutputData`
//! - **Frontends**: Handle all rendering (REPL, MCP, kaijutsu)

use serde::{Deserialize, Serialize};

use crate::result::ExecResult;

// ============================================================
// Structured Output (Tree-of-Tables Model)
// ============================================================

/// Entry type for rendering hints (colors, icons).
///
/// This unified enum is used by both the new OutputNode system
/// and the legacy DisplayHint::Table for backward compatibility.
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(rename_all = "lowercase")]
pub enum EntryType {
    /// Generic text content.
    #[default]
    Text,
    /// Regular file.
    File,
    /// Directory.
    Directory,
    /// Executable file.
    Executable,
    /// Symbolic link.
    Symlink,
}

/// A node in the output tree.
///
/// All fields are always serialized (no skip_serializing_if) for predictable shape
/// across JSON, postcard, and bincode formats.
///
/// `text` is Option<String> because None and Some("") are semantically distinct:
/// - None: this is a named entry (file listing, table row), not a text node
/// - Some(""): this IS a text node whose content is empty (e.g. `echo ""`)
///
/// The `is_text_only()` method depends on this distinction.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(default)]
pub struct OutputNode {
    /// Primary identifier (filename, key, label).
    pub name: String,
    /// Rendering hint (colors, icons).
    pub entry_type: EntryType,
    /// Text content (for echo, cat, exec).
    pub text: Option<String>,
    /// Additional columns (for ls -l, ps, env).
    pub cells: Vec<String>,
    /// Child nodes (for tree, find).
    pub children: Vec<OutputNode>,
}

impl OutputNode {
    /// Create a new node with a name.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ..Default::default()
        }
    }

    /// Create a text-only node (for echo, cat, etc.).
    pub fn text(content: impl Into<String>) -> Self {
        Self {
            text: Some(content.into()),
            ..Default::default()
        }
    }

    /// Set the entry type for rendering hints.
    pub fn with_entry_type(mut self, entry_type: EntryType) -> Self {
        self.entry_type = entry_type;
        self
    }

    /// Set additional columns for tabular output.
    pub fn with_cells(mut self, cells: Vec<String>) -> Self {
        self.cells = cells;
        self
    }

    /// Set child nodes for tree output.
    pub fn with_children(mut self, children: Vec<OutputNode>) -> Self {
        self.children = children;
        self
    }

    /// Set text content.
    pub fn with_text(mut self, text: impl Into<String>) -> Self {
        self.text = Some(text.into());
        self
    }

    /// Check if this is a text-only node.
    pub fn is_text_only(&self) -> bool {
        self.text.is_some() && self.name.is_empty() && self.cells.is_empty() && self.children.is_empty()
    }

    /// Check if this node has children.
    pub fn has_children(&self) -> bool {
        !self.children.is_empty()
    }

    /// Get the display name, potentially with text content.
    pub fn display_name(&self) -> &str {
        if self.name.is_empty() {
            self.text.as_deref().unwrap_or("")
        } else {
            &self.name
        }
    }
}

/// Structured output data from a command.
///
/// This is the top-level structure for command output.
/// It contains optional column headers and a list of root nodes.
///
/// `headers` is Option<Vec<String>> because None means "not tabular" while
/// Some(vec![]) means "tabular with no column headers." The rendering dispatch
/// in to_json() and the REPL formatter branch on this distinction.
///
/// # Rendering Rules
///
/// | Structure | Interactive | Piped/Model |
/// |-----------|-------------|-------------|
/// | Single node with `text` | Print text | Print text |
/// | Flat nodes, `name` only | Multi-column, colored | One per line |
/// | Flat nodes with `cells` | Aligned table | TSV or names only |
/// | Nested `children` | Box-drawing tree | Brace notation |
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(default)]
pub struct OutputData {
    /// Column headers (optional, for table output).
    pub headers: Option<Vec<String>>,
    /// Top-level nodes.
    pub root: Vec<OutputNode>,
}

impl OutputData {
    /// Create new empty output data.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create output data with a single text node.
    ///
    /// This is the simplest form for commands like `echo`.
    pub fn text(content: impl Into<String>) -> Self {
        Self {
            headers: None,
            root: vec![OutputNode::text(content)],
        }
    }

    /// Create output data with named nodes (for ls, etc.).
    pub fn nodes(nodes: Vec<OutputNode>) -> Self {
        Self {
            headers: None,
            root: nodes,
        }
    }

    /// Create output data with headers and nodes (for ls -l, ps, etc.).
    pub fn table(headers: Vec<String>, nodes: Vec<OutputNode>) -> Self {
        Self {
            headers: Some(headers),
            root: nodes,
        }
    }

    /// Set column headers.
    pub fn with_headers(mut self, headers: Vec<String>) -> Self {
        self.headers = Some(headers);
        self
    }

    /// Check if this output is simple text (single text-only node).
    pub fn is_simple_text(&self) -> bool {
        self.root.len() == 1 && self.root[0].is_text_only()
    }

    /// Check if this output is a flat list (no nested children).
    pub fn is_flat(&self) -> bool {
        self.root.iter().all(|n| !n.has_children())
    }

    /// Check if this output has tabular data (nodes with cells).
    pub fn is_tabular(&self) -> bool {
        self.root.iter().any(|n| !n.cells.is_empty())
    }

    /// Get the text content if this is simple text output.
    pub fn as_text(&self) -> Option<&str> {
        if self.is_simple_text() {
            self.root[0].text.as_deref()
        } else {
            None
        }
    }

    /// Convert to canonical string output (for pipes).
    ///
    /// This produces a simple string representation suitable for
    /// piping to other commands:
    /// - Text nodes: their text content
    /// - Named nodes: names joined by newlines
    /// - Tabular nodes (name + cells): TSV format (name\tcell1\tcell2...)
    /// - Nested nodes: brace notation
    pub fn to_canonical_string(&self) -> String {
        if let Some(text) = self.as_text() {
            return text.to_string();
        }

        // For flat lists (with or without cells), output one line per node
        if self.is_flat() {
            return self.root.iter()
                .map(|n| {
                    if n.cells.is_empty() {
                        n.display_name().to_string()
                    } else {
                        // For tabular data, use TSV format
                        let mut parts = vec![n.display_name().to_string()];
                        parts.extend(n.cells.iter().cloned());
                        parts.join("\t")
                    }
                })
                .collect::<Vec<_>>()
                .join("\n");
        }

        // For trees, use brace notation
        fn format_node(node: &OutputNode) -> String {
            if node.children.is_empty() {
                node.name.clone()
            } else {
                let children: Vec<String> = node.children.iter()
                    .map(format_node)
                    .collect();
                format!("{}/{{{}}}", node.name, children.join(","))
            }
        }

        self.root.iter()
            .map(format_node)
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// Serialize to a JSON value for `--json` flag handling.
    ///
    /// Bare data, no envelope — optimized for `jq` patterns.
    ///
    /// | Structure | JSON |
    /// |-----------|------|
    /// | Simple text | `"hello world"` |
    /// | Flat list (names only) | `["file1", "file2"]` |
    /// | Table (headers + cells) | `[{"col1": "v1", ...}, ...]` |
    /// | Tree (nested children) | `{"dir": {"file": null}}` |
    pub fn to_json(&self) -> serde_json::Value {
        // Simple text -> JSON string
        if let Some(text) = self.as_text() {
            return serde_json::Value::String(text.to_string());
        }

        // Table -> array of objects keyed by headers
        if let Some(ref headers) = self.headers {
            let rows: Vec<serde_json::Value> = self.root.iter().map(|node| {
                let mut map = serde_json::Map::new();
                // First header maps to node.name
                if let Some(first) = headers.first() {
                    map.insert(first.clone(), serde_json::Value::String(node.name.clone()));
                }
                // Remaining headers map to cells
                for (header, cell) in headers.iter().skip(1).zip(node.cells.iter()) {
                    map.insert(header.clone(), serde_json::Value::String(cell.clone()));
                }
                serde_json::Value::Object(map)
            }).collect();
            return serde_json::Value::Array(rows);
        }

        // Tree -> nested object
        if !self.is_flat() {
            fn node_to_json(node: &OutputNode) -> serde_json::Value {
                if node.children.is_empty() {
                    serde_json::Value::Null
                } else {
                    let mut map = serde_json::Map::new();
                    for child in &node.children {
                        map.insert(child.name.clone(), node_to_json(child));
                    }
                    serde_json::Value::Object(map)
                }
            }

            // Single root node -> its children as the top-level object
            if self.root.len() == 1 {
                return node_to_json(&self.root[0]);
            }
            // Multiple root nodes -> object with each root as a key
            let mut map = serde_json::Map::new();
            for node in &self.root {
                map.insert(node.name.clone(), node_to_json(node));
            }
            return serde_json::Value::Object(map);
        }

        // Flat list -> array of strings
        let items: Vec<serde_json::Value> = self.root.iter()
            .map(|n| serde_json::Value::String(n.display_name().to_string()))
            .collect();
        serde_json::Value::Array(items)
    }
}

// ============================================================
// Output Format (Global --json flag)
// ============================================================

/// Output serialization format, requested via global flags.
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    /// JSON serialization via OutputData::to_json()
    Json,
}

/// Transform an ExecResult into the requested output format.
///
/// Serializes regardless of exit code — commands like `diff` (exit 1 = files differ)
/// and `grep` (exit 1 = no matches) use non-zero exits for semantic meaning,
/// not errors. The `--json` contract must hold for all exit codes.
pub fn apply_output_format(mut result: ExecResult, format: OutputFormat) -> ExecResult {
    if result.output.is_none() && result.out.is_empty() {
        return result;
    }
    match format {
        OutputFormat::Json => {
            let json_str = if let Some(ref output) = result.output {
                serde_json::to_string_pretty(&output.to_json())
                    .unwrap_or_else(|_| "null".to_string())
            } else {
                // Text-only: wrap as JSON string
                serde_json::to_string(&result.out)
                    .unwrap_or_else(|_| "null".to_string())
            };
            result.out = json_str;
            // Clear sentinel — format already applied, prevents double-encoding
            result.output = None;
            result
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn entry_type_variants() {
        assert_ne!(EntryType::File, EntryType::Directory);
        assert_ne!(EntryType::Directory, EntryType::Executable);
        assert_ne!(EntryType::Executable, EntryType::Symlink);
    }

    #[test]
    fn to_json_simple_text() {
        let output = OutputData::text("hello world");
        assert_eq!(output.to_json(), serde_json::json!("hello world"));
    }

    #[test]
    fn to_json_flat_list() {
        let output = OutputData::nodes(vec![
            OutputNode::new("file1"),
            OutputNode::new("file2"),
            OutputNode::new("file3"),
        ]);
        assert_eq!(output.to_json(), serde_json::json!(["file1", "file2", "file3"]));
    }

    #[test]
    fn to_json_table() {
        let output = OutputData::table(
            vec!["NAME".into(), "SIZE".into(), "TYPE".into()],
            vec![
                OutputNode::new("foo.rs").with_cells(vec!["1024".into(), "file".into()]),
                OutputNode::new("bar/").with_cells(vec!["4096".into(), "dir".into()]),
            ],
        );
        assert_eq!(output.to_json(), serde_json::json!([
            {"NAME": "foo.rs", "SIZE": "1024", "TYPE": "file"},
            {"NAME": "bar/", "SIZE": "4096", "TYPE": "dir"},
        ]));
    }

    #[test]
    fn to_json_tree() {
        let child1 = OutputNode::new("main.rs").with_entry_type(EntryType::File);
        let child2 = OutputNode::new("utils.rs").with_entry_type(EntryType::File);
        let subdir = OutputNode::new("lib")
            .with_entry_type(EntryType::Directory)
            .with_children(vec![child2]);
        let root = OutputNode::new("src")
            .with_entry_type(EntryType::Directory)
            .with_children(vec![child1, subdir]);

        let output = OutputData::nodes(vec![root]);
        assert_eq!(output.to_json(), serde_json::json!({
            "main.rs": null,
            "lib": {"utils.rs": null},
        }));
    }

    #[test]
    fn to_json_tree_multiple_roots() {
        let root1 = OutputNode::new("src")
            .with_entry_type(EntryType::Directory)
            .with_children(vec![OutputNode::new("main.rs")]);
        let root2 = OutputNode::new("docs")
            .with_entry_type(EntryType::Directory)
            .with_children(vec![OutputNode::new("README.md")]);

        let output = OutputData::nodes(vec![root1, root2]);
        assert_eq!(output.to_json(), serde_json::json!({
            "src": {"main.rs": null},
            "docs": {"README.md": null},
        }));
    }

    #[test]
    fn to_json_empty() {
        let output = OutputData::new();
        assert_eq!(output.to_json(), serde_json::json!([]));
    }

    #[test]
    fn apply_output_format_clears_sentinel() {
        let output = OutputData::table(
            vec!["NAME".into()],
            vec![OutputNode::new("test")],
        );
        let result = ExecResult::with_output(output);
        assert!(result.output.is_some(), "before: sentinel present");

        let formatted = apply_output_format(result, OutputFormat::Json);
        assert!(formatted.output.is_none(), "after Json: sentinel cleared");
    }

    #[test]
    fn apply_output_format_no_double_encoding() {
        let output = OutputData::nodes(vec![
            OutputNode::new("file1"),
            OutputNode::new("file2"),
        ]);
        let result = ExecResult::with_output(output);

        let after_json = apply_output_format(result, OutputFormat::Json);
        let json_out = after_json.out.clone();
        assert!(after_json.output.is_none(), "sentinel cleared by Json");

        let parsed: serde_json::Value = serde_json::from_str(&json_out).expect("valid JSON");
        assert_eq!(parsed, serde_json::json!(["file1", "file2"]));
    }
}
