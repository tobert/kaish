//! ExecResult — the structured result of every command execution.
//!
//! After every command in kaish, the special variable `$?` contains an ExecResult:
//!
//! ```kaish
//! api-call endpoint=/users
//! if ${?.ok}; then
//!     echo "Got ${?.data.count} users"
//! else
//!     echo "Error: ${?.err}"
//! fi
//! ```
//!
//! This differs from traditional shells where `$?` is just an integer exit code.
//! In kaish, we capture the full context: exit code, stdout, parsed data, and errors.
//!
//! # Structured Output (Tree-of-Tables Model)
//!
//! The unified output model uses `OutputData` containing a tree of `OutputNode`s:
//!
//! - **Builtins**: Pure data producers returning `OutputData`
//! - **Frontends**: Handle all rendering (REPL, MCP, kaijutsu)

use crate::ast::Value;
use serde::Serialize;

// ============================================================
// Structured Output (Tree-of-Tables Model)
// ============================================================

/// Entry type for rendering hints (colors, icons).
///
/// This unified enum is used by both the new OutputNode system
/// and the legacy DisplayHint::Table for backward compatibility.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize)]
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
/// Nodes can carry text, tabular cells, and nested children.
/// This is the building block for all structured output.
///
/// # Examples
///
/// Simple text output (echo):
/// ```
/// # use kaish_kernel::interpreter::OutputNode;
/// let node = OutputNode::text("hello world");
/// ```
///
/// File listing (ls):
/// ```
/// # use kaish_kernel::interpreter::{OutputNode, EntryType};
/// let node = OutputNode::new("file.txt").with_entry_type(EntryType::File);
/// ```
///
/// Table row (ls -l):
/// ```
/// # use kaish_kernel::interpreter::{OutputNode, EntryType};
/// let node = OutputNode::new("file.txt")
///     .with_cells(vec!["drwxr-xr-x".into(), "4096".into()])
///     .with_entry_type(EntryType::Directory);
/// ```
///
/// Tree node with children:
/// ```
/// # use kaish_kernel::interpreter::{OutputNode, EntryType};
/// let child = OutputNode::new("main.rs").with_entry_type(EntryType::File);
/// let parent = OutputNode::new("src")
///     .with_entry_type(EntryType::Directory)
///     .with_children(vec![child]);
/// ```
#[derive(Debug, Clone, PartialEq, Default, Serialize)]
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
/// # Rendering Rules
///
/// | Structure | Interactive | Piped/Model |
/// |-----------|-------------|-------------|
/// | Single node with `text` | Print text | Print text |
/// | Flat nodes, `name` only | Multi-column, colored | One per line |
/// | Flat nodes with `cells` | Aligned table | TSV or names only |
/// | Nested `children` | Box-drawing tree | Brace notation |
#[derive(Debug, Clone, PartialEq, Default, Serialize)]
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
        // Simple text → JSON string
        if let Some(text) = self.as_text() {
            return serde_json::Value::String(text.to_string());
        }

        // Table → array of objects keyed by headers
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

        // Tree → nested object
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

            // Single root node → its children as the top-level object
            if self.root.len() == 1 {
                return node_to_json(&self.root[0]);
            }
            // Multiple root nodes → object with each root as a key
            let mut map = serde_json::Map::new();
            for node in &self.root {
                map.insert(node.name.clone(), node_to_json(node));
            }
            return serde_json::Value::Object(map);
        }

        // Flat list → array of strings
        let items: Vec<serde_json::Value> = self.root.iter()
            .map(|n| serde_json::Value::String(n.display_name().to_string()))
            .collect();
        serde_json::Value::Array(items)
    }
}

// ============================================================
// Output Format (Global --json / --toon flags)
// ============================================================

/// Output serialization format, requested via global flags.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    /// JSON serialization via OutputData::to_json()
    Json,
    /// Compact token-efficient format (future)
    Toon,
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
            result
        }
        OutputFormat::Toon => {
            // Stub: falls through unchanged until to_toon() is implemented
            result
        }
    }
}

/// The result of executing a command or pipeline.
///
/// Fields accessible via `${?.field}`:
/// - `code` — exit code (0 = success)
/// - `ok` — true if code == 0
/// - `err` — error message if failed
/// - `out` — raw stdout as string
/// - `data` — parsed JSON from stdout (if valid JSON)
#[derive(Debug, Clone, PartialEq)]
pub struct ExecResult {
    /// Exit code. 0 means success.
    pub code: i64,
    /// Raw standard output as a string (canonical for pipes).
    pub out: String,
    /// Raw standard error as a string.
    pub err: String,
    /// Parsed JSON data from stdout, if stdout was valid JSON.
    pub data: Option<Value>,
    /// Structured output data for rendering.
    pub output: Option<OutputData>,
}

impl ExecResult {
    /// Create a successful result with output.
    pub fn success(out: impl Into<String>) -> Self {
        let out = out.into();
        let data = Self::try_parse_json(&out);
        Self {
            code: 0,
            out,
            err: String::new(),
            data,
            output: None,
        }
    }

    /// Create a successful result with structured output data.
    ///
    /// This is the preferred constructor for new code. The `OutputData`
    /// provides a unified model for all output types.
    pub fn with_output(output: OutputData) -> Self {
        let out = output.to_canonical_string();
        let data = Self::try_parse_json(&out);
        Self {
            code: 0,
            out,
            err: String::new(),
            data,
            output: Some(output),
        }
    }

    /// Create a successful result with structured data.
    pub fn success_data(data: Value) -> Self {
        let out = value_to_json(&data).to_string();
        Self {
            code: 0,
            out,
            err: String::new(),
            data: Some(data),
            output: None,
        }
    }

    /// Create a successful result with both text output and structured data.
    ///
    /// Use this when a command should have:
    /// - Text output for pipes and traditional shell usage
    /// - Structured data for iteration and programmatic access
    ///
    /// The data field takes precedence for command substitution in contexts
    /// like `for i in $(cmd)` where the structured data can be iterated.
    pub fn success_with_data(out: impl Into<String>, data: Value) -> Self {
        Self {
            code: 0,
            out: out.into(),
            err: String::new(),
            data: Some(data),
            output: None,
        }
    }

    /// Create a failed result with an error message.
    pub fn failure(code: i64, err: impl Into<String>) -> Self {
        Self {
            code,
            out: String::new(),
            err: err.into(),
            data: None,
            output: None,
        }
    }

    /// Create a result from raw output streams.
    pub fn from_output(code: i64, stdout: impl Into<String>, stderr: impl Into<String>) -> Self {
        let out = stdout.into();
        let data = if code == 0 {
            Self::try_parse_json(&out)
        } else {
            None
        };
        Self {
            code,
            out,
            err: stderr.into(),
            data,
            output: None,
        }
    }

    /// True if the command succeeded (exit code 0).
    pub fn ok(&self) -> bool {
        self.code == 0
    }

    /// Get a field by name, for variable access like `${?.field}`.
    pub fn get_field(&self, name: &str) -> Option<Value> {
        match name {
            "code" => Some(Value::Int(self.code)),
            "ok" => Some(Value::Bool(self.ok())),
            "out" => Some(Value::String(self.out.clone())),
            "err" => Some(Value::String(self.err.clone())),
            "data" => self.data.clone(),
            _ => None,
        }
    }

    /// Try to parse a string as JSON, returning a Value if successful.
    fn try_parse_json(s: &str) -> Option<Value> {
        let trimmed = s.trim();
        if trimmed.is_empty() {
            return None;
        }
        serde_json::from_str::<serde_json::Value>(trimmed)
            .ok()
            .map(json_to_value)
    }
}

impl Default for ExecResult {
    fn default() -> Self {
        Self::success("")
    }
}

/// Convert serde_json::Value to our AST Value.
///
/// Primitives are mapped to their corresponding Value variants.
/// Arrays and objects are preserved as `Value::Json` - use `jq` to query them.
pub fn json_to_value(json: serde_json::Value) -> Value {
    match json {
        serde_json::Value::Null => Value::Null,
        serde_json::Value::Bool(b) => Value::Bool(b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::Int(i)
            } else if let Some(f) = n.as_f64() {
                Value::Float(f)
            } else {
                Value::String(n.to_string())
            }
        }
        serde_json::Value::String(s) => Value::String(s),
        // Arrays and objects are preserved as Json values
        serde_json::Value::Array(_) | serde_json::Value::Object(_) => Value::Json(json),
    }
}

/// Convert our AST Value to serde_json::Value for serialization.
pub fn value_to_json(value: &Value) -> serde_json::Value {
    match value {
        Value::Null => serde_json::Value::Null,
        Value::Bool(b) => serde_json::Value::Bool(*b),
        Value::Int(i) => serde_json::Value::Number((*i).into()),
        Value::Float(f) => {
            serde_json::Number::from_f64(*f)
                .map(serde_json::Value::Number)
                .unwrap_or(serde_json::Value::Null)
        }
        Value::String(s) => serde_json::Value::String(s.clone()),
        Value::Json(json) => json.clone(),
        Value::Blob(blob) => {
            let mut map = serde_json::Map::new();
            map.insert("_type".to_string(), serde_json::Value::String("blob".to_string()));
            map.insert("id".to_string(), serde_json::Value::String(blob.id.clone()));
            map.insert("size".to_string(), serde_json::Value::Number(blob.size.into()));
            map.insert("contentType".to_string(), serde_json::Value::String(blob.content_type.clone()));
            if let Some(hash) = &blob.hash {
                let hash_hex: String = hash.iter().map(|b| format!("{:02x}", b)).collect();
                map.insert("hash".to_string(), serde_json::Value::String(hash_hex));
            }
            serde_json::Value::Object(map)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn success_creates_ok_result() {
        let result = ExecResult::success("hello world");
        assert!(result.ok());
        assert_eq!(result.code, 0);
        assert_eq!(result.out, "hello world");
        assert!(result.err.is_empty());
    }

    #[test]
    fn failure_creates_non_ok_result() {
        let result = ExecResult::failure(1, "command not found");
        assert!(!result.ok());
        assert_eq!(result.code, 1);
        assert_eq!(result.err, "command not found");
    }

    #[test]
    fn json_stdout_is_parsed() {
        // JSON objects/arrays are now stored as Value::Json for direct access
        let result = ExecResult::success(r#"{"count": 42, "items": ["a", "b"]}"#);
        assert!(result.data.is_some());
        let data = result.data.unwrap();
        // Objects are stored as Value::Json
        assert!(matches!(data, Value::Json(_)));
        // Verify the structure is preserved
        if let Value::Json(json) = data {
            assert_eq!(json.get("count"), Some(&serde_json::json!(42)));
            assert_eq!(json.get("items"), Some(&serde_json::json!(["a", "b"])));
        }
    }

    #[test]
    fn non_json_stdout_has_no_data() {
        let result = ExecResult::success("just plain text");
        assert!(result.data.is_none());
    }

    #[test]
    fn get_field_code() {
        let result = ExecResult::failure(127, "not found");
        assert_eq!(result.get_field("code"), Some(Value::Int(127)));
    }

    #[test]
    fn get_field_ok() {
        let success = ExecResult::success("hi");
        let failure = ExecResult::failure(1, "err");
        assert_eq!(success.get_field("ok"), Some(Value::Bool(true)));
        assert_eq!(failure.get_field("ok"), Some(Value::Bool(false)));
    }

    #[test]
    fn get_field_out_and_err() {
        let result = ExecResult::from_output(1, "stdout text", "stderr text");
        assert_eq!(result.get_field("out"), Some(Value::String("stdout text".into())));
        assert_eq!(result.get_field("err"), Some(Value::String("stderr text".into())));
    }

    #[test]
    fn get_field_data() {
        let result = ExecResult::success(r#"{"key": "value"}"#);
        let data = result.get_field("data");
        assert!(data.is_some());
    }

    #[test]
    fn get_field_unknown_returns_none() {
        let result = ExecResult::success("");
        assert_eq!(result.get_field("nonexistent"), None);
    }

    #[test]
    fn success_data_creates_result_with_value() {
        let value = Value::String("test data".into());
        let result = ExecResult::success_data(value.clone());
        assert!(result.ok());
        assert_eq!(result.data, Some(value));
    }

    #[test]
    fn entry_type_variants() {
        // Just verify the enum variants exist and are distinct
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
}
