//! Structured output model (Tree-of-Tables) and output format handling.
//!
//! The unified output model uses `OutputData` containing a tree of `OutputNode`s:
//!
//! - **Builtins**: Pure data producers returning `OutputData`
//! - **Frontends**: Handle all rendering (REPL, MCP, kaijutsu)

use serde::{Deserialize, Serialize};

use crate::result::{ExecResult, LatchRequest};

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
    ///
    /// Three-state semantics:
    /// - `None` — named entry (file listing, table row), not text
    /// - `Some("")` — text node with empty content (e.g., `echo ""`)
    /// - `Some("x")` — text node with content
    ///
    /// `is_text_only()` returns true iff text is Some AND name/cells/children are empty.
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

    /// Estimate brace-notation byte size without materializing.
    pub fn estimated_byte_size(&self) -> usize {
        if self.children.is_empty() {
            self.name.len() + self.text.as_ref().map_or(0, |t| t.len())
        } else {
            // "name/{child1,child2,...}"
            let mut size = self.name.len() + 2; // name + "/{" + "}"
            for (i, child) in self.children.iter().enumerate() {
                if i > 0 {
                    size += 1; // comma separator
                }
                size += child.estimated_byte_size();
            }
            size + 1 // closing brace
        }
    }

    /// Write brace-notation to a writer. Returns bytes written.
    pub fn write_canonical(&self, w: &mut dyn std::io::Write, budget: usize) -> std::io::Result<usize> {
        if self.children.is_empty() {
            w.write_all(self.name.as_bytes())?;
            return Ok(self.name.len());
        }
        let mut written = 0;
        w.write_all(self.name.as_bytes())?;
        written += self.name.len();
        if written >= budget {
            return Ok(written);
        }
        w.write_all(b"/{")?;
        written += 2;
        for (i, child) in self.children.iter().enumerate() {
            if written >= budget {
                break;
            }
            if i > 0 {
                w.write_all(b",")?;
                written += 1;
            }
            written += child.write_canonical(w, budget.saturating_sub(written))?;
        }
        w.write_all(b"}")?;
        written += 1;
        Ok(written)
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
#[non_exhaustive]
pub struct OutputData {
    /// Column headers (optional, for table output).
    pub headers: Option<Vec<String>>,
    /// Top-level nodes.
    pub root: Vec<OutputNode>,
    /// Render-only override for `--json` consumers. When `Some`,
    /// `to_json()` returns this verbatim instead of inferring from
    /// `headers` / `root` / `cells`. Use it when a builtin wants its
    /// `--json` shape to be richer than the table form (e.g. grep
    /// emitting per-match objects with submatches and byte offsets).
    ///
    /// Skipped by serde (and thus by postcard / bincode) — this is a
    /// transient render hint, not part of the persisted shape.
    #[serde(skip)]
    #[cfg_attr(feature = "schema", schemars(skip))]
    pub rich_json: Option<serde_json::Value>,
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
            rich_json: None,
        }
    }

    /// Create output data with named nodes (for ls, etc.).
    pub fn nodes(nodes: Vec<OutputNode>) -> Self {
        Self {
            headers: None,
            root: nodes,
            rich_json: None,
        }
    }

    /// Create output data with headers and nodes (for ls -l, ps, etc.).
    pub fn table(headers: Vec<String>, nodes: Vec<OutputNode>) -> Self {
        Self {
            headers: Some(headers),
            root: nodes,
            rich_json: None,
        }
    }

    /// Set column headers.
    pub fn with_headers(mut self, headers: Vec<String>) -> Self {
        self.headers = Some(headers);
        self
    }

    /// Attach a render-only `--json` override. See `rich_json` field doc.
    pub fn with_rich_json(mut self, value: serde_json::Value) -> Self {
        self.rich_json = Some(value);
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

    /// Extract the owned String from a single-text-node OutputData.
    /// Returns `Err(self)` for non-simple-text output (tables, trees, multi-node),
    /// giving the caller back the unconsumed OutputData.
    pub fn into_text(mut self) -> Result<String, Self> {
        if self.root.len() == 1 && self.root[0].is_text_only() {
            Ok(self.root.pop().and_then(|n| n.text).unwrap_or_default())
        } else {
            Err(self)
        }
    }

    /// Estimate canonical string byte size without materializing.
    ///
    /// Lower bound — actual may be slightly larger due to formatting.
    /// Mirrors `to_canonical_string()` structure but only accumulates sizes.
    pub fn estimated_byte_size(&self) -> usize {
        if self.root.len() == 1 && self.root[0].is_text_only() {
            return self.root[0].text.as_ref().map_or(0, |t| t.len());
        }

        if self.is_flat() {
            let mut size = 0;
            for (i, n) in self.root.iter().enumerate() {
                if i > 0 {
                    size += 1; // newline separator
                }
                size += n.display_name().len();
                for cell in &n.cells {
                    size += 1 + cell.len(); // tab + cell
                }
            }
            return size;
        }

        // Tree: estimate brace notation
        let mut size = 0;
        for (i, n) in self.root.iter().enumerate() {
            if i > 0 {
                size += 1; // newline separator
            }
            size += n.estimated_byte_size();
        }
        size
    }

    /// Write canonical representation to a writer with optional byte budget.
    ///
    /// Returns total bytes written. Stops after budget exceeded (imprecise:
    /// one write past the limit is fine — caller uses this for spill detection,
    /// not for exact truncation).
    pub fn write_canonical(&self, w: &mut dyn std::io::Write, budget: Option<usize>) -> std::io::Result<usize> {
        let mut written = 0usize;
        let budget = budget.unwrap_or(usize::MAX);

        if self.root.len() == 1 && self.root[0].is_text_only() {
            if let Some(ref text) = self.root[0].text {
                w.write_all(text.as_bytes())?;
                return Ok(text.len());
            }
            return Ok(0);
        }

        if self.is_flat() {
            for (i, n) in self.root.iter().enumerate() {
                if i > 0 {
                    w.write_all(b"\n")?;
                    written += 1;
                }
                let name = n.display_name();
                w.write_all(name.as_bytes())?;
                written += name.len();
                for cell in &n.cells {
                    w.write_all(b"\t")?;
                    w.write_all(cell.as_bytes())?;
                    written += 1 + cell.len();
                }
                if written > budget {
                    return Ok(written);
                }
            }
            return Ok(written);
        }

        // Tree: brace notation
        for (i, n) in self.root.iter().enumerate() {
            if i > 0 {
                w.write_all(b"\n")?;
                written += 1;
            }
            written += n.write_canonical(w, budget.saturating_sub(written))?;
            if written > budget {
                return Ok(written);
            }
        }
        Ok(written)
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
        // Builtin-supplied override wins (used by `grep --json` to expose
        // submatches/byte offsets that don't fit the table model).
        if let Some(rich) = &self.rich_json {
            return rich.clone();
        }
        // Simple text -> JSON string
        if let Some(text) = self.as_text() {
            return serde_json::Value::String(text.to_string());
        }

        // Table -> array of objects keyed by headers. Nodes with children
        // (e.g. `ls -lR`, where root nodes are directory groups holding the
        // actual entries) nest those entries under a "children" key — dropping
        // them would silently lose every file and size.
        if let Some(ref headers) = self.headers {
            fn row_to_json(node: &OutputNode, headers: &[String]) -> serde_json::Value {
                let mut map = serde_json::Map::new();
                // First header maps to node.name
                if let Some(first) = headers.first() {
                    map.insert(first.clone(), serde_json::Value::String(node.name.clone()));
                }
                // Remaining headers map to cells
                for (header, cell) in headers.iter().skip(1).zip(node.cells.iter()) {
                    map.insert(header.clone(), serde_json::Value::String(cell.clone()));
                }
                if !node.children.is_empty() {
                    let children: Vec<serde_json::Value> = node
                        .children
                        .iter()
                        .map(|child| row_to_json(child, headers))
                        .collect();
                    map.insert("children".to_string(), serde_json::Value::Array(children));
                }
                serde_json::Value::Object(map)
            }
            let rows: Vec<serde_json::Value> = self
                .root
                .iter()
                .map(|node| row_to_json(node, headers))
                .collect();
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

/// Build the `--json` error/latch envelope for a result carrying a pending
/// confirmation latch (`.latch` is `Some`) — `{"error", "code", "data"?,
/// "latch"}`. The ONE place a latch gets folded into `--json` output, so a
/// latched `wait` (which carries text output, e.g. `"[1] Latched\n"`) and a
/// latched `rm` (bare exit-2 failure, no output at all) converge on the same
/// shape instead of diverging by which branch of [`apply_output_format`] they
/// happen to take. `error` is `result.err` if non-empty, else the rendered
/// text — a latch result is always diagnostic-shaped, so something readable
/// belongs under `error` either way. A tool that also attached structured
/// data to the result keeps it reachable under `data`, alongside (never
/// clobbered by) the latch.
fn latch_envelope(result: &ExecResult, latch: &LatchRequest) -> serde_json::Value {
    let error = if !result.err.is_empty() {
        result.err.clone()
    } else {
        result.text_out().into_owned()
    };
    let mut obj = serde_json::json!({
        "error": error,
        "code": result.code,
    });
    if let Some(data) = &result.data {
        obj["data"] = crate::result::value_to_json(data);
    }
    // Infallible: LatchRequest is String/Vec<String>/u64 fields only.
    if let Ok(v) = serde_json::to_value(latch) {
        obj["latch"] = v;
    }
    obj
}

/// Transform an ExecResult into the requested output format.
///
/// Serializes regardless of exit code — commands like `diff` (exit 1 = files differ)
/// and `grep` (exit 1 = no matches) use non-zero exits for semantic meaning,
/// not errors. The `--json` contract must hold for all exit codes.
pub fn apply_output_format(mut result: ExecResult, format: OutputFormat) -> ExecResult {
    // Binary results serialize as the self-describing base64 envelope, never a
    // lossy-decoded JSON string. See docs/binary-data.md.
    if result.is_bytes() {
        let envelope = crate::bytes::bytes_to_envelope(result.out_bytes().unwrap_or(&[]));
        match format {
            OutputFormat::Json => {
                result.set_out(
                    serde_json::to_string(&envelope).unwrap_or_else(|_| "null".to_string()),
                );
                result.data = Some(crate::result::json_to_value(envelope));
                result.set_output(None);
            }
        }
        return result;
    }
    // A confirmation-latch request is control-plane, not stdout data — surface
    // it under its own `latch` key in ONE canonical envelope regardless of
    // whether the result ALSO carries text output. Handled here, before the
    // has-output/no-output branches below, because `wait %1`'s result sets
    // `OutputData::text` alongside `.latch` and so used to take the has_output()
    // branch below, which never looked at `.latch` at all — the nonce was
    // completely unreachable from `wait %1 --json` (GH #124 part 1). `rm`'s bare
    // exit-2 failure (no output) hits this same branch too, so both converge on
    // `latch_envelope` and can't diverge a third time.
    if let Some(latch) = &result.latch {
        let obj = match format {
            OutputFormat::Json => latch_envelope(&result, latch),
        };
        let out = serde_json::to_string(&obj).unwrap_or_else(|_| "null".to_string());
        result.data = Some(crate::result::json_to_value(obj));
        result.set_out(out);
        result.set_output(None);
        return result;
    }
    if !result.has_output() && result.text_out().is_empty() {
        // No stdout to format. A failure that carries a diagnostic message must
        // still honor --json — otherwise the message leaks out as plain text
        // even though structured output was requested. Emit a JSON error object
        // so the contract holds on the error path. A clean non-zero exit with no
        // message (e.g. `grep` no-match, exit 1) is not an error and stays empty.
        if !result.ok() && !result.err.is_empty() {
            match format {
                OutputFormat::Json => {
                    let mut obj = serde_json::json!({
                        "error": result.err,
                        "code": result.code,
                    });
                    // A tool that attached structured data to an error result
                    // must keep it reachable under --json — nest it under `data`
                    // so the envelope holds the diagnostic *and* the structured
                    // truth instead of clobbering one with the other.
                    if let Some(data) = &result.data {
                        obj["data"] = crate::result::value_to_json(data);
                    }
                    let out =
                        serde_json::to_string(&obj).unwrap_or_else(|_| "null".to_string());
                    result.set_out(out);
                    result.data = Some(crate::result::json_to_value(obj));
                }
            }
        }
        return result;
    }
    match format {
        OutputFormat::Json => {
            if let Some(output) = result.output() {
                let json_value = output.to_json();
                // NLL: borrow of result via output ends here (json_value is owned)
                result.set_out(serde_json::to_string(&json_value)
                    .unwrap_or_else(|_| "null".to_string()));
                result.data = Some(crate::result::json_to_value(json_value));
            } else if let Some(data) = &result.data {
                // Structured data already present (e.g. jq, any `success_with_data`
                // builtin): serialize that, not the rendered text. Re-wrapping the
                // text as a JSON string would double-encode it (`"1"` not `1`) and
                // clobber the real value. `.data` is the structured truth.
                let json_out = serde_json::to_string(&crate::result::value_to_json(data))
                    .unwrap_or_else(|_| "null".to_string());
                result.set_out(json_out);
            } else {
                // Text-only: wrap as JSON string. .data mirrors the same string.
                let text = result.text_out().into_owned();
                let json_out = serde_json::to_string(&text)
                    .unwrap_or_else(|_| "null".to_string());
                result.data = Some(crate::value::Value::String(text));
                result.set_out(json_out);
            }
            // Clear sentinel — format already applied, prevents double-encoding
            result.set_output(None);
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
    fn to_json_table_with_children() {
        // `ls -lR --json` builds a table whose root nodes are directory
        // groups and whose actual entries (with size/type cells) live in
        // `children`. The table serializer must recurse into them or every
        // file and size is silently dropped — corruption, not an error.
        let output = OutputData::table(
            vec!["NAME".into(), "TYPE".into(), "SIZE".into()],
            vec![
                OutputNode::new(".")
                    .with_entry_type(EntryType::Directory)
                    .with_children(vec![
                        OutputNode::new("top.txt").with_cells(vec!["-".into(), "6".into()]),
                        OutputNode::new("a").with_cells(vec!["d".into(), "60".into()]),
                    ]),
                OutputNode::new("a")
                    .with_entry_type(EntryType::Directory)
                    .with_children(vec![
                        OutputNode::new("mid.txt").with_cells(vec!["-".into(), "3".into()]),
                    ]),
            ],
        );
        assert_eq!(output.to_json(), serde_json::json!([
            {"NAME": ".", "children": [
                {"NAME": "top.txt", "TYPE": "-", "SIZE": "6"},
                {"NAME": "a", "TYPE": "d", "SIZE": "60"},
            ]},
            {"NAME": "a", "children": [
                {"NAME": "mid.txt", "TYPE": "-", "SIZE": "3"},
            ]},
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
        assert!(result.has_output(), "before: sentinel present");

        let formatted = apply_output_format(result, OutputFormat::Json);
        assert!(!formatted.has_output(), "after Json: sentinel cleared");
    }

    #[test]
    fn apply_output_format_no_double_encoding() {
        let output = OutputData::nodes(vec![
            OutputNode::new("file1"),
            OutputNode::new("file2"),
        ]);
        let result = ExecResult::with_output(output);

        let after_json = apply_output_format(result, OutputFormat::Json);
        let json_out = after_json.text_out().into_owned();
        assert!(!after_json.has_output(), "sentinel cleared by Json");

        let parsed: serde_json::Value = serde_json::from_str(&json_out).expect("valid JSON");
        assert_eq!(parsed, serde_json::json!(["file1", "file2"]));
    }

    #[test]
    fn apply_output_format_populates_data() {
        let output = OutputData::nodes(vec![
            OutputNode::new("file1"),
            OutputNode::new("file2"),
        ]);
        let result = ExecResult::with_output(output);
        assert!(result.data.is_none(), "before: no data on non-text output");

        let formatted = apply_output_format(result, OutputFormat::Json);
        assert!(formatted.data.is_some(), "after Json: data populated");

        // data should match the JSON in out
        let data = formatted.data.unwrap();
        assert!(matches!(data, crate::value::Value::Json(_)), "data should be Json variant");
        if let crate::value::Value::Json(json) = data {
            assert_eq!(json, serde_json::json!(["file1", "file2"]));
        }
    }

    #[test]
    fn apply_output_format_prefers_structured_data_over_text() {
        // A `success_with_data` result (e.g. jq '.a' on {"a":1}) carries the
        // structured scalar in `.data` and the rendered text in stdout. --json
        // must serialize the structured value (1), not re-wrap the text ("1").
        use crate::value::Value;
        let result = ExecResult::success_with_data("1", Value::Int(1));
        assert!(!result.has_output(), "no OutputData sentinel on this path");

        let formatted = apply_output_format(result, OutputFormat::Json);
        let json_out = formatted.text_out().into_owned();
        let parsed: serde_json::Value = serde_json::from_str(&json_out).expect("valid JSON");
        assert_eq!(parsed, serde_json::json!(1), "number, not the string \"1\"");
        // The structured `.data` is preserved, not clobbered down to a string.
        assert_eq!(formatted.data, Some(Value::Int(1)));
    }

    #[test]
    fn apply_output_format_compact_json() {
        let output = OutputData::nodes(vec![
            OutputNode::new("file1"),
            OutputNode::new("file2"),
        ]);
        let result = ExecResult::with_output(output);

        let formatted = apply_output_format(result, OutputFormat::Json);
        // Compact JSON: no pretty-printing (no newlines within the array)
        let out = formatted.text_out();
        assert!(!out.contains('\n'), "should be compact JSON, got: {}", out);
        assert_eq!(&*out, r#"["file1","file2"]"#);
    }

    #[test]
    fn apply_output_format_emits_json_error_object_on_failure() {
        // A failure with empty stdout and a populated err must still honor
        // --json: emit {"error", "code"} rather than leaking the message as
        // plain text (e.g. `grep --json --bogus-flag`).
        let result = ExecResult::failure(2, "grep: unknown flag --bogus-flag");
        assert!(!result.has_output());
        assert!(result.text_out().is_empty());

        let formatted = apply_output_format(result, OutputFormat::Json);
        let out = formatted.text_out().into_owned();
        let parsed: serde_json::Value = serde_json::from_str(&out).expect("valid JSON");
        assert_eq!(
            parsed,
            serde_json::json!({"error": "grep: unknown flag --bogus-flag", "code": 2})
        );
        // .data mirrors the JSON object.
        assert!(matches!(formatted.data, Some(crate::value::Value::Json(_))));
    }

    #[test]
    fn apply_output_format_preserves_structured_data_on_error() {
        // A latch result is a failure (exit 2, empty stdout, populated err) that
        // ALSO carries a structured nonce payload on .data. Under --json the
        // error-envelope path must not clobber that payload — the nonce stays
        // reachable, nested under `data`, alongside the error/code envelope.
        let mut result = ExecResult::failure(2, "rm: confirmation required (latch enabled)");
        result.data = Some(crate::value::Value::Json(serde_json::json!({
            "nonce": "a3f7b2c1",
            "command": "rm",
            "paths": ["important.dat"],
            "hint": "rm --confirm=\"a3f7b2c1\" important.dat",
            "ttl": 60,
        })));

        let formatted = apply_output_format(result, OutputFormat::Json);
        let out = formatted.text_out().into_owned();
        let parsed: serde_json::Value = serde_json::from_str(&out).expect("valid JSON");
        assert_eq!(parsed["error"], "rm: confirmation required (latch enabled)");
        assert_eq!(parsed["code"], 2);
        assert_eq!(parsed["data"]["nonce"], "a3f7b2c1");
        assert_eq!(parsed["data"]["ttl"], 60);
        // .data mirrors the serialized envelope (nonce reachable from the struct too).
        match &formatted.data {
            Some(crate::value::Value::Json(v)) => assert_eq!(v["data"]["nonce"], "a3f7b2c1"),
            other => panic!("expected nested JSON data, got {other:?}"),
        }
    }

    #[test]
    fn apply_output_format_surfaces_latch_even_when_result_has_output() {
        // GH #124 part 1: `wait %1`'s gate result carries BOTH text output
        // (the "[1] Latched\n" status line, via `.output`/`.out`) AND `.latch`
        // — unlike `rm`'s bare exit-2 failure, which has no output at all. The
        // old code only merged `.latch` into the JSON envelope on the
        // no-output error branch, so a result with output (like wait's) took
        // the has_output() branch instead and the nonce never appeared under
        // `--json`. Mirrors wait.rs::finish()'s exact construction.
        let mut result = ExecResult::from_output(2, "[1] Latched\n", "");
        result.set_output(Some(OutputData::text("[1] Latched\n")));
        assert!(result.has_output(), "precondition: this result DOES have output");
        result.latch = Some(Box::new(LatchRequest {
            nonce: "a3f7b2c1".to_string(),
            command: "rm".to_string(),
            paths: vec!["precious.txt".to_string()],
            hint: "rm --confirm=\"a3f7b2c1\" precious.txt".to_string(),
            tool: "rm".to_string(),
            argv: vec!["precious.txt".to_string()],
            ttl: 60,
            job_id: None,
        }));

        let formatted = apply_output_format(result, OutputFormat::Json);
        let out = formatted.text_out().into_owned();
        let parsed: serde_json::Value = serde_json::from_str(&out).expect("valid JSON");
        assert_eq!(
            parsed["latch"]["nonce"], "a3f7b2c1",
            "the nonce must be reachable from a latched result that also has \
             output, not just the no-output error path: {parsed}"
        );
        assert_eq!(parsed["latch"]["command"], "rm");
        assert_eq!(parsed["code"], 2);
        // No .err was set (mirrors wait.rs), so the rendered text becomes the
        // diagnostic `error` field.
        assert_eq!(parsed["error"], "[1] Latched\n");
        assert!(
            formatted.latch_request().is_some(),
            "the typed latch must survive --json formatting"
        );
    }

    #[test]
    fn apply_output_format_leaves_clean_no_match_empty() {
        // grep no-match: exit 1, empty stdout, empty err. Not an error — must
        // NOT be wrapped in a JSON error object; stays empty.
        let result = ExecResult::failure(1, "");
        let formatted = apply_output_format(result, OutputFormat::Json);
        assert!(formatted.text_out().is_empty());
        assert!(formatted.data.is_none());
    }

    #[test]
    fn apply_output_format_empty_success_stays_empty() {
        let result = ExecResult::success("");
        let formatted = apply_output_format(result, OutputFormat::Json);
        assert!(formatted.text_out().is_empty());
        assert!(formatted.data.is_none());
    }

    #[test]
    fn estimated_byte_size_text_only_node() {
        let node = OutputNode::text("hello world");
        // Text-only node: name is empty, text is "hello world" (11 bytes)
        assert_eq!(node.estimated_byte_size(), 11);
    }

    #[test]
    fn estimated_byte_size_named_node() {
        let node = OutputNode::new("file.txt");
        assert_eq!(node.estimated_byte_size(), 8);
    }

    #[test]
    fn write_canonical_respects_budget() {
        let parent = OutputNode::new("root")
            .with_children(vec![
                OutputNode::new("aaaa"),
                OutputNode::new("bbbb"),
                OutputNode::new("cccc"),
            ]);
        // With a very small budget, should stop writing children early
        let mut buf = Vec::new();
        let written = parent.write_canonical(&mut buf, 8).unwrap();
        let output = String::from_utf8(buf).unwrap();
        // "root" (4) + "/{" (2) = 6, then budget check kicks in after first child
        assert!(written <= 16, "should respect budget, wrote {} bytes: {}", written, output);
        // Should at least write the root name
        assert!(output.starts_with("root"), "should start with root: {}", output);
    }

    #[test]
    fn into_text_simple() {
        let data = OutputData::text("hello");
        assert_eq!(data.into_text(), Ok("hello".to_string()));
    }

    #[test]
    fn into_text_non_simple() {
        let data = OutputData::nodes(vec![OutputNode::new("a"), OutputNode::new("b")]);
        assert!(data.into_text().is_err());
    }

    #[test]
    fn into_text_empty() {
        let data = OutputData::text("");
        assert_eq!(data.into_text(), Ok("".to_string()));
    }
}
