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
//! # Display Hints
//!
//! Commands can return display hints to indicate how output should be formatted
//! for different audiences (humans vs. models):
//!
//! - `DisplayHint::None` — Use raw `out` as-is
//! - `DisplayHint::Table` — Tabular data, REPL handles column layout
//! - `DisplayHint::Tree` — Tree structure, REPL chooses format style

use crate::ast::Value;

/// Display hint for command output.
///
/// Tools can specify how their output should be formatted for different audiences:
/// - **Humans** → Pretty columns, colors, traditional tree
/// - **Models** → Token-efficient compact formats (brace notation, JSON)
#[derive(Debug, Clone, Default, PartialEq)]
pub enum DisplayHint {
    /// No special formatting - use raw `out` as-is.
    #[default]
    None,

    /// Tabular data - REPL handles column layout.
    Table {
        /// Optional column headers.
        headers: Option<Vec<String>>,
        /// Table rows (each row is a vector of cell values).
        rows: Vec<Vec<String>>,
        /// Entry metadata for coloring (is_dir, is_executable, etc.).
        entry_types: Option<Vec<EntryType>>,
    },

    /// Tree structure - REPL chooses traditional vs compact.
    Tree {
        /// Root directory name.
        root: String,
        /// Tree structure as JSON for flexible rendering.
        structure: serde_json::Value,
        /// Pre-rendered traditional format (for human display).
        traditional: String,
        /// Pre-rendered compact format (for model/piped display).
        compact: String,
    },
}

/// Entry type for colorizing file listings.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EntryType {
    /// Regular file.
    File,
    /// Directory.
    Directory,
    /// Executable file.
    Executable,
    /// Symbolic link.
    Symlink,
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
    /// Display hint for formatting output.
    pub hint: DisplayHint,
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
            hint: DisplayHint::default(),
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
            hint: DisplayHint::default(),
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
            hint: DisplayHint::default(),
        }
    }

    /// Create a failed result with an error message.
    pub fn failure(code: i64, err: impl Into<String>) -> Self {
        Self {
            code,
            out: String::new(),
            err: err.into(),
            data: None,
            hint: DisplayHint::default(),
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
            hint: DisplayHint::default(),
        }
    }

    /// Create a successful result with table data for smart column layout.
    ///
    /// The REPL will format this as columns for TTY or one-per-line for piped output.
    pub fn success_table(
        rows: Vec<Vec<String>>,
        entry_types: Option<Vec<EntryType>>,
    ) -> Self {
        // Build canonical output (one entry per line, first column only)
        let out = rows.iter()
            .filter_map(|row| row.first())
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        Self {
            code: 0,
            out,
            err: String::new(),
            data: None,
            hint: DisplayHint::Table {
                headers: None,
                rows,
                entry_types,
            },
        }
    }

    /// Create a successful result with table data including headers.
    pub fn success_table_with_headers(
        headers: Vec<String>,
        rows: Vec<Vec<String>>,
        entry_types: Option<Vec<EntryType>>,
    ) -> Self {
        // Build canonical output (one entry per line, first column only)
        let out = rows.iter()
            .filter_map(|row| row.first())
            .cloned()
            .collect::<Vec<_>>()
            .join("\n");
        Self {
            code: 0,
            out,
            err: String::new(),
            data: None,
            hint: DisplayHint::Table {
                headers: Some(headers),
                rows,
                entry_types,
            },
        }
    }

    /// Create a successful result with tree structure for format selection.
    ///
    /// - `root` — Root directory name
    /// - `structure` — Tree as JSON for flexible rendering
    /// - `traditional` — Pre-rendered traditional format with box-drawing
    /// - `compact` — Pre-rendered compact brace notation
    pub fn success_tree(
        root: String,
        structure: serde_json::Value,
        traditional: String,
        compact: String,
    ) -> Self {
        Self {
            code: 0,
            out: compact.clone(), // Use compact format as canonical for pipes
            err: String::new(),
            data: None,
            hint: DisplayHint::Tree {
                root,
                structure,
                traditional,
                compact,
            },
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

    // --- Tests for DisplayHint and new constructors ---

    #[test]
    fn default_hint_is_none() {
        let result = ExecResult::success("test");
        assert_eq!(result.hint, DisplayHint::None);
    }

    #[test]
    fn success_table_sets_hint() {
        let rows = vec![
            vec!["file1.txt".to_string()],
            vec!["file2.txt".to_string()],
        ];
        let types = vec![EntryType::File, EntryType::Directory];
        let result = ExecResult::success_table(rows.clone(), Some(types.clone()));

        assert!(result.ok());
        // Canonical output is names joined by newlines
        assert_eq!(result.out, "file1.txt\nfile2.txt");
        match result.hint {
            DisplayHint::Table { headers, rows: r, entry_types } => {
                assert!(headers.is_none());
                assert_eq!(r.len(), 2);
                assert_eq!(entry_types, Some(types));
            }
            _ => panic!("Expected Table hint"),
        }
    }

    #[test]
    fn success_table_with_headers_sets_hint() {
        let headers = vec!["Name".to_string(), "Size".to_string()];
        let rows = vec![
            vec!["foo.rs".to_string(), "1024".to_string()],
            vec!["bar.rs".to_string(), "2048".to_string()],
        ];
        let result = ExecResult::success_table_with_headers(
            headers.clone(),
            rows.clone(),
            None,
        );

        assert!(result.ok());
        // Canonical output uses first column
        assert_eq!(result.out, "foo.rs\nbar.rs");
        match result.hint {
            DisplayHint::Table { headers: h, rows: r, entry_types } => {
                assert_eq!(h, Some(headers));
                assert_eq!(r.len(), 2);
                assert!(entry_types.is_none());
            }
            _ => panic!("Expected Table hint"),
        }
    }

    #[test]
    fn success_tree_sets_hint() {
        let structure = serde_json::json!({"src": {"main.rs": null}});
        let result = ExecResult::success_tree(
            "myproject".to_string(),
            structure.clone(),
            "myproject/\n└── src/\n    └── main.rs".to_string(),
            "myproject/{src/{main.rs}}".to_string(),
        );

        assert!(result.ok());
        // Canonical output is compact format
        assert_eq!(result.out, "myproject/{src/{main.rs}}");
        match result.hint {
            DisplayHint::Tree { root, structure: s, traditional, compact } => {
                assert_eq!(root, "myproject");
                assert_eq!(s, structure);
                assert!(traditional.contains("└──"));
                assert!(compact.contains("{"));
            }
            _ => panic!("Expected Tree hint"),
        }
    }

    #[test]
    fn entry_type_variants() {
        // Just verify the enum variants exist and are distinct
        assert_ne!(EntryType::File, EntryType::Directory);
        assert_ne!(EntryType::Directory, EntryType::Executable);
        assert_ne!(EntryType::Executable, EntryType::Symlink);
    }

    #[test]
    fn display_hint_default_is_none() {
        let hint: DisplayHint = Default::default();
        assert_eq!(hint, DisplayHint::None);
    }
}
