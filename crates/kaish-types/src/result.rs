//! ExecResult — the structured result of every command execution.
//!
//! After every command in kaish, the special variable `$?` contains an ExecResult.

use crate::output::OutputData;
use crate::value::Value;

/// The result of executing a command or pipeline.
///
/// Fields accessible via `${?.field}`:
/// - `code` — exit code (0 = success)
/// - `ok` — true if code == 0
/// - `err` — error message if failed
/// - `out` — raw stdout as string
/// - `data` — parsed JSON from stdout (if valid JSON)
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
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
    ///
    /// **JSON auto-detection**: On success (code 0), stdout is checked for valid
    /// JSON. If it parses, the result is stored in `.data` as structured data.
    /// This enables `for i in $(external-command)` to iterate over JSON arrays
    /// returned by MCP tools and external commands. This is intentional — external
    /// tools communicate structured data via JSON stdout, and kaish makes it
    /// available for iteration without requiring manual `jq` parsing.
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
        let result = ExecResult::success(r#"{"count": 42, "items": ["a", "b"]}"#);
        assert!(result.data.is_some());
        let data = result.data.unwrap();
        assert!(matches!(data, Value::Json(_)));
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
}
