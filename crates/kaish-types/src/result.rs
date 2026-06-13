//! ExecResult — the structured result of every command execution.
//!
//! After every command in kaish, the special variable `$?` contains an ExecResult.

use std::borrow::Cow;
use std::collections::BTreeMap;

use crate::output::OutputData;
use crate::value::Value;

/// A command's stdout payload: text, or raw bytes.
///
/// `Text` xor `Bytes` — the enum makes the invalid both-set state
/// unrepresentable (an earlier draft used two sibling fields; see
/// `docs/binary-data.md`). Serializes wire-compatibly: `Text` is a bare JSON
/// string (unchanged from when `out` was a `String`), `Bytes` is the base64
/// envelope from [`crate::bytes`].
#[derive(Debug, Clone, PartialEq)]
pub enum OutputPayload {
    /// UTF-8 text — the common case, canonical for pipes.
    Text(String),
    /// Raw bytes — binary output (set by binary-aware builtins). Until the
    /// Phase-2 pipe/consumption rework, no builtin produces this in practice.
    Bytes(Vec<u8>),
}

impl Default for OutputPayload {
    fn default() -> Self {
        OutputPayload::Text(String::new())
    }
}

impl serde::Serialize for OutputPayload {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            // Bare string keeps the historical `"out":"…"` wire shape.
            OutputPayload::Text(t) => serializer.serialize_str(t),
            OutputPayload::Bytes(b) => crate::bytes::bytes_to_envelope(b).serialize(serializer),
        }
    }
}

impl<'de> serde::Deserialize<'de> for OutputPayload {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let v = serde_json::Value::deserialize(deserializer)?;
        match v {
            serde_json::Value::String(s) => Ok(OutputPayload::Text(s)),
            other => match crate::bytes::envelope_to_bytes(&other) {
                Some(b) => Ok(OutputPayload::Bytes(b)),
                None => Err(serde::de::Error::custom(
                    "ExecResult.out: expected a string or a base64 bytes envelope",
                )),
            },
        }
    }
}

/// Returned when a binary result is asked to behave as text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryNotText {
    /// Number of binary bytes that could not be coerced.
    pub len: usize,
}

impl std::fmt::Display for BinaryNotText {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "output is binary ({} bytes), not text — pipe through base64/xxd or redirect to a file",
            self.len
        )
    }
}

impl std::error::Error for BinaryNotText {}

/// The result of executing a command or pipeline.
///
/// `$?` in script syntax is the POSIX exit code (an integer). To read the
/// previous command's structured `.data` (or its captured stdout) from
/// inside a script, use the `kaish-last` builtin and pipe / capture its
/// output. Inside Rust callers, read `.data`, `.text_out()`, etc. directly.
///
/// Notes on the fields:
/// - `code` — exit code (0 = success)
/// - `err` — error message if failed
/// - `out` — raw stdout as string
/// - `data` — structured data; only set by builtins/tools that opt in
///   (e.g. `seq`, `jq`, `cut`, `find`, `glob`, `split`). External commands
///   never populate this — pipe their stdout through `jq` to get it.
#[derive(Debug, Clone, Default, PartialEq, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct ExecResult {
    /// Exit code. 0 means success.
    pub code: i64,
    /// Standard output payload — text (canonical for pipes) or raw bytes.
    out: OutputPayload,
    /// Raw standard error as a string.
    pub err: String,
    /// Structured data — only populated when a builtin/tool sets it explicitly.
    /// Stdout is *never* sniffed; this stays `None` for external commands.
    pub data: Option<Value>,
    /// Structured output data for rendering.
    output: Option<OutputData>,
    /// True if the output limiter capped this result. Either the overflow was
    /// written to a disk spill file (the `out` message carries the path) or it
    /// was truncated in memory (Memory spill mode — head+tail only, no
    /// recoverable file). Both cases remap the exit code to 3.
    pub did_spill: bool,
    /// The command's original exit code before spill logic overwrote it with 2 or 3.
    /// Present only when `did_spill` is true and `code` was changed.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub original_code: Option<i64>,
    /// MIME content type hint (e.g., "text/markdown", "image/svg+xml").
    /// When set, downstream consumers can use this instead of sniffing content.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub content_type: Option<String>,
    /// Opaque key-value context propagated from tools through execution.
    /// Intermediaries (kaish) carry but don't interpret. Consumers read known keys.
    /// Follows W3C Baggage semantics — useful for OTel trace propagation,
    /// application-level hints, etc.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub baggage: BTreeMap<String, String>,
}

impl ExecResult {
    /// Create a successful result with output.
    pub fn success(out: impl Into<String>) -> Self {
        Self {
            code: 0,
            out: OutputPayload::Text(out.into()),
            err: String::new(),
            data: None,
            output: None,
            did_spill: false,
            original_code: None,
            content_type: None,
            baggage: BTreeMap::new(),
        }
    }

    /// Create a successful result with structured output data.
    ///
    /// The `OutputData` is the source of truth. Text is materialized lazily
    /// via `text_out()` when needed (pipes, redirects, command substitution).
    pub fn with_output(output: OutputData) -> Self {
        // Simple text: move string into .out directly for efficient Cow::Borrowed.
        // Structured output: store in .output, materialize lazily.
        match output.into_text() {
            Ok(text) => Self::success(text),
            Err(output) => Self {
                code: 0,
                out: OutputPayload::Text(String::new()),
                err: String::new(),
                data: None,
                output: Some(output),
                did_spill: false,
                original_code: None,
                content_type: None,
                baggage: BTreeMap::new(),
            },
        }
    }

    /// Create a successful result whose stdout is raw bytes (binary payload).
    pub fn success_bytes(bytes: Vec<u8>) -> Self {
        let mut r = Self::success("");
        r.out = OutputPayload::Bytes(bytes);
        r
    }

    /// Create a successful result with structured data.
    pub fn success_data(data: Value) -> Self {
        let out = value_to_json(&data).to_string();
        Self {
            code: 0,
            out: OutputPayload::Text(out),
            err: String::new(),
            data: Some(data),
            output: None,
            did_spill: false,
            original_code: None,
            content_type: None,
            baggage: BTreeMap::new(),
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
            out: OutputPayload::Text(out.into()),
            err: String::new(),
            data: Some(data),
            output: None,
            did_spill: false,
            original_code: None,
            content_type: None,
            baggage: BTreeMap::new(),
        }
    }

    /// Create a failed result with an error message.
    pub fn failure(code: i64, err: impl Into<String>) -> Self {
        Self {
            code,
            out: OutputPayload::Text(String::new()),
            err: err.into(),
            data: None,
            output: None,
            did_spill: false,
            original_code: None,
            content_type: None,
            baggage: BTreeMap::new(),
        }
    }

    /// Create a result from raw output streams.
    ///
    /// `data` is left empty — kaish does not sniff stdout for JSON. To get
    /// structured iteration from an external command, pipe through `jq`:
    /// `for i in $(curl ... | jq .); do ...`.
    pub fn from_output(code: i64, stdout: impl Into<String>, stderr: impl Into<String>) -> Self {
        Self {
            code,
            out: OutputPayload::Text(stdout.into()),
            err: stderr.into(),
            data: None,
            output: None,
            did_spill: false,
            original_code: None,
            content_type: None,
            baggage: BTreeMap::new(),
        }
    }

    /// Create a successful result with structured output and explicit pipe text.
    ///
    /// Use this when a builtin needs custom text formatting that differs from
    /// the canonical `OutputData::to_canonical_string()` representation.
    pub fn with_output_and_text(output: OutputData, text: impl Into<String>) -> Self {
        Self {
            code: 0,
            out: OutputPayload::Text(text.into()),
            err: String::new(),
            data: None,
            output: Some(output),
            did_spill: false,
            original_code: None,
            content_type: None,
            baggage: BTreeMap::new(),
        }
    }

    /// Create a result from parts — for kernel struct literal sites.
    pub fn from_parts(
        code: i64,
        out: String,
        err: String,
        data: Option<Value>,
    ) -> Self {
        Self {
            code,
            out: OutputPayload::Text(out),
            err,
            data,
            output: None,
            did_spill: false,
            original_code: None,
            content_type: None,
            baggage: BTreeMap::new(),
        }
    }

    /// Builder: set the exit code, returning self for chaining.
    pub fn with_code(mut self, code: i64) -> Self {
        self.code = code;
        self
    }

    // ── Read accessors ──

    /// Get text output, materializing from OutputData on demand.
    ///
    /// Returns the text payload if non-empty, otherwise falls back to
    /// `OutputData::to_canonical_string()`. This is the canonical way to
    /// get text for pipes, command substitution, and file redirects.
    ///
    /// **Binary payloads** decode lossily here (`U+FFFD` for invalid UTF-8). No
    /// builtin produces a `Bytes` payload yet (Phase 1), so this lossy path is
    /// unreachable in practice; the loud-error guard for binary lives in
    /// [`Self::try_text_out`], which the Phase-2 text sinks adopt. See
    /// `docs/binary-data.md`.
    pub fn text_out(&self) -> Cow<'_, str> {
        match &self.out {
            OutputPayload::Text(s) if !s.is_empty() => Cow::Borrowed(s),
            OutputPayload::Bytes(b) => match std::str::from_utf8(b) {
                Ok(s) => Cow::Borrowed(s),
                Err(_) => Cow::Owned(String::from_utf8_lossy(b).into_owned()),
            },
            // Empty text → fall back to structured output's canonical string.
            _ => match self.output {
                Some(ref output) => Cow::Owned(output.to_canonical_string()),
                None => Cow::Borrowed(""),
            },
        }
    }

    /// Get text output, or a [`BinaryNotText`] error if the payload is binary
    /// and not valid UTF-8. This is the boundary guard for text sinks (`echo`,
    /// interpolation, `$()` capture) — adopted as those paths grow byte
    /// awareness (Phase 2). Valid-UTF-8 bytes coerce; everything else is loud.
    pub fn try_text_out(&self) -> Result<Cow<'_, str>, BinaryNotText> {
        match &self.out {
            OutputPayload::Bytes(b) => std::str::from_utf8(b)
                .map(Cow::Borrowed)
                .map_err(|_| BinaryNotText { len: b.len() }),
            _ => Ok(self.text_out()),
        }
    }

    /// Raw bytes if this result carries a binary payload, else `None`.
    pub fn out_bytes(&self) -> Option<&[u8]> {
        match &self.out {
            OutputPayload::Bytes(b) => Some(b),
            OutputPayload::Text(_) => None,
        }
    }

    /// True if the stdout payload is raw bytes rather than text.
    pub fn is_bytes(&self) -> bool {
        matches!(self.out, OutputPayload::Bytes(_))
    }

    /// Get a reference to structured output data.
    pub fn output(&self) -> Option<&OutputData> {
        self.output.as_ref()
    }

    /// True if structured output data is present.
    pub fn has_output(&self) -> bool {
        self.output.is_some()
    }

    // ── Mutation accessors ──

    /// Replace `.out` with text.
    pub fn set_out(&mut self, s: String) {
        self.out = OutputPayload::Text(s);
    }

    /// Replace `.out` with raw bytes (binary payload).
    pub fn set_out_bytes(&mut self, b: Vec<u8>) {
        self.out = OutputPayload::Bytes(b);
    }

    /// Append text to `.out`. A binary payload is appended to as raw UTF-8 bytes.
    pub fn push_out(&mut self, s: &str) {
        match &mut self.out {
            OutputPayload::Text(t) => t.push_str(s),
            OutputPayload::Bytes(b) => b.extend_from_slice(s.as_bytes()),
        }
    }

    /// Clear `.out` back to empty text.
    pub fn clear_out(&mut self) {
        self.out = OutputPayload::Text(String::new());
    }

    /// Replace `.output`.
    pub fn set_output(&mut self, o: Option<OutputData>) {
        self.output = o;
    }

    /// Take `.output`, leaving None.
    pub fn take_output(&mut self) -> Option<OutputData> {
        self.output.take()
    }

    /// Materialize: if `.out` is empty and `.output` is present,
    /// populate `.out` from canonical string and clear `.output`.
    pub fn materialize(&mut self) {
        if matches!(&self.out, OutputPayload::Text(s) if s.is_empty()) {
            if let Some(ref output) = self.output {
                self.out = OutputPayload::Text(output.to_canonical_string());
            }
        }
        self.output = None;
    }

    /// Take `.output` only if `.out` is empty (no custom text),
    /// so caller can stream directly without materializing.
    pub fn take_output_for_stream(&mut self) -> Option<OutputData> {
        if matches!(&self.out, OutputPayload::Text(s) if s.is_empty()) {
            self.output.take()
        } else {
            None
        }
    }

    /// True if the command succeeded (exit code 0).
    pub fn ok(&self) -> bool {
        self.code == 0
    }

    /// Set content type hint, returning self for chaining.
    pub fn with_content_type(mut self, ct: impl Into<String>) -> Self {
        self.content_type = Some(ct.into());
        self
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
        // A base64 byte envelope round-trips back to inline Bytes; any other
        // object/array stays structured Json.
        serde_json::Value::Object(_) => match crate::bytes::envelope_to_bytes(&json) {
            Some(bytes) => Value::Bytes(bytes),
            None => Value::Json(json),
        },
        serde_json::Value::Array(_) => Value::Json(json),
    }
}

/// Convert our AST Value to serde_json::Value for serialization.
pub fn value_to_json(value: &Value) -> serde_json::Value {
    match value {
        Value::Null => serde_json::Value::Null,
        Value::Bool(b) => serde_json::Value::Bool(*b),
        Value::Int(i) => serde_json::Value::Number((*i).into()),
        Value::Float(f) => {
            // JSON has no NaN/Infinity. Rather than silently collapse them to
            // null (data loss), serialize the non-finite value to its string
            // form ("NaN", "inf", "-inf") so the information survives the trip.
            serde_json::Number::from_f64(*f)
                .map(serde_json::Value::Number)
                .unwrap_or_else(|| serde_json::Value::String(f.to_string()))
        }
        Value::String(s) => serde_json::Value::String(s.clone()),
        Value::Json(json) => json.clone(),
        Value::Bytes(data) => crate::bytes::bytes_to_envelope(data),
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
        assert_eq!(&*result.text_out(),"hello world");
        assert!(result.err.is_empty());
    }

    #[test]
    fn value_to_json_finite_float_is_number() {
        assert_eq!(value_to_json(&Value::Float(3.5)), serde_json::json!(3.5));
    }

    #[test]
    fn value_to_json_non_finite_float_serializes_to_string() {
        // JSON has no NaN/Infinity — preserve the info as a string, never null.
        assert_eq!(value_to_json(&Value::Float(f64::NAN)), serde_json::json!("NaN"));
        assert_eq!(value_to_json(&Value::Float(f64::INFINITY)), serde_json::json!("inf"));
        assert_eq!(
            value_to_json(&Value::Float(f64::NEG_INFINITY)),
            serde_json::json!("-inf")
        );
        // Crucially: not null (the old data-losing behavior).
        assert_ne!(value_to_json(&Value::Float(f64::NAN)), serde_json::Value::Null);
    }

    #[test]
    fn failure_creates_non_ok_result() {
        let result = ExecResult::failure(1, "command not found");
        assert!(!result.ok());
        assert_eq!(result.code, 1);
        assert_eq!(result.err, "command not found");
    }

    #[test]
    fn success_does_not_sniff_json_stdout() {
        // External-command stdout is never sniffed for JSON. Tools that want
        // structured data must call success_with_data() / success_data().
        let result = ExecResult::success(r#"{"count": 42, "items": ["a", "b"]}"#);
        assert!(result.data.is_none());
        assert_eq!(&*result.text_out(),r#"{"count": 42, "items": ["a", "b"]}"#);
    }

    #[test]
    fn from_output_does_not_sniff_json_stdout() {
        let result = ExecResult::from_output(0, r#"[1, 2, 3]"#, "");
        assert!(result.data.is_none());
        assert_eq!(&*result.text_out(),"[1, 2, 3]");
    }

    #[test]
    fn non_json_stdout_has_no_data() {
        let result = ExecResult::success("just plain text");
        assert!(result.data.is_none());
    }

    #[test]
    fn success_data_creates_result_with_value() {
        let value = Value::String("test data".into());
        let result = ExecResult::success_data(value.clone());
        assert!(result.ok());
        assert_eq!(result.data, Some(value));
    }

    #[test]
    fn did_spill_defaults_to_false() {
        assert!(!ExecResult::success("hi").did_spill);
        assert!(!ExecResult::failure(1, "err").did_spill);
        assert!(!ExecResult::from_output(0, "out", "err").did_spill);
    }

    #[test]
    fn did_spill_is_serialized() {
        let mut result = ExecResult::success("hi");
        result.did_spill = true;
        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("\"did_spill\":true"));
    }

    #[test]
    fn original_code_omitted_when_none() {
        let result = ExecResult::success("hi");
        let json = serde_json::to_string(&result).unwrap();
        assert!(!json.contains("original_code"));
    }

    #[test]
    fn original_code_present_when_set() {
        let mut result = ExecResult::success("hi");
        result.original_code = Some(0);
        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("\"original_code\":0"));
    }

    #[test]
    fn default_is_empty_success() {
        let result = ExecResult::default();
        assert!(result.ok());
        assert!(result.text_out().is_empty());
        assert!(result.data.is_none());
        assert!(result.content_type.is_none());
        assert!(result.baggage.is_empty());
    }

    #[test]
    fn from_parts_creates_result() {
        let result = ExecResult::from_parts(42, "out".into(), "err".into(), None);
        assert_eq!(result.code, 42);
        assert_eq!(&*result.text_out(),"out");
        assert_eq!(result.err, "err");
        assert!(result.data.is_none());
        assert!(result.output.is_none());
    }

    #[test]
    fn with_code_sets_code() {
        let result = ExecResult::success("hi").with_code(42);
        assert_eq!(result.code, 42);
        assert_eq!(&*result.text_out(),"hi");
    }

    #[test]
    fn output_getter() {
        use crate::output::{OutputData, OutputNode};
        // Use structured (non-text) output so with_output preserves .output
        let nodes = OutputData::nodes(vec![OutputNode::new("a"), OutputNode::new("b")]);
        let result = ExecResult::with_output(nodes);
        assert!(result.output().is_some());
        assert!(result.has_output());

        // Simple text now routes to .out, so output is None
        let text_result = ExecResult::with_output(OutputData::text("test"));
        assert!(!text_result.has_output());
        assert_eq!(&*text_result.text_out(), "test");

        let plain = ExecResult::success("text");
        assert!(plain.output().is_none());
        assert!(!plain.has_output());
    }

    #[test]
    fn set_out_and_push_out_and_clear_out() {
        let mut result = ExecResult::success("");
        result.set_out("hello".into());
        assert_eq!(&*result.text_out(),"hello");
        result.push_out(" world");
        assert_eq!(&*result.text_out(),"hello world");
        result.clear_out();
        assert!(result.text_out().is_empty());
    }

    #[test]
    fn set_output_and_take_output() {
        use crate::output::OutputData;
        let mut result = ExecResult::success("");
        assert!(result.take_output().is_none());

        result.set_output(Some(OutputData::text("data")));
        assert!(result.has_output());

        let taken = result.take_output();
        assert!(taken.is_some());
        assert!(!result.has_output());
    }

    #[test]
    fn materialize_populates_out_from_output() {
        use crate::output::{OutputData, OutputNode};
        // Use structured output to test materialization
        let nodes = OutputData::nodes(vec![OutputNode::new("a"), OutputNode::new("b")]);
        let mut result = ExecResult::with_output(nodes);
        // Raw text payload is empty before materialize (text_out() would
        // already fall back to the OutputData canonical string).
        assert!(matches!(&result.out, OutputPayload::Text(s) if s.is_empty()));
        assert!(result.has_output());
        result.materialize();
        assert_eq!(&*result.text_out(),"a\nb");
        assert!(result.output.is_none());
    }

    #[test]
    fn value_bytes_round_trips_through_envelope() {
        let v = Value::Bytes(vec![0u8, 1, 2, 255, 128]);
        let json = value_to_json(&v);
        assert_eq!(json["_type"], "bytes");
        assert_eq!(json["len"], 5);
        // json_to_value recognizes the envelope and reconstructs Bytes.
        assert_eq!(json_to_value(json), v);
        // A plain object is NOT mistaken for bytes.
        let obj = serde_json::json!({"name": "amy"});
        assert!(matches!(json_to_value(obj), Value::Json(_)));
    }

    #[test]
    fn output_payload_text_serializes_as_bare_string() {
        // Wire compatibility: a text result's `out` stays a plain JSON string,
        // exactly as when `out` was a `String`.
        let r = ExecResult::success("hello");
        let json: serde_json::Value = serde_json::from_str(&serde_json::to_string(&r).unwrap()).unwrap();
        assert_eq!(json["out"], "hello");
        // Round-trips back to a Text payload.
        let back: ExecResult = serde_json::from_value(json).unwrap();
        assert_eq!(&*back.text_out(), "hello");
        assert!(!back.is_bytes());
    }

    #[test]
    fn success_bytes_carries_binary_and_round_trips() {
        let r = ExecResult::success_bytes(vec![0u8, 159, 146, 150]); // invalid UTF-8
        assert!(r.is_bytes());
        assert_eq!(r.out_bytes(), Some(&[0u8, 159, 146, 150][..]));
        // try_text_out is the loud guard: invalid UTF-8 → error, not mangling.
        assert!(r.try_text_out().is_err());
        // text_out (infallible) decodes lossily — Phase-1 fallback.
        assert!(r.text_out().contains('\u{fffd}'));
        // Serializes as a base64 envelope and round-trips back to bytes.
        let json: serde_json::Value = serde_json::to_value(&r).unwrap();
        assert_eq!(json["out"]["_type"], "bytes");
        let back: ExecResult = serde_json::from_value(json).unwrap();
        assert_eq!(back.out_bytes(), Some(&[0u8, 159, 146, 150][..]));
    }

    #[test]
    fn valid_utf8_bytes_coerce_to_text() {
        let r = ExecResult::success_bytes(b"plain text".to_vec());
        assert!(r.is_bytes());
        assert_eq!(r.try_text_out().unwrap(), "plain text");
        assert_eq!(&*r.text_out(), "plain text");
    }

    #[test]
    fn materialize_preserves_existing_out() {
        use crate::output::OutputData;
        let mut result = ExecResult::with_output_and_text(OutputData::text("ignored"), "custom");
        result.materialize();
        assert_eq!(&*result.text_out(),"custom");
    }

    #[test]
    fn take_output_for_stream_when_out_empty() {
        use crate::output::{OutputData, OutputNode};
        // Use structured output — text now goes to .out directly
        let nodes = OutputData::nodes(vec![OutputNode::new("a")]);
        let mut result = ExecResult::with_output(nodes);
        let taken = result.take_output_for_stream();
        assert!(taken.is_some());
        assert!(!result.has_output());
    }

    #[test]
    fn with_output_simple_text_populates_out_directly() {
        use crate::output::OutputData;
        let result = ExecResult::with_output(OutputData::text("hello"));
        // Simple text should go to .out, not .output
        assert!(!result.has_output());
        assert_eq!(&*result.text_out(), "hello");
        // Even JSON-shaped text is NOT auto-parsed — .data stays None.
        let json_result = ExecResult::with_output(OutputData::text(r#"{"key": 1}"#));
        assert!(json_result.data.is_none());
    }

    #[test]
    fn take_output_for_stream_when_out_populated() {
        use crate::output::OutputData;
        let mut result = ExecResult::with_output_and_text(OutputData::text("x"), "custom");
        let taken = result.take_output_for_stream();
        assert!(taken.is_none());
        assert!(result.has_output()); // not taken
    }
}
