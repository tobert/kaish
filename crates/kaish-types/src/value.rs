//! Value types for kaish's AST and runtime.

use serde::{Deserialize, Deserializer, Serialize, Serializer};

/// A literal value.
///
/// Supports primitives (null, bool, int, float, string), structured JSON data
/// (arrays and objects), and binary blob references.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    /// Structured JSON data (arrays, objects, nested structures).
    /// Use `jq` to query/extract values.
    Json(serde_json::Value),
    /// Inline binary data — the shell's single binary value. Carries the bytes
    /// themselves (keys, a file header, a random draw, a `dd` block), pipe-native
    /// since the pipe is already a byte buffer. Serializes as the base64 envelope
    /// from [`crate::bytes`]. Persisting large binary is a separate VFS concern
    /// (plain files under `/v/blobs`), not a `Value`. See `docs/binary-data.md`.
    Bytes(Vec<u8>),
}

impl Serialize for Value {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        // Delegate to value_to_json for consistent JSON representation.
        // Float NaN → null, Bytes → {_type: "bytes", ...}, Json → inline.
        crate::result::value_to_json(self).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Value {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let json = serde_json::Value::deserialize(deserializer)?;
        Ok(crate::result::json_to_value(json))
    }
}

