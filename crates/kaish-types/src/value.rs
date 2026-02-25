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
    /// Reference to binary data stored in the virtual filesystem.
    Blob(BlobRef),
}

impl Serialize for Value {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        // Delegate to value_to_json for consistent JSON representation.
        // Float NaN → null, BlobRef → {_type: "blob", ...}, Json → inline.
        crate::result::value_to_json(self).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Value {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let json = serde_json::Value::deserialize(deserializer)?;
        Ok(crate::result::json_to_value(json))
    }
}

/// Reference to binary data stored in `/v/blobs/{id}`.
///
/// Binary data flows through the blob storage system rather than being
/// encoded as base64 in text fields.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BlobRef {
    /// Unique identifier, also the path suffix: `/v/blobs/{id}`
    pub id: String,
    /// Size of the blob in bytes.
    pub size: u64,
    /// MIME content type (e.g., "image/png", "application/octet-stream").
    pub content_type: String,
    /// Optional hash for integrity verification (SHA-256).
    pub hash: Option<Vec<u8>>,
}

impl BlobRef {
    /// Create a new blob reference.
    pub fn new(id: impl Into<String>, size: u64, content_type: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            size,
            content_type: content_type.into(),
            hash: None,
        }
    }

    /// Create a blob reference with a hash.
    pub fn with_hash(mut self, hash: Vec<u8>) -> Self {
        self.hash = Some(hash);
        self
    }

    /// Get the VFS path for this blob.
    pub fn path(&self) -> String {
        format!("/v/blobs/{}", self.id)
    }

    /// Format size for display (e.g., "1.2MB", "456KB").
    pub fn formatted_size(&self) -> String {
        const KB: u64 = 1024;
        const MB: u64 = 1024 * KB;
        const GB: u64 = 1024 * MB;

        if self.size >= GB {
            format!("{:.1}GB", self.size as f64 / GB as f64)
        } else if self.size >= MB {
            format!("{:.1}MB", self.size as f64 / MB as f64)
        } else if self.size >= KB {
            format!("{:.1}KB", self.size as f64 / KB as f64)
        } else {
            format!("{}B", self.size)
        }
    }
}
