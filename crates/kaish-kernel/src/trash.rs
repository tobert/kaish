//! Trash backend trait and types.
//!
//! Abstracts trash operations behind a trait so the implementation
//! can be swapped (system trash, WASI, pure-Rust disk trash, etc.)
//! without changing builtins.

#[cfg(feature = "native")]
use std::ffi::OsString;
use std::path::{Path, PathBuf};

use async_trait::async_trait;

/// Errors from trash operations.
#[derive(Debug, thiserror::Error)]
pub enum TrashError {
    #[error("{0}")]
    Backend(String),
    #[error("task join failed: {0}")]
    Join(String),
}

/// Opaque trash item identifier.
///
/// Wraps backend-specific IDs so callers don't depend on the `trash` crate directly.
pub struct TrashId(pub(crate) TrashIdInner);

pub(crate) enum TrashIdInner {
    /// System trash: wraps the `trash` crate's `OsString` ID.
    #[cfg(feature = "native")]
    System(OsString),
    /// Placeholder — TrashId is opaque and never constructed without a backend.
    #[cfg(not(feature = "native"))]
    _Unavailable,
}

impl TrashId {
    /// Create a TrashId wrapping a system trash item ID.
    #[cfg(feature = "native")]
    pub(crate) fn system(id: OsString) -> Self {
        Self(TrashIdInner::System(id))
    }
}

impl std::fmt::Debug for TrashId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.0 {
            #[cfg(feature = "native")]
            TrashIdInner::System(_) => f.write_str("TrashId::System(..)"),
            #[cfg(not(feature = "native"))]
            TrashIdInner::_Unavailable => f.write_str("TrashId::Unavailable"),
        }
    }
}

/// A trashed item, independent of backend.
#[derive(Debug)]
pub struct TrashEntry {
    pub id: TrashId,
    pub name: String,
    pub original_path: PathBuf,
    /// Seconds since UNIX epoch when the item was deleted.
    pub deleted_at: i64,
}

/// Find restore matches: exact (1) wins, else substring.
///
/// Single pass over items. Returns matched items or error message.
/// Used by backends that need name-based matching (e.g., SystemTrash).
pub fn find_restore_match<T>(items: Vec<(String, T)>, target: &str) -> Result<Vec<T>, String> {
    let mut exact = Vec::new();
    let mut substring = Vec::new();
    let mut substring_names = Vec::new();

    for (name, item) in items {
        if name == target {
            exact.push(item);
        } else if name.contains(target) {
            substring_names.push(name);
            substring.push(item);
        }
    }

    if exact.len() == 1 {
        return Ok(exact);
    }

    // Combine exact + substring if no single exact match
    let mut all_names: Vec<String> = Vec::new();
    if !exact.is_empty() {
        all_names.extend(std::iter::repeat_n(target.to_string(), exact.len()));
    }
    all_names.extend(substring_names);

    let mut all: Vec<T> = exact;
    all.extend(substring);

    if all.is_empty() {
        return Err(format!("'{}' not found in trash", target));
    }
    if all.len() > 1 {
        return Err(format!(
            "multiple matches for '{}': {}. Be more specific.",
            target,
            all_names.join(", ")
        ));
    }
    Ok(all)
}

/// Backend trait for trash operations.
#[async_trait]
pub trait TrashBackend: Send + Sync {
    /// Move a file or directory to trash.
    async fn trash(&self, path: &Path) -> Result<(), TrashError>;

    /// List trashed items, optionally filtered by name substring.
    async fn list(&self, filter: Option<&str>) -> Result<Vec<TrashEntry>, TrashError>;

    /// Find entries matching a name (exact first, then substring).
    ///
    /// Returns matched entries or an error describing the ambiguity.
    async fn find_by_name(&self, name: &str) -> Result<Vec<TrashEntry>, TrashError>;

    /// Restore trashed items to their original locations.
    async fn restore(&self, entries: Vec<TrashEntry>) -> Result<(), TrashError>;

    /// Permanently delete all trashed items. Returns count purged.
    async fn purge_all(&self) -> Result<usize, TrashError>;
}
