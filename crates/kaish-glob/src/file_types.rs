//! rg-style file-type filtering — the `--ftype` surface shared by search builtins.
//!
//! Wraps `ignore::types` so kaish's search builtins (`grep`, `glob`) share one
//! file-type model and one source of type names. `--ftype rust` selects `*.rs`;
//! `--ftype-not rust` excludes it; `--ftype-list` enumerates the known types.
//!
//! `--ftype` is the kaish-wide convention for file-type filtering (distinct from
//! glob's `-t`/`--type`, which is *entry kind* — file/dir/symlink, the fd convention).

use std::sync::Arc;

use ignore::types::{Types, TypesBuilder};
use thiserror::Error;

/// Failure building a file-type filter from `--ftype`/`--ftype-not` names.
#[derive(Debug, Error)]
pub enum FileTypeError {
    /// A requested type name isn't one of the known definitions. Loud by
    /// design — `--ftype nonsense` must fail, never silently match nothing.
    #[error("unknown file type '{0}' (see --ftype-list)")]
    UnknownType(String),
    /// A type definition's glob failed to compile. Shouldn't happen with the
    /// built-in defaults; surfaced rather than swallowed.
    #[error("file-type filter error: {0}")]
    Build(String),
}

/// Build a file-type filter from rg-style type names.
///
/// Returns `Ok(None)` when both lists are empty (no filtering configured).
/// `select` is a whitelist (only matching files pass); `negate` excludes
/// matching files; both may be combined per ripgrep semantics. Unknown names
/// are an error — never a silent empty match.
///
/// The returned `Types` goes straight into `WalkOptions::types`; the walker
/// applies it per-file and leaves directories untraversed-through (a `Types`
/// match on a directory is always `None`, so traversal is unaffected).
pub fn build_file_types(
    select: &[String],
    negate: &[String],
) -> Result<Option<Arc<Types>>, FileTypeError> {
    if select.is_empty() && negate.is_empty() {
        return Ok(None);
    }
    let mut builder = TypesBuilder::new();
    builder.add_defaults();
    for name in select {
        builder.select(name);
    }
    for name in negate {
        builder.negate(name);
    }
    match builder.build() {
        Ok(types) => Ok(Some(Arc::new(types))),
        Err(ignore::Error::UnrecognizedFileType(name)) => Err(FileTypeError::UnknownType(name)),
        Err(other) => Err(FileTypeError::Build(other.to_string())),
    }
}

/// All known file-type definitions as `(name, globs)` rows, for `--ftype-list`.
///
/// Sorted by name (the `ignore` crate sorts definitions and their globs).
pub fn list_file_types() -> Vec<(String, Vec<String>)> {
    let mut builder = TypesBuilder::new();
    builder.add_defaults();
    builder
        .definitions()
        .into_iter()
        .map(|def| (def.name().to_string(), def.globs().to_vec()))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_lists_mean_no_filter() {
        assert!(build_file_types(&[], &[]).expect("ok").is_none());
    }

    #[test]
    fn select_known_type_builds() {
        let types = build_file_types(&["rust".to_string()], &[]).expect("ok");
        assert!(types.is_some());
    }

    #[test]
    fn negate_known_type_builds() {
        let types = build_file_types(&[], &["rust".to_string()]).expect("ok");
        assert!(types.is_some());
    }

    #[test]
    fn unknown_type_is_loud() {
        let err = build_file_types(&["definitely-not-a-type".to_string()], &[])
            .expect_err("unknown type must error");
        assert!(
            matches!(err, FileTypeError::UnknownType(ref name) if name == "definitely-not-a-type"),
            "got {err:?}",
        );
    }

    #[test]
    fn list_is_nonempty_and_maps_rust_to_rs() {
        let defs = list_file_types();
        assert!(!defs.is_empty());
        let (_, globs) = defs
            .iter()
            .find(|(name, _)| name == "rust")
            .expect("rust type must be defined");
        assert!(globs.iter().any(|g| g == "*.rs"), "rust globs: {globs:?}");
    }
}
