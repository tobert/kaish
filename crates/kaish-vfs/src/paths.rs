//! Path normalization shared by the virtual backends.

use std::path::{Path, PathBuf};

/// Normalize a path: remove leading `/`, resolve `.` and `..`.
///
/// Virtual backends (`MemoryFs`, `OverlayFs`) key their internal maps on the
/// normalized form so `a/./b`, `/a/b`, and `a/b` all address the same entry.
pub(crate) fn normalize(path: &Path) -> PathBuf {
    let mut result = PathBuf::new();
    for component in path.components() {
        match component {
            std::path::Component::RootDir => {}
            std::path::Component::CurDir => {}
            std::path::Component::ParentDir => {
                result.pop();
            }
            std::path::Component::Normal(s) => {
                result.push(s);
            }
            std::path::Component::Prefix(_) => {}
        }
    }
    result
}
