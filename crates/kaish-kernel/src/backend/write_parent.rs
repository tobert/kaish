//! Hints that name a usable path or command in a redirect error.

use std::path::Path;

use super::KernelBackend;

/// Suggest the nearest declared mount whose root is writable and searchable.
pub(crate) async fn mounted_path_hint(backend: &dyn KernelBackend, path: &Path) -> String {
    let mut mounts = backend.mounts();
    mounts.sort_by_key(|mount| {
        let shared = path.components().zip(mount.path.components())
            .take_while(|(left, right)| left == right).count();
        (std::cmp::Reverse(shared), mount.path.clone())
    });
    for mount in mounts.into_iter().filter(|mount| !mount.read_only) {
        // Skip a root that cannot be probed; its error is not the redirect's.
        if let Ok(access) = backend.path_access(&mount.path).await
            && access.writable && access.executable
        {
            return format!("write under {}", hint_path(&mount.path));
        }
    }
    "no writable mounted path is available".to_string()
}

/// Quote a path as one word in the suggested command.
pub(crate) fn hint_path(path: &Path) -> String {
    let text = path.to_string_lossy();
    if text
        .chars()
        .all(|character| character.is_alphanumeric() || "/._-".contains(character))
    {
        text.into_owned()
    } else {
        // Double quotes keep apostrophes literal without joined shell words.
        format!(
            "\"{}\"",
            text.replace('\\', "\\\\")
                .replace('"', "\\\"")
                .replace('$', "\\$")
        )
    }
}
