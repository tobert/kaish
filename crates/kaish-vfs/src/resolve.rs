//! Root-scoped path resolution with an explicit symlink policy.
//!
//! [`resolve_beneath`] is the one function a backend rooted at a host
//! directory calls before every operation. It maps a root-relative path to
//! a host path, follows symlinks according to [`Follow`], and refuses any
//! result outside the root. The containment check lives inside so that a
//! caller cannot forget it.

use std::io;
use std::path::{Component, Path, PathBuf};

/// Whether the last path component follows a symlink.
///
/// Every policy follows symlinks in the parent directories, as the OS does
/// for every call including `lstat(2)`. The policies differ only on the
/// final component.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Follow {
    /// Follow the final component. For read, write, stat, list, mkdir:
    /// the operation acts on the link's target.
    ///
    /// Containment is checked on the fully resolved path, so a link inside
    /// the root that points outside it is refused.
    Final,
    /// Keep the final component literal. For remove, both sides of rename,
    /// lstat, read_link, and the link side of symlink: the operation acts
    /// on the link itself.
    ///
    /// Containment is checked on the resolved parent plus the literal
    /// name. A link inside the root that points outside it passes, because
    /// the link is the object of the operation. An operation that then
    /// writes through the returned path has weaker containment than under
    /// `Final`.
    ParentOnly,
}

/// Resolve `path` under `root` and refuse any result outside `root`.
///
/// `path` is root-relative; a leading `/` is stripped. `.` and `..` are
/// resolved lexically first, and `..` above the root is an error before
/// anything touches the filesystem. Components that do not exist yet are
/// appended literally to the deepest existing ancestor, so a path for a
/// file about to be created resolves the same way as an existing one.
///
/// Errors: `PermissionDenied` when the result is outside `root`;
/// `NotFound` when `root` itself does not exist; any I/O error from
/// canonicalizing an existing ancestor.
pub fn resolve_beneath(root: &Path, path: &Path, follow: Follow) -> io::Result<PathBuf> {
    let canonical_root = root.canonicalize().map_err(|error| {
        io::Error::new(
            error.kind(),
            format!("mount root {}: {error}", root.display()),
        )
    })?;
    let lexical = normalize_under(root, path)?;

    let resolved = match follow {
        // A dangling link does not "exist", so it falls through to the
        // parent case and stays literal, which is what open(2) then sees.
        Follow::Final if lexical.exists() => lexical.canonicalize()?,
        Follow::Final | Follow::ParentOnly => {
            if lexical == root {
                canonical_root.clone()
            } else {
                canonicalize_deepest_ancestor(root, &lexical)?
            }
        }
    };

    if !resolved.starts_with(&canonical_root) {
        return Err(io::Error::new(
            io::ErrorKind::PermissionDenied,
            format!(
                "path escapes root: {} is not under {}",
                resolved.display(),
                canonical_root.display()
            ),
        ));
    }
    Ok(resolved)
}

/// Join `path` onto `root` resolving `.` and `..` lexically. `..` above
/// the root is refused without touching the filesystem.
fn normalize_under(root: &Path, path: &Path) -> io::Result<PathBuf> {
    let path = path.strip_prefix("/").unwrap_or(path);
    let mut normalized = root.to_path_buf();
    for component in path.components() {
        match component {
            Component::ParentDir => {
                if normalized == root || !normalized.pop() || !normalized.starts_with(root) {
                    return Err(io::Error::new(
                        io::ErrorKind::PermissionDenied,
                        format!("path escapes root: {}", path.display()),
                    ));
                }
            }
            Component::Normal(name) => normalized.push(name),
            Component::CurDir => {}
            Component::RootDir | Component::Prefix(_) => {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!("path is not root-relative: {}", path.display()),
                ));
            }
        }
    }
    Ok(normalized)
}

/// Canonicalize the deepest existing ancestor of `lexical` (its parent or
/// above; never `lexical` itself) and append the remaining components
/// literally.
fn canonicalize_deepest_ancestor(root: &Path, lexical: &Path) -> io::Result<PathBuf> {
    let mut ancestor = lexical.parent().unwrap_or(root);
    while ancestor != root && !ancestor.exists() {
        ancestor = ancestor.parent().unwrap_or(root);
    }
    let canonical = ancestor.canonicalize()?;
    let remainder = lexical.strip_prefix(ancestor).map_err(|_| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("path {} is not under {}", lexical.display(), ancestor.display()),
        )
    })?;
    Ok(canonical.join(remainder))
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;

    fn root() -> tempfile::TempDir {
        tempfile::tempdir().expect("tempdir")
    }

    #[test]
    fn dotdot_above_root_is_refused_before_any_io() {
        let dir = root();
        // Nothing under this path exists, so a resolver that only checks
        // existing paths would fall through to a raw join.
        let error = resolve_beneath(dir.path(), Path::new("../../nonexistent/x"), Follow::Final)
            .expect_err("escape");
        assert_eq!(error.kind(), io::ErrorKind::PermissionDenied);
        let error = resolve_beneath(dir.path(), Path::new("a/../../x"), Follow::ParentOnly)
            .expect_err("escape");
        assert_eq!(error.kind(), io::ErrorKind::PermissionDenied);
    }

    #[test]
    fn dotdot_inside_root_is_resolved_lexically() {
        let dir = root();
        let resolved = resolve_beneath(dir.path(), Path::new("a/../b"), Follow::Final).expect("ok");
        assert_eq!(resolved, dir.path().canonicalize().expect("root").join("b"));
    }

    #[test]
    fn a_missing_chain_is_appended_to_the_deepest_existing_ancestor() {
        let dir = root();
        std::fs::create_dir(dir.path().join("a")).expect("mkdir");
        let resolved =
            resolve_beneath(dir.path(), Path::new("a/b/c/d"), Follow::ParentOnly).expect("ok");
        assert_eq!(
            resolved,
            dir.path().canonicalize().expect("root").join("a/b/c/d")
        );
    }

    #[test]
    fn final_follows_a_link_and_parent_only_keeps_it() {
        let dir = root();
        std::fs::write(dir.path().join("target"), b"t").expect("write");
        std::os::unix::fs::symlink("target", dir.path().join("link")).expect("symlink");
        let canonical = dir.path().canonicalize().expect("root");

        let followed = resolve_beneath(dir.path(), Path::new("link"), Follow::Final).expect("ok");
        assert_eq!(followed, canonical.join("target"));

        let kept = resolve_beneath(dir.path(), Path::new("link"), Follow::ParentOnly).expect("ok");
        assert_eq!(kept, canonical.join("link"));
    }

    #[test]
    fn a_link_pointing_outside_is_refused_under_final_and_kept_under_parent_only() {
        let dir = root();
        let outside = root();
        std::os::unix::fs::symlink(outside.path(), dir.path().join("escape")).expect("symlink");

        let error = resolve_beneath(dir.path(), Path::new("escape"), Follow::Final)
            .expect_err("escape");
        assert_eq!(error.kind(), io::ErrorKind::PermissionDenied);

        let kept =
            resolve_beneath(dir.path(), Path::new("escape"), Follow::ParentOnly).expect("ok");
        assert_eq!(kept, dir.path().canonicalize().expect("root").join("escape"));
    }

    #[test]
    fn an_escaping_intermediate_link_is_refused_under_every_policy() {
        let dir = root();
        let outside = root();
        std::os::unix::fs::symlink(outside.path(), dir.path().join("escape")).expect("symlink");

        for follow in [Follow::Final, Follow::ParentOnly] {
            let error = resolve_beneath(dir.path(), Path::new("escape/file"), follow)
                .expect_err("escape");
            assert_eq!(error.kind(), io::ErrorKind::PermissionDenied, "{follow:?}");
        }
    }

    #[test]
    fn a_dangling_link_stays_literal_under_final() {
        let dir = root();
        std::os::unix::fs::symlink("nowhere", dir.path().join("link")).expect("symlink");
        let resolved = resolve_beneath(dir.path(), Path::new("link"), Follow::Final).expect("ok");
        assert_eq!(resolved, dir.path().canonicalize().expect("root").join("link"));
    }

    #[test]
    fn the_root_itself_resolves_under_every_policy() {
        let dir = root();
        let canonical = dir.path().canonicalize().expect("root");
        for follow in [Follow::Final, Follow::ParentOnly] {
            assert_eq!(resolve_beneath(dir.path(), Path::new(""), follow).expect("ok"), canonical);
            assert_eq!(resolve_beneath(dir.path(), Path::new("/"), follow).expect("ok"), canonical);
        }
    }

    #[test]
    fn a_missing_root_is_an_error_not_a_guess() {
        let dir = root();
        let gone = dir.path().join("gone");
        let error = resolve_beneath(&gone, Path::new("x"), Follow::Final).expect_err("missing");
        assert_eq!(error.kind(), io::ErrorKind::NotFound);
    }
}
