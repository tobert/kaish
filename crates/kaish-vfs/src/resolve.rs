//! Root-scoped path resolution with an explicit symlink policy.
//!
//! [`resolve_beneath`] is the one function a backend rooted at a host
//! directory calls before every operation. It maps a root-relative path to
//! a host path, follows symlinks according to [`Follow`], and refuses any
//! result outside the root. The containment check lives inside so that a
//! caller cannot forget it.

use std::io;
use std::path::{Component, Path, PathBuf};

/// What the caller promises about the last path component.
///
/// Every policy follows symlinks in the parent directories, as the OS does
/// for every call including `lstat(2)`. The policies differ only on the
/// final component, and the policy must match the syscall that follows:
/// the containment check is only as good as the path the kernel then acts
/// on.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Follow {
    /// The operation follows the final component: read, write, stat, list,
    /// mkdir, set_mtime, and every other call that opens or inspects the
    /// target. Containment is checked on the fully resolved path, so a
    /// link inside the root that points outside it is refused.
    Final,
    /// The operation acts on the link itself and never follows it: remove,
    /// both sides of rename, lstat, read_link, and the link side of
    /// symlink. Containment is checked on the resolved parent plus the
    /// literal name, so a link inside the root that points outside it
    /// passes.
    ///
    /// Valid only for a syscall that does not follow the final component.
    /// Paired with one that does (`open`, `stat`, `truncate`), the check
    /// validates a path the kernel will not use, and the write lands
    /// outside the root.
    LinkItself,
}

/// Resolve `path` under `root` and refuse any result outside `root`.
///
/// `path` is root-relative; a leading `/` is stripped. `.` and `..` are
/// resolved lexically first, and `..` above the root is an error before
/// anything touches the filesystem. The deepest existing ancestor is
/// canonicalized and the components that do not exist yet are appended
/// literally: a component that does not exist cannot be a symlink, so
/// containment on the existing prefix is containment on the whole path.
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
        Follow::Final if lexical.exists() => lexical.canonicalize()?,
        // open(2) with O_CREAT follows a dangling link and creates its
        // target, so containment is checked where the chain ends.
        Follow::Final => {
            let end = end_of_dangling_chain(&lexical)?;
            if end == root {
                canonical_root.clone()
            } else {
                canonicalize_deepest_ancestor(root, &end)?
            }
        }
        Follow::LinkItself => {
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

/// Follow a chain of symlinks from a path that does not exist to the path
/// the chain names. A path that is not a symlink is returned as is.
fn end_of_dangling_chain(path: &Path) -> io::Result<PathBuf> {
    let mut current = path.to_path_buf();
    for _ in 0..MAX_SYMLINK_HOPS {
        let is_link = current
            .symlink_metadata()
            .map(|meta| meta.file_type().is_symlink())
            .unwrap_or(false);
        if !is_link {
            return Ok(current);
        }
        let target = current.read_link()?;
        let next = if target.is_absolute() {
            target
        } else {
            current.parent().unwrap_or(Path::new("/")).join(target)
        };
        current = fold_dots(&next);
    }
    Err(io::Error::other(format!(
        "too many levels of symbolic links: {}",
        path.display()
    )))
}

/// Linux's ELOOP limit.
const MAX_SYMLINK_HOPS: usize = 40;

/// Resolve `.` and `..` lexically in a host path; `..` at the top stays
/// there.
fn fold_dots(path: &Path) -> PathBuf {
    let mut out = PathBuf::new();
    for component in path.components() {
        match component {
            Component::ParentDir => {
                out.pop();
            }
            Component::CurDir => {}
            other => out.push(other),
        }
    }
    out
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
    #![allow(clippy::expect_used)]
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
        let error = resolve_beneath(dir.path(), Path::new("a/../../x"), Follow::LinkItself)
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
            resolve_beneath(dir.path(), Path::new("a/b/c/d"), Follow::LinkItself).expect("ok");
        assert_eq!(
            resolved,
            dir.path().canonicalize().expect("root").join("a/b/c/d")
        );
    }

    #[test]
    fn final_follows_a_link_and_link_itself_keeps_it() {
        let dir = root();
        std::fs::write(dir.path().join("target"), b"t").expect("write");
        std::os::unix::fs::symlink("target", dir.path().join("link")).expect("symlink");
        let canonical = dir.path().canonicalize().expect("root");

        let followed = resolve_beneath(dir.path(), Path::new("link"), Follow::Final).expect("ok");
        assert_eq!(followed, canonical.join("target"));

        let kept = resolve_beneath(dir.path(), Path::new("link"), Follow::LinkItself).expect("ok");
        assert_eq!(kept, canonical.join("link"));
    }

    #[test]
    fn a_link_pointing_outside_is_refused_under_final_and_kept_under_link_itself() {
        let dir = root();
        let outside = root();
        std::os::unix::fs::symlink(outside.path(), dir.path().join("escape")).expect("symlink");

        let error = resolve_beneath(dir.path(), Path::new("escape"), Follow::Final)
            .expect_err("escape");
        assert_eq!(error.kind(), io::ErrorKind::PermissionDenied);

        let kept =
            resolve_beneath(dir.path(), Path::new("escape"), Follow::LinkItself).expect("ok");
        assert_eq!(kept, dir.path().canonicalize().expect("root").join("escape"));
    }

    #[test]
    fn an_escaping_intermediate_link_is_refused_under_every_policy() {
        let dir = root();
        let outside = root();
        std::os::unix::fs::symlink(outside.path(), dir.path().join("escape")).expect("symlink");

        for follow in [Follow::Final, Follow::LinkItself] {
            let error = resolve_beneath(dir.path(), Path::new("escape/file"), follow)
                .expect_err("escape");
            assert_eq!(error.kind(), io::ErrorKind::PermissionDenied, "{follow:?}");
        }
    }

    #[test]
    fn a_dangling_link_resolves_to_its_target_under_final() {
        // open(2) with O_CREAT creates the target, so that is the path checked.
        let dir = root();
        std::os::unix::fs::symlink("nowhere", dir.path().join("link")).expect("symlink");
        let resolved = resolve_beneath(dir.path(), Path::new("link"), Follow::Final).expect("ok");
        assert_eq!(resolved, dir.path().canonicalize().expect("root").join("nowhere"));

        let kept = resolve_beneath(dir.path(), Path::new("link"), Follow::LinkItself).expect("ok");
        assert_eq!(kept, dir.path().canonicalize().expect("root").join("link"));
    }

    #[test]
    fn a_dangling_link_pointing_outside_is_refused_under_final() {
        let dir = root();
        std::os::unix::fs::symlink("../escape", dir.path().join("link")).expect("symlink");
        let error = resolve_beneath(dir.path(), Path::new("link"), Follow::Final)
            .expect_err("escape");
        assert_eq!(error.kind(), io::ErrorKind::PermissionDenied);

        // Two hops: a dangling link to a dangling link that escapes.
        std::os::unix::fs::symlink("link", dir.path().join("hop")).expect("symlink");
        let error = resolve_beneath(dir.path(), Path::new("hop"), Follow::Final)
            .expect_err("escape");
        assert_eq!(error.kind(), io::ErrorKind::PermissionDenied);
    }

    #[test]
    fn a_dangling_link_loop_is_an_error_not_a_hang() {
        let dir = root();
        std::os::unix::fs::symlink("b", dir.path().join("a")).expect("symlink");
        std::os::unix::fs::symlink("a", dir.path().join("b")).expect("symlink");
        let error = resolve_beneath(dir.path(), Path::new("a"), Follow::Final)
            .expect_err("loop");
        assert!(error.to_string().contains("symbolic links"), "{error}");
    }

    #[test]
    fn the_root_itself_resolves_under_every_policy() {
        let dir = root();
        let canonical = dir.path().canonicalize().expect("root");
        for follow in [Follow::Final, Follow::LinkItself] {
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
