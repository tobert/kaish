//! VFS router for mount point management.
//!
//! Routes filesystem operations to the appropriate backend based on path.

use super::{DirEntry, Filesystem};
use kaish_vfs::PathAccess;
use async_trait::async_trait;
use std::collections::BTreeMap;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;

// `MountInfo` now lives in kaish-types::backend (pure data, part of the
// KernelBackend contract). Re-exported here so existing `vfs::MountInfo`
// paths keep working.
pub use kaish_types::backend::MountInfo;

/// Mode reported for a directory the router synthesizes rather than reads
/// from a mount: the root, and any ancestor of a mount that has no mount of
/// its own. Readable and searchable, never writable — these directories are
/// derived from the mount table and the router creates nothing in them.
const SYNTHESIZED_DIRECTORY_MODE: u32 = 0o555;

/// Routes filesystem operations to mounted backends.
///
/// Mount points are matched by longest prefix. For example, if `/mnt` and
/// `/mnt/project` are both mounted, a path like `/mnt/project/src/main.rs`
/// will be routed to the `/mnt/project` mount.
#[derive(Default)]
pub struct VfsRouter {
    /// Mount points, keyed by path. Uses BTreeMap for ordered iteration.
    mounts: BTreeMap<PathBuf, Arc<dyn Filesystem>>,
}

impl std::fmt::Debug for VfsRouter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VfsRouter")
            .field("mounts", &self.mounts.keys().collect::<Vec<_>>())
            .finish()
    }
}

impl VfsRouter {
    /// Create a new empty VFS router.
    pub fn new() -> Self {
        Self {
            mounts: BTreeMap::new(),
        }
    }

    /// Mount a filesystem at the given path.
    ///
    /// The path should be absolute (start with `/`). If a filesystem is
    /// already mounted at this path, it will be replaced.
    pub fn mount(&mut self, path: impl Into<PathBuf>, fs: impl Filesystem + 'static) {
        let path = Self::normalize_mount_path(path.into());
        self.mounts.insert(path, Arc::new(fs));
    }

    /// Mount a filesystem (already wrapped in Arc) at the given path.
    pub fn mount_arc(&mut self, path: impl Into<PathBuf>, fs: Arc<dyn Filesystem>) {
        let path = Self::normalize_mount_path(path.into());
        self.mounts.insert(path, fs);
    }

    /// Unmount the filesystem at the given path.
    ///
    /// Returns `true` if a mount was removed, `false` if nothing was mounted there.
    pub fn unmount(&mut self, path: impl AsRef<Path>) -> bool {
        let path = Self::normalize_mount_path(path.as_ref().to_path_buf());
        self.mounts.remove(&path).is_some()
    }

    /// List all current mounts.
    pub fn list_mounts(&self) -> Vec<MountInfo> {
        self.mounts
            .iter()
            .map(|(path, fs)| MountInfo {
                path: path.clone(),
                read_only: fs.read_only(),
                resident_bytes: fs.resident_bytes(),
            })
            .collect()
    }

    /// Normalize a mount path: ensure it starts with `/` and has no trailing slash.
    fn normalize_mount_path(path: PathBuf) -> PathBuf {
        let s = path.to_string_lossy();
        let s = s.trim_end_matches('/');
        if s.is_empty() {
            PathBuf::from("/")
        } else if !s.starts_with('/') {
            PathBuf::from(format!("/{}", s))
        } else {
            PathBuf::from(s)
        }
    }

    /// Resolve a VFS path to a real filesystem path.
    ///
    /// Returns `Some(path)` if the VFS path maps to a real filesystem (like LocalFs),
    /// or `None` if the path is in a virtual filesystem (like MemoryFs).
    ///
    /// This is needed for tools like `git` that must use real paths with external libraries.
    pub fn resolve_real_path(&self, path: &Path) -> Option<PathBuf> {
        let (fs, relative) = self.find_mount(path).ok()?;
        fs.real_path(&relative)
    }

    /// Returns true if some registered mount covers this path.
    ///
    /// Used by embedder overlay backends (`VirtualOverlayBackend`) to decide
    /// whether a path belongs to this router's mounts or should be delegated
    /// to the embedder's own backend — without hardcoding a mount prefix.
    pub(crate) fn has_mount(&self, path: &Path) -> bool {
        self.find_mount(path).is_ok()
    }

    /// Returns true if some mount lives strictly *below* `dir` — i.e. `dir` is a
    /// proper ancestor of a mount point (`has_mount_under("/v")` is true when
    /// `/v/jobs` is mounted, even though nothing is mounted at `/v` itself).
    ///
    /// Distinct from `has_mount`, which is true only when `dir` is *covered* by
    /// a mount. Together they let an overlay treat an intermediate path like
    /// `/v` as an existing directory (the union of its child mounts) while still
    /// delegating unclaimed leaves to the embedder's backend.
    pub(crate) fn has_mount_under(&self, dir: &Path) -> bool {
        let dir = Self::normalize_mount_path(dir.to_path_buf());
        let dir_str = dir.to_string_lossy();
        self.mounts.keys().any(|mount_path| {
            let mount_str = mount_path.to_string_lossy();
            if dir_str == "/" {
                mount_str != "/"
            } else {
                mount_str.starts_with(&format!("{}/", dir_str))
            }
        })
    }

    /// Synthesize the child directory entries of `dir` from the mount roster:
    /// the first path component below `dir` of every mount that lives under it
    /// (`/v` over mounts `/v/jobs`, `/v/blobs` → `blobs`, `jobs`). `dir` is
    /// expected to be a non-root ancestor with no mount of its own; root is
    /// handled by `list_root`, which also folds in a `/` mount's real contents.
    /// Recover a mount point's ancestor from a `NotFound`.
    ///
    /// A mount at `/a/b/c` implies `/a` and `/a/b` are directories, the way a
    /// real mount implies its mount point's parents. The router synthesizes
    /// them because no backend owns them.
    ///
    /// Mounting `/` makes `mount_of` match every path, so the backend covering
    /// `/` is asked for `/a`, answers `NotFound`, and that reaches the caller
    /// before the ancestor check below the `Err` arm can run. That check is
    /// therefore unreachable whenever a root mount exists, which is the
    /// ordinary embedder shape. This runs on the answer instead of on the
    /// routing.
    ///
    /// Only `NotFound` is recovered. Any other error is the backend's answer
    /// about a path it owns and must reach the caller unchanged.
    fn or_synthesized_ancestor<T>(
        &self,
        path: &Path,
        error: io::Error,
        synthesize: impl FnOnce() -> T,
    ) -> io::Result<T> {
        if error.kind() == io::ErrorKind::NotFound && self.has_mount_under(path) {
            Ok(synthesize())
        } else {
            Err(error)
        }
    }

    fn list_mount_children(&self, dir: &Path) -> Vec<DirEntry> {
        let dir = Self::normalize_mount_path(dir.to_path_buf());
        let prefix = format!("{}/", dir.to_string_lossy());
        let mut seen = std::collections::HashSet::new();
        let mut entries = Vec::new();
        for mount_path in self.mounts.keys() {
            let mount_str = mount_path.to_string_lossy();
            if let Some(rest) = mount_str.strip_prefix(&prefix) {
                let first = rest.split('/').next().unwrap_or("");
                if !first.is_empty() && seen.insert(first.to_string()) {
                    entries.push(DirEntry::directory(first));
                }
            }
        }
        entries.sort_by(|a, b| a.name.cmp(&b.name));
        entries
    }

    /// The final path component, for naming a synthesized directory entry
    /// (`/v` → `v`). Falls back to `/` for a path with no component.
    fn path_basename(path: &Path) -> String {
        path.file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| "/".to_string())
    }

    /// Find the mount point for a given path.
    ///
    /// Returns the mount and the path relative to that mount.
    fn find_mount(&self, path: &Path) -> io::Result<(Arc<dyn Filesystem>, PathBuf)> {
        let (_, fs, relative) = self.mount_of(path)?;
        Ok((fs, relative))
    }

    /// The mount that owns `path`: its mount point, its filesystem, and the
    /// path relative to the mount point.
    fn mount_of(&self, path: &Path) -> io::Result<(&Path, Arc<dyn Filesystem>, PathBuf)> {
        let path_str = path.to_string_lossy();
        let normalized = if path_str.starts_with('/') {
            path.to_path_buf()
        } else {
            PathBuf::from(format!("/{}", path_str))
        };

        // Find longest matching mount point
        let mut best_match: Option<(&PathBuf, &Arc<dyn Filesystem>)> = None;

        for (mount_path, fs) in &self.mounts {
            let mount_str = mount_path.to_string_lossy();

            // Check if the path starts with this mount point
            let is_match = if mount_str == "/" {
                true // Root matches everything
            } else {
                let normalized_str = normalized.to_string_lossy();
                normalized_str == mount_str.as_ref()
                    || normalized_str.starts_with(&format!("{}/", mount_str))
            };

            if is_match {
                // Keep the longest match
                let dominated = best_match
                    .as_ref()
                    .is_none_or(|(bp, _)| mount_path.as_os_str().len() > bp.as_os_str().len());
                if dominated {
                    best_match = Some((mount_path, fs));
                }
            }
        }

        match best_match {
            Some((mount_path, fs)) => {
                // Calculate relative path
                let mount_str = mount_path.to_string_lossy();
                let normalized_str = normalized.to_string_lossy();

                let relative = if mount_str == "/" {
                    normalized_str.trim_start_matches('/').to_string()
                } else {
                    normalized_str
                        .strip_prefix(mount_str.as_ref())
                        .unwrap_or("")
                        .trim_start_matches('/')
                        .to_string()
                };

                Ok((mount_path.as_path(), Arc::clone(fs), PathBuf::from(relative)))
            }
            None => Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("no mount point for path: {}", path.display()),
            )),
        }
    }
}

/// Resolve `.` and `..` lexically in an absolute VFS path; `..` at the root
/// stays at the root.
fn lexical_absolute(path: &Path) -> PathBuf {
    let mut out = PathBuf::from("/");
    for component in path.components() {
        match component {
            std::path::Component::Normal(name) => out.push(name),
            std::path::Component::ParentDir => {
                out.pop();
            }
            _ => {}
        }
    }
    out
}

/// The relative path from directory `from` to `to`, both absolute and
/// lexically normalized: `..` for each component of `from` past the common
/// prefix, then the rest of `to`.
fn relative_path_from(from: &Path, to: &Path) -> PathBuf {
    let mut from_parts = from.components().skip(1).peekable();
    let mut to_parts = to.components().skip(1).peekable();
    while let (Some(a), Some(b)) = (from_parts.peek(), to_parts.peek()) {
        if a != b {
            break;
        }
        from_parts.next();
        to_parts.next();
    }
    let mut relative = PathBuf::new();
    for _ in from_parts {
        relative.push("..");
    }
    for part in to_parts {
        relative.push(part);
    }
    if relative.as_os_str().is_empty() {
        relative.push(".");
    }
    relative
}

#[async_trait]
impl Filesystem for VfsRouter {
    #[tracing::instrument(level = "trace", skip(self), fields(path = %path.display()))]
    async fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        let (fs, relative) = self.find_mount(path)?;
        fs.read(&relative).await
    }

    #[tracing::instrument(level = "trace", skip(self), fields(path = %path.display()))]
    async fn read_range(
        &self,
        path: &Path,
        range: Option<kaish_vfs::ReadRange>,
    ) -> io::Result<Vec<u8>> {
        // Forward the range to the mount so range-aware backends (e.g. DevFs's
        // /dev/zero) see the requested byte count. Falling through to the trait
        // default would call our own `read` (whole file) and slice afterwards,
        // which would hang or error on an infinite device.
        let (fs, relative) = self.find_mount(path)?;
        fs.read_range(&relative, range).await
    }

    #[tracing::instrument(level = "trace", skip(self, data), fields(path = %path.display(), size = data.len()))]
    async fn write(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        let (fs, relative) = self.find_mount(path)?;
        fs.write(&relative, data).await
    }

    #[tracing::instrument(level = "trace", skip(self, data), fields(path = %path.display(), size = data.len()))]
    async fn append(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        // Forward to the mount so a real append (LocalFs's O_APPEND, say)
        // reaches it. Falling through to the trait default would call our
        // own `read` and `write`, which route to the mount's read and write
        // individually — never its `append` override, and losing the
        // atomicity that override exists to provide.
        let (fs, relative) = self.find_mount(path)?;
        fs.append(&relative, data).await
    }

    #[tracing::instrument(level = "trace", skip(self), fields(path = %path.display()))]
    async fn list(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        // Special case: listing root might need to show mount points
        let path_str = path.to_string_lossy();
        if path_str.is_empty() || path_str == "/" {
            return self.list_root().await;
        }

        let answer = match self.find_mount(path) {
            Ok((fs, relative)) => fs.list(&relative).await,
            Err(e) => Err(e),
        };
        match answer {
            Ok(entries) => Ok(entries),
            // An ancestor of a mount lists the mounts beneath it rather
            // than 404ing.
            Err(e) => self.or_synthesized_ancestor(path, e, || self.list_mount_children(path)),
        }
    }

    #[tracing::instrument(level = "trace", skip(self), fields(path = %path.display()))]
    async fn stat(&self, path: &Path) -> io::Result<DirEntry> {
        // Special case: root always exists
        let path_str = path.to_string_lossy();
        if path_str.is_empty() || path_str == "/" {
            return Ok(DirEntry::directory("/"));
        }

        // Check if path is a mount point itself
        let normalized = Self::normalize_mount_path(path.to_path_buf());
        if self.mounts.contains_key(&normalized) {
            let name = path
                .file_name()
                .map(|n| n.to_string_lossy().into_owned())
                .unwrap_or_else(|| "/".to_string());
            return Ok(DirEntry::directory(name));
        }

        let answer = match self.find_mount(path) {
            Ok((fs, relative)) => fs.stat(&relative).await,
            Err(e) => Err(e),
        };
        match answer {
            Ok(entry) => Ok(entry),
            // An ancestor of a mount (`/v` above `/v/jobs`) is a synthesized
            // directory, whether the routing missed or the backend did.
            Err(e) => {
                self.or_synthesized_ancestor(path, e, || {
                    DirEntry::directory(Self::path_basename(path))
                })
            }
        }
    }

    async fn read_link(&self, path: &Path) -> io::Result<PathBuf> {
        let (fs, relative) = self.find_mount(path)?;
        fs.read_link(&relative).await
    }

    /// Delegates to the mount that owns `path`, translating VFS-absolute to
    /// mount-relative going in and back going out — the mount answers in
    /// its own namespace, same as every other `Filesystem` method here.
    ///
    /// `.` and `..` are folded lexically before routing, so a `..` that
    /// walks from one mount into another (or into a synthesized ancestor)
    /// resolves against the right one, the way `symlink`'s absolute-target
    /// rewrite already folds before it picks a mount.
    ///
    /// Falls back the way `stat` does: a synthesized ancestor of a mount
    /// (`/v` above `/v/jobs`) is a directory the router creates, never a
    /// symlink, so it canonicalizes to itself.
    async fn canonicalize(&self, path: &Path, allow_missing_final: bool) -> io::Result<PathBuf> {
        let normalized = lexical_absolute(path);
        if normalized == Path::new("/") {
            return Ok(PathBuf::from("/"));
        }

        let answer = match self.mount_of(&normalized) {
            Ok((mount_path, fs, relative)) => {
                let mount_path = mount_path.to_path_buf();
                fs.canonicalize(&relative, allow_missing_final)
                    .await
                    .map(|resolved| mount_path.join(resolved))
            }
            Err(e) => Err(e),
        };
        match answer {
            Ok(resolved) => Ok(resolved),
            Err(e) => self.or_synthesized_ancestor(&normalized, e, || normalized.clone()),
        }
    }

    async fn symlink(&self, target: &Path, link: &Path) -> io::Result<()> {
        let (link_mount, fs, relative_link) = self.mount_of(link)?;
        // A backend refuses an absolute target: it has no namespace to read
        // one in. The router has the namespace, so an absolute VFS target on
        // the link's own mount is rewritten relative to the link's directory.
        // The stored spelling is what readlink then shows.
        let target = if target.is_absolute() {
            let target = lexical_absolute(target);
            let (target_mount, _, _) = self.mount_of(&target)?;
            if target_mount != link_mount {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!(
                        "symlink target {} is on mount {} and the link {} is on mount {}; a link cannot cross mounts",
                        target.display(),
                        target_mount.display(),
                        link.display(),
                        link_mount.display()
                    ),
                ));
            }
            let link_dir = lexical_absolute(link);
            let link_dir = link_dir.parent().unwrap_or(Path::new("/"));
            relative_path_from(link_dir, &target)
        } else {
            target.to_path_buf()
        };
        fs.symlink(&target, &relative_link).await
    }

    async fn lstat(&self, path: &Path) -> io::Result<DirEntry> {
        // Special case: root always exists
        let path_str = path.to_string_lossy();
        if path_str.is_empty() || path_str == "/" {
            return Ok(DirEntry::directory("/"));
        }

        // Check if path is a mount point itself
        let normalized = Self::normalize_mount_path(path.to_path_buf());
        if self.mounts.contains_key(&normalized) {
            let name = path
                .file_name()
                .map(|n| n.to_string_lossy().into_owned())
                .unwrap_or_else(|| "/".to_string());
            return Ok(DirEntry::directory(name));
        }

        let answer = match self.find_mount(path) {
            Ok((fs, relative)) => fs.lstat(&relative).await,
            Err(e) => Err(e),
        };
        match answer {
            Ok(entry) => Ok(entry),
            // A synthesized ancestor is a directory, never a symlink, so
            // lstat and stat agree about it.
            Err(e) => {
                self.or_synthesized_ancestor(path, e, || {
                    DirEntry::directory(Self::path_basename(path))
                })
            }
        }
    }

    async fn mkdir(&self, path: &Path) -> io::Result<()> {
        let (fs, relative) = self.find_mount(path)?;
        fs.mkdir(&relative).await
    }

    async fn set_mtime(&self, path: &Path, mtime: std::time::SystemTime) -> io::Result<()> {
        let (fs, relative) = self.find_mount(path)?;
        fs.set_mtime(&relative, mtime).await
    }

    async fn remove(&self, path: &Path) -> io::Result<()> {
        let (fs, relative) = self.find_mount(path)?;
        fs.remove(&relative).await
    }

    async fn rename(&self, from: &Path, to: &Path) -> io::Result<()> {
        let (from_fs, from_relative) = self.find_mount(from)?;
        let (to_fs, to_relative) = self.find_mount(to)?;

        // Check if both paths are on the same mount by comparing Arc pointers
        if !Arc::ptr_eq(&from_fs, &to_fs) {
            return Err(io::Error::new(
                io::ErrorKind::Unsupported,
                "cannot rename across different mount points",
            ));
        }

        from_fs.rename(&from_relative, &to_relative).await
    }

    /// Delegates to the mount that owns the path, so the answer is that
    /// mount's — not the whole router's. `read_only()` below is the
    /// whole-router question and cannot answer for one path: a router with a
    /// writable `/` and a read-only `/v/bin` is read-only for neither.
    ///
    /// Falls back the way `stat` does. `stat` synthesizes a directory for the
    /// root and for any ancestor of a mount (`/v` above `/v/jobs`), so those
    /// paths exist, and an answer here that errored on them would contradict
    /// it: `[[ -e /v ]]` true and `[[ -r /v ]]` false about the same path.
    /// A synthesized directory is readable and searchable, and never
    /// writable — the router creates nothing in one.
    async fn path_access(&self, path: &Path) -> io::Result<PathAccess> {
        let path_str = path.to_string_lossy();
        if path_str.is_empty() || path_str == "/" {
            return Ok(PathAccess::resolve(Some(SYNTHESIZED_DIRECTORY_MODE), true));
        }
        let answer = match self.find_mount(path) {
            Ok((fs, relative)) => fs.path_access(&relative).await,
            Err(e) => Err(e),
        };
        match answer {
            Ok(access) => Ok(access),
            Err(e) => self.or_synthesized_ancestor(path, e, || {
                PathAccess::resolve(Some(SYNTHESIZED_DIRECTORY_MODE), true)
            }),
        }
    }

    fn read_only(&self) -> bool {
        // Router is read-only iff every mount is. Empty router returns
        // false — a router with no mounts isn't meaningfully read-only,
        // and false preserves the behaviour callers saw before this change.
        if self.mounts.is_empty() {
            return false;
        }
        self.mounts.values().all(|fs| fs.read_only())
    }
}

impl VfsRouter {
    /// List the root directory, synthesizing entries from mount points.
    async fn list_root(&self) -> io::Result<Vec<DirEntry>> {
        let mut entries = Vec::new();
        let mut seen_names = std::collections::HashSet::new();

        for mount_path in self.mounts.keys() {
            let mount_str = mount_path.to_string_lossy();
            if mount_str == "/" {
                // Root mount: list its contents directly
                if let Some(fs) = self.mounts.get(mount_path)
                    && let Ok(root_entries) = fs.list(Path::new("")).await {
                        for entry in root_entries {
                            if seen_names.insert(entry.name.clone()) {
                                entries.push(entry);
                            }
                        }
                    }
            } else {
                // Non-root mount: extract first path component
                let first_component = mount_str
                    .trim_start_matches('/')
                    .split('/')
                    .next()
                    .unwrap_or("");

                if !first_component.is_empty() && seen_names.insert(first_component.to_string()) {
                    entries.push(DirEntry::directory(first_component));
                }
            }
        }

        entries.sort_by(|a, b| a.name.cmp(&b.name));
        Ok(entries)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::MemoryFs;

    #[tokio::test]
    async fn symlink_absolute_target_on_the_same_mount_is_stored_relative() {
        let mut router = VfsRouter::new();
        let data = MemoryFs::new();
        data.write(Path::new("etc/hosts"), b"hosts").await.unwrap();
        data.mkdir(Path::new("home")).await.unwrap();
        router.mount("/data", data);

        router
            .symlink(Path::new("/data/etc/hosts"), Path::new("/data/home/link"))
            .await
            .unwrap();

        assert_eq!(
            router.read_link(Path::new("/data/home/link")).await.unwrap(),
            PathBuf::from("../etc/hosts")
        );
        assert_eq!(router.read(Path::new("/data/home/link")).await.unwrap(), b"hosts");
    }

    #[tokio::test]
    async fn symlink_absolute_target_beside_the_link_is_a_bare_name() {
        let mut router = VfsRouter::new();
        let root = MemoryFs::new();
        root.write(Path::new("a/target"), b"t").await.unwrap();
        router.mount("/", root);

        router
            .symlink(Path::new("/a/target"), Path::new("/a/link"))
            .await
            .unwrap();
        assert_eq!(
            router.read_link(Path::new("/a/link")).await.unwrap(),
            PathBuf::from("target")
        );
    }

    #[tokio::test]
    async fn symlink_absolute_target_with_dotdot_is_normalized_first() {
        let mut router = VfsRouter::new();
        let root = MemoryFs::new();
        root.write(Path::new("etc/hosts"), b"hosts").await.unwrap();
        root.mkdir(Path::new("home")).await.unwrap();
        router.mount("/", root);

        router
            .symlink(Path::new("/home/../etc/./hosts"), Path::new("/home/link"))
            .await
            .unwrap();
        assert_eq!(
            router.read_link(Path::new("/home/link")).await.unwrap(),
            PathBuf::from("../etc/hosts")
        );
        assert_eq!(router.read(Path::new("/home/link")).await.unwrap(), b"hosts");
    }

    #[tokio::test]
    async fn symlink_relative_target_is_stored_verbatim() {
        let mut router = VfsRouter::new();
        router.mount("/data", MemoryFs::new());

        router
            .symlink(Path::new("../x/../y"), Path::new("/data/d/link"))
            .await
            .unwrap();
        assert_eq!(
            router.read_link(Path::new("/data/d/link")).await.unwrap(),
            PathBuf::from("../x/../y")
        );
    }

    #[tokio::test]
    async fn symlink_across_mounts_is_refused_and_creates_nothing() {
        let mut router = VfsRouter::new();
        router.mount("/data", MemoryFs::new());
        let scratch = MemoryFs::new();
        scratch.write(Path::new("x"), b"x").await.unwrap();
        router.mount("/scratch", scratch);

        let error = router
            .symlink(Path::new("/scratch/x"), Path::new("/data/link"))
            .await
            .unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::InvalidInput);
        let message = error.to_string();
        assert!(message.contains("/scratch") && message.contains("/data"), "{message}");
        assert!(router.lstat(Path::new("/data/link")).await.is_err(), "nothing created");
    }

    #[test]
    fn relative_path_from_walks_up_then_down() {
        let rel = |from: &str, to: &str| relative_path_from(Path::new(from), Path::new(to));
        assert_eq!(rel("/a/b", "/a/c/d"), PathBuf::from("../c/d"));
        assert_eq!(rel("/a", "/a/x"), PathBuf::from("x"));
        assert_eq!(rel("/", "/x/y"), PathBuf::from("x/y"));
        assert_eq!(rel("/a/b/c", "/"), PathBuf::from("../../.."));
        assert_eq!(rel("/a/b", "/a/b"), PathBuf::from("."));
    }

    #[tokio::test]
    async fn test_basic_mount() {
        let mut router = VfsRouter::new();
        let scratch = MemoryFs::new();
        scratch.write(Path::new("test.txt"), b"hello").await.unwrap();
        router.mount("/scratch", scratch);

        let data = router.read(Path::new("/scratch/test.txt")).await.unwrap();
        assert_eq!(data, b"hello");
    }

    #[tokio::test]
    async fn test_multiple_mounts() {
        let mut router = VfsRouter::new();

        let scratch = MemoryFs::new();
        scratch.write(Path::new("a.txt"), b"scratch").await.unwrap();
        router.mount("/scratch", scratch);

        let data = MemoryFs::new();
        data.write(Path::new("b.txt"), b"data").await.unwrap();
        router.mount("/data", data);

        assert_eq!(
            router.read(Path::new("/scratch/a.txt")).await.unwrap(),
            b"scratch"
        );
        assert_eq!(
            router.read(Path::new("/data/b.txt")).await.unwrap(),
            b"data"
        );
    }

    #[tokio::test]
    async fn test_nested_mount() {
        let mut router = VfsRouter::new();

        let outer = MemoryFs::new();
        outer.write(Path::new("outer.txt"), b"outer").await.unwrap();
        router.mount("/mnt", outer);

        let inner = MemoryFs::new();
        inner.write(Path::new("inner.txt"), b"inner").await.unwrap();
        router.mount("/mnt/project", inner);

        // /mnt/outer.txt should come from outer mount
        assert_eq!(
            router.read(Path::new("/mnt/outer.txt")).await.unwrap(),
            b"outer"
        );

        // /mnt/project/inner.txt should come from inner mount
        assert_eq!(
            router.read(Path::new("/mnt/project/inner.txt")).await.unwrap(),
            b"inner"
        );
    }

    #[tokio::test]
    async fn test_list_root() {
        let mut router = VfsRouter::new();
        router.mount("/scratch", MemoryFs::new());
        router.mount("/mnt/a", MemoryFs::new());
        router.mount("/mnt/b", MemoryFs::new());

        let entries = router.list(Path::new("/")).await.unwrap();
        let names: Vec<_> = entries.iter().map(|e| &e.name).collect();

        assert!(names.contains(&&"scratch".to_string()));
        assert!(names.contains(&&"mnt".to_string()));
    }

    #[tokio::test]
    async fn test_unmount() {
        let mut router = VfsRouter::new();

        let fs = MemoryFs::new();
        fs.write(Path::new("test.txt"), b"data").await.unwrap();
        router.mount("/scratch", fs);

        assert!(router.read(Path::new("/scratch/test.txt")).await.is_ok());

        router.unmount("/scratch");

        assert!(router.read(Path::new("/scratch/test.txt")).await.is_err());
    }

    #[tokio::test]
    async fn test_list_mounts() {
        let mut router = VfsRouter::new();
        router.mount("/scratch", MemoryFs::new());
        router.mount("/data", MemoryFs::new());

        let mounts = router.list_mounts();
        assert_eq!(mounts.len(), 2);

        let paths: Vec<_> = mounts.iter().map(|m| &m.path).collect();
        assert!(paths.contains(&&PathBuf::from("/scratch")));
        assert!(paths.contains(&&PathBuf::from("/data")));
    }

    #[tokio::test]
    async fn test_no_mount_error() {
        let router = VfsRouter::new();
        let result = router.read(Path::new("/nothing/here.txt")).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::NotFound);
    }

    #[tokio::test]
    async fn test_root_mount() {
        let mut router = VfsRouter::new();

        let root = MemoryFs::new();
        root.write(Path::new("at-root.txt"), b"root file").await.unwrap();
        router.mount("/", root);

        let data = router.read(Path::new("/at-root.txt")).await.unwrap();
        assert_eq!(data, b"root file");
    }

    #[tokio::test]
    async fn test_write_through_router() {
        let mut router = VfsRouter::new();
        router.mount("/scratch", MemoryFs::new());

        router
            .write(Path::new("/scratch/new.txt"), b"created")
            .await
            .unwrap();

        let data = router.read(Path::new("/scratch/new.txt")).await.unwrap();
        assert_eq!(data, b"created");
    }

    #[tokio::test]
    async fn test_stat_mount_point() {
        let mut router = VfsRouter::new();
        router.mount("/scratch", MemoryFs::new());

        let entry = router.stat(Path::new("/scratch")).await.unwrap();
        assert!(entry.is_dir());
    }

    #[tokio::test]
    async fn test_stat_root() {
        let router = VfsRouter::new();
        let entry = router.stat(Path::new("/")).await.unwrap();
        assert!(entry.is_dir());
    }

    #[tokio::test]
    async fn test_rename_same_mount() {
        let mut router = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("old.txt"), b"data").await.unwrap();
        router.mount("/scratch", mem);

        router.rename(Path::new("/scratch/old.txt"), Path::new("/scratch/new.txt")).await.unwrap();

        // New path exists
        let data = router.read(Path::new("/scratch/new.txt")).await.unwrap();
        assert_eq!(data, b"data");

        // Old path doesn't exist
        assert!(!router.exists(Path::new("/scratch/old.txt")).await);
    }

    #[tokio::test]
    async fn test_rename_cross_mount_fails() {
        let mut router = VfsRouter::new();
        let mem1 = MemoryFs::new();
        mem1.write(Path::new("file.txt"), b"data").await.unwrap();
        router.mount("/mount1", mem1);
        router.mount("/mount2", MemoryFs::new());

        let result = router.rename(Path::new("/mount1/file.txt"), Path::new("/mount2/file.txt")).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::Unsupported);
    }

    // `stat` synthesizes a directory for the root and for any ancestor of a
    // mount, so those paths exist. `path_access` has to agree with `stat`
    // about the same path — going straight to `find_mount` errors where
    // `stat` succeeds, and `[[ -e /v ]]` would be true while `[[ -r /v ]]`
    // was false about the identical path.
    #[tokio::test]
    async fn path_access_agrees_with_stat_on_synthesized_directories() {
        let mut router = VfsRouter::new();
        router.mount("/v/docs", MemoryFs::new());

        for path in ["/", "/v"] {
            let path = Path::new(path);
            assert!(
                router.stat(path).await.is_ok(),
                "{} is synthesized by stat",
                path.display()
            );
            let access = router
                .path_access(path)
                .await
                .unwrap_or_else(|e| panic!("path_access must not error where stat succeeds: {e}"));
            assert!(access.readable, "{} must be readable", path.display());
            assert!(access.executable, "{} must be searchable", path.display());
            assert!(
                !access.writable,
                "the router creates nothing in {}",
                path.display()
            );
        }
    }

    // The synthesis must not swallow a genuinely absent path.
    #[tokio::test]
    async fn path_access_errors_on_a_path_with_no_mount() {
        let mut router = VfsRouter::new();
        router.mount("/v/docs", MemoryFs::new());
        assert!(router.path_access(Path::new("/nope")).await.is_err());
        assert!(router.path_access(Path::new("/v/docs/absent")).await.is_err());
    }

    // A real mount answers for itself, not with the synthesized defaults.
    #[tokio::test]
    async fn path_access_at_a_mount_point_asks_the_mount() {
        let mut router = VfsRouter::new();
        router.mount("/rw", MemoryFs::new());
        router.mount("/ro", BuiltinFsStub);

        assert!(router.path_access(Path::new("/rw")).await.unwrap().writable);
        assert!(!router.path_access(Path::new("/ro")).await.unwrap().writable);
        assert!(router.path_access(Path::new("/ro")).await.unwrap().readable);
    }

    /// A minimal read-only mount that reports no mode, standing in for
    /// `BuiltinFs`/`JobFs` without dragging a ToolRegistry into this module.
    struct BuiltinFsStub;

    #[async_trait]
    impl Filesystem for BuiltinFsStub {
        async fn read(&self, _path: &Path) -> io::Result<Vec<u8>> {
            Ok(Vec::new())
        }
        async fn write(&self, _path: &Path, _data: &[u8]) -> io::Result<()> {
            Err(io::Error::new(io::ErrorKind::PermissionDenied, "read-only"))
        }
        async fn list(&self, _path: &Path) -> io::Result<Vec<DirEntry>> {
            Ok(Vec::new())
        }
        async fn stat(&self, _path: &Path) -> io::Result<DirEntry> {
            Ok(DirEntry::directory("."))
        }
        async fn mkdir(&self, _path: &Path) -> io::Result<()> {
            Err(io::Error::new(io::ErrorKind::PermissionDenied, "read-only"))
        }
        async fn remove(&self, _path: &Path) -> io::Result<()> {
            Err(io::Error::new(io::ErrorKind::PermissionDenied, "read-only"))
        }
        fn read_only(&self) -> bool {
            true
        }
    }

    #[tokio::test]
    async fn read_only_empty_router_returns_false() {
        let router = VfsRouter::new();
        assert!(!router.read_only());
    }

    #[cfg(feature = "localfs")]
    #[tokio::test]
    async fn read_only_all_read_only_mounts_returns_true() {
        use crate::vfs::LocalFs;

        let t1 = tempfile::tempdir().unwrap();
        let t2 = tempfile::tempdir().unwrap();

        let mut router = VfsRouter::new();
        router.mount("/a", LocalFs::read_only(t1.path().to_path_buf()));
        router.mount("/b", LocalFs::read_only(t2.path().to_path_buf()));

        assert!(router.read_only());
    }

    #[cfg(feature = "localfs")]
    #[tokio::test]
    async fn read_only_mixed_mounts_returns_false() {
        use crate::vfs::LocalFs;

        let t1 = tempfile::tempdir().unwrap();

        let mut router = VfsRouter::new();
        router.mount("/ro", LocalFs::read_only(t1.path().to_path_buf()));
        router.mount("/rw", MemoryFs::new());

        assert!(!router.read_only());
    }

    // An intermediate directory that has no mount of its own but sits *above*
    // one or more mounts (e.g. `/v` over `/v/jobs`, `/v/blobs`) must present as
    // a real, listable directory synthesized from the mount roster — not the
    // `NotFound` the bare `find_mount` returns. This is what lets a kaish shell
    // (and SFTP over the bare router) navigate `/v` when the mounts sit at
    // `/v/*`, and it's the router half of the `/v` overlay-tuning fix.
    #[tokio::test]
    async fn test_list_synthesizes_intermediate_dir() {
        let mut router = VfsRouter::new();
        router.mount("/v/jobs", MemoryFs::new());
        router.mount("/v/blobs", MemoryFs::new());

        let entries = router.list(Path::new("/v")).await.unwrap();
        let names: Vec<_> = entries.iter().map(|e| e.name.as_str()).collect();
        assert_eq!(names, vec!["blobs", "jobs"]); // sorted, synthesized from mounts
    }

    #[tokio::test]
    async fn test_stat_intermediate_dir_is_directory() {
        let mut router = VfsRouter::new();
        router.mount("/v/jobs", MemoryFs::new());

        assert!(router.stat(Path::new("/v")).await.unwrap().is_dir());
        assert!(router.lstat(Path::new("/v")).await.unwrap().is_dir());
    }

    #[tokio::test]
    async fn test_deep_intermediate_dir() {
        let mut router = VfsRouter::new();
        router.mount("/v/etc/rc", MemoryFs::new());

        let v: Vec<_> = router.list(Path::new("/v")).await.unwrap();
        assert_eq!(v.iter().map(|e| e.name.as_str()).collect::<Vec<_>>(), vec!["etc"]);
        let etc: Vec<_> = router.list(Path::new("/v/etc")).await.unwrap();
        assert_eq!(etc.iter().map(|e| e.name.as_str()).collect::<Vec<_>>(), vec!["rc"]);
        assert!(router.stat(Path::new("/v/etc")).await.unwrap().is_dir());
    }

    #[tokio::test]
    async fn test_has_mount_under() {
        let mut router = VfsRouter::new();
        router.mount("/v/jobs", MemoryFs::new());

        assert!(router.has_mount_under(Path::new("/v")));
        assert!(router.has_mount_under(Path::new("/")));
        // The mount point itself has nothing *below* it.
        assert!(!router.has_mount_under(Path::new("/v/jobs")));
        assert!(!router.has_mount_under(Path::new("/other")));
    }

    #[tokio::test]
    async fn test_nonexistent_ancestor_still_notfound() {
        let mut router = VfsRouter::new();
        router.mount("/v/jobs", MemoryFs::new());

        // A path with no mount at or below it stays NotFound — synthesis is
        // only for genuine ancestors of a mount.
        assert_eq!(
            router.list(Path::new("/nope")).await.unwrap_err().kind(),
            io::ErrorKind::NotFound
        );
        assert_eq!(
            router.stat(Path::new("/nope")).await.unwrap_err().kind(),
            io::ErrorKind::NotFound
        );
    }
}
