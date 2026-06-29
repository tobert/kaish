//! Local filesystem backend.
//!
//! Provides access to real filesystem paths, with optional read-only mode.

use crate::traits::{DirEntry, DirEntryKind, Filesystem, ReadRange};
use async_trait::async_trait;
use std::io;
use std::path::{Path, PathBuf};
use tokio::fs;

/// Local filesystem backend.
///
/// All operations are relative to `root`. For example, if `root` is
/// `/home/amy/project`, then `read("src/main.rs")` reads
/// `/home/amy/project/src/main.rs`.
#[derive(Debug, Clone)]
pub struct LocalFs {
    root: PathBuf,
    read_only: bool,
}

impl LocalFs {
    /// Create a new local filesystem rooted at the given path.
    ///
    /// The path must exist and be a directory.
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self {
            root: root.into(),
            read_only: false,
        }
    }

    /// Create a read-only local filesystem.
    pub fn read_only(root: impl Into<PathBuf>) -> Self {
        Self {
            root: root.into(),
            read_only: true,
        }
    }

    /// Set whether this filesystem is read-only.
    pub fn set_read_only(&mut self, read_only: bool) {
        self.read_only = read_only;
    }

    /// Get the root path.
    pub fn root(&self) -> &Path {
        &self.root
    }

    /// Resolve a relative path to an absolute path within the root.
    ///
    /// Returns an error if the path escapes the root (via `..`).
    fn resolve(&self, path: &Path) -> io::Result<PathBuf> {
        // Strip leading slash if present
        let path = path.strip_prefix("/").unwrap_or(path);

        // Join with root
        let full = self.root.join(path);

        // Canonicalize to resolve symlinks and ..
        // For non-existent paths, we need to check parent
        let canonical = if full.exists() {
            full.canonicalize()?
        } else {
            // For new files, canonicalize parent and append filename
            let parent = full.parent().ok_or_else(|| {
                io::Error::new(io::ErrorKind::InvalidInput, "invalid path")
            })?;
            let filename = full.file_name().ok_or_else(|| {
                io::Error::new(io::ErrorKind::InvalidInput, "invalid path")
            })?;

            if parent.exists() {
                parent.canonicalize()?.join(filename)
            } else {
                // Parent doesn't exist, just use the path as-is
                // (will fail on actual operation)
                full
            }
        };

        // Verify we haven't escaped the root
        let canonical_root = self.root.canonicalize().unwrap_or_else(|_| self.root.clone());
        if !canonical.starts_with(&canonical_root) {
            return Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                format!(
                    "path escapes root: {} is not under {}",
                    canonical.display(),
                    canonical_root.display()
                ),
            ));
        }

        Ok(canonical)
    }

    /// Resolve a path within the root WITHOUT following symlinks.
    ///
    /// Used by `lstat()` and `read_link()` which must not follow symlinks.
    /// Validates that the path stays within the sandbox by normalizing
    /// path components (resolving `.` and `..`) without canonicalization.
    fn resolve_no_follow(&self, path: &Path) -> io::Result<PathBuf> {
        let path = path.strip_prefix("/").unwrap_or(path);

        let mut normalized = self.root.clone();
        for component in path.components() {
            match component {
                std::path::Component::ParentDir => {
                    if normalized == self.root {
                        return Err(io::Error::new(
                            io::ErrorKind::PermissionDenied,
                            "path escapes root",
                        ));
                    }
                    normalized.pop();
                    if !normalized.starts_with(&self.root) {
                        return Err(io::Error::new(
                            io::ErrorKind::PermissionDenied,
                            "path escapes root",
                        ));
                    }
                }
                std::path::Component::Normal(c) => normalized.push(c),
                std::path::Component::CurDir => {} // skip
                _ => {}
            }
        }

        // Final containment check
        if !normalized.starts_with(&self.root) {
            return Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                "path escapes root",
            ));
        }
        Ok(normalized)
    }

    /// Resolve a path for an unlink/remove: follow intermediate symlinks but
    /// NOT the final component, mirroring `unlink(2)`/`rmdir(2)` semantics.
    ///
    /// `resolve()` canonicalizes the *whole* path, so removing a symlink would
    /// resolve to (and operate on) its target — `rm symlink-to-dir` could then
    /// delete the target's contents. Here we canonicalize the parent only and
    /// re-attach the literal final component, so `symlink_metadata` + `remove_*`
    /// act on the link itself. The root-containment check still applies to the
    /// resolved parent, so an intermediate symlink escaping the root is rejected.
    fn resolve_for_unlink(&self, path: &Path) -> io::Result<PathBuf> {
        let path = path.strip_prefix("/").unwrap_or(path);
        let full = self.root.join(path);

        let parent = full
            .parent()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "invalid path"))?;
        let filename = full
            .file_name()
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "invalid path"))?;

        // Canonicalize the parent (following intermediate symlinks); if the
        // parent doesn't exist there's nothing to remove, so surface NotFound.
        let canonical = parent.canonicalize()?.join(filename);

        let canonical_root = self.root.canonicalize().unwrap_or_else(|_| self.root.clone());
        if !canonical.starts_with(&canonical_root) {
            return Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                format!(
                    "path escapes root: {} is not under {}",
                    canonical.display(),
                    canonical_root.display()
                ),
            ));
        }
        Ok(canonical)
    }

    /// Check if write operations are allowed.
    fn check_writable(&self) -> io::Result<()> {
        if self.read_only {
            Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                "filesystem is read-only",
            ))
        } else {
            Ok(())
        }
    }

    /// Extract permissions from std::fs::Metadata (unix only).
    #[cfg(unix)]
    fn extract_permissions(meta: &std::fs::Metadata) -> Option<u32> {
        use std::os::unix::fs::PermissionsExt;
        Some(meta.permissions().mode())
    }

    #[cfg(not(unix))]
    fn extract_permissions(_meta: &std::fs::Metadata) -> Option<u32> {
        None
    }

    /// Build a [`DirEntry`] for one directory member named by `path`, *without*
    /// following symlinks — or `Ok(None)` if that member has vanished since the
    /// enclosing `read_dir` snapshot was taken.
    ///
    /// [`list`](LocalFs::list) stats each entry in a step separate from the `read_dir`
    /// that yielded it, so on a *live* directory an entry can be unlinked in that window
    /// (a concurrent writer, a build churning `target/`, an editor swapping a temp file).
    /// A removed entry is not a listing failure: report it gone so the caller skips it —
    /// the way `ls(1)` tolerates a file deleted mid-scan — rather than letting one
    /// `ENOENT` from a single sibling sink the whole listing. Any *other* stat error
    /// (e.g. `EACCES`) is genuine and still propagates. A *dangling* symlink is reported,
    /// not skipped: `symlink_metadata` stats the link itself, which still exists.
    async fn dir_entry_no_follow(path: &Path) -> io::Result<Option<DirEntry>> {
        let metadata = match fs::symlink_metadata(path).await {
            Ok(m) => m,
            Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(None),
            Err(e) => return Err(e),
        };
        let file_type = metadata.file_type();

        let (kind, symlink_target) = if file_type.is_symlink() {
            // The link's own target, best-effort — a dangling link still lists.
            (DirEntryKind::Symlink, fs::read_link(path).await.ok())
        } else if file_type.is_dir() {
            (DirEntryKind::Directory, None)
        } else {
            // Special files (sockets, pipes, devices) → File. See stat().
            (DirEntryKind::File, None)
        };

        Ok(Some(DirEntry {
            name: path
                .file_name()
                .map(|n| n.to_string_lossy().into_owned())
                .unwrap_or_default(),
            kind,
            size: metadata.len(),
            modified: metadata.modified().ok(),
            permissions: Self::extract_permissions(&metadata),
            symlink_target,
        }))
    }
}

#[async_trait]
impl Filesystem for LocalFs {
    async fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        let full_path = self.resolve(path)?;
        fs::read(&full_path).await
    }

    async fn read_range(&self, path: &Path, range: Option<ReadRange>) -> io::Result<Vec<u8>> {
        // A byte range gets a true positional read — seek to the offset and read
        // at most `limit` bytes — so chunked/streaming scans and `head -c` never
        // pull the whole file into memory. Line ranges (and a bare `None`) fall
        // back to the default whole-file read + slice: line slicing needs the
        // full text anyway, and `None` means "everything".
        let Some(r) = range else {
            return self.read(path).await;
        };
        if r.offset.is_none() && r.limit.is_none() {
            let content = self.read(path).await?;
            return Ok(r.apply(&content));
        }
        let full_path = self.resolve(path)?;
        let offset = r.offset.unwrap_or(0);
        let limit = r.limit;
        // Blocking std file I/O on the blocking pool, mirroring `set_mtime`.
        tokio::task::spawn_blocking(move || -> io::Result<Vec<u8>> {
            use std::io::{Read, Seek, SeekFrom};
            let mut file = std::fs::File::open(&full_path)?;
            if offset > 0 {
                // Seeking past EOF is allowed; the following read yields 0 bytes,
                // which matches the empty slice an out-of-range request returns.
                file.seek(SeekFrom::Start(offset))?;
            }
            let mut buf = Vec::new();
            match limit {
                Some(limit) => {
                    file.take(limit).read_to_end(&mut buf)?;
                }
                None => {
                    file.read_to_end(&mut buf)?;
                }
            }
            Ok(buf)
        })
        .await
        .map_err(io::Error::other)?
    }

    async fn write(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        self.check_writable()?;
        let full_path = self.resolve(path)?;

        // Ensure parent directory exists
        if let Some(parent) = full_path.parent() {
            fs::create_dir_all(parent).await?;
        }

        fs::write(&full_path, data).await
    }

    async fn set_mtime(&self, path: &Path, mtime: std::time::SystemTime) -> io::Result<()> {
        self.check_writable()?;
        let full_path = self.resolve(path)?;
        // `set_modified` calls futimens(2) on the fd. Run it on the blocking
        // pool — std file I/O must not occupy an async worker. We open with
        // write access because that is what POSIX touch-to-update requires.
        tokio::task::spawn_blocking(move || {
            let file = std::fs::OpenOptions::new().write(true).open(&full_path)?;
            file.set_modified(mtime)
        })
        .await
        .map_err(io::Error::other)?
    }

    async fn list(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        let full_path = self.resolve(path)?;
        let mut entries = Vec::new();
        let mut dir = fs::read_dir(&full_path).await?;

        while let Some(entry) = dir.next_entry().await? {
            // Stat each entry separately from the `read_dir` above, so an entry unlinked
            // in that window is skipped (`Ok(None)`), not fatal — one vanished sibling
            // must not sink the whole listing. See `dir_entry_no_follow`.
            if let Some(de) = Self::dir_entry_no_follow(&entry.path()).await? {
                entries.push(de);
            }
        }

        entries.sort_by(|a, b| a.name.cmp(&b.name));
        Ok(entries)
    }

    async fn stat(&self, path: &Path) -> io::Result<DirEntry> {
        let full_path = self.resolve(path)?;
        // stat follows symlinks
        let meta = fs::metadata(&full_path).await?;

        let kind = if meta.is_dir() {
            DirEntryKind::Directory
        } else {
            // Unix special files (sockets, pipes, block/char devices) are classified
            // as File. kaish doesn't operate on special files, and adding a variant
            // would force match-arm changes everywhere for no practical benefit.
            DirEntryKind::File
        };

        let name = path
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| "/".to_string());

        Ok(DirEntry {
            name,
            kind,
            size: meta.len(),
            modified: meta.modified().ok(),
            permissions: Self::extract_permissions(&meta),
            symlink_target: None, // stat follows symlinks
        })
    }

    async fn lstat(&self, path: &Path) -> io::Result<DirEntry> {
        // lstat doesn't follow symlinks - validate containment without canonicalization
        let full_path = self.resolve_no_follow(path)?;

        // Use symlink_metadata which doesn't follow symlinks
        let meta = fs::symlink_metadata(&full_path).await?;

        let file_type = meta.file_type();
        let kind = if file_type.is_symlink() {
            DirEntryKind::Symlink
        } else if meta.is_dir() {
            DirEntryKind::Directory
        } else {
            // Special files (sockets, pipes, devices) → File. See stat() comment.
            DirEntryKind::File
        };

        let symlink_target = if file_type.is_symlink() {
            fs::read_link(&full_path).await.ok()
        } else {
            None
        };

        let name = path
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| "/".to_string());

        Ok(DirEntry {
            name,
            kind,
            size: meta.len(),
            modified: meta.modified().ok(),
            permissions: Self::extract_permissions(&meta),
            symlink_target,
        })
    }

    async fn read_link(&self, path: &Path) -> io::Result<PathBuf> {
        let full_path = self.resolve_no_follow(path)?;
        fs::read_link(&full_path).await
    }

    async fn symlink(&self, target: &Path, link: &Path) -> io::Result<()> {
        self.check_writable()?;

        // Validate absolute symlink targets stay within sandbox.
        // `resolve` would strip the leading slash and treat `/etc/passwd` as
        // root-relative, so `<root>/etc/passwd` always "contains". For symlink
        // targets the OS follows the literal absolute path, so compare the
        // canonical target (or the literal path if it doesn't exist yet) to
        // the canonical root.
        if target.is_absolute() {
            let canonical_root = self.root.canonicalize().unwrap_or_else(|_| self.root.clone());
            let canonical_target = target.canonicalize().unwrap_or_else(|_| target.to_path_buf());
            if !canonical_target.starts_with(&canonical_root) {
                return Err(io::Error::new(
                    io::ErrorKind::PermissionDenied,
                    format!("symlink target escapes root: {}", target.display()),
                ));
            }
        }

        let link_path = self.resolve_no_follow(link)?;

        // Ensure parent directory exists
        if let Some(parent) = link_path.parent() {
            fs::create_dir_all(parent).await?;
        }

        #[cfg(unix)]
        {
            tokio::fs::symlink(target, &link_path).await
        }
        #[cfg(windows)]
        {
            // Windows needs to know if target is a file or directory
            // Default to file symlink; for directories use symlink_dir
            tokio::fs::symlink_file(target, &link_path).await
        }
    }

    async fn mkdir(&self, path: &Path) -> io::Result<()> {
        self.check_writable()?;
        let full_path = self.resolve(path)?;
        fs::create_dir_all(&full_path).await
    }

    async fn remove(&self, path: &Path) -> io::Result<()> {
        self.check_writable()?;
        // Never follow the final symlink: a symlink (even to a directory) must
        // be unlinked, not resolved to its target. `symlink_metadata` reports
        // the link itself, so a symlink takes the `remove_file` (unlink) branch.
        let full_path = self.resolve_for_unlink(path)?;
        let meta = fs::symlink_metadata(&full_path).await?;

        if meta.is_dir() {
            fs::remove_dir(&full_path).await
        } else {
            fs::remove_file(&full_path).await
        }
    }

    async fn rename(&self, from: &Path, to: &Path) -> io::Result<()> {
        self.check_writable()?;
        // Don't follow the source's final symlink: `mv link new` must rename the
        // link itself, not canonicalize to (and move) its target. The dest keeps
        // normal resolution — it's the path we're creating, and resolve()'s
        // missing-parent fallback pairs with the create_dir_all below.
        let from_path = self.resolve_for_unlink(from)?;
        let to_path = self.resolve(to)?;

        // Ensure parent directory exists for destination
        if let Some(parent) = to_path.parent() {
            fs::create_dir_all(parent).await?;
        }

        fs::rename(&from_path, &to_path).await
    }

    fn read_only(&self) -> bool {
        self.read_only
    }

    fn real_path(&self, path: &Path) -> Option<PathBuf> {
        self.resolve(path).ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    use std::sync::atomic::{AtomicU64, Ordering};

    static TEST_COUNTER: AtomicU64 = AtomicU64::new(0);

    fn temp_dir() -> PathBuf {
        let id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
        env::temp_dir().join(format!("kaish-test-{}-{}", std::process::id(), id))
    }

    async fn setup() -> (LocalFs, PathBuf) {
        let dir = temp_dir();
        let _ = fs::remove_dir_all(&dir).await;
        fs::create_dir_all(&dir).await.unwrap();
        (LocalFs::new(&dir), dir)
    }

    async fn cleanup(dir: &Path) {
        let _ = fs::remove_dir_all(dir).await;
    }

    #[tokio::test]
    async fn test_write_and_read() {
        let (fs, dir) = setup().await;

        fs.write(Path::new("test.txt"), b"hello").await.unwrap();
        let data = fs.read(Path::new("test.txt")).await.unwrap();
        assert_eq!(data, b"hello");

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_read_range_bytes_positional() {
        let (fs, dir) = setup().await;
        fs.write(Path::new("data.bin"), b"0123456789abcdef")
            .await
            .unwrap();

        // Mid-file slice.
        let mid = fs
            .read_range(Path::new("data.bin"), Some(ReadRange::bytes(4, 5)))
            .await
            .unwrap();
        assert_eq!(mid, b"45678");

        // Offset to end, with a limit that runs past EOF.
        let tail = fs
            .read_range(Path::new("data.bin"), Some(ReadRange::bytes(10, 999)))
            .await
            .unwrap();
        assert_eq!(tail, b"abcdef");

        // Offset past EOF yields an empty read (no error) — the streaming
        // loop relies on this to detect EOF.
        let past = fs
            .read_range(Path::new("data.bin"), Some(ReadRange::bytes(100, 8)))
            .await
            .unwrap();
        assert!(past.is_empty());

        // `None` is still the whole file.
        let whole = fs
            .read_range(Path::new("data.bin"), None)
            .await
            .unwrap();
        assert_eq!(whole, b"0123456789abcdef");

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_read_range_reconstructs_file_in_chunks() {
        let (fs, dir) = setup().await;
        let payload: Vec<u8> = (0..1000u32).map(|i| (i % 251) as u8).collect();
        fs.write(Path::new("big.bin"), &payload).await.unwrap();

        let mut rebuilt = Vec::new();
        let mut offset = 0u64;
        loop {
            let chunk = fs
                .read_range(Path::new("big.bin"), Some(ReadRange::bytes(offset, 256)))
                .await
                .unwrap();
            if chunk.is_empty() {
                break;
            }
            offset += chunk.len() as u64;
            rebuilt.extend_from_slice(&chunk);
        }
        assert_eq!(rebuilt, payload);

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_nested_write() {
        let (fs, dir) = setup().await;

        fs.write(Path::new("a/b/c.txt"), b"nested").await.unwrap();
        let data = fs.read(Path::new("a/b/c.txt")).await.unwrap();
        assert_eq!(data, b"nested");

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_read_only() {
        let (_, dir) = setup().await;
        let fs = LocalFs::read_only(&dir);

        let result = fs.write(Path::new("test.txt"), b"data").await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::PermissionDenied);

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_list() {
        let (fs, dir) = setup().await;

        fs.write(Path::new("a.txt"), b"a").await.unwrap();
        fs.write(Path::new("b.txt"), b"b").await.unwrap();
        fs.mkdir(Path::new("subdir")).await.unwrap();

        let entries = fs.list(Path::new("")).await.unwrap();
        assert_eq!(entries.len(), 3);

        let names: Vec<_> = entries.iter().map(|e| &e.name).collect();
        assert!(names.contains(&&"a.txt".to_string()));
        assert!(names.contains(&&"b.txt".to_string()));
        assert!(names.contains(&&"subdir".to_string()));

        cleanup(&dir).await;
    }

    // A directory entry can disappear between the `read_dir` snapshot and the per-entry
    // `symlink_metadata` `list` does on a *live* directory. The helper that does that stat
    // must report a vanished entry as gone (skippable), not as a hard error — otherwise one
    // unlinked sibling sinks the whole listing (the bug this fixes).
    #[tokio::test]
    async fn dir_entry_no_follow_reports_a_vanished_entry_as_gone() {
        let (_fs, dir) = setup().await;
        // A path under the (real) test dir that does not exist — exactly what a stat sees
        // when the entry was unlinked after `read_dir` yielded it.
        let gone = dir.join("already-unlinked");
        let got = LocalFs::dir_entry_no_follow(&gone).await.unwrap();
        assert!(
            got.is_none(),
            "a missing entry must be reported as gone (Ok(None)), not an error — got {got:?}"
        );
        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn dir_entry_no_follow_builds_a_real_files_entry() {
        let (fs, dir) = setup().await;
        fs.write(Path::new("hi.txt"), b"hello").await.unwrap();

        let entry = LocalFs::dir_entry_no_follow(&dir.join("hi.txt"))
            .await
            .unwrap()
            .expect("a present file must produce an entry");
        assert_eq!(entry.name, "hi.txt");
        assert_eq!(entry.kind, DirEntryKind::File);
        assert_eq!(entry.size, 5);

        cleanup(&dir).await;
    }

    // The skip is precise: it drops only entries whose stat says *gone*. A **dangling**
    // symlink is not gone — `symlink_metadata` stats the link itself, which still exists —
    // so it must stay in the listing (as a Symlink), proving we don't over-skip.
    #[cfg(unix)]
    #[tokio::test]
    async fn list_keeps_a_dangling_symlink() {
        let (fs, dir) = setup().await;
        fs.write(Path::new("real.txt"), b"r").await.unwrap();
        std::os::unix::fs::symlink("nowhere", dir.join("dangling")).unwrap();

        let entries = fs.list(Path::new("")).await.unwrap();
        let dangling = entries
            .iter()
            .find(|e| e.name == "dangling")
            .expect("a dangling symlink must still be listed, not skipped as gone");
        assert_eq!(dangling.kind, DirEntryKind::Symlink);
        assert!(entries.iter().any(|e| e.name == "real.txt"));

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_stat() {
        let (fs, dir) = setup().await;

        fs.write(Path::new("file.txt"), b"content").await.unwrap();
        fs.mkdir(Path::new("dir")).await.unwrap();

        let file_entry = fs.stat(Path::new("file.txt")).await.unwrap();
        assert!(file_entry.is_file());
        assert_eq!(file_entry.size, 7);

        let dir_entry = fs.stat(Path::new("dir")).await.unwrap();
        assert!(dir_entry.is_dir());

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_remove() {
        let (fs, dir) = setup().await;

        fs.write(Path::new("file.txt"), b"data").await.unwrap();
        assert!(fs.exists(Path::new("file.txt")).await);

        fs.remove(Path::new("file.txt")).await.unwrap();
        assert!(!fs.exists(Path::new("file.txt")).await);

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_path_escape_blocked() {
        let (fs, dir) = setup().await;

        // Trying to escape via .. should fail
        let result = fs.read(Path::new("../../../etc/passwd")).await;
        assert!(result.is_err());

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_lstat_path_escape_blocked() {
        // Bug H: lstat must validate path containment
        let (fs, dir) = setup().await;

        let result = fs.lstat(Path::new("../../etc/passwd")).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::PermissionDenied);

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_read_link_path_escape_blocked() {
        // Bug H: read_link must validate path containment
        let (fs, dir) = setup().await;

        let result = fs.read_link(Path::new("../../etc/passwd")).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::PermissionDenied);

        cleanup(&dir).await;
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn test_lstat_on_valid_symlink() {
        // Regression: lstat should still work for valid symlinks
        let (fs, dir) = setup().await;

        fs.write(Path::new("target.txt"), b"content").await.unwrap();
        fs.symlink(Path::new("target.txt"), Path::new("link.txt"))
            .await
            .unwrap();

        let entry = fs.lstat(Path::new("link.txt")).await.unwrap();
        assert!(entry.is_symlink(), "lstat should report symlink kind");

        cleanup(&dir).await;
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn test_symlink_absolute_target_escape_blocked() {
        // Bug I: absolute symlink targets must stay within sandbox
        let (fs, dir) = setup().await;

        let result = fs
            .symlink(Path::new("/etc/passwd"), Path::new("escape_link"))
            .await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::PermissionDenied);

        cleanup(&dir).await;
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn test_remove_symlink_to_dir_unlinks_link_not_target() {
        // Safety regression: `rm <symlink-to-dir>` must unlink the link and
        // leave the target directory (and its contents) intact. resolve()
        // canonicalizes, so before the resolve_for_unlink fix remove() would
        // operate on the target — deleting/erroring on real data.
        let (fs, dir) = setup().await;

        fs.mkdir(Path::new("realdir")).await.unwrap();
        fs.write(Path::new("realdir/keep.txt"), b"precious")
            .await
            .unwrap();
        fs.symlink(Path::new("realdir"), Path::new("link"))
            .await
            .unwrap();

        fs.remove(Path::new("link")).await.unwrap();

        // The symlink is gone...
        assert!(
            fs.lstat(Path::new("link")).await.is_err(),
            "symlink should be unlinked"
        );
        // ...but the target dir and its file survive untouched.
        assert!(
            fs.exists(Path::new("realdir")).await,
            "target dir must survive"
        );
        assert_eq!(
            fs.read(Path::new("realdir/keep.txt")).await.unwrap(),
            b"precious"
        );

        cleanup(&dir).await;
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn test_remove_symlink_to_file_unlinks_link_not_target() {
        let (fs, dir) = setup().await;

        fs.write(Path::new("target.txt"), b"content").await.unwrap();
        fs.symlink(Path::new("target.txt"), Path::new("link.txt"))
            .await
            .unwrap();

        fs.remove(Path::new("link.txt")).await.unwrap();

        assert!(
            fs.lstat(Path::new("link.txt")).await.is_err(),
            "symlink should be unlinked"
        );
        assert_eq!(
            fs.read(Path::new("target.txt")).await.unwrap(),
            b"content",
            "target file must survive"
        );

        cleanup(&dir).await;
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn test_symlink_relative_target_allowed() {
        // Regression: relative symlink targets should still be allowed
        let (fs, dir) = setup().await;

        fs.write(Path::new("target.txt"), b"content").await.unwrap();
        let result = fs
            .symlink(Path::new("target.txt"), Path::new("rel_link"))
            .await;
        assert!(result.is_ok());

        cleanup(&dir).await;
    }
}
