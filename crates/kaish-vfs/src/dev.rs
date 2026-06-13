//! Synthetic device filesystem.
//!
//! Mounted at `/dev` in the hermetic VFS modes (sandboxed / no-local) where the
//! host's real `/dev` is not reachable. It provides software implementations of
//! the handful of character devices shell scripts actually lean on:
//!
//! - `/dev/null` — a sink: writes are discarded, reads return empty.
//! - `/dev/zero` — an endless stream of zero bytes. Because kaish reads whole
//!   files into memory (there is no infinite stream), a *whole-device* read
//!   (`cat /dev/zero`) is a loud error; a counted read (`head -c N /dev/zero`)
//!   yields exactly `N` zero bytes via [`Filesystem::read_range`].
//!
//! `/dev/urandom` is deliberately absent: random bytes are ~never valid UTF-8,
//! and kaish's pipes and `OutputData` are UTF-8 text, so raw entropy cannot
//! flow through intact yet. See `docs/issues.md` (binary-data path) before
//! adding it.

use crate::traits::{DirEntry, DirEntryKind, Filesystem, ReadRange};
use async_trait::async_trait;
use std::io;
use std::path::Path;

/// Upper bound on a single counted device read. A `head -c N /dev/zero` for an
/// absurd `N` would otherwise try to allocate `N` bytes up front and wedge the
/// kernel; we refuse loudly instead of OOMing.
const MAX_DEVICE_READ_BYTES: u64 = 64 * 1024 * 1024;

/// The synthetic `/dev`.
#[derive(Debug, Default, Clone, Copy)]
pub struct DevFs;

/// A device this filesystem knows how to synthesize.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Device {
    /// Discards writes, reads empty.
    Null,
    /// Endless zero bytes; only counted reads are answerable.
    Zero,
}

impl DevFs {
    /// Create a new synthetic device filesystem.
    pub fn new() -> Self {
        Self
    }

    /// Names of the devices exposed under this mount, for directory listing.
    const NAMES: [&'static str; 2] = ["null", "zero"];

    /// Resolve a mount-relative path to a known device. Paths arrive with the
    /// `/dev` prefix already stripped by the router, so we see `null`/`zero`.
    fn device(path: &Path) -> Option<Device> {
        match path.to_str()?.trim_start_matches('/') {
            "null" => Some(Device::Null),
            "zero" => Some(Device::Zero),
            _ => None,
        }
    }

    /// True when the path refers to the mount root itself.
    fn is_root(path: &Path) -> bool {
        let s = path.to_string_lossy();
        let trimmed = s.trim_matches('/');
        trimmed.is_empty() || trimmed == "."
    }

    fn not_found(path: &Path) -> io::Error {
        io::Error::new(
            io::ErrorKind::NotFound,
            format!("no such device: /dev/{}", path.display()),
        )
    }

    /// The error for asking an infinite device for "everything". Names the fix.
    fn unbounded(name: &str) -> io::Error {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            format!(
                "/dev/{name} is an endless device; reading the whole of it is unbounded. \
                 Read a fixed number of bytes instead, e.g. `head -c 32 /dev/{name}`"
            ),
        )
    }

    fn entry(name: &str) -> DirEntry {
        DirEntry {
            name: name.to_string(),
            kind: DirEntryKind::File,
            size: 0,
            modified: None,
            permissions: None,
            symlink_target: None,
        }
    }
}

#[async_trait]
impl Filesystem for DevFs {
    async fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        match Self::device(path) {
            Some(Device::Null) => Ok(Vec::new()),
            Some(Device::Zero) => Err(Self::unbounded("zero")),
            None => Err(Self::not_found(path)),
        }
    }

    async fn read_range(&self, path: &Path, range: Option<ReadRange>) -> io::Result<Vec<u8>> {
        match Self::device(path) {
            // The sink ignores any range — it is always empty.
            Some(Device::Null) => Ok(Vec::new()),
            Some(Device::Zero) => {
                let Some(range) = range else {
                    return Err(Self::unbounded("zero"));
                };
                // Only a byte count is meaningful for an endless stream; a line
                // range over infinite zeros is just as unbounded.
                let Some(limit) = range.limit else {
                    return Err(Self::unbounded("zero"));
                };
                if limit > MAX_DEVICE_READ_BYTES {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        format!(
                            "requested {limit} bytes from /dev/zero exceeds the device read cap \
                             of {MAX_DEVICE_READ_BYTES} bytes"
                        ),
                    ));
                }
                Ok(vec![0u8; limit as usize])
            }
            None => Err(Self::not_found(path)),
        }
    }

    async fn write(&self, path: &Path, _data: &[u8]) -> io::Result<()> {
        // Both devices accept and discard writes — `cmd > /dev/null` is the
        // whole point. Writing to an unknown device is still an error.
        match Self::device(path) {
            Some(_) => Ok(()),
            None => Err(Self::not_found(path)),
        }
    }

    async fn list(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        if Self::is_root(path) {
            return Ok(Self::NAMES.iter().map(|n| Self::entry(n)).collect());
        }
        if Self::device(path).is_some() {
            return Err(io::Error::new(
                io::ErrorKind::NotADirectory,
                format!("not a directory: /dev/{}", path.display()),
            ));
        }
        Err(Self::not_found(path))
    }

    async fn stat(&self, path: &Path) -> io::Result<DirEntry> {
        if Self::is_root(path) {
            return Ok(DirEntry {
                name: "dev".to_string(),
                kind: DirEntryKind::Directory,
                size: 0,
                modified: None,
                permissions: None,
                symlink_target: None,
            });
        }
        match Self::device(path) {
            Some(Device::Null) => Ok(Self::entry("null")),
            Some(Device::Zero) => Ok(Self::entry("zero")),
            None => Err(Self::not_found(path)),
        }
    }

    async fn mkdir(&self, path: &Path) -> io::Result<()> {
        Err(io::Error::new(
            io::ErrorKind::PermissionDenied,
            format!("/dev is read-only: cannot create {}", path.display()),
        ))
    }

    async fn remove(&self, path: &Path) -> io::Result<()> {
        Err(io::Error::new(
            io::ErrorKind::PermissionDenied,
            format!("/dev is read-only: cannot remove {}", path.display()),
        ))
    }

    fn read_only(&self) -> bool {
        // Writes to the devices "succeed" (they discard), so the mount is not
        // read-only in the sense the router cares about — refusing a write
        // would break `> /dev/null`.
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn null_reads_empty() {
        let fs = DevFs::new();
        assert_eq!(fs.read(Path::new("null")).await.unwrap(), b"");
        // A counted read of null is still empty.
        assert_eq!(
            fs.read_range(Path::new("null"), Some(ReadRange::bytes(0, 16)))
                .await
                .unwrap(),
            b""
        );
    }

    #[tokio::test]
    async fn null_discards_writes() {
        let fs = DevFs::new();
        fs.write(Path::new("null"), b"anything at all").await.unwrap();
    }

    #[tokio::test]
    async fn zero_counted_read_yields_zeros() {
        let fs = DevFs::new();
        let out = fs
            .read_range(Path::new("zero"), Some(ReadRange::bytes(0, 8)))
            .await
            .unwrap();
        assert_eq!(out, vec![0u8; 8]);
    }

    #[tokio::test]
    async fn zero_whole_read_is_loud_error() {
        let fs = DevFs::new();
        let err = fs.read(Path::new("zero")).await.unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::InvalidInput);
        assert!(err.to_string().contains("head -c"), "should name the fix: {err}");

        // A None range through read_range is the same unbounded ask.
        let err = fs.read_range(Path::new("zero"), None).await.unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::InvalidInput);
    }

    #[tokio::test]
    async fn zero_read_cap_is_enforced() {
        let fs = DevFs::new();
        let err = fs
            .read_range(Path::new("zero"), Some(ReadRange::bytes(0, MAX_DEVICE_READ_BYTES + 1)))
            .await
            .unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::InvalidInput);
        assert!(err.to_string().contains("cap"), "should mention the cap: {err}");
    }

    #[tokio::test]
    async fn unknown_device_is_not_found() {
        let fs = DevFs::new();
        assert_eq!(
            fs.read(Path::new("urandom")).await.unwrap_err().kind(),
            io::ErrorKind::NotFound
        );
        assert_eq!(
            fs.write(Path::new("sda"), b"x").await.unwrap_err().kind(),
            io::ErrorKind::NotFound
        );
    }

    #[tokio::test]
    async fn list_shows_devices() {
        let fs = DevFs::new();
        let names: Vec<_> = fs
            .list(Path::new(""))
            .await
            .unwrap()
            .into_iter()
            .map(|e| e.name)
            .collect();
        assert_eq!(names, vec!["null".to_string(), "zero".to_string()]);
    }

    #[tokio::test]
    async fn stat_devices_and_root() {
        let fs = DevFs::new();
        assert_eq!(fs.stat(Path::new("")).await.unwrap().kind, DirEntryKind::Directory);
        assert_eq!(fs.stat(Path::new("null")).await.unwrap().kind, DirEntryKind::File);
        assert_eq!(fs.stat(Path::new("zero")).await.unwrap().kind, DirEntryKind::File);
        assert_eq!(
            fs.stat(Path::new("nope")).await.unwrap_err().kind(),
            io::ErrorKind::NotFound
        );
    }
}
