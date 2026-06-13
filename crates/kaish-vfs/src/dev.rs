//! Synthetic device filesystem.
//!
//! Mounted at `/dev` in the hermetic VFS modes (sandboxed / no-local) where the
//! host's real `/dev` is not reachable. It provides software implementations of
//! the handful of character devices shell scripts actually lean on:
//!
//! - `/dev/null` — a sink: writes are discarded, reads return empty.
//! - `/dev/zero` — an endless stream of zero bytes.
//! - `/dev/urandom`, `/dev/random` — endless cryptographic random bytes from
//!   the OS CSPRNG (`getrandom`). Both alias the same non-blocking source.
//!
//! The endless devices (`zero`, `urandom`, `random`) have no whole-device read:
//! kaish reads whole files into memory, so `cat /dev/urandom` is a loud error.
//! A counted read — `head -c N`, `dd … count=…` — yields exactly the requested
//! bytes via [`Filesystem::read_range`]. Raw random bytes only become *useful*
//! through a binary-aware tool (`dd`) since kaish pipes are UTF-8 text; see
//! `docs/binary-data.md`.

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
    /// Endless cryptographic random bytes (OS CSPRNG via `getrandom`).
    /// `/dev/urandom` and `/dev/random` both map here.
    Random,
}

impl Device {
    /// The device's source word, for error messages.
    fn name(self) -> &'static str {
        match self {
            Device::Null => "null",
            Device::Zero => "zero",
            Device::Random => "urandom",
        }
    }
}

impl DevFs {
    /// Create a new synthetic device filesystem.
    pub fn new() -> Self {
        Self
    }

    /// Names of the devices exposed under this mount, for directory listing.
    const NAMES: [&'static str; 4] = ["null", "random", "urandom", "zero"];

    /// Resolve a mount-relative path to a known device. Paths arrive with the
    /// `/dev` prefix already stripped by the router, so we see `null`/`zero`/…
    fn device(path: &Path) -> Option<Device> {
        match path.to_str()?.trim_start_matches('/') {
            "null" => Some(Device::Null),
            "zero" => Some(Device::Zero),
            "urandom" | "random" => Some(Device::Random),
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
            Some(dev) => Err(Self::unbounded(dev.name())), // endless: no whole read
            None => Err(Self::not_found(path)),
        }
    }

    async fn read_range(&self, path: &Path, range: Option<ReadRange>) -> io::Result<Vec<u8>> {
        let Some(dev) = Self::device(path) else {
            return Err(Self::not_found(path));
        };
        // The sink ignores any range — it is always empty.
        if dev == Device::Null {
            return Ok(Vec::new());
        }
        // Endless stream: only a byte count is answerable. A None range or a
        // line-only range is the unbounded "give me everything" ask.
        let limit = match range.and_then(|r| r.limit) {
            Some(n) => n,
            None => return Err(Self::unbounded(dev.name())),
        };
        if limit > MAX_DEVICE_READ_BYTES {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!(
                    "requested {limit} bytes from /dev/{} exceeds the device read cap \
                     of {MAX_DEVICE_READ_BYTES} bytes",
                    dev.name()
                ),
            ));
        }
        let mut buf = vec![0u8; limit as usize];
        if dev == Device::Random {
            getrandom::fill(&mut buf).map_err(|e| {
                io::Error::other(format!("/dev/{}: entropy source failed: {e}", dev.name()))
            })?;
        }
        Ok(buf) // Device::Zero leaves the buffer zeroed
    }

    async fn write(&self, path: &Path, _data: &[u8]) -> io::Result<()> {
        // Every device accepts and discards writes — `cmd > /dev/null` is the
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
            Some(_) => Ok(Self::entry(
                path.to_str().unwrap_or_default().trim_start_matches('/'),
            )),
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
    async fn urandom_counted_read_is_random_and_sized() {
        let fs = DevFs::new();
        let a = fs
            .read_range(Path::new("urandom"), Some(ReadRange::bytes(0, 32)))
            .await
            .unwrap();
        assert_eq!(a.len(), 32, "exact byte count");
        // Two draws of 32 bytes are astronomically unlikely to match.
        let b = fs
            .read_range(Path::new("urandom"), Some(ReadRange::bytes(0, 32)))
            .await
            .unwrap();
        assert_ne!(a, b, "entropy: two draws must differ");
        // `random` aliases the same source.
        let c = fs
            .read_range(Path::new("random"), Some(ReadRange::bytes(0, 8)))
            .await
            .unwrap();
        assert_eq!(c.len(), 8);
    }

    #[tokio::test]
    async fn urandom_whole_read_is_loud_error() {
        let fs = DevFs::new();
        let err = fs.read(Path::new("urandom")).await.unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::InvalidInput);
        assert!(err.to_string().contains("head -c"), "names the fix: {err}");
    }

    #[tokio::test]
    async fn unknown_device_is_not_found() {
        let fs = DevFs::new();
        assert_eq!(
            fs.read(Path::new("sda")).await.unwrap_err().kind(),
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
        assert_eq!(
            names,
            vec![
                "null".to_string(),
                "random".to_string(),
                "urandom".to_string(),
                "zero".to_string()
            ]
        );
    }

    #[tokio::test]
    async fn stat_devices_and_root() {
        let fs = DevFs::new();
        assert_eq!(fs.stat(Path::new("")).await.unwrap().kind, DirEntryKind::Directory);
        for dev in ["null", "zero", "urandom", "random"] {
            let e = fs.stat(Path::new(dev)).await.unwrap();
            assert_eq!(e.kind, DirEntryKind::File, "{dev}");
            assert_eq!(e.name, dev, "stat names the device");
        }
        assert_eq!(
            fs.stat(Path::new("nope")).await.unwrap_err().kind(),
            io::ErrorKind::NotFound
        );
    }
}
