//! BuiltinFs — read-only VFS that lists builtins as file entries under `/v/bin/`.
//!
//! The entries are not executable and nothing here ever executes. `stat`
//! reports no mode, so `test -x /v/bin/grep` answers NO, and `real_path`
//! returns `None`, so there is no path for exec(2) to open. `read` returns a
//! line opening with `#!`, which makes an entry look executable.
//!
//! Running a builtin goes by name through the `ToolRegistry` and never routes
//! through this filesystem. `/v/bin` is an inventory to list and read, not a
//! directory of programs. Reporting `0o111` would flip `test -x /v/bin/*` from
//! NO to YES — a behavior change that wants its own decision.
//!
//! `read_only()` is `true`, and the closed `-w` default depends on it staying
//! true — see `Filesystem::path_access`.

use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use async_trait::async_trait;

use crate::tools::ToolRegistry;
use super::{DirEntry, Filesystem};

/// A read-only filesystem that exposes registered builtins as file
/// entries. Listable and readable; not executable — see the module docs.
pub struct BuiltinFs {
    tools: Arc<ToolRegistry>,
}

impl BuiltinFs {
    pub fn new(tools: Arc<ToolRegistry>) -> Self {
        Self { tools }
    }
}

#[async_trait]
impl Filesystem for BuiltinFs {
    async fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        let name = path.to_str().unwrap_or("");
        if self.tools.get(name).is_some() {
            Ok(format!("#!/v/bin — kaish builtin: {}\n", name).into_bytes())
        } else {
            // The name alone — "builtin not found" restated
            // `ErrorKind::NotFound`'s own meaning, and
            // `BackendError::NotFound`'s Display adds "not found:" once
            // when this crosses that boundary, and named neither the
            // requested name nor a path either way.
            Err(io::Error::new(io::ErrorKind::NotFound, name.to_string()))
        }
    }

    async fn write(&self, _path: &Path, _data: &[u8]) -> io::Result<()> {
        Err(io::Error::new(io::ErrorKind::ReadOnlyFilesystem, "/v/bin is read-only"))
    }

    async fn list(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        let p = path.to_str().unwrap_or("");
        if !p.is_empty() && p != "." {
            // Wrong kind before this fix too: every entry under `/v/bin` is
            // a file, so listing one is `NotADirectory`, not `NotFound` —
            // and the path was missing outright.
            return Err(io::Error::new(io::ErrorKind::NotADirectory, p.to_string()));
        }
        let mut entries: Vec<DirEntry> = self.tools.names().iter().map(|name| {
            DirEntry::file(name.to_string(), 0)
        }).collect();
        entries.sort_by(|a, b| a.name.cmp(&b.name));
        Ok(entries)
    }

    async fn stat(&self, path: &Path) -> io::Result<DirEntry> {
        let name = path.to_str().unwrap_or("");
        if name.is_empty() || name == "." {
            return Ok(DirEntry::directory("."));
        }
        if self.tools.get(name).is_some() {
            Ok(DirEntry::file(name, 0))
        } else {
            Err(io::Error::new(io::ErrorKind::NotFound, name.to_string()))
        }
    }

    async fn mkdir(&self, _path: &Path) -> io::Result<()> {
        Err(io::Error::new(io::ErrorKind::ReadOnlyFilesystem, "/v/bin is read-only"))
    }

    async fn remove(&self, _path: &Path) -> io::Result<()> {
        Err(io::Error::new(io::ErrorKind::ReadOnlyFilesystem, "/v/bin is read-only"))
    }

    fn read_only(&self) -> bool {
        true
    }

    fn real_path(&self, _path: &Path) -> Option<PathBuf> {
        None
    }
}
