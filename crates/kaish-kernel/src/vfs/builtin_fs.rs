//! BuiltinFs — read-only VFS that presents builtins as executable entries under `/v/bin/`.

use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use async_trait::async_trait;

use crate::tools::ToolRegistry;
use super::{DirEntry, Filesystem};

/// A read-only filesystem that exposes registered builtins as entries.
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
            Err(io::Error::new(io::ErrorKind::NotFound, "builtin not found"))
        }
    }

    async fn write(&self, _path: &Path, _data: &[u8]) -> io::Result<()> {
        Err(io::Error::new(io::ErrorKind::PermissionDenied, "/v/bin is read-only"))
    }

    async fn list(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        let p = path.to_str().unwrap_or("");
        if !p.is_empty() && p != "." {
            return Err(io::Error::new(io::ErrorKind::NotFound, "not a directory"));
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
            Err(io::Error::new(io::ErrorKind::NotFound, "builtin not found"))
        }
    }

    async fn mkdir(&self, _path: &Path) -> io::Result<()> {
        Err(io::Error::new(io::ErrorKind::PermissionDenied, "/v/bin is read-only"))
    }

    async fn remove(&self, _path: &Path) -> io::Result<()> {
        Err(io::Error::new(io::ErrorKind::PermissionDenied, "/v/bin is read-only"))
    }

    fn read_only(&self) -> bool {
        true
    }

    fn real_path(&self, _path: &Path) -> Option<PathBuf> {
        None
    }
}
