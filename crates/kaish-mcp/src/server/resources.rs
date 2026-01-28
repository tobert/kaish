//! MCP resources backed by the VFS.
//!
//! Exposes VFS contents as MCP resources with the `kaish://vfs/` URI scheme.

use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::{Context, Result};
use kaish_kernel::vfs::{Filesystem, VfsRouter};


/// Parse a kaish://vfs/ URI into a VFS path.
///
/// Returns `None` if the URI doesn't match the expected scheme.
pub fn parse_resource_uri(uri: &str) -> Option<PathBuf> {
    let prefix = "kaish://vfs";

    if uri == prefix {
        return Some(PathBuf::from("/"));
    }

    uri.strip_prefix("kaish://vfs/")
        .map(|path| PathBuf::from("/").join(path))
}

/// Build a resource URI from a VFS path.
pub fn build_resource_uri(path: &Path) -> String {
    let path_str = path.to_string_lossy();
    let path_str = path_str.trim_start_matches('/');

    if path_str.is_empty() {
        "kaish://vfs".to_string()
    } else {
        format!("kaish://vfs/{}", path_str)
    }
}

/// Resource info for MCP.
#[derive(Debug, Clone)]
pub struct ResourceInfo {
    /// Resource URI.
    pub uri: String,
    /// Human-readable name.
    pub name: String,
    /// Optional description.
    pub description: Option<String>,
    /// MIME type.
    pub mime_type: Option<String>,
}

/// List resources under a VFS path.
pub async fn list_resources(
    vfs: &Arc<VfsRouter>,
    path: &Path,
) -> Result<Vec<ResourceInfo>> {
    let entries = vfs.list(path).await.context("Failed to list VFS path")?;

    let mut resources = Vec::new();

    for entry in entries {
        let entry_path = path.join(&entry.name);
        let uri = build_resource_uri(&entry_path);

        let (description, mime_type) = match entry.entry_type {
            kaish_kernel::vfs::EntryType::Directory => {
                (Some("Directory".to_string()), Some("inode/directory".to_string()))
            }
            kaish_kernel::vfs::EntryType::File => {
                let mime = guess_mime_type(&entry.name);
                (None, Some(mime))
            }
        };

        resources.push(ResourceInfo {
            uri,
            name: entry.name,
            description,
            mime_type,
        });
    }

    Ok(resources)
}

/// Read a resource from the VFS.
pub async fn read_resource(
    vfs: &Arc<VfsRouter>,
    path: &Path,
) -> Result<ResourceContent> {
    let metadata = vfs.stat(path).await.context("Failed to stat VFS path")?;

    if metadata.is_dir {
        // For directories, return a listing
        let entries = vfs.list(path).await.context("Failed to list directory")?;
        let listing: Vec<String> = entries.iter().map(|e| {
            match e.entry_type {
                kaish_kernel::vfs::EntryType::Directory => format!("{}/", e.name),
                _ => e.name.clone(),
            }
        }).collect();

        return Ok(ResourceContent::Text {
            text: listing.join("\n"),
            mime_type: "text/plain".to_string(),
        });
    }

    // Read file content
    let data = vfs.read(path).await.context("Failed to read VFS path")?;
    let mime_type = guess_mime_type(&path.to_string_lossy());

    // Check if it's text or binary
    if is_likely_text(&data) {
        let text = String::from_utf8_lossy(&data).into_owned();
        Ok(ResourceContent::Text { text, mime_type })
    } else {
        Ok(ResourceContent::Blob {
            data,
            mime_type,
        })
    }
}

/// Resource content.
#[derive(Debug, Clone)]
pub enum ResourceContent {
    /// Text content.
    Text {
        text: String,
        mime_type: String,
    },
    /// Binary content.
    Blob {
        data: Vec<u8>,
        mime_type: String,
    },
}

/// Guess MIME type from filename.
fn guess_mime_type(name: &str) -> String {
    let ext = name.rsplit('.').next().unwrap_or("");

    match ext.to_lowercase().as_str() {
        // Text
        "txt" => "text/plain",
        "md" | "markdown" => "text/markdown",
        "json" => "application/json",
        "toml" => "application/toml",
        "yaml" | "yml" => "application/yaml",
        "xml" => "application/xml",
        "html" | "htm" => "text/html",
        "css" => "text/css",
        "csv" => "text/csv",

        // Code
        "rs" => "text/x-rust",
        "py" => "text/x-python",
        "js" => "text/javascript",
        "ts" => "text/typescript",
        "go" => "text/x-go",
        "c" | "h" => "text/x-c",
        "cpp" | "hpp" | "cc" => "text/x-c++",
        "java" => "text/x-java",
        "sh" | "bash" => "text/x-shellscript",
        "kai" => "text/x-kaish",

        // Config
        "ini" | "cfg" | "conf" => "text/plain",
        "env" => "text/plain",

        // Binary
        "png" => "image/png",
        "jpg" | "jpeg" => "image/jpeg",
        "gif" => "image/gif",
        "webp" => "image/webp",
        "svg" => "image/svg+xml",
        "pdf" => "application/pdf",
        "zip" => "application/zip",
        "tar" => "application/x-tar",
        "gz" => "application/gzip",

        _ => "application/octet-stream",
    }
    .to_string()
}

/// Check if data is likely text (contains no null bytes and is valid UTF-8ish).
fn is_likely_text(data: &[u8]) -> bool {
    // Empty files are text
    if data.is_empty() {
        return true;
    }

    // Check first 8KB for null bytes (common binary indicator)
    let sample = &data[..data.len().min(8192)];

    // If it has null bytes, it's probably binary
    if sample.contains(&0) {
        return false;
    }

    // Try to parse as UTF-8
    std::str::from_utf8(sample).is_ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_resource_uri() {
        assert_eq!(
            parse_resource_uri("kaish://vfs"),
            Some(PathBuf::from("/"))
        );
        assert_eq!(
            parse_resource_uri("kaish://vfs/"),
            Some(PathBuf::from("/"))
        );
        assert_eq!(
            parse_resource_uri("kaish://vfs/foo/bar.txt"),
            Some(PathBuf::from("/foo/bar.txt"))
        );
        assert_eq!(parse_resource_uri("file:///foo"), None);
        assert_eq!(parse_resource_uri("https://example.com"), None);
    }

    #[test]
    fn test_build_resource_uri() {
        assert_eq!(build_resource_uri(Path::new("/")), "kaish://vfs");
        assert_eq!(build_resource_uri(Path::new("/foo")), "kaish://vfs/foo");
        assert_eq!(
            build_resource_uri(Path::new("/foo/bar.txt")),
            "kaish://vfs/foo/bar.txt"
        );
    }

    #[test]
    fn test_guess_mime_type() {
        assert_eq!(guess_mime_type("file.rs"), "text/x-rust");
        assert_eq!(guess_mime_type("file.json"), "application/json");
        assert_eq!(guess_mime_type("file.png"), "image/png");
        assert_eq!(guess_mime_type("file.kai"), "text/x-kaish");
        assert_eq!(guess_mime_type("noext"), "application/octet-stream");
    }

    #[test]
    fn test_is_likely_text() {
        assert!(is_likely_text(b"Hello, world!"));
        assert!(is_likely_text(b""));
        assert!(is_likely_text("UTF-8: é".as_bytes()));
        assert!(!is_likely_text(b"Binary\x00data"));
    }
}
