//! Core async file walker using KernelBackend.
//!
//! Provides recursive directory traversal with filtering support.

use std::path::{Path, PathBuf};

use crate::backend::KernelBackend;

use super::{GlobPath, IgnoreFilter, IncludeExclude};

/// Types of entries to include in walk results.
#[derive(Debug, Clone, Copy, Default)]
pub struct EntryTypes {
    /// Include regular files.
    pub files: bool,
    /// Include directories.
    pub dirs: bool,
}

impl EntryTypes {
    /// Include only files.
    pub fn files_only() -> Self {
        Self {
            files: true,
            dirs: false,
        }
    }

    /// Include only directories.
    pub fn dirs_only() -> Self {
        Self {
            files: false,
            dirs: true,
        }
    }

    /// Include both files and directories.
    pub fn all() -> Self {
        Self {
            files: true,
            dirs: true,
        }
    }
}

/// Options for file walking.
#[derive(Debug, Clone)]
pub struct WalkOptions {
    /// Maximum depth to recurse (None = unlimited).
    pub max_depth: Option<usize>,
    /// Types of entries to include.
    pub entry_types: EntryTypes,
    /// Respect .gitignore files and default ignores.
    pub respect_gitignore: bool,
    /// Include hidden files (starting with .).
    pub include_hidden: bool,
    /// Include/exclude filters.
    pub filter: IncludeExclude,
}

impl Default for WalkOptions {
    fn default() -> Self {
        Self {
            max_depth: None,
            entry_types: EntryTypes::files_only(),
            respect_gitignore: true,
            include_hidden: false,
            filter: IncludeExclude::new(),
        }
    }
}

/// Async file walker using KernelBackend.
///
/// # Examples
/// ```ignore
/// use kaish_kernel::walker::{FileWalker, WalkOptions, GlobPath};
///
/// let walker = FileWalker::new(backend, "src")
///     .with_pattern(GlobPath::new("**/*.rs").unwrap())
///     .with_options(WalkOptions::default());
///
/// let files = walker.collect().await?;
/// ```
pub struct FileWalker<'a> {
    backend: &'a dyn KernelBackend,
    root: PathBuf,
    pattern: Option<GlobPath>,
    options: WalkOptions,
    ignore_filter: Option<IgnoreFilter>,
}

impl<'a> FileWalker<'a> {
    /// Create a new file walker starting at the given root.
    pub fn new(backend: &'a dyn KernelBackend, root: impl AsRef<Path>) -> Self {
        Self {
            backend,
            root: root.as_ref().to_path_buf(),
            pattern: None,
            options: WalkOptions::default(),
            ignore_filter: None,
        }
    }

    /// Set a glob pattern to filter results.
    pub fn with_pattern(mut self, pattern: GlobPath) -> Self {
        self.pattern = Some(pattern);
        self
    }

    /// Set walk options.
    pub fn with_options(mut self, options: WalkOptions) -> Self {
        self.options = options;
        self
    }

    /// Set the ignore filter explicitly.
    pub fn with_ignore(mut self, filter: IgnoreFilter) -> Self {
        self.ignore_filter = Some(filter);
        self
    }

    /// Collect all matching paths.
    pub async fn collect(mut self) -> anyhow::Result<Vec<PathBuf>> {
        // Set up ignore filter
        let ignore_filter = if self.options.respect_gitignore {
            let filter = self
                .ignore_filter
                .take()
                .unwrap_or_else(IgnoreFilter::with_defaults);

            // Try to load .gitignore from root
            let gitignore_path = self.root.join(".gitignore");
            if self.backend.exists(&gitignore_path).await {
                if let Ok(gitignore) =
                    IgnoreFilter::from_gitignore(&gitignore_path, self.backend).await
                {
                    // For now, just use the gitignore (we'll merge later)
                    Some(gitignore)
                } else {
                    Some(filter)
                }
            } else {
                Some(filter)
            }
        } else {
            self.ignore_filter.take()
        };

        self.ignore_filter = ignore_filter;

        let mut results = Vec::new();
        let mut stack = vec![(self.root.clone(), 0usize)];

        while let Some((dir, depth)) = stack.pop() {
            // Check max depth
            if let Some(max) = self.options.max_depth {
                if depth > max {
                    continue;
                }
            }

            // List directory contents
            let entries = match self.backend.list(&dir).await {
                Ok(entries) => entries,
                Err(_) => continue, // Silently skip unreadable dirs
            };

            for entry in entries {
                let full_path = dir.join(&entry.name);

                // Check hidden files
                if !self.options.include_hidden && entry.name.starts_with('.') {
                    continue;
                }

                // Check ignore filter
                if let Some(ref filter) = self.ignore_filter {
                    let relative = self.relative_path(&full_path);
                    if filter.is_ignored(&relative, entry.is_dir) {
                        continue;
                    }
                }

                // Check include/exclude filter
                if !self.options.filter.is_empty() {
                    let relative = self.relative_path(&full_path);
                    if self.options.filter.should_exclude(&relative) {
                        continue;
                    }
                    // Also check filename only for patterns like "*_test.rs"
                    if let Some(name) = full_path.file_name() {
                        if self
                            .options
                            .filter
                            .should_exclude(Path::new(name))
                        {
                            continue;
                        }
                    }
                }

                if entry.is_dir {
                    // Queue directory for recursion
                    stack.push((full_path.clone(), depth + 1));

                    // Yield directory if wanted
                    if self.options.entry_types.dirs && self.matches_pattern(&full_path) {
                        results.push(full_path);
                    }
                } else {
                    // Yield file if wanted
                    if self.options.entry_types.files && self.matches_pattern(&full_path) {
                        results.push(full_path);
                    }
                }
            }
        }

        Ok(results)
    }

    fn relative_path(&self, full_path: &Path) -> PathBuf {
        full_path
            .strip_prefix(&self.root)
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|_| full_path.to_path_buf())
    }

    fn matches_pattern(&self, path: &Path) -> bool {
        match &self.pattern {
            Some(pattern) => {
                let relative = self.relative_path(path);
                pattern.matches(&relative)
            }
            None => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_test_fs() -> Arc<VfsRouter> {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();

        // Create a test directory structure
        mem.mkdir(Path::new("src")).await.unwrap();
        mem.mkdir(Path::new("src/lib")).await.unwrap();
        mem.mkdir(Path::new("test")).await.unwrap();
        mem.mkdir(Path::new(".git")).await.unwrap();
        mem.mkdir(Path::new("node_modules")).await.unwrap();

        mem.write(Path::new("src/main.rs"), b"fn main() {}")
            .await
            .unwrap();
        mem.write(Path::new("src/lib.rs"), b"pub mod lib;")
            .await
            .unwrap();
        mem.write(Path::new("src/lib/utils.rs"), b"pub fn util() {}")
            .await
            .unwrap();
        mem.write(Path::new("test/main_test.rs"), b"#[test]")
            .await
            .unwrap();
        mem.write(Path::new("README.md"), b"# Test").await.unwrap();
        mem.write(Path::new(".hidden"), b"secret").await.unwrap();
        mem.write(Path::new(".git/config"), b"[core]")
            .await
            .unwrap();
        mem.write(Path::new("node_modules/pkg.json"), b"{}")
            .await
            .unwrap();

        vfs.mount("/", mem);
        Arc::new(vfs)
    }

    #[tokio::test]
    async fn test_walk_all_files() {
        let vfs = make_test_fs().await;
        let backend = crate::LocalBackend::new(vfs);

        let walker = FileWalker::new(&backend, "/").with_options(WalkOptions {
            respect_gitignore: false,
            include_hidden: true,
            ..Default::default()
        });

        let files = walker.collect().await.unwrap();

        assert!(files.iter().any(|p| p.ends_with("main.rs")));
        assert!(files.iter().any(|p| p.ends_with("lib.rs")));
        assert!(files.iter().any(|p| p.ends_with("README.md")));
        assert!(files.iter().any(|p| p.ends_with(".hidden")));
    }

    #[tokio::test]
    async fn test_walk_with_pattern() {
        let vfs = make_test_fs().await;
        let backend = crate::LocalBackend::new(vfs);

        let walker = FileWalker::new(&backend, "/")
            .with_pattern(GlobPath::new("**/*.rs").unwrap())
            .with_options(WalkOptions {
                respect_gitignore: false,
                ..Default::default()
            });

        let files = walker.collect().await.unwrap();

        assert!(files.iter().any(|p| p.ends_with("main.rs")));
        assert!(files.iter().any(|p| p.ends_with("lib.rs")));
        assert!(files.iter().any(|p| p.ends_with("utils.rs")));
        assert!(!files.iter().any(|p| p.ends_with("README.md")));
    }

    #[tokio::test]
    async fn test_walk_respects_gitignore() {
        let vfs = make_test_fs().await;
        let backend = crate::LocalBackend::new(vfs);

        let walker = FileWalker::new(&backend, "/").with_options(WalkOptions {
            respect_gitignore: true,
            ..Default::default()
        });

        let files = walker.collect().await.unwrap();

        // .git and node_modules should be ignored
        assert!(!files
            .iter()
            .any(|p| p.to_string_lossy().contains(".git")));
        assert!(!files
            .iter()
            .any(|p| p.to_string_lossy().contains("node_modules")));

        // Regular files should be present
        assert!(files.iter().any(|p| p.ends_with("main.rs")));
    }

    #[tokio::test]
    async fn test_walk_hides_dotfiles() {
        let vfs = make_test_fs().await;
        let backend = crate::LocalBackend::new(vfs);

        let walker = FileWalker::new(&backend, "/").with_options(WalkOptions {
            include_hidden: false,
            respect_gitignore: false,
            ..Default::default()
        });

        let files = walker.collect().await.unwrap();

        assert!(!files.iter().any(|p| p.ends_with(".hidden")));
        assert!(files.iter().any(|p| p.ends_with("main.rs")));
    }

    #[tokio::test]
    async fn test_walk_max_depth() {
        let vfs = make_test_fs().await;
        let backend = crate::LocalBackend::new(vfs);

        let walker = FileWalker::new(&backend, "/").with_options(WalkOptions {
            max_depth: Some(1),
            respect_gitignore: false,
            include_hidden: true,
            ..Default::default()
        });

        let files = walker.collect().await.unwrap();

        // Files at depth 1 (directly under /)
        assert!(files.iter().any(|p| p.ends_with("README.md")));
        // Files at depth 2 (under /src)
        assert!(files.iter().any(|p| p.ends_with("main.rs")));
        // Files at depth 3 (under /src/lib) should NOT be present
        assert!(!files.iter().any(|p| p.ends_with("utils.rs")));
    }

    #[tokio::test]
    async fn test_walk_directories() {
        let vfs = make_test_fs().await;
        let backend = crate::LocalBackend::new(vfs);

        let walker = FileWalker::new(&backend, "/").with_options(WalkOptions {
            entry_types: EntryTypes::dirs_only(),
            respect_gitignore: false,
            ..Default::default()
        });

        let dirs = walker.collect().await.unwrap();

        assert!(dirs.iter().any(|p| p.ends_with("src")));
        assert!(dirs.iter().any(|p| p.ends_with("lib")));
        assert!(!dirs.iter().any(|p| p.ends_with("main.rs")));
    }

    #[tokio::test]
    async fn test_walk_with_filter() {
        let vfs = make_test_fs().await;
        let backend = crate::LocalBackend::new(vfs);

        let mut filter = IncludeExclude::new();
        filter.exclude("*_test.rs");

        let walker = FileWalker::new(&backend, "/")
            .with_pattern(GlobPath::new("**/*.rs").unwrap())
            .with_options(WalkOptions {
                filter,
                respect_gitignore: false,
                ..Default::default()
            });

        let files = walker.collect().await.unwrap();

        assert!(files.iter().any(|p| p.ends_with("main.rs")));
        assert!(!files.iter().any(|p| p.ends_with("main_test.rs")));
    }
}
