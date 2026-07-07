//! Core async file walker, generic over `WalkerFs`.
//!
//! Provides recursive directory traversal with filtering support.

use std::collections::HashSet;
use std::fmt;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::{WalkerDirEntry, WalkerError, WalkerFs};
use crate::glob_path::GlobPath;
use crate::ignore::IgnoreFilter;
use crate::filter::IncludeExclude;

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

/// Callback invoked when a non-fatal error occurs during walking.
///
/// Receives the path where the error occurred and the error itself.
/// This allows callers to log or collect errors without aborting the walk.
pub type ErrorCallback = Arc<dyn Fn(&Path, &WalkerError) + Send + Sync>;

/// Options for file walking.
pub struct WalkOptions {
    /// Maximum depth to recurse (None = unlimited).
    pub max_depth: Option<usize>,
    /// Suppress yielding entries whose containing directory is at depth less
    /// than this. Descent is unaffected — deeper entries are still found.
    /// `None` and `Some(0)` are equivalent (yield everything).
    pub min_depth: Option<usize>,
    /// Skip files whose size exceeds this many bytes. Files for which the
    /// underlying `WalkerFs::file_size` returns `None` (size unknown) are
    /// always yielded regardless of the limit.
    pub max_filesize: Option<u64>,
    /// Types of entries to include.
    pub entry_types: EntryTypes,
    /// Respect .gitignore files and default ignores.
    pub respect_gitignore: bool,
    /// Include hidden files (starting with .).
    pub include_hidden: bool,
    /// Include/exclude filters.
    pub filter: IncludeExclude,
    /// Follow symbolic links into directories (default `false`).
    /// When false, symlink directories are yielded as files rather than recursed.
    /// When true, cycle detection prevents infinite loops.
    pub follow_symlinks: bool,
    /// Optional callback for non-fatal errors (unreadable dirs, bad .gitignore).
    /// Default `None` silently skips errors (preserving original behavior).
    pub on_error: Option<ErrorCallback>,
    /// File-type filter using ripgrep's `ignore::types::Types`.
    /// Builds e.g. with `TypesBuilder::new().add_defaults().select("rust")`.
    /// Pure path-name matching — no I/O.
    pub types: Option<Arc<ignore::types::Types>>,
}

impl fmt::Debug for WalkOptions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("WalkOptions")
            .field("max_depth", &self.max_depth)
            .field("min_depth", &self.min_depth)
            .field("max_filesize", &self.max_filesize)
            .field("entry_types", &self.entry_types)
            .field("respect_gitignore", &self.respect_gitignore)
            .field("include_hidden", &self.include_hidden)
            .field("filter", &self.filter)
            .field("follow_symlinks", &self.follow_symlinks)
            .field("on_error", &self.on_error.as_ref().map(|_| "..."))
            .field("types", &self.types.as_ref().map(|_| "..."))
            .finish()
    }
}

impl Clone for WalkOptions {
    fn clone(&self) -> Self {
        Self {
            max_depth: self.max_depth,
            min_depth: self.min_depth,
            max_filesize: self.max_filesize,
            entry_types: self.entry_types,
            respect_gitignore: self.respect_gitignore,
            include_hidden: self.include_hidden,
            filter: self.filter.clone(),
            follow_symlinks: self.follow_symlinks,
            on_error: self.on_error.clone(),
            types: self.types.clone(),
        }
    }
}

impl Default for WalkOptions {
    fn default() -> Self {
        Self {
            max_depth: None,
            min_depth: None,
            max_filesize: None,
            entry_types: EntryTypes::files_only(),
            respect_gitignore: true,
            include_hidden: false,
            filter: IncludeExclude::new(),
            follow_symlinks: false,
            on_error: None,
            types: None,
        }
    }
}

/// Async file walker, generic over any `WalkerFs` implementation.
///
/// # Examples
/// ```ignore
/// use kaish_glob::{FileWalker, WalkOptions, GlobPath};
///
/// let walker = FileWalker::new(&my_fs, "src")
///     .with_pattern(GlobPath::new("**/*.rs").unwrap())
///     .with_options(WalkOptions::default());
///
/// let files = walker.collect().await?;
/// ```
pub struct FileWalker<'a, F: WalkerFs> {
    fs: &'a F,
    root: PathBuf,
    pattern: Option<GlobPath>,
    options: WalkOptions,
    ignore_filter: Option<IgnoreFilter>,
}

impl<'a, F: WalkerFs> FileWalker<'a, F> {
    /// Create a new file walker starting at the given root.
    pub fn new(fs: &'a F, root: impl AsRef<Path>) -> Self {
        Self {
            fs,
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
    pub async fn collect(mut self) -> Result<Vec<PathBuf>, crate::WalkerError> {
        // Set up base ignore filter
        let base_filter = if self.options.respect_gitignore {
            let mut filter = self
                .ignore_filter
                .take()
                .unwrap_or_else(IgnoreFilter::with_defaults);

            // Try to load .gitignore from root
            let gitignore_path = self.root.join(".gitignore");
            if self.fs.exists(&gitignore_path).await {
                match IgnoreFilter::from_gitignore(&gitignore_path, self.fs).await {
                    Ok(gitignore) => filter.merge(&gitignore),
                    Err(err) => {
                        if let Some(ref cb) = self.options.on_error {
                            cb(&gitignore_path, &err);
                        }
                    }
                }
            }
            Some(filter)
        } else {
            self.ignore_filter.take()
        };

        let mut results = Vec::new();
        // Track visited directories for symlink cycle detection (only when following symlinks)
        let mut visited_dirs: HashSet<PathBuf> = HashSet::new();
        if self.options.follow_symlinks {
            visited_dirs.insert(self.root.clone());
        }
        // Stack carries: (directory, depth, ignore_filter for this dir)
        let mut stack = vec![(self.root.clone(), 0usize, base_filter.clone())];

        while let Some((dir, depth, current_filter)) = stack.pop() {
            // Check max depth
            if let Some(max) = self.options.max_depth
                && depth > max {
                    continue;
                }

            // List directory contents
            let entries = match self.fs.list_dir(&dir).await {
                Ok(entries) => entries,
                Err(err) => {
                    if let Some(ref cb) = self.options.on_error {
                        cb(&dir, &err);
                    }
                    continue;
                }
            };

            // Sort entries by name for deterministic traversal order
            let mut entries: Vec<_> = entries
                .into_iter()
                .map(|e| {
                    let name = e.name().to_string();
                    let is_dir = e.is_dir();
                    let is_symlink = e.is_symlink();
                    (name, is_dir, is_symlink)
                })
                .collect();
            entries.sort_by(|a, b| a.0.cmp(&b.0));

            // Collect directories to push in reverse order so alphabetically-first
            // directories are popped first from the LIFO stack.
            let mut dirs_to_push = Vec::new();

            for (entry_name, entry_is_dir, entry_is_symlink) in entries {
                let full_path = dir.join(&entry_name);

                // Hidden-file rule (bash, no `dotglob`). With a glob pattern the
                // leading-dot decision is made per-component by `matches_pattern`
                // (yield) and `could_descend` (traversal) below: `*` skips
                // dotfiles while `.*`/`.github`/`**/.env` reach them. With no
                // pattern — a plain recursive walk — hide dot entries unless
                // `include_hidden`.
                if !self.options.include_hidden
                    && self.pattern.is_none()
                    && entry_name.starts_with('.')
                {
                    continue;
                }

                // Check ignore filter
                if let Some(ref filter) = current_filter {
                    let relative = self.relative_path(&full_path);
                    if filter.is_ignored(&relative, entry_is_dir) {
                        continue;
                    }
                }

                // Check type filter (-tjs / -Trust style filename matching).
                // `Types::matched` returns Match::None for directories, so dirs
                // always pass through and we can still recurse into them.
                if let Some(ref types) = self.options.types
                    && types.matched(&full_path, entry_is_dir).is_ignore() {
                        continue;
                    }

                // Check include/exclude filter. Both the relative path and the
                // bare filename are offered (patterns like "*_test.rs" are
                // written against filenames); a directory is only pruned by an
                // explicit exclude — an include list must not stop traversal.
                if !self.options.filter.is_empty() {
                    let relative = self.relative_path(&full_path);
                    let name = full_path.file_name().map(Path::new);
                    if self
                        .options
                        .filter
                        .excludes_entry(&relative, name, entry_is_dir)
                    {
                        continue;
                    }
                }

                if entry_is_dir {
                    // Symlink directory handling
                    if entry_is_symlink && !self.options.follow_symlinks {
                        // Don't recurse into symlink dirs — yield as a file entry
                        if self.options.entry_types.files
                            && self.matches_pattern(&full_path)
                            && self.depth_yields(depth)
                            && self.size_within_limit(self.fs, &full_path).await
                        {
                            results.push(full_path);
                        }
                        continue;
                    }

                    // Cycle detection when following symlinks
                    if entry_is_symlink && self.options.follow_symlinks {
                        let canonical = self.fs.canonicalize(&full_path).await;
                        if !visited_dirs.insert(canonical) {
                            // Already visited this real directory — symlink cycle
                            if let Some(ref cb) = self.options.on_error {
                                cb(
                                    &full_path,
                                    &WalkerError::SymlinkCycle(full_path.display().to_string()),
                                );
                            }
                            continue;
                        }
                    }

                    // Check for nested .gitignore in this directory
                    let child_filter = if self.options.respect_gitignore {
                        let gitignore_path = full_path.join(".gitignore");
                        if self.fs.exists(&gitignore_path).await {
                            match IgnoreFilter::from_gitignore(&gitignore_path, self.fs).await {
                                Ok(nested_gitignore) => {
                                    // Merge with parent filter
                                    current_filter
                                        .as_ref()
                                        .map(|f| f.merged_with(&nested_gitignore))
                                        .or(Some(nested_gitignore))
                                }
                                Err(err) => {
                                    if let Some(ref cb) = self.options.on_error {
                                        cb(&gitignore_path, &err);
                                    }
                                    current_filter.clone()
                                }
                            }
                        } else {
                            current_filter.clone()
                        }
                    } else {
                        current_filter.clone()
                    };

                    // Only recurse if some entry beneath this directory could
                    // still match. `could_descend` honours the leading-dot rule,
                    // so `**` enters visible dirs but not hidden ones (without
                    // dotglob), while an explicitly named `.github` is entered.
                    let should_recurse = match &self.pattern {
                        None => true,
                        Some(pat) => {
                            let relative = self.relative_path(&full_path);
                            pat.could_descend(&relative, self.options.include_hidden)
                        }
                    };

                    if should_recurse {
                        dirs_to_push.push((full_path.clone(), depth + 1, child_filter));
                    }

                    // Yield directory if wanted
                    if self.options.entry_types.dirs
                        && self.matches_pattern(&full_path)
                        && self.depth_yields(depth)
                    {
                        results.push(full_path);
                    }
                } else {
                    // Yield file if wanted
                    if self.options.entry_types.files
                        && self.matches_pattern(&full_path)
                        && self.depth_yields(depth)
                        && self.size_within_limit(self.fs, &full_path).await
                    {
                        results.push(full_path);
                    }
                }
            }

            // Push directories in reverse order so alphabetically-first dirs
            // are popped first from the LIFO stack.
            dirs_to_push.reverse();
            stack.extend(dirs_to_push);
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
                pattern.matches_walk(&relative, self.options.include_hidden)
            }
            None => true,
        }
    }

    /// Whether an entry at the given containing-directory depth should be
    /// yielded under the current `min_depth` setting.
    fn depth_yields(&self, depth: usize) -> bool {
        match self.options.min_depth {
            None | Some(0) => true,
            Some(min) => depth >= min,
        }
    }

    /// Whether a file at `path` is within the configured `max_filesize`.
    /// Files whose size cannot be determined (`file_size` returns `None`)
    /// are always considered within the limit.
    async fn size_within_limit(&self, fs: &F, path: &Path) -> bool {
        let Some(limit) = self.options.max_filesize else {
            return true;
        };
        match fs.file_size(path).await {
            Some(size) => size <= limit,
            None => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{WalkerDirEntry, WalkerError, WalkerFs};
    use std::collections::HashMap;
    use std::sync::Arc;
    use tokio::sync::RwLock;

    /// Simple in-memory dir entry for testing.
    struct MemEntry {
        name: String,
        is_dir: bool,
        is_symlink: bool,
    }

    impl WalkerDirEntry for MemEntry {
        fn name(&self) -> &str { &self.name }
        fn is_dir(&self) -> bool { self.is_dir }
        fn is_file(&self) -> bool { !self.is_dir }
        fn is_symlink(&self) -> bool { self.is_symlink }
    }

    /// In-memory filesystem for testing the walker.
    ///
    /// Supports files, directories, and symbolic links (directory symlinks).
    struct MemoryFs {
        files: Arc<RwLock<HashMap<PathBuf, Vec<u8>>>>,
        dirs: Arc<RwLock<std::collections::HashSet<PathBuf>>>,
        /// Symlink path → target path (for directory symlinks)
        symlinks: Arc<RwLock<HashMap<PathBuf, PathBuf>>>,
    }

    impl MemoryFs {
        fn new() -> Self {
            let mut dirs = std::collections::HashSet::new();
            dirs.insert(PathBuf::from("/"));
            Self {
                files: Arc::new(RwLock::new(HashMap::new())),
                dirs: Arc::new(RwLock::new(dirs)),
                symlinks: Arc::new(RwLock::new(HashMap::new())),
            }
        }

        async fn add_file(&self, path: &str, content: &[u8]) {
            let path = PathBuf::from(path);
            // Ensure parent dirs exist
            if let Some(parent) = path.parent() {
                self.ensure_dirs(parent).await;
            }
            self.files.write().await.insert(path, content.to_vec());
        }

        async fn add_dir(&self, path: &str) {
            self.ensure_dirs(&PathBuf::from(path)).await;
        }

        /// Add a directory symlink: `link` points to `target`.
        /// The symlink appears as a directory entry and is listed under its parent.
        async fn add_dir_symlink(&self, link: &str, target: &str) {
            let link_path = PathBuf::from(link);
            let target_path = PathBuf::from(target);
            // Ensure parent of link exists
            if let Some(parent) = link_path.parent() {
                self.ensure_dirs(parent).await;
            }
            // Register as a directory so it appears in listings
            self.dirs.write().await.insert(link_path.clone());
            self.symlinks.write().await.insert(link_path, target_path);
        }

        /// Resolve symlinks in a path by checking each prefix component.
        /// This mimics how a real filesystem resolves intermediate symlinks.
        fn resolve_path(path: &Path, symlinks: &HashMap<PathBuf, PathBuf>) -> PathBuf {
            let mut resolved = PathBuf::new();
            for component in path.components() {
                resolved.push(component);
                // Check if the current prefix is a symlink and resolve it
                if let Some(target) = symlinks.get(&resolved) {
                    resolved = target.clone();
                }
            }
            resolved
        }

        async fn ensure_dirs(&self, path: &Path) {
            let mut dirs = self.dirs.write().await;
            let mut current = PathBuf::new();
            for component in path.components() {
                current.push(component);
                dirs.insert(current.clone());
            }
        }
    }

    #[async_trait::async_trait]
    impl WalkerFs for MemoryFs {
        type DirEntry = MemEntry;

        async fn list_dir(&self, path: &Path) -> Result<Vec<MemEntry>, WalkerError> {
            let symlinks = self.symlinks.read().await;

            // Resolve symlinks in the path: check each prefix to see if it's a symlink
            let resolved = Self::resolve_path(path, &symlinks);

            let files = self.files.read().await;
            let dirs = self.dirs.read().await;

            let mut entries = Vec::new();
            let mut seen = std::collections::HashSet::new();

            // Find files directly under this dir
            for file_path in files.keys() {
                if let Some(parent) = file_path.parent() {
                    if parent == resolved {
                        if let Some(name) = file_path.file_name() {
                            let name_str = name.to_string_lossy().to_string();
                            if seen.insert(name_str.clone()) {
                                entries.push(MemEntry {
                                    name: name_str,
                                    is_dir: false,
                                    is_symlink: false,
                                });
                            }
                        }
                    }
                }
            }

            // Find subdirs directly under this dir
            for dir_path in dirs.iter() {
                if let Some(parent) = dir_path.parent() {
                    if parent == resolved && dir_path != &resolved {
                        if let Some(name) = dir_path.file_name() {
                            let name_str = name.to_string_lossy().to_string();
                            if seen.insert(name_str.clone()) {
                                let is_symlink = symlinks.contains_key(dir_path);
                                entries.push(MemEntry {
                                    name: name_str,
                                    is_dir: true,
                                    is_symlink,
                                });
                            }
                        }
                    }
                }
            }

            Ok(entries)
        }

        async fn read_file(&self, path: &Path) -> Result<Vec<u8>, WalkerError> {
            let files = self.files.read().await;
            files.get(path)
                .cloned()
                .ok_or_else(|| WalkerError::NotFound(path.display().to_string()))
        }

        async fn is_dir(&self, path: &Path) -> bool {
            self.dirs.read().await.contains(path)
        }

        async fn exists(&self, path: &Path) -> bool {
            self.files.read().await.contains_key(path)
                || self.dirs.read().await.contains(path)
        }

        async fn canonicalize(&self, path: &Path) -> PathBuf {
            let symlinks = self.symlinks.read().await;
            Self::resolve_path(path, &symlinks)
        }
    }

    async fn make_test_fs() -> MemoryFs {
        let fs = MemoryFs::new();

        fs.add_dir("/src").await;
        fs.add_dir("/src/lib").await;
        fs.add_dir("/test").await;
        fs.add_dir("/.git").await;
        fs.add_dir("/node_modules").await;

        fs.add_file("/src/main.rs", b"fn main() {}").await;
        fs.add_file("/src/lib.rs", b"pub mod lib;").await;
        fs.add_file("/src/lib/utils.rs", b"pub fn util() {}").await;
        fs.add_file("/test/main_test.rs", b"#[test]").await;
        fs.add_file("/README.md", b"# Test").await;
        fs.add_file("/.hidden", b"secret").await;
        fs.add_file("/.git/config", b"[core]").await;
        fs.add_file("/node_modules/pkg.json", b"{}").await;

        fs
    }

    #[tokio::test]
    async fn test_walk_all_files() {
        let fs = make_test_fs().await;

        let walker = FileWalker::new(&fs, "/").with_options(WalkOptions {
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
        let fs = make_test_fs().await;

        let walker = FileWalker::new(&fs, "/")
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
        let fs = make_test_fs().await;

        let walker = FileWalker::new(&fs, "/").with_options(WalkOptions {
            respect_gitignore: true,
            ..Default::default()
        });

        let files = walker.collect().await.unwrap();

        assert!(!files
            .iter()
            .any(|p| p.to_string_lossy().contains(".git")));
        assert!(!files
            .iter()
            .any(|p| p.to_string_lossy().contains("node_modules")));

        assert!(files.iter().any(|p| p.ends_with("main.rs")));
    }

    #[tokio::test]
    async fn test_walk_hides_dotfiles() {
        let fs = make_test_fs().await;

        let walker = FileWalker::new(&fs, "/").with_options(WalkOptions {
            include_hidden: false,
            respect_gitignore: false,
            ..Default::default()
        });

        let files = walker.collect().await.unwrap();

        assert!(!files.iter().any(|p| p.ends_with(".hidden")));
        assert!(files.iter().any(|p| p.ends_with("main.rs")));
    }

    #[tokio::test]
    async fn test_dot_pattern_matches_dotfiles() {
        // `.*` explicitly names a leading dot, so it matches dotfiles (bash).
        let fs = MemoryFs::new();
        fs.add_file("/.gitignore", b"x").await;
        fs.add_file("/.env", b"x").await;
        fs.add_file("/visible.txt", b"x").await;

        let walker = FileWalker::new(&fs, "/")
            .with_pattern(GlobPath::new(".*").unwrap())
            .with_options(WalkOptions {
                respect_gitignore: false,
                ..Default::default()
            });
        let files = walker.collect().await.unwrap();

        assert!(files.iter().any(|p| p.ends_with(".gitignore")));
        assert!(files.iter().any(|p| p.ends_with(".env")));
        assert!(!files.iter().any(|p| p.ends_with("visible.txt")));
    }

    #[tokio::test]
    async fn test_star_skips_dotfiles() {
        // A bare `*` never matches a leading dot without dotglob.
        let fs = MemoryFs::new();
        fs.add_file("/.env", b"x").await;
        fs.add_file("/visible.txt", b"x").await;

        let walker = FileWalker::new(&fs, "/")
            .with_pattern(GlobPath::new("*").unwrap())
            .with_options(WalkOptions {
                respect_gitignore: false,
                entry_types: EntryTypes::all(),
                ..Default::default()
            });
        let files = walker.collect().await.unwrap();

        assert!(!files.iter().any(|p| p.ends_with(".env")));
        assert!(files.iter().any(|p| p.ends_with("visible.txt")));
    }

    #[tokio::test]
    async fn test_literal_dotdir_is_traversed() {
        // An explicitly named `.github` directory is descended into.
        let fs = MemoryFs::new();
        fs.add_file("/.github/workflows/ci.yml", b"x").await;
        fs.add_file("/.github/.secret", b"x").await;

        let walker = FileWalker::new(&fs, "/")
            .with_pattern(GlobPath::new(".github/**/*.yml").unwrap())
            .with_options(WalkOptions {
                respect_gitignore: false,
                ..Default::default()
            });
        let files = walker.collect().await.unwrap();

        assert!(files.iter().any(|p| p.ends_with("ci.yml")));
    }

    #[tokio::test]
    async fn test_dotdir_star_excludes_nested_dotfiles() {
        // `.github/*` reaches into the named dot dir, but `*` still skips the
        // dot-prefixed children inside it.
        let fs = MemoryFs::new();
        fs.add_file("/.github/config.yml", b"x").await;
        fs.add_file("/.github/.secret", b"x").await;

        let walker = FileWalker::new(&fs, "/")
            .with_pattern(GlobPath::new(".github/*").unwrap())
            .with_options(WalkOptions {
                respect_gitignore: false,
                entry_types: EntryTypes::all(),
                ..Default::default()
            });
        let files = walker.collect().await.unwrap();

        assert!(files.iter().any(|p| p.ends_with("config.yml")));
        assert!(!files.iter().any(|p| p.ends_with(".secret")));
    }

    #[tokio::test]
    async fn test_globstar_skips_dotdirs_without_dotglob() {
        // `**` does not descend into hidden directories without dotglob.
        let fs = MemoryFs::new();
        fs.add_file("/.github/buried.rs", b"x").await;
        fs.add_file("/top.rs", b"x").await;

        let walker = FileWalker::new(&fs, "/")
            .with_pattern(GlobPath::new("**/*.rs").unwrap())
            .with_options(WalkOptions {
                respect_gitignore: false,
                ..Default::default()
            });
        let files = walker.collect().await.unwrap();

        assert!(files.iter().any(|p| p.ends_with("top.rs")));
        assert!(!files.iter().any(|p| p.ends_with("buried.rs")));
    }

    #[tokio::test]
    async fn test_globstar_then_explicit_dotfile() {
        // `**/.env` reaches a dotfile at the root and inside visible dirs, but
        // not inside a hidden dir (which `**` cannot traverse without dotglob).
        let fs = MemoryFs::new();
        fs.add_file("/.env", b"x").await;
        fs.add_file("/sub/.env", b"x").await;
        fs.add_file("/.hidden/.env", b"x").await;
        fs.add_file("/sub/visible.txt", b"x").await;

        let walker = FileWalker::new(&fs, "/")
            .with_pattern(GlobPath::new("**/.env").unwrap())
            .with_options(WalkOptions {
                respect_gitignore: false,
                ..Default::default()
            });
        let files = walker.collect().await.unwrap();

        assert_eq!(files.iter().filter(|p| p.ends_with(".env")).count(), 2, "{files:?}");
        assert!(files.iter().any(|p| p == &PathBuf::from("/.env")));
        assert!(files.iter().any(|p| p == &PathBuf::from("/sub/.env")));
        assert!(!files.iter().any(|p| p.starts_with("/.hidden")));
    }

    #[tokio::test]
    async fn test_globstar_then_explicit_dotdir() {
        // `**/.github/*.yml` enters the named dot dir at any depth.
        let fs = MemoryFs::new();
        fs.add_file("/.github/ci.yml", b"x").await;
        fs.add_file("/sub/.github/release.yml", b"x").await;

        let walker = FileWalker::new(&fs, "/")
            .with_pattern(GlobPath::new("**/.github/*.yml").unwrap())
            .with_options(WalkOptions {
                respect_gitignore: false,
                ..Default::default()
            });
        let files = walker.collect().await.unwrap();

        assert!(files.iter().any(|p| p.ends_with("ci.yml")), "{files:?}");
        assert!(files.iter().any(|p| p.ends_with("release.yml")), "{files:?}");
    }

    #[tokio::test]
    async fn test_include_hidden_acts_like_dotglob() {
        // include_hidden == dotglob: `**` then reaches hidden directories.
        let fs = MemoryFs::new();
        fs.add_file("/.github/buried.rs", b"x").await;

        let walker = FileWalker::new(&fs, "/")
            .with_pattern(GlobPath::new("**/*.rs").unwrap())
            .with_options(WalkOptions {
                respect_gitignore: false,
                include_hidden: true,
                ..Default::default()
            });
        let files = walker.collect().await.unwrap();

        assert!(files.iter().any(|p| p.ends_with("buried.rs")));
    }

    #[tokio::test]
    async fn test_walk_max_depth() {
        let fs = make_test_fs().await;

        let walker = FileWalker::new(&fs, "/").with_options(WalkOptions {
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
        let fs = make_test_fs().await;

        let walker = FileWalker::new(&fs, "/").with_options(WalkOptions {
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
        let fs = make_test_fs().await;

        let mut filter = IncludeExclude::new();
        filter.exclude("*_test.rs");

        let walker = FileWalker::new(&fs, "/")
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

    #[tokio::test]
    async fn test_walk_nested_gitignore() {
        let fs = MemoryFs::new();

        fs.add_dir("/src").await;
        fs.add_dir("/src/subdir").await;
        fs.add_file("/root.rs", b"root").await;
        fs.add_file("/src/main.rs", b"main").await;
        fs.add_file("/src/ignored.log", b"log").await;
        fs.add_file("/src/subdir/util.rs", b"util").await;
        fs.add_file("/src/subdir/local_ignore.txt", b"ignored").await;

        fs.add_file("/.gitignore", b"*.log").await;
        fs.add_file("/src/subdir/.gitignore", b"*.txt").await;

        let walker = FileWalker::new(&fs, "/")
            .with_options(WalkOptions {
                respect_gitignore: true,
                include_hidden: true,
                ..Default::default()
            });

        let files = walker.collect().await.unwrap();

        assert!(files.iter().any(|p| p.ends_with("root.rs")));
        assert!(files.iter().any(|p| p.ends_with("main.rs")));
        assert!(files.iter().any(|p| p.ends_with("util.rs")));

        assert!(!files.iter().any(|p| p.ends_with("ignored.log")));
        assert!(!files.iter().any(|p| p.ends_with("local_ignore.txt")));
    }

    /// FS that reports a stub file size for every file.
    /// Used for max_filesize tests.
    struct SizedFs {
        inner: MemoryFs,
        sizes: HashMap<PathBuf, u64>,
    }

    #[async_trait::async_trait]
    impl WalkerFs for SizedFs {
        type DirEntry = MemEntry;
        async fn list_dir(&self, path: &Path) -> Result<Vec<MemEntry>, WalkerError> {
            self.inner.list_dir(path).await
        }
        async fn read_file(&self, path: &Path) -> Result<Vec<u8>, WalkerError> {
            self.inner.read_file(path).await
        }
        async fn is_dir(&self, path: &Path) -> bool { self.inner.is_dir(path).await }
        async fn exists(&self, path: &Path) -> bool { self.inner.exists(path).await }
        async fn file_size(&self, path: &Path) -> Option<u64> {
            self.sizes.get(path).copied()
        }
    }

    #[tokio::test]
    async fn test_walk_max_filesize_skips_large_files() {
        let inner = MemoryFs::new();
        inner.add_file("/small.txt", b"tiny").await;
        inner.add_file("/big.bin", b"larger payload").await;
        let mut sizes = HashMap::new();
        sizes.insert(PathBuf::from("/small.txt"), 1_024); // 1 KB
        sizes.insert(PathBuf::from("/big.bin"), 2 * 1_048_576); // 2 MB
        let fs = SizedFs { inner, sizes };

        let walker = FileWalker::new(&fs, "/").with_options(WalkOptions {
            respect_gitignore: false,
            max_filesize: Some(1_048_576), // 1 MB cap
            ..Default::default()
        });

        let files = walker.collect().await.unwrap();

        assert!(files.iter().any(|p| p.ends_with("small.txt")));
        assert!(!files.iter().any(|p| p.ends_with("big.bin")));
    }

    #[tokio::test]
    async fn test_walk_max_filesize_unknown_size_yields() {
        // file_size returning None means "unknown" — must NOT be skipped.
        let fs = MemoryFs::new();
        fs.add_file("/unknown.txt", b"x").await;

        let walker = FileWalker::new(&fs, "/").with_options(WalkOptions {
            respect_gitignore: false,
            max_filesize: Some(0), // even with zero cap, unknown sizes pass
            ..Default::default()
        });

        let files = walker.collect().await.unwrap();
        assert!(files.iter().any(|p| p.ends_with("unknown.txt")));
    }

    #[tokio::test]
    async fn test_walk_min_depth_skips_root_files() {
        let fs = MemoryFs::new();
        fs.add_file("/at_root.txt", b"r").await;
        fs.add_dir("/sub").await;
        fs.add_file("/sub/nested.txt", b"n").await;
        fs.add_dir("/sub/deeper").await;
        fs.add_file("/sub/deeper/deep.txt", b"d").await;

        let walker = FileWalker::new(&fs, "/").with_options(WalkOptions {
            respect_gitignore: false,
            min_depth: Some(1), // skip yields when containing dir is at depth < 1
            ..Default::default()
        });

        let files = walker.collect().await.unwrap();

        // /at_root.txt is at depth 0 (containing dir = root, depth 0) — skipped.
        assert!(!files.iter().any(|p| p.ends_with("at_root.txt")));
        // /sub/nested.txt is at depth 1 — yielded.
        assert!(files.iter().any(|p| p.ends_with("nested.txt")));
        // /sub/deeper/deep.txt is at depth 2 — yielded.
        assert!(files.iter().any(|p| p.ends_with("deep.txt")));
    }

    #[tokio::test]
    async fn test_walk_types_select_only_rust() {
        let fs = MemoryFs::new();
        fs.add_file("/src/main.rs", b"r").await;
        fs.add_file("/src/main.py", b"p").await;
        fs.add_file("/src/main.js", b"j").await;
        fs.add_file("/README.md", b"m").await;

        let mut tb = ignore::types::TypesBuilder::new();
        tb.add_defaults();
        tb.select("rust");
        let types = std::sync::Arc::new(tb.build().expect("types build"));

        let walker = FileWalker::new(&fs, "/").with_options(WalkOptions {
            respect_gitignore: false,
            types: Some(types),
            ..Default::default()
        });

        let files = walker.collect().await.unwrap();

        assert!(files.iter().any(|p| p.ends_with("main.rs")));
        assert!(!files.iter().any(|p| p.ends_with("main.py")));
        assert!(!files.iter().any(|p| p.ends_with("main.js")));
        assert!(!files.iter().any(|p| p.ends_with("README.md")));
    }

    #[tokio::test]
    async fn test_walk_types_negate_excludes() {
        let fs = MemoryFs::new();
        fs.add_file("/src/main.rs", b"r").await;
        fs.add_file("/src/main.py", b"p").await;
        fs.add_file("/README.md", b"m").await;

        let mut tb = ignore::types::TypesBuilder::new();
        tb.add_defaults();
        tb.negate("rust");
        let types = std::sync::Arc::new(tb.build().expect("types build"));

        let walker = FileWalker::new(&fs, "/").with_options(WalkOptions {
            respect_gitignore: false,
            types: Some(types),
            ..Default::default()
        });

        let files = walker.collect().await.unwrap();

        // Rust files excluded.
        assert!(!files.iter().any(|p| p.ends_with("main.rs")));
        // Other files yielded.
        assert!(files.iter().any(|p| p.ends_with("main.py")));
        assert!(files.iter().any(|p| p.ends_with("README.md")));
    }

    #[tokio::test]
    async fn test_walk_min_depth_still_descends() {
        // min_depth must NOT prevent descent — only suppress yields above the threshold.
        let fs = MemoryFs::new();
        fs.add_dir("/level1").await;
        fs.add_dir("/level1/level2").await;
        fs.add_file("/level1/level2/found.txt", b"f").await;

        let walker = FileWalker::new(&fs, "/").with_options(WalkOptions {
            respect_gitignore: false,
            min_depth: Some(2),
            ..Default::default()
        });

        let files = walker.collect().await.unwrap();
        assert!(files.iter().any(|p| p.ends_with("found.txt")));
    }

    #[tokio::test]
    async fn test_walk_error_callback() {
        use std::sync::Mutex;

        /// Filesystem that returns errors for specific directories.
        struct ErrorFs {
            inner: MemoryFs,
            error_paths: Vec<PathBuf>,
        }

        #[async_trait::async_trait]
        impl WalkerFs for ErrorFs {
            type DirEntry = MemEntry;

            async fn list_dir(&self, path: &Path) -> Result<Vec<MemEntry>, WalkerError> {
                if self.error_paths.iter().any(|p| p == path) {
                    return Err(WalkerError::PermissionDenied(path.display().to_string()));
                }
                self.inner.list_dir(path).await
            }

            async fn read_file(&self, path: &Path) -> Result<Vec<u8>, WalkerError> {
                self.inner.read_file(path).await
            }

            async fn is_dir(&self, path: &Path) -> bool {
                self.inner.is_dir(path).await
            }

            async fn exists(&self, path: &Path) -> bool {
                self.inner.exists(path).await
            }
        }

        let inner = MemoryFs::new();
        inner.add_dir("/readable").await;
        inner.add_dir("/forbidden").await;
        inner.add_file("/readable/ok.txt", b"ok").await;
        inner.add_file("/forbidden/secret.txt", b"secret").await;

        let fs = ErrorFs {
            inner,
            error_paths: vec![PathBuf::from("/forbidden")],
        };

        let errors: Arc<Mutex<Vec<(PathBuf, String)>>> = Arc::new(Mutex::new(Vec::new()));
        let errors_cb = errors.clone();

        let walker = FileWalker::new(&fs, "/").with_options(WalkOptions {
            respect_gitignore: false,
            include_hidden: true,
            on_error: Some(Arc::new(move |path, err| {
                errors_cb.lock().unwrap().push((path.to_path_buf(), err.to_string()));
            })),
            ..Default::default()
        });

        let files = walker.collect().await.unwrap();

        assert!(files.iter().any(|p| p.ends_with("ok.txt")));
        assert!(!files.iter().any(|p| p.ends_with("secret.txt")));

        let errors = errors.lock().unwrap();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].0, PathBuf::from("/forbidden"));
        assert!(errors[0].1.contains("permission denied"));
    }

    #[tokio::test]
    async fn test_walk_deterministic_order() {
        let fs = MemoryFs::new();

        // Add directories and files in non-alphabetical order
        fs.add_dir("/charlie").await;
        fs.add_dir("/alpha").await;
        fs.add_dir("/bravo").await;
        fs.add_file("/charlie/c.txt", b"c").await;
        fs.add_file("/alpha/a.txt", b"a").await;
        fs.add_file("/bravo/b.txt", b"b").await;

        let walker = FileWalker::new(&fs, "/").with_options(WalkOptions {
            respect_gitignore: false,
            ..Default::default()
        });

        let files = walker.collect().await.unwrap();

        // Results should be in alphabetical traversal order:
        // alpha/a.txt, bravo/b.txt, charlie/c.txt
        assert_eq!(files.len(), 3);
        assert!(files[0].ends_with("alpha/a.txt"));
        assert!(files[1].ends_with("bravo/b.txt"));
        assert!(files[2].ends_with("charlie/c.txt"));

        // Run again to verify determinism
        let walker2 = FileWalker::new(&fs, "/").with_options(WalkOptions {
            respect_gitignore: false,
            ..Default::default()
        });
        let files2 = walker2.collect().await.unwrap();
        assert_eq!(files, files2);
    }

    #[tokio::test]
    async fn test_symlinks_not_followed_by_default() {
        let fs = MemoryFs::new();

        fs.add_dir("/real").await;
        fs.add_file("/real/data.txt", b"data").await;
        // /link → /real (symlink directory)
        fs.add_dir_symlink("/link", "/real").await;

        let walker = FileWalker::new(&fs, "/").with_options(WalkOptions {
            respect_gitignore: false,
            // follow_symlinks defaults to false
            ..Default::default()
        });

        let files = walker.collect().await.unwrap();

        // /real/data.txt should be found
        assert!(files.iter().any(|p| p.ends_with("real/data.txt")));
        // /link should be yielded as a file entry (not recursed)
        assert!(files.iter().any(|p| p.ends_with("link")));
        // Should NOT find files under /link/ since we don't follow
        assert!(!files.iter().any(|p| p.to_string_lossy().contains("link/data")));
    }

    #[tokio::test]
    async fn test_symlinks_followed() {
        let fs = MemoryFs::new();

        fs.add_dir("/real").await;
        fs.add_file("/real/data.txt", b"data").await;
        // /link → /real
        fs.add_dir_symlink("/link", "/real").await;

        let walker = FileWalker::new(&fs, "/").with_options(WalkOptions {
            respect_gitignore: false,
            follow_symlinks: true,
            ..Default::default()
        });

        let files = walker.collect().await.unwrap();

        // Both the real path and symlinked path should have data.txt
        assert!(files.iter().any(|p| p.ends_with("real/data.txt")));
        assert!(files.iter().any(|p| p.ends_with("link/data.txt")));
    }

    #[tokio::test]
    async fn test_symlink_cycle_detection() {
        use std::sync::Mutex;

        let fs = MemoryFs::new();

        // Create a cycle: /a → /b, /b → /a
        fs.add_dir("/a").await;
        fs.add_dir("/b").await;
        fs.add_file("/a/file_a.txt", b"a").await;
        fs.add_file("/b/file_b.txt", b"b").await;
        // /a/link_to_b → /b, /b/link_to_a → /a
        fs.add_dir_symlink("/a/link_to_b", "/b").await;
        fs.add_dir_symlink("/b/link_to_a", "/a").await;

        let errors: Arc<Mutex<Vec<(PathBuf, String)>>> = Arc::new(Mutex::new(Vec::new()));
        let errors_cb = errors.clone();

        let walker = FileWalker::new(&fs, "/").with_options(WalkOptions {
            respect_gitignore: false,
            follow_symlinks: true,
            on_error: Some(Arc::new(move |path, err| {
                errors_cb.lock().unwrap().push((path.to_path_buf(), err.to_string()));
            })),
            ..Default::default()
        });

        let files = walker.collect().await.unwrap();

        // Real files should be found
        assert!(files.iter().any(|p| p.ends_with("file_a.txt")));
        assert!(files.iter().any(|p| p.ends_with("file_b.txt")));

        // Cycle should be detected and reported
        let errors = errors.lock().unwrap();
        assert!(
            errors.iter().any(|(_, msg)| msg.contains("symlink cycle")),
            "expected symlink cycle error, got: {errors:?}"
        );

        // Walk should terminate (not infinite loop) — the fact we got here proves it
    }
}
