//! Configurable ignore file policy for file-walking tools.
//!
//! Controls which gitignore-format files are loaded and how broadly
//! ignore rules apply. Per-mode defaults protect MCP agents from
//! context flooding while leaving REPL users unrestricted.

use std::path::Path;

use crate::walker::{IgnoreFilter, WalkerFs};

/// Controls which tools respect the ignore configuration.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IgnoreScope {
    /// Polite tools (glob, tree, grep, ls, expand_glob) respect config.
    /// `find` remains unrestricted — traditional POSIX behavior.
    Advisory,
    /// ALL file-walking tools respect config, including `find`.
    /// Protects agents from context flooding.
    Enforced,
}

/// Centralized ignore file configuration.
///
/// Threaded through `KernelConfig` → `ExecContext` → tools.
/// Runtime-mutable via the `ignore` builtin.
#[derive(Debug, Clone)]
pub struct IgnoreConfig {
    scope: IgnoreScope,
    ignore_files: Vec<String>,
    use_defaults: bool,
    auto_gitignore: bool,
}

impl IgnoreConfig {
    /// No filtering — REPL/embedded/test default.
    pub fn none() -> Self {
        Self {
            scope: IgnoreScope::Advisory,
            ignore_files: Vec::new(),
            use_defaults: false,
            auto_gitignore: false,
        }
    }

    /// MCP-safe defaults: enforced scope, .gitignore loaded, defaults on.
    pub fn mcp() -> Self {
        Self {
            scope: IgnoreScope::Enforced,
            ignore_files: vec![".gitignore".to_string()],
            use_defaults: true,
            auto_gitignore: true,
        }
    }

    /// Whether any filtering is configured.
    pub fn is_active(&self) -> bool {
        self.use_defaults || self.auto_gitignore || !self.ignore_files.is_empty()
    }

    pub fn scope(&self) -> IgnoreScope {
        self.scope
    }

    /// Whether the FileWalker should auto-load nested .gitignore files.
    pub fn auto_gitignore(&self) -> bool {
        self.auto_gitignore
    }

    pub fn use_defaults(&self) -> bool {
        self.use_defaults
    }

    pub fn files(&self) -> &[String] {
        &self.ignore_files
    }

    pub fn set_scope(&mut self, scope: IgnoreScope) {
        self.scope = scope;
    }

    pub fn set_defaults(&mut self, on: bool) {
        self.use_defaults = on;
    }

    pub fn set_auto_gitignore(&mut self, on: bool) {
        self.auto_gitignore = on;
    }

    pub fn add_file(&mut self, name: &str) {
        if !self.ignore_files.iter().any(|f| f == name) {
            self.ignore_files.push(name.to_string());
        }
    }

    pub fn remove_file(&mut self, name: &str) {
        self.ignore_files.retain(|f| f != name);
    }

    pub fn clear(&mut self) {
        self.ignore_files.clear();
        self.use_defaults = false;
        self.auto_gitignore = false;
    }

    /// Build an `IgnoreFilter` from the configured file list and defaults.
    ///
    /// Loads each ignore file relative to `root` via the given `WalkerFs`.
    /// Returns `None` if no filtering is configured.
    pub async fn build_filter<F: WalkerFs>(
        &self,
        root: &Path,
        fs: &F,
    ) -> Option<IgnoreFilter> {
        if !self.is_active() {
            return None;
        }

        let mut filter = if self.use_defaults {
            IgnoreFilter::with_defaults()
        } else {
            IgnoreFilter::new()
        };

        for filename in &self.ignore_files {
            let path = root.join(filename);
            if let Ok(file_filter) = IgnoreFilter::from_gitignore(&path, fs).await {
                filter.merge(&file_filter);
            }
            // Silently skip files that don't exist or can't be read
        }

        Some(filter)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_none_is_inactive() {
        let config = IgnoreConfig::none();
        assert!(!config.is_active());
        assert_eq!(config.scope(), IgnoreScope::Advisory);
        assert!(!config.auto_gitignore());
    }

    #[test]
    fn test_mcp_is_active() {
        let config = IgnoreConfig::mcp();
        assert!(config.is_active());
        assert_eq!(config.scope(), IgnoreScope::Enforced);
        assert!(config.auto_gitignore());
        assert!(config.use_defaults());
        assert_eq!(config.files(), &[".gitignore"]);
    }

    #[test]
    fn test_add_remove_files() {
        let mut config = IgnoreConfig::none();
        assert!(!config.is_active());

        config.add_file(".dockerignore");
        assert!(config.is_active());
        assert_eq!(config.files(), &[".dockerignore"]);

        // No duplicates
        config.add_file(".dockerignore");
        assert_eq!(config.files().len(), 1);

        config.remove_file(".dockerignore");
        assert!(config.files().is_empty());
    }

    #[test]
    fn test_clear() {
        let mut config = IgnoreConfig::mcp();
        config.clear();
        assert!(!config.is_active());
        assert!(config.files().is_empty());
        assert!(!config.use_defaults());
        assert!(!config.auto_gitignore());
    }

    #[test]
    fn test_set_scope() {
        let mut config = IgnoreConfig::none();
        config.set_scope(IgnoreScope::Enforced);
        assert_eq!(config.scope(), IgnoreScope::Enforced);
    }

    #[test]
    fn test_defaults_toggle() {
        let mut config = IgnoreConfig::none();
        config.set_defaults(true);
        assert!(config.is_active());
        config.set_defaults(false);
        assert!(!config.is_active());
    }

    #[test]
    fn test_auto_gitignore_alone_is_active() {
        let mut config = IgnoreConfig::none();
        assert!(!config.is_active());
        config.set_auto_gitignore(true);
        assert!(config.is_active());
    }

    mod async_tests {
        use super::*;
        use crate::walker::{WalkerDirEntry, WalkerError, WalkerFs};
        use std::collections::HashMap;
        use std::path::PathBuf;

        struct MemEntry;
        impl WalkerDirEntry for MemEntry {
            fn name(&self) -> &str { "" }
            fn is_dir(&self) -> bool { false }
            fn is_file(&self) -> bool { true }
            fn is_symlink(&self) -> bool { false }
        }

        struct FakeFs(HashMap<PathBuf, Vec<u8>>);

        #[async_trait::async_trait]
        impl WalkerFs for FakeFs {
            type DirEntry = MemEntry;
            async fn list_dir(&self, _: &Path) -> Result<Vec<MemEntry>, WalkerError> {
                Ok(vec![])
            }
            async fn read_file(&self, path: &Path) -> Result<Vec<u8>, WalkerError> {
                self.0.get(path)
                    .cloned()
                    .ok_or_else(|| WalkerError::NotFound(path.display().to_string()))
            }
            async fn is_dir(&self, _: &Path) -> bool { false }
            async fn exists(&self, path: &Path) -> bool { self.0.contains_key(path) }
        }

        #[tokio::test]
        async fn test_build_filter_none_returns_none() {
            let config = IgnoreConfig::none();
            let fs = FakeFs(HashMap::new());
            assert!(config.build_filter(Path::new("/"), &fs).await.is_none());
        }

        #[tokio::test]
        async fn test_build_filter_defaults_returns_some() {
            let mut config = IgnoreConfig::none();
            config.set_defaults(true);
            let fs = FakeFs(HashMap::new());

            let filter = config.build_filter(Path::new("/"), &fs).await;
            assert!(filter.is_some());
            let filter = filter.unwrap();
            // Default filter should ignore target/ and node_modules/
            assert!(filter.is_name_ignored("target", true));
            assert!(filter.is_name_ignored("node_modules", true));
            assert!(!filter.is_name_ignored("src", true));
        }

        #[tokio::test]
        async fn test_build_filter_loads_gitignore() {
            let mut config = IgnoreConfig::none();
            config.add_file(".gitignore");

            let mut files = HashMap::new();
            files.insert(PathBuf::from("/project/.gitignore"), b"*.log\nbuild/\n".to_vec());
            let fs = FakeFs(files);

            let filter = config.build_filter(Path::new("/project"), &fs).await;
            assert!(filter.is_some());
            let filter = filter.unwrap();
            assert!(filter.is_name_ignored("debug.log", false));
            assert!(filter.is_name_ignored("build", true));
            assert!(!filter.is_name_ignored("src", true));
        }

        #[tokio::test]
        async fn test_build_filter_missing_file_skipped() {
            let mut config = IgnoreConfig::none();
            config.add_file(".gitignore");
            config.add_file(".nonexistent");

            let mut files = HashMap::new();
            files.insert(PathBuf::from("/root/.gitignore"), b"*.tmp\n".to_vec());
            let fs = FakeFs(files);

            // Should not error — missing .nonexistent is silently skipped
            let filter = config.build_filter(Path::new("/root"), &fs).await;
            assert!(filter.is_some());
            let filter = filter.unwrap();
            assert!(filter.is_name_ignored("test.tmp", false));
        }

        #[tokio::test]
        async fn test_build_filter_defaults_plus_gitignore_merged() {
            let config = IgnoreConfig::mcp();

            let mut files = HashMap::new();
            files.insert(PathBuf::from("/project/.gitignore"), b"*.secret\n".to_vec());
            let fs = FakeFs(files);

            let filter = config.build_filter(Path::new("/project"), &fs).await;
            assert!(filter.is_some());
            let filter = filter.unwrap();
            // Defaults
            assert!(filter.is_name_ignored("target", true));
            assert!(filter.is_name_ignored("node_modules", true));
            // From .gitignore
            assert!(filter.is_name_ignored("passwords.secret", false));
            // Normal files pass through
            assert!(!filter.is_name_ignored("main.rs", false));
        }
    }
}
