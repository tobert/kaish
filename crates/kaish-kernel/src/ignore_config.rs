//! Configurable ignore file policy for file-walking tools.
//!
//! Controls which gitignore-format files are loaded and how broadly
//! ignore rules apply. Per-mode defaults: sandboxed agents get `Enforced`
//! filtering (context-flood protection), the interactive REPL gets the same
//! filters at `Advisory` scope (recoverable per call via `--no-ignore` or
//! per session via `kaish-ignore`), and bare embedded/test kernels get none.

use std::path::{Path, PathBuf};

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
    /// When true, also load the user's global gitignore file (the path is
    /// resolved from `core.excludesFile` in `~/.gitconfig`, falling back to
    /// `~/.config/git/ignore` per git's own resolution). Off by default to
    /// keep tests hermetic.
    use_global_gitignore: bool,
    /// Test-only override for the global gitignore path. When `Some`, this
    /// path is read instead of resolving via git's standard config lookup.
    /// Production callers leave this `None`.
    global_gitignore_path_override: Option<PathBuf>,
}

impl IgnoreConfig {
    /// No filtering — embedded/test default.
    pub fn none() -> Self {
        Self {
            scope: IgnoreScope::Advisory,
            ignore_files: Vec::new(),
            use_defaults: false,
            auto_gitignore: false,
            use_global_gitignore: false,
            global_gitignore_path_override: None,
        }
    }

    /// Interactive-REPL defaults (GH #134): the same filters as `agent()` —
    /// `.gitignore` loaded, default ignore list on — but at **Advisory**
    /// scope, so `find` stays POSIX-unrestricted and a human can recover the
    /// unfiltered view per call (`--no-ignore`) or per session
    /// (`kaish-ignore clear`).
    pub fn interactive() -> Self {
        Self {
            scope: IgnoreScope::Advisory,
            ..Self::agent()
        }
    }

    /// Sandboxed-agent defaults: enforced scope, .gitignore loaded, defaults on.
    ///
    /// NOTE: `interactive()` inherits every field but `scope` from here via
    /// struct-update syntax — a new field added to this preset carries into
    /// the REPL preset unless `interactive()` overrides it.
    pub fn agent() -> Self {
        Self {
            scope: IgnoreScope::Enforced,
            ignore_files: vec![".gitignore".to_string()],
            use_defaults: true,
            auto_gitignore: true,
            use_global_gitignore: false,
            global_gitignore_path_override: None,
        }
    }

    /// Whether any filtering is configured.
    pub fn is_active(&self) -> bool {
        self.use_defaults
            || self.auto_gitignore
            || !self.ignore_files.is_empty()
            || self.use_global_gitignore
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

    /// Toggle whether the user's global gitignore is loaded. When enabled,
    /// the path comes from `core.excludesFile` (falling back to
    /// `~/.config/git/ignore` per git's lookup), unless an override has
    /// been set via `set_global_gitignore_path` for tests.
    pub fn set_use_global_gitignore(&mut self, on: bool) {
        self.use_global_gitignore = on;
    }

    pub fn use_global_gitignore(&self) -> bool {
        self.use_global_gitignore
    }

    /// Test hook: substitute the global gitignore lookup with a fixed path.
    /// Production callers leave this unset.
    pub fn set_global_gitignore_path(&mut self, path: Option<PathBuf>) {
        self.global_gitignore_path_override = path;
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
        self.use_global_gitignore = false;
        self.global_gitignore_path_override = None;
    }

    /// Build an `IgnoreFilter` from the configured file list and defaults.
    ///
    /// Loads each ignore file relative to `root` via the given `WalkerFs`.
    /// Returns `None` if no filtering is configured.
    ///
    /// **Ancestor walk-up.** For each configured ignore filename, this also
    /// walks up the directory tree from `root` and loads the same filename
    /// from each ancestor. Rules from ancestor files are *rebased* onto the
    /// walker's relative-path frame: anchored rules pointing into the
    /// walker's subtree get their prefix stripped; rules pointing outside
    /// are dropped; unanchored rules pass through unchanged. Matches git's
    /// behavior of honoring `.gitignore` files in any ancestor directory.
    /// Closer ancestors get higher priority (added later).
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

        // Global gitignore (one notch above hardcoded defaults). Reads real
        // disk regardless of which `WalkerFs` we're walking, since the
        // global file lives outside any project tree. Silently skipped if
        // the file doesn't exist or cannot be read.
        if self.use_global_gitignore {
            let path = self
                .global_gitignore_path_override
                .clone()
                .or_else(ignore::gitignore::gitconfig_excludes_path);
            if let Some(path) = path
                && let Ok(content) = std::fs::read_to_string(&path)
            {
                for line in content.lines() {
                    filter.add_rule(line);
                }
            }
        }

        // Walk up from `root` collecting ancestor directories and the
        // relative path from each ancestor down to `root`. We build the
        // list closest-first, then reverse so farther ancestors merge
        // into the filter earlier (= lower priority).
        let mut ancestors: Vec<(PathBuf, String)> = Vec::new();
        let mut current = root;
        while let Some(parent) = current.parent() {
            // strip_prefix yields the path from parent down to root.
            if let Ok(rel) = root.strip_prefix(parent) {
                ancestors.push((
                    parent.to_path_buf(),
                    rel.to_string_lossy().into_owned(),
                ));
            }
            if parent == current {
                break;
            }
            current = parent;
        }
        ancestors.reverse(); // farthest ancestor first

        for (ancestor_dir, prefix) in &ancestors {
            for filename in &self.ignore_files {
                let path = ancestor_dir.join(filename);
                if !fs.exists(&path).await {
                    continue;
                }
                let Ok(bytes) = fs.read_file(&path).await else {
                    continue;
                };
                let text = String::from_utf8_lossy(&bytes);
                for line in text.lines() {
                    if let Some(rebased) = rebase_gitignore_line(line, prefix) {
                        filter.add_rule(&rebased);
                    }
                }
            }
        }

        // Root-level ignore files merge last (highest priority).
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

/// Rewrite a single gitignore line so its rule, when interpreted relative to
/// a walker root, produces the same set of matches it would have produced if
/// interpreted relative to the gitignore's own (ancestor) directory.
///
/// `prefix` is the path from the gitignore's directory down to the walker
/// root, e.g. `prefix = "b/c"` when the gitignore lives at `/a/.gitignore`
/// and the walker is rooted at `/a/b/c`. An empty prefix means the gitignore
/// is at the walker root itself (caller should usually take the fast path
/// and use `IgnoreFilter::from_gitignore` directly in that case).
///
/// Returns `None` for blank/comment lines, or for anchored rules whose
/// target path lies outside the walker's subtree (dropped because they
/// can never match anything we'll walk).
fn rebase_gitignore_line(line: &str, prefix: &str) -> Option<String> {
    let trimmed = line.trim();
    if trimmed.is_empty() || trimmed.starts_with('#') {
        return None;
    }

    // Split off the negation marker first so we can re-emit it.
    let (negated, rest) = if let Some(stripped) = trimmed.strip_prefix('!') {
        (true, stripped)
    } else {
        (false, trimmed)
    };

    // And the directory-only suffix.
    let (dir_only, rest) = if let Some(stripped) = rest.strip_suffix('/') {
        (true, stripped)
    } else {
        (false, rest)
    };

    // A rule is "anchored" in git semantics when it has a leading `/`
    // OR an internal `/`. Unanchored patterns match anywhere in the tree
    // and need no rebasing.
    let leading_slash = rest.starts_with('/');
    let body = rest.trim_start_matches('/');
    let is_anchored = leading_slash || body.contains('/');

    let prefix = prefix.trim_matches('/');

    let new_body: String = if !is_anchored {
        // Unanchored — passes through unchanged.
        body.to_string()
    } else if prefix.is_empty() {
        // Walker is at the gitignore's own directory — rule is already in
        // the right frame; preserve the leading-slash anchor.
        format!("/{body}")
    } else {
        // The rule's anchored path is interpreted from the gitignore's
        // directory. Strip our `prefix/` to translate into walker frame;
        // drop entirely if the rule points outside.
        if body == prefix {
            // Rule targets the walker root itself — irrelevant once we're
            // walking inside it.
            return None;
        }
        let prefix_with_slash = format!("{prefix}/");
        match body.strip_prefix(&prefix_with_slash) {
            Some(stripped) => format!("/{stripped}"),
            None => return None,
        }
    };

    let mut out = String::new();
    if negated {
        out.push('!');
    }
    out.push_str(&new_body);
    if dir_only {
        out.push('/');
    }
    Some(out)
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
    fn test_agent_preset_is_active() {
        let config = IgnoreConfig::agent();
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
        let mut config = IgnoreConfig::agent();
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
            let config = IgnoreConfig::agent();

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

        /// Parent-directory `.gitignore` walk-up. When the walker is started
        /// at `/a/b`, a `.gitignore` at `/a/` should still apply (per git
        /// semantics — git looks at every ancestor up to the repo root).
        ///
        /// Unanchored rule (`*.log`) matches anywhere — must hide files in
        /// the walker's tree. Anchored rule with explicit subpath
        /// (`b/secret.txt`) must match the file at the right location once
        /// rebased to the walker frame.
        #[tokio::test]
        async fn test_build_filter_parent_gitignore_walk_up() {
            let mut config = IgnoreConfig::none();
            config.add_file(".gitignore");

            let mut files = HashMap::new();
            files.insert(
                PathBuf::from("/a/.gitignore"),
                b"*.log\nb/secret.txt\n".to_vec(),
            );
            let fs = FakeFs(files);

            // Walker rooted at /a/b — its files have paths relative to /a/b.
            let filter = config.build_filter(Path::new("/a/b"), &fs).await;
            assert!(filter.is_some(), "filter should be loaded from ancestor");
            let filter = filter.unwrap();

            // Unanchored *.log rule from /a/.gitignore must reach into /a/b.
            assert!(
                filter.is_ignored(Path::new("debug.log"), false),
                "ancestor's *.log must apply in subtree",
            );
            assert!(
                filter.is_ignored(Path::new("nested/dir/app.log"), false),
                "ancestor's *.log must reach nested files in subtree",
            );

            // The anchored "b/secret.txt" from /a/.gitignore points at /a/b/secret.txt,
            // which in our walker frame is just "secret.txt".
            assert!(
                filter.is_ignored(Path::new("secret.txt"), false),
                "anchored ancestor rule must rebase to walker frame",
            );

            // A regular file still passes through.
            assert!(!filter.is_ignored(Path::new("main.rs"), false));
        }

        /// `.ignore` / `.rgignore` files are loaded with higher precedence
        /// than `.gitignore`. A negation in `.ignore` should override a
        /// matching ignore from `.gitignore` — that's the rg behavior we
        /// want.
        #[tokio::test]
        async fn test_build_filter_dot_ignore_overrides_gitignore() {
            let mut config = IgnoreConfig::none();
            // Order matters: later-added = higher precedence.
            config.add_file(".gitignore");
            config.add_file(".ignore");
            config.add_file(".rgignore");

            let mut files = HashMap::new();
            files.insert(PathBuf::from("/proj/.gitignore"), b"*.log\n".to_vec());
            // .ignore un-ignores keep.log
            files.insert(PathBuf::from("/proj/.ignore"), b"!keep.log\n".to_vec());
            let fs = FakeFs(files);

            let filter = config.build_filter(Path::new("/proj"), &fs).await;
            assert!(filter.is_some());
            let filter = filter.unwrap();

            assert!(
                filter.is_ignored(Path::new("debug.log"), false),
                ".gitignore *.log still applies",
            );
            assert!(
                !filter.is_ignored(Path::new("keep.log"), false),
                ".ignore negation must override .gitignore",
            );
        }

        /// Global gitignore file is honored when the flag is set. The walker
        /// FS doesn't carry the global file (it lives outside any project);
        /// the read goes through real disk via tokio. We use the
        /// `set_global_gitignore_path` test hook so we don't depend on
        /// `$HOME` / `$XDG_CONFIG_HOME` and stay safe under parallel tests.
        #[tokio::test]
        async fn test_build_filter_global_gitignore_honored() {
            let tmp = tempfile::tempdir().expect("tempdir");
            let global_path = tmp.path().join("git_ignore");
            // Write the fixture with std::fs (not tokio::fs) so this test
            // compiles in the minimal `--no-default-features` build, which
            // doesn't enable tokio's `fs` feature. Production reads this file
            // with `std::fs::read_to_string` too (see build_filter), so this
            // stays faithful to the real path.
            std::fs::write(&global_path, b"*.global_secret\n").expect("write global gitignore");

            let mut config = IgnoreConfig::none();
            config.set_use_global_gitignore(true);
            config.set_global_gitignore_path(Some(global_path));

            // build_filter ignores the WalkerFs for the global file (it
            // reads real disk), so an empty FakeFs is fine.
            let fs = FakeFs(HashMap::new());

            let filter = config.build_filter(Path::new("/proj"), &fs).await;
            assert!(filter.is_some(), "global gitignore must activate filtering");
            let filter = filter.unwrap();

            assert!(
                filter.is_ignored(Path::new("creds.global_secret"), false),
                "global gitignore rule must apply",
            );
            assert!(!filter.is_ignored(Path::new("main.rs"), false));
        }

        /// Global gitignore enabled but file missing: silent skip, no error,
        /// no rules added (filter still active because the flag is set).
        #[tokio::test]
        async fn test_build_filter_global_gitignore_missing_file_ok() {
            let tmp = tempfile::tempdir().expect("tempdir");
            // Path that doesn't exist.
            let global_path = tmp.path().join("does_not_exist");

            let mut config = IgnoreConfig::none();
            config.set_use_global_gitignore(true);
            config.set_global_gitignore_path(Some(global_path));

            let fs = FakeFs(HashMap::new());
            let filter = config.build_filter(Path::new("/proj"), &fs).await;
            // Active flag still produces Some(filter), even if no rules loaded.
            assert!(filter.is_some());
        }

        /// `.rgignore` is highest-precedence and can override `.ignore`.
        #[tokio::test]
        async fn test_build_filter_rgignore_highest_precedence() {
            let mut config = IgnoreConfig::none();
            config.add_file(".gitignore");
            config.add_file(".ignore");
            config.add_file(".rgignore");

            let mut files = HashMap::new();
            files.insert(PathBuf::from("/proj/.ignore"), b"!keep.log\n".to_vec());
            // .rgignore re-ignores keep.log; should win.
            files.insert(PathBuf::from("/proj/.rgignore"), b"keep.log\n".to_vec());
            let fs = FakeFs(files);

            let filter = config.build_filter(Path::new("/proj"), &fs).await;
            let filter = filter.unwrap();

            assert!(
                filter.is_ignored(Path::new("keep.log"), false),
                ".rgignore must override .ignore",
            );
        }

        /// Ancestor anchored rule that points OUTSIDE the walker root is
        /// dropped: `/a/.gitignore` saying `c/foo.txt` (= /a/c/foo.txt)
        /// must not match `/a/b/c/foo.txt` in our subtree.
        #[tokio::test]
        async fn test_build_filter_parent_anchored_rule_outside_subtree_dropped() {
            let mut config = IgnoreConfig::none();
            config.add_file(".gitignore");

            let mut files = HashMap::new();
            files.insert(PathBuf::from("/a/.gitignore"), b"c/foo.txt\n".to_vec());
            let fs = FakeFs(files);

            // Walker rooted at /a/b — the ancestor rule's anchored target
            // /a/c/foo.txt is NOT under our subtree, so it should be dropped.
            let filter = config.build_filter(Path::new("/a/b"), &fs).await;
            assert!(filter.is_some());
            let filter = filter.unwrap();

            // /a/b/c/foo.txt → relative "c/foo.txt" must NOT match the
            // rebased-and-dropped ancestor rule.
            assert!(
                !filter.is_ignored(Path::new("c/foo.txt"), false),
                "anchored ancestor rule outside subtree must be dropped",
            );
        }
    }
}
