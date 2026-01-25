//! Gitignore-style pattern matching.
//!
//! Implements gitignore semantics for filtering files during walks.

use std::path::Path;

use crate::backend::KernelBackend;
use crate::glob::glob_match;

/// A compiled ignore rule from a gitignore file.
#[derive(Debug, Clone)]
struct IgnoreRule {
    /// The pattern to match.
    pattern: String,
    /// True if this rule negates (starts with !).
    negated: bool,
    /// True if this rule only matches directories (ends with /).
    dir_only: bool,
    /// True if this pattern is anchored (contains / not at end).
    anchored: bool,
}

impl IgnoreRule {
    fn parse(line: &str) -> Option<Self> {
        let line = line.trim();

        // Skip empty lines and comments
        if line.is_empty() || line.starts_with('#') {
            return None;
        }

        let mut pattern = line.to_string();
        let mut negated = false;
        let mut dir_only = false;

        // Check for negation
        if let Some(stripped) = pattern.strip_prefix('!') {
            negated = true;
            pattern = stripped.to_string();
        }

        // Check for directory-only pattern
        if let Some(stripped) = pattern.strip_suffix('/') {
            dir_only = true;
            pattern = stripped.to_string();
        }

        // Check if anchored (contains / that's not at the end)
        let anchored = pattern.contains('/');

        // Remove leading /
        if let Some(stripped) = pattern.strip_prefix('/') {
            pattern = stripped.to_string();
        }

        Some(IgnoreRule {
            pattern,
            negated,
            dir_only,
            anchored,
        })
    }

    fn matches(&self, path: &Path, is_dir: bool) -> bool {
        // Dir-only rules only match directories
        if self.dir_only && !is_dir {
            return false;
        }

        let path_str = path.to_string_lossy();

        if self.anchored {
            // Anchored patterns match from the root
            self.glob_match_path(&path_str)
        } else {
            // Non-anchored patterns can match anywhere
            // Try matching the full path
            if self.glob_match_path(&path_str) {
                return true;
            }

            // Try matching just the filename
            if let Some(name) = path.file_name() {
                let name_str = name.to_string_lossy();
                if glob_match(&self.pattern, &name_str) {
                    return true;
                }
            }

            false
        }
    }

    fn glob_match_path(&self, path: &str) -> bool {
        // Handle ** in patterns by converting to a match that works
        if self.pattern.contains("**") {
            // For patterns like "**/*.rs", we need special handling
            self.match_with_globstar(path)
        } else {
            glob_match(&self.pattern, path)
        }
    }

    fn match_with_globstar(&self, path: &str) -> bool {
        // Split pattern by **
        let parts: Vec<&str> = self.pattern.split("**").collect();

        if parts.len() == 2 {
            let prefix = parts[0].trim_end_matches('/');
            let suffix = parts[1].trim_start_matches('/');

            // Check prefix
            let remaining = if prefix.is_empty() {
                path
            } else if let Some(rest) = path.strip_prefix(prefix) {
                rest.trim_start_matches('/')
            } else {
                return false;
            };

            // Check suffix
            if suffix.is_empty() {
                return true;
            }

            // Try matching suffix against any tail of the path
            for (i, _) in remaining.char_indices() {
                let tail = &remaining[i..];
                if glob_match(suffix, tail) {
                    return true;
                }
            }

            // Also try matching just the suffix
            glob_match(suffix, remaining)
        } else {
            // Complex pattern with multiple **, fall back to component matching
            glob_match(&self.pattern.replace("**", "*"), path)
        }
    }
}

/// Filter for gitignore-style patterns.
#[derive(Debug, Clone, Default)]
pub struct IgnoreFilter {
    rules: Vec<IgnoreRule>,
}

impl IgnoreFilter {
    /// Create an empty ignore filter.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a filter with default ignores for common build artifacts.
    pub fn with_defaults() -> Self {
        let mut filter = Self::new();

        // Always ignore .git
        filter.add_rule(".git");

        // Common build/dependency directories
        filter.add_rule("node_modules");
        filter.add_rule("target"); // Rust
        filter.add_rule("__pycache__");
        filter.add_rule(".venv");
        filter.add_rule("venv");
        filter.add_rule("dist");
        filter.add_rule("build");
        filter.add_rule(".next"); // Next.js

        filter
    }

    /// Load patterns from a gitignore file.
    pub async fn from_gitignore(
        path: &Path,
        backend: &dyn KernelBackend,
    ) -> anyhow::Result<Self> {
        let content = backend.read(path, None).await?;
        let text = String::from_utf8_lossy(&content);

        let mut filter = Self::new();
        for line in text.lines() {
            if let Some(rule) = IgnoreRule::parse(line) {
                filter.rules.push(rule);
            }
        }

        Ok(filter)
    }

    /// Add a rule from a pattern string.
    pub fn add_rule(&mut self, pattern: &str) {
        if let Some(rule) = IgnoreRule::parse(pattern) {
            self.rules.push(rule);
        }
    }

    /// Check if a path should be ignored.
    ///
    /// Returns true if the path matches any non-negated rule
    /// and doesn't match a later negated rule.
    pub fn is_ignored(&self, path: &Path, is_dir: bool) -> bool {
        let mut ignored = false;

        for rule in &self.rules {
            if rule.matches(path, is_dir) {
                ignored = !rule.negated;
            }
        }

        ignored
    }

    /// Check if a path component (single name) should be ignored.
    ///
    /// This is for quick filtering during directory traversal.
    pub fn is_name_ignored(&self, name: &str, is_dir: bool) -> bool {
        self.is_ignored(Path::new(name), is_dir)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_patterns() {
        let mut filter = IgnoreFilter::new();
        filter.add_rule("*.log");
        filter.add_rule("temp/");

        assert!(filter.is_ignored(Path::new("app.log"), false));
        assert!(filter.is_ignored(Path::new("debug.log"), false));
        assert!(!filter.is_ignored(Path::new("app.txt"), false));

        assert!(filter.is_ignored(Path::new("temp"), true));
        assert!(!filter.is_ignored(Path::new("temp"), false)); // dir-only
    }

    #[test]
    fn test_negation() {
        let mut filter = IgnoreFilter::new();
        filter.add_rule("*.log");
        filter.add_rule("!important.log");

        assert!(filter.is_ignored(Path::new("debug.log"), false));
        assert!(!filter.is_ignored(Path::new("important.log"), false));
    }

    #[test]
    fn test_anchored_patterns() {
        let mut filter = IgnoreFilter::new();
        filter.add_rule("/root.txt");
        filter.add_rule("anywhere.txt");

        assert!(filter.is_ignored(Path::new("root.txt"), false));
        assert!(!filter.is_ignored(Path::new("sub/root.txt"), false));

        assert!(filter.is_ignored(Path::new("anywhere.txt"), false));
        assert!(filter.is_ignored(Path::new("sub/anywhere.txt"), false));
    }

    #[test]
    fn test_directory_patterns() {
        let mut filter = IgnoreFilter::new();
        filter.add_rule("build/");

        assert!(filter.is_ignored(Path::new("build"), true));
        assert!(!filter.is_ignored(Path::new("build"), false)); // file named "build"
    }

    #[test]
    fn test_globstar() {
        let mut filter = IgnoreFilter::new();
        filter.add_rule("**/*.log");

        assert!(filter.is_ignored(Path::new("app.log"), false));
        assert!(filter.is_ignored(Path::new("logs/app.log"), false));
        assert!(filter.is_ignored(Path::new("var/logs/app.log"), false));
    }

    #[test]
    fn test_defaults() {
        let filter = IgnoreFilter::with_defaults();

        assert!(filter.is_ignored(Path::new(".git"), true));
        assert!(filter.is_ignored(Path::new("node_modules"), true));
        assert!(filter.is_ignored(Path::new("target"), true));
        assert!(filter.is_ignored(Path::new("__pycache__"), true));
    }

    #[test]
    fn test_comments_and_empty() {
        let mut filter = IgnoreFilter::new();
        filter.add_rule("# comment");
        filter.add_rule("");
        filter.add_rule("  ");
        filter.add_rule("valid.txt");

        assert_eq!(filter.rules.len(), 1);
        assert!(filter.is_ignored(Path::new("valid.txt"), false));
    }

    #[test]
    fn test_path_patterns() {
        let mut filter = IgnoreFilter::new();
        filter.add_rule("logs/*.log");

        assert!(filter.is_ignored(Path::new("logs/app.log"), false));
        assert!(!filter.is_ignored(Path::new("other/app.log"), false));
        assert!(!filter.is_ignored(Path::new("app.log"), false));
    }
}
