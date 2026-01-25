//! rsync-style include/exclude filters.
//!
//! Filters are processed in order. The first matching rule wins.
//! If no rule matches, the path is included by default.

use std::path::Path;

use super::GlobPath;

/// Result of checking a path against filters.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FilterResult {
    /// Path is explicitly included.
    Include,
    /// Path is explicitly excluded.
    Exclude,
    /// No filter matched this path.
    NoMatch,
}

/// An rsync-style include/exclude filter.
///
/// Rules are processed in order. The first matching rule determines
/// whether the path is included or excluded. If no rule matches,
/// the path is considered to have no explicit ruling (NoMatch).
///
/// # Examples
/// ```
/// use kaish_kernel::walker::{IncludeExclude, FilterResult};
/// use std::path::Path;
///
/// let mut filter = IncludeExclude::new();
/// filter.include("*.rs");
/// filter.exclude("*_test.rs");
///
/// // Note: order matters! First match wins.
/// // In this case, *.rs matches first, so test files ARE included.
/// ```
#[derive(Debug, Clone, Default)]
pub struct IncludeExclude {
    rules: Vec<(FilterAction, CompiledRule)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FilterAction {
    Include,
    Exclude,
}

#[derive(Debug, Clone)]
struct CompiledRule {
    glob: Option<GlobPath>,
    raw: String,
}

impl CompiledRule {
    fn new(pattern: &str) -> Self {
        // Try to compile as GlobPath
        let glob = GlobPath::new(pattern).ok();

        Self {
            glob,
            raw: pattern.to_string(),
        }
    }

    fn matches(&self, path: &Path) -> bool {
        if let Some(ref glob) = self.glob {
            return glob.matches(path);
        }

        // Fallback: simple string matching
        let path_str = path.to_string_lossy();

        // Also try matching just the filename
        if let Some(name) = path.file_name() {
            let name_str = name.to_string_lossy();
            if crate::glob::glob_match(&self.raw, &name_str) {
                return true;
            }
        }

        crate::glob::glob_match(&self.raw, &path_str)
    }
}

impl IncludeExclude {
    /// Create an empty filter set.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add an include pattern.
    ///
    /// Paths matching this pattern will be included (if checked before
    /// any exclude pattern matches).
    pub fn include(&mut self, pattern: &str) {
        self.rules
            .push((FilterAction::Include, CompiledRule::new(pattern)));
    }

    /// Add an exclude pattern.
    ///
    /// Paths matching this pattern will be excluded (if checked before
    /// any include pattern matches).
    pub fn exclude(&mut self, pattern: &str) {
        self.rules
            .push((FilterAction::Exclude, CompiledRule::new(pattern)));
    }

    /// Check a path against the filter rules.
    ///
    /// Returns the first matching rule's action, or `NoMatch` if no rules match.
    pub fn check(&self, path: &Path) -> FilterResult {
        for (action, rule) in &self.rules {
            if rule.matches(path) {
                return match action {
                    FilterAction::Include => FilterResult::Include,
                    FilterAction::Exclude => FilterResult::Exclude,
                };
            }
        }

        FilterResult::NoMatch
    }

    /// Check if a path should be filtered out.
    ///
    /// Returns true if the path should be excluded (either explicitly excluded
    /// or not matching any include patterns when in strict mode).
    pub fn should_exclude(&self, path: &Path) -> bool {
        matches!(self.check(path), FilterResult::Exclude)
    }

    /// Check if any rules are defined.
    pub fn is_empty(&self) -> bool {
        self.rules.is_empty()
    }

    /// Get the number of rules.
    pub fn len(&self) -> usize {
        self.rules.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_filter() {
        let filter = IncludeExclude::new();
        assert_eq!(filter.check(Path::new("any.txt")), FilterResult::NoMatch);
        assert!(!filter.should_exclude(Path::new("any.txt")));
    }

    #[test]
    fn test_include() {
        let mut filter = IncludeExclude::new();
        filter.include("*.rs");

        assert_eq!(filter.check(Path::new("main.rs")), FilterResult::Include);
        assert_eq!(filter.check(Path::new("main.txt")), FilterResult::NoMatch);
    }

    #[test]
    fn test_exclude() {
        let mut filter = IncludeExclude::new();
        filter.exclude("*.log");

        assert_eq!(filter.check(Path::new("app.log")), FilterResult::Exclude);
        assert!(filter.should_exclude(Path::new("app.log")));
        assert!(!filter.should_exclude(Path::new("app.txt")));
    }

    #[test]
    fn test_order_matters() {
        // First rule wins
        let mut filter = IncludeExclude::new();
        filter.include("*.rs");
        filter.exclude("*_test.rs");

        // *.rs matches first, so test files are included
        assert_eq!(
            filter.check(Path::new("parser_test.rs")),
            FilterResult::Include
        );

        // Reverse order
        let mut filter = IncludeExclude::new();
        filter.exclude("*_test.rs");
        filter.include("*.rs");

        // *_test.rs matches first, so test files are excluded
        assert_eq!(
            filter.check(Path::new("parser_test.rs")),
            FilterResult::Exclude
        );
    }

    #[test]
    fn test_globstar_patterns() {
        let mut filter = IncludeExclude::new();
        filter.include("**/*.rs");
        filter.exclude("**/test/**");

        assert_eq!(filter.check(Path::new("src/main.rs")), FilterResult::Include);
        assert_eq!(
            filter.check(Path::new("src/lib/utils.rs")),
            FilterResult::Include
        );
    }

    #[test]
    fn test_path_patterns() {
        let mut filter = IncludeExclude::new();
        filter.exclude("logs/*");

        assert!(filter.should_exclude(Path::new("logs/app.log")));
        assert!(!filter.should_exclude(Path::new("other/app.log")));
    }

    #[test]
    fn test_multiple_patterns() {
        let mut filter = IncludeExclude::new();
        filter.include("*.rs");
        filter.include("*.go");
        filter.include("*.py");
        filter.exclude("*_test.*");

        assert_eq!(filter.check(Path::new("main.rs")), FilterResult::Include);
        assert_eq!(filter.check(Path::new("server.go")), FilterResult::Include);
        assert_eq!(filter.check(Path::new("main_test.rs")), FilterResult::Include); // *.rs matches first!
    }

    #[test]
    fn test_brace_expansion() {
        let mut filter = IncludeExclude::new();
        filter.include("*.{rs,go,py}");

        assert_eq!(filter.check(Path::new("main.rs")), FilterResult::Include);
        assert_eq!(filter.check(Path::new("main.go")), FilterResult::Include);
        assert_eq!(filter.check(Path::new("main.py")), FilterResult::Include);
        assert_eq!(filter.check(Path::new("main.js")), FilterResult::NoMatch);
    }
}
