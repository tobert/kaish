//! Path-aware glob matching with globstar (`**`) support.
//!
//! Extends the basic glob matching in `glob.rs` to handle patterns
//! that span directory boundaries with `**`:
//!
//! - `**/*.rs` matches `foo.rs`, `src/foo.rs`, `a/b/c/foo.rs`
//! - `src/**` matches everything under src/
//! - `a/**/z` matches `a/z`, `a/b/z`, `a/b/c/z`

use std::path::Path;
use thiserror::Error;

use crate::glob::glob_match;

/// Errors when parsing glob patterns.
#[derive(Debug, Clone, Error)]
pub enum PatternError {
    #[error("empty pattern")]
    Empty,
    #[error("invalid pattern: {0}")]
    Invalid(String),
}

/// A segment of a path pattern.
#[derive(Debug, Clone, PartialEq)]
pub enum PathSegment {
    /// Literal directory or file name: "src", "main.rs"
    Literal(String),
    /// Pattern with wildcards: "*.rs", "test_?"
    Pattern(String),
    /// Globstar: matches zero or more directory components
    Globstar,
}

/// A path-aware glob pattern with globstar support.
///
/// # Examples
/// ```
/// use kaish_glob::GlobPath;
/// use std::path::Path;
///
/// let pattern = GlobPath::new("**/*.rs").unwrap();
/// assert!(pattern.matches(Path::new("main.rs")));
/// assert!(pattern.matches(Path::new("src/main.rs")));
/// assert!(pattern.matches(Path::new("src/lib/utils.rs")));
/// assert!(!pattern.matches(Path::new("README.md")));
/// ```
#[derive(Debug, Clone)]
pub struct GlobPath {
    segments: Vec<PathSegment>,
    anchored: bool,
}

impl GlobPath {
    /// Parse a glob pattern into a GlobPath.
    ///
    /// Patterns starting with `/` are anchored to the root.
    /// `**` matches zero or more directory components.
    pub fn new(pattern: &str) -> Result<Self, PatternError> {
        if pattern.is_empty() {
            return Err(PatternError::Empty);
        }

        let (pattern, anchored) = if let Some(stripped) = pattern.strip_prefix('/') {
            (stripped, true)
        } else {
            (pattern, false)
        };

        let mut segments = Vec::new();

        for part in pattern.split('/') {
            if part.is_empty() {
                continue;
            }

            if part == "**" {
                // Consecutive globstars collapse to one
                if !matches!(segments.last(), Some(PathSegment::Globstar)) {
                    segments.push(PathSegment::Globstar);
                }
            } else if Self::is_literal(part) {
                segments.push(PathSegment::Literal(part.to_string()));
            } else {
                segments.push(PathSegment::Pattern(part.to_string()));
            }
        }

        Ok(GlobPath { segments, anchored })
    }

    /// Check if a path matches this pattern.
    pub fn matches(&self, path: &Path) -> bool {
        let components: Vec<&str> = path
            .components()
            .filter_map(|c| c.as_os_str().to_str())
            .collect();

        self.match_segments(&self.segments, &components, 0, 0)
    }

    /// Get the static prefix of the pattern (directories before any wildcard).
    ///
    /// This is useful for optimization: we can start the walk from this prefix
    /// instead of the root.
    ///
    /// # Examples
    /// ```
    /// use kaish_glob::GlobPath;
    /// use std::path::PathBuf;
    ///
    /// let pattern = GlobPath::new("src/lib/**/*.rs").unwrap();
    /// assert_eq!(pattern.static_prefix(), Some(PathBuf::from("src/lib")));
    ///
    /// let pattern = GlobPath::new("**/*.rs").unwrap();
    /// assert_eq!(pattern.static_prefix(), None);
    /// ```
    pub fn static_prefix(&self) -> Option<std::path::PathBuf> {
        let mut prefix = std::path::PathBuf::new();

        for segment in &self.segments {
            match segment {
                PathSegment::Literal(s) => prefix.push(s),
                _ => break,
            }
        }

        if prefix.as_os_str().is_empty() {
            None
        } else {
            Some(prefix)
        }
    }

    /// Split the pattern into its deepest static directory prefix and the
    /// remaining pattern to match beneath it.
    ///
    /// Used to start a walk from the literal leading directories instead of
    /// the filesystem root: walking from `/` is O(filesystem) and skips
    /// hidden intermediate directories, so `/tmp/.tmpXXXX/*.txt` would match
    /// nothing. At least one segment is always kept in the remaining pattern,
    /// so an all-literal pattern (`/a/b/c.txt`) walks `/a/b` and matches
    /// `c.txt` rather than trying to descend into the file itself. The
    /// returned pattern is unanchored (the anchor is consumed by the caller's
    /// walk root).
    ///
    /// # Examples
    /// ```
    /// use kaish_glob::GlobPath;
    /// use std::path::{Path, PathBuf};
    ///
    /// let (dir, rest) = GlobPath::new("/a/b/*.txt").unwrap().split_static_dir();
    /// assert_eq!(dir, PathBuf::from("a/b"));
    /// assert!(rest.matches(Path::new("c.txt")));
    ///
    /// // All-literal: the final component stays in the match pattern.
    /// let (dir, rest) = GlobPath::new("/a/b/c.txt").unwrap().split_static_dir();
    /// assert_eq!(dir, PathBuf::from("a/b"));
    /// assert!(rest.matches(Path::new("c.txt")));
    ///
    /// // No static prefix (leading wildcard / globstar): empty dir, full pattern.
    /// let (dir, _rest) = GlobPath::new("**/*.rs").unwrap().split_static_dir();
    /// assert_eq!(dir, PathBuf::new());
    /// ```
    pub fn split_static_dir(&self) -> (std::path::PathBuf, GlobPath) {
        let leading_literals = self
            .segments
            .iter()
            .take_while(|s| matches!(s, PathSegment::Literal(_)))
            .count();
        // Never consume the final segment — leave something to match.
        let prefix_len = leading_literals.min(self.segments.len().saturating_sub(1));

        let mut prefix = std::path::PathBuf::new();
        for segment in &self.segments[..prefix_len] {
            if let PathSegment::Literal(s) = segment {
                prefix.push(s);
            }
        }

        let remaining = GlobPath {
            segments: self.segments[prefix_len..].to_vec(),
            anchored: false,
        };
        (prefix, remaining)
    }

    /// Check if the pattern only matches directories.
    pub fn is_dir_only(&self) -> bool {
        matches!(self.segments.last(), Some(PathSegment::Globstar))
    }

    /// Check if the pattern is anchored (starts with /).
    pub fn is_anchored(&self) -> bool {
        self.anchored
    }

    /// Check if the pattern contains a globstar (`**`).
    ///
    /// Patterns with globstar require recursive directory traversal.
    /// Patterns without globstar only match at a fixed depth.
    pub fn has_globstar(&self) -> bool {
        self.segments.iter().any(|s| matches!(s, PathSegment::Globstar))
    }

    /// Get the depth of the pattern (number of path components).
    ///
    /// Returns `None` if the pattern contains globstar (variable depth).
    pub fn fixed_depth(&self) -> Option<usize> {
        if self.has_globstar() {
            None
        } else {
            Some(self.segments.len())
        }
    }

    /// Check if a string is a literal (no wildcards).
    fn is_literal(s: &str) -> bool {
        !s.contains('*') && !s.contains('?') && !s.contains('[') && !s.contains('{')
    }

    /// Recursive segment matching with backtracking for globstar.
    fn match_segments(
        &self,
        segments: &[PathSegment],
        components: &[&str],
        seg_idx: usize,
        comp_idx: usize,
    ) -> bool {
        // Both exhausted - match!
        if seg_idx >= segments.len() && comp_idx >= components.len() {
            return true;
        }

        // Segments exhausted but components remain - no match
        // (unless we ended with globstar, which is already consumed)
        if seg_idx >= segments.len() {
            return false;
        }

        match &segments[seg_idx] {
            PathSegment::Globstar => {
                // Globstar matches zero or more components
                // Try matching with 0, 1, 2, ... components consumed
                for skip in 0..=(components.len() - comp_idx) {
                    if self.match_segments(segments, components, seg_idx + 1, comp_idx + skip) {
                        return true;
                    }
                }
                false
            }

            PathSegment::Literal(lit) => {
                if comp_idx >= components.len() {
                    return false;
                }
                if components[comp_idx] == lit {
                    self.match_segments(segments, components, seg_idx + 1, comp_idx + 1)
                } else {
                    false
                }
            }

            PathSegment::Pattern(pat) => {
                if comp_idx >= components.len() {
                    return false;
                }
                if self.matches_component(pat, components[comp_idx]) {
                    self.match_segments(segments, components, seg_idx + 1, comp_idx + 1)
                } else {
                    false
                }
            }
        }
    }

    /// Match a single component against a pattern (with brace expansion).
    fn matches_component(&self, pattern: &str, component: &str) -> bool {
        glob_match(pattern, component)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn test_literal_pattern() {
        let pat = GlobPath::new("src/main.rs").unwrap();
        assert!(pat.matches(Path::new("src/main.rs")));
        assert!(!pat.matches(Path::new("src/lib.rs")));
        assert!(!pat.matches(Path::new("main.rs")));
    }

    #[test]
    fn test_simple_wildcard() {
        let pat = GlobPath::new("*.rs").unwrap();
        assert!(pat.matches(Path::new("main.rs")));
        assert!(pat.matches(Path::new("lib.rs")));
        assert!(!pat.matches(Path::new("main.go")));
        assert!(!pat.matches(Path::new("src/main.rs"))); // Only matches single component
    }

    #[test]
    fn test_globstar_prefix() {
        let pat = GlobPath::new("**/*.rs").unwrap();
        assert!(pat.matches(Path::new("main.rs")));
        assert!(pat.matches(Path::new("src/main.rs")));
        assert!(pat.matches(Path::new("src/lib/utils.rs")));
        assert!(pat.matches(Path::new("a/b/c/d/e.rs")));
        assert!(!pat.matches(Path::new("main.go")));
        assert!(!pat.matches(Path::new("src/main.go")));
    }

    #[test]
    fn test_globstar_suffix() {
        let pat = GlobPath::new("src/**").unwrap();
        assert!(pat.matches(Path::new("src")));
        assert!(pat.matches(Path::new("src/main.rs")));
        assert!(pat.matches(Path::new("src/lib/utils.rs")));
        assert!(!pat.matches(Path::new("test/main.rs")));
    }

    #[test]
    fn test_globstar_middle() {
        let pat = GlobPath::new("a/**/z").unwrap();
        assert!(pat.matches(Path::new("a/z")));
        assert!(pat.matches(Path::new("a/b/z")));
        assert!(pat.matches(Path::new("a/b/c/z")));
        assert!(pat.matches(Path::new("a/b/c/d/e/z")));
        assert!(!pat.matches(Path::new("b/c/z")));
        assert!(!pat.matches(Path::new("a/z/extra")));
    }

    #[test]
    fn test_consecutive_globstars() {
        let pat = GlobPath::new("a/**/**/z").unwrap();
        assert!(pat.matches(Path::new("a/z")));
        assert!(pat.matches(Path::new("a/b/z")));
        assert!(pat.matches(Path::new("a/b/c/z")));
    }

    #[test]
    fn test_brace_expansion() {
        let pat = GlobPath::new("*.{rs,go,py}").unwrap();
        assert!(pat.matches(Path::new("main.rs")));
        assert!(pat.matches(Path::new("server.go")));
        assert!(pat.matches(Path::new("script.py")));
        assert!(!pat.matches(Path::new("style.css")));
    }

    #[test]
    fn test_brace_with_globstar() {
        let pat = GlobPath::new("**/*.{rs,go}").unwrap();
        assert!(pat.matches(Path::new("main.rs")));
        assert!(pat.matches(Path::new("src/lib.go")));
        assert!(pat.matches(Path::new("a/b/c/d.rs")));
        assert!(!pat.matches(Path::new("src/main.py")));
    }

    #[test]
    fn test_question_mark() {
        let pat = GlobPath::new("file?.txt").unwrap();
        assert!(pat.matches(Path::new("file1.txt")));
        assert!(pat.matches(Path::new("fileA.txt")));
        assert!(!pat.matches(Path::new("file12.txt")));
        assert!(!pat.matches(Path::new("file.txt")));
    }

    #[test]
    fn test_char_class() {
        let pat = GlobPath::new("[abc].rs").unwrap();
        assert!(pat.matches(Path::new("a.rs")));
        assert!(pat.matches(Path::new("b.rs")));
        assert!(pat.matches(Path::new("c.rs")));
        assert!(!pat.matches(Path::new("d.rs")));
    }

    #[test]
    fn test_static_prefix() {
        assert_eq!(
            GlobPath::new("src/lib/**/*.rs").unwrap().static_prefix(),
            Some(std::path::PathBuf::from("src/lib"))
        );

        assert_eq!(
            GlobPath::new("src/**").unwrap().static_prefix(),
            Some(std::path::PathBuf::from("src"))
        );

        assert_eq!(GlobPath::new("**/*.rs").unwrap().static_prefix(), None);

        assert_eq!(GlobPath::new("*.rs").unwrap().static_prefix(), None);
    }

    #[test]
    fn test_anchored_pattern() {
        let pat = GlobPath::new("/src/*.rs").unwrap();
        assert!(pat.is_anchored());
        assert!(pat.matches(Path::new("src/main.rs")));
    }

    #[test]
    fn test_empty_pattern() {
        assert!(matches!(GlobPath::new(""), Err(PatternError::Empty)));
    }

    #[test]
    fn test_has_globstar() {
        assert!(GlobPath::new("**/*.rs").unwrap().has_globstar());
        assert!(GlobPath::new("src/**").unwrap().has_globstar());
        assert!(GlobPath::new("a/**/z").unwrap().has_globstar());
        assert!(!GlobPath::new("*.rs").unwrap().has_globstar());
        assert!(!GlobPath::new("src/*.rs").unwrap().has_globstar());
        assert!(!GlobPath::new("src/lib/main.rs").unwrap().has_globstar());
    }

    #[test]
    fn test_fixed_depth() {
        assert_eq!(GlobPath::new("*.rs").unwrap().fixed_depth(), Some(1));
        assert_eq!(GlobPath::new("src/*.rs").unwrap().fixed_depth(), Some(2));
        assert_eq!(GlobPath::new("a/b/c.txt").unwrap().fixed_depth(), Some(3));
        assert_eq!(GlobPath::new("**/*.rs").unwrap().fixed_depth(), None);
        assert_eq!(GlobPath::new("src/**").unwrap().fixed_depth(), None);
    }

    #[test]
    fn test_hidden_files() {
        let pat = GlobPath::new("**/*.rs").unwrap();
        assert!(pat.matches(Path::new(".hidden.rs")));
        assert!(pat.matches(Path::new(".config/settings.rs")));
    }

    #[test]
    fn test_complex_real_world() {
        let pat = GlobPath::new("**/*_test.rs").unwrap();
        assert!(pat.matches(Path::new("parser_test.rs")));
        assert!(pat.matches(Path::new("src/lexer_test.rs")));
        assert!(pat.matches(Path::new("crates/kernel/tests/eval_test.rs")));
        assert!(!pat.matches(Path::new("parser.rs")));

        let pat = GlobPath::new("src/**/*.{rs,go}").unwrap();
        assert!(pat.matches(Path::new("src/main.rs")));
        assert!(pat.matches(Path::new("src/api/handler.go")));
        assert!(!pat.matches(Path::new("test/main.rs")));
    }
}
