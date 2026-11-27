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

        let mut budget = MatchBudget::new(self.segments.len(), components.len(), self.has_globstar());
        self.match_segments(&self.segments, &components, 0, 0, &mut budget)
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

    /// Match a path under file-walk semantics, honouring the leading-dot rule.
    ///
    /// When `dotglob` is false (the default) a leading `.` in a path component
    /// is matched only by a segment that explicitly begins with a literal `.`:
    /// bare wildcards (`*`, `?`, `[…]`) and globstar (`**`) skip dot entries, so
    /// `*` hides dotfiles while `.*`, `.github`, and `**/.env` reach them.
    /// `dotglob == true` disables the rule (bash `shopt -s dotglob`).
    ///
    /// This differs from [`matches`](Self::matches), which is dotfile-agnostic
    /// and used for include/exclude filtering of already-walked paths.
    pub fn matches_walk(&self, path: &Path, dotglob: bool) -> bool {
        let components: Vec<&str> = path
            .components()
            .filter_map(|c| c.as_os_str().to_str())
            .collect();
        let mut budget = MatchBudget::new(self.segments.len(), components.len(), self.has_globstar());
        self.walk_match(&components, 0, 0, dotglob, false, &mut budget)
    }

    /// Whether the walker should descend into the directory at relative path
    /// `dir` — i.e. whether some entry beneath it could still match.
    ///
    /// Honours the same leading-dot rule as [`matches_walk`](Self::matches_walk):
    /// `**` does not descend into hidden directories without `dotglob`, while an
    /// explicitly named dot directory (`.github`, or a `.foo` segment reached
    /// through a zero-width `**`) is entered.
    pub fn could_descend(&self, dir: &Path, dotglob: bool) -> bool {
        let components: Vec<&str> = dir
            .components()
            .filter_map(|c| c.as_os_str().to_str())
            .collect();
        let mut budget = MatchBudget::new(self.segments.len(), components.len(), self.has_globstar());
        self.walk_match(&components, 0, 0, dotglob, true, &mut budget)
    }

    /// Shared engine for [`matches_walk`] and [`could_descend`].
    ///
    /// In full-match mode (`prefix == false`) it answers "does this complete
    /// path match?". In prefix mode (`prefix == true`) `components` are a
    /// directory's path and it answers "could a deeper entry match?", which the
    /// walker uses to decide descent.
    fn walk_match(
        &self,
        components: &[&str],
        seg_idx: usize,
        comp_idx: usize,
        dotglob: bool,
        prefix: bool,
        budget: &mut MatchBudget,
    ) -> bool {
        if comp_idx >= components.len() {
            return if prefix {
                // Directory prefix fully consumed: descend if any segment
                // remains for a child component to match.
                seg_idx < self.segments.len()
            } else {
                // Full match: only trailing globstars may match zero components.
                self.segments[seg_idx..]
                    .iter()
                    .all(|s| matches!(s, PathSegment::Globstar))
            };
        }
        if seg_idx >= self.segments.len() {
            return false;
        }
        if budget.is_failed(seg_idx, comp_idx) || !budget.tick() {
            return false;
        }

        let matched = match &self.segments[seg_idx] {
            PathSegment::Globstar => {
                // Match zero components...
                if self.walk_match(components, seg_idx + 1, comp_idx, dotglob, prefix, budget) {
                    true
                } else if dotglob || !components[comp_idx].starts_with('.') {
                    // ...or consume one component and stay on the globstar. Without
                    // dotglob, `**` never traverses a hidden component.
                    self.walk_match(components, seg_idx, comp_idx + 1, dotglob, prefix, budget)
                } else {
                    false
                }
            }

            PathSegment::Literal(lit) => {
                if components[comp_idx] == *lit {
                    self.walk_match(
                        components,
                        seg_idx + 1,
                        comp_idx + 1,
                        dotglob,
                        prefix,
                        budget,
                    )
                } else {
                    false
                }
            }

            PathSegment::Pattern(pat) => {
                let comp = components[comp_idx];
                // A bare wildcard segment does not match a leading dot.
                if comp.starts_with('.') && !dotglob && !pattern_leads_with_dot(pat) {
                    false
                } else if self.matches_component(pat, comp) {
                    self.walk_match(
                        components,
                        seg_idx + 1,
                        comp_idx + 1,
                        dotglob,
                        prefix,
                        budget,
                    )
                } else {
                    false
                }
            }
        };

        if !matched {
            budget.mark_failed(seg_idx, comp_idx);
        }
        matched
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
        budget: &mut MatchBudget,
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

        if budget.is_failed(seg_idx, comp_idx) || !budget.tick() {
            return false;
        }

        let matched = match &segments[seg_idx] {
            PathSegment::Globstar => {
                // Globstar matches zero or more components
                // Try matching with 0, 1, 2, ... components consumed
                let mut found = false;
                for skip in 0..=(components.len() - comp_idx) {
                    if self.match_segments(
                        segments,
                        components,
                        seg_idx + 1,
                        comp_idx + skip,
                        budget,
                    ) {
                        found = true;
                        break;
                    }
                }
                found
            }

            PathSegment::Literal(lit) => {
                if comp_idx >= components.len() {
                    false
                } else if components[comp_idx] == lit {
                    self.match_segments(segments, components, seg_idx + 1, comp_idx + 1, budget)
                } else {
                    false
                }
            }

            PathSegment::Pattern(pat) => {
                if comp_idx >= components.len() {
                    false
                } else if self.matches_component(pat, components[comp_idx]) {
                    self.match_segments(segments, components, seg_idx + 1, comp_idx + 1, budget)
                } else {
                    false
                }
            }
        };

        if !matched {
            budget.mark_failed(seg_idx, comp_idx);
        }
        matched
    }

    /// Match a single component against a pattern (with brace expansion).
    fn matches_component(&self, pattern: &str, component: &str) -> bool {
        glob_match(pattern, component)
    }
}

/// Whether a wildcard segment explicitly names a leading dot — i.e. some brace
/// alternative begins with a literal `.` (`.*`, `.[bg]it`, `{.,}config`). A
/// leading wildcard (`*`, `?`, `[…]`) does not count — matching bash, even a
/// character class that *could* match `.` (`[.]foo`) does not, because the
/// first pattern character is `[`, not a literal `.`.
fn pattern_leads_with_dot(pattern: &str) -> bool {
    crate::glob::expand_braces(pattern)
        .iter()
        .any(|alt| alt.starts_with('.'))
}

const MAX_GLOB_STEPS: usize = 100_000;
const INLINE_MEMO_BITS: usize = 256;
const MAX_MEMO_CELLS: usize = 65_536;

enum FailedMemo {
    None,
    /// Inline 256-bit set on the stack so common globstar paths allocate nothing.
    Inline([u64; 4]),
    Heap(Vec<bool>),
}

/// Memoizes failed `(seg_idx, comp_idx)` states for globstar patterns so
/// repeated `**` backtracking stays O(segments × components) without false
/// negatives up to `MAX_MEMO_CELLS`. Beyond that cell table cap, `MAX_GLOB_STEPS`
/// bounds total work so pathological inputs cannot hang.
struct MatchBudget {
    steps: usize,
    stride: usize,
    failed: FailedMemo,
}

impl MatchBudget {
    fn new(seg_count: usize, comp_count: usize, has_globstar: bool) -> Self {
        let cols = comp_count.saturating_add(1);
        let failed = if !has_globstar {
            FailedMemo::None
        } else {
            let cells = seg_count.saturating_add(1).saturating_mul(cols);
            if cells <= INLINE_MEMO_BITS {
                FailedMemo::Inline([0; 4])
            } else if cells <= MAX_MEMO_CELLS {
                FailedMemo::Heap(vec![false; cells])
            } else {
                FailedMemo::None
            }
        };
        Self {
            steps: 0,
            stride: cols,
            failed,
        }
    }

    fn is_failed(&self, seg_idx: usize, comp_idx: usize) -> bool {
        match &self.failed {
            FailedMemo::None => false,
            FailedMemo::Inline(bits) => {
                let idx = seg_idx * self.stride + comp_idx;
                let word = idx / 64;
                word < bits.len() && (bits[word] & (1u64 << (idx % 64))) != 0
            }
            FailedMemo::Heap(vec) => vec.get(seg_idx * self.stride + comp_idx).copied().unwrap_or(false),
        }
    }

    fn mark_failed(&mut self, seg_idx: usize, comp_idx: usize) {
        match &mut self.failed {
            FailedMemo::None => {}
            FailedMemo::Inline(bits) => {
                let idx = seg_idx * self.stride + comp_idx;
                let word = idx / 64;
                if word < bits.len() {
                    bits[word] |= 1u64 << (idx % 64);
                }
            }
            FailedMemo::Heap(vec) => {
                if let Some(slot) = vec.get_mut(seg_idx * self.stride + comp_idx) {
                    *slot = true;
                }
            }
        }
    }

    fn tick(&mut self) -> bool {
        if self.steps >= MAX_GLOB_STEPS {
            return false;
        }
        self.steps += 1;
        true
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
    fn test_matches_walk_leading_dot_rule() {
        let no = false; // dotglob off

        // Bare wildcard skips dotfiles; explicit dot segment matches them.
        assert!(!GlobPath::new("*").unwrap().matches_walk(Path::new(".env"), no));
        assert!(GlobPath::new("*").unwrap().matches_walk(Path::new("visible"), no));
        assert!(GlobPath::new(".*").unwrap().matches_walk(Path::new(".env"), no));
        assert!(!GlobPath::new(".*").unwrap().matches_walk(Path::new("visible"), no));

        // Explicit dot directory, and the `*` inside still hides dotfiles.
        assert!(GlobPath::new(".github/*").unwrap().matches_walk(Path::new(".github/ci.yml"), no));
        assert!(!GlobPath::new(".github/*").unwrap().matches_walk(Path::new(".github/.secret"), no));

        // Globstar does not match or traverse hidden components without dotglob.
        assert!(!GlobPath::new("**/*.rs").unwrap().matches_walk(Path::new(".hidden.rs"), no));
        assert!(!GlobPath::new("**/*.rs").unwrap().matches_walk(Path::new(".git/config.rs"), no));
        assert!(GlobPath::new("**/*.rs").unwrap().matches_walk(Path::new("src/main.rs"), no));

        // Explicit dot segment AFTER a globstar (the regression DeepSeek found).
        assert!(GlobPath::new("**/.env").unwrap().matches_walk(Path::new(".env"), no));
        assert!(GlobPath::new("**/.env").unwrap().matches_walk(Path::new("sub/.env"), no));
        assert!(!GlobPath::new("**/.env").unwrap().matches_walk(Path::new(".hidden/.env"), no));
        assert!(GlobPath::new("**/.github/*.yml").unwrap()
            .matches_walk(Path::new(".github/ci.yml"), no));
        assert!(GlobPath::new("**/.github/*.yml").unwrap()
            .matches_walk(Path::new("sub/.github/ci.yml"), no));

        // dotglob disables the rule (bash `shopt -s dotglob`).
        assert!(GlobPath::new("*").unwrap().matches_walk(Path::new(".env"), true));
        assert!(GlobPath::new("**/*.rs").unwrap().matches_walk(Path::new(".git/config.rs"), true));
    }

    #[test]
    fn test_could_descend_leading_dot_rule() {
        let no = false;

        // `**` descends into visible dirs but not hidden ones.
        assert!(GlobPath::new("**/.env").unwrap().could_descend(Path::new("sub"), no));
        assert!(!GlobPath::new("**/.env").unwrap().could_descend(Path::new(".hidden"), no));

        // An explicitly named dot dir is entered, including through zero-width `**`.
        assert!(GlobPath::new(".github/*").unwrap().could_descend(Path::new(".github"), no));
        assert!(GlobPath::new("**/.github/*.yml").unwrap()
            .could_descend(Path::new(".github"), no));

        // Bare `*` (fixed depth 1) needs no descent; `**` enters visible dirs.
        assert!(!GlobPath::new("*").unwrap().could_descend(Path::new("sub"), no));
        assert!(GlobPath::new("src/*.rs").unwrap().could_descend(Path::new("src"), no));
        assert!(!GlobPath::new("src/*.rs").unwrap().could_descend(Path::new("other"), no));

        // dotglob lets `**` descend into hidden dirs.
        assert!(GlobPath::new("**/*.rs").unwrap().could_descend(Path::new(".git"), true));
        assert!(!GlobPath::new("**/*.rs").unwrap().could_descend(Path::new(".git"), no));
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

    #[test]
    fn test_pathological_multi_globstar_terminates_fast() {
        let deep = Path::new("a/b/c/d/e/f/g/h/i/j/k/l/m/n/o/p/q/r/s/t/u/v/w/x/y/z");
        let pat = GlobPath::new("**/**/**/**/**/**/**/**/nope").unwrap();
        assert!(!pat.matches(deep));
        assert!(!pat.matches_walk(deep, false));
        assert!(pat.could_descend(deep, false));

        let pat_alt = GlobPath::new("**/*/**/**/*/**/**/*/**/z").unwrap();
        assert!(pat_alt.matches(deep));
        assert!(pat_alt.matches_walk(deep, false));
    }
}
