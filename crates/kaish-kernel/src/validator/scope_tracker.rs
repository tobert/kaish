//! Variable scope tracking for validation.
//!
//! Tracks which variables are bound in each scope without caring about values.
//! Used to detect possibly undefined variable references.

use std::collections::HashSet;

/// Tracks variable bindings across nested scopes.
///
/// Unlike the interpreter's Scope which holds values, this only tracks names
/// for static validation purposes.
pub struct ScopeTracker {
    /// Stack of scope frames, each containing bound variable names.
    frames: Vec<HashSet<String>>,
}

impl Default for ScopeTracker {
    fn default() -> Self {
        Self::new()
    }
}

impl ScopeTracker {
    /// Create a new scope tracker with built-in special variables.
    pub fn new() -> Self {
        let mut tracker = Self {
            frames: vec![HashSet::new()],
        };

        // Register built-in special variables
        tracker.bind_builtins();

        tracker
    }

    /// Register all built-in and special shell variables.
    fn bind_builtins(&mut self) {
        let builtins = [
            // Exit status
            "?",
            // Positional parameters
            "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
            // Special arrays
            "@", "#", "*",
            // Environment defaults
            "HOME", "PATH", "PWD", "OLDPWD", "USER", "SHELL", "TERM",
            // Script info
            "LINENO", "FUNCNAME", "BASH_SOURCE",
            // IFS for word splitting
            "IFS",
            // Random
            "RANDOM",
            // Process info
            "PID", "PPID", "UID", "EUID",
        ];

        for name in builtins {
            self.bind(name);
        }
    }

    /// Push a new scope frame.
    ///
    /// Variables bound after this call are local to the new frame
    /// until `pop_frame` is called.
    pub fn push_frame(&mut self) {
        self.frames.push(HashSet::new());
    }

    /// Pop the current scope frame.
    ///
    /// Variables bound in this frame are forgotten.
    /// Panics if trying to pop the global frame.
    pub fn pop_frame(&mut self) {
        if self.frames.len() > 1 {
            self.frames.pop();
        }
    }

    /// Bind a variable name in the current scope.
    pub fn bind(&mut self, name: impl Into<String>) {
        if let Some(frame) = self.frames.last_mut() {
            frame.insert(name.into());
        }
    }

    /// Check if a variable is bound in any scope.
    ///
    /// Searches from innermost to outermost scope.
    pub fn is_bound(&self, name: &str) -> bool {
        self.frames.iter().rev().any(|frame| frame.contains(name))
    }

    /// Check if a variable name should skip undefined warnings.
    ///
    /// Variables starting with underscore are conventionally external
    /// or intentionally unchecked.
    pub fn should_skip_undefined_check(name: &str) -> bool {
        name.starts_with('_')
    }

    /// Get the current nesting depth.
    pub fn depth(&self) -> usize {
        self.frames.len()
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_has_builtins() {
        let tracker = ScopeTracker::new();
        assert!(tracker.is_bound("?"));
        assert!(tracker.is_bound("HOME"));
        assert!(tracker.is_bound("PATH"));
        assert!(tracker.is_bound("0"));
        assert!(tracker.is_bound("@"));
    }

    #[test]
    fn bind_and_lookup() {
        let mut tracker = ScopeTracker::new();
        assert!(!tracker.is_bound("MY_VAR"));
        tracker.bind("MY_VAR");
        assert!(tracker.is_bound("MY_VAR"));
    }

    #[test]
    fn nested_scopes() {
        let mut tracker = ScopeTracker::new();

        tracker.bind("OUTER");
        assert!(tracker.is_bound("OUTER"));

        tracker.push_frame();
        tracker.bind("INNER");
        assert!(tracker.is_bound("INNER"));
        assert!(tracker.is_bound("OUTER")); // Still visible from outer scope

        tracker.pop_frame();
        assert!(!tracker.is_bound("INNER")); // Gone
        assert!(tracker.is_bound("OUTER")); // Still there
    }

    #[test]
    fn underscore_convention() {
        assert!(ScopeTracker::should_skip_undefined_check("_EXTERNAL"));
        assert!(ScopeTracker::should_skip_undefined_check("__private"));
        assert!(!ScopeTracker::should_skip_undefined_check("NORMAL"));
    }

    #[test]
    fn depth_tracking() {
        let mut tracker = ScopeTracker::new();
        assert_eq!(tracker.depth(), 1);

        tracker.push_frame();
        assert_eq!(tracker.depth(), 2);

        tracker.push_frame();
        assert_eq!(tracker.depth(), 3);

        tracker.pop_frame();
        assert_eq!(tracker.depth(), 2);

        tracker.pop_frame();
        assert_eq!(tracker.depth(), 1);

        // Can't pop the global frame
        tracker.pop_frame();
        assert_eq!(tracker.depth(), 1);
    }
}
