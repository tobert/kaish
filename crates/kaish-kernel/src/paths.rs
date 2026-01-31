//! XDG Base Directory paths for kaish and embedders.
//!
//! This module provides two layers of path helpers:
//!
//! 1. **XDG primitives** — Generic XDG base directories that embedders can use
//!    to compose their own application-specific paths.
//!
//! 2. **kaish-specific paths** — Convenience functions for kaish's own paths,
//!    built on top of the primitives.
//!
//! # XDG Base Directory Specification
//!
//! | Purpose | XDG Variable | Default |
//! |---------|--------------|---------|
//! | Runtime | `$XDG_RUNTIME_DIR` | `/run/user/$UID` or `/tmp` |
//! | Data | `$XDG_DATA_HOME` | `~/.local/share` |
//! | Config | `$XDG_CONFIG_HOME` | `~/.config` |
//! | Cache | `$XDG_CACHE_HOME` | `~/.cache` |
//!
//! # Example: Embedder Path Composition
//!
//! ```
//! use kaish_kernel::paths::{xdg_data_home, home_dir};
//! use std::path::PathBuf;
//!
//! // Embedders compose their own paths on top of XDG primitives
//! fn my_app_data_dir() -> PathBuf {
//!     xdg_data_home().join("myapp")
//! }
//!
//! fn my_app_worktrees_dir() -> PathBuf {
//!     my_app_data_dir().join("worktrees")
//! }
//! ```

use std::path::PathBuf;

use directories::BaseDirs;

// ═══════════════════════════════════════════════════════════════════════════
// XDG Primitives — For embedders to compose their own paths
// ═══════════════════════════════════════════════════════════════════════════

/// Get the user's home directory.
///
/// Returns `$HOME` or falls back to `/tmp` if not set.
///
/// # Example
///
/// ```
/// use kaish_kernel::paths::home_dir;
///
/// let home = home_dir();
/// assert!(home.is_absolute());
/// ```
pub fn home_dir() -> PathBuf {
    std::env::var("HOME")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("/tmp"))
}

/// Get XDG data home directory.
///
/// Returns `$XDG_DATA_HOME` or falls back to `~/.local/share`.
///
/// Embedders use this to compose their own data paths:
/// ```
/// use kaish_kernel::paths::xdg_data_home;
///
/// let myapp_data = xdg_data_home().join("myapp");
/// ```
pub fn xdg_data_home() -> PathBuf {
    BaseDirs::new()
        .map(|d| d.data_dir().to_path_buf())
        .unwrap_or_else(|| home_dir().join(".local").join("share"))
}

/// Get XDG config home directory.
///
/// Returns `$XDG_CONFIG_HOME` or falls back to `~/.config`.
///
/// Embedders use this to compose their own config paths:
/// ```
/// use kaish_kernel::paths::xdg_config_home;
///
/// let myapp_config = xdg_config_home().join("myapp");
/// ```
pub fn xdg_config_home() -> PathBuf {
    BaseDirs::new()
        .map(|d| d.config_dir().to_path_buf())
        .unwrap_or_else(|| home_dir().join(".config"))
}

/// Get XDG cache home directory.
///
/// Returns `$XDG_CACHE_HOME` or falls back to `~/.cache`.
///
/// Embedders use this to compose their own cache paths:
/// ```
/// use kaish_kernel::paths::xdg_cache_home;
///
/// let myapp_cache = xdg_cache_home().join("myapp");
/// ```
pub fn xdg_cache_home() -> PathBuf {
    BaseDirs::new()
        .map(|d| d.cache_dir().to_path_buf())
        .unwrap_or_else(|| home_dir().join(".cache"))
}

/// Get XDG runtime directory.
///
/// Returns `$XDG_RUNTIME_DIR` or falls back to system temp directory.
///
/// Embedders use this to compose their own runtime paths:
/// ```
/// use kaish_kernel::paths::xdg_runtime_dir;
///
/// let myapp_sockets = xdg_runtime_dir().join("myapp");
/// ```
pub fn xdg_runtime_dir() -> PathBuf {
    std::env::var("XDG_RUNTIME_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| std::env::temp_dir())
}

// ═══════════════════════════════════════════════════════════════════════════
// kaish-Specific Paths — Built on XDG primitives
// ═══════════════════════════════════════════════════════════════════════════

/// Get the kaish runtime directory for sockets.
///
/// Uses `$XDG_RUNTIME_DIR/kaish` or falls back to `/tmp/kaish`.
pub fn runtime_dir() -> PathBuf {
    xdg_runtime_dir().join("kaish")
}

/// Get the kaish data directory for persistent state.
///
/// Uses `$XDG_DATA_HOME/kaish` or falls back to `~/.local/share/kaish`.
pub fn data_dir() -> PathBuf {
    xdg_data_home().join("kaish")
}

/// Get the kaish config directory.
///
/// Uses `$XDG_CONFIG_HOME/kaish` or falls back to `~/.config/kaish`.
pub fn config_dir() -> PathBuf {
    xdg_config_home().join("kaish")
}

/// Get the kaish cache directory.
///
/// Uses `$XDG_CACHE_HOME/kaish` or falls back to `~/.cache/kaish`.
pub fn cache_dir() -> PathBuf {
    xdg_cache_home().join("kaish")
}

/// Get the kernels directory.
pub fn kernels_dir() -> PathBuf {
    data_dir().join("kernels")
}

#[cfg(test)]
mod tests {
    use super::*;

    // ═══════════════════════════════════════════════════════════════════════
    // XDG Primitive Tests
    // ═══════════════════════════════════════════════════════════════════════

    #[test]
    fn home_dir_is_absolute() {
        let home = home_dir();
        assert!(home.is_absolute());
    }

    #[test]
    fn xdg_data_home_defaults_to_local_share() {
        // When $XDG_DATA_HOME is not set, should be under home
        let data = xdg_data_home();
        assert!(data.is_absolute());
        // Should end with .local/share or be the XDG override
        let path_str = data.to_string_lossy();
        assert!(
            path_str.ends_with(".local/share") || std::env::var("XDG_DATA_HOME").is_ok(),
            "Expected .local/share or XDG override, got: {}",
            path_str
        );
    }

    #[test]
    fn xdg_config_home_defaults_to_config() {
        let config = xdg_config_home();
        assert!(config.is_absolute());
        let path_str = config.to_string_lossy();
        assert!(
            path_str.ends_with(".config") || std::env::var("XDG_CONFIG_HOME").is_ok(),
            "Expected .config or XDG override, got: {}",
            path_str
        );
    }

    #[test]
    fn xdg_cache_home_defaults_to_cache() {
        let cache = xdg_cache_home();
        assert!(cache.is_absolute());
        let path_str = cache.to_string_lossy();
        assert!(
            path_str.ends_with(".cache") || std::env::var("XDG_CACHE_HOME").is_ok(),
            "Expected .cache or XDG override, got: {}",
            path_str
        );
    }

    #[test]
    fn xdg_runtime_dir_is_absolute() {
        let runtime = xdg_runtime_dir();
        assert!(runtime.is_absolute());
    }

    // ═══════════════════════════════════════════════════════════════════════
    // kaish-Specific Path Tests
    // ═══════════════════════════════════════════════════════════════════════

    #[test]
    fn kaish_paths_are_under_kaish() {
        assert!(runtime_dir().ends_with("kaish"));
        assert!(data_dir().ends_with("kaish"));
        assert!(config_dir().ends_with("kaish"));
        assert!(cache_dir().ends_with("kaish"));
    }

    #[test]
    fn kaish_paths_build_on_xdg_primitives() {
        // kaish paths should be XDG base + "kaish"
        assert_eq!(data_dir(), xdg_data_home().join("kaish"));
        assert_eq!(config_dir(), xdg_config_home().join("kaish"));
        assert_eq!(cache_dir(), xdg_cache_home().join("kaish"));
        assert_eq!(runtime_dir(), xdg_runtime_dir().join("kaish"));
    }

    #[test]
    fn kernels_dir_is_under_data() {
        let kernels = kernels_dir();
        let data = data_dir();
        assert!(kernels.starts_with(&data));
        assert!(kernels.ends_with("kernels"));
    }
}
