//! The walk-filter flag family: `--include`/`--exclude` must actually filter
//! (including when repeated), directories must still be traversed when an
//! include list is present, and a malformed depth/level/timeout value must be
//! loud — never a silent "unlimited"/"disabled" fallback.
//!
//! Pre-fix behavior this pins against regressing:
//! - `glob --include` was a complete no-op (the walker only consulted
//!   exclude rules).
//! - repeating `--include`/`--exclude` silently kept only the LAST value
//!   (bound `Option<String>`, last-write-wins) while glob's help claimed
//!   "can be repeated" — grep's variant produced silent false negatives.
//! - `glob --depth=abc`, `tree -L abc`, `find -maxdepth xyz` exited 0 and
//!   walked unlimited; `spawn timeout=abc` silently disabled the timeout.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::{Kernel, KernelConfig};
use std::fs;
use std::path::Path;

fn kernel_at(dir: &Path) -> Kernel {
    let config = KernelConfig::repl().with_cwd(dir.to_path_buf());
    Kernel::new(config).expect("kernel")
}

async fn run(kernel: &Kernel, script: &str) -> ExecResult {
    kernel.execute(script).await.expect("kernel execute")
}

/// A small tree with three file types and a subdirectory: TODO markers in
/// one file per extension so grep hits are attributable.
fn seed(dir: &std::path::Path) {
    fs::write(dir.join("a.rs"), "TODO alpha\n").unwrap();
    fs::write(dir.join("b.rs"), "beta\n").unwrap();
    fs::write(dir.join("c.toml"), "TODO gamma\n").unwrap();
    fs::write(dir.join("d.md"), "TODO delta\n").unwrap();
    fs::create_dir(dir.join("sub")).unwrap();
    fs::write(dir.join("sub/e.rs"), "TODO epsilon\n").unwrap();
}

// ─── glob: include/exclude do what they say ─────────────────────────────────

#[tokio::test]
async fn glob_include_actually_filters() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    let r = run(&kernel, "glob '*' --include='*.rs'").await;
    assert_eq!(r.code, 0, "glob succeeds: {}", r.err);
    let out = r.text_out();
    assert!(out.contains("a.rs") && out.contains("b.rs"), "keeps .rs: {out}");
    assert!(!out.contains("c.toml") && !out.contains("d.md"), "--include filters: {out}");
}

#[tokio::test]
async fn glob_include_repeats_accumulate() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    let r = run(&kernel, "glob '*' --include='*.rs' --include='*.toml'").await;
    assert_eq!(r.code, 0, "glob succeeds: {}", r.err);
    let out = r.text_out();
    assert!(out.contains("a.rs") && out.contains("c.toml"), "both includes kept: {out}");
    assert!(!out.contains("d.md"), "non-included dropped: {out}");
}

#[tokio::test]
async fn glob_exclude_repeats_accumulate() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    let r = run(&kernel, "glob '*' --exclude='a.rs' --exclude='b.rs'").await;
    assert_eq!(r.code, 0, "glob succeeds: {}", r.err);
    let out = r.text_out();
    assert!(
        !out.contains("a.rs") && !out.contains("b.rs"),
        "BOTH excludes apply (first one used to be silently dropped): {out}"
    );
    assert!(out.contains("c.toml"), "non-excluded survive: {out}");
}

/// An include list must not stop directory traversal: `sub` matches no
/// include pattern but the `.rs` file inside it must still be found.
#[tokio::test]
async fn glob_include_does_not_block_recursion() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    let r = run(&kernel, "glob '**/*' --include='*.rs'").await;
    assert_eq!(r.code, 0, "glob succeeds: {}", r.err);
    let out = r.text_out();
    assert!(out.contains("e.rs"), "recursion reaches sub/e.rs: {out}");
    assert!(!out.contains("c.toml"), "include still filters files: {out}");
}

/// Excluding a directory name still prunes the whole subtree.
#[tokio::test]
async fn glob_exclude_still_prunes_directories() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    let r = run(&kernel, "glob '**/*.rs' --exclude=sub").await;
    assert_eq!(r.code, 0, "glob succeeds: {}", r.err);
    let out = r.text_out();
    assert!(out.contains("a.rs"), "top-level files kept: {out}");
    assert!(!out.contains("e.rs"), "excluded dir pruned: {out}");
}

// ─── loud bad values ─────────────────────────────────────────────────────────

#[tokio::test]
async fn glob_bad_depth_is_loud() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    let r = run(&kernel, "glob '*' --depth=abc").await;
    assert_eq!(r.code, 2, "bad depth is a usage error, not unlimited: {r:?}");
    assert!(r.err.contains("depth"), "names the flag: {}", r.err);

    let neg = run(&kernel, "glob '*' --depth=-1").await;
    assert_eq!(neg.code, 2, "negative depth is refused, not wrapped: {neg:?}");
}

#[tokio::test]
async fn tree_bad_level_is_loud() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    let r = run(&kernel, "tree -L abc .").await;
    assert_eq!(r.code, 2, "bad -L is a usage error, not unlimited: {r:?}");

    let neg = run(&kernel, "tree --level=-3 .").await;
    assert_eq!(neg.code, 2, "negative level refused: {neg:?}");
}

#[tokio::test]
async fn find_bad_maxdepth_is_loud() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    let r = run(&kernel, "find . -maxdepth xyz").await;
    assert_ne!(r.code, 0, "bad -maxdepth errors instead of walking unlimited: {r:?}");
    assert!(r.err.contains("maxdepth"), "names the flag: {}", r.err);

    let r = run(&kernel, "find . -mindepth xyz").await;
    assert_ne!(r.code, 0, "bad -mindepth errors instead of no-filter: {r:?}");
}

#[cfg(all(unix, feature = "subprocess"))]
#[tokio::test]
async fn spawn_bad_timeout_is_loud() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    // The parse error fires before any process is spawned.
    let r = run(&kernel, "spawn --command=/bin/true --timeout=abc").await;
    assert_eq!(r.code, 2, "bad timeout refuses instead of running unbounded: {r:?}");
    assert!(r.err.contains("timeout"), "names the flag: {}", r.err);

    let neg = run(&kernel, "spawn --command=/bin/true --timeout=-5").await;
    assert_eq!(neg.code, 2, "negative timeout refused, not u64-wrapped: {neg:?}");
}

// ─── grep: repeated include/exclude across a recursive walk ─────────────────

#[tokio::test]
async fn grep_include_repeats_accumulate() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    let r = run(&kernel, "grep -rn TODO . --include='*.rs' --include='*.toml'").await;
    assert_eq!(r.code, 0, "matches exist: {r:?}");
    let out = r.text_out();
    assert!(out.contains("a.rs"), "first include searched (used to be silently dropped): {out}");
    assert!(out.contains("c.toml"), "second include searched: {out}");
    assert!(out.contains("e.rs"), "recursion under include list still works: {out}");
    assert!(!out.contains("d.md"), "non-included files not searched: {out}");
}

#[tokio::test]
async fn grep_exclude_repeats_accumulate() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    let r = run(&kernel, "grep -rn TODO . --exclude='*.toml' --exclude='*.md'").await;
    assert_eq!(r.code, 0, "matches exist: {r:?}");
    let out = r.text_out();
    assert!(out.contains("a.rs") && out.contains("e.rs"), ".rs TODOs found: {out}");
    assert!(
        !out.contains("c.toml") && !out.contains("d.md"),
        "BOTH excludes apply: {out}"
    );
}

#[tokio::test]
async fn grep_single_include_still_works() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    let r = run(&kernel, "grep -rn TODO . --include='*.rs'").await;
    assert_eq!(r.code, 0, "matches exist: {r:?}");
    let out = r.text_out();
    assert!(out.contains("a.rs") && out.contains("e.rs"), "rs files searched: {out}");
    assert!(!out.contains("c.toml"), "single include filters: {out}");
}
