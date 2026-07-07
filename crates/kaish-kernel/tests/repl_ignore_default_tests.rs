//! The reference REPL's kernel preset is ignore-aware by default (GH #134):
//! `KernelConfig::repl()` loads `.gitignore` and the default ignore list
//! (.git, target, node_modules, …) at **Advisory** scope — the polite walkers
//! (glob, tree, grep -r, ls) filter, `find` stays POSIX-unrestricted, and
//! both `--no-ignore` and `kaish-ignore clear` recover the unfiltered view
//! per call / per session.

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

/// A repo-shaped fixture: a kept source file, a .gitignore'd log, and a
/// default-ignored target/ dir — with a NEEDLE in each searchable file.
fn seed(dir: &Path) {
    fs::write(dir.join("keep.rs"), "NEEDLE in keep\n").unwrap();
    fs::write(dir.join("secret.log"), "NEEDLE in secret\n").unwrap();
    fs::write(dir.join(".gitignore"), "secret.log\n").unwrap();
    fs::create_dir(dir.join("target")).unwrap();
    fs::write(dir.join("target/dep.rs"), "NEEDLE in dep\n").unwrap();
}

#[tokio::test]
async fn repl_glob_respects_gitignore_and_defaults() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    let r = run(&kernel, "glob '**/*'").await;
    assert_eq!(r.code, 0, "glob succeeds: {}", r.err);
    let out = r.text_out();
    assert!(out.contains("keep.rs"), "visible file listed: {out}");
    assert!(!out.contains("secret.log"), ".gitignore respected: {out}");
    assert!(!out.contains("dep.rs"), "default ignore (target/) respected: {out}");
}

#[tokio::test]
async fn repl_grep_respects_ignores() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    let r = run(&kernel, "grep -rn NEEDLE .").await;
    assert_eq!(r.code, 0, "match exists: {r:?}");
    let out = r.text_out();
    assert!(out.contains("keep.rs"), "visible match found: {out}");
    assert!(!out.contains("secret.log"), "ignored file not searched: {out}");
    assert!(!out.contains("dep.rs"), "target/ not searched: {out}");
}

/// Advisory scope: `--no-ignore` recovers the unfiltered view per call.
#[tokio::test]
async fn no_ignore_flag_recovers_everything() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    let r = run(&kernel, "glob '**/*' --no-ignore").await;
    assert_eq!(r.code, 0, "glob succeeds: {}", r.err);
    let out = r.text_out();
    assert!(out.contains("secret.log"), "--no-ignore sees gitignored files: {out}");
    assert!(out.contains("dep.rs"), "--no-ignore sees target/: {out}");
}

/// Advisory scope: `find` keeps its POSIX everything-visible tradition.
#[tokio::test]
async fn find_stays_unrestricted_under_advisory() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    let r = run(&kernel, "find .").await;
    assert_eq!(r.code, 0, "find succeeds: {}", r.err);
    let out = r.text_out();
    assert!(out.contains("secret.log"), "find sees gitignored files: {out}");
    assert!(out.contains("dep.rs"), "find walks target/: {out}");
}

/// The session-wide opt-out still works: `kaish-ignore clear` disables all
/// filtering at runtime.
#[tokio::test]
async fn kaish_ignore_clear_disables_filtering() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    run(&kernel, "kaish-ignore clear").await;
    let r = run(&kernel, "glob '**/*'").await;
    assert_eq!(r.code, 0, "glob succeeds: {}", r.err);
    let out = r.text_out();
    assert!(out.contains("secret.log"), "clear disables .gitignore: {out}");
    assert!(out.contains("dep.rs"), "clear disables defaults: {out}");
}
