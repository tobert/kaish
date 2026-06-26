//! Kernel-routed tests for tilde expansion in `[[ ]]` file tests.
//!
//! `[[ -f ~/x ]]` must expand `~` to the session HOME before stat'ing, the
//! same way argv positionals do. The async `eval_test_async` path (the real
//! kernel path) used to stat the literal `~/x`, so every `~`-prefixed file test
//! was false. See docs/issues.md P2 "Interpreter trio".

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

use std::collections::HashMap;
use std::path::Path;

use kaish_kernel::ast::Value;
use kaish_kernel::{Kernel, KernelConfig};

fn tempdir() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("file-test-tilde-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

/// A localfs kernel with the given cwd and HOME (HOME comes from the frontend
/// via `initial_vars`, the way the REPL seeds it).
fn kernel_with_cwd_home(cwd: &Path, home: &Path) -> Kernel {
    let mut vars = HashMap::new();
    vars.insert(
        "HOME".to_string(),
        Value::String(home.to_string_lossy().into_owned()),
    );
    let config = KernelConfig::repl()
        .with_cwd(cwd.to_path_buf())
        .with_initial_vars(vars)
        .with_latch(false)
        .with_trash(false);
    Kernel::new(config).expect("kernel")
}

/// The common case: cwd and HOME are the same directory.
fn kernel_at(dir: &Path) -> Kernel {
    kernel_with_cwd_home(dir, dir)
}

#[tokio::test]
async fn file_test_expands_tilde_for_existing_file() {
    let dir = tempdir();
    std::fs::write(dir.path().join("present.txt"), b"hi").unwrap();
    let kernel = kernel_at(dir.path());

    let out = kernel
        .execute("[[ -f ~/present.txt ]] && echo 'yes' || echo 'no'")
        .await
        .unwrap();
    assert_eq!(out.text_out().trim(), "yes", "~ was not expanded in -f");
}

#[tokio::test]
async fn file_test_tilde_dir() {
    let dir = tempdir();
    std::fs::create_dir(dir.path().join("sub")).unwrap();
    let kernel = kernel_at(dir.path());

    let out = kernel
        .execute("[[ -d ~/sub ]] && echo 'yes' || echo 'no'")
        .await
        .unwrap();
    assert_eq!(out.text_out().trim(), "yes", "~ was not expanded in -d");
}

#[tokio::test]
async fn file_test_tilde_resolves_against_home_not_cwd() {
    // HOME and cwd are *different* dirs; the file lives only under HOME. So
    // `~/only-in-home.txt` is found only if `~` expands to HOME:
    //   - correct impl  → stats `<HOME>/only-in-home.txt` (present) → "yes"
    //   - non-expanding → stats the literal `~/only-in-home.txt` relative to
    //     cwd (absent there) → "no"
    // This fails if tilde expansion is ever dropped (the absent-file form is
    // not differential — a literal `~/x` is absent for the wrong reason).
    let home = tempdir();
    let cwd = tempdir();
    std::fs::write(home.path().join("only-in-home.txt"), b"hi").unwrap();
    let kernel = kernel_with_cwd_home(cwd.path(), home.path());

    let out = kernel
        .execute("[[ -f ~/only-in-home.txt ]] && echo 'yes' || echo 'no'")
        .await
        .unwrap();
    assert_eq!(
        out.text_out().trim(),
        "yes",
        "`~` did not resolve against the session HOME"
    );
}
