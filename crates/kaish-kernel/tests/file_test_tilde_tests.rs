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

// --- relative paths resolve against the session cwd (GH #101) ---------------

/// A bare relative path in a `[[ ]]` file test must stat against the session
/// cwd, not the process cwd. `eval_test_async` used to hand the raw string to
/// `backend.stat`, so `[[ -f rel ]]` silently missed a file that exists in the
/// shell's cwd — a wrong `false` flowing into `if`/`&&`.
#[tokio::test]
async fn file_test_relative_path_resolves_against_session_cwd() {
    let dir = tempdir();
    std::fs::write(dir.path().join("here.txt"), b"hi").unwrap();
    let kernel = kernel_at(dir.path());

    let out = kernel
        .execute("[[ -f here.txt ]] && echo 'yes' || echo 'no'")
        .await
        .unwrap();
    assert_eq!(out.text_out().trim(), "yes", "relative -f ignored the session cwd");
}

/// The reported repro (GH #101): after `cd sub`, a relative `[[ -f f ]]` must
/// honor the new cwd.
#[tokio::test]
async fn file_test_honors_cd() {
    let dir = tempdir();
    std::fs::create_dir(dir.path().join("sub")).unwrap();
    std::fs::write(dir.path().join("sub").join("f.txt"), b"hi").unwrap();
    let kernel = kernel_at(dir.path());

    let out = kernel
        .execute("cd sub; [[ -f f.txt ]] && echo 'yes' || echo 'no'")
        .await
        .unwrap();
    assert_eq!(out.text_out().trim(), "yes", "[[ -f ]] did not honor cd");
}

/// Differential guard: `[[ -f rel ]]` and the VFS-aware `test -f rel` must
/// agree on a relative path after `cd` (they disagreed before this fix —
/// `test` resolved the cwd, `[[` did not).
#[tokio::test]
async fn double_bracket_and_test_agree_on_relative_path() {
    let dir = tempdir();
    std::fs::create_dir(dir.path().join("sub")).unwrap();
    std::fs::write(dir.path().join("sub").join("f.txt"), b"hi").unwrap();
    let kernel = kernel_at(dir.path());

    let out = kernel
        .execute(
            "cd sub\n\
             [[ -f f.txt ]]; b=$?\n\
             test -f f.txt; t=$?\n\
             [[ $b -eq $t ]] && echo agree || echo \"disagree b=$b t=$t\"",
        )
        .await
        .unwrap();
    assert_eq!(
        out.text_out().trim(),
        "agree",
        "[[ -f ]] and test -f disagreed on a relative path"
    );
}
