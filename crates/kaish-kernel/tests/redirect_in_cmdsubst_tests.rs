//! Redirects inside command substitution: `$(cmd > file)`.
//!
//! Two coupled behaviors are pinned here:
//!
//! 1. **Grammar** — the `$()` body accepts trailing redirects. The command-sub
//!    grammar (`cmd_subst_parser`) used to hardcode `redirects: vec![]`, so a
//!    `>`/`>>`/`2>` inside `$()` was a parse error even though the top-level
//!    command grammar has accepted them all along.
//!
//! 2. **Capture semantics** — when a command's stdout is redirected to a file
//!    inside `$()`, the substitution captures the empty string (bash parity:
//!    the bytes went to the file, not to the capture). This must hold even for
//!    structured-data builtins: `apply_redirects` clears `.out`/`.output` AND
//!    `.data` on a stdout-overwrite/append, so a `.data`-producing command
//!    (e.g. `fromjson`) whose stdout was redirected does not leak its `.data`
//!    into the captured value. `Expr::CommandSubst` prefers `.data` over text,
//!    so a stale `.data` here would reintroduce the leak in the exact opposite
//!    direction from the quoted-`$()` interpolation bug.
//!
//! Real-FS via `tempfile::tempdir()` per the no-hardcoded-system-paths rule.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

mod common;

use common::kernel_at;

/// The grammar half: a stdout redirect inside `$()` parses and runs, and the
/// bytes land in the file — not the capture (which is empty, bash parity).
#[tokio::test]
async fn stdout_redirect_inside_cmdsubst_writes_file_and_captures_empty() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute(r#"x=$(echo hi > out.txt); echo "[$x]""#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "{r:?}");
    // stdout went to the file, so the capture is empty.
    assert_eq!(r.text_out().trim(), "[]", "capture must be empty: {r:?}");

    // and the file received the redirected output.
    let f = kernel.execute("cat out.txt").await.expect("cat");
    assert_eq!(f.text_out().trim(), "hi");
}

/// The append variant (`>>`) parses and behaves the same way for the capture.
#[tokio::test]
async fn stdout_append_redirect_inside_cmdsubst_captures_empty() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute(r#"echo a > out.txt; x=$(echo b >> out.txt); echo "[$x]""#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out().trim(), "[]", "capture must be empty: {r:?}");

    let f = kernel.execute("cat out.txt").await.expect("cat");
    assert_eq!(f.text_out(), "a\nb\n");
}

/// The capture-semantics half, at its sharpest: a real structured-data builtin
/// (`fromjson` sets BOTH `.out` text and `.data`) whose stdout is redirected
/// must NOT leak `.data` into the capture. Before the `.data`-clearing fix,
/// `x` captured `[1,2,3]` (from `.data`) instead of the empty string, so this
/// rendered `[[1,2,3]]`.
#[tokio::test]
async fn data_producing_builtin_redirect_inside_cmdsubst_does_not_leak_data() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute(r#"x=$(fromjson '[1,2,3]' > out.txt); echo "[$x]""#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(
        r.text_out().trim(),
        "[]",
        "fromjson's .data must not leak into the capture once stdout was redirected: {r:?}"
    );

    // The structured value still reached the file (canonical compact JSON).
    let f = kernel.execute("cat out.txt").await.expect("cat");
    assert_eq!(f.text_out().trim(), "[1,2,3]");
}

/// A *stderr* redirect inside `$()` must NOT touch the stdout capture: only
/// stdout-affecting redirects clear the captured value. Guards against an
/// over-broad clear that would empty every `$(cmd 2> file)` capture.
#[tokio::test]
async fn stderr_redirect_inside_cmdsubst_preserves_stdout_capture() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute(r#"x=$(echo hi 2> err.txt); echo "[$x]""#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(
        r.text_out().trim(),
        "[hi]",
        "a stderr redirect must leave the stdout capture intact: {r:?}"
    );
}

/// `1>&2` / `>&2` merges stdout into stderr — it is still a stdout redirect for
/// capture purposes, so a structured-data builtin's `.data` must NOT leak into
/// the `$()` capture (bash parity: stdout went to stderr, the capture is empty).
/// Regression guard for the `MergeStdout` arm clearing `.data`, not just `.out`.
#[tokio::test]
async fn merge_stdout_to_stderr_inside_cmdsubst_does_not_leak_data() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute(r#"x=$(fromjson '[1,2,3]' >&2); echo "[$x]""#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(
        r.text_out().trim(),
        "[]",
        "fromjson's .data must not leak into the capture when stdout is merged to stderr: {r:?}"
    );
}

// The sharpest test of the parser cycle-break — a `$(...)` in the redirect
// *target*, itself inside a `$(...)` — lives as a parser unit test
// (`parser::tests::parse_cmd_subst_redirect_target_with_nested_subst`): the
// hazard it guards is parser *construction* (infinite recursion / stack
// overflow), which is a parse-layer concern. End-to-end execution of a
// `$()`-valued redirect target on a *bare* single command is blocked by a
// separate, pre-existing bug (the dispatcher isn't attached, so the target
// `$()` can't run) — GH #90, orthogonal to redirect-in-`$()`.
