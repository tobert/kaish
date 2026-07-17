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

#![cfg(feature = "localfs")]
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

// ─── GH #90: `$()` in a redirect target on a *bare* single command ──────────
//
// The parser cycle-break for a `$(...)` in the redirect *target* itself nested
// inside a `$(...)` is pinned by a parser unit test
// (`parser::tests::parse_cmd_subst_redirect_target_with_nested_subst`) — that
// hazard is parser *construction* (infinite recursion). The *execution* half is
// pinned here.
//
// #90 reported that a `$()`-valued redirect target on a bare single command
// (not part of a pipeline or `&&`/`||` chain) failed with "could not evaluate
// redirect target" — the dispatcher wasn't attached, so `eval_redirect_target`
// fell back to the sync evaluator, which can't run `$()`. It's resolved: bare
// commands no longer take a reduced sync fast path — every statement runs
// through `execute_pipeline`, which attaches `ctx.dispatcher` (kernel.rs, "This
// is the single execution path — no fast path for single commands"), so the
// target `$()` runs regardless of surrounding syntax. These pin that.

/// The exact #90 repro: a `$()` redirect *target* on a bare single command
/// (`echo x > $(echo f)`) evaluates the substitution and writes the file.
#[tokio::test]
async fn bare_command_cmdsubst_redirect_target_writes_file() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute("echo hi > $(echo out.txt)")
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "bare-command redirect target must run $(): {r:?}");

    let f = kernel.execute("cat out.txt").await.expect("cat");
    assert_eq!(f.text_out().trim(), "hi", "the file received the output: {f:?}");
}

/// A `$()` *stdin* redirect target (`cat < $(echo f)`) on a bare command runs
/// too — the same dispatcher path serves input redirects.
#[tokio::test]
async fn bare_command_cmdsubst_stdin_redirect_target() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    kernel.execute("echo payload > src.txt").await.expect("seed");
    let r = kernel
        .execute("cat < $(echo src.txt)")
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out().trim(), "payload", "stdin target $() must resolve: {r:?}");
}

/// The nested form #90 called out: a redirect inside `$()` whose target is
/// itself a `$()` — `$(cmd > $(subst))`. The parser handles the nesting; the
/// target-eval step used to fail. Now it executes: capture is empty (stdout
/// went to the file) and the file has the bytes.
#[tokio::test]
async fn nested_cmdsubst_redirect_target_executes() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute(r#"x=$(echo hi > $(echo inner.txt)); echo "[$x]""#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out().trim(), "[]", "stdout went to the file, capture empty: {r:?}");

    let f = kernel.execute("cat inner.txt").await.expect("cat");
    assert_eq!(f.text_out().trim(), "hi", "the nested-target file received the output: {f:?}");
}

/// A `$()` redirect target on a *pipeline stage* (`echo x | cat > $(echo g)`).
/// On a bare kernel the stage context copies `dispatcher: None` too, so this
/// path had the same GH #90 gap — the fix threads the stage's own dispatcher.
#[tokio::test]
async fn pipeline_stage_cmdsubst_redirect_target_writes_file() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute("echo piped | cat > $(echo g.txt)")
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "{r:?}");

    let f = kernel.execute("cat g.txt").await.expect("cat");
    assert_eq!(f.text_out().trim(), "piped", "the pipeline redirect target resolved: {f:?}");
}
