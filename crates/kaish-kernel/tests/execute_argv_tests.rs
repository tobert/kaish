//! Kernel-routed tests for [`Kernel::execute_argv`] — the argv-native peer of
//! `execute(&str)`.
//!
//! Two surfaces are proven here:
//!  - **Convergence** (the differential the design leans on): for single
//!    commands, the argv door yields the *same* `ExecResult` as the string door
//!    (`execute`). Every assertion drives both and compares, so the corpus is an
//!    oracle for free.
//!  - **The semantic contract that only the argv door can express**: tokens are
//!    literal (no glob/`$VAR`/split), typed `Value`s ride through as positionals,
//!    and the shared tail (`--json`, the confirmation latch, error codes) is
//!    reachable.
//!
//! The classifier itself is unit-/property-tested in-crate
//! (`kernel::argv_classify_tests`); this file is the end-to-end net above it.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
// KernelConfig::repl() mounts the real filesystem; echo/ls/rm are localfs.
#![cfg(feature = "localfs")]

use std::path::Path;

use kaish_kernel::ast::Value;
use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::{Kernel, KernelConfig};

fn tempdir() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("execute-argv-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

/// Real-FS kernel rooted at `dir`, latch/trash off by default (tests opt in).
fn kernel_at(dir: &Path) -> Kernel {
    let config = KernelConfig::repl()
        .with_cwd(dir.to_path_buf())
        .with_latch(false)
        .with_trash(false);
    Kernel::new(config).expect("kernel")
}

/// `Value::String` shorthand.
fn s(text: &str) -> Value {
    Value::String(text.to_string())
}

/// `(trimmed stdout, exit code)` — the observable surface we compare doors on.
fn observe(r: &ExecResult) -> (String, i64) {
    (r.text_out().trim().to_string(), r.code)
}

// ============================================================================
// Convergence: argv door ≡ string door for single commands
// ============================================================================

#[tokio::test]
async fn argv_door_matches_string_door() {
    let dir = tempdir();
    let kernel = kernel_at(dir.path());

    // (command name, argv tokens, the equivalent command string)
    let cases: &[(&str, Vec<Value>, &str)] = &[
        ("echo", vec![s("hello"), s("world")], "echo hello world"),
        // A flag is classified as a flag (getopt), same as the bare lexer token.
        ("echo", vec![s("-n"), s("hi")], "echo -n hi"),
        // `--` ends flags; the following `-n` is a literal positional on both.
        ("echo", vec![s("--"), s("-n")], "echo -- -n"),
        // One word containing a space: argv passes it intact; the string form
        // needs quoting to mean the same thing. Both are one positional.
        ("echo", vec![s("a b c")], "echo 'a b c'"),
        // A bareword `key=value` to a non-allowlist command stringifies back to a
        // `"key=value"` positional (bash: `cat foo=bar`), identical on both doors.
        ("echo", vec![s("A=1")], "echo A=1"),
        ("true", vec![], "true"),
        ("false", vec![], "false"),
    ];

    for (name, argv, string) in cases {
        let via_argv = kernel.execute_argv(name, argv).await.expect("execute_argv");
        let via_string = kernel.execute(string).await.expect("execute");
        assert_eq!(
            observe(&via_argv),
            observe(&via_string),
            "doors diverged for `{string}` (argv {argv:?})"
        );
    }
}

// ============================================================================
// The contract only the argv door can express
// ============================================================================

#[tokio::test]
async fn argv_tokens_are_literal_not_globbed() {
    let dir = tempdir();
    std::fs::write(dir.path().join("a.txt"), "a").unwrap();
    std::fs::write(dir.path().join("b.txt"), "b").unwrap();
    let kernel = kernel_at(dir.path());

    // The string door globs `*.txt` against the cwd...
    let (globbed, code) = observe(&kernel.execute("echo *.txt").await.unwrap());
    assert_eq!(code, 0);
    assert_eq!(globbed, "a.txt b.txt", "string door should glob");

    // ...but an argv token is literal: no glob expansion ever happens.
    let (literal, code) = observe(&kernel.execute_argv("echo", &[s("*.txt")]).await.unwrap());
    assert_eq!(code, 0);
    assert_eq!(literal, "*.txt", "argv token must stay literal, not glob");
}

#[tokio::test]
async fn argv_tokens_do_not_interpolate_variables() {
    let kernel = kernel_at(tempdir().path());
    kernel.execute("HOME_LIKE=secret").await.unwrap();

    // `$HOME_LIKE` is a literal four+ char token, not a variable reference.
    let (out, code) = observe(&kernel.execute_argv("echo", &[s("$HOME_LIKE")]).await.unwrap());
    assert_eq!(code, 0);
    assert_eq!(out, "$HOME_LIKE", "argv token must not interpolate");
}

#[tokio::test]
async fn tilde_expands_consistently_with_the_string_door() {
    // A leading `~` is expanded against the session HOME by the *shared* binder,
    // so the argv door and the string door agree (kaish expands `~` uniformly,
    // unlike bash's quoting rules — see EMBEDDING.md). This pins consistency, not
    // a claim that argv tokens are byte-for-byte literal w.r.t. `~`.
    let kernel = kernel_at(tempdir().path());
    kernel.execute("HOME=/home/agent").await.unwrap();

    let via_argv = observe(&kernel.execute_argv("echo", &[s("~/work")]).await.unwrap());
    let via_string = observe(&kernel.execute("echo ~/work").await.unwrap());
    assert_eq!(via_argv, via_string);
    assert_eq!(via_argv.0, "/home/agent/work", "tilde should expand via the shared binder");
}

#[tokio::test]
async fn typed_values_ride_through_as_positionals() {
    let kernel = kernel_at(tempdir().path());

    // A non-string value is a positional carrying the value — never flag-parsed,
    // even when it would *look* like a flag as a string.
    let (out, code) = observe(&kernel.execute_argv("echo", &[Value::Int(-9)]).await.unwrap());
    assert_eq!(code, 0);
    assert_eq!(out, "-9", "Int rides through as a positional value");

    // A JSON value reaches the builtin (here `echo` renders it); the point is it
    // is delivered as one positional rather than dropped or split.
    let json = Value::Json(serde_json::json!([1, 2, 3]));
    let (out, code) = observe(&kernel.execute_argv("echo", &[json]).await.unwrap());
    assert_eq!(code, 0);
    assert!(out.contains('1') && out.contains('3'), "JSON positional reached echo: {out:?}");
}

// ============================================================================
// Shared tail is reachable: --json, error codes, the confirmation latch
// ============================================================================

#[tokio::test]
async fn json_output_transform_applies_via_argv() {
    let dir = tempdir();
    std::fs::write(dir.path().join("only.txt"), "x").unwrap();
    let kernel = kernel_at(dir.path());

    // The `--json` transform is a kernel-level concern; it must apply through the
    // argv door exactly as through the string door.
    let via_argv = kernel.execute_argv("ls", &[s("--json")]).await.unwrap();
    let via_string = kernel.execute("ls --json").await.unwrap();
    assert_eq!(observe(&via_argv), observe(&via_string));
    assert!(
        via_argv.text_out().contains("only.txt"),
        "ls --json via argv should list the file: {}",
        via_argv.text_out()
    );
}

#[tokio::test]
async fn request_timeout_interrupts_a_hung_command() {
    use std::time::Duration;
    // The kernel's configured request_timeout must apply to the argv door too,
    // for safety parity with the string door (a hung command can't run forever).
    let config = KernelConfig::repl()
        .with_cwd(tempdir().path().to_path_buf())
        .with_latch(false)
        .with_trash(false)
        .with_request_timeout(Duration::from_millis(200));
    let kernel = Kernel::new(config).expect("kernel");

    // `sleep 5` blocks far past the 200ms deadline; the watchdog must interrupt
    // it and surface exit 124. The outer tokio timeout fails the test fast rather
    // than hanging if the watchdog never fires.
    let r = tokio::time::timeout(Duration::from_secs(10), kernel.execute_argv("sleep", &[s("5")]))
        .await
        .expect("execute_argv must return well before 10s (watchdog should fire at 200ms)")
        .expect("execute_argv");
    assert_eq!(r.code, 124, "request_timeout should yield exit 124, err: {}", r.err);
}

#[tokio::test]
async fn unknown_command_is_127() {
    let kernel = kernel_at(tempdir().path());
    let r = kernel
        .execute_argv("definitely-not-a-real-command-xyz", &[])
        .await
        .unwrap();
    assert_eq!(r.code, 127, "unknown command should be 127, err: {}", r.err);
}

#[tokio::test]
async fn latch_round_trips_through_argv_door() {
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").unwrap();
    let kernel = kernel_at(dir.path());

    // Enable the latch *via argv too* (`set -o latch`).
    let enabled = kernel.execute_argv("set", &[s("-o"), s("latch")]).await.unwrap();
    assert_eq!(enabled.code, 0, "set -o latch failed: {}", enabled.err);

    // First rm is gated: exit 2, file untouched, nonce in structured `.data`.
    let gated = kernel.execute_argv("rm", &[s("precious.txt")]).await.unwrap();
    assert_eq!(gated.code, 2, "expected latch exit 2, err: {}", gated.err);
    assert!(
        dir.path().join("precious.txt").exists(),
        "file must survive the latch gate"
    );

    let nonce = match gated.data.as_ref().expect("latch result carries .data") {
        Value::Json(j) => j["nonce"].as_str().expect("nonce string in .data").to_string(),
        other => panic!("expected Value::Json latch data, got {other:?}"),
    };

    // Confirm through the argv door: `rm --confirm=<nonce> precious.txt`.
    let confirmed = kernel
        .execute_argv("rm", &[s(&format!("--confirm={nonce}")), s("precious.txt")])
        .await
        .unwrap();
    assert_eq!(confirmed.code, 0, "confirm failed: {}", confirmed.err);
    assert!(
        !dir.path().join("precious.txt").exists(),
        "file should be deleted after argv confirmation"
    );
}
