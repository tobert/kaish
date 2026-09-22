//! A `true`/`false` literal passed as the *value* of a value-taking flag must
//! survive to the builtin, not be swallowed by bool-flagify.
//!
//! `spawn --command true` lexes `true` as `Value::Bool(true)`; the old
//! `flagify_bool_named` moved any bool-valued named entry into the flag set, so
//! `--command` arrived with no value and clap errored "a value is required".
//! Now flagify skips keys the schema declares value-taking, so the literal
//! command `true` reaches spawn.
//!
//! `spawn` is `subprocess`-gated, so this binary only exercises the fix when
//! that feature is on (it is under `cargo test --all` via workspace feature
//! unification; a bare `-p kaish-kernel` run compiles it to nothing).
#![cfg(feature = "subprocess")]

mod common;

use common::kernel_at;

#[tokio::test]
async fn spawn_command_true_is_not_swallowed() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    // `common::kernel_at` seeds no PATH (the kernel never reads the OS env
    // on its own) — `spawn`'s own `--command` resolution has none of its
    // own OS-PATH fallback either, so `true` needs PATH exported here, the
    // same way other test kernels seed it via `initial_vars`. Exporting it
    // at runtime instead of touching the shared helper keeps this fix local
    // to the one test file that actually spawns an external command.
    let path = std::env::var("PATH").unwrap_or_default();
    let result = kernel
        .execute(&format!("export PATH=\"{path}\"; spawn --command true"))
        .await
        .expect("execute");
    assert!(
        !result.err.contains("a value is required"),
        "the literal command should reach spawn: {:?}",
        result.err
    );
    // spawn backgrounds `true` successfully (proves the value reached the
    // builtin; a vacuous "command not found: spawn" would be 127).
    assert_eq!(result.code, 0, "spawn should succeed: {:?}", result.err);
}

#[tokio::test]
async fn spawn_command_quoted_true_still_works() {
    // The pre-existing workaround keeps working — no regression.
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let path = std::env::var("PATH").unwrap_or_default();
    let result = kernel
        .execute(&format!("export PATH=\"{path}\"; spawn --command 'true'"))
        .await
        .expect("execute");
    assert_ne!(result.code, 2, "quoted form regressed: {:?}", result.err);
}
