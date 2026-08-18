//! `set -o <name>` / `set +o <name>` must fail loudly on a name kaish does
//! not implement, instead of silently no-opping. Regression coverage for the
//! bug where `set -o pipefail` (or any typo) returned exit 0 and left the
//! caller believing an option had taken effect.
//!
//! `Kernel::transient()` is used throughout — these tests exercise `set`'s
//! option parsing only, not the filesystem, so no `localfs` feature is
//! needed and the suite compiles/passes under `--no-default-features` too.

use kaish_kernel::Kernel;

// ---------------------------------------------------------------------------
// Controls: real options still work, through the real parser/dispatch path.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn set_o_trash_still_enables() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("set -o trash; set").await.unwrap();
    assert!(result.ok(), "err={}", result.err);
    assert!(result.text_out().contains("set -o trash"), "{:?}", result.text_out());
}

#[tokio::test]
async fn set_plus_o_trash_still_disables() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("set -o trash; set +o trash; set")
        .await
        .unwrap();
    assert!(result.ok(), "err={}", result.err);
    assert!(
        !result.text_out().contains("set -o trash"),
        "{:?}",
        result.text_out()
    );
}

#[tokio::test]
async fn set_o_glob_still_works() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("set -o glob; echo ran").await.unwrap();
    assert!(result.ok(), "err={}", result.err);
    assert_eq!(result.text_out().trim(), "ran");
}

#[tokio::test]
async fn set_plus_o_glob_still_works() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("set +o glob; echo ran").await.unwrap();
    assert!(result.ok(), "err={}", result.err);
    assert_eq!(result.text_out().trim(), "ran");
}

#[tokio::test]
async fn set_o_output_limit_bare_still_enables_default() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("set -o output-limit; set").await.unwrap();
    assert!(result.ok(), "err={}", result.err);
    assert!(
        result.text_out().contains("set -o output-limit="),
        "{:?}",
        result.text_out()
    );
}

#[tokio::test]
async fn set_o_output_limit_with_size_still_works() {
    // parse_size only accepts a single-letter K/M suffix (see
    // output_limit.rs's doc comment) — "1mb" is not a size it parses, so
    // this uses "1M", the accepted spelling for the same value the brief's
    // shorthand named.
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("set -o output-limit=1M; set")
        .await
        .unwrap();
    assert!(result.ok(), "err={}", result.err);
    assert!(
        result.text_out().contains("set -o output-limit=1M"),
        "{:?}",
        result.text_out()
    );
}

#[tokio::test]
async fn set_plus_o_output_limit_still_disables() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("set -o output-limit=16K; set +o output-limit; set")
        .await
        .unwrap();
    assert!(result.ok(), "err={}", result.err);
    assert!(
        !result.text_out().contains("output-limit"),
        "{:?}",
        result.text_out()
    );
}

// ---------------------------------------------------------------------------
// The bug: unknown -o/+o names must fail loudly, and name the valid set.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn set_o_unknown_name_fails_loudly() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("set -o bogusname").await.unwrap();
    assert!(!result.ok(), "an unknown -o name must not succeed");
    assert_ne!(result.code, 0);
    assert!(
        result.err.contains("bogusname"),
        "error should name the bad option: {:?}",
        result.err
    );
    assert!(
        result.err.contains("glob") && result.err.contains("trash") && result.err.contains("output-limit"),
        "error should name the valid set: {:?}",
        result.err
    );
}

#[tokio::test]
async fn set_o_unknown_name_stops_sequential_execution() {
    // `;` does not stop on failure by default in kaish (matching bash
    // without `-e`), so enable `-e` first — the unknown -o name's failure
    // must still be loud enough to halt the sequence.
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("set -e; set -o bogusname; echo ran")
        .await
        .unwrap();
    assert!(!result.ok(), "err={}", result.err);
    assert!(
        !result.text_out().contains("ran"),
        "with -e, execution must stop at the failing set, not continue to echo: {:?}",
        result.text_out()
    );
}

#[tokio::test]
async fn set_plus_o_unknown_name_fails_loudly() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("set +o bogusname").await.unwrap();
    assert!(!result.ok());
    assert_ne!(result.code, 0);
    assert!(result.err.contains("bogusname"), "{:?}", result.err);
}

#[tokio::test]
async fn set_o_pipefail_fails_loudly() {
    // limits.md documents pipefail as a deliberate omission, not an
    // oversight — an agent reaching for the bash safety must be told kaish
    // has none, not left believing it took effect.
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("set -o pipefail").await.unwrap();
    assert!(!result.ok(), "set -o pipefail must fail, not silently no-op");
    assert_ne!(result.code, 0);
    assert!(
        result.err.contains("pipefail"),
        "error should name pipefail: {:?}",
        result.err
    );
}

#[tokio::test]
async fn set_o_output_limit_unparseable_size_fails_loudly() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("set -o output-limit=banana")
        .await
        .unwrap();
    assert!(!result.ok(), "an unparseable size must not silently no-op");
    assert_ne!(result.code, 0);
    assert!(
        result.err.contains("banana"),
        "error should name the bad size: {:?}",
        result.err
    );
}

// ---------------------------------------------------------------------------
// The flags-split parse path (`flags=["o"], positional=["trash"]`) must
// agree with the `-o` positional path on which names are valid.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn set_o_flag_split_path_still_enables_known_option() {
    // A lone `-o NAME` with no other flags/positionals on the token binds
    // NAME as a bare positional and "o" as a boolean flag (set.rs's own
    // fallback comment describes this quirk). Covered here through the real
    // parser/binder, not a hand-built ToolArgs.
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("set -o trash; set").await.unwrap();
    assert!(result.ok(), "err={}", result.err);
    assert!(result.text_out().contains("set -o trash"));
}

#[tokio::test]
async fn set_o_flag_split_path_rejects_unknown_option() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("set -o bogusname").await.unwrap();
    assert!(
        !result.ok(),
        "the flag-split path must reject unknown names exactly like -o does"
    );
    assert!(result.err.contains("bogusname"), "{:?}", result.err);
}
