//! GH #189 item 3: `--flag=true` on a bool flag must bind like the bare
//! `--flag` (a flag presence), not as a literal `Value::Bool` that a clap
//! `bool` field's `SetTrue` action rejects.
//!
//! Before the fix this only worked for the ~20 builtins that happened to
//! call `ToolArgs::flagify_bool_named` themselves in `execute()` — anything
//! else, including the kernel-owned `--json` (deliberately excluded from
//! every builtin's *own* schema — see `clap_schema::is_skipped` — so no
//! builtin's own `flagify_bool_named` call ever normalized it either), hit a
//! clap parse error. The fix moved the normalization into the shared kernel
//! binder (`kernel::bind_tool_args`'s `Arg::Named` arm) so it applies once,
//! for every schema-aware tool, regardless of whether the tool also calls
//! `flagify_bool_named` itself.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated()).expect("kernel")
}

/// `--json` is excluded from every builtin's OWN schema (it's the kernel-
/// owned global flag), so no per-builtin `flagify_bool_named` call ever
/// covered it — this is the case that could only be fixed in the kernel.
#[tokio::test]
async fn seq_json_equals_true_renders_json() {
    let k = kernel();
    let r = k.execute("seq --json=true 1 3").await.expect("kernel execute");
    assert_eq!(r.code, 0, "seq --json=true must not be a clap parse error: {r:?}");
    assert_eq!(r.text_out().trim(), r#"["1","2","3"]"#, "got: {}", r.text_out());
}

/// `seq` never calls `flagify_bool_named` on its OWN bool flags either
/// (`--width`/`-w`) — `--width=true` used to hand clap's `bool` field a
/// value it rejects.
#[tokio::test]
async fn seq_width_equals_true_does_not_crash() {
    let k = kernel();
    let r = k.execute("seq --width=true 1 3").await.expect("kernel execute");
    assert_eq!(r.code, 0, "seq --width=true must not be a clap parse error: {r:?}");
}

/// `--json=false` behaves like omitting the flag: plain text output.
#[tokio::test]
async fn seq_json_equals_false_renders_plain_text() {
    let k = kernel();
    let r = k.execute("seq --json=false 1 3").await.expect("kernel execute");
    assert_eq!(r.code, 0, "got: {r:?}");
    assert_eq!(r.text_out().trim(), "1\n2\n3");
}

/// The bare `--json` flag (no `=true`) is unaffected — pinning the
/// unglued-value control case alongside the glued one above.
#[tokio::test]
async fn seq_bare_json_flag_still_works() {
    let k = kernel();
    let r = k.execute("seq --json 1 3").await.expect("kernel execute");
    assert_eq!(r.code, 0, "got: {r:?}");
    assert_eq!(r.text_out().trim(), r#"["1","2","3"]"#);
}
