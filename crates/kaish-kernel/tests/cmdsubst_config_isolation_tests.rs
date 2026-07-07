//! Command substitution `$(...)` must isolate session-config mutations, not
//! just scope/cwd (GH #139).
//!
//! `Expr::CommandSubst` (bare `$(...)`) and `StringPart::CommandSubst`
//! (`"...$(...)..."` interpolation) both save/restore `scope`/`cwd`/`prev_cwd`
//! around the substitution's execution, but left `aliases`/`ignore_config`/
//! `output_limit` unguarded — so `x=$(kaish-ignore clear)`,
//! `$(kaish-output-limit off)`, or `$(unalias name)` silently mutated the
//! persistent kernel session, the same class of bug #138 fixed for the
//! plain-statement path. Every test runs the mutation inside `$(...)`, then
//! reads the config back via a LATER, separate `kernel.execute()` call —
//! matching how `cd`/variable-assignment isolation is already proven for
//! command substitution.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{IgnoreConfig, Kernel, KernelConfig, OutputLimitConfig};

async fn run(kernel: &Kernel, script: &str) -> (String, i64) {
    let result = kernel.execute(script).await.expect("kernel execute");
    (result.text_out().to_string(), result.code)
}

// ─────────────────────────── ignore_config ───────────────────────────

fn ignore_kernel() -> Kernel {
    let config = KernelConfig::isolated().with_ignore_config(IgnoreConfig::agent());
    Kernel::new(config).expect("kernel")
}

#[tokio::test]
async fn bare_cmd_subst_kaish_ignore_clear_does_not_leak() {
    let kernel = ignore_kernel();

    let (_, code) = run(&kernel, "x=$(kaish-ignore clear)").await;
    assert_eq!(code, 0);

    let (out, code) = run(&kernel, "kaish-ignore").await;
    assert_eq!(code, 0);
    assert!(
        out.contains(".gitignore"),
        "ignore_config must survive a bare $(...) mutation: {out}"
    );
}

#[tokio::test]
async fn string_interpolated_cmd_subst_kaish_ignore_clear_does_not_leak() {
    let kernel = ignore_kernel();

    // Quoted-string interpolation is the OTHER command-substitution call site
    // (`StringPart::CommandSubst`) — must be fixed identically to the bare form.
    let (_, code) = run(&kernel, r#"echo "before $(kaish-ignore clear) after""#).await;
    assert_eq!(code, 0);

    let (out, code) = run(&kernel, "kaish-ignore").await;
    assert_eq!(code, 0);
    assert!(
        out.contains(".gitignore"),
        "ignore_config must survive a string-interpolated $(...) mutation: {out}"
    );
}

// ─────────────────────────── output_limit ───────────────────────────

fn output_limit_kernel() -> Kernel {
    // in_memory(): no disk spill, so the test leaves nothing on the host fs.
    let config =
        KernelConfig::isolated().with_output_limit(OutputLimitConfig::agent().in_memory());
    Kernel::new(config).expect("kernel")
}

#[tokio::test]
async fn cmd_subst_kaish_output_limit_off_does_not_leak() {
    let kernel = output_limit_kernel();

    let (_, code) = run(&kernel, "x=$(kaish-output-limit off)").await;
    assert_eq!(code, 0);

    let (out, code) = run(&kernel, "kaish-output-limit").await;
    assert_eq!(code, 0);
    assert!(
        out.contains("8K"),
        "output_limit must survive $(...): the agent 8K default must still be set: {out}"
    );
}

// ─────────────────────────── aliases ───────────────────────────

fn alias_kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated()).expect("kernel")
}

#[tokio::test]
async fn cmd_subst_unalias_does_not_leak() {
    let kernel = alias_kernel();

    let (_, code) = run(&kernel, "alias greet='echo hi'").await;
    assert_eq!(code, 0);

    let (_, code) = run(&kernel, "x=$(unalias greet)").await;
    assert_eq!(code, 0);

    let (out, code) = run(&kernel, "greet").await;
    assert_eq!(code, 0, "alias must still resolve after $(...): {out}");
    assert_eq!(out.trim(), "hi");
}
