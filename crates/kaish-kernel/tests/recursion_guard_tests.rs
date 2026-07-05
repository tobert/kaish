//! GH #46/#47: the statement engine's dynamic re-entry — command substitution,
//! shell-function calls, and `.kai` script sourcing — is depth-bounded, so a
//! runaway or mutually recursive script returns a LOUD error instead of
//! overflowing the native stack (a SIGSEGV/abort with no diagnostic).
//!
//! These MUST run the recursion on a stack sized to `RECOMMENDED_STACK_SIZE`:
//! a default `#[tokio::test]` thread (~2 MB) overflows a deep recursion long
//! before the cap trips — and that overflow is the very bug under test (it
//! would abort the whole test binary). So each case drives a fresh
//! current-thread runtime on a `std::thread` with the recommended stack,
//! exactly as the REPL and embedders are told to size their execution threads.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::{Kernel, KernelConfig, MAX_RECURSION_DEPTH, RECOMMENDED_STACK_SIZE};

/// Execute `script` through a fresh in-memory kernel on a recommended-size
/// stack (so the recursion can reach the cap before it could overflow).
fn run_on_recommended_stack(script: String) -> ExecResult {
    std::thread::Builder::new()
        .name("recursion-test".to_string())
        .stack_size(RECOMMENDED_STACK_SIZE)
        .spawn(move || {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .expect("current-thread runtime");
            rt.block_on(async move {
                let kernel = Kernel::new(KernelConfig::isolated()).expect("kernel");
                kernel.execute(&script).await.expect("execute returns a result")
            })
        })
        .expect("spawn recommended-stack thread")
        .join()
        .expect("recommended-stack thread joined")
}

/// A self-recursive function with no base case returns a loud "maximum
/// recursion depth" error — not a stack overflow. Without the guard this
/// aborts the process (`f() { f; }; f` was a debug/release SIGSEGV).
#[test]
fn runaway_function_recursion_is_a_loud_error() {
    let result = run_on_recommended_stack("f() { f; }; f".to_string());
    assert_ne!(result.code, 0, "runaway recursion must fail loudly: {result:?}");
    assert!(
        result.err.contains("maximum recursion depth"),
        "expected a recursion-depth error, got: {result:?}"
    );
}

/// Mutual recursion (`a → b → a`) trips the same guard — the bound is on the
/// dynamic re-entry chain, not on any one function name.
#[test]
fn mutual_recursion_is_a_loud_error() {
    let result = run_on_recommended_stack("a() { b; }; b() { a; }; a".to_string());
    assert_ne!(result.code, 0, "mutual recursion must fail loudly: {result:?}");
    assert!(
        result.err.contains("maximum recursion depth"),
        "expected a recursion-depth error, got: {result:?}"
    );
}

/// Legitimate bounded recursion well within the cap succeeds — the guard is a
/// ceiling on runaways, not a tax on real recursion. `$(( ))` is arithmetic
/// (not a command-substitution re-entry), so this is pure function-call depth.
#[test]
fn bounded_recursion_within_cap_succeeds() {
    let depth = MAX_RECURSION_DEPTH / 2;
    let script = format!(
        "down() {{ if [[ $1 -le 0 ]]; then echo done; else down $(($1 - 1)); fi; }}; down {depth}"
    );
    let result = run_on_recommended_stack(script);
    assert_eq!(result.code, 0, "bounded recursion must succeed: {result:?}");
    assert!(result.text_out().contains("done"), "must reach the base case: {result:?}");
}

/// `source`/`.` is the fourth re-entry point — a `.kai` file that sources
/// itself runs its statements inline via the statement engine and recurses
/// unbounded. It's intercepted as a special form before the other guarded
/// paths, so it carries its own guard. Uses an in-memory `/v` file the kernel
/// can source (no host FS).
#[test]
fn self_sourcing_script_is_a_loud_error() {
    let after = std::thread::Builder::new()
        .stack_size(RECOMMENDED_STACK_SIZE)
        .spawn(|| {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .expect("runtime");
            rt.block_on(async {
                let kernel = Kernel::new(KernelConfig::isolated()).expect("kernel");
                // Write a self-sourcing script into the in-memory VFS, then run it.
                kernel
                    .execute("echo 'source /v/self.kai' > /v/self.kai")
                    .await
                    .expect("seed");
                kernel.execute("source /v/self.kai").await.expect("execute")
            })
        })
        .expect("spawn")
        .join()
        .expect("join");
    assert_ne!(after.code, 0, "self-sourcing must fail loudly: {after:?}");
    assert!(
        after.err.contains("maximum recursion depth"),
        "expected a recursion-depth error, got: {after:?}"
    );
}

/// The RAII guard balances the counter on unwind: after a tripped recursion, a
/// later command on the same kernel is unaffected — no leaked depth that would
/// spuriously trip an unrelated, shallow call.
#[test]
fn counter_is_balanced_after_a_tripped_recursion() {
    let after = std::thread::Builder::new()
        .stack_size(RECOMMENDED_STACK_SIZE)
        .spawn(|| {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .expect("runtime");
            rt.block_on(async {
                let kernel = Kernel::new(KernelConfig::isolated()).expect("kernel");
                let tripped = kernel.execute("f() { f; }; f").await.expect("first execute");
                assert_ne!(tripped.code, 0, "the first execute must trip the guard: {tripped:?}");
                // Same kernel, a trivial command: it starts at depth 0 again.
                kernel.execute("echo alive").await.expect("second execute")
            })
        })
        .expect("spawn")
        .join()
        .expect("join");
    assert_eq!(after.code, 0, "a command after a tripped recursion must succeed: {after:?}");
    assert_eq!(after.text_out().trim(), "alive", "recursion counter leaked across executes: {after:?}");
}
