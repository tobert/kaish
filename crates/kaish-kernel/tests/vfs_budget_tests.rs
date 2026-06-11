//! Kernel-routed tests for VFS byte-budget accounting (Part A).
//!
//! All tests use NoLocal/isolated kernels (or tiny budgets) — no real system
//! paths, no /tmp writes. VFS paths only.

use kaish_kernel::{Kernel, KernelConfig};

/// Build a NoLocal kernel with a specific budget (in bytes).
fn budgeted_kernel(limit: u64) -> Kernel {
    Kernel::new(
        KernelConfig::isolated().with_vfs_budget(limit),
    )
    .expect("kernel creation")
}

/// Build a NoLocal kernel with no budget (unbounded).
fn unbounded_kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated().without_vfs_budget()).expect("kernel creation")
}

/// Run a script through the kernel and return `(trimmed stdout, exit code)`.
async fn run(kernel: &Kernel, script: &str) -> (String, i64) {
    let result = kernel.execute(script).await.expect("kernel execute");
    (result.text_out().trim().to_string(), result.code)
}

// ---------------------------------------------------------------------------
// Budget enforcement: write past the limit fails loudly with budget label
// ---------------------------------------------------------------------------

#[tokio::test]
async fn budget_exceeded_fails_loudly_with_budget_label() {
    // 10-byte budget; writing 20 bytes exceeds it.
    let kernel = budgeted_kernel(10);

    // Write 20 "x" characters (20 bytes).
    let (out, code) = run(&kernel, r#"echo "xxxxxxxxxxxxxxxxxxxx" > /v/big.txt"#).await;
    assert_ne!(code, 0, "write past budget should fail: out={out:?}");

    // The error message should name the budget label so the user knows which
    // knob to raise.
    let err_text = {
        let result = kernel.execute(r#"echo "xxxxxxxxxxxxxxxxxxxx" > /v/big.txt"#).await.expect("execute");
        result.err
    };
    assert!(
        err_text.contains("vfs-memory") || out.contains("vfs-memory"),
        "error must name the 'vfs-memory' budget: err={err_text:?} out={out:?}"
    );
}

#[tokio::test]
async fn budget_exceeded_prior_content_intact() {
    // 50-byte budget.
    let kernel = budgeted_kernel(50);

    // Write a small file that fits.
    let (_, code) = run(&kernel, r#"echo "hello" > /v/small.txt"#).await;
    assert_eq!(code, 0, "small write should succeed");

    // Now write a huge file that would exceed the budget.
    let (_, code2) = run(&kernel, r#"echo "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" > /v/huge.txt"#).await;
    assert_ne!(code2, 0, "huge write should fail");

    // Prior content must still be intact.
    let (out, code3) = run(&kernel, "cat /v/small.txt").await;
    assert_eq!(code3, 0, "cat of prior file should still work");
    assert!(out.contains("hello"), "prior content must survive failed write: got {out:?}");
}

// ---------------------------------------------------------------------------
// without_vfs_budget: unbounded — a modest write succeeds
// ---------------------------------------------------------------------------

#[tokio::test]
async fn without_vfs_budget_is_unbounded() {
    // A 1 MiB write should succeed when no budget is set.
    let kernel = unbounded_kernel();

    // Build a string that is exactly 1 MiB - 1 byte to stay safe with newline.
    let payload = "x".repeat(1024 * 1024 - 1);
    let script = format!(r#"echo "{payload}" > /v/big.txt"#);
    let (_, code) = run(&kernel, &script).await;
    assert_eq!(code, 0, "1MiB write should succeed with no budget");
}

#[tokio::test]
async fn with_vfs_budget_1mib_fails_2mib_write() {
    // A kernel with a 1 MiB cap should reject a 2 MiB write.
    let kernel = budgeted_kernel(1024 * 1024);

    let two_mb = "x".repeat(2 * 1024 * 1024);
    let script = format!(r#"echo "{two_mb}" > /v/toobig.txt"#);
    let result = kernel.execute(&script).await.expect("execute");
    assert_ne!(result.code, 0, "2 MiB write should be rejected by 1 MiB budget");
}

// ---------------------------------------------------------------------------
// One budget across mounts: NoLocal has /, /tmp, /v all sharing the pool
// ---------------------------------------------------------------------------

#[tokio::test]
async fn one_budget_across_mounts_nolocal() {
    // 2048-byte budget shared across / and /tmp.
    // We use the `write` builtin to write exact bytes (no echo newline overhead).
    let kernel = budgeted_kernel(2048);

    // Write 900 bytes to /v
    let p1 = "a".repeat(900);
    let s1 = format!(r#"write /v/part1.txt "{p1}""#);
    let (_, c1) = run(&kernel, &s1).await;
    assert_eq!(c1, 0, "first 900-byte write to /v should succeed");

    // Write 900 bytes to /tmp — same budget pool
    let p2 = "b".repeat(900);
    let s2 = format!(r#"write /tmp/part2.txt "{p2}""#);
    let (_, c2) = run(&kernel, &s2).await;
    assert_eq!(c2, 0, "second 900-byte write to /tmp should also succeed (900+900=1800 < 2048)");

    // A write of 300 bytes would push total to 2100 > 2048, so it should fail.
    let p3 = "c".repeat(300);
    let s3 = format!(r#"write /v/overflow.txt "{p3}""#);
    let (_, c3) = run(&kernel, &s3).await;
    assert_ne!(c3, 0, "300-byte write should fail — budget would be exceeded (1800+300=2100>2048)");
}

// ---------------------------------------------------------------------------
// kaish-mounts --json shows resident_bytes and budget object
// ---------------------------------------------------------------------------

#[tokio::test]
async fn mounts_json_shows_resident_bytes_for_written_file() {
    let kernel = budgeted_kernel(1024 * 1024);

    // Write a known-size file.
    let (_, c) = run(&kernel, r#"echo "hello" > /v/test.txt"#).await;
    assert_eq!(c, 0, "write should succeed");

    // kaish-mounts --json
    let result = kernel.execute("kaish-mounts --json").await.expect("execute");
    let json_str = result.text_out();

    // Parse the JSON.
    let v: serde_json::Value = serde_json::from_str(json_str.trim())
        .unwrap_or_else(|e| panic!("kaish-mounts --json must produce valid JSON: {e}\ngot: {json_str}"));

    // Must have a "mounts" array.
    let mounts = v.get("mounts").expect("must have 'mounts' key");
    assert!(mounts.is_array(), "'mounts' must be an array");

    // Find the /v mount and check resident_bytes > 0.
    let v_mount = mounts
        .as_array()
        .unwrap()
        .iter()
        .find(|m| m.get("path").and_then(|p| p.as_str()) == Some("/v"))
        .expect("must have /v mount in output");

    let resident = v_mount.get("resident_bytes").expect("must have resident_bytes");
    assert!(
        resident.is_number() && resident.as_u64().unwrap_or(0) > 0,
        "/v resident_bytes should be > 0 after write: got {resident:?}"
    );

    // Must have a "budget" object (because we set one).
    let budget = v.get("budget").expect("must have 'budget' key for budgeted kernel");
    assert_eq!(budget.get("label").and_then(|l| l.as_str()), Some("vfs-memory"));
    let limit = budget.get("limit").and_then(|l| l.as_u64()).expect("must have numeric limit");
    assert_eq!(limit, 1024 * 1024, "budget limit should be 1 MiB");
    let used = budget.get("used").and_then(|u| u.as_u64()).expect("must have numeric used");
    assert!(used > 0, "used must be > 0 after write");
}

#[tokio::test]
async fn mounts_json_no_budget_object_for_unbounded_kernel() {
    let kernel = unbounded_kernel();

    let result = kernel.execute("kaish-mounts --json").await.expect("execute");
    let json_str = result.text_out();

    let v: serde_json::Value = serde_json::from_str(json_str.trim())
        .unwrap_or_else(|e| panic!("must produce valid JSON: {e}\ngot: {json_str}"));

    // "mounts" key must be present.
    assert!(v.get("mounts").is_some(), "'mounts' must be present");

    // "budget" key must be absent for an unbounded kernel.
    assert!(
        v.get("budget").is_none(),
        "'budget' must be absent for unbounded kernel: got {v:?}"
    );
}

#[tokio::test]
async fn mounts_json_resident_bytes_null_for_nolocal_root_unbudgeted() {
    // An unbounded kernel's memory mounts still track resident_bytes
    // (they return Some(0) initially), NOT None — LocalFs would return None.
    // NoLocal means no LocalFs at all, so all three mounts are MemoryFs.
    let kernel = unbounded_kernel();
    let result = kernel.execute("kaish-mounts --json").await.expect("execute");
    let json_str = result.text_out();
    let v: serde_json::Value = serde_json::from_str(json_str.trim())
        .unwrap_or_else(|e| panic!("must produce valid JSON: {e}\ngot: {json_str}"));

    let mounts = v["mounts"].as_array().expect("must have mounts array");

    // Every NoLocal mount is a MemoryFs — resident_bytes should be 0 (Some(0)), not null.
    for m in mounts {
        let path = m.get("path").and_then(|p| p.as_str()).unwrap_or("?");
        // /v/jobs and /v/bin are JobFs and BuiltinFs — they return None (null is fine).
        // /v, /tmp, / are MemoryFs — they should be Some(0) initially.
        if path == "/v" || path == "/tmp" || path == "/" {
            let rb = m.get("resident_bytes").expect("must have resident_bytes field");
            assert!(
                rb.is_number(),
                "MemoryFs mount {path} should have numeric resident_bytes, got {rb:?}"
            );
        }
    }
}

// ---------------------------------------------------------------------------
// mcp() profile has 64 MiB budget by default
// ---------------------------------------------------------------------------

#[cfg(feature = "localfs")]
#[tokio::test]
async fn mcp_profile_has_64mib_budget() {
    let config = KernelConfig::mcp();
    assert_eq!(
        config.vfs_budget_bytes,
        Some(64 * 1024 * 1024),
        "mcp() must default to 64 MiB VFS budget"
    );
}

#[cfg(feature = "localfs")]
#[tokio::test]
async fn without_vfs_budget_removes_mcp_budget() {
    let config = KernelConfig::mcp().without_vfs_budget();
    assert_eq!(config.vfs_budget_bytes, None);
}

#[cfg(feature = "localfs")]
#[tokio::test]
async fn with_vfs_budget_sets_arbitrary_size() {
    let config = KernelConfig::isolated().with_vfs_budget(4 * 1024 * 1024);
    assert_eq!(config.vfs_budget_bytes, Some(4 * 1024 * 1024));
}

// ---------------------------------------------------------------------------
// Profiles that should NOT have a budget by default
// ---------------------------------------------------------------------------

#[test]
fn isolated_no_budget_by_default() {
    assert_eq!(KernelConfig::isolated().vfs_budget_bytes, None);
}

#[cfg(feature = "localfs")]
#[test]
fn repl_no_budget_by_default() {
    assert_eq!(KernelConfig::repl().vfs_budget_bytes, None);
}

#[cfg(feature = "localfs")]
#[test]
fn transient_no_budget_by_default() {
    assert_eq!(KernelConfig::transient().vfs_budget_bytes, None);
}

// ---------------------------------------------------------------------------
// Embedder surface: ByteBudget is nameable through kaish-kernel alone
// ---------------------------------------------------------------------------

/// A `with_backend` embedder (kaibo's shape: it builds its own `MemoryFs` and
/// mounts it via `Kernel::with_backend`) must be able to attach a labeled
/// budget without a direct kaish-vfs dependency — `ByteBudget` is re-exported
/// through `kaish_kernel::vfs`.
#[tokio::test]
async fn byte_budget_reexport_serves_with_backend_embedders() {
    use std::sync::Arc;
    use kaish_kernel::vfs::{ByteBudget, MemoryFs, VfsRouter};
    use kaish_kernel::{KernelBackend, LocalBackend};

    let budget = Arc::new(ByteBudget::labeled(64, "scratch"));
    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::with_budget(budget.clone()));
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    let kernel = kaish_kernel::Kernel::with_backend(
        backend,
        KernelConfig::isolated(),
        |_| {},
        |_| {},
    )
    .expect("with_backend kernel");

    let (_, code) = run(&kernel, r#"write /big.txt "0123456789012345678901234567890123456789012345678901234567890123456789""#).await;
    assert_ne!(code, 0, "write past the embedder's budget must fail loudly");
    let result = kernel.execute("echo probe").await.expect("execute");
    assert_eq!(result.code, 0, "kernel still healthy after budget rejection");
}
