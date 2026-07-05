//! Ground-truth measurement of native stack consumed per statement-engine
//! re-entry level (`$(…)` command substitution) — the quantity that sets
//! [`kaish_kernel::RECOMMENDED_STACK_SIZE`] / [`kaish_kernel::MAX_RECURSION_DEPTH`]
//! (GH #46/#47) and that GH #48's allocation pass is trying to shrink.
//!
//! Why a runtime probe and not `-Zprint-type-sizes`: the coroutine *layout*
//! (future struct size) is computed at MIR level and is invariant to
//! optimization, so the type-size tool cannot see the native-stack win from
//! `opt-level` or from removing per-poll temporaries. Only running the real
//! recursion and reading the stack pointer at each depth measures it.
//!
//! How it works: a trivial `stackprobe` builtin calls a `#[inline(never)]` sync
//! helper that returns the address of a stack local — a genuine native stack
//! pointer at the current poll depth (the tool's own `async` body is heap-boxed
//! by `async_trait`, so we must step into a sync frame to read the real SP).
//! A script of `stackprobe $(stackprobe $(…))` nests N command substitutions;
//! because pure builtins never yield, the whole nest runs in one synchronous
//! poll, so every outer level's frames are live when the innermost probe fires.
//! Consecutive probe addresses therefore differ by exactly one re-entry level's
//! native stack. We report the median interior delta (robust to the shallow
//! entry/exit levels, which differ from steady state).

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::{Arc, Mutex};

use async_trait::async_trait;
use kaish_kernel::tools::{ToolArgs, ToolCtx, ToolSchema};
use kaish_kernel::vfs::{MemoryFs, VfsRouter};
use kaish_kernel::{Kernel, KernelBackend, KernelConfig, LocalBackend, Tool};
use kaish_types::ExecResult;

/// Return the address of a stack local. `#[inline(never)]` + `black_box` keep
/// the frame real so the pointer reflects the current native stack depth rather
/// than being optimized away or folded into the caller.
#[inline(never)]
fn stack_marker() -> usize {
    let x = 0u8;
    std::hint::black_box(&x as *const u8 as usize)
}

/// A leaf builtin that records the native stack pointer each time it runs.
struct StackProbe {
    marks: Arc<Mutex<Vec<usize>>>,
}

#[async_trait]
impl Tool for StackProbe {
    fn name(&self) -> &str {
        "stackprobe"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("stackprobe", "test tool: record the native stack pointer")
    }

    async fn execute(&self, _args: ToolArgs, _ctx: &mut dyn ToolCtx) -> ExecResult {
        self.marks.lock().unwrap().push(stack_marker());
        // Empty stdout so the wrapping `$(…)` substitutes to nothing.
        ExecResult::success("")
    }
}

/// `stackprobe $(stackprobe $(… $(stackprobe)))` — `depth` nested command
/// substitutions wrapping `depth + 1` probes.
fn nested_script(depth: usize) -> String {
    let mut s = String::from("stackprobe");
    for _ in 0..depth {
        s = format!("stackprobe $({s})");
    }
    s
}

/// Median of a slice (sorted copy). Robust central tendency for the per-level
/// deltas, ignoring the atypical outermost/innermost frames.
fn median(v: &[usize]) -> usize {
    let mut s = v.to_vec();
    s.sort_unstable();
    s[s.len() / 2]
}

#[test]
fn per_level_native_stack_cost() {
    // Drive on a generously sized thread so the measurement itself never
    // overflows regardless of profile (debug is ~hundreds of KB/level).
    let marks: Arc<Mutex<Vec<usize>>> = Arc::new(Mutex::new(Vec::new()));
    let marks_for_thread = marks.clone();

    let handle = std::thread::Builder::new()
        .stack_size(128 * 1024 * 1024)
        .spawn(move || {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();
            rt.block_on(async {
                let mut vfs = VfsRouter::new();
                vfs.mount("/", MemoryFs::new());
                let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
                let probe_marks = marks_for_thread.clone();
                let kernel = Kernel::with_backend(backend, KernelConfig::isolated(), |_| {}, move |tools| {
                    tools.register(StackProbe { marks: probe_marks.clone() });
                })
                .expect("build kernel")
                .into_arc();

                // Stay well under MAX_RECURSION_DEPTH (32); each `$()` is one level.
                let depth = 24;
                let result = kernel.execute(&nested_script(depth)).await.expect("execute");
                assert_eq!(result.code, 0, "probe script failed: {}", result.err);
            });
        })
        .unwrap();
    handle.join().unwrap();

    let addrs = marks.lock().unwrap().clone();
    assert!(addrs.len() >= 10, "expected many probe samples, got {}", addrs.len());

    // Adjacent probes differ by one re-entry level; the stack grows one way, so
    // take absolute differences. Drop the first and last (entry/exit frames are
    // not steady state) and report the median interior delta.
    let mut deltas: Vec<usize> = addrs
        .windows(2)
        .map(|w| w[0].abs_diff(w[1]))
        .collect();
    deltas.sort_unstable();
    let interior = &deltas[1..deltas.len().saturating_sub(1)];
    let med = median(interior);
    let min = interior.first().copied().unwrap_or(0);
    let max = interior.last().copied().unwrap_or(0);

    eprintln!(
        "\n=== per-$()-level native stack ===\n  samples: {}\n  median : {} bytes ({} KiB)\n  min    : {} bytes\n  max    : {} bytes\n",
        addrs.len(),
        med,
        med / 1024,
        min,
        max,
    );

    // Catastrophic-regression guard only — the real number is the eprintln
    // above. Generous enough to hold in unoptimized debug (~hundreds of KB),
    // tight enough to catch a runaway (e.g. an un-boxed multi-MB frame). If this
    // trips, a hot future grew a large by-value field across an await.
    assert!(
        med < 1024 * 1024,
        "per-level native stack {med} bytes exceeds 1 MiB — a hot recursive future \
         likely grew a large by-value field; investigate before relaxing this bound"
    );
}
