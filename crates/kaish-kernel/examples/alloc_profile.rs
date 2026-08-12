//! Heap-allocation profile of representative embedder workloads (GH #48).
//!
//! GH #48's "Approach" asks for a real profile *before* the wide refactors:
//! "Profile a representative workload (heap + allocation count) to find the real
//! peaks before changing anything… Treat the bullets above as hypotheses to
//! confirm, not a task list." The batch-review burndown that followed was a
//! static read of async frame sizes; this is the missing measurement. It is a
//! committed harness, not a one-off script, so the next person can re-run it
//! against a later kernel and compare.
//!
//! # Running it
//!
//! ```text
//! # Release numbers with symbols (what the report quotes):
//! CARGO_PROFILE_RELEASE_DEBUG=1 \
//!   cargo run -p kaish-kernel --release --features dhat-heap \
//!             --example alloc_profile -- all
//!
//! # One workload, or `list` to see them:
//! cargo run -p kaish-kernel --release --features dhat-heap \
//!           --example alloc_profile -- grep-pipeline
//! ```
//!
//! Each workload writes `dhat-heap-<workload>.json` in the working directory
//! (override with `KAISH_PROFILE_OUT`). The harness summarizes it in-process
//! (top program points by bytes and by allocation count), so no external viewer
//! is needed — but the file is the real DHAT format and can be loaded into the
//! DHAT viewer (<https://nnethercote.github.io/dh_view/dh_view.html>) for the
//! full tree.
//!
//! # Why dhat and not a counting global allocator
//!
//! A hand-rolled counting allocator gives totals but no attribution, and the
//! workspace sets `unsafe_code = "deny"` — writing `unsafe impl GlobalAlloc`
//! here would mean punching a hole in that for a profiling toy. dhat owns the
//! `unsafe` inside the dependency, and its per-allocation backtraces are exactly
//! the "attributed to call sites" part of the ask. Cost: it is gated behind the
//! `dhat-heap` feature and this example is `required-features`-gated on it, so
//! `cargo build`, `cargo test --all`, and `cargo clippy --all --all-targets` all
//! skip it and neither the dependency nor the allocator reaches a normal build.
//!
//! # What is and is not measured
//!
//! The profiler starts *after* the kernel and fixtures are built and after one
//! warm-up run of the same script, so lazily-initialized globals (regex caches,
//! schema tables) are not charged to the workload. `kernel-construct` is the
//! deliberate exception: constructing the kernel *is* its workload, because an
//! agent embedder typically builds a fresh kernel per request (see
//! `docs/EMBEDDING.md`, "Embedders typically run a fresh kernel per request").

// Harness code: unwrap/expect on known-good fixtures is the idiom here, and a
// panic IS the harness failing. Examples are not `#[test]` context, so the
// workspace restriction lints need an explicit allow (see CLAUDE.md).
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::fmt::Write as _;
use std::path::{Path, PathBuf};

use kaish_kernel::{Kernel, KernelConfig};

#[global_allocator]
static ALLOC: dhat::Alloc = dhat::Alloc;

/// One measured scenario: a script plus how many times to run it.
struct Workload {
    /// CLI selector.
    name: &'static str,
    /// Why this shape is representative of how embedders drive kaish.
    rationale: &'static str,
    /// What one iteration means, for the per-iteration column.
    unit: &'static str,
    iterations: usize,
    kind: Kind,
}

enum Kind {
    /// Run `script` through `Kernel::execute` on a kernel built once.
    Script(&'static str),
    /// Build a fresh kernel per iteration and run one trivial command on it.
    KernelConstruct,
}

fn workloads() -> Vec<Workload> {
    vec![
        Workload {
            name: "kernel-construct",
            rationale: "an agent embedder builds a fresh kernel per request \
                        (EMBEDDING.md); this is the fixed cost of showing up",
            unit: "one Kernel::new + one trivial execute",
            iterations: 20,
            kind: Kind::KernelConstruct,
        },
        Workload {
            name: "execute-call",
            rationale: "the kaibo/kaijutsu round trip: one execute() per tool \
                        call on an already-built kernel — isolates per-call \
                        fixed cost (lex, parse, validate, ctx snapshot, dispatch)",
            unit: "one kernel.execute(\"echo …\")",
            iterations: 200,
            kind: Kind::Script("echo hello world"),
        },
        Workload {
            name: "grep-pipeline",
            rationale: "builtin-heavy pipeline over many files — the read-only \
                        codebase-analysis shape kaibo exists to serve",
            unit: "one grep|sed|wc pipeline over the fixture tree",
            iterations: 20,
            kind: Kind::Script("grep -rn needle . | sed 's/needle/found/' | wc -l"),
        },
        Workload {
            name: "cmdsubst",
            rationale: "command substitution re-enters the statement engine — \
                        the recursion path GH #46/#47/#48 are all about",
            unit: "one script with 8 nested + 4 sibling $( )",
            iterations: 20,
            kind: Kind::Script(
                "echo $(echo $(echo $(echo $(echo $(echo $(echo $(echo deep)))))))\n\
                 echo $(echo a) $(echo b) $(echo c) $(echo d)",
            ),
        },
        Workload {
            name: "loop-many-commands",
            rationale: "many small commands in one script — per-command dispatch \
                        cost, where the ExecContext materialization lives",
            unit: "one 200-iteration for loop (200 dispatches)",
            iterations: 10,
            kind: Kind::Script("for i in $(seq 1 200); do echo $i; done"),
        },
        Workload {
            name: "scatter-gather",
            rationale: "scatter forks a kernel per worker — the widest \
                        context-materialization path kaish has",
            unit: "one 8-way scatter|gather",
            iterations: 20,
            kind: Kind::Script("seq 1 8 | scatter --as N | echo $N | gather"),
        },
    ]
}

/// Build the fixture tree: 64 files of 40 lines each across 8 subdirectories,
/// a fraction of them containing the string the grep workload searches for.
///
/// `tempfile::tempdir()` per the project's no-real-system-paths rule.
fn build_fixture() -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("tempdir");
    let files = 64;
    let lines = 40;
    for f in 0..files {
        let sub = dir.path().join(format!("pkg{}", f % 8));
        std::fs::create_dir_all(&sub).expect("mkdir");
        let mut body = String::new();
        for l in 0..lines {
            if (f + l) % 17 == 0 {
                let _ = writeln!(body, "line {l}: the needle is here");
            } else {
                let _ = writeln!(body, "line {l}: ordinary content for file {f}");
            }
        }
        std::fs::write(sub.join(format!("file{f}.txt")), body).expect("write fixture");
    }
    dir
}

/// A kernel shaped like an embedder's: sandboxed to a root it owns, hermetic
/// env, validation on, trash forced off so the measurement doesn't depend
/// on the developer's `KAISH_TRASH`.
fn build_kernel(root: &Path) -> Kernel {
    let config = KernelConfig::agent_with_root(root.to_path_buf())
        .with_trash(false);
    Kernel::new(config).expect("build kernel")
}

async fn run_script(kernel: &Kernel, script: &str) {
    let result = kernel.execute(script).await.expect("kernel execute");
    assert_eq!(result.code, 0, "workload script failed: {}", result.err);
}

/// Run one workload: warm up outside the profiler, then measure `iterations`.
async fn profile(workload: &Workload, root: &Path, out_dir: &Path) -> Summary {
    let file = out_dir.join(format!("dhat-heap-{}.json", workload.name));

    match &workload.kind {
        Kind::Script(script) => {
            let kernel = build_kernel(root);
            run_script(&kernel, script).await; // warm-up, unprofiled

            let profiler = dhat::Profiler::builder().file_name(&file).build();
            for _ in 0..workload.iterations {
                run_script(&kernel, script).await;
            }
            let stats = dhat::HeapStats::get();
            drop(profiler); // writes the JSON
            Summary::new(workload, stats, &file)
        }
        Kind::KernelConstruct => {
            {
                let kernel = build_kernel(root); // warm-up, unprofiled
                run_script(&kernel, "true").await;
            }

            let profiler = dhat::Profiler::builder().file_name(&file).build();
            for _ in 0..workload.iterations {
                let kernel = build_kernel(root);
                run_script(&kernel, "true").await;
            }
            let stats = dhat::HeapStats::get();
            drop(profiler);
            Summary::new(workload, stats, &file)
        }
    }
}

struct Summary {
    name: &'static str,
    unit: &'static str,
    iterations: usize,
    total_blocks: u64,
    total_bytes: u64,
    max_blocks: usize,
    max_bytes: usize,
    /// Top program points, already sorted and rendered.
    by_bytes: Vec<PpRow>,
    by_blocks: Vec<PpRow>,
}

#[derive(Clone)]
struct PpRow {
    bytes: u64,
    blocks: u64,
    site: String,
}

impl Summary {
    fn new(workload: &Workload, stats: dhat::HeapStats, file: &Path) -> Self {
        let (by_bytes, by_blocks) = top_program_points(file, 12);
        Self {
            name: workload.name,
            unit: workload.unit,
            iterations: workload.iterations,
            total_blocks: stats.total_blocks,
            total_bytes: stats.total_bytes,
            max_blocks: stats.max_blocks,
            max_bytes: stats.max_bytes,
            by_bytes,
            by_blocks,
        }
    }
}

/// Frames that are allocation plumbing rather than a call site worth naming.
/// The first frame that isn't one of these is the attribution we print.
const PLUMBING: &[&str] = &[
    "alloc::",
    "<alloc::",
    "core::",
    "<core::",
    "std::",
    "<std::",
    "hashbrown::",
    "<hashbrown::",
    "__rust",
    "malloc",
    "realloc",
    "dhat::",
];

/// Strip dhat's `0xADDR: ` prefix; what remains is `symbol (file:line)`.
fn clean_frame(raw: &str) -> &str {
    raw.split_once(": ").map(|(_, rest)| rest).unwrap_or(raw)
}

fn is_plumbing(sym: &str) -> bool {
    PLUMBING.iter().any(|p| sym.starts_with(p))
}

/// Parse the dhat JSON back and rank its program points. Reading our own output
/// keeps the harness self-contained: a terminal-only run still answers "where
/// did the allocations come from" without the external DHAT viewer.
fn top_program_points(file: &Path, n: usize) -> (Vec<PpRow>, Vec<PpRow>) {
    let Ok(text) = std::fs::read_to_string(file) else {
        return (Vec::new(), Vec::new());
    };
    let Ok(json) = serde_json::from_str::<serde_json::Value>(&text) else {
        return (Vec::new(), Vec::new());
    };
    let ftbl: Vec<&str> = json["ftbl"]
        .as_array()
        .map(|a| a.iter().filter_map(|v| v.as_str()).collect())
        .unwrap_or_default();
    let empty = Vec::new();
    let pps = json["pps"].as_array().unwrap_or(&empty);

    let mut rows: Vec<PpRow> = pps
        .iter()
        .map(|pp| {
            let frames: Vec<&str> = pp["fs"]
                .as_array()
                .map(|a| {
                    a.iter()
                        .filter_map(|i| i.as_u64())
                        .filter_map(|i| ftbl.get(i as usize).copied())
                        .collect()
                })
                .unwrap_or_default();
            // First non-plumbing frame names the site; keep one more for context.
            let cleaned: Vec<&str> = frames.iter().map(|f| clean_frame(f)).collect();
            let head = cleaned.iter().position(|f| !is_plumbing(f)).unwrap_or(0);
            let site = cleaned
                .iter()
                .skip(head)
                .take(2)
                .copied()
                .collect::<Vec<_>>()
                .join("  <-  ");
            PpRow {
                bytes: pp["tb"].as_u64().unwrap_or(0),
                blocks: pp["tbk"].as_u64().unwrap_or(0),
                site: if site.is_empty() { "<unattributed>".to_string() } else { site },
            }
        })
        .collect();

    rows.sort_by_key(|r| std::cmp::Reverse(r.bytes));
    let by_bytes: Vec<PpRow> = rows.iter().take(n).cloned().collect();

    rows.sort_by_key(|r| std::cmp::Reverse(r.blocks));
    let by_blocks: Vec<PpRow> = rows.iter().take(n).cloned().collect();

    (by_bytes, by_blocks)
}

fn report(summary: &Summary) {
    let iters = summary.iterations as f64;
    println!("\n==============================================================");
    println!("  {}", summary.name);
    println!("  unit: {} x {}", summary.unit, summary.iterations);
    println!("--------------------------------------------------------------");
    println!(
        "  total allocations : {:>12}   ({:>12.0} per iter)",
        summary.total_blocks,
        summary.total_blocks as f64 / iters,
    );
    println!(
        "  total bytes       : {:>12}   ({:>12.0} per iter)",
        summary.total_bytes,
        summary.total_bytes as f64 / iters,
    );
    println!("  peak live blocks  : {:>12}", summary.max_blocks);
    println!("  peak live bytes   : {:>12}", summary.max_bytes);

    println!("\n  top program points by TOTAL BYTES");
    for row in &summary.by_bytes {
        println!("    {:>12} B  {:>9} allocs   {}", row.bytes, row.blocks, row.site);
    }
    println!("\n  top program points by ALLOCATION COUNT");
    for row in &summary.by_blocks {
        println!("    {:>9} allocs  {:>12} B   {}", row.blocks, row.bytes, row.site);
    }
}

fn usage(all: &[Workload]) {
    println!("usage: alloc_profile <workload|all|list>\n");
    for w in all {
        println!("  {:<20} {}", w.name, w.rationale);
    }
}

fn main() {
    let all = workloads();
    let arg = std::env::args().nth(1).unwrap_or_else(|| "all".to_string());
    if arg == "list" || arg == "--help" || arg == "-h" {
        usage(&all);
        return;
    }
    let selected: Vec<usize> = if arg == "all" {
        (0..all.len()).collect()
    } else {
        match all.iter().position(|w| w.name == arg) {
            Some(i) => vec![i],
            None => {
                eprintln!("unknown workload: {arg}\n");
                usage(&all);
                std::process::exit(2);
            }
        }
    };

    let out_dir: PathBuf = std::env::var_os("KAISH_PROFILE_OUT")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("."));

    // The interpreter recurses on the native stack and the cmdsubst workload
    // exercises exactly that, so size the driver thread the way EMBEDDING.md
    // tells embedders to. `current_thread` keeps the profile deterministic —
    // dhat serializes on a global lock anyway.
    let handle = std::thread::Builder::new()
        .stack_size(kaish_kernel::RECOMMENDED_STACK_SIZE)
        .spawn(move || {
            let runtime = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .expect("tokio runtime");
            runtime.block_on(async move {
                let fixture = build_fixture();
                for i in selected {
                    let summary = profile(&all[i], fixture.path(), &out_dir).await;
                    report(&summary);
                }
            });
        })
        .expect("spawn driver thread");
    handle.join().expect("driver thread");
}
