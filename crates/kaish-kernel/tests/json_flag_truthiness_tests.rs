//! One truthiness rule for `--json=VALUE`, across every argument binder.
//!
//! `--json` is kernel-owned, but three binders decide independently whether a
//! given spelling turns it on, and before this they disagreed:
//!
//! | spelling       | typed          | raw_argv | verbatim |
//! |----------------|----------------|----------|----------|
//! | `--json=true`  | on             | on       | on       |
//! | `--json=false` | off            | off      | off      |
//! | `--json=1`     | **exit 2, on** | on       | on       |
//! | `--json=0`     | **exit 2, on** | off      | off      |
//! | `--json=yes`   | **exit 2, on** | on       | on       |
//!
//! Every typed row exited 2 with the error itself rendered as JSON, because
//! the format was switched on before clap ever saw the argv.
//!
//! The typed path let `--json=VALUE` fall through to `named`, so the builtin's
//! clap parser met a bool flag carrying a value and rejected it — after
//! `apply_from_args` had already switched the format on via `has_flag`, whose
//! `Value::Int` fell to a catch-all `true` and made `--json=0` *enable* JSON.
//!
//! These tests pin the same verdict for the same spelling on all three paths,
//! observed where a user sees it: the shape of what the kernel prints.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use async_trait::async_trait;
use kaish_kernel::tools::{ToolArgs, ToolCtx, ToolSchema};
use kaish_kernel::vfs::{MemoryFs, VfsRouter};
use kaish_kernel::{Kernel, KernelBackend, KernelConfig, LocalBackend, Tool};
use kaish_types::{ExecResult, OutputData};

/// Every probe returns this one line, so "did `--json` turn on" reads as
/// `"probe"` (a JSON string) versus `probe` (raw text) no matter which binder
/// the tool was bound through.
const PROBE_TEXT: &str = "probe";

macro_rules! probe_tool {
    ($ty:ident, $name:literal, $schema:expr) => {
        struct $ty;

        #[async_trait]
        impl Tool for $ty {
            fn name(&self) -> &str {
                $name
            }

            fn schema(&self) -> ToolSchema {
                $schema
            }

            async fn execute(&self, _args: ToolArgs, _ctx: &mut dyn ToolCtx) -> ExecResult {
                ExecResult::with_output(OutputData::text(PROBE_TEXT))
            }
        }
    };
}

probe_tool!(TypedProbe, "typedprobe", ToolSchema::new("typedprobe", "typed probe"));
probe_tool!(
    RawProbe,
    "rawprobe",
    ToolSchema::new("rawprobe", "raw-argv probe").with_raw_argv()
);
probe_tool!(
    VerbProbe,
    "verbprobe",
    ToolSchema::new("verbprobe", "verbatim probe").with_verbatim_argv()
);

fn kernel_with_probes() -> Kernel {
    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::new());
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    Kernel::with_backend(backend, KernelConfig::isolated(), |_| {}, |tools| {
        tools.register(TypedProbe);
        tools.register(RawProbe);
        tools.register(VerbProbe);
    })
    .expect("with_backend kernel")
}

/// One row per binder, plus a real builtin.
///
/// The three probe tools pin what each *binder* decides. `seq` pins the half
/// of the bug that lives past the binder: a probe tool never runs clap, so it
/// cannot show `--json=1` being rejected by a `bool` field's `SetTrue` action
/// — only a builtin that actually parses its argv can.
///
/// `(command, text form, JSON form)`.
const CASES: [(&str, &str, &str); 4] = [
    ("typedprobe", PROBE_TEXT, "\"probe\""),
    ("rawprobe", PROBE_TEXT, "\"probe\""),
    ("verbprobe", PROBE_TEXT, "\"probe\""),
    ("seq 1", "1", "[\"1\"]"),
];

/// Run `<command> <flag>` and report whether the kernel rendered JSON.
///
/// Exit code is checked here too: a spelling that is merely *off* must still
/// succeed. Before the fix the typed path exited 2 on `--json=1`, which no
/// stdout-shape assertion alone would have caught.
async fn json_verdict(kernel: &Kernel, case: (&str, &str, &str), flag: &str) -> bool {
    let (command, text_form, json_form) = case;
    let script = format!("{command} {flag}");
    let result = kernel.execute(&script).await.expect("kernel execute");
    let out = result.text_out().trim().to_string();
    assert_eq!(
        result.code, 0,
        "`{script}` must succeed whether or not --json is on; got {} with {out:?}",
        result.code
    );
    if out == json_form {
        true
    } else if out == text_form {
        false
    } else {
        panic!("`{script}` printed neither {json_form:?} nor {text_form:?}: {out:?}")
    }
}

/// Assert every binder agrees with `expected` for one spelling.
async fn assert_all_paths(flag: &str, expected: bool) {
    let kernel = kernel_with_probes();
    for case in CASES {
        let got = json_verdict(&kernel, case, flag).await;
        assert_eq!(
            got, expected,
            "`{} {flag}`: expected --json {}, got {}",
            case.0,
            if expected { "on" } else { "off" },
            if got { "on" } else { "off" }
        );
    }
}

#[tokio::test]
async fn bare_json_is_on_everywhere() {
    assert_all_paths("--json", true).await;
}

#[tokio::test]
async fn json_true_is_on_everywhere() {
    assert_all_paths("--json=true", true).await;
}

#[tokio::test]
async fn json_false_is_off_everywhere() {
    assert_all_paths("--json=false", false).await;
}

/// `1` is on and `0` is off — the spelling raw_argv and verbatim already
/// honored, and the one the typed path both rejected (exit 2) and got
/// backwards (`Value::Int(0)` was truthy, so the error came out as JSON).
#[tokio::test]
async fn json_one_is_on_everywhere() {
    assert_all_paths("--json=1", true).await;
}

#[tokio::test]
async fn json_zero_is_off_everywhere() {
    assert_all_paths("--json=0", false).await;
}

#[tokio::test]
async fn json_yes_is_on_everywhere() {
    assert_all_paths("--json=yes", true).await;
}

/// Only the empty string, `false`, and `0` are off; every other string is on.
/// `no` reads as off to a human and is deliberately NOT — the rule is written
/// once, in `global_flag_value_is_truthy`, and this pins it so a future reader
/// changes the rule rather than one of its three call sites.
#[tokio::test]
async fn json_no_is_on_everywhere() {
    assert_all_paths("--json=no", true).await;
}

#[tokio::test]
async fn json_empty_string_is_off_everywhere() {
    assert_all_paths("--json=\"\"", false).await;
}

/// scatter/gather parse their own options in the pipeline runner, bypassing
/// `Tool::execute()` and therefore `GlobalFlags::apply_from_args` — so they
/// read `--json` off the raw AST instead (GH #222). That reader matched only
/// a bare `Arg::LongFlag`, so `--json=1` on a scatter/gather option error
/// printed plain text while every other builtin printed a JSON envelope.
/// Found by review, not by the suite: no probe tool reaches this path.
#[tokio::test]
async fn scatter_option_errors_honor_the_json_value_form() {
    let kernel = kernel_with_probes();

    /// `(stdout, stderr)` from a scatter option-parse failure.
    async fn scatter_error(kernel: &Kernel, flag: &str) -> (String, String) {
        // `--limit x` fails inside the pipeline runner's own option parse,
        // before any ToolArgs exists to read a flag from.
        let script = format!("seq 2 | scatter --limit x {flag} | gather");
        let result = kernel.execute(&script).await.expect("kernel execute");
        let err = result.err.trim().to_string();
        assert!(
            err.contains("expected a positive integer"),
            "`{script}` did not reach the option-parse error: {err:?}"
        );
        (result.text_out().trim().to_string(), err)
    }

    // On: the error is also rendered to stdout as the JSON envelope every
    // other builtin produces under --json.
    for flag in ["--json", "--json=1", "--json=yes"] {
        let (out, _) = scatter_error(&kernel, flag).await;
        let parsed = serde_json::from_str::<serde_json::Value>(&out);
        assert!(parsed.is_ok(), "`{flag}` must render the error as JSON, got {out:?}");
        assert_eq!(
            parsed.unwrap().get("code").and_then(serde_json::Value::as_i64),
            Some(2),
            "`{flag}` envelope should carry the exit code"
        );
    }

    // Off: stderr only, nothing rendered to stdout.
    for flag in ["--json=0", "--json=false"] {
        let (out, _) = scatter_error(&kernel, flag).await;
        assert!(out.is_empty(), "`{flag}` must leave stdout empty, got {out:?}");
    }
}
