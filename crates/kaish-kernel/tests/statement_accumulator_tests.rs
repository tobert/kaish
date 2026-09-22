//! A function body, a sourced file, a `.kai` script, and a `$(…)` block each
//! build one result from a sequence of statements. That result follows the
//! rules the top-level statement loop follows:
//!
//! - `code`, `fault`, `original_code`, `content_type`, and `baggage` come from
//!   the last statement that ran.
//! - `did_spill` stays set once any statement spilled.
//! - `data` is the last statement's value only when that statement marked it
//!   as a value.
//!
//! Each rule is checked against the same statement run directly, so a wrapper
//! that reports something the statement did not is visible as a difference.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::collections::BTreeMap;
use std::sync::Arc;

use async_trait::async_trait;
use kaish_kernel::tools::{ToolArgs, ToolCtx, ToolSchema};
use kaish_kernel::vfs::{MemoryFs, VfsRouter};
use kaish_kernel::{Kernel, KernelBackend, KernelConfig, LocalBackend, Tool};
use kaish_types::{ExecResult, Value};

/// An embedder tool that tags its result, the way kaijutsu or an MCP engine
/// does.
struct TaggedTool;

#[async_trait]
impl Tool for TaggedTool {
    fn name(&self) -> &str {
        "tagged"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("tagged", "returns a tagged result")
    }

    async fn execute(&self, _args: ToolArgs, _ctx: &mut dyn ToolCtx) -> ExecResult {
        let mut result = ExecResult::success("tagged\n").with_content_type("text/markdown");
        let mut baggage = BTreeMap::new();
        baggage.insert("trace_id".to_string(), "abc123".to_string());
        result.baggage = baggage;
        result
    }
}

/// The wrappers that run a body as a command.
/// Scripts live in `/scripts`, which is the only PATH entry.
const WRAPPERS: [&str; 3] = ["function", "source", "script"];

async fn kernel() -> Kernel {
    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::new());
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    let kernel = Kernel::with_backend(backend, KernelConfig::isolated(), |_| {}, |tools| {
        tools.register(TaggedTool);
    })
    .expect("with_backend kernel");
    kernel.execute("mkdir /scripts; PATH=/scripts").await.unwrap();
    kernel
}

/// Define `body` in `wrapper` form and return the command that runs it.
async fn define(kernel: &Kernel, wrapper: &str, body: &str) -> String {
    match wrapper {
        "function" => {
            kernel.execute(&format!("function wrapped {{ {body} }}")).await.unwrap();
            "wrapped".to_string()
        }
        "source" => {
            write_file(kernel, "/sourced.kai", body).await;
            "source /sourced.kai".to_string()
        }
        "script" => {
            write_file(kernel, "/scripts/wrapped.kai", body).await;
            "wrapped".to_string()
        }
        other => panic!("unknown wrapper {other}"),
    }
}

/// Collects one line per wrapper that broke a rule, so a failure names every
/// wrapper that drifted rather than the first.
#[derive(Default)]
struct Failures(Vec<String>);

impl Failures {
    fn check(&mut self, holds: bool, message: String) {
        if !holds {
            self.0.push(message);
        }
    }

    fn assert_none(self) {
        assert!(self.0.is_empty(), "{}", self.0.join("\n"));
    }
}

async fn write_file(kernel: &Kernel, path: &str, body: &str) {
    let written = kernel
        .execute(&format!("cat > {path} <<'EOF'\n{body}\nEOF"))
        .await
        .unwrap();
    assert!(written.ok(), "writing {path} failed: {written:?}");
}

// --- fault --------------------------------------------------------------------

/// Control: the statement run directly aborts an `if`.
#[tokio::test]
async fn a_faulting_test_aborts_an_if_when_run_directly() {
    let kernel = kernel().await;
    let outcome = kernel.execute("if test 1 -eq abc; then echo y; else echo n; fi").await;
    assert!(outcome.is_err(), "a fault is not a boolean: {outcome:?}");
}

#[tokio::test]
async fn a_body_ending_in_a_fault_aborts_an_if() {
    let mut failures = Failures::default();
    for wrapper in WRAPPERS {
        let kernel = kernel().await;
        let run = define(&kernel, wrapper, "echo before; test 1 -eq abc").await;
        let outcome = kernel.execute(&format!("if {run}; then echo y; else echo n; fi")).await;
        failures.check(
            outcome.is_err(),
            format!("{wrapper}: a body ending in a fault selected a branch: {outcome:?}"),
        );
    }
    failures.assert_none();
}

#[tokio::test]
async fn a_body_ending_in_a_fault_reports_the_fault() {
    let mut failures = Failures::default();
    for wrapper in WRAPPERS {
        let kernel = kernel().await;
        let run = define(&kernel, wrapper, "test 1 -eq abc").await;
        let result = kernel.execute(&run).await.unwrap();
        failures.check(result.code == 2 && result.fault, format!("{wrapper}: want exit 2 and fault: {result:?}"));
    }
    failures.assert_none();
}

/// `fault` is the last statement's, not sticky: a later statement that
/// decided something is a decided result.
#[tokio::test]
async fn a_fault_before_the_last_statement_does_not_stay() {
    let mut failures = Failures::default();
    for wrapper in WRAPPERS {
        let kernel = kernel().await;
        let run = define(&kernel, wrapper, "test 1 -eq abc; false").await;
        let outcome = kernel.execute(&format!("if {run}; then echo y; else echo n; fi")).await;
        let decided = matches!(&outcome, Ok(result) if result.text_out().ends_with("n\n"));
        failures.check(decided, format!("{wrapper}: `false` decides the body: {outcome:?}"));
    }
    failures.assert_none();
}

/// A fault from an earlier statement is not attached to a later error.
#[tokio::test]
async fn a_fault_before_an_error_does_not_make_the_error_a_fault() {
    let mut failures = Failures::default();
    for wrapper in WRAPPERS {
        let kernel = kernel().await;
        let run = define(&kernel, wrapper, "test 1 -eq abc; x=$((1/0))").await;
        let result = kernel.execute(&run).await.unwrap();
        failures.check(
            result.err.contains("divides by zero") && !result.fault,
            format!("{wrapper}: want the error, not a fault: {result:?}"),
        );
    }
    failures.assert_none();
}

// --- did_spill / original_code ------------------------------------------------

#[tokio::test]
async fn a_spill_in_the_last_statement_reaches_the_result() {
    let mut failures = Failures::default();
    for wrapper in WRAPPERS {
        let kernel = kernel().await;
        kernel.execute("set -o output-limit=64").await.unwrap();
        let run = define(&kernel, wrapper, "seq 1 5000").await;
        let result = kernel.execute(&run).await.unwrap();
        failures.check(
            result.did_spill && result.code == 3 && result.original_code == Some(0),
            format!(
                "{wrapper}: want did_spill, code 3, original_code Some(0) from seq: \
                 did_spill={} code={} original_code={:?}",
                result.did_spill, result.code, result.original_code
            ),
        );
    }
    failures.assert_none();
}

/// `did_spill` stays set; `original_code` follows the last statement.
#[tokio::test]
async fn a_spill_before_the_last_statement_stays_and_its_code_does_not() {
    let mut failures = Failures::default();
    for wrapper in WRAPPERS {
        let kernel = kernel().await;
        kernel.execute("set -o output-limit=64").await.unwrap();
        let run = define(&kernel, wrapper, "seq 1 5000; false").await;
        let result = kernel.execute(&run).await.unwrap();
        failures.check(
            result.did_spill && result.original_code.unwrap_or(result.code) == 1,
            format!(
                "{wrapper}: want did_spill and real exit 1 from `false`: did_spill={} code={} original_code={:?}",
                result.did_spill, result.code, result.original_code
            ),
        );
    }
    failures.assert_none();
}

#[tokio::test]
async fn a_spill_before_an_error_survives_on_the_failed_result() {
    let mut failures = Failures::default();
    for wrapper in WRAPPERS {
        let kernel = kernel().await;
        kernel.execute("set -o output-limit=64").await.unwrap();
        let run = define(&kernel, wrapper, "seq 1 5000; x=$((1/0))").await;
        let result = kernel.execute(&run).await.unwrap();
        failures.check(
            result.err.contains("divides by zero") && result.did_spill && result.original_code == Some(0),
            format!(
                "{wrapper}: want the error, did_spill, original_code Some(0) from seq: \
                 did_spill={} code={} original_code={:?} err={:?}",
                result.did_spill, result.code, result.original_code, result.err
            ),
        );
    }
    failures.assert_none();
}

// --- content_type / baggage ---------------------------------------------------

/// Control: the tool's tags reach the result when it runs directly.
#[tokio::test]
async fn an_embedder_tool_s_tags_reach_the_result_when_run_directly() {
    let kernel = kernel().await;
    let result = kernel.execute("tagged").await.unwrap();
    assert_eq!(result.content_type.as_deref(), Some("text/markdown"), "{result:?}");
    assert_eq!(result.baggage.get("trace_id").map(String::as_str), Some("abc123"), "{result:?}");
}

#[tokio::test]
async fn an_embedder_tool_s_tags_reach_the_result_from_the_last_statement() {
    let mut failures = Failures::default();
    for wrapper in WRAPPERS {
        let kernel = kernel().await;
        let run = define(&kernel, wrapper, "echo before; tagged").await;
        let result = kernel.execute(&run).await.unwrap();
        failures.check(
            result.text_out() == "before\ntagged\n"
                && result.content_type.as_deref() == Some("text/markdown")
                && result.baggage.get("trace_id").map(String::as_str) == Some("abc123"),
            format!("{wrapper}: want both outputs and the tool's tags: {result:?}"),
        );
    }
    failures.assert_none();
}

/// The tags describe the last statement's output, not the whole body's.
#[tokio::test]
async fn an_embedder_tool_s_tags_do_not_outlive_a_later_statement() {
    let mut failures = Failures::default();
    for wrapper in WRAPPERS {
        let kernel = kernel().await;
        let run = define(&kernel, wrapper, "tagged; echo after").await;
        let result = kernel.execute(&run).await.unwrap();
        failures.check(
            result.content_type.is_none() && result.baggage.is_empty(),
            format!("{wrapper}: `echo` set no tags: {result:?}"),
        );
    }
    failures.assert_none();
}

// --- typed data ---------------------------------------------------------------

async fn bound(kernel: &Kernel, command: &str) -> Option<Value> {
    let result = kernel.execute(&format!("x=$({command})")).await.unwrap();
    assert!(result.ok(), "{command}: {result:?}");
    kernel.get_var("x").await
}

/// Each wrapper binds what the same statements bind in a plain `$(…)`.
#[tokio::test]
async fn a_body_binds_what_the_same_statements_bind_directly() {
    let bodies = [
        // A value from the last statement stays typed.
        "echo before; fromjson '[1,2]'",
        // A later statement replaces an earlier value.
        "fromjson '[1,2]'; seq 1 2",
        // A structured view of printed text is not a value.
        "printf 'a\\tb\\n' > /t; cut -f2 /t",
    ];
    let mut failures = Failures::default();
    for body in bodies {
        let direct = bound(&kernel().await, body).await;
        for wrapper in WRAPPERS {
            let kernel = kernel().await;
            let run = define(&kernel, wrapper, body).await;
            let wrapped = bound(&kernel, &run).await;
            failures.check(
                wrapped == direct,
                format!("{wrapper} `{body}`: bound {wrapped:?}, directly {direct:?}"),
            );
        }
    }
    failures.assert_none();
}

/// Control: the first body above really is typed when run directly.
#[tokio::test]
async fn a_value_binds_typed_through_a_substitution() {
    let kernel = kernel().await;
    assert_eq!(
        bound(&kernel, "echo before; fromjson '[1,2]'").await,
        Some(Value::Json(serde_json::json!([1, 2])))
    );
}

/// `return` leaves a function body and a sourced file the same way.
#[tokio::test]
async fn a_return_after_a_value_binds_the_same_in_function_and_source() {
    let body = "fromjson '[1,2]'; return";
    let kernel = kernel().await;
    let run = define(&kernel, "function", body).await;
    let from_function = bound(&kernel, &run).await;
    let run = define(&kernel, "source", body).await;
    let from_source = bound(&kernel, &run).await;
    assert_eq!(from_function, from_source);
}
