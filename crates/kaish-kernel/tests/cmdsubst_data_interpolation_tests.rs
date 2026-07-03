//! Regression tests for the SILENT DATA LOSS bug: `"...$(cmd)..."` used to read
//! only `.out` via `try_text_out()` (`kernel.rs`, `StringPart::CommandSubst`).
//! A tool/builtin that sets structured `.data` but leaves `.out` empty (the
//! shape `ExecResult::from_parts(0, "", "", Some(data))` produces directly)
//! interpolated to `""` — the collection silently evaporated instead of
//! rendering, even though a bare `"$x"` on the same value renders it as
//! compact JSON (or the plain scalar form).
//!
//! kaish's own shipped builtins (`fromjson`, `keys`, `values`) all populate
//! `.out` via `ExecResult::success_data`, so they never hit this hole in
//! practice today — the fixture tools below construct the `.data`-only,
//! empty-`.out` shape directly (the same shape a `KernelBackend`-registered
//! embedder tool can produce via `ToolResult::with_data("", json)`), to pin
//! the fix at the exact seam it protects.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use async_trait::async_trait;
use kaish_kernel::tools::{ToolArgs, ToolCtx, ToolSchema};
use kaish_kernel::vfs::{MemoryFs, VfsRouter};
use kaish_kernel::{Kernel, KernelBackend, KernelConfig, LocalBackend, Tool};
use kaish_types::{ExecResult, Value};

/// Returns a record (`{"a":1,"b":2}`) as `.data` only — `.out` is empty text.
struct RecordDataTool;

#[async_trait]
impl Tool for RecordDataTool {
    fn name(&self) -> &str {
        "recdata"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("recdata", "test fixture: record in .data, empty .out")
    }

    async fn execute(&self, _args: ToolArgs, _ctx: &mut dyn ToolCtx) -> ExecResult {
        let data = Value::Json(serde_json::json!({"a": 1, "b": 2}));
        ExecResult::from_parts(0, String::new(), String::new(), Some(data))
    }
}

/// Returns a list (`[1,2,3]`) as `.data` only — `.out` is empty text.
struct ListDataTool;

#[async_trait]
impl Tool for ListDataTool {
    fn name(&self) -> &str {
        "listdata"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("listdata", "test fixture: list in .data, empty .out")
    }

    async fn execute(&self, _args: ToolArgs, _ctx: &mut dyn ToolCtx) -> ExecResult {
        let data = Value::Json(serde_json::json!([1, 2, 3]));
        ExecResult::from_parts(0, String::new(), String::new(), Some(data))
    }
}

/// Returns a scalar string (`hello`) as `.data` only — `.out` is empty text.
/// The scalar case proves the fix reuses `value_to_string` (unquoted `String`
/// rendering), not a naive `serde_json::to_string` of the `.data` (which
/// would quote it as `"hello"`).
struct ScalarDataTool;

#[async_trait]
impl Tool for ScalarDataTool {
    fn name(&self) -> &str {
        "scalardata"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("scalardata", "test fixture: scalar string in .data, empty .out")
    }

    async fn execute(&self, _args: ToolArgs, _ctx: &mut dyn ToolCtx) -> ExecResult {
        let data = Value::String("hello".to_string());
        ExecResult::from_parts(0, String::new(), String::new(), Some(data))
    }
}

/// Sets BOTH non-empty `.out` and `.data` (a differing collection) — proves
/// text still wins when present; the `.data` fallback must never override it.
struct OutWinsTool;

#[async_trait]
impl Tool for OutWinsTool {
    fn name(&self) -> &str {
        "outwins"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("outwins", "test fixture: non-empty .out plus .data")
    }

    async fn execute(&self, _args: ToolArgs, _ctx: &mut dyn ToolCtx) -> ExecResult {
        let data = Value::Json(serde_json::json!({"should_not_render": true}));
        ExecResult::from_parts(0, "TEXT-WINS".to_string(), String::new(), Some(data))
    }
}

fn kernel_with_tools() -> Arc<Kernel> {
    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::new());
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    Kernel::with_backend(backend, KernelConfig::isolated(), |_| {}, |tools| {
        tools.register(RecordDataTool);
        tools.register(ListDataTool);
        tools.register(ScalarDataTool);
        tools.register(OutWinsTool);
    })
    .expect("with_backend kernel")
    .into_arc()
}

/// The bug, pinned: a record-valued `.data`-only result must render as
/// compact JSON inside a quoted string, not evaporate to "".
#[tokio::test]
async fn record_data_only_result_renders_as_compact_json_in_string() {
    let kernel = kernel_with_tools();
    let r = kernel
        .execute(r#"msg="parsed: $(recdata) done"; echo "$msg""#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0);
    assert_eq!(r.text_out().trim(), r#"parsed: {"a":1,"b":2} done"#);
}

/// Same bug, list shape.
#[tokio::test]
async fn list_data_only_result_renders_as_compact_json_in_string() {
    let kernel = kernel_with_tools();
    let r = kernel
        .execute(r#"msg="items: $(listdata) done"; echo "$msg""#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0);
    assert_eq!(r.text_out().trim(), "items: [1,2,3] done");
}

/// A record `.data`-only result must render identically whether captured via
/// `"$(cmd)"` directly or round-tripped through a bare `"$x"` — same helper,
/// same output, per the decided fix (reuse `value_to_string`).
#[tokio::test]
async fn cmdsubst_and_bare_var_render_a_collection_identically() {
    let kernel = kernel_with_tools();
    let direct = kernel
        .execute(r#"echo "got: $(recdata)""#)
        .await
        .expect("execute");
    let via_var = kernel
        .execute(r#"x=$(recdata); echo "got: $x""#)
        .await
        .expect("execute");
    assert_eq!(direct.text_out().trim(), via_var.text_out().trim());
}

/// Scalar `.data`-only result: renders as the plain scalar (unquoted), not
/// JSON-quoted — proving reuse of `value_to_string`, not raw JSON serialization
/// of `.data`.
#[tokio::test]
async fn scalar_data_only_result_renders_unquoted_in_string() {
    let kernel = kernel_with_tools();
    let r = kernel
        .execute(r#"msg="say: $(scalardata) done"; echo "$msg""#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0);
    assert_eq!(r.text_out().trim(), "say: hello done");
}

/// Non-empty `.out` still wins over `.data` — existing/unchanged behavior.
#[tokio::test]
async fn nonempty_out_wins_over_data_in_string() {
    let kernel = kernel_with_tools();
    let r = kernel
        .execute(r#"msg="result: $(outwins) done"; echo "$msg""#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0);
    assert_eq!(r.text_out().trim(), "result: TEXT-WINS done");
}

/// A shipped builtin's `.data`-only success (`fromjson` on a record) already
/// populates `.out` via `ExecResult::success_data`, so this exercises the
/// unchanged text-wins path end-to-end through a real builtin, not just the
/// fixture tools above.
#[tokio::test]
async fn fromjson_record_in_string_interpolation() {
    let kernel = kernel_with_tools();
    let r = kernel
        .execute(r#"j='{"a":1,"b":2}'; msg="parsed: $(fromjson <<< $j) done"; echo "$msg""#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0);
    assert_eq!(r.text_out().trim(), r#"parsed: {"a":1,"b":2} done"#);
}

/// `keys` on a record is also `.data`-only via `success_data` — same
/// unchanged-text-wins coverage for a list-shaped result.
#[tokio::test]
async fn keys_list_in_string_interpolation() {
    let kernel = kernel_with_tools();
    let r = kernel
        .execute(r#"h={"a": 1, "b": 2}; msg="keys: $(keys $h) done"; echo "$msg""#)
        .await
        .expect("execute");
    assert_eq!(r.code, 0);
    assert_eq!(r.text_out().trim(), r#"keys: ["a","b"] done"#);
}
