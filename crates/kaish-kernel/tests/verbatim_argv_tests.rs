//! `ArgBinding::Verbatim`: a tool receives every word after its name in source
//! order, post-expansion, instead of the typed `positional`/`named`/`flags`
//! decomposition.
//!
//! The typed binding is lossy for a clap **subcommand tree**: `flags` is a
//! `HashSet` and `named` a `BTreeMap`, so order is gone and a repeated flag
//! collapses. An embedder tool (`kj`, `git`, `curl`) then has to invert that
//! back into an argv clap accepts, and no inversion can recover what the
//! decomposition dropped. Verbatim binding hands the words over untouched, so
//! there is nothing to invert.
//!
//! What these tests pin:
//! - order and multiplicity survive (the two things decomposition destroys),
//! - `--json` stays kernel-owned: stripped from `words` at any position and
//!   still applied to the output format,
//! - a `Typed` tool is unaffected — `Typed` is the default and nothing
//!   existing changes behavior.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use async_trait::async_trait;
use kaish_kernel::tools::{ToolArgs, ToolCtx, ToolSchema};
use kaish_kernel::vfs::{MemoryFs, VfsRouter};
use kaish_kernel::{Kernel, KernelBackend, KernelConfig, LocalBackend, Tool};
use kaish_types::{ExecResult, OutputData, Value};

/// Render one bound word so a test can assert on the whole stream as text.
/// Binary keeps its length rather than its bytes — the point is that it
/// arrived as `Value::Bytes` and never crossed the argv/text boundary.
fn show(value: &Value) -> String {
    match value {
        Value::String(s) => s.clone(),
        Value::Int(i) => i.to_string(),
        Value::Bytes(b) => format!("<bytes:{}>", b.len()),
        other => format!("{other:?}"),
    }
}

/// A verbatim tool. It reports the exact word stream it was handed, plus
/// whether the kernel lifted `--json` out of that stream on its behalf.
struct VerbTool;

#[async_trait]
impl Tool for VerbTool {
    fn name(&self) -> &str {
        "verb"
    }

    fn schema(&self) -> ToolSchema {
        // A subcommand tree, the shape verbatim binding exists for.
        ToolSchema::new("verb", "verbatim test tool")
            .subcommand(
                ToolSchema::new("block", "block commands")
                    .subcommand(ToolSchema::new("list", "list blocks")),
            )
            .with_verbatim_argv()
    }

    async fn execute(&self, args: ToolArgs, _ctx: &mut dyn ToolCtx) -> ExecResult {
        let words = match args.words.as_deref() {
            Some(words) => words.iter().map(show).collect::<Vec<_>>().join("|"),
            None => "<none>".to_string(),
        };
        let json = if args.has_flag("json") { "yes" } else { "no" };
        let leftovers = format!(
            "p={} n={} f={}",
            args.positional.len(),
            args.named.len(),
            // `json` is the kernel's own flag, lifted out of the words; it is
            // not part of the tool's argv, so it doesn't count as leftover.
            args.flags.iter().filter(|f| f.as_str() != "json").count(),
        );
        ExecResult::with_output(OutputData::text(format!(
            "WORDS[{words}] JSON[{json}] {leftovers}"
        )))
    }
}

/// The default (`Typed`) binding, unchanged. Reports the decomposition so a
/// regression in the typed path shows up as a changed string rather than as a
/// silently different shape.
struct TypedTool;

#[async_trait]
impl Tool for TypedTool {
    fn name(&self) -> &str {
        "typedtool"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("typedtool", "typed test tool")
    }

    async fn execute(&self, args: ToolArgs, _ctx: &mut dyn ToolCtx) -> ExecResult {
        let positional = args.positional.iter().map(show).collect::<Vec<_>>().join("|");
        let mut flags: Vec<&str> = args.flags.iter().map(String::as_str).collect();
        flags.sort_unstable();
        let named = args
            .named
            .iter()
            .map(|(k, v)| format!("{k}={}", show(v)))
            .collect::<Vec<_>>()
            .join(",");
        let words = if args.words.is_some() { "some" } else { "none" };
        ExecResult::with_output(OutputData::text(format!(
            "POS[{positional}] FLAGS[{}] NAMED[{named}] WORDS[{words}]",
            flags.join(",")
        )))
    }
}

/// A verbatim tool that also owns its output — the `kj` shape, and the reason
/// both knobs exist. `with_owned_output()` promises the tool consumes `--json`
/// itself and emits final bytes, so the flag has to reach its argv.
struct OwnedVerbTool;

#[async_trait]
impl Tool for OwnedVerbTool {
    fn name(&self) -> &str {
        "ownedverb"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("ownedverb", "verbatim tool that renders its own output")
            .with_verbatim_argv()
            .with_owned_output()
    }

    async fn execute(&self, args: ToolArgs, _ctx: &mut dyn ToolCtx) -> ExecResult {
        let words = match args.words.as_deref() {
            Some(words) => words.iter().map(show).collect::<Vec<_>>().join("|"),
            None => "<none>".to_string(),
        };
        ExecResult::with_output(OutputData::text(format!("WORDS[{words}]")))
    }
}

fn kernel_with_tools() -> Arc<Kernel> {
    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::new());
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    Kernel::with_backend(backend, KernelConfig::isolated(), |_| {}, |tools| {
        tools.register(VerbTool);
        tools.register(TypedTool);
        tools.register(OwnedVerbTool);
    })
    .expect("with_backend kernel")
    .into_arc()
}

async fn run(script: &str) -> String {
    let kernel = kernel_with_tools();
    kernel
        .execute(script)
        .await
        .expect("kernel execute")
        .text_out()
        .trim()
        .to_string()
}

/// The motivating case: a subcommand path, a value flag that belongs to the
/// *leaf*, and a repeated flag. Typed binding would render this as
/// `--limit=5 -- block list` with one `--include`; verbatim hands over exactly
/// what was written.
#[tokio::test]
async fn verbatim_keeps_order_and_repeats() {
    let out = run("verb block list --limit 5 --include a --include b").await;
    assert!(
        out.contains("WORDS[block|list|--limit|5|--include|a|--include|b]"),
        "verbatim words lost order or multiplicity; got {out:?}",
    );
}

/// Typed decomposition is order-independent by design, so nothing but a
/// verbatim binding can carry a flag that must stay *after* its subcommand.
/// This pins that `positional`/`named`/`flags` are left empty for a verbatim
/// tool — a tool that reads both would otherwise see the same argument twice.
#[tokio::test]
async fn verbatim_leaves_the_typed_decomposition_empty() {
    let out = run("verb block list --limit 5").await;
    assert!(out.contains("p=0 n=0 f=0"), "expected an empty typed decomposition; got {out:?}");
}

/// `--json` is the kernel's, at any position. Here it trails the subcommand
/// path, which is where a user of a subcommand tree naturally puts it.
#[tokio::test]
async fn json_is_stripped_from_a_final_position_and_applied() {
    let out = run("verb block list --json").await;
    assert!(
        out.contains("WORDS[block|list]"),
        "--json must not reach a verbatim tool's words; got {out:?}",
    );
    assert!(out.contains("JSON[yes]"), "--json must still be applied; got {out:?}");
}

/// The same removal from a non-final position — the strip is positionally
/// blind, not a trailing-token special case.
#[tokio::test]
async fn json_is_stripped_from_a_non_final_position_and_applied() {
    let out = run("verb --json block list --limit 5").await;
    assert!(
        out.contains("WORDS[block|list|--limit|5]"),
        "--json must be removed wherever it sits; got {out:?}",
    );
    assert!(out.contains("JSON[yes]"), "--json must still be applied; got {out:?}");
}

/// Applied means the kernel really re-rendered the output as JSON, not just
/// that the tool could see the flag. The tool returns text either way, so the
/// visible difference is the JSON string encoding the kernel adds.
#[tokio::test]
async fn json_reaches_the_output_format() {
    let plain = run("verb block list").await;
    assert!(
        plain.starts_with("WORDS["),
        "expected unencoded text without --json; got {plain:?}",
    );
    let jsonified = run("verb block list --json").await;
    assert!(
        jsonified.starts_with('"') && jsonified.contains("WORDS["),
        "expected the kernel's JSON rendering with --json; got {jsonified:?}",
    );
}

/// `--json=true` / `--json=0` are the same kernel flag in `=` form: removed
/// from the words either way, applied only when truthy.
#[tokio::test]
async fn json_equals_form_is_kernel_owned_too() {
    let on = run("verb block --json=true").await;
    assert!(on.contains("JSON[yes]"), "--json=true should apply; got {on:?}");
    let off = run("verb block --json=0").await;
    assert!(off.contains("JSON[no]"), "--json=0 should not apply; got {off:?}");
    assert!(
        off.contains("WORDS[block]"),
        "--json=0 must still be removed from the words; got {off:?}",
    );
}

/// Past `--` every token is an operand, including one spelled `--json`. This
/// matches what the kernel already does for the typed and raw-argv paths.
#[tokio::test]
async fn json_after_double_dash_is_an_operand() {
    let out = run("verb block -- --json").await;
    assert!(
        out.contains("WORDS[block|--|--json]"),
        "a post-`--` --json is the tool's operand; got {out:?}",
    );
    assert!(out.contains("JSON[no]"), "a post-`--` --json must not set the format; got {out:?}");
}

/// Verbatim binding is opt-in. A `Typed` tool decomposes exactly as before and
/// never sees `words`.
#[tokio::test]
async fn typed_tool_is_unaffected() {
    let out = run("typedtool alpha --flag --key=value beta").await;
    assert!(
        out.contains("POS[alpha|beta]"),
        "typed positionals changed; got {out:?}",
    );
    assert!(out.contains("FLAGS[flag]"), "typed flags changed; got {out:?}");
    assert!(out.contains("NAMED[key=value]"), "typed named args changed; got {out:?}");
    assert!(out.contains("WORDS[none]"), "a typed tool must not receive words; got {out:?}");
}

/// `--json` on a typed tool keeps working the way it always has: it lands in
/// `flags` (where a typed tool has always found it) and the kernel renders the
/// output as JSON.
#[tokio::test]
async fn typed_tool_json_still_applies() {
    let out = run("typedtool alpha --json").await;
    assert!(
        out.starts_with('"') && out.contains("POS[alpha]"),
        "typed --json should still render JSON; got {out:?}",
    );
    assert!(out.contains("FLAGS[json]"), "typed --json should still reach flags; got {out:?}");
}

/// A tool that owns its output must receive `--json` in its own argv.
///
/// The two knobs cite the same exemplar (`kj`), so the combination is the
/// realistic case, not a corner. Before this was fixed the kernel stripped
/// `--json` from the words on the tool's behalf and then declined to render
/// JSON itself, because `owns_output` suppresses `apply_output_format` — so the
/// flag reached nobody and asking for JSON did nothing at all.
#[tokio::test]
async fn an_owned_output_tool_receives_json_in_its_own_words() {
    let out = run("ownedverb block list --json").await;
    assert_eq!(
        out, "WORDS[block|list|--json]",
        "a tool that renders its own output must see --json among its words"
    );
}

/// The `--json=VALUE` spelling reaches an owned-output tool intact too — the
/// tool decides what the value means, because it is the one rendering.
#[tokio::test]
async fn an_owned_output_tool_receives_the_json_equals_form_too() {
    let out = run("ownedverb block --json=false").await;
    assert_eq!(
        out, "WORDS[block|--json=false]",
        "the tool owns the flag, including deciding its value is off"
    );
}

/// The lift still happens for a verbatim tool that does NOT own its output —
/// the fix must not hand every verbatim tool a flag the kernel is handling.
#[tokio::test]
async fn a_verbatim_tool_that_does_not_own_output_still_has_json_lifted() {
    let out = run("verb block list --json").await;
    assert!(
        out.contains("WORDS[block|list]") && !out.contains("--json"),
        "--json stays kernel-owned for a tool that does not render its own: {out}"
    );
    assert!(out.contains("JSON[yes]"), "and is still applied: {out}");
    // The kernel rendered the tool's text as a JSON string, which is the half
    // an owned-output tool suppresses — the two rows differ in both directions.
    assert!(out.starts_with('"'), "the kernel rendered this one: {out}");
}
