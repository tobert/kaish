//! The prose a builtin publishes to the model must not name kernel internals.
//!
//! `params_from_clap` copies a clap **argument**'s `///` doc into
//! `ParamSchema.description`, and the kernel ships that to the model through
//! `Kernel::tool_schemas()` — so those lines are published prose, governed at
//! full weight by `docs/style.md`. The leak is easy to reintroduce because the
//! doc comment sits in a Rust file next to code that legitimately discusses
//! clap, and nothing about writing it looks like writing documentation.
//!
//! This walks the real registry rather than grepping the source, so a leak is
//! caught wherever the text comes from: a doc comment, a hand-written
//! `ToolSchema::param` call, or a builtin that composes its schema at runtime.
//!
//! `docs/style.md`, "Do not leak the kernel" is the rule; the sweep that
//! motivated this test is recorded in the git history.
//!
//! Coverage depends on features: the registry only holds what the build
//! enabled, so a default run does not walk `timeout`, `tokens`, or `ps`. Run
//! `cargo test -p kaish-kernel --features full --test published_prose_tests`
//! to cover every capability-gated builtin. Both were green when this landed.
//! (`--all-features` does not build — a `schemars` bound in `kaish-types`
//! fails independently of this test.)

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

/// Substrings that only ever appear when Rust or clap mechanism has escaped
/// into published text. Each was a real leak found while clearing the style
/// guide's punch list, so this list is evidence, not speculation.
const MECHANISM_MARKERS: &[&str] = &[
    "to_argv",
    "consumes=",
    "args.named",
    "args.positional",
    "ArgAction",
    "ParamSchema",
    "ToolArgs",
    "clap",
    "Sink —",
    "Sink -",
    "module docs",
];

/// Every published param description, as `(tool, param, text)`. Walks
/// subcommand trees too — a leaf's params are published exactly like a root's.
fn published_descriptions() -> Vec<(String, String, String)> {
    fn walk(
        prefix: &str,
        schema: &kaish_kernel::tools::ToolSchema,
        out: &mut Vec<(String, String, String)>,
    ) {
        let name = if prefix.is_empty() {
            schema.name.clone()
        } else {
            format!("{prefix} {}", schema.name)
        };
        for param in &schema.params {
            out.push((name.clone(), param.name.clone(), param.description.clone()));
        }
        for sub in &schema.subcommands {
            walk(&name, sub, out);
        }
    }

    let kernel = Kernel::new(KernelConfig::isolated()).expect("isolated kernel");
    let mut out = Vec::new();
    for schema in kernel.tool_schemas() {
        walk("", &schema, &mut out);
    }
    out
}

#[test]
fn published_param_descriptions_name_no_kernel_internals() {
    let mut leaks: Vec<String> = Vec::new();

    for (tool, param, description) in published_descriptions() {
        for marker in MECHANISM_MARKERS {
            if description.contains(marker) {
                leaks.push(format!(
                    "{tool} --{param}: contains {marker:?} — {description}"
                ));
            }
        }
    }

    assert!(
        leaks.is_empty(),
        "these param descriptions ship kernel mechanism to the model. Move the \
         mechanism to a `//` comment and leave the `///` describing what the \
         reader must predict (docs/style.md, \"Do not leak the kernel\"):\n  {}",
        leaks.join("\n  ")
    );
}

/// A published description that is empty tells the model nothing about a flag
/// it can still pass. Catching this is cheap and it is the same class of
/// omission as a leak: the reader cannot predict behavior from what shipped.
#[test]
fn every_published_param_carries_a_description() {
    let missing: Vec<String> = published_descriptions()
        .into_iter()
        .filter(|(_, _, description)| description.trim().is_empty())
        .map(|(tool, param, _)| format!("{tool} --{param}"))
        .collect();

    assert!(
        missing.is_empty(),
        "these params publish an empty description — give the clap field a `///` \
         line describing what it does:\n  {}",
        missing.join("\n  ")
    );
}
