//! Structured output survives to the embedder.
//!
//! Builtins build an `OutputData` tree — `ls` tags every node with an
//! `EntryType` whose own doc calls it a "rendering hint (colors, icons)". An
//! embedder that wants to lay out or colorize that listing itself needs the
//! tree, not the tab-separated rendering of it.
//!
//! Before this, `Kernel::execute` always returned `output() == None`:
//! `accumulate_result` calls `materialize()`, which renders the tree into
//! `.out` and drops it. The rule here is the one the rest of the code already
//! assumes (`take_output_for_stream`, the REPL's renderer): **structured
//! output survives only when it is the whole of the output.** Two statements
//! cannot both keep a tree — there is no meaningful concatenation of two, and
//! a half-structured result would render one and silently drop the other.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

use kaish_kernel::interpreter::EntryType;
use kaish_kernel::{Kernel, KernelConfig};

fn kernel_at(dir: &std::path::Path) -> Kernel {
    Kernel::new(
        KernelConfig::repl()
            .with_cwd(dir.to_path_buf())
            .with_trash(false),
    )
    .expect("failed to create kernel")
}

/// A fixture with one directory and one plain file, so `EntryType` has two
/// values to distinguish.
fn fixture() -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::create_dir(dir.path().join("subdir")).expect("mkdir");
    std::fs::write(dir.path().join("plain.txt"), b"x").expect("write");
    dir
}

/// The headline: an embedder calling `execute` gets the tree, with the
/// per-node type that makes colorizing possible at all.
#[tokio::test]
async fn an_embedder_receives_the_structured_tree() {
    let dir = fixture();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("ls").await.expect("execution failed");
    let output = result
        .output()
        .expect("ls must hand its structured tree to the embedder");

    let mut seen: Vec<(&str, String)> = output
        .root
        .iter()
        .map(|n| (n.name.as_str(), format!("{:?}", n.entry_type)))
        .collect();
    seen.sort();

    assert_eq!(
        seen,
        vec![
            ("plain.txt", format!("{:?}", EntryType::File)),
            ("subdir", format!("{:?}", EntryType::Directory)),
        ],
        "the embedder must be able to tell a directory from a file"
    );
}

/// The text rendering keeps working. Preserving the tree must not change what
/// a shell user or a pipe sees.
#[tokio::test]
async fn the_text_rendering_is_unchanged() {
    let dir = fixture();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("ls").await.expect("execution failed");
    let text = result.text_out();
    let mut lines: Vec<&str> = text.trim().lines().collect();
    lines.sort();
    assert_eq!(lines, vec!["plain.txt", "subdir"]);
}

/// Two statements fall back to text. There is no meaningful concatenation of
/// two trees, and keeping the second while `.out` holds the first would render
/// one statement and silently drop the other — the exact bug the old
/// unconditional `materialize()` was avoiding.
#[tokio::test]
async fn two_producing_statements_fall_back_to_text() {
    let dir = fixture();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("ls; ls").await.expect("execution failed");

    assert!(
        result.output().is_none(),
        "a two-statement program must not present one statement's tree as the whole result"
    );
    let text = result.text_out();
    assert_eq!(
        text.matches("subdir").count(),
        2,
        "both statements' output must survive as text, got {text:?}"
    );
}

/// Text before a tree also falls back — the invariant is "structured output is
/// the WHOLE output", not "the last statement wins".
#[tokio::test]
async fn text_before_a_tree_falls_back_to_text() {
    let dir = fixture();
    let kernel = kernel_at(dir.path());

    let result = kernel
        .execute("echo hello; ls")
        .await
        .expect("execution failed");

    assert!(result.output().is_none(), "echo's text must not be dropped");
    let text = result.text_out();
    assert!(text.contains("hello"), "lost the echo: {text:?}");
    assert!(text.contains("subdir"), "lost the listing: {text:?}");
}

/// A pipeline's last stage is what the embedder sees, and a stage that emits
/// plain text leaves nothing structured behind.
#[tokio::test]
async fn a_pipeline_through_a_text_filter_has_no_tree() {
    let dir = fixture();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("ls | cat").await.expect("execution failed");
    assert!(result.text_out().contains("subdir"));
}

/// `--json` still wins. The JSON renderer consumes the tree, and the embedder
/// asked for JSON — handing back a tree as well would be two answers.
#[tokio::test]
async fn json_output_is_unaffected() {
    let dir = fixture();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("ls --json").await.expect("execution failed");
    let text = result.text_out();
    assert!(text.trim_start().starts_with('['), "expected JSON, got {text:?}");
    assert!(text.contains("subdir"), "got {text:?}");
}
