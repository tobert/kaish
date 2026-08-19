//! Structured output survives to the embedder.
//!
//! Builtins build an `OutputData` tree — `ls` tags every node with an
//! `EntryType` whose own doc calls it a "rendering hint (colors, icons)". An
//! embedder that wants to lay out or colorize that listing itself needs the
//! tree, not the tab-separated rendering of it.
//!
//! The kernel already *intends* this: `Kernel::execute`'s statement loop
//! carries the last statement's tree out explicitly ("for MCP TOON encoding",
//! kernel.rs). It never arrived, because `apply_redirects` ended with an
//! unconditional `materialize()` that folded the tree into `.out` and dropped
//! it before the carry could read it. Nothing needed that eager fold: every
//! reader goes through `text_out()`, which already renders `.output` when
//! `.out` is empty.
//!
//! So the contract under test is the kernel's existing one — **the last
//! statement's tree reaches the caller** — plus the guarantee that making it
//! arrive changes no text anywhere.

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

/// Across statements it is the LAST one's tree — the kernel's existing rule,
/// chosen at the loop rather than in `accumulate_result` precisely because
/// accumulation also runs per loop iteration. The earlier statements' text is
/// still all there, so nothing is lost by carrying the tree.
#[tokio::test]
async fn the_last_statements_tree_is_the_one_carried() {
    let dir = fixture();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("ls; ls").await.expect("execution failed");

    let output = result.output().expect("the last statement's tree");
    assert_eq!(output.root.len(), 2, "one tree, not two concatenated");
    let text = result.text_out();
    assert_eq!(
        text.matches("subdir").count(),
        2,
        "both statements' text must survive, got {text:?}"
    );
}

/// A statement that produces no tree carries none — `echo` last means the
/// caller gets text, not a stale listing from the statement before it.
#[tokio::test]
async fn a_text_only_last_statement_carries_no_tree() {
    let dir = fixture();
    let kernel = kernel_at(dir.path());

    let result = kernel
        .execute("ls; echo hello")
        .await
        .expect("execution failed");

    assert!(
        result.output().is_none(),
        "echo has no tree, so none may be carried from the ls before it"
    );
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
