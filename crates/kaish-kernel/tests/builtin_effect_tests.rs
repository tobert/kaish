//! Every built-in tool declares its `ToolEffect` (`read_only`, `session`,
//! `write`, `delegate`) on its `ToolSchema`.
//!
//! Drift guard: adding a builtin to `register_builtins` without calling
//! `.with_effect(...)` in its `schema()` fails this suite. Embedder gates
//! (such as kaijutsu's consent gate) derive their read-only builtin list from
//! `ToolSchema::is_read_only()` instead of maintaining a hand-rolled list.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::collections::BTreeMap;

use kaish_kernel::tools::{register_builtins, ToolEffect, ToolRegistry};
use kaish_kernel::{Kernel, KernelConfig};

#[test]
fn every_registered_builtin_declares_an_effect() {
    let mut registry = ToolRegistry::new();
    register_builtins(&mut registry);

    let missing: Vec<String> = registry
        .schemas()
        .iter()
        .filter(|s| s.effect.is_none())
        .map(|s| s.name.clone())
        .collect();
    assert!(
        missing.is_empty(),
        "registered builtins without ToolSchema::effect (call .with_effect(...) in schema()): {missing:?}"
    );
}

#[test]
fn every_builtin_with_destructive_operations_is_write() {
    let mut registry = ToolRegistry::new();
    register_builtins(&mut registry);

    let mismatched: Vec<(String, Option<ToolEffect>, Vec<String>)> = registry
        .schemas()
        .iter()
        .filter(|s| !s.operations.is_empty() && s.effect != Some(ToolEffect::Write))
        .map(|s| (s.name.clone(), s.effect, s.operations.clone()))
        .collect();
    assert!(
        mismatched.is_empty(),
        "builtins declaring destructive operations must have ToolEffect::Write: {mismatched:?}"
    );
}

#[test]
fn key_builtins_have_expected_effect_classes() {
    let mut registry = ToolRegistry::new();
    register_builtins(&mut registry);
    let by_name: BTreeMap<String, Option<ToolEffect>> = registry
        .schemas()
        .into_iter()
        .map(|s| (s.name, s.effect))
        .collect();

    for name in [
        "cat", "ls", "grep", "find", "glob", "head", "tail", "wc", "stat",
        "diff", "cmp", "jq", "seq", "echo", "printf", "sort", "uniq", "cut",
        "awk", "command", "type", "kaish-validate", "kaish-tools", "plan", "help", "test",
    ] {
        assert_eq!(
            by_name.get(name).copied().flatten(),
            Some(ToolEffect::ReadOnly),
            "`{name}` must be ReadOnly"
        );
    }

    for name in ["cd", "export", "unset", "set", "alias", "unalias", "read", "push", "kaish-clear"] {
        assert_eq!(
            by_name.get(name).copied().flatten(),
            Some(ToolEffect::Session),
            "`{name}` must be Session"
        );
    }

    for name in [
        "rm", "mv", "cp", "write", "edit", "patch", "sed", "tee", "dd",
        "mkdir", "touch", "ln", "mktemp", "kill", "jobs", "kaish-vfs", "kaish-trash",
    ] {
        assert_eq!(
            by_name.get(name).copied().flatten(),
            Some(ToolEffect::Write),
            "`{name}` must be Write"
        );
    }

    for name in ["env", "timeout", "scatter", "gather"] {
        assert_eq!(
            by_name.get(name).copied().flatten(),
            Some(ToolEffect::Delegate),
            "`{name}` must be Delegate"
        );
    }
}

#[test]
fn kernel_tool_schemas_exposes_effect_metadata() {
    let kernel = Kernel::new(KernelConfig::isolated()).expect("kernel");
    let cat = kernel
        .tool_schemas()
        .into_iter()
        .find(|s| s.name == "cat")
        .expect("cat schema");
    assert!(cat.is_read_only());

    let rm = kernel
        .tool_schemas()
        .into_iter()
        .find(|s| s.name == "rm")
        .expect("rm schema");
    assert!(!rm.is_read_only());
    assert_eq!(rm.effect, Some(ToolEffect::Write));
}
