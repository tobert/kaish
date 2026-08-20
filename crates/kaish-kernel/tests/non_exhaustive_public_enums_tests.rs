//! Demonstrates the point of `#[non_exhaustive]` on kaish-kernel's public
//! enums: a downstream crate matching one of them must add a wildcard arm,
//! or it fails to compile.
//!
//! This file lives under `tests/`, so cargo builds it as its own crate that
//! depends on `kaish-kernel` like any embedder would — unlike a `#[cfg(test)]`
//! module inside `src/`, which stays in the defining crate and is unaffected
//! by `#[non_exhaustive]`. Deleting the `_ =>` arm below and running
//! `cargo test -p kaish-kernel --test non_exhaustive_public_enums_tests`
//! reproduces the compile error this attribute exists to force:
//!
//! ```text
//! error[E0004]: non-exhaustive patterns: `_` not covered
//!   = note: `SpillMode` is marked as non-exhaustive, so a wildcard `_` is
//!           necessary to match exhaustively
//! ```
//!
//! That is the mechanism behind five undeclared breaking changes in 0.15.0:
//! an embedder's exhaustive match over a kaish-kernel enum silently stopped
//! covering reality the moment a variant was added. `#[non_exhaustive]` moves
//! that discovery from a bug report to a compiler error, at the embedder's
//! own build.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::ast::{BinaryOp, RedirectKind};
use kaish_kernel::ignore_config::IgnoreScope;
use kaish_kernel::output_limit::SpillMode;

/// A wildcard arm is mandatory here — remove it and this file stops
/// compiling, because `SpillMode` is `#[non_exhaustive]`.
fn describe_spill_mode(mode: SpillMode) -> &'static str {
    match mode {
        SpillMode::Disk => "spills overflow to a file",
        SpillMode::Memory => "truncates in memory",
        // A future SpillMode variant lands here, not in a silently-wrong
        // "disk" or "memory" bucket.
        _ => "unknown spill mode — kaish-kernel added a variant this build predates",
    }
}

/// Same contract for a config enum.
fn describe_ignore_scope(scope: IgnoreScope) -> &'static str {
    match scope {
        IgnoreScope::Advisory => "advisory",
        IgnoreScope::Enforced => "enforced",
        _ => "unknown ignore scope — kaish-kernel added a variant this build predates",
    }
}

/// Same contract for an AST node enum — these grow as the grammar grows.
fn describe_redirect_kind(kind: &RedirectKind) -> &'static str {
    match kind {
        RedirectKind::StdoutOverwrite => ">",
        RedirectKind::StdoutAppend => ">>",
        RedirectKind::Stdin => "<",
        RedirectKind::HereDoc(_) => "<<",
        RedirectKind::HereString => "<<<",
        RedirectKind::Stderr => "2>",
        RedirectKind::Both => "&>",
        RedirectKind::MergeStderr => "2>&1",
        RedirectKind::MergeStdout => "1>&2",
        _ => "unknown redirect kind — kaish-kernel added a variant this build predates",
    }
}

#[test]
fn spill_mode_wildcard_arm_covers_both_current_variants() {
    assert_eq!(describe_spill_mode(SpillMode::Disk), "spills overflow to a file");
    assert_eq!(describe_spill_mode(SpillMode::Memory), "truncates in memory");
}

#[test]
fn ignore_scope_wildcard_arm_covers_both_current_variants() {
    assert_eq!(describe_ignore_scope(IgnoreScope::Advisory), "advisory");
    assert_eq!(describe_ignore_scope(IgnoreScope::Enforced), "enforced");
}

#[test]
fn redirect_kind_wildcard_arm_covers_every_current_variant() {
    assert_eq!(describe_redirect_kind(&RedirectKind::StdoutOverwrite), ">");
    assert_eq!(describe_redirect_kind(&RedirectKind::MergeStdout), "1>&2");
}

/// `BinaryOp` is the deliberate exception (see its doc comment in
/// `ast/types.rs`): it is not `#[non_exhaustive]`, so this exhaustive match
/// needs no wildcard and still compiles. If a variant is ever added here, it
/// is meant to be a compile error for every embedder — the point being made,
/// not a bug.
#[test]
fn binary_op_stays_exhaustively_matchable_on_purpose() {
    fn describe(op: BinaryOp) -> &'static str {
        match op {
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
        }
    }
    assert_eq!(describe(BinaryOp::And), "&&");
    assert_eq!(describe(BinaryOp::Or), "||");
}
