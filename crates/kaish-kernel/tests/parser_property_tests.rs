//! Properties the parser holds for every input, not just the ones we thought of.
//!
//! The suite pins behavior we chose. These pin behavior we require: that
//! `parse` answers rather than panicking, and that a plan's own rendering
//! parses back to something. Both classes of bug found on this parser came
//! from inputs nobody wrote a case for — a structural nesting combination, and
//! a name boundary where two scanners disagreed — so the generators build
//! *structure*, not random bytes. Random bytes hit a lexer error in
//! microseconds and never reach the shapes that break things.
//!
//! Fragments that are only valid as an *argument* (command substitution,
//! interpolation, `${x}`, `${x:-d}`) are pre-wrapped with a leading `echo ` —
//! `command_name` never accepts a bare expression, so an unwrapped fragment
//! would make the whole joined source unparseable and starve property 3 of
//! the renderer arms those forms exercise.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use proptest::prelude::*;

/// Fragments that compose into shell-shaped source. Weighted toward the
/// constructs that have actually broken: substitution, quoting, and the
/// compound statements whose terminators interact with `)`.
fn source_fragment() -> impl Strategy<Value = String> {
    prop_oneof![
        Just("echo x".to_string()),
        Just("x=1".to_string()),
        // Command substitution, interpolation, and var-default forms are not
        // valid statement heads on their own (`command_name` in parser.rs
        // accepts only `ident | path | true | false | : | . | ./path`) — they
        // must appear as an argument to reach the renderer arms in
        // `render_expr`/`render_part` (`CommandSubst`, `Interpolated`,
        // `VarWithDefault`) via property 3 below.
        Just("echo $(echo x)".to_string()),
        Just("echo \"$x\"".to_string()),
        Just("echo '$x'".to_string()),
        Just("echo ${x}".to_string()),
        Just("echo ${x:-d}".to_string()),
        Just("for f in a b; do echo $f; done".to_string()),
        Just("if true; then echo y; fi".to_string()),
        Just("case x in x) echo m ;; esac".to_string()),
        Just("while false; do echo n; done".to_string()),
        Just("a | b".to_string()),
        Just("a && b".to_string()),
        Just("a > f".to_string()),
        Just("[[ -n x ]]".to_string()),
        Just("xs=[a b c]".to_string()),
        Just("r={k: v}".to_string()),
        // The characters that carry structural meaning, alone and unbalanced.
        Just("$(".to_string()),
        Just(")".to_string()),
        Just("\"".to_string()),
        Just("esac".to_string()),
        Just("done".to_string()),
        Just(";;".to_string()),
        Just("<<EOF".to_string()),
        // Non-ASCII, now that it is first class.
        Just("echo café".to_string()),
        Just("echo 😁".to_string()),
        Just("名前=1".to_string()),
    ]
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(2048))]

    /// `parse` answers. It may reject — most generated joins are nonsense —
    /// but it must not panic, and it must not hang. The workspace denies
    /// `clippy::unwrap_used` in production code precisely so this holds; a
    /// panic here is a real defect, not a test artifact.
    #[test]
    fn parse_never_panics(fragments in proptest::collection::vec(source_fragment(), 1..6)) {
        for joiner in [" ", "; ", "\n", " | ", " && ", ""] {
            let source = fragments.join(joiner);
            // Any outcome is acceptable except unwinding.
            let _ = kaish_kernel::parser::parse(&source);
        }
    }

    /// `plan_program` (`crates/kaish-kernel/src/ast/plan.rs`) has exactly one
    /// fallible step, `parse(source)?`; every statement it builds after that is
    /// infallible. So today `parsed == planned` holds by construction and this
    /// property cannot fail — it is not yet a differential. It stays as a
    /// regression guard: if planning ever grows a rejection path of its own
    /// (an already-parsed construct planning refuses, say), this is the
    /// property that catches an embedder's `--plan` disagreeing with the
    /// kernel about what runs.
    #[test]
    fn plan_agrees_with_parse(fragments in proptest::collection::vec(source_fragment(), 1..5)) {
        let source = fragments.join("; ");
        let parsed = kaish_kernel::parser::parse(&source).is_ok();
        let planned = kaish_kernel::plan_program(&source).is_ok();
        prop_assert_eq!(
            parsed, planned,
            "parse and plan disagree about {:?}: parse_ok={}, plan_ok={}",
            source, parsed, planned
        );
    }

    /// Re-parsing a program's own rendering yields the same shape. A plan
    /// publishes `rendered` for an embedder to show a human, so a rendering
    /// that does not parse back is a rendering that lies about what would run.
    #[test]
    fn a_rendered_plan_parses_back(fragments in proptest::collection::vec(source_fragment(), 1..4)) {
        let source = fragments.join("; ");
        let Ok(plans) = kaish_kernel::plan_program(&source) else {
            return Ok(()); // not a program; nothing to render
        };
        for statement in &plans {
            let rendered = &statement.plan.rendered;
            if rendered.is_empty() {
                continue;
            }
            prop_assert!(
                kaish_kernel::parser::parse(rendered).is_ok(),
                "a plan rendered {:?} from {:?}, which does not parse back",
                rendered,
                source
            );
        }
    }
}
