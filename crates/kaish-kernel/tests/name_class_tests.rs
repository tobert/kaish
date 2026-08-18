//! A variable name has to read as what it is.
//!
//! Names accept identifiers in any script plus emoji. They refuse characters
//! that do not show themselves, because a name the reader cannot see is a name
//! the reader cannot check: `a\u{a0}b` renders as two words and is one name,
//! and `a\u{200b}b` renders as `ab` and is a different variable from `ab`.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated()).expect("kernel")
}

async fn run(source: &str) -> (i64, String) {
    let k = kernel();
    let r = k.execute(source).await.expect("kernel execute");
    (r.code, r.text_out().trim().to_string())
}

/// Everything a reader can see stays legal, in any script.
#[tokio::test]
async fn visible_names_still_work() {
    for (source, expected) in [
        ("v=ok; echo $v", "ok"),
        ("_v=ok; echo $_v", "ok"),
        ("café=ok; echo $café", "ok"),
        ("名前=ok; echo \"${名前}\"", "ok"),
        ("переменная=ok; echo $переменная", "ok"),
        ("Ω=ok; echo ${Ω}", "ok"),
        ("😁=grin; echo $😁", "grin"),
        ("x😁=ok; echo $x😁", "ok"),
        // An emoji ZWJ sequence is one glyph, so the joiner inside it is fine.
        ("👨\u{200d}👩=family; echo ${👨\u{200d}👩}", "family"),
        // A variation selector picks the emoji rendering of the glyph before it.
        ("❤\u{fe0f}=love; echo ${❤\u{fe0f}}", "love"),
    ] {
        let (code, out) = run(source).await;
        assert_eq!(code, 0, "{source:?} exited {code}");
        assert_eq!(out, expected, "for: {source:?}");
    }
}

/// A name holding whitespace looks like more than one word. Refused, and the
/// message names the codepoint — the reader cannot see it otherwise.
#[tokio::test]
async fn whitespace_in_a_name_is_refused_by_codepoint() {
    for (source, codepoint) in [
        ("a\u{a0}b=1", "U+00A0"),
        ("a\u{3000}b=1", "U+3000"),
        ("echo $a\u{a0}b", "U+00A0"),
    ] {
        let k = kernel();
        let err = k.execute(source).await;
        let text: String = match err {
            Err(e) => format!("{e:#}"),
            Ok(r) => {
                assert_ne!(r.code, 0, "{source:?} should not succeed");
                r.text_out().to_string()
            }
        };
        assert!(text.contains(codepoint), "{source:?} must name {codepoint}, got: {text}");
        assert!(text.contains("quote"), "the message must say what to do: {text}");
    }
}

/// Zero-width and bidirectional controls change what the source appears to
/// say. Refused for the same reason.
#[tokio::test]
async fn invisible_characters_in_a_name_are_refused() {
    for source in [
        "a\u{200b}b=1",  // ZERO WIDTH SPACE — renders as `ab`
        "a\u{202e}b=1",  // RIGHT-TO-LEFT OVERRIDE — reorders what follows
        "a\u{200c}b=1",  // ZERO WIDTH NON-JOINER
        "a\u{feff}b=1",  // ZERO WIDTH NO-BREAK SPACE
        "a\u{ad}b=1",    // SOFT HYPHEN
        "a\u{200d}b=1",  // ZWJ between letters, not carrying an emoji
    ] {
        let k = kernel();
        let failed = match k.execute(source).await {
            Err(_) => true,
            Ok(r) => r.code != 0,
        };
        assert!(failed, "{source:?} should be refused");
    }
}

/// The class is identifiers plus emoji, not "anything non-ASCII" — typography
/// and mathematics are not names.
#[tokio::test]
async fn non_identifier_symbols_are_refused() {
    for source in ["a→b=1", "a«b=1", "a⌘b=1"] {
        let k = kernel();
        let failed = match k.execute(source).await {
            Err(_) => true,
            Ok(r) => r.code != 0,
        };
        assert!(failed, "{source:?} should be refused");
    }
}

/// The refusal is about NAMES. The same characters in data are untouched —
/// this is the line the change must not cross.
#[tokio::test]
async fn the_same_characters_are_fine_as_data() {
    for (source, expected) in [
        ("echo \"a\u{a0}b\"", "a\u{a0}b"),
        ("echo \"a\u{200b}b\"", "a\u{200b}b"),
        ("echo \"a→b\"", "a→b"),
        ("x=\"a\u{a0}b\"; echo \"$x\"", "a\u{a0}b"),
        ("echo a→b", "a→b"),
    ] {
        let (code, out) = run(source).await;
        assert_eq!(code, 0, "{source:?} exited {code}");
        assert_eq!(out, expected, "for: {source:?}");
    }
}

// ── property: the four doors agree, for names we did not think of ───────

/// Names built from a generator rather than a list, because the bug this file
/// exists for was a *boundary* between two scanners — the kind of defect that
/// hides between the cases anyone writes by hand.
fn a_legal_name() -> impl proptest::strategy::Strategy<Value = String> {
    use proptest::prelude::*;
    proptest::collection::vec(
        prop_oneof![
            Just('a'), Just('Z'), Just('_'), Just('7'),
            Just('é'), Just('名'), Just('Ω'), Just('д'), Just('😁'),
        ],
        1..6,
    )
    .prop_map(|chars| chars.into_iter().collect::<String>())
    // A name cannot start with a digit — that is a positional parameter.
    .prop_filter("no digit-leading names", |s| !s.starts_with(|c: char| c.is_ascii_digit()))
}

proptest::proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(256))]

    /// Whatever the name, `$N`, `${N}`, `"$N"`, and `"${N}"` reach the same
    /// variable. This is the property that would have caught the truncation
    /// bug before a line of the fix was written: the quoted door collected a
    /// name with an ASCII-only rule and silently read a different variable.
    #[test]
    fn every_spelling_of_a_reference_agrees(name in a_legal_name()) {
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("runtime");
        let outputs: Vec<String> = runtime.block_on(async {
            let mut seen = Vec::new();
            for reference in [
                format!("echo ${name}"),
                format!("echo ${{{name}}}"),
                format!("echo \"${name}\""),
                format!("echo \"${{{name}}}\""),
            ] {
                let k = kernel();
                let source = format!("{name}=bound; {reference}");
                let r = k.execute(&source).await.expect("kernel execute");
                seen.push(r.text_out().trim().to_string());
            }
            seen
        });
        proptest::prop_assert!(
            outputs.iter().all(|o| o == "bound"),
            "the four doors disagreed for name {name:?}: {outputs:?}"
        );
    }
}

/// The text of a refusal, however it arrived. A name caught before execution
/// comes back as an `Err`; one caught by a builtin comes back as a nonzero
/// result. Both are refusals, and a test that knows only one shape would call
/// half the doors broken.
async fn refusal(source: &str) -> Option<String> {
    let k = kernel();
    match k.execute(source).await {
        Err(e) => Some(format!("{e:#}")),
        // A builtin's refusal goes to `err`, a parse-time one to the `Err`
        // above — read both, or half the doors look like they said nothing.
        Ok(r) if r.code != 0 => Some(format!("{}{}", r.text_out(), r.err)),
        Ok(_) => None,
    }
}

// ── every door, not just the four written spellings ──────────────────────

/// The doors a name can enter through that are *not* `$x`/`${x}`/`"$x"`/`N=`.
///
/// Each one is a place a name arrives as a runtime word or under a keyword,
/// where the parser's name scan does not meet it as a name-carrying token. A
/// door missing from this rule is the silent-write shape the whole name class
/// exists to close: bound through one spelling, refused by every read.
fn every_name_door(name: &str) -> Vec<(&'static str, String)> {
    vec![
        ("assign", format!("{name}=1")),
        ("read $x", format!("echo ${name}")),
        ("read \"$x\"", format!("echo \"${name}\"")),
        ("read \"${x}\"", format!("echo \"${{{name}}}\"")),
        ("read \"${x:-d}\"", format!("echo \"${{{name}:-d}}\"")),
        ("read \"${#x}\"", format!("echo \"${{#{name}}}\"")),
        ("for", format!("for {name} in 1; do echo hi; done")),
        ("export", format!("export {name}=1")),
        ("read builtin", format!("read {name}")),
        ("unset", format!("unset {name}")),
        ("push", format!("push {name} 1")),
    ]
}

/// Every door refuses an invisible name, and says which character it refused.
///
/// The zero-width space is the sharp case: `a\u{200b}b` renders as `ab`, so a
/// door that accepts it binds a variable the author believes is `ab` and no
/// read can reach.
#[tokio::test]
async fn every_door_refuses_an_invisible_name() {
    let name = "a\u{200b}b";
    for (door, source) in every_name_door(name) {
        let text = refusal(&source)
            .await
            .unwrap_or_else(|| panic!("the `{door}` door accepted an invisible name: {source:?}"));
        assert!(
            text.contains("U+200B"),
            "the `{door}` door refused without naming the character: {text}"
        );
    }
}

/// The same doors, for a name holding whitespace that does not look like it.
#[tokio::test]
async fn every_door_refuses_a_no_break_space() {
    let name = "a\u{00a0}b";
    for (door, source) in every_name_door(name) {
        let text = refusal(&source)
            .await
            .unwrap_or_else(|| panic!("the `{door}` door accepted a no-break space: {source:?}"));
        assert!(
            text.contains("U+00A0"),
            "the `{door}` door refused without naming the character: {text}"
        );
    }
}

/// A legal name works through every one of those doors, so the refusals above
/// are the rule doing its job rather than the doors being broken.
#[tokio::test]
async fn every_door_accepts_a_visible_name() {
    for source in [
        "café=1; echo $café",
        "café=1; echo \"$café\"",
        "for café in 1; do echo $café; done",
        "export café=1",
        "x=1; unset x",
        "xs=[]; push xs 1",
        "名前=1; echo \"${名前:-d}\"",
    ] {
        let (code, out) = run(source).await;
        assert_eq!(code, 0, "a visible name was refused: {source:?} -> {out:?}");
    }
}

/// `export` refuses a dotted name and teaches the bracket form.
///
/// The assignment door already refuses `a.b=1`, and `${a.b}` is a loud
/// brackets-only error — so an `export` that accepted it would be the one
/// door minting a variable no read path can reach.
#[tokio::test]
async fn export_refuses_a_dotted_name() {
    let text = refusal("export a.b=1").await.expect("export accepted a dotted name");
    assert!(text.contains("bracket access"), "export did not teach the bracket form: {text}");
    assert!(text.contains("a[b]"), "export did not show the corrected spelling: {text}");
}

/// `scatter --as` refuses a name no read could reach, at option-parse time.
#[tokio::test]
async fn scatter_as_refuses_an_invisible_name() {
    let text = refusal("echo 1 | scatter --as a\u{200b}b -- echo hi | gather")
        .await
        .expect("scatter --as accepted an invisible name");
    assert!(text.contains("U+200B"), "scatter --as refused without naming it: {text}");
}

// ── property: no name is legal at one door and illegal at another ────────

/// A name with exactly one invisible character spliced into an otherwise
/// legal one. The generator builds the *illegal* half of the space — the
/// legal-name property above cannot see a door disagreement, because a door
/// that never refuses anything agrees with every other door on legal input.
fn an_illegal_name() -> impl proptest::strategy::Strategy<Value = String> {
    use proptest::prelude::*;
    (
        proptest::collection::vec(
            prop_oneof![Just('a'), Just('Z'), Just('_'), Just('é'), Just('名')],
            1..4,
        ),
        prop_oneof![
            Just('\u{200b}'), // ZERO WIDTH SPACE
            Just('\u{00a0}'), // NO-BREAK SPACE
            Just('\u{00ad}'), // SOFT HYPHEN
            Just('\u{2066}'), // LEFT-TO-RIGHT ISOLATE
        ],
        proptest::collection::vec(
            prop_oneof![Just('a'), Just('Z'), Just('_'), Just('é'), Just('名')],
            1..4,
        ),
    )
        .prop_map(|(head, bad, tail)| {
            let mut s: String = head.into_iter().collect();
            s.push(bad);
            s.extend(tail);
            s
        })
}

proptest::proptest! {
    #![proptest_config(proptest::test_runner::Config::with_cases(64))]

    /// Whatever the illegal name, every door refuses it. One door accepting
    /// what the others refuse is the silent write this class exists to close,
    /// and it is a boundary defect — the kind that hides between hand cases.
    #[test]
    fn no_door_accepts_what_another_refuses(name in an_illegal_name()) {
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("runtime");
        let unrefused: Vec<&str> = runtime.block_on(async {
            let mut unrefused = Vec::new();
            for (door, source) in every_name_door(&name) {
                // Not merely "it failed" — `read NAME` fails for want of
                // stdin whatever the name, and that would pass this property
                // without the rule ever running. The refusal has to name the
                // character it refused.
                let named = refusal(&source).await.is_some_and(|t| t.contains("U+"));
                if !named {
                    unrefused.push(door);
                }
            }
            unrefused
        });
        proptest::prop_assert!(
            unrefused.is_empty(),
            "these doors did not refuse the illegal name {name:?}: {unrefused:?}"
        );
    }
}

// ── an argv `key=value` word is data, not a name ─────────────────────────

/// `echo k=v` passes a word; `k=1` binds a variable. Both spell `Ident` `=`,
/// and only the second is a name — a word's bytes are its own, so refusing one
/// for holding a character a *name* may not hold rejects a valid program.
#[tokio::test]
async fn an_argv_key_value_word_is_not_a_name() {
    for source in [
        "echo a\u{200b}b=bar",
        "echo a\u{00a0}b=bar",
        "echo k=v",
        "echo a\u{200b}b",
    ] {
        let (code, out) = run(source).await;
        assert_eq!(code, 0, "an argv word was refused as if it were a name: {source:?} -> {out:?}");
    }
}

/// The assignment spelling is still refused wherever a statement can start.
/// These are the positions the argv rule above must not have opened up.
#[tokio::test]
async fn an_assignment_target_is_a_name_at_every_statement_start() {
    let name = "a\u{200b}b";
    for source in [
        format!("{name}=1"),
        format!("x=1; {name}=1"),
        format!("true && {name}=1"),
        format!("true || {name}=1"),
        format!("local {name} = 1"),
        format!("if true; then {name}=1; fi"),
        format!("for i in 1; do {name}=1; done"),
    ] {
        assert!(
            refusal(&source).await.is_some(),
            "an assignment target was accepted: {source:?}"
        );
    }
}

/// CANARY — a known gap, deliberately pinned so it cannot go quiet.
///
/// A *second* assignment in an env-scoped prefix (`x=1 BAD=2 cmd`) is not
/// reached by the name rule. The parse-time scan tells an assignment target
/// from an argv word by what precedes it, and here both are preceded by the
/// previous assignment's value, so the two are indistinguishable one token
/// back. The first assignment in the prefix IS checked.
///
/// **If this test fails, the gap was closed — that is good news.** Delete this
/// test and add the case to `every_name_door`. The intended fix is in
/// `validator/walker.rs::validate_assignment`, which sees every `Assignment`
/// including each one in `Stmt::EnvScoped` and has no ambiguity about what is
/// a target; it needs a new `IssueCode`, which is why it did not land here —
/// `E018` was being taken by the `#`-at-word-start branch at the same time and
/// two branches minting the same code would collide on merge.
#[tokio::test]
async fn canary_env_prefix_second_assignment_escapes_the_name_rule() {
    let (code, _out) = run("x=1 a\u{200b}b=2 echo hi").await;
    assert_eq!(
        code, 0,
        "the env-prefix gap is closed — see this test's doc comment for what to do"
    );
}
