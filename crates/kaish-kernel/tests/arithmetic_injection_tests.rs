//! kaish's arithmetic-injection invariant: parsed SOURCE may run an
//! explicit `$(...)`, but a VALUE that reaches `$(( ))` — through a
//! variable, a collection element, an index, or command output — is never
//! reparsed as source.
//!
//! The payload is Vidar Holen's bash arithmetic-injection example
//! (<https://www.thegreenplace.net/> / shellharden's writeups cover the
//! same bug class): a string shaped like an array-subscript expression
//! that hides a command substitution. bash's `(( ))` and `[[ x -eq y ]]`
//! both re-evaluate such a string as an expression, running the hidden
//! command. kaish's own arithmetic coercion (`arithmetic::parse_numeric_string`
//! and the `[[`/`test` numeric-compare path via `value_to_num`, both in
//! `crates/kaish-kernel/src/`) never calls the expression parser on a
//! resolved value — only on literal `$(( ))` SOURCE text — so the payload
//! must always refuse loudly with the injected command never having run.
//!
//! Stderr assertion method: the refusal diagnostic legitimately quotes the
//! payload back (`` `num` holds `a[$(echo INJECTED >&2)]+42` ``), so
//! `stderr.contains("INJECTED")` is true for BOTH a real command run and a
//! quoted-back error message — a naive substring check cannot tell them
//! apart and would pass even if the command actually ran. Instead this file
//! checks for a stderr LINE that is EXACTLY the marker: `echo INJECTED >&2`
//! prints `INJECTED` as its own line if it runs; the diagnostic only ever
//! embeds `INJECTED` inside a longer sentence, never alone on a line.
//! `stderr_has_exact_marker_line` proves this distinction works by testing
//! both a refusal (no exact line) and a script that genuinely runs the
//! command (an exact line present).
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};
use rstest::rstest;

mod common;

const MARKER: &str = "INJECTED";

/// The classic payload: array-subscript-shaped text hiding a command
/// substitution that writes the marker to stderr, then a bareword tail
/// (`+42`) so a naive "strip to digits" reader would still see a number.
const PAYLOAD: &str = "a[$(echo INJECTED >&2)]+42";

struct Ran {
    code: i64,
    stdout: String,
    stderr: String,
}

/// Some refusals (observed on `[[ ]]`'s numeric comparators) surface as a
/// hard `Err` from `Kernel::execute` rather than an `Ok` result carrying a
/// nonzero code and a populated `err` field — both are "refused," so this
/// folds either shape into one `Ran` instead of panicking on the `Err` arm.
async fn run(source: &str) -> Ran {
    let k = Kernel::new(KernelConfig::isolated()).expect("kernel");
    match k.execute(source).await {
        Ok(r) => Ran { code: r.code, stdout: r.text_out().trim().to_string(), stderr: r.err.clone() },
        Err(e) => Ran { code: -1, stdout: String::new(), stderr: format!("{e}") },
    }
}

/// True only when some stderr LINE is exactly `marker` — not a substring
/// match, which the refusal diagnostic's quoted-back payload would also
/// satisfy.
fn stderr_has_exact_marker_line(stderr: &str, marker: &str) -> bool {
    stderr.lines().any(|line| line == marker)
}

/// Asserts the injected command never ran: kaish refused (nonzero exit)
/// and no stderr line is the bare marker.
fn assert_refused_without_running_injection(r: &Ran, source: &str) {
    assert_ne!(r.code, 0, "{source:?} must refuse the payload as non-numeric, not run it");
    assert!(
        !stderr_has_exact_marker_line(&r.stderr, MARKER),
        "{source:?}: the injected command ran — stderr had a bare {MARKER:?} line:\n{}",
        r.stderr
    );
}

// ── Prove the assertion method itself: contains() lies, exact-line doesn't ──

/// Negative control: a refusal's diagnostic legitimately contains the raw
/// payload text, so `stderr.contains(MARKER)` is true here even though the
/// command never ran — this is exactly the false-positive `stderr_has_exact_marker_line`
/// exists to avoid.
#[tokio::test]
async fn refusal_diagnostic_contains_marker_as_a_substring_not_a_line() {
    let r = run(&format!(r#"num='{PAYLOAD}'; echo $((num))"#)).await;
    assert_ne!(r.code, 0, "the payload must be refused");
    assert!(r.stderr.contains(MARKER), "the diagnostic should quote the payload back: {}", r.stderr);
    assert!(
        !stderr_has_exact_marker_line(&r.stderr, MARKER),
        "no line should be the bare marker: {}",
        r.stderr
    );
}

/// Positive control: a script that genuinely runs the marker command must
/// be detected by `stderr_has_exact_marker_line` — proving the checker
/// isn't just always returning false.
#[tokio::test]
async fn exact_line_check_detects_a_real_run() {
    let r = run("echo $(( 1 + $(echo INJECTED >&2; echo 5) ))").await;
    assert_eq!(r.code, 0, "{:?}", r.stderr);
    assert_eq!(r.stdout, "6");
    assert!(
        stderr_has_exact_marker_line(&r.stderr, MARKER),
        "an explicit $(...) in the arithmetic SOURCE must run and its stderr line must be visible: {}",
        r.stderr
    );
}

// ── 1. The Holen payload through every numeric entry point ─────────────

#[tokio::test]
async fn payload_through_dollar_paren_paren() {
    let r = run(&format!(r#"num='{PAYLOAD}'; echo $((num))"#)).await;
    assert_refused_without_running_injection(&r, "$((num))");
}

#[tokio::test]
async fn payload_through_bare_double_paren() {
    let r = run(&format!("num='{PAYLOAD}'; (( num ))")).await;
    assert_refused_without_running_injection(&r, "(( num ))");
}

#[rstest]
#[case("-eq")]
#[case("-ne")]
#[case("-gt")]
#[case("-lt")]
#[case("-ge")]
#[case("-le")]
#[tokio::test]
async fn payload_through_double_bracket_numeric_comparators(#[case] op: &str) {
    let source = format!(r#"num='{PAYLOAD}'; [[ $num {op} 42 ]]"#);
    let r = run(&source).await;
    assert_refused_without_running_injection(&r, &source);
}

#[rstest]
#[case("-eq")]
#[case("-ne")]
#[case("-gt")]
#[case("-lt")]
#[case("-ge")]
#[case("-le")]
#[tokio::test]
async fn payload_through_test_builtin_numeric_comparators(#[case] op: &str) {
    let source = format!(r#"num='{PAYLOAD}'; test "$num" {op} 42"#);
    let r = run(&source).await;
    assert_refused_without_running_injection(&r, &source);
}

// ── 2. Recursive name chain: a value is never resolved as another name ──

/// `a` holds the literal text `b`; `$((a))` must read `b` as data (fails:
/// not a number) rather than looking up a variable named `b`, which in
/// turn holds `c+1` — itself never evaluated, so `c`'s value (41) is never
/// reached and the answer is never 42.
#[tokio::test]
async fn recursive_name_chain_does_not_resolve() {
    let r = run("a='b'; b='c+1'; c=41; echo $((a))").await;
    assert_ne!(r.code, 0, "`a` holds the literal text `b`, not a reference to chase");
    assert_ne!(r.stdout, "42", "must never resolve a -> b -> c+1 -> c=41 -> 42");
}

// ── 3. Dynamic index and a collection element — both stay data ─────────

/// `idx` is used as an array index inside `xs[idx]` — the brackets ARE
/// arithmetic source (by design: `xs[i]` reads `i` as an expression), but
/// `idx`'s runtime VALUE (the payload) must not itself be reparsed when
/// resolved as that expression's leaf.
#[tokio::test]
async fn dynamic_index_value_is_not_reparsed() {
    let r = run(&format!("xs=[10 20 30]; idx='{PAYLOAD}'; echo $((xs[idx]))")).await;
    assert_refused_without_running_injection(&r, "xs[idx]");
}

/// A collection element holding expression-shaped text: `xs[0]` resolves to
/// the payload string, which must be refused as data, not reparsed.
///
/// The element is single-quoted, not double: a double-quoted string
/// interpolates `$(...)` at PARSE time (kaish's normal, unrelated string
/// interpolation), which would run the marker command while building the
/// list — before arithmetic ever sees the value — and give a false
/// positive for the wrong reason. Single quotes keep the payload inert
/// text until `xs[0]` is coerced.
#[tokio::test]
async fn collection_element_holding_expression_text_is_not_reparsed() {
    let r = run(&format!("xs=['{PAYLOAD}']; echo $((xs[0]))")).await;
    assert_refused_without_running_injection(&r, "xs[0]");
}

// ── 4. Command output holding expression-shaped text stays data ─────────

/// The command that runs (`printf`) is harmless; its printed TEXT happens
/// to be expression-shaped (contains the payload). That output must be
/// read as one integer or refused — never reparsed and executed a second
/// time.
#[tokio::test]
async fn command_output_holding_expression_text_is_not_reparsed() {
    let r = run(&format!(r#"echo $(( $(printf '%s' '{PAYLOAD}') ))"#)).await;
    assert_refused_without_running_injection(&r, "$(printf ...)");
}

// ── 5. The counterexample: an explicit $(...) in SOURCE still runs ─────

/// Without this the suite would also pass on an implementation that broke
/// command substitution outright. `$(...)` written directly in the `$(( ))`
/// SOURCE is not a value flowing in from elsewhere — it is source the
/// author wrote, and kaish runs it by design.
#[tokio::test]
async fn explicit_command_substitution_in_source_still_runs() {
    let r = run("echo $(( 2 * $(echo 21) ))").await;
    assert_eq!(r.code, 0, "{:?}", r.stderr);
    assert_eq!(r.stdout, "42");
}
