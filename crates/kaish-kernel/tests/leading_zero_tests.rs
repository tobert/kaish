//! A leading zero makes a word text; where kaish needs a number, it is an error.
//!
//! `007` is not a JSON number (RFC 8259 admits `0` or a nonzero leading digit),
//! and `fromjson '007'` has always refused it. The lexer now agrees, so a
//! leading-zero numeral is text everywhere a word is text — `chmod 0644`,
//! `echo 007` — and the positions that need a real number say so instead of
//! reinterpreting the digits.
//!
//! The failure this replaces was silent in both directions: `echo 007` printed
//! `7`, and `$((010 + 1))` answered 11 where bash answers 9.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

mod common;

async fn run(source: &str) -> (i64, String, String) {
    let k = Kernel::new(KernelConfig::isolated()).expect("kernel");
    let r = k.execute(source).await.expect("kernel execute");
    (r.code, r.text_out().trim().to_string(), r.err.clone())
}

/// Every diagnostic a failing statement produces, however it refused.
async fn err_of(source: &str) -> String {
    let k = Kernel::new(KernelConfig::isolated()).expect("kernel").into_arc();
    match k.execute(source).await {
        Ok(r) => {
            assert!(!r.ok(), "{source:?} should fail");
            format!("{}{}", r.text_out(), r.err)
        }
        Err(e) => format!("{e:?}"),
    }
}

// ── A word is text, and keeps the text that was typed ──────────────────────

#[tokio::test]
async fn a_leading_zero_word_keeps_its_digits() {
    for (source, expected) in
        [("echo 007", "007"), ("echo 0644", "0644"), ("echo 00", "00"), ("echo 007.5", "007.5")]
    {
        let (code, out, err) = run(source).await;
        assert_eq!(code, 0, "{source:?} must run: {err:?}");
        assert_eq!(out, expected, "{source:?} lost the text that was typed");
    }
}

/// The mode operand is the case that decides this: it is by far the most
/// common leading-zero word in real shell, and it must cost nothing.
#[tokio::test]
async fn a_mode_operand_survives_as_typed() {
    let (code, out, _) = run("echo 0755 0644 0000").await;
    assert_eq!(code, 0);
    assert_eq!(out, "0755 0644 0000");
}

#[tokio::test]
async fn a_leading_zero_word_is_a_string_not_a_number() {
    let (_, out, _) = run("echo $(typeof 007)").await;
    assert_eq!(out, "string", "007 must not type as a number");
    let (_, out, _) = run("echo $(typeof 7)").await;
    assert_eq!(out, "number", "an ordinary numeral is untouched");
}

// ── Where kaish needs a number, the leading zero is named ──────────────────

#[tokio::test]
async fn break_and_continue_name_the_leading_zero() {
    for (source, fix) in [
        ("for i in 1 2; do break 007; done", "write `break 7`"),
        ("for i in 1 2; do continue 010; done", "write `continue 10`"),
    ] {
        let text = err_of(source).await;
        assert!(text.contains("(leading zero)"), "must name the cause: {text:?}");
        assert!(text.contains(fix), "must name the fix: {text:?}");
    }
}

/// The validator that produces the message above runs only after the grammar
/// has already failed. A loop count that parses must never reach it.
#[tokio::test]
async fn an_ordinary_loop_count_still_parses() {
    let (code, out, err) = run("for i in 1 2 3; do break 2; done; echo done").await;
    assert_eq!(code, 0, "break 2 must still parse: {err:?}");
    assert_eq!(out, "done");
}

/// `-0` is a valid count and a valid JSON number, but its source text does not
/// round-trip, so it lexes as `NumericLiteral` rather than `Int`. The count
/// grammar matched only `Int`, which turned `break -0` into a parse error --
/// a regression this rule introduced and nothing else caught.
#[tokio::test]
async fn a_negative_zero_count_still_parses() {
    for source in [
        "for i in 1 2 3; do break -0; done; echo done",
        "for i in 1 2 3; do continue -0; done; echo done",
    ] {
        let (code, out, err) = run(source).await;
        assert_eq!(code, 0, "-0 is a number, not a leading zero: {source} {err:?}");
        assert_eq!(out, "done", "{source}");
    }
}

/// The message may reword a diagnosis and must never author one. `break` is
/// also a bareword an argument list accepts, and `echo break 007` fails ON the
/// numeral exactly like the statement does, so position in the token stream is
/// not enough to tell them apart.
#[tokio::test]
async fn the_count_message_never_speaks_for_an_argument() {
    for source in ["echo break 007", "echo continue 007"] {
        let text = err_of(source).await;
        assert!(
            !text.contains("takes a loop count"),
            "{source:?} is not a loop statement: {text:?}"
        );
    }
}

/// And it must not answer a real error elsewhere on the line with this one.
#[tokio::test]
async fn a_real_error_elsewhere_still_wins() {
    let text = err_of("if true; then echo hi; done
break 007").await;
    assert!(text.contains("found 'done'"), "the grammar's own error must stand: {text:?}");
    assert!(!text.contains("takes a loop count"), "must not mask it: {text:?}");
}

/// Every statement-start context the gate admits, so narrowing it cannot
/// quietly stop diagnosing the case it exists for.
#[tokio::test]
async fn the_count_message_reaches_every_statement_position() {
    for source in [
        "for i in 1 2; do break 007; done",
        "for i in 1 2; do echo a; break 007; done",
        "while true; do
break 007
done",
        "for i in 1 2; do if true; then break 007; fi; done",
        "for i in 1 2; do true && break 007; done",
        "for i in 1 2; do continue 007; done",
    ] {
        let text = err_of(source).await;
        assert!(text.contains("takes a loop count"), "{source:?} must be diagnosed: {text:?}");
    }
}

/// `break -022` is not fixed by writing `break 22`.
#[tokio::test]
async fn the_suggested_count_keeps_its_sign() {
    let text = err_of("for i in 1 2; do break -022; done").await;
    assert!(text.contains("write `break -22`"), "the sign must survive: {text:?}");
}

/// bash reads `010` as octal and answers 9; kaish reads no octal and would
/// answer 11. Answering a different number than the shell the author learned
/// is the outcome worth refusing.
#[tokio::test]
async fn arithmetic_refuses_a_leading_zero_rather_than_reading_decimal() {
    let text = err_of("echo $((010 + 1))").await;
    assert!(text.contains("(leading zero)"), "must name the cause: {text:?}");
    assert!(text.contains("no octal"), "must say kaish reads no octal: {text:?}");
    assert!(text.contains("write `10`"), "must name the fix: {text:?}");
    assert!(!text.contains("11"), "must not answer 11: {text:?}");
}

#[tokio::test]
async fn ordinary_arithmetic_is_untouched() {
    let (code, out, err) = run("echo $((10 + 1))").await;
    assert_eq!(code, 0, "{err:?}");
    assert_eq!(out, "11");
}

#[tokio::test]
async fn a_list_index_names_the_leading_zero() {
    let text = err_of("xs=[10 20 30]; echo ${xs[007]}").await;
    assert!(text.contains("(leading zero)"), "must name the cause: {text:?}");
    assert!(text.contains("${xs[7]}"), "must name the fix: {text:?}");
}

// ── Read and write agree on the same subscript ─────────────────────────────

/// `${r[007]}` read fine while `r[007]=v` was a parse error, and the read
/// resolved to index 7 — so a record whose key really is "007" could not be
/// reached by the name it was stored under.
#[tokio::test]
async fn a_record_key_that_is_a_leading_zero_numeral_round_trips() {
    let (code, out, err) = run(r#"r={"007":9}; echo ${r[007]}"#).await;
    assert_eq!(code, 0, "reading a 007 key must work: {err:?}");
    assert_eq!(out, "9");

    let (code, out, err) = run("r={}; r[007]=nine; echo ${r[007]}").await;
    assert_eq!(code, 0, "writing a 007 key must work: {err:?}");
    assert_eq!(out, "nine", "the write and the read must name the same key");
}

/// A slice carries two number positions, and the leading zero was silent in
/// both: `${xs[007:2]}` sliced from 7, inverted the range, and returned an
/// empty list. An empty result is the one wrong answer a caller cannot tell
/// from a correct one.
#[tokio::test]
async fn a_slice_bound_refuses_a_leading_zero() {
    for (source, fix) in [
        ("xs=[1 2 3]; echo ${xs[007:2]}", "${xs[7:2]}"),
        ("xs=[1 2 3]; echo ${xs[0:007]}", "${xs[0:7]}"),
    ] {
        let text = err_of(source).await;
        assert!(text.contains("(leading zero)"), "must name the cause: {text:?}");
        assert!(text.contains(fix), "must name the fix: {text:?}");
    }
}

/// The slice grammar is easy to break while refusing one spelling of it.
#[tokio::test]
async fn every_ordinary_slice_spelling_still_works() {
    for (source, expected) in [
        ("xs=[1 2 3]; echo ${xs[0:2]}", "[1,2]"),
        ("xs=[1 2 3]; echo ${xs[:2]}", "[1,2]"),
        ("xs=[1 2 3]; echo ${xs[1:]}", "[2,3]"),
        ("xs=[1 2 3]; echo ${xs[-2:]}", "[2,3]"),
    ] {
        let (code, out, err) = run(source).await;
        assert_eq!(code, 0, "{source:?} must run: {err:?}");
        assert_eq!(out, expected, "{source:?}");
    }
}

/// A variable holding `010` is a number position when it reaches arithmetic,
/// and parsing it decimal answers 10 where bash answers 8. The literal case
/// was already refused; this is the same numeral arriving by another road.
#[tokio::test]
async fn arithmetic_refuses_a_leading_zero_that_arrives_in_a_variable() {
    for (source, decimal) in
        [("x=010; echo $((x))", "write `10`"), ("x=007; echo $((x + 1))", "write `7`")]
    {
        let text = err_of(source).await;
        assert!(text.contains("(leading zero)"), "must name the cause: {text:?}");
        assert!(text.contains(decimal), "must name the fix: {text:?}");
    }
    let (code, out, err) = run("x=10; echo $((x + 1))").await;
    assert_eq!(code, 0, "an ordinary variable must still work: {err:?}");
    assert_eq!(out, "11");
}

/// `${r[-0]}` read as index 0 while `r[-0]=v` was a parse error, and `${r[1.0]}`
/// read as a key while the write refused. Neither is a leading-zero numeral —
/// they are numerals whose source text does not round-trip — but they reach
/// the subscript through the same token, so the two sides must classify alike.
#[tokio::test]
async fn read_and_write_classify_every_numeral_subscript_alike() {
    let read = err_of("r={}; echo ${r[-0]}").await;
    let write = err_of("r={}; r[-0]=v").await;
    assert!(read.contains("integer index on a record"), "read: {read:?}");
    assert!(write.contains("integer index on a record"), "write must agree: {write:?}");

    let (code, out, err) = run("r={}; r[1.0]=v; echo ${r[1.0]}").await;
    assert_eq!(code, 0, "a 1.0 key must round-trip: {err:?}");
    assert_eq!(out, "v");
}

#[tokio::test]
async fn an_ordinary_index_is_untouched() {
    let (code, out, err) = run("xs=[10 20 30]; echo ${xs[1]}").await;
    assert_eq!(code, 0, "{err:?}");
    assert_eq!(out, "20");
}

// ── The plan document and the run agree ────────────────────────────────────

/// `--plan` reported the redirect target as `-0` while execution created a
/// file named `0`. A plan that describes a write that never happens is the
/// one failure a plan consumer cannot detect for itself.
#[cfg(feature = "localfs")]
#[tokio::test]
async fn a_redirect_target_writes_the_file_the_plan_names() {
    for target in ["-0", "007", "0.10", "1.0"] {
        let dir = tempfile::tempdir().expect("tempdir");
        let kernel = common::kernel_at(dir.path());

        let planned = kernel
            .plan_program(&format!("echo hi > {target}"))
            .expect("plan")
            .first()
            .map(|s| s.plan.rendered.clone())
            .expect("one statement");
        assert!(
            planned.ends_with(target),
            "the plan must name {target:?} as typed, got {planned:?}"
        );

        let (_, code) = common::run(&kernel, &format!("echo hi > {target}")).await;
        assert!(
            dir.path().join(target).exists(),
            "the plan promised {target:?} (exit {code}); the run created {:?}",
            std::fs::read_dir(dir.path())
                .expect("read_dir")
                .filter_map(|e| e.ok().map(|e| e.file_name()))
                .collect::<Vec<_>>()
        );
    }
}

// ── A comparison operand is a number position too ──────────────────────────

/// `[[ 010 -eq 10 ]]` was true: the string parsed as decimal 10, the number
/// arithmetic refuses to answer. bash answers 8 here (octal), so this is the
/// same three-answers case as `$((010))`, and it takes the same refusal.
#[tokio::test]
async fn numeric_comparison_refuses_a_leading_zero_rather_than_reading_decimal() {
    for source in [
        "[[ 010 -eq 10 ]]",
        "test 010 -eq 10",
        "[[ 10 -lt 0100 ]]",
        "x=010; [[ $x -eq 10 ]]",
        "x=-007; test $x -eq -7",
        r#"[[ "01" -eq "1" ]]"#,
        r#"X="01"; [[ "$X" -eq 1 ]]"#,
    ] {
        let text = err_of(source).await;
        assert!(text.contains("(leading zero)"), "{source:?} must name the cause: {text:?}");
        assert!(text.contains("no octal"), "{source:?} must say kaish reads no octal: {text:?}");
        assert!(
            text.contains("write `10`")
                || text.contains("write `100`")
                || text.contains("write `-7`")
                || text.contains("write `1`"),
            "{source:?} must name the fix: {text:?}"
        );
    }
}

#[tokio::test]
async fn ordinary_numeric_comparison_is_untouched() {
    for source in [
        "[[ 10 -eq 10 ]]",
        "[[ 0 -eq 0 ]]",
        "[[ -0 -eq 0 ]]",
        "[[ 0.5 -gt 0 ]]",
        "[[ 0.10 -lt 1 ]]",
        "test 100 -gt 10",
        "x=$(fromjson 10); [[ $x -eq 10 ]]",
    ] {
        let (code, _, err) = run(source).await;
        assert_eq!(code, 0, "{source:?} must be true: {err:?}");
    }
}

// ── A numeral kaish cannot hold names the limit and the fix ────────────────

/// One past `i64::MAX` was "invalid number" with nothing to do about it. The
/// numeral is a valid JSON number, so the error names the limit kaish adds and
/// the quoting that keeps the text.
#[tokio::test]
async fn an_integer_past_64_bits_names_the_limit_and_the_fix() {
    for source in ["echo 9223372036854775808", "echo -9223372036854775809", "x=18446744073709551616"] {
        let text = err_of(source).await;
        assert!(text.contains("64-bit"), "{source:?} must name the limit: {text:?}");
        assert!(text.contains("quote"), "{source:?} must name the fix: {text:?}");
        assert!(!text.contains("invalid number"), "{source:?} must not say only 'invalid': {text:?}");
    }
    let (code, out, _) = run("echo \"18446744073709551616\"").await;
    assert_eq!((code, out.as_str()), (0, "18446744073709551616"), "the quoted form is the fix");
    let (code, out, _) = run("echo 9223372036854775807 -9223372036854775808").await;
    assert_eq!((code, out.as_str()), (0, "9223372036854775807 -9223372036854775808"));
}

/// A leading zero makes a word text before overflow ever gets to matter —
/// `09223372036854775808` is one past `i64::MAX`, but it is text (leading
/// zero) first, same as `007`, not a 64-bit refusal.
#[tokio::test]
async fn a_leading_zero_numeral_past_64_bits_is_still_text() {
    let (code, out, err) = run("echo 09223372036854775808").await;
    assert_eq!(code, 0, "must run: {err:?}");
    assert_eq!(out, "09223372036854775808", "leading zero wins over overflow");
    let (_, out, _) = run("echo $(typeof 09223372036854775808)").await;
    assert_eq!(out, "string", "09223372036854775808 must not type as a number");

    // The un-zeroed overflow still refuses.
    let text = err_of("echo 9223372036854775808").await;
    assert!(text.contains("64-bit"), "must still name the 64-bit limit: {text:?}");
}

/// `$(( … ))` parses its own numerals separately from the lexer, and used to
/// say only "invalid number" for an overflowing literal. It now names the
/// same 64-bit limit the lexer does.
#[tokio::test]
async fn arithmetic_overflow_literal_names_the_64_bit_limit() {
    let text = err_of("echo $((9223372036854775808))").await;
    assert!(text.contains("64-bit"), "must name the limit: {text:?}");
    assert!(!text.contains("invalid number"), "must not say only 'invalid': {text:?}");

    // Overflow from addition is a different failure and keeps its own wording.
    let text = err_of("echo $((9223372036854775807 + 1))").await;
    assert!(text.contains("overflow"), "addition overflow must still say overflow: {text:?}");
}
