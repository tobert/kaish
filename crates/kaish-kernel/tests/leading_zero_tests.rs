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
