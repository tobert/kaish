//! Diagnostics stay clinical: the fact, the data, and the fix — no rationale.
//!
//! Error strings are the one surface where the "keep the why" weight does not
//! apply: an agent reads a failure more often than any help topic and needs
//! what is wrong and what to write next, not the reasoning behind the rule
//! (Amy's 2026-08-18 feedback; the why lives in comments and docs). Each row
//! pins one trimmed message by asserting the rule or fix survived AND the
//! causal clause that was cut does not creep back in.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

/// The diagnostic text a failed statement produces, whether the refusal is a
/// runtime result or a pre-execution validation/parse error.
async fn err_of(source: &str) -> String {
    let kernel = Kernel::new(KernelConfig::isolated())
        .expect("failed to create kernel")
        .into_arc();
    match kernel.execute(source).await {
        Ok(r) => {
            assert!(!r.ok(), "{source:?} should fail");
            format!("{}{}", r.text_out(), r.err)
        }
        Err(e) => format!("{e:?}"),
    }
}

#[tokio::test]
async fn ascii_name_error_states_rule_without_rationale() {
    let text = err_of("a-b=1").await;
    assert!(
        text.contains("an ASCII name is letters, digits, and `_`"),
        "the rule must survive: {text:?}"
    );
    assert!(
        !text.contains("because") && !text.contains("fails to read back"),
        "the causal clause must stay out: {text:?}"
    );
}

#[tokio::test]
async fn dotted_name_error_names_the_fix_without_rationale() {
    // `unset` is a runtime door: it formats the `NameError` itself, so this
    // pins the `DottedName` Display branch. (The assignment spelling
    // `user.email=x` reaches the validator's E017 instead, which carries its
    // own message.)
    let text = err_of("unset user.email").await;
    assert!(
        text.contains("name[key]"),
        "the fix must survive: {text:?}"
    );
    assert!(
        !text.contains("kaish reads a dot"),
        "the causal clause must stay out: {text:?}"
    );
}

#[tokio::test]
async fn general_name_error_keeps_fact_and_fix_without_principle() {
    // U+FF01 (fullwidth `!`) is none of whitespace, invisible, or ambiguous
    // ASCII — it reaches the general NameError branch.
    let text = err_of("read 'a！b'").await;
    assert!(
        text.contains("not a letter, digit, or emoji") && text.contains("U+FF01"),
        "the character and codepoint must survive: {text:?}"
    );
    assert!(
        text.contains("quote the word"),
        "the fix must survive: {text:?}"
    );
    assert!(
        !text.contains("has to read as what it is"),
        "the abstract principle must stay out: {text:?}"
    );
}

#[tokio::test]
async fn hashed_target_error_states_rule_without_rationale() {
    let text = err_of("abc#3=5").await;
    assert!(
        text.contains("cannot contain `#`"),
        "the rule must survive: {text:?}"
    );
    assert!(
        !text.contains("is a word character"),
        "the causal clause must stay out: {text:?}"
    );
}

#[tokio::test]
async fn mid_word_hash_error_keeps_both_fixes_without_rationale() {
    let text = err_of("echo $x#3").await;
    assert!(
        text.contains("quote the whole word") && text.contains("space before `#`"),
        "both fixes must survive: {text:?}"
    );
    assert!(
        !text.contains("would drop the rest of the line"),
        "the causal clause must stay out: {text:?}"
    );
}

#[tokio::test]
async fn scatter_null_item_names_the_fix_without_rationale() {
    let kernel = Kernel::new(KernelConfig::isolated())
        .expect("failed to create kernel")
        .into_arc();
    let r = kernel
        .execute("echo '[1, null, 3]' | fromjson | scatter | echo $ITEM | gather")
        .await
        .expect("execute");
    assert!(!r.ok());
    let text = format!("{}{}", r.text_out(), r.err);
    assert!(
        text.contains("filter nulls out"),
        "the fix must survive: {text:?}"
    );
    assert!(
        !text.contains("refusing to bind"),
        "the refusal narration must stay out: {text:?}"
    );
}
