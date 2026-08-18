//! An ASCII name is letters, digits, and `_` — nothing that cannot be read back.
//!
//! kaish once admitted `-`, `@`, and `:` in a name because the `Ident` token
//! admits them for words. None of the three survives a round trip: `$a-b`
//! reads `$a` and then the literal `-b`, `$a@b` is an adjacent-words error, and
//! `a:b` has no read spelling at all. A name that binds through one spelling
//! and cannot be read through another is the silent write the name rule exists
//! to remove, so the ambiguous characters are refused at every door.
//!
//! Words are untouched: `echo a-b`, `ls -l`, and `my-file.txt` are data.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated()).expect("kernel")
}

/// The text of a refusal, however it arrived — pre-execution as an `Err`, or
/// from a builtin as a nonzero result.
async fn refusal(source: &str) -> Option<String> {
    let k = kernel();
    match k.execute(source).await {
        Err(e) => Some(format!("{e:#}")),
        Ok(r) if r.code != 0 => Some(format!("{}{}", r.text_out(), r.err)),
        Ok(_) => None,
    }
}

async fn run(source: &str) -> (i64, String) {
    let k = kernel();
    let r = k.execute(source).await.expect("kernel execute");
    (r.code, r.text_out().trim().to_string())
}

/// Every door a name can enter through, so one rule can be checked at all of
/// them. `read` is here because it is the door that prompted the rule.
fn every_name_door(name: &str) -> Vec<(&'static str, String)> {
    vec![
        ("assign", format!("{name}=1")),
        ("env prefix", format!("x=1 {name}=2 echo hi")),
        ("for", format!("for {name} in 1; do echo hi; done")),
        ("export", format!("export {name}=1")),
        ("read", format!("read {name}")),
        ("unset", format!("unset {name}")),
        ("push", format!("push {name} 1")),
    ]
}

/// The three ambiguous characters are refused at every door.
#[tokio::test]
async fn ambiguous_ascii_is_refused_at_every_door() {
    for name in ["a-b", "a@b", "a:b"] {
        for (door, source) in every_name_door(name) {
            assert!(
                refusal(&source).await.is_some(),
                "the `{door}` door accepted the ambiguous name {name:?}: {source:?}"
            );
        }
    }
}

/// A legal name still works at every one of those doors, so the refusals above
/// are the rule doing its job rather than the doors being broken.
#[tokio::test]
async fn a_legal_name_still_works_at_every_door() {
    for source in [
        "a_b=1; echo $a_b",
        "x=1 a_b=2 echo hi",
        "for a_b in 1; do echo $a_b; done",
        "export a_b=1",
        "x=1; unset x",
        "xs=[]; push xs 1",
        "café=1; echo $café",
        "a1=1; echo $a1",
    ] {
        let (code, out) = run(source).await;
        assert_eq!(code, 0, "a legal name was refused: {source:?} -> {out:?}");
    }
}

/// Words keep every character a name may not hold. This is the test that would
/// catch a fix applied to the lexer instead of to the name rule.
#[tokio::test]
async fn words_are_not_names() {
    for (source, expected) in [
        ("echo a-b", "a-b"),
        ("echo a@b", "a@b"),
        ("echo a:b", "a:b"),
        ("echo my-file.txt", "my-file.txt"),
        ("echo user@host", "user@host"),
        ("echo key=a-b", "key=a-b"),
        ("echo -- --flag-like", "--flag-like"),
    ] {
        let (code, out) = run(source).await;
        assert_eq!(code, 0, "a word was refused: {source:?} -> {out:?}");
        assert!(out.contains(expected), "{source:?} printed {out:?}, expected {expected:?}");
    }
}

/// The refusal says which character it refused, so the author can see it.
#[tokio::test]
async fn the_refusal_names_the_character() {
    for (name, ch) in [("a-b", "-"), ("a@b", "@"), ("a:b", ":")] {
        let text = refusal(&format!("{name}=1"))
            .await
            .unwrap_or_else(|| panic!("{name} was accepted"));
        assert!(
            text.contains(ch),
            "the refusal for {name:?} did not name {ch:?}: {text}"
        );
    }
}

/// A subscript is data, and keeps what a name may not hold.
#[tokio::test]
async fn a_subscript_is_not_a_name() {
    for source in [
        r#"r={}; r[a-b]=1; echo "${r[a-b]}""#,
        r#"r={}; r["a:b"]=1; echo ok"#,
    ] {
        let (code, out) = run(source).await;
        assert_eq!(code, 0, "a subscript was refused: {source:?} -> {out:?}");
    }
}

/// A special parameter is one ASCII punctuation character, and the class must
/// not refuse it. `${$}` was the case that caught this: the ASCII rule read
/// `$` as punctuation in a name and refused the session identifier.
///
/// The exemption is ASCII-only on purpose — a lone zero-width character IS
/// lexable as a name start, and is still refused. That row is the reason the
/// exemption is not simply "any one-character name".
#[tokio::test]
async fn a_special_parameter_is_not_refused() {
    // `${#}` is left out deliberately: it fails on main too (the braced form
    // of `$#` is unsupported, unrelated to the name class), so asserting it
    // here would pin someone else's bug to this rule.
    for source in ["echo ${$}", "echo $$", "echo $?", "echo $#"] {
        let (code, out) = run(source).await;
        assert_eq!(code, 0, "a special parameter was refused: {source:?} -> {out:?}");
    }
    // The narrow-exemption guard: still refused, still by codepoint.
    let text = refusal("\u{200b}=1").await.expect("a lone zero-width name was accepted");
    assert!(text.contains("U+200B"), "refused without naming the character: {text}");
}
