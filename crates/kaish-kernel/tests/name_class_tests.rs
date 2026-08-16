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
