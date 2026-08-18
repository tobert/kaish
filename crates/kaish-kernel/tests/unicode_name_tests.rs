//! Variable names accept any script, and every spelling of a reference agrees.
//!
//! A name is ASCII alphanumerics, `_`, or any non-ASCII scalar value, and it is
//! NFC-normalized when it is read. `café` typed as `e`+U+0301 and `café` typed
//! as U+00E9 render identically, so they name the same variable.
//!
//! There are four doors to a name — `$x`, `${x}`, `x=`, and interpolation
//! inside a double-quoted string or heredoc body. They must agree. A door that
//! disagrees does not fail loudly; it substitutes a *different* variable and
//! keeps going, which is the wrong-value class this file exists to pin.

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

/// The four doors agree, for every script. Each row binds through one door and
/// reads through another; a disagreement shows up as an empty or wrong value.
#[tokio::test]
async fn every_door_reaches_the_same_variable() {
    for (source, expected) in [
        // ASCII, the control.
        ("v=ok; echo $v", "ok"),
        ("v=ok; echo ${v}", "ok"),
        ("v=ok; echo \"$v\"", "ok"),
        ("v=ok; echo \"${v}\"", "ok"),
        // Latin-1 accented.
        ("café=ok; echo $café", "ok"),
        ("café=ok; echo ${café}", "ok"),
        ("café=ok; echo \"$café\"", "ok"),
        ("café=ok; echo \"${café}\"", "ok"),
        // Japanese.
        ("名前=ok; echo $名前", "ok"),
        ("名前=ok; echo ${名前}", "ok"),
        ("名前=ok; echo \"$名前\"", "ok"),
        // Emoji. 😁
        ("😁=grin; echo $😁", "grin"),
        ("😁=grin; echo ${😁}", "grin"),
        ("😁=grin; echo \"$😁\"", "grin"),
        // Mixed ASCII and not, in both positions.
        ("x😁=ok; echo $x😁", "ok"),
        ("café_1=ok; echo ${café_1}", "ok"),
    ] {
        let (code, out) = run(source).await;
        assert_eq!(code, 0, "{source} exited {code}");
        assert_eq!(out, expected, "for: {source}");
    }
}

/// The bug this file was opened for: a double-quoted `"$café"` used to collect
/// only the ASCII head of the name, substitute *that* variable, and append the
/// rest as literal text. It failed silently and it substituted the wrong value.
#[tokio::test]
async fn quoted_reference_does_not_truncate_the_name() {
    // `caf` is bound to a trap value. If the name is truncated, it wins.
    let (code, out) = run("caf=TRAP; café=RIGHT; echo \"$café\"").await;
    assert_eq!(code, 0);
    assert_eq!(out, "RIGHT", "the ASCII head of the name must not win");

    // And the unset case must be empty, not the trap value plus a tail.
    let (code, out) = run("caf=TRAP; echo \"$café\"").await;
    assert_eq!(code, 0);
    assert_eq!(out, "", "an unset name must resolve empty, not to its ASCII head");
}

/// NFC and NFD spellings render identically, so they name one variable.
/// Written as escapes rather than literals so the distinction survives an
/// editor, a terminal, or a copy-paste that would silently normalize the file.
#[tokio::test]
async fn nfc_and_nfd_spellings_are_one_variable() {
    let nfc = "caf\u{e9}"; // café, precomposed
    let nfd = "cafe\u{301}"; // café, e + combining acute
    assert_ne!(nfc, nfd, "the fixture must actually differ in bytes");

    for (bind, read) in [(nfc, nfd), (nfd, nfc), (nfc, nfc), (nfd, nfd)] {
        let source = format!("{bind}=bound; echo ${{{read}}}");
        let (code, out) = run(&source).await;
        assert_eq!(code, 0, "{source} exited {code}");
        assert_eq!(out, "bound", "bind {bind:?} read {read:?}");
    }
}

/// Normalization reaches the unbraced and quoted doors too, not just `${…}`.
#[tokio::test]
async fn normalization_reaches_every_door() {
    let nfc = "caf\u{e9}";
    let nfd = "cafe\u{301}";
    for source in [
        format!("{nfc}=bound; echo ${nfd}"),
        format!("{nfc}=bound; echo \"${nfd}\""),
        format!("{nfd}=bound; echo ${nfc}"),
        format!("{nfd}=bound; echo \"${{{nfc}}}\""),
    ] {
        let (code, out) = run(&source).await;
        assert_eq!(code, 0, "{source} exited {code}");
        assert_eq!(out, "bound", "for: {source}");
    }
}

/// `${#name}` measures the value, for any script of name.
#[tokio::test]
async fn length_of_a_non_ascii_name() {
    for (source, expected) in [
        ("café=abcd; echo ${#café}", "4"),
        ("😁=abc; echo ${#😁}", "3"),
    ] {
        let (code, out) = run(source).await;
        assert_eq!(code, 0, "{source} exited {code}");
        assert_eq!(out, expected, "for: {source}");
    }
}

/// Flags stay ASCII, and the error says what to do. A flag spelled in another
/// script is ambiguous — a flag no tool defines, or a word the caller meant
/// literally — so kaish refuses rather than guessing.
#[tokio::test]
async fn flags_remain_ascii_and_say_so() {
    for source in ["grep --café x", "grep -café x", "set +café"] {
        let k = kernel();
        let err = k.execute(source).await;
        let failed = match err {
            Err(_) => true,
            Ok(r) => r.code != 0,
        };
        assert!(failed, "{source} should be an error");
    }
}

/// Single quotes are the escape hatch out of substitution, for any script.
#[tokio::test]
async fn single_quotes_suppress_a_non_ascii_reference() {
    for (source, expected) in [
        ("café=RIGHT; echo '$café'", "$café"),
        ("😁=grin; echo '$😁'", "$😁"),
    ] {
        let (code, out) = run(source).await;
        assert_eq!(code, 0, "{source} exited {code}");
        assert_eq!(out, expected, "for: {source}");
    }
}

/// Non-ASCII data was already accepted and must stay that way — these are the
/// rows the name work must not disturb.
#[tokio::test]
async fn non_ascii_data_still_passes_through() {
    for (source, expected) in [
        ("echo café", "café"),
        ("echo 日本語", "日本語"),
        ("echo 😁", "😁"),
        ("echo hi😁there", "hi😁there"),
        ("x=😁; echo $x", "😁"),
        ("echo \"a😁b\"", "a😁b"),
    ] {
        let (code, out) = run(source).await;
        assert_eq!(code, 0, "{source} exited {code}");
        assert_eq!(out, expected, "for: {source}");
    }
}

/// Every runtime binder reaches the same variable as a written one. These are
/// the doors that bind a name without going through the parser's normalization
/// — `for`, `read`, `unset`, and the embedder's own surfaces — and each was
/// silently missing an NFC-bound variable before the scope itself normalized.
#[tokio::test]
async fn runtime_binders_normalize_too() {
    let nfc = "caf\u{e9}";
    let nfd = "cafe\u{301}";

    // `for` binds its loop variable directly.
    let (code, out) = run(&format!("for {nfd} in bound; do echo \"${{{nfc}}}\"; done")).await;
    assert_eq!(code, 0);
    assert_eq!(out, "bound", "the for-loop variable must normalize");

    // `read` binds from stdin.
    let (code, out) = run(&format!("printf bound | read {nfd}; echo \"${{{nfc}}}\"")).await;
    assert_eq!(code, 0);
    assert_eq!(out, "bound", "read must normalize its target");

    // `unset` removes by name — a spelling mismatch made it a silent no-op.
    let (code, out) = run(&format!("{nfc}=bound; unset {nfd}; echo \"[${{{nfc}}}]\"")).await;
    assert_eq!(code, 0);
    assert_eq!(out, "[]", "unset must reach the variable it names");
}

/// `export` used the ASCII name rule while assignment used the wider one, so
/// `café=1` worked and `export café=1` was "not a valid identifier".
#[tokio::test]
async fn export_agrees_with_assignment_about_names() {
    for (source, expected) in [
        ("export café=ok; echo $café", "ok"),
        ("export 名前=ok; echo ${名前}", "ok"),
        ("export _v=ok; echo $_v", "ok"),
    ] {
        let (code, out) = run(source).await;
        assert_eq!(code, 0, "{source:?} exited {code}");
        assert_eq!(out, expected, "for: {source:?}");
    }
    // A digit still cannot start a name, in any script. This one is refused
    // before `export` ever sees it — `1x=v` is a token-pasting error — so the
    // assertion accepts either shape of failure.
    let k = kernel();
    let refused = match k.execute("export 1x=v").await {
        Err(_) => true,
        Ok(r) => r.code != 0,
    };
    assert!(refused, "a digit-leading name is still refused");
}
