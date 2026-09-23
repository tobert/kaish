//! Kernel-routed tests for `sed`'s and `awk`'s regex dialects.
//!
//! `sed`'s default (no `-E`/`-r`) mode reads GNU BRE, where `\|`/`\(…\)`/`\{N\}`
//! are operators and the bare forms are literal (issue #60's headline case,
//! `fn consult(`); `-E`/`-r` opts into strict ERE, where that's reversed. `awk`
//! has no BRE — it reads gawk's ERE, where the bare forms are already the
//! operators and the escapes above are already literal; see
//! `tests/sed_gnu_regex_tests.rs` and `tests/awk_gnu_regex_tests.rs` for the
//! GNU-recorded regex tables. This file is the older, narrower regression
//! suite from issue #60 and its follow-up; kept and converted to the GNU
//! values rather than deleted. Driven through `kernel.execute()` so flag
//! binding and dispatch run the real path.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use tempfile::tempdir;

use common::{kernel_at, run};

#[tokio::test]
async fn sed_bre_alternation_default() {
    let kernel = kernel_at(tempdir().unwrap().path());
    let (out, code) = run(&kernel, r#"printf 'cat\ndog\nfish\n' | sed 's/cat\|dog/X/'"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "X\nX\nfish", "alternation replaces cat and dog: {out:?}");
}

#[tokio::test]
async fn sed_bre_capture_groups_default() {
    let kernel = kernel_at(tempdir().unwrap().path());
    let (out, code) = run(&kernel, r#"echo ab | sed 's/\(a\)\(b\)/\2\1/'"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out.trim(), "ba", "BRE groups swap via \\2\\1: {out:?}");
}

#[tokio::test]
async fn sed_extended_flag_makes_pipe_literal() {
    let kernel = kernel_at(tempdir().unwrap().path());
    // `-E` strict ERE: `\|` is a literal pipe, so only the "cat|dog" line matches.
    let (out, code) =
        run(&kernel, r#"printf 'cat|dog\ncat\n' | sed -E 's/cat\|dog/X/'"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "X\ncat", "-E treats \\| as literal pipe: {out:?}");
}

/// awk has no BRE: `\|` is a literal pipe in a gawk ERE (an escaped meta is
/// always literal there), so `/cat\|dog/` matches the 8-character text
/// `cat|dog`, not "cat" or "dog" — confirmed against `gawk 5.4.1`. This
/// inverts the old (buggy) hybrid, which treated `\|` as alternation here
/// the same way `sed` genuinely does.
#[tokio::test]
async fn awk_backslash_pipe_is_literal_in_pattern() {
    let kernel = kernel_at(tempdir().unwrap().path());
    let (out, code) =
        run(&kernel, r#"printf 'cat\ndog\nfish\n' | awk '/cat\|dog/ {print}'"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "", "\\| is a literal pipe, so neither line has the text cat|dog: {out:?}");
}

#[tokio::test]
async fn awk_backslash_pipe_is_literal_in_match_operator() {
    let kernel = kernel_at(tempdir().unwrap().path());
    let (out, code) =
        run(&kernel, r#"echo hello | awk '{ if ($0 ~ /he\|xy/) print "match" }'"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out.trim(), "", "\"hello\" has no literal he|xy: {out:?}");
}

#[tokio::test]
async fn awk_backslash_pipe_is_literal_in_gsub() {
    let kernel = kernel_at(tempdir().unwrap().path());
    let (out, code) =
        run(&kernel, r#"echo 'cat dog fish' | awk '{gsub(/cat\|dog/, "X"); print}'"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out.trim(), "cat dog fish", "no literal cat|dog to replace: {out:?}");
}

/// The bare form (real gawk alternation) is unaffected by the fix above.
#[tokio::test]
async fn awk_bare_pipe_alternates() {
    let kernel = kernel_at(tempdir().unwrap().path());
    let (out, code) =
        run(&kernel, r#"printf 'cat\ndog\nfish\n' | awk '/cat|dog/ {print}'"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "cat\ndog", "bare | alternates in gawk ERE: {out:?}");
}

#[tokio::test]
async fn sed_dash_r_alias_makes_pipe_literal() {
    let kernel = kernel_at(tempdir().unwrap().path());
    // `-r` is the GNU alias for `-E`; it binds through the short_alias path in
    // the kernel, so pin it separately from `-E`.
    let (out, code) =
        run(&kernel, r#"printf 'cat|dog\ncat\n' | sed -r 's/cat\|dog/X/'"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "X\ncat", "-r treats \\| as literal pipe: {out:?}");
}

/// A genuinely unmatched `\)` (default GNU BRE mode: no `\(` opened it)
/// fails loudly, with a dialect hint naming sed's strict-ERE flags.
#[tokio::test]
async fn sed_unmatched_group_close_carries_dialect_hint() {
    let kernel = kernel_at(tempdir().unwrap().path());

    let err = kernel
        .execute(r#"echo x | sed 's/a\)/X/'"#)
        .await
        .expect_err("unmatched group should fail sed validation");
    let msg = err.to_string();
    assert!(msg.contains("GNU BRE"), "sed should explain the dialect: {msg}");
    assert!(msg.contains("-E/-r"), "sed should offer its strict-ERE flags: {msg}");
}

/// A genuinely unmatched open paren fails loudly in awk too — gawk and
/// kaish's engine agree bare `(` always starts a group. Unlike the escaped
/// form above, awk carries no dialect hint (there's no rewrite to explain:
/// awk's ERE already matches the engine's, see `gawk_ere_to_regex`).
#[tokio::test]
async fn awk_unmatched_group_open_fails() {
    let kernel = kernel_at(tempdir().unwrap().path());

    let result = kernel
        .execute(r#"echo x | awk '/a(/ {print}'"#)
        .await
        .expect("awk regex failure is a runtime error, not a validation error");
    assert_ne!(result.code, 0, "unmatched ( should fail: {result:?}");
}

/// A literal paren — the audit's headline case for `grep`/`sed` — is simply
/// valid in an escaped-form awk pattern; there's no rewrite left to fail on.
#[tokio::test]
async fn awk_escaped_paren_is_a_literal_match_not_a_compile_error() {
    let kernel = kernel_at(tempdir().unwrap().path());

    let (out, code) = run(&kernel, r#"echo 'fn consult(q)' | awk '/fn consult\(/ {print}'"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out.trim(), "fn consult(q)", "\\( is a literal paren in gawk ERE: {out:?}");
}

// === FS / split() separators: `\|` means a LITERAL pipe, like gawk ===
//
// gawk demotes `\|` in a dynamic-regex string to plain `|` (with a warning) and
// then applies the POSIX single-char-FS-is-literal rule — NF=3 on `a|b|c`. The
// naive rewrite order (single-char check on the raw separator, then BRE rewrite
// in the regex branch) yields the empty-alternation regex `|`, which silently
// splits between every character (NF=7). Regression tests for issue #60's
// follow-up finding.

#[tokio::test]
async fn awk_capital_f_backslash_pipe_splits_on_literal_pipe() {
    let kernel = kernel_at(tempdir().unwrap().path());
    let (out, code) = run(&kernel, r#"echo 'a|b|c' | awk -F '\|' '{print NF, $2}'"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out.trim(), "3 b", "-F '\\|' is a literal pipe, like gawk: {out:?}");
}

#[tokio::test]
async fn awk_fs_assignment_backslash_pipe_splits_on_literal_pipe() {
    let kernel = kernel_at(tempdir().unwrap().path());
    let (out, code) =
        run(&kernel, r#"echo 'a|b|c' | awk 'BEGIN{FS="\\|"} {print NF, $2}'"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out.trim(), "3 b", "FS=\"\\\\|\" is a literal pipe, like gawk: {out:?}");
}

#[tokio::test]
async fn awk_split_backslash_pipe_splits_on_literal_pipe() {
    let kernel = kernel_at(tempdir().unwrap().path());
    let (out, code) = run(
        &kernel,
        r#"echo x | awk '{n = split("a|b|c", parts, "\\|"); print n, parts[2]}'"#,
    )
    .await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out.trim(), "3 b", "split() on \"\\\\|\" is a literal pipe: {out:?}");
}

#[tokio::test]
async fn awk_multichar_fs_still_gets_bre_rewrite() {
    let kernel = kernel_at(tempdir().unwrap().path());
    // A separator that stays multi-char after the rewrite is still an ERE:
    // `--\|;;` → `--|;;` alternates between `--` and `;;`.
    let (out, code) =
        run(&kernel, r#"echo 'a--b;;c' | awk -F '--\|;;' '{print NF, $3}'"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out.trim(), "3 c", "multi-char FS alternates on \\|: {out:?}");
}

/// An FS/split() separator that's invalid AFTER the rewrite errors naming the
/// separator as the user wrote it (not the rewritten form the engine saw), and
/// carries the dialect hint (without `-E` — awk has none). PR #65 follow-ups.
#[tokio::test]
async fn awk_invalid_separator_names_raw_form_loudly() {
    let kernel = kernel_at(tempdir().unwrap().path());

    // FS path: a bare, unescaped `(` is a real ERE group with nothing to
    // close it — a loud runtime error, not a silent literal-split fallback
    // that would miscount fields. (`xx\(` — the escaped form — is now a
    // valid separator: a literal `(`, matching gawk's `-F '\('`.)
    let result = kernel
        .execute(r#"echo x | awk -F 'xx(' '{print NF}'"#)
        .await
        .expect("invalid FS is a runtime error, not a validation error");
    assert_ne!(result.code, 0, "invalid FS should fail: {result:?}");
    assert!(result.err.contains("xx("), "names the separator: {}", result.err);

    // split() path: same contract.
    let result = kernel
        .execute(r#"echo x | awk '{n = split("abc", a, "yy("); print n}'"#)
        .await
        .expect("invalid split() separator is a runtime error");
    assert_ne!(result.code, 0, "invalid separator should fail: {result:?}");
    assert!(result.err.contains("yy("), "names the separator: {}", result.err);
}

/// The escaped form of the separator above (`\(`) is now a literal `(`,
/// matching gawk exactly — `-F '\('` is no longer the FS/split() failure
/// case; a bare `(` is (see `awk_invalid_separator_names_raw_form_loudly`).
/// `-F`'s CLI-value unescaping (`awk_unescape_cli_value`) strips the
/// backslash before FS is even compiled, so `-F '\('` becomes the
/// single-character literal `(`, exactly as `gawk -F '\(' ...` warns
/// (`escape sequence \`(' treated as plain \`('`) and then splits on.
#[tokio::test]
async fn awk_escaped_paren_separator_is_a_literal_field_separator() {
    let kernel = kernel_at(tempdir().unwrap().path());

    let (out, code) = run(&kernel, r#"echo 'a(b(c' | awk -F '\(' '{print NF, $2}'"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out.trim(), "3 b", "\\( splits on the literal paren, like gawk: {out:?}");

    // A program string literal doesn't get the CLI unescape: `"\\("` is the
    // recognized `\\` escape (one backslash) followed by a bare `(`, so the
    // runtime string stays two characters and goes through the regex path
    // instead — `\(` there is still a literal paren (an escaped meta,
    // literal in gawk ERE), so the split result is the same.
    let (out, code) = run(
        &kernel,
        r#"echo x | awk '{n = split("a(b(c", arr, "\\("); print n, arr[2]}'"#,
    )
    .await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out.trim(), "3 b", "split() on \"\\\\(\" is a literal paren: {out:?}");
}
