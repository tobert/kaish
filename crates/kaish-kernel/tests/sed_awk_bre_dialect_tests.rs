//! Kernel-routed tests for the GNU BRE backslash-meta superset in `sed` and
//! `awk` (issue #60). Default mode rewrites `\|`/`\(…\)`/`\{N\}`/`\+` to their
//! ERE meaning; sed's `-E`/`-r` opts back into strict ERE (awk has no such flag,
//! so it always rewrites). Driven through `kernel.execute()` so flag binding and
//! dispatch run the real path.

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

#[tokio::test]
async fn awk_bre_alternation_in_pattern() {
    let kernel = kernel_at(tempdir().unwrap().path());
    let (out, code) =
        run(&kernel, r#"printf 'cat\ndog\nfish\n' | awk '/cat\|dog/ {print}'"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "cat\ndog", "awk /re/ alternates on \\|: {out:?}");
}

#[tokio::test]
async fn awk_bre_alternation_in_match_operator() {
    let kernel = kernel_at(tempdir().unwrap().path());
    let (out, code) =
        run(&kernel, r#"echo hello | awk '{ if ($0 ~ /he\|xy/) print "match" }'"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out.trim(), "match", "awk ~ alternates on \\|: {out:?}");
}

#[tokio::test]
async fn awk_bre_alternation_in_gsub() {
    let kernel = kernel_at(tempdir().unwrap().path());
    let (out, code) =
        run(&kernel, r#"echo 'cat dog fish' | awk '{gsub(/cat\|dog/, "X"); print}'"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out.trim(), "X X fish", "gsub alternates on \\|: {out:?}");
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

/// A formerly-literal escape the rewrite breaks (`a\)` → unmatched `)`) fails
/// loudly WITH a dialect hint, in both sed and awk (awk's hint has no `-E`).
#[tokio::test]
async fn rewritten_meta_compile_errors_carry_dialect_hint() {
    let kernel = kernel_at(tempdir().unwrap().path());

    let err = kernel
        .execute(r#"echo x | sed 's/a\)/X/'"#)
        .await
        .expect_err("unmatched group should fail sed validation");
    let msg = err.to_string();
    assert!(msg.contains("GNU BRE"), "sed should explain the dialect: {msg}");
    assert!(msg.contains("-E/-r"), "sed should offer its strict-ERE flags: {msg}");

    let result = kernel
        .execute(r#"echo x | awk '/a\)/ {print}'"#)
        .await
        .expect("awk regex failure is a runtime error, not a validation error");
    assert_ne!(result.code, 0, "unmatched group should fail: {result:?}");
    assert!(result.err.contains("GNU BRE"), "awk should explain the dialect: {}", result.err);
    assert!(
        !result.err.contains("-E"),
        "awk has no -E flag and must not suggest one: {}",
        result.err
    );
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
async fn awk_invalid_separator_error_names_raw_form_with_hint() {
    let kernel = kernel_at(tempdir().unwrap().path());

    // FS path: `xx\(` → rewrite → `xx(` (unclosed group). The error must show
    // `xx\(` — the separator as written — plus the dialect note.
    let result = kernel
        .execute(r#"echo x | awk -F 'xx\(' '{print NF}'"#)
        .await
        .expect("invalid FS is a runtime error, not a validation error");
    assert_ne!(result.code, 0, "invalid FS should fail: {result:?}");
    // The separator appears {:?}-quoted, so the raw `xx\(` renders as `"xx\\("`.
    assert!(result.err.contains(r"xx\\("), "names raw separator: {}", result.err);
    assert!(result.err.contains("GNU BRE"), "carries the dialect hint: {}", result.err);
    assert!(!result.err.contains("-E"), "awk must not suggest -E: {}", result.err);

    // split() path: same contract.
    let result = kernel
        .execute(r#"echo x | awk '{n = split("abc", a, "yy\\("); print n}'"#)
        .await
        .expect("invalid split() separator is a runtime error");
    assert_ne!(result.code, 0, "invalid separator should fail: {result:?}");
    assert!(result.err.contains(r"yy\\("), "names raw separator: {}", result.err);
    assert!(result.err.contains("GNU BRE"), "carries the dialect hint: {}", result.err);
}
