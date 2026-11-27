//! Regression tests for builtin fidelity fixes (GH #192):
//! - `ls -- -1`, `head -- -N`, `tail -- -N` end-of-options handling
//! - `sort` multi-`-k`, `.C` char offsets, `-k2,1`, and per-key `b`/`f`/`V`
//! - `sed` non-quiet `p`/`s///p` auto-print and empty `//` pattern reuse
//! - `date -d` spaced sign operators (`2025-01-15 - 1 day`)

#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

#[tokio::test]
async fn double_dash_protects_negative_filenames_in_ls_head_tail() {
    let kernel = Kernel::new(KernelConfig::named("double-dash-neg")).unwrap();
    kernel
        .execute("mkdir -p /d; write /d/-1 \"a\\nb\\nc\\n\"; write /d/-2 \"x\\ny\\nz\\n\"; write /d/other \"ok\\n\"")
        .await
        .unwrap();

    // `ls -- -1` lists only `-1`, not the whole directory.
    let res = kernel.execute("cd /d; ls -- -1").await.unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out().trim(), "-1");

    // `ls -1 -- -1` uses `-1` as flag AND lists `-1` as filename.
    let res = kernel.execute("cd /d; ls -1 -- -1").await.unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out().trim(), "-1");

    // `head -- -2` reads the file named `-2`, not 2 lines of stdin.
    let res = kernel.execute("cd /d; head -- -2").await.unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out(), "x\ny\nz\n");

    // `head -1 -- -2` reads 1 line of the file named `-2`.
    let res = kernel.execute("cd /d; head -1 -- -2").await.unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out(), "x\n");

    // `tail -- -2` reads the file named `-2`.
    let res = kernel.execute("cd /d; tail -- -2").await.unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out(), "x\ny\nz\n");

    // `tail -1 -- -2` reads the last line of the file named `-2`.
    let res = kernel.execute("cd /d; tail -1 -- -2").await.unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out(), "z\n");
}

#[tokio::test]
async fn sort_supports_multiple_keys_char_offsets_and_modifiers() {
    let kernel = Kernel::new(KernelConfig::named("sort-multi-k")).unwrap();

    // Multiple `-k` keys: primary alpha on col 1, secondary numeric desc on col 2.
    let res = kernel
        .execute("printf 'b 2\\na 10\\na 2\\nb 10\\n' | sort -k 1,1 -k 2,2nr")
        .await
        .unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out(), "a 10\na 2\nb 10\nb 2\n");

    // `.C` character offsets within a field: sort by chars 2..=3 of field 1.
    let res = kernel
        .execute("printf 'xca\\nyab\\nzbb\\n' | sort -k 1.2,1.3")
        .await
        .unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out(), "yab\nzbb\nxca\n");

    // `-k 2,1` (stop < start) yields an empty key and falls back to whole-line tiebreak.
    let res = kernel
        .execute("printf 'b 1\\na 9\\n' | sort -k 2,1")
        .await
        .unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out(), "a 9\nb 1\n");

    // Per-key `b` (ignore leading blanks) and `f` (case-insensitive).
    let res = kernel
        .execute("printf '1,  beta\\n2,Alpha\\n3, gamma\\n' | sort -t , -k 2,2bf")
        .await
        .unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out(), "2,Alpha\n1,  beta\n3, gamma\n");

    // `-u` deduplicates by the active keys and keeps the first line in input order.
    let res = kernel
        .execute("printf 'a 1 y\\na 1 x\\na 2 z\\n' | sort -u -k 1,1 -k 2,2n")
        .await
        .unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out(), "a 1 y\na 2 z\n");
}

#[tokio::test]
async fn sed_non_quiet_print_and_empty_regex_reuse() {
    let kernel = Kernel::new(KernelConfig::named("sed-fidelity")).unwrap();

    // Non-quiet `p` prints each line twice (explicit `p` + auto-print).
    let res = kernel.execute("printf 'a\\nb\\n' | sed 'p'").await.unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out(), "a\na\nb\nb\n");

    // Non-quiet `s///p` prints substituted lines twice and unchanged lines once.
    let res = kernel
        .execute("printf 'foo\\nbar\\n' | sed 's/foo/baz/p'")
        .await
        .unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out(), "baz\nbaz\nbar\n");

    // Quiet `-n 's///p'` still prints only substituted lines once.
    let res = kernel
        .execute("printf 'foo\\nbar\\n' | sed -n 's/foo/baz/p'")
        .await
        .unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out(), "baz\n");

    // Empty `//` reuses the previous regex within one script and across `-e`.
    let res = kernel
        .execute("printf 'foo 1\\nbar 2\\n' | sed '/foo/s//FOO/'")
        .await
        .unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out(), "FOO 1\nbar 2\n");

    let res = kernel
        .execute("printf 'foo\\nbar\\n' | sed -e '/foo/s//FOO/' -e '//d'")
        .await
        .unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    // Second `-e` reuses `foo`; `FOO` no longer matches `foo`, `bar` doesn't either.
    assert_eq!(res.text_out(), "FOO\nbar\n");

    // Identity substitution (`s/foo/&/p`) still counts as a substitution made.
    let res = kernel
        .execute("printf 'foo\\nbar\\n' | sed -n 's/foo/&/p'")
        .await
        .unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out(), "foo\n");

    // Empty `//` preserves `I` flag from the previous regex.
    let res = kernel
        .execute("printf 'foo FOO\\n' | sed 's/foo/X/I; s//Y/'")
        .await
        .unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out(), "X Y\n");

    // Empty `//` with no prior regex errors loudly (caught at validation time).
    let err = kernel
        .execute("printf 'a\\n' | sed 's//b/'")
        .await
        .expect_err("empty regex with no prior pattern must fail")
        .to_string();
    assert!(
        err.contains("no previous regular expression"),
        "unexpected error: {err}"
    );
}

#[tokio::test]
async fn date_d_accepts_spaced_sign_operators() {
    let kernel = Kernel::new(KernelConfig::named("date-spaced-sign")).unwrap();

    let res = kernel
        .execute("date -u -d '2025-01-15 - 1 day' +%Y-%m-%d")
        .await
        .unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out().trim(), "2025-01-14");

    let res = kernel
        .execute("date -u -d '2025-01-15 + 2 days' +%Y-%m-%d")
        .await
        .unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out().trim(), "2025-01-17");

    let res = kernel
        .execute("date -u -d '2025-01-15 + 1 day - 2 hours' '+%Y-%m-%d %H:%M'")
        .await
        .unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out().trim(), "2025-01-15 22:00");

    // Relative anchor words (`now`, `today`, `yesterday`) compose with spaced offsets.
    let res = kernel
        .execute("date -u -d 'now - 1 day' +%Y")
        .await
        .unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out().trim().len(), 4);
}
