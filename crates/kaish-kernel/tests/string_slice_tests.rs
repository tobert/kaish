//! Spec tests: `${s[a:b]}` slices a string the same way it slices a list.
//!
//! kaish already sliced lists with `${xs[1:3]}` — start:end, end-exclusive,
//! negatives counting from the end, either bound omittable. Strings now use
//! the same form and the same rules, so there is one slice convention in the
//! language rather than two.
//!
//! Deliberately NOT bash's `${s:offset:length}`: that is offset-and-length
//! where kaish's brackets are start-and-end, so the two spellings would
//! disagree on what `1:3` means. The bash form is a loud error pointing here.
//!
//! Slicing counts **characters, not bytes** — kaish refuses lossy text
//! elsewhere, and a byte slice can split a multi-byte sequence.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::Kernel;

/// A parse error surfaces as `Err` from `execute`, not as a non-zero result,
/// so both shapes are folded into (output, code, message) here.
async fn run(prog: &str) -> (String, i64, String) {
    let kernel = Kernel::transient().unwrap();
    match kernel.execute(prog).await {
        Ok(r) => (r.text_out().trim().to_string(), r.code, r.err.clone()),
        Err(e) => (String::new(), 2, format!("{e:#}")),
    }
}

// ---------------------------------------------------------------------------
// The core rule, mirroring the list slice exactly.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn a_slice_takes_the_half_open_range() {
    let (out, code, err) = run(r#"v="abcdefghij"; echo "[${v[0:5]}]""#).await;
    assert_eq!(code, 0, "should succeed: {err}");
    assert_eq!(out, "[abcde]");
}

#[tokio::test]
async fn a_slice_starting_mid_string_is_end_exclusive() {
    let (out, code, err) = run(r#"v="abcdefghij"; echo "[${v[2:5]}]""#).await;
    assert_eq!(code, 0, "should succeed: {err}");
    assert_eq!(out, "[cde]", "2..5 is three characters, not four");
}

#[tokio::test]
async fn an_omitted_start_means_the_beginning() {
    let (out, code, _) = run(r#"v="abcdefghij"; echo "[${v[:3]}]""#).await;
    assert_eq!(code, 0);
    assert_eq!(out, "[abc]");
}

#[tokio::test]
async fn an_omitted_end_means_the_rest() {
    let (out, code, _) = run(r#"v="abcdefghij"; echo "[${v[7:]}]""#).await;
    assert_eq!(code, 0);
    assert_eq!(out, "[hij]");
}

// ---------------------------------------------------------------------------
// Negatives count from the end, as they already do for lists.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn a_negative_start_counts_from_the_end() {
    let (out, code, _) = run(r#"v="abcdefghij"; echo "[${v[-3:]}]""#).await;
    assert_eq!(code, 0);
    assert_eq!(out, "[hij]");
}

#[tokio::test]
async fn a_negative_end_trims_from_the_end() {
    let (out, code, _) = run(r#"v="abcdefghij"; echo "[${v[:-2]}]""#).await;
    assert_eq!(code, 0);
    assert_eq!(out, "[abcdefgh]");
}

// ---------------------------------------------------------------------------
// Out-of-range clamps rather than erroring — same as the list slice.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn an_over_long_end_clamps_to_the_string() {
    let (out, code, _) = run(r#"v="abc"; echo "[${v[0:99]}]""#).await;
    assert_eq!(code, 0);
    assert_eq!(out, "[abc]");
}

#[tokio::test]
async fn an_inverted_range_is_empty_not_an_error() {
    let (out, code, _) = run(r#"v="abcdef"; echo "[${v[4:2]}]""#).await;
    assert_eq!(code, 0);
    assert_eq!(out, "[]");
}

// ---------------------------------------------------------------------------
// Characters, not bytes. A byte slice would split these.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn slicing_counts_characters_not_bytes() {
    // Each of these is 3 bytes in UTF-8; a byte-based slice would produce
    // mojibake or an error instead of the first two characters.
    let (out, code, err) = run(r#"v="日本語です"; echo "[${v[0:2]}]""#).await;
    assert_eq!(code, 0, "should succeed: {err}");
    assert_eq!(out, "[日本]");
}

#[tokio::test]
async fn a_negative_slice_also_counts_characters() {
    let (out, code, _) = run(r#"v="日本語です"; echo "[${v[-2:]}]""#).await;
    assert_eq!(code, 0);
    assert_eq!(out, "[です]");
}

// ---------------------------------------------------------------------------
// The list slice is untouched — the whole point is one convention, not two.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn the_list_slice_still_behaves_exactly_as_before() {
    let (out, code, _) = run(r#"xs=[1 2 3 4 5]; echo "[${xs[1:3]}]""#).await;
    assert_eq!(code, 0);
    assert_eq!(out, "[[2,3]]");
}

// ---------------------------------------------------------------------------
// Everything that is not a string or a collection is a loud error.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn slicing_a_number_is_a_loud_error() {
    let (_, code, err) = run(r#"n=42; echo "${n[0:1]}""#).await;
    assert_ne!(code, 0, "a number has no slice");
    assert!(!err.is_empty(), "the failure must say something");
}

#[tokio::test]
async fn slicing_a_record_is_a_loud_error() {
    let (_, code, err) = run(r#"r={a: 1}; echo "${r[0:1]}""#).await;
    assert_ne!(code, 0, "a record has no order to slice");
    assert!(err.contains("record"), "message should name the shape: {err}");
}

// ---------------------------------------------------------------------------
// bash's `${v:0:5}` used to expand to nothing at all — silently, which turned
// `"${d:0:4}/file"` into `/file`. It must be loud, and it must say what to
// write instead.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn the_bash_substring_form_is_a_loud_error_naming_the_kaish_form() {
    let (_, code, err) = run(r#"v="abcdefghij"; echo "[${v:0:5}]""#).await;
    assert_ne!(code, 0, "the bash form must not silently expand to nothing");
    assert!(
        err.contains("[0:5]") || err.contains("["),
        "the error must point at the bracket form: {err}"
    );
}

#[tokio::test]
async fn the_bash_form_with_an_omitted_offset_names_the_right_kaish_slice() {
    // bash's `${v::5}` omits the offset (defaults to 0); the kaish suggestion
    // must be `[0:5]`, not `[5:]` — trimming the two leading colons naively
    // discarded the "offset is zero, not five" distinction.
    let (_, code, err) = run(r#"v="abcdefghij"; echo "[${v::5}]""#).await;
    assert_ne!(code, 0, "the bash form must not silently expand to nothing");
    assert!(
        err.contains("[0:5]"),
        "an omitted offset means 0, so the suggestion must be [0:5], not [5:]: {err}"
    );
}

#[tokio::test]
async fn the_bash_form_does_not_silently_collapse_a_path() {
    // The shape that made this urgent: a path built from a substring silently
    // became root-relative, so `rm "${d:0:4}/file"` targeted `/file`.
    let (out, code, _) = run(r#"d="/tmp/somedir"; echo "[${d:0:4}/file]""#).await;
    assert_ne!(code, 0, "must not produce a quietly wrong path: got {out}");
}
