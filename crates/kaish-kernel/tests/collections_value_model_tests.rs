//! Value-model invariants for native collections (lists & records).
//!
//! Provenance: the 2026-07-03 cross-model coverage review (gemini-pro batch +
//! fable deliberation over a kaibo dossier) — both independently flagged that
//! the *load-bearing* semantics here had zero integration tests: copy-on-assign
//! aliasing, function-scope × mutation, read-modify-write ordering, iteration
//! snapshots, and the fromjson/tojson round-trip law the design doc declares
//! "test-pinned". Every test pins RATIFIED design (docs/arrays-and-hashes.md),
//! with one deliberate exception: `local_without_initializer_does_not_parse`
//! guards current *implementation* behavior because the write-through safety
//! property depends on it (see that test's comment). Behaviors the review
//! flagged as unratified are otherwise NOT asserted here — they're queued for
//! Decisions-section ratification first.
//!
//! The shared failure mode these guard against is the project's cardinal sin:
//! a plausible-but-wrong value with no error. In particular, a future
//! "make clones cheap" Rc/Arc refactor of the value model would corrupt both
//! bindings in every aliasing test below while the rest of the suite stays
//! green — that's the tripwire.
//!
//! Kernel-routed via `KernelConfig::isolated()` (pure data, no localfs), same
//! harness as `collection_access_tests.rs`.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use kaish_kernel::{Kernel, KernelConfig};

async fn setup() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated().with_skip_validation(true))
        .expect("kernel")
        .into()
}

/// Run a script, returning (trimmed stdout, exit code, err text).
async fn run(k: &Kernel, script: &str) -> (String, i64, String) {
    let r = k.execute(script).await.expect("kernel execute");
    (r.text_out().trim().to_string(), r.code, r.err.clone())
}

/// Run a script that must fail loudly (via Err or nonzero code), returning the
/// error text.
async fn loud_err(k: &Kernel, script: &str) -> String {
    match k.execute(script).await {
        Err(e) => format!("{e:#}"),
        Ok(r) => {
            assert_ne!(r.code, 0, "expected loud failure, got success: {r:?}");
            r.err.clone()
        }
    }
}

// ═══════════════════════════════════════════════════════════════════════
// Copy-on-assign: `b=$a` copies the VALUE; collections are never shared
// references (design doc "Value semantics — copy-on-assign").
// ═══════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn mutating_the_original_leaves_the_copy() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        "xs=[1 2 3]\nys=$xs\nxs[0]=9\nxs[2]=99\necho ${xs[0]} ${ys[0]} ${ys[2]}",
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "9 1 3", "copy must not observe writes to the original");
}

#[tokio::test]
async fn mutating_the_copy_leaves_the_original() {
    let k = setup().await;
    let (out, code, err) =
        run(&k, "xs=[1 2 3]\nys=$xs\nys[0]=9\necho ${xs[0]} ${ys[0]}").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "1 9", "original must not observe writes to the copy");
}

#[tokio::test]
async fn extracted_subvalue_is_an_owned_copy() {
    // The sharpest aliasing probe: a nested sub-value read out of a record must
    // be an owned copy — mutating it can't reach back into the parent, and the
    // parent's later state can't leak forward into it.
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        "cfg={db: {host: localhost, port: 5432}}\n\
         db=${cfg[db]}\n\
         db[host]=remote\n\
         echo ${cfg[db][host]} ${db[host]}",
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "localhost remote", "parent record must not move");
}

#[tokio::test]
async fn push_on_a_copy_does_not_grow_the_original() {
    let k = setup().await;
    let (out, code, err) =
        run(&k, "xs=[1 2 3]\nzs=$xs\npush zs 4\necho ${#xs} ${#zs}").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "3 4");
}

#[tokio::test]
async fn key_insert_on_a_copy_does_not_grow_the_original() {
    let k = setup().await;
    let (out, code, err) =
        run(&k, "r1={a: 1}\nr2=$r1\nr2[b]=2\necho ${#r1} ${#r2}").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "1 2");
}

// ═══════════════════════════════════════════════════════════════════════
// Function scope × mutation. Write-through follows from the ratified
// pieces: functions run in shared scope (sh-compatible) and `b=$a` is
// copy-on-assign — so an un-`local`ed bracket write reaches the caller,
// and `local xs=$xs` is the protection idiom (frame-local copy).
// ═══════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn subscript_write_in_a_function_reaches_the_caller() {
    let k = setup().await;
    let (out, code, err) =
        run(&k, "xs=[1 2 3]\nf() { xs[0]=99; }\nf\necho ${xs[0]}").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "99", "shared-scope write-through");
}

#[tokio::test]
async fn push_in_a_function_reaches_the_caller() {
    let k = setup().await;
    let (out, code, err) =
        run(&k, "xs=[1]\nf() { push xs 2; }\nf\necho ${#xs}").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "2");
}

#[tokio::test]
async fn local_copy_shadows_and_protects_the_caller() {
    // `local xs=$xs` takes a frame-local COPY; writes land there and die with
    // the frame. If copy-on-assign ever became aliasing, or `local` stopped
    // creating a binding that shadows walk_write's frame walk, the outer echo
    // flips to 77 — silent cross-scope corruption.
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        "xs=[1 2 3]\n\
         f() { local xs=$xs; xs[0]=77; echo ${xs[0]}; }\n\
         f\n\
         echo ${xs[0]}",
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "77\n1", "inner sees the copy; outer is untouched");
}

#[tokio::test]
async fn local_without_initializer_does_not_parse() {
    // Guard for a hazard that is structurally impossible today: `local ys`
    // (no initializer) is a PARSE error, so a subscript write after it can
    // never silently fall through the frame walk to the caller's collection.
    // This pins implementation behavior, not ratified design (the file-header
    // exception): if bare `local ys` is ever accepted, this fails and the
    // write-through interaction MUST be decided deliberately (see the
    // 2026-07-03 coverage review). Asserting "parse" specifically so a future
    // accept-then-fail-at-runtime path can't satisfy this test vacuously.
    let k = setup().await;
    let err = loud_err(&k, "ys=[a b c]\nf() { local ys; ys[0]=zzz; }\nf").await;
    assert!(err.contains("parse"), "must be a PARSE-level rejection: {err}");
}

// ═══════════════════════════════════════════════════════════════════════
// Read-modify-write: the RHS is evaluated against the PRE-write state.
// ═══════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn rhs_reads_the_pre_write_list() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        "xs=[10 20 30]\nxs[0]=${xs[-1]}\necho ${xs[0]} ${xs[1]} ${xs[2]}",
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "30 20 30");
}

#[tokio::test]
async fn rhs_reads_the_pre_write_record() {
    let k = setup().await;
    let (out, code, err) =
        run(&k, "r={a: 1, b: 2}\nr[a]=${r[b]}\necho ${r[a]} ${r[b]}").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "2 2");
}

#[tokio::test]
async fn push_reading_its_own_tail_appends_exactly_once() {
    let k = setup().await;
    let (out, code, err) =
        run(&k, "ys=[1 2]\npush ys ${ys[-1]}\necho ${#ys} ${ys[2]}").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "3 2");
}

#[tokio::test]
async fn swap_via_temp_works() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        "xs=[10 20]\nt=${xs[0]}\nxs[0]=${xs[1]}\nxs[1]=$t\necho ${xs[0]} ${xs[1]}",
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "20 10");
}

// ═══════════════════════════════════════════════════════════════════════
// Iteration snapshot: `for v in $(values $xs)` materializes before the
// loop runs — mutations inside the body land on the variable but never
// feed back into the trip count or the yielded elements.
// ═══════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn growing_the_source_during_iteration_keeps_the_trip_count() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        "xs=[1 2 3]\n\
         count=0\n\
         for v in $(values $xs); do\n\
           push xs 99\n\
           count=$((count + 1))\n\
         done\n\
         echo $count ${#xs}",
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "3 6", "3 trips from the snapshot; 3 pushes still landed");
}

#[tokio::test]
async fn replacing_the_source_during_iteration_yields_the_snapshot() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        "ys=[a b c]\n\
         for v in $(values $ys); do\n\
           ys=[]\n\
           echo $v\n\
         done",
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "a\nb\nc", "all three pre-loop elements");
}

// ═══════════════════════════════════════════════════════════════════════
// Round-trip law (design doc: "Roundtrip law, test-pinned" — this is the
// pin): fromjson "$(tojson $x)" is structural identity, THROUGH the $()
// and double-quoted-interpolation boundaries.
// ═══════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn roundtrip_preserves_nested_structure() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        "data={tags: [a b c], meta: {active: true}}\n\
         d2=$(fromjson \"$(tojson $data)\")\n\
         echo ${d2[tags][1]} ${d2[meta][active]} ${#d2[tags]}",
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "b true 3");
}

#[tokio::test]
async fn roundtrip_preserves_empty_container_types() {
    // Empty list and empty record must survive as THEMSELVES — an empty `{}`
    // deserializing as a list (or vice versa) is a silent shape corruption.
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"e=$(fromjson '{"l":[],"r":{}}')
e2=$(fromjson "$(tojson $e)")
echo ${#e2[l]} ${#e2[r]} $(typeof ${e2[l]}) $(typeof ${e2[r]})"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "0 0 list record");
}

#[tokio::test]
async fn roundtrip_preserves_key_insertion_order() {
    // Rides the workspace-wide serde_json `preserve_order` feature; if that
    // ever drops, keys scramble silently and every `keys`-ordered consumer
    // breaks. (jaq-core 3 made insertion order part of the 0.10.0 contract.)
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"r=$(fromjson '{"z":1,"a":2,"m":3}')
keys $(fromjson "$(tojson $r)")"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, r#"["z","a","m"]"#);
}

#[tokio::test]
async fn roundtrip_preserves_number_fidelity() {
    // Integers beyond f64's 2^53 mantissa stay exact (i64 path — no float
    // laundering), floats stay floats, and `2` does not become `2.0`.
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"n=$(fromjson '{"big": 9007199254740993, "f": 1.5, "i": 2}')
n2=$(fromjson "$(tojson $n)")
echo ${n2[big]} ${n2[f]} ${n2[i]}"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "9007199254740993 1.5 2");
}

#[tokio::test]
async fn roundtrip_preserves_unicode_and_odd_keys() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"u=$(fromjson '{"héllo":1,"":2,"a b":3}')
u2=$(fromjson "$(tojson $u)")
echo ${#u2} ${u2[héllo]}"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "3 1");
}

// ═══════════════════════════════════════════════════════════════════════
// Decision B cross-path agreement + exact-string record keys. The
// arithmetic evaluator and the interpolation resolver live in different
// modules and can drift independently — pin them against each other.
// ═══════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn arithmetic_bareword_and_dynamic_subscripts_agree() {
    // Decision B: inside $(( )), a bareword subscript is a VARIABLE read;
    // ${xs[$i]} is the interpolation spelling of the same access. All three
    // must yield the same element, from one script.
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        "xs=[10 20 30]\ni=2\necho $((xs[i])) ${xs[$i]} $((xs[$i]))",
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "30 30 30");
}

#[tokio::test]
async fn quoted_numeric_record_keys_are_exact_strings() {
    // "1" and "01" are DISTINCT keys — no layer may normalize numeric-looking
    // record keys through an integer parse (that would silently collide them).
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"r=$(fromjson '{"1": "one", "01": "zero-one"}')
echo ${r["1"]} ${r["01"]} ${#r}"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "one zero-one 2");
}

#[tokio::test]
async fn bareword_numeric_subscript_on_a_record_is_loud() {
    // A bareword digit is an integer index, even on a record — kaish refuses
    // rather than coercing to the string key (the error names the quoted fix).
    let k = setup().await;
    let err = loud_err(&k, r#"r=$(fromjson '{"1": "one"}'); echo ${r[1]}"#).await;
    assert!(err.contains("integer index on a record"), "got: {err}");
    assert!(err.contains(r#"${r["1"]}"#), "should name the quoted-key fix: {err}");
}

#[tokio::test]
async fn quoted_numeric_key_on_a_list_is_loud() {
    // The mirror image: a quoted "1" is a string key and must NOT coerce into
    // list index 1 — silent coercion here inverts the one-subscript rule.
    let k = setup().await;
    let err = loud_err(&k, r#"xs=[a b]; echo ${xs["1"]}"#).await;
    assert!(err.contains("string key on a list"), "got: {err}");
}

#[tokio::test]
async fn dynamic_numeric_value_keys_a_record_as_a_string() {
    // `${r[$n]}` with n=1: the dynamic value arrives as the string "1" and
    // keys the record — the lenient dynamic seam, per the shipped resolver.
    let k = setup().await;
    let (out, code, err) =
        run(&k, r#"r=$(fromjson '{"1": "one"}'); n=1; echo ${r[$n]}"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "one");
}

#[tokio::test]
async fn dynamic_key_lvalue_writes_the_resolved_key() {
    // u[$k]=v must resolve $k during the write descent — writing a literal
    // "$k" key instead would be silent corruption.
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        "u={port: 8080}\nk=port\nu[$k]=9090\necho ${u[port]} ${#u}",
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "9090 1", "updated in place — no phantom literal-$k key");
}

// ═══════════════════════════════════════════════════════════════════════
// Boundary pins: literals stay out of argv; `in` refuses scalar RHS even
// when it's glob-shaped; dot access stays dead inside strings; shape
// errors are not swallowed by interpolation.
// ═══════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn spread_at_argv_is_a_parse_error() {
    // `...` is a value-position token only; `cmd ...$xs` argv splat is
    // explicitly excluded by the design. Asserting a PARSE-level rejection so
    // a future accept-then-glob/stringify path can't pass this vacuously.
    let k = setup().await;
    let err = loud_err(&k, "xs=[a b]\necho ...$xs").await;
    assert!(err.contains("parse"), "argv spread must be a parse error: {err}");
}

#[tokio::test]
async fn glob_shaped_scalar_rhs_for_in_is_loud() {
    // `[[ x in * ]]` — the `*` must not glob-expand or silently scalar-match;
    // it's the same loud string-RHS error as any other scalar.
    let k = setup().await;
    let err = loud_err(&k, "if [[ x in * ]]; then echo hit; fi").await;
    assert!(
        err.contains("list or record"),
        "should name the RHS requirement: {err}"
    );
}

#[tokio::test]
async fn dot_access_in_a_string_leaves_the_dot_literal() {
    // "$user.name" expands $user (compact JSON) and leaves `.name` as text —
    // reinforcing that access is bracket-shaped. No salvage, no error.
    let k = setup().await;
    let (out, code, err) =
        run(&k, r#"user={name: amy}; echo "$user.name""#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, r#"{"name":"amy"}.name"#);
}

#[tokio::test]
async fn shape_error_inside_a_string_is_not_swallowed() {
    // Interpolation swallows UNDEFINED ROOTS to "" (bash parity, pinned in
    // collection_access_tests) — but a SHAPE error must stay loud, never
    // render as partial output like "hello ".
    let k = setup().await;
    let err = loud_err(&k, r#"u={name: amy}; echo "hello ${u[0]}""#).await;
    assert!(err.contains("integer index on a record"), "got: {err}");
}
