//! Kernel-routed regression tests for binary (`Value::Bytes`) at TEXT SINKS.
//!
//! A binary value captured via `$(...)` (e.g. `b=$(cat blob)` — `cat` of a
//! non-UTF-8 file yields `Value::Bytes`) must go LOUD when it reaches a text
//! sink — string interpolation (`"x=$b"`), a bare word into an external-command
//! argv (`prog $b`), or the `echo` text-output builtin — never render the
//! `[binary: N bytes]` placeholder. The placeholder where the user's real bytes
//! should be is silent data corruption (crash beats corrupt).
//!
//! All tests route through `kernel.execute()` so the full dispatch chain runs.

// Test-fixture code: unwrap/expect on known-good setup is the idiom.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use common::kernel_at;
use std::fs;
use tempfile::tempdir;

/// Invalid-UTF-8 octets so `cat` marks the capture binary (`Value::Bytes`)
/// rather than a `String` — that's the path that used to reach the placeholder.
const BIN: &[u8] = b"\xff\x00\xfe\x80\x01\xc0kaish\xf5";

/// Assert a script ran loud: either `execute` returned `Err`, or it returned a
/// nonzero `ExecResult` whose error names the binary problem — and in NO case
/// did the `[binary: N bytes]` placeholder leak into stdout OR stderr.
///
/// Checks for `"cannot be used as"` rather than a bare `"binary"` substring —
/// every text-sink guard in this PR (`value_to_text_sink[_named]`) shares that
/// exact wording, whereas a merely-coincidental "binary" (e.g. the leaked
/// placeholder itself embedded in an unrelated "No such file or directory"
/// message) would false-pass a bare substring check.
async fn assert_loud_binary(script: &str) {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());
    match kernel.execute(script).await {
        Ok(r) => {
            assert_ne!(r.code, 0, "binary at a text sink must be a nonzero exit: {script:?}");
            assert!(
                r.err.contains("cannot be used as"),
                "error should name the binary problem, got err={:?} for {script:?}",
                r.err
            );
            assert!(
                !r.text_out().contains("[binary") && !r.err.contains("[binary"),
                "the placeholder must NOT leak to stdout or stderr: out={:?} err={:?}",
                r.text_out(),
                r.err
            );
        }
        Err(e) => {
            // The alternate `{:#}` form walks the full `anyhow` cause chain —
            // some paths (e.g. `Stmt::Assignment`) wrap the real cause behind
            // a generic `.context("failed to evaluate assignment")`, so a
            // bare `{}` would only show that wrapper and miss the actual
            // binary-data message underneath.
            let msg = format!("{:#}", e);
            assert!(
                msg.contains("cannot be used as"),
                "execute error should name the binary problem, got {msg:?} for {script:?}"
            );
        }
    }
}

#[tokio::test]
async fn interpolating_a_binary_capture_is_loud_not_placeholder() {
    // The primary live bug: capture bytes, then splice into a string.
    assert_loud_binary(r#"b=$(cat src.bin); echo "x=$b""#).await;
}

#[tokio::test]
async fn bare_word_binary_into_echo_is_loud() {
    // `echo` is a pure text-output sink; a bare `$b` binary word goes loud.
    assert_loud_binary("b=$(cat src.bin); echo $b").await;
}

#[tokio::test]
async fn binary_arg_into_printf_is_loud() {
    // `printf` is a pure text-output sink like `echo`; a binary operand goes
    // loud instead of the `[binary: N bytes]` placeholder (kaibo C1).
    assert_loud_binary(r#"b=$(cat src.bin); printf "val=%s\n" "$b""#).await;
}

#[tokio::test]
async fn binary_in_default_expansion_is_loud() {
    // `${b:-fallback}` where b is present-and-binary must also go loud, not
    // render the placeholder (the value is present, so the default never fires).
    assert_loud_binary(r#"b=$(cat src.bin); echo "v=${b:-none}""#).await;
}

#[tokio::test]
async fn text_capture_interpolation_is_unaffected() {
    // Control: a normal text var still interpolates fine.
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute(r#"t=$(echo hi); echo "x=$t""#).await.unwrap();
    assert_eq!(result.code, 0, "text var must still work: {}", result.err);
    assert_eq!(result.text_out().trim(), "x=hi");
}

#[tokio::test]
async fn text_capture_bare_word_is_unaffected() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("t=$(echo hi); echo $t").await.unwrap();
    assert_eq!(result.code, 0, "text var must still work: {}", result.err);
    assert_eq!(result.text_out().trim(), "hi");
}

/// External-command argv (`build_args_flat`): a bare `$b` binary word must go
/// loud crossing the process boundary. Gated on Linux + subprocess because it
/// spawns a real `/bin/echo`.
#[cfg(all(target_os = "linux", feature = "subprocess"))]
#[tokio::test]
async fn bare_word_binary_into_external_argv_is_loud() {
    assert_loud_binary("b=$(cat src.bin); /bin/echo $b").await;
}

// ── Remaining text sinks (GH #93 item 1) ──
//
// The primary sinks above (string interpolation, bare-word external argv,
// `echo`) were fixed in 0.11.0 via `value_to_text_sink`. These tests cover
// the sinks that still fell through to `value_to_string`'s `[binary: N
// bytes]` placeholder: builtin path-positional coercion, env-var export, the
// redirect target, and the `==`/`in`/`case`-glob semantic ops.

/// `mkdir`'s path-positional loop used to `value_to_string` a `Value::Bytes`
/// operand straight into a directory name.
#[tokio::test]
async fn mkdir_binary_path_positional_is_loud() {
    assert_loud_binary("b=$(cat src.bin); mkdir $b").await;
}

/// `rm`'s path-positional loop, same class as `mkdir`.
#[tokio::test]
async fn rm_binary_path_positional_is_loud() {
    assert_loud_binary("b=$(cat src.bin); rm $b").await;
}

/// `cp`'s destination operand — also exercises the restructured single-source
/// gate-overwrite path that used to stringify the source a second time.
#[tokio::test]
async fn cp_binary_dest_is_loud() {
    assert_loud_binary("b=$(cat src.bin); cp src.bin $b").await;
}

/// `ls`'s multi-path positional list (the `.map(value_to_string).collect()`
/// pattern shared by `find`/`grep`/`sed -i`).
#[tokio::test]
async fn ls_binary_path_positional_is_loud() {
    assert_loud_binary("b=$(cat src.bin); ls $b").await;
}

/// `[[ -f $b ]]` — the VFS-aware `FileTest` arm in `kernel.rs::eval_test_async`
/// used to stat a file literally named `[binary: N bytes]`.
#[tokio::test]
async fn double_bracket_file_test_binary_path_is_loud() {
    assert_loud_binary("b=$(cat src.bin); if [[ -f $b ]]; then echo hit; fi").await;
}

/// `test -f $b` — the `test` builtin's own (separate) file-test implementation,
/// which mirrors `[[`'s but must independently guard the same way.
#[tokio::test]
async fn test_builtin_file_test_binary_path_is_loud() {
    assert_loud_binary("b=$(cat src.bin); test -f $b").await;
}

/// A bare `$b` binary word as a redirect target used to become a file
/// literally named `[binary: N bytes]` instead of erroring — the collection
/// guard (`structured_boundary_error`) already fired for lists/records, but
/// binary fell through `eval_redirect_target`'s local `value_to_string`.
#[tokio::test]
async fn redirect_target_binary_is_loud() {
    assert_loud_binary("b=$(cat src.bin); echo hi > $b").await;
}

/// `case $b in ...)` glob-matched against the `[binary: N bytes]` placeholder
/// instead of erroring — Decision E territory, same as `==`/`in`.
#[tokio::test]
async fn case_glob_binary_operand_is_loud() {
    assert_loud_binary(
        r#"b=$(cat src.bin); case $b in
    x) echo matched ;;
    *) echo default ;;
esac"#,
    )
    .await;
}

/// `[[ $b == x ]]` used to fall through `values_equal`'s mixed-scalar arm and
/// silently compare the *other* side's text against the `[binary: N bytes]`
/// placeholder rather than erroring.
#[tokio::test]
async fn equality_binary_operand_is_loud() {
    assert_loud_binary(r#"b=$(cat src.bin); if [[ $b == x ]]; then echo hit; fi"#).await;
}

/// `[[ $b in $record ]]` — record-key membership used to stringify the
/// binary needle into a lookup key via `value_to_string`.
#[tokio::test]
async fn membership_binary_needle_against_record_is_loud() {
    assert_loud_binary(r#"b=$(cat src.bin); r={"k": 1}; if [[ $b in $r ]]; then echo hit; fi"#)
        .await;
}

/// Exporting a binary value and then running ANY external command used to
/// silently pass `[binary: N bytes]` as the child's env var value. Gated like
/// the argv test above — spawns a real process.
#[cfg(all(target_os = "linux", feature = "subprocess"))]
#[tokio::test]
async fn env_export_of_binary_value_is_loud() {
    assert_loud_binary("b=$(cat src.bin); export BIN=$b; /bin/true").await;
}

// ── `ExecContext::expand_paths` — a second, independent path-coercion sink
// (found via kaibo review of this PR) ──
//
// `cat`/`head`/`tail`/`wc`/`checksum`/`file`/`base64_tool`/`tac`/`xxd` all
// funnel their path positionals through this one shared helper. It used to
// silently `continue` (skip) a `Value::Bytes` operand rather than reporting
// it — worse than the placeholder-stringify bug, since the value just
// vanished from the list, so most of these builtins fell back to reading
// STDIN instead of erroring on the binary path the user actually gave.

#[tokio::test]
async fn cat_binary_path_positional_is_loud() {
    assert_loud_binary("b=$(cat src.bin); cat $b").await;
}

#[tokio::test]
async fn head_binary_path_does_not_silently_fall_back_to_stdin() {
    // Prove it's not just "loud" but genuinely refuses to substitute stdin:
    // feed distinguishable stdin content and confirm it never appears.
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel
        .execute("b=$(cat src.bin); echo from-stdin | head $b")
        .await
        .unwrap();
    assert_ne!(result.code, 0, "err={}", result.err);
    assert!(result.err.contains("cannot be used as"), "err={:?}", result.err);
    assert!(
        !result.text_out().contains("from-stdin"),
        "must not silently read stdin instead of erroring on the binary path: {:?}",
        result.text_out()
    );
}

#[tokio::test]
async fn wc_binary_path_positional_is_loud() {
    assert_loud_binary("b=$(cat src.bin); wc $b").await;
}

// ── `expand_paths` again — GH #121: the same `_ => continue` catch-all also
// silently dropped `Value::Json` (list/record), `Value::Bool`, and
// `Value::Null` path operands (only `Value::Bytes` was fixed above, by #117).
// These land via `fromjson`, mirroring the `$(cat src.bin)` capture idiom the
// Bytes tests above use.

#[tokio::test]
async fn cat_list_path_positional_is_loud() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("xs=$(fromjson '[1,2,3]'); cat $xs").await.unwrap();
    assert_ne!(result.code, 0);
    assert!(result.err.contains("cannot use a list as a path"), "err={:?}", result.err);
}

#[tokio::test]
async fn cat_record_path_positional_is_loud() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute(r#"r=$(fromjson '{"k":1}'); cat $r"#).await.unwrap();
    assert_ne!(result.code, 0);
    assert!(result.err.contains("cannot use a record as a path"), "err={:?}", result.err);
}

#[tokio::test]
async fn cat_bool_path_positional_is_loud() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("b=$(fromjson true); cat $b").await.unwrap();
    assert_ne!(result.code, 0);
    assert!(result.err.contains("cannot use a bool (true) as a path"), "err={:?}", result.err);
}

#[tokio::test]
async fn cat_null_path_positional_is_loud() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("n=$(fromjson null); cat $n").await.unwrap();
    assert_ne!(result.code, 0);
    assert!(result.err.contains("cannot use null as a path"), "err={:?}", result.err);
}

#[tokio::test]
async fn head_list_path_does_not_silently_fall_back_to_stdin() {
    // Same "prove it's not just loud, it genuinely refuses the stdin
    // fallback" shape as `head_binary_path_does_not_silently_fall_back_to_stdin`
    // above, but for a list operand instead of binary.
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel
        .execute("xs=$(fromjson '[1,2,3]'); echo from-stdin | head $xs")
        .await
        .unwrap();
    assert_ne!(result.code, 0, "err={}", result.err);
    assert!(result.err.contains("cannot use a list as a path"), "err={:?}", result.err);
    assert!(
        !result.text_out().contains("from-stdin"),
        "must not silently read stdin instead of erroring on the list path: {:?}",
        result.text_out()
    );
}

// ── `cd`/`awk`/`basename`/`diff` — `ToolArgs::get_string`-based path reads
// (found via the same review) ──
//
// `get_string` (in the `kaish-types` leaf crate) already silently returns
// `None` for a `Value::Bytes` operand — not a corruption in itself, but each
// of these callers treats `None` as "operand absent" and does something
// silently wrong with a *present* binary operand: `cd` falls back to $HOME,
// `awk` falls back to reading stdin. `basename`/`diff` already errored loudly
// (just with a generic "missing" message); fixed for a clearer message and
// consistency with the rest of this sweep.

#[tokio::test]
async fn cd_binary_path_does_not_silently_go_to_home() {
    assert_loud_binary("b=$(cat src.bin); cd $b").await;
}

#[tokio::test]
async fn awk_binary_path_does_not_silently_fall_back_to_stdin() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel
        .execute(r#"b=$(cat src.bin); echo from-stdin | awk '{print}' $b"#)
        .await
        .unwrap();
    assert_ne!(result.code, 0, "err={}", result.err);
    assert!(result.err.contains("cannot be used as"), "err={:?}", result.err);
    assert!(
        !result.text_out().contains("from-stdin"),
        "must not silently read stdin instead of erroring on the binary path: {:?}",
        result.text_out()
    );
}

#[tokio::test]
async fn basename_binary_path_is_loud() {
    assert_loud_binary("b=$(cat src.bin); basename $b").await;
}

#[tokio::test]
async fn diff_binary_first_file_is_loud() {
    assert_loud_binary("b=$(cat src.bin); diff $b src.bin").await;
}

// ── The rest of the `ToolArgs::get_string`-based path readers (same review
// pass as `cd`/`awk` above) — `get_path_string` threaded through each ──

#[tokio::test]
async fn sed_binary_path_does_not_silently_fall_back_to_stdin() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel
        .execute("b=$(cat src.bin); echo from-stdin | sed 's/a/b/' $b")
        .await
        .unwrap();
    assert_ne!(result.code, 0, "err={}", result.err);
    assert!(result.err.contains("cannot be used as"), "err={:?}", result.err);
    assert!(
        !result.text_out().contains("from-stdin"),
        "must not silently read stdin instead of erroring on the binary path: {:?}",
        result.text_out()
    );
}

#[tokio::test]
async fn uniq_binary_path_does_not_silently_fall_back_to_stdin() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel
        .execute("b=$(cat src.bin); echo from-stdin | uniq $b")
        .await
        .unwrap();
    assert_ne!(result.code, 0, "err={}", result.err);
    assert!(result.err.contains("cannot be used as"), "err={:?}", result.err);
    assert!(
        !result.text_out().contains("from-stdin"),
        "must not silently read stdin instead of erroring on the binary path: {:?}",
        result.text_out()
    );
}

#[tokio::test]
async fn jq_binary_path_does_not_silently_fall_back_to_stdin() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel
        .execute(r#"b=$(cat src.bin); echo '{"from":"stdin"}' | jq '.' $b"#)
        .await
        .unwrap();
    assert_ne!(result.code, 0, "err={}", result.err);
    assert!(result.err.contains("cannot be used as"), "err={:?}", result.err);
    assert!(
        !result.text_out().contains("stdin"),
        "must not silently read stdin instead of erroring on the binary path: {:?}",
        result.text_out()
    );
}

#[tokio::test]
async fn tree_binary_path_is_loud() {
    assert_loud_binary("b=$(cat src.bin); tree $b").await;
}

#[tokio::test]
async fn write_binary_path_is_loud() {
    assert_loud_binary(r#"b=$(cat src.bin); write $b "content""#).await;
}

#[tokio::test]
async fn ln_binary_target_is_loud() {
    assert_loud_binary("b=$(cat src.bin); ln -s $b link.txt").await;
}

#[tokio::test]
async fn patch_binary_file_override_is_loud() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel
        .execute(r#"b=$(cat src.bin); echo "diff" | patch --file=$b"#)
        .await
        .unwrap();
    assert_ne!(result.code, 0, "err={}", result.err);
    assert!(result.err.contains("cannot be used as"), "err={:?}", result.err);
}

#[tokio::test]
async fn validate_binary_path_does_not_silently_fall_back_to_stdin() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel
        .execute("b=$(cat src.bin); echo 'echo hi' | kaish-validate $b")
        .await
        .unwrap();
    assert_ne!(result.code, 0, "err={}", result.err);
    assert!(result.err.contains("cannot be used as"), "err={:?}", result.err);
}

#[tokio::test]
async fn checksum_binary_check_override_is_loud() {
    assert_loud_binary("b=$(cat src.bin); checksum --check=$b").await;
}

/// `seq --separator=$BIN` (GH #120): `parsed.separator` comes from clap's
/// re-parse of `to_argv()`'s output, which already stringified the binary
/// value into the `[binary: N bytes]` placeholder by the time clap sees it —
/// checking the clap field before the untouched raw `ToolArgs` value hides a
/// binary separator entirely, silently splicing the placeholder text between
/// the generated numbers instead of erroring. Mirrors the checksum/patch
/// reorder fix from the #93 item-1 PR.
#[tokio::test]
async fn seq_separator_binary_is_loud() {
    assert_loud_binary("b=$(cat src.bin); seq --separator=$b 1 3").await;
}

/// `cut --fields=$BIN` (GH #120, found via kaibo review of this PR's own
/// audit): my first pass wrongly classified `fields`/`characters` as
/// already-loud like `delimiter` — in fact `select_indices` silently parses
/// the `[binary: N bytes]` placeholder as zero valid indices (no digits, no
/// `-`), so `cut` would silently emit one blank line per input line instead
/// of erroring. Same clap-field-first ordering hazard as `seq --separator`.
#[tokio::test]
async fn cut_fields_binary_is_loud_not_blank_output() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel
        .execute(r#"b=$(cat src.bin); echo "a,b,c" | cut --fields=$b -d,"#)
        .await
        .unwrap();
    assert_ne!(result.code, 0, "err={}", result.err);
    assert!(result.err.contains("cannot be used as"), "err={:?}", result.err);
    assert!(
        !result.text_out().contains('\n') || result.text_out().is_empty(),
        "must not silently emit blank output lines: {:?}",
        result.text_out()
    );
}

/// `cut --characters=$BIN` — same class as `fields` above.
#[tokio::test]
async fn cut_characters_binary_is_loud_not_blank_output() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel
        .execute("b=$(cat src.bin); echo abcdef | cut --characters=$b")
        .await
        .unwrap();
    assert_ne!(result.code, 0, "err={}", result.err);
    assert!(result.err.contains("cannot be used as"), "err={:?}", result.err);
}

/// `awk --field-separator=$BIN` (GH #120, missed in the original audit, found
/// via kaibo review): the placeholder would silently become awk's literal
/// `FS` — since it never matches real input, every line becomes a single
/// field instead of erroring. Same clap-field-first ordering hazard.
#[tokio::test]
async fn awk_field_separator_binary_is_loud() {
    assert_loud_binary(r#"b=$(cat src.bin); echo "a:b:c" | awk --field-separator=$b '{print $2}'"#)
        .await;
}

/// `cmp`'s two file operands used to be read off `parsed.paths` (the
/// clap-parsed, `to_argv()`-serialized field) instead of `args.positional` —
/// found via a third kaibo pass over this PR. A binary first operand silently
/// became a "No such file" error for a nonexistent `[binary: N bytes]` path
/// rather than naming the real problem.
#[tokio::test]
async fn cmp_binary_first_operand_is_loud() {
    assert_loud_binary("b=$(cat src.bin); cmp $b src.bin").await;
}

/// `exec`'s command name — the argv loop was already guarded, but the command
/// word itself read via `get_string`'s silent `None`, so `exec $BIN` used to
/// misreport as "exec: missing command" rather than the binary-sink error
/// (kaibo review of this PR). Gated on subprocess since exec is subprocess-only.
#[cfg(feature = "subprocess")]
#[tokio::test]
async fn exec_binary_command_is_loud() {
    assert_loud_binary("b=$(cat src.bin); exec $b arg1").await;
}

/// `printf`'s format positional — its format *arguments* were already
/// guarded, but the format string itself read via `get_string`'s silent
/// `None`, so `printf $BIN` used to misreport as "printf: missing format
/// argument" (kaibo review of this PR).
#[tokio::test]
async fn printf_binary_format_is_loud() {
    assert_loud_binary("b=$(cat src.bin); printf $b").await;
}

// `spawn`'s bareword `command=value` form actually routes through the
// (separately tracked, #116) WordAssign-reconstruction fallback rather than a
// named arg — `--command=`/`--cwd=` is the form that reaches `ToolArgs.named`
// untouched, which is what these two are testing.

#[cfg(feature = "subprocess")]
#[tokio::test]
async fn spawn_binary_command_is_loud() {
    assert_loud_binary("b=$(cat src.bin); spawn --command=$b").await;
}

#[cfg(all(target_os = "linux", feature = "subprocess"))]
#[tokio::test]
async fn spawn_binary_cwd_is_loud() {
    assert_loud_binary(r#"b=$(cat src.bin); spawn --command="/bin/true" --cwd=$b"#).await;
}

/// A binary value spliced into a heredoc body. Heredocs in real scripts
/// resolve through the async evaluator (`kernel.rs`), which already composes
/// through the guarded `eval_string_part_async` — this locks in that the
/// user-visible behavior is loud (the *sync* evaluator's own heredoc
/// assembly, `interpreter/eval.rs`, is covered directly in eval.rs's unit
/// tests since it isn't reachable from a real script).
#[tokio::test]
async fn heredoc_body_binary_var_is_loud() {
    assert_loud_binary("b=$(cat src.bin); cat <<EOF\n$b\nEOF").await;
}

/// A binary value used as a record's interpolated key (`{"$b": 1}`). Same
/// async-composition note as the heredoc test above.
#[tokio::test]
async fn record_key_binary_var_is_loud() {
    assert_loud_binary(r#"b=$(cat src.bin); r={"$b": 1}"#).await;
}

/// Control: `${#b}` is the byte count, not a loud error — binary length is
/// well-defined (unlike splicing it into text), so this must NOT regress into
/// erroring. Locks in the existing `value_length` behavior this PR relies on.
#[tokio::test]
async fn length_of_binary_is_byte_count_not_loud() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("b=$(cat src.bin); echo ${#b}").await.unwrap();
    assert_eq!(result.code, 0, "err={}", result.err);
    assert_eq!(result.text_out().trim(), BIN.len().to_string());
}

// ── GH #116: residual `value_to_string` binary-fallback sinks ──
//
// The #93 item-1 PR closed the five primary text sinks (string interpolation,
// echo, printf, path positionals, redirect targets, ==/in/case-glob). These
// tests cover the sinks that fell outside that PR's scope: `key=value`
// (WordAssign) reassembly in the argument binder (both the async kernel.rs
// path and its sync scheduler/pipeline.rs twin — the twin has its own direct
// unit test in pipeline.rs since scatter/gather is the only script-level route
// to it), and a handful of builtins' own non-path scalar fallbacks.

/// `awk -v x=$BIN` — `consume_flag_positionals`'s WordAssign reassembly arm.
#[tokio::test]
async fn awk_dash_v_binary_assignment_is_loud() {
    assert_loud_binary("b=$(cat src.bin); awk -v x=$b 'BEGIN{}'").await;
}

/// `test --foo=$BIN` — `test`'s raw-argv fast path, the `Arg::Named` arm.
#[tokio::test]
async fn test_builtin_raw_argv_named_binary_is_loud() {
    assert_loud_binary("b=$(cat src.bin); test --foo=$b").await;
}

/// `test foo=$BIN` — `test`'s raw-argv fast path, the `Arg::WordAssign` arm.
#[tokio::test]
async fn test_builtin_raw_argv_word_assign_binary_is_loud() {
    assert_loud_binary("b=$(cat src.bin); test foo=$b").await;
}

/// `test $BIN` — the raw-argv fast path's `Arg::Positional` arm intentionally
/// preserves every operand's real type (untouched Bytes included) since
/// `test` needs it for typed comparisons elsewhere; the guard belongs in
/// `test`'s own single-operand truthiness check instead. Found via kaibo
/// review of the rest of this PR (GH #116) — the Named/WordAssign siblings in
/// the same raw-argv arm were already guarded, but the bare-positional
/// truthiness path used `value_to_string` unguarded.
#[tokio::test]
async fn test_builtin_bare_positional_truthiness_binary_is_loud() {
    assert_loud_binary("b=$(cat src.bin); test $b").await;
}

/// `test -z $BIN` — same class as the bare-positional truthiness case above,
/// for the explicit empty-string operator.
#[tokio::test]
async fn test_builtin_dash_z_binary_is_loud() {
    assert_loud_binary("b=$(cat src.bin); test -z $b").await;
}

/// `test -n $BIN` — same class, for the non-empty-string operator.
#[tokio::test]
async fn test_builtin_dash_n_binary_is_loud() {
    assert_loud_binary("b=$(cat src.bin); test -n $b").await;
}

/// `dd if=$BIN` — the main-loop `Arg::WordAssign` non-word-assign fallback
/// (the general `key=value` → positional path any tool outside
/// export/alias/unalias uses). This is issue #116's own headline example.
#[tokio::test]
async fn dd_word_assign_binary_is_loud() {
    assert_loud_binary("b=$(cat src.bin); dd if=$b").await;
}

/// `alias name=$BIN` — the named-arg (`args.named`) half of alias's own
/// value_to_string fallback.
#[tokio::test]
async fn alias_named_binary_value_is_loud() {
    assert_loud_binary("b=$(cat src.bin); alias name=$b").await;
}

/// `alias $BIN` (bare positional) — the positional half of the same fallback.
#[tokio::test]
async fn alias_positional_binary_is_loud() {
    assert_loud_binary("b=$(cat src.bin); alias $b").await;
}

/// `unalias $BIN`.
#[tokio::test]
async fn unalias_binary_is_loud() {
    assert_loud_binary("b=$(cat src.bin); unalias $b").await;
}

/// `unset $BIN`.
#[tokio::test]
async fn unset_binary_is_loud() {
    assert_loud_binary("b=$(cat src.bin); unset $b").await;
}

/// `kill --signal=$BIN %1` — the NAMED-flag `--signal` fallback only; the
/// positional target form is guarded separately and untouched here.
#[tokio::test]
async fn kill_signal_named_binary_is_loud() {
    assert_loud_binary("b=$(cat src.bin); kill --signal=$b %1").await;
}

/// `grep --ftype=$BIN pattern file` — `read_repeatable_strings`'s array arm.
/// `--ftype` is repeatable, so even a single binary occurrence is JSON-encoded
/// as a base64 byte envelope inside a `Json(Array)` by the binder
/// (`push_repeatable_value` → `value_to_json`); the array arm used to silently
/// DROP that entry (`filter_map(v.as_str())` skips non-string JSON) rather
/// than erroring — worse than a placeholder, since the filter just vanished
/// and grep ran unfiltered against the real data.
///
/// Searches a plain-text file (not `src.bin`) so the assertion isolates the
/// `--ftype` guard: `src.bin` itself would trip grep's unrelated "refuses to
/// search binary content" guard first and mask whether this fix fired at all
/// (caught empirically — the pre-fix version of this test still failed, but
/// for that unrelated reason, not the silently-dropped filter).
#[tokio::test]
async fn grep_ftype_binary_is_loud_not_silently_dropped() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    fs::write(dir.path().join("plain.txt"), "hello world\n").unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel
        .execute("b=$(cat src.bin); grep --ftype=$b pattern plain.txt")
        .await
        .unwrap();
    assert_ne!(result.code, 0, "err={}", result.err);
    assert!(
        result.err.contains("cannot be used as"),
        "error should name the binary problem, got err={:?}",
        result.err
    );
}
