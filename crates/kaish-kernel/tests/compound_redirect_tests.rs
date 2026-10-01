//! Redirects on compound statements: `{ …; } > out`, `while …; done < f`,
//! `for …; done 2>&1`, `if …; fi > log`, `case … esac > out`.
//!
//! The redirects open before the body runs, apply to everything the body
//! writes, and leave `exit`, `return`, `break`, and `continue` working: a
//! redirect does not make the compound a subshell.
//!
//! A plan publishes a compound's redirects on every command inside it, so an
//! embedder that judges writes by `PlannedCommand::redirects` sees the write.
//!
//! Expectations are bash's, from running each row against bash, except where
//! a test says otherwise.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::ast::sexpr::format_program;
use kaish_kernel::parser::parse;
use kaish_kernel::{plan_program, ExecuteOptions, Kernel, KernelConfig, KernelError};
use rstest::rstest;

/// An in-memory kernel: `/` and `/tmp` are memory filesystems.
fn kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated()).expect("failed to create kernel")
}

/// The same kernel with the validator off, so the runtime's own refusals
/// are what a test sees.
fn unvalidated_kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated().with_skip_validation(true)).expect("failed to create kernel")
}

async fn run(source: &str) -> (String, i64) {
    let result = kernel().execute(source).await.expect("execution failed");
    (result.text_out().into_owned(), result.code)
}

// ---- parse shape ------------------------------------------------------------

#[rstest]
#[case::group_stdout(
    "{ echo a; } > out",
    r#"(redirected (group (cmd echo (pos (string "a")))) (redir > (string "out")))"#
)]
#[case::if_stdout(
    "if true; then echo a; fi > log",
    r#"(redirected (if (cmd true) (then (cmd echo (pos (string "a")))) (else)) (redir > (string "log")))"#
)]
#[case::while_stdin(
    "while read l; do echo $l; done < f",
    r#"(redirected (while (cmd read (pos (string "l"))) (do (cmd echo (pos (varref l))))) (redir < (string "f")))"#
)]
#[case::two_redirects(
    "{ echo a; } > out 2>&1",
    r#"(redirected (group (cmd echo (pos (string "a")))) (redir > (string "out")) (redir 2>&1 (null)))"#
)]
#[case::in_a_pipeline(
    "{ echo a; } 2>&1 | cat",
    r#"(pipeline (redirected (group (cmd echo (pos (string "a")))) (redir 2>&1 (null))) (cmd cat))"#
)]
fn a_redirect_after_a_compound_parses(#[case] source: &str, #[case] expected: &str) {
    let program = parse(source).unwrap_or_else(|e| panic!("`{source}` must parse: {e:?}"));
    assert_eq!(format_program(&program), expected, "`{source}`");
}

/// Two stdin sources on one compound are ambiguous, as on a command.
#[test]
fn two_stdin_redirects_on_a_compound_are_refused() {
    let errors = parse("{ cat; } < a < b").expect_err("must be refused");
    assert!(
        errors[0].message.contains("multiple stdin redirects"),
        "{:?}",
        errors
    );
}

// ---- running ----------------------------------------------------------------

#[rstest]
#[case::group_to_a_file("{ echo a; echo b; } > /out; cat /out", "a\nb\n", 0)]
#[case::if_to_a_file("if true; then echo yes; fi > /log; cat /log", "yes\n", 0)]
#[case::for_to_a_file("for i in 1 2; do echo $i; done > /f; cat /f", "1\n2\n", 0)]
#[case::case_to_a_file("case a in a) echo A ;; esac > /c; cat /c", "A\n", 0)]
#[case::while_reads_a_file(
    "printf 'a\\nb\\n' > /f; while read l; do echo \"L:$l\"; done < /f",
    "L:a\nL:b\n",
    0
)]
#[case::group_reads_a_file("printf 'a\\nb\\nc\\n' > /in; { read x; read y; echo \"$x$y\"; } < /in", "ab\n", 0)]
#[case::here_string("{ read x; echo \"got $x\"; } <<< hello", "got hello\n", 0)]
#[case::append("echo one > /ap; { echo two; } >> /ap; cat /ap", "one\ntwo\n", 0)]
#[case::stderr_to_a_file("{ echo out; echo err >&2; } 2> /e; echo \"e:$(cat /e)\"", "out\ne:err\n", 0)]
#[case::both_to_a_file("{ echo out; echo err >&2; } &> /b; cat /b", "out\nerr\n", 0)]
#[case::merge_into_a_pipe("{ echo out; echo err >&2; } 2>&1 | wc -l", "2\n", 0)]
#[case::stdout_away_from_the_pipe("{ echo a; } 2>&1 > /dev/null | wc -l", "0\n", 0)]
#[case::redirected_stage_feeds_a_pipe(
    "printf '1\\n2\\n3\\n' > /n; while read l; do echo \"$l\"; done < /n | sort -r",
    "3\n2\n1\n",
    0
)]
#[case::last_stage_to_a_file("echo ab | { read v; echo \"v=$v\"; } > /vo; cat /vo", "v=ab\n", 0)]
#[case::input_redirect_beats_the_pipe("echo in-file > /in; echo piped | { cat; } < /in", "in-file\n", 0)]
#[case::status_of_the_body("{ echo a; false; } > /dev/null; echo $?", "1\n", 0)]
#[case::session_changes_stay("{ x=2; echo hi; } > /dev/null; echo $x", "2\n", 0)]
#[case::cwd_stays("{ cd /tmp; pwd; } > /dev/null; pwd", "/tmp\n", 0)]
#[tokio::test]
async fn a_redirected_compound_runs(#[case] source: &str, #[case] stdout: &str, #[case] code: i64) {
    let (out, actual_code) = run(source).await;
    assert_eq!(out, stdout, "`{source}` stdout");
    assert_eq!(actual_code, code, "`{source}` exit code");
}

/// Redirects apply left to right and open before the body runs.
#[rstest]
// Both targets open; stdout ends at the last one.
#[case::last_stdout_target_wins("{ echo a; } > /o1 > /o2; echo \"o1:$(cat /o1) o2:$(cat /o2)\"", "o1: o2:a\n")]
// The inner redirect applies first; the outer target opens and stays empty.
#[case::inner_redirect_first("{ echo a > /i1; } > /i2; echo \"i1:$(cat /i1) i2:$(cat /i2)\"", "i1:a i2:\n")]
// `>` truncates before the body reads.
#[case::truncates_before_the_body("echo data > /s; { cat /s; } > /s; echo \"size:$(wc -c < /s)\"", "size:0\n")]
#[tokio::test]
async fn compound_redirects_open_before_the_body(#[case] source: &str, #[case] stdout: &str) {
    let (out, code) = run(source).await;
    assert_eq!(out, stdout, "`{source}`");
    assert_eq!(code, 0, "`{source}`");
}

/// A target that cannot open means the body does not run, exit 1.
#[tokio::test]
async fn a_target_that_cannot_open_skips_the_body() {
    let (out, code) = run("{ touch /ran; } > /nodir/a; echo \"rc=$?\"; [[ -e /ran ]] && echo ran || echo skipped").await;
    assert_eq!(out, "rc=1\nskipped\n");
    assert_eq!(code, 0);
}

/// bash: `set -e; { echo hi; } > /nonexistent/x; echo after` exits 1.
#[tokio::test]
async fn a_redirect_that_cannot_open_trips_errexit() {
    let (out, code) = run("set -e; { echo hi; } > /nodir/x; echo after").await;
    assert_eq!(out, "");
    assert_eq!(code, 1);
}

// ---- control flow survives the redirect ---------------------------------------

/// bash: `f() { { exit 4; } >/dev/null; echo after; }; f` exits 4.
#[tokio::test]
async fn exit_inside_a_redirected_group_exits_the_script() {
    let (out, code) = run("f() { { exit 4; } > /dev/null; echo after; }; f; echo after2").await;
    assert_eq!(out, "");
    assert_eq!(code, 4);
}

#[tokio::test]
async fn exit_inside_a_redirected_if_exits_the_script() {
    let (out, code) = run(
        "if false; then exit 1; fi > /log; echo after; if true; then exit 3; fi > /log; echo notreached",
    )
    .await;
    assert_eq!(out, "after\n");
    assert_eq!(code, 3);
}

/// Output written before the `exit` still reaches the redirect target.
#[tokio::test]
async fn output_before_exit_reaches_the_target() {
    let kernel = kernel();
    let result = kernel.execute("{ echo kept; exit 2; } > /x").await.expect("runs");
    assert_eq!(result.code, 2);
    assert_eq!(result.text_out(), "", "stdout went to the file, not the result");
    let cat = kernel.execute("cat /x").await.expect("runs");
    assert_eq!(cat.text_out(), "kept\n");
}

#[tokio::test]
async fn return_inside_a_redirected_group_returns() {
    let (out, code) = run("f() { { return 5; echo no; } > /ret; echo no2; }; f; echo \"rc=$?\"; wc -c < /ret").await;
    assert_eq!(out, "rc=5\n0\n");
    assert_eq!(code, 0);
}

#[rstest]
#[case::continue_skips("for i in 1 2 3; do { [[ $i == 2 ]] && continue; echo $i; } > /dev/null; echo i$i; done", "i1\ni3\n")]
#[case::break_stops("for i in 1 2 3; do { [[ $i == 2 ]] && break; echo $i; } > \"/fo$i\"; done; cat /fo1; [[ -e /fo2 ]] && echo fo2; [[ -e /fo3 ]] || echo no-fo3", "1\nfo2\nno-fo3\n")]
#[tokio::test]
async fn loop_control_inside_a_redirected_group_reaches_the_loop(#[case] source: &str, #[case] stdout: &str) {
    let (out, code) = run(source).await;
    assert_eq!(out, stdout, "`{source}`");
    assert_eq!(code, 0, "`{source}`");
}

/// errexit inside a redirected body exits the script, and what the body
/// printed first still lands in the file.
#[tokio::test]
async fn errexit_inside_a_redirected_body_exits() {
    let kernel = kernel();
    let result = kernel
        .execute("set -e; f() { { echo first; false; echo no; } > /fe; echo no2; }; f; echo after")
        .await
        .expect("runs");
    assert_eq!(result.code, 1);
    assert_eq!(result.text_out(), "");
    let cat = kernel.execute("cat /fe").await.expect("runs");
    assert_eq!(cat.text_out(), "first\n");
}

// ---- stdout and stderr ordering ---------------------------------------------------

/// kaish collects a compound's stdout and stderr separately, so `2>&1` puts
/// all of stderr after all of stdout. bash interleaves them line by line
/// (`out1 err1 out2 err2`). A known difference, shared with a function call
/// (`f 2>&1`); this test fails if the ordering changes, so update
/// `limits.md` with it.
#[tokio::test]
async fn merged_stderr_follows_stdout() {
    let (out, code) = run("for i in 1 2; do echo out$i; echo err$i >&2; done 2>&1 | cat").await;
    assert_eq!(out, "out1\nout2\nerr1\nerr2\n");
    assert_eq!(code, 0);
}

// ---- stdin scoping --------------------------------------------------------------

/// Known bug, shared with commands (`read x < g; cat` loses the session's
/// stdin the same way): an input redirect on a compound replaces the
/// session's stdin for the rest of the call instead of only for the
/// compound. bash prints `L:a`, `L:b`, then `S`. The input-redirect scoping
/// fix restores the displaced stdin in `finish_redirects`, which this
/// wrapper shares with command stages. When that fix lands this test fails;
/// change the expected output to "L:a\nL:b\nS\n".
#[tokio::test]
async fn known_bug_an_input_redirect_on_a_compound_outlives_it() {
    let kernel = kernel();
    let result = kernel
        .execute_with_options(
            "printf 'a\\nb\\n' > /f; while read l; do echo \"L:$l\"; done < /f; cat",
            ExecuteOptions::new().with_stdin("S\n"),
        )
        .await
        .expect("runs");
    assert_eq!(result.text_out(), "L:a\nL:b\n");
}

// ---- plan: a compound's redirects reach every command inside it ---------------
//
// An embedder that classifies a statement as read-only because no
// `PlannedCommand` carries a write redirect must see `{ cat a; } > out` as a
// write. Each test here fails if `collect_stmt` stops pushing a compound's
// redirects down onto the commands it contains.

/// `(name, [(kind, target)])` for every planned command in `source`'s first
/// statement.
fn planned(source: &str) -> Vec<(String, Vec<(String, String)>)> {
    let plans = plan_program(source).unwrap_or_else(|e| panic!("`{source}` must plan: {e:?}"));
    plans[0]
        .plan
        .commands
        .iter()
        .map(|c| {
            let redirects = c
                .redirects
                .iter()
                .map(|r| (r.kind.clone(), r.target.display()))
                .collect();
            (c.name.clone(), redirects)
        })
        .collect()
}

fn redirect(kind: &str, target: &str) -> (String, String) {
    (kind.to_string(), target.to_string())
}

#[test]
fn a_group_redirect_reaches_the_command_inside() {
    assert_eq!(
        planned("{ cat a; } > out"),
        vec![("cat".to_string(), vec![redirect(">", "out")])],
    );
}

#[test]
fn a_loop_redirect_reaches_the_commands_in_the_body() {
    assert_eq!(
        planned("for f in a b; do cat $f; done > out"),
        vec![("cat".to_string(), vec![redirect(">", "out")])],
    );
}

#[test]
fn a_while_redirect_reaches_the_condition_and_the_body() {
    assert_eq!(
        planned("while read l; do rm $l; done < list"),
        vec![
            ("read".to_string(), vec![redirect("<", "list")]),
            ("rm".to_string(), vec![redirect("<", "list")]),
        ],
    );
}

#[test]
fn a_group_redirect_reaches_a_nested_if() {
    assert_eq!(
        planned("{ if true; then rm x; fi; } > out"),
        vec![
            ("true".to_string(), vec![redirect(">", "out")]),
            ("rm".to_string(), vec![redirect(">", "out")]),
        ],
    );
}

/// The command's own redirect comes first, then the enclosing compound's,
/// innermost first: the order they apply in.
#[test]
fn nested_redirects_reach_the_command_innermost_first() {
    assert_eq!(
        planned("{ { cat a > in1; } > in2; } 2> err"),
        vec![(
            "cat".to_string(),
            vec![redirect(">", "in1"), redirect(">", "in2"), redirect("2>", "err")],
        )],
    );
}

#[test]
fn a_redirected_group_in_a_pipeline_reaches_only_its_own_commands() {
    assert_eq!(
        planned("{ cat a; } > out | wc -l"),
        vec![
            ("cat".to_string(), vec![redirect(">", "out")]),
            ("wc".to_string(), vec![]),
        ],
    );
}

/// A command substitution inside the body is a command too. Its stdout goes
/// to the substitution, but its stderr goes to the compound's `2>`; the plan
/// reports the redirect either way, the safe direction.
#[test]
fn a_compound_redirect_reaches_a_substitution_inside() {
    assert_eq!(
        planned("{ x=$(cat secret); echo \"$x\"; } > out"),
        vec![
            ("cat".to_string(), vec![redirect(">", "out")]),
            ("echo".to_string(), vec![redirect(">", "out")]),
        ],
    );
}

#[test]
fn a_backgrounded_redirected_group_marks_its_commands() {
    let plans = plan_program("{ cat a; } > out &").expect("plans");
    let command = &plans[0].plan.commands[0];
    assert!(command.background, "{command:?}");
    assert_eq!(command.redirects[0].kind, ">");
}

#[rstest]
#[case::group("{ echo a; }", "group", "{ echo a; }")]
#[case::redirected_group("{ echo a; } > out", "redirected", "{ echo a; } > out")]
#[case::redirected_for("for x in a; do echo $x; done 2>&1", "redirected", "for x in a; do echo ${x}; done 2>&1")]
fn statement_kind_and_rendering(#[case] source: &str, #[case] kind: &str, #[case] rendered: &str) {
    let plans = plan_program(source).expect("plans");
    assert_eq!(plans[0].plan.statement_kind, kind, "`{source}`");
    assert_eq!(plans[0].plan.rendered, rendered, "`{source}`");
}

// ---- refusals ---------------------------------------------------------------------

fn validation_codes(error: KernelError) -> Vec<(String, String)> {
    let KernelError::Validation { issues, .. } = error else {
        panic!("must be KernelError::Validation, not {error:?}");
    };
    issues
        .iter()
        .map(|issue| (issue.code.code().to_string(), issue.message.clone()))
        .collect()
}

/// A redirect on a compound applies to the commands inside it. With none,
/// nothing would carry the redirect into a plan, so it is refused.
#[tokio::test]
async fn a_redirect_on_a_compound_with_no_command_is_a_validation_error() {
    let kernel = kernel();
    kernel.execute("echo keep > /out").await.expect("runs");
    let error = kernel.execute("{ x=1; } > /out").await.expect_err("must be refused");
    assert_eq!(
        validation_codes(error),
        vec![(
            "E024".to_string(),
            "`{ x=1; } > /out`: a redirect on a compound statement applies to the commands \
             inside it, and this one has none; put the redirect on a command, e.g. `: > /out`"
                .to_string(),
        )],
    );
    let cat = kernel.execute("cat /out").await.expect("runs");
    assert_eq!(cat.text_out(), "keep\n", "the target must not be opened");
}

/// With the validator off, the runtime refuses the same statement before it
/// opens anything.
#[tokio::test]
async fn a_redirect_on_a_compound_with_no_command_is_refused_at_runtime() {
    let kernel = unvalidated_kernel();
    kernel.execute("echo keep > /out").await.expect("runs");
    let result = kernel.execute("{ x=1; } > /out; echo \"rc=$?\"").await.expect("runs");
    assert_eq!(result.text_out(), "rc=2\n");
    assert!(result.err.contains("put the redirect on a command"), "{result:?}");
    let cat = kernel.execute("cat /out").await.expect("runs");
    assert_eq!(cat.text_out(), "keep\n", "the target must not be opened");
}

/// A here-doc on a compound is refused: pipe it in instead.
#[tokio::test]
async fn a_heredoc_on_a_compound_is_a_validation_error() {
    let kernel = kernel();
    let error = kernel
        .execute("while read l; do echo $l; done <<EOF\na\nEOF\n")
        .await
        .expect_err("must be refused");
    assert_eq!(
        validation_codes(error),
        vec![(
            "E025".to_string(),
            "here-doc `<<EOF` cannot feed a compound statement; pipe it in: \
             `cat <<EOF | while …; done`"
                .to_string(),
        )],
    );
}

#[tokio::test]
async fn a_heredoc_on_a_compound_is_refused_at_runtime() {
    let kernel = unvalidated_kernel();
    let result = kernel
        .execute("{ cat; } <<EOF\na\nEOF\necho \"rc=$?\"")
        .await
        .expect("runs");
    assert_eq!(result.text_out(), "rc=2\n");
    assert!(result.err.contains("here-doc `<<EOF` cannot feed a compound statement"), "{result:?}");
}

/// The piped form the refusal names works.
#[tokio::test]
async fn the_heredoc_fix_runs() {
    let (out, code) = run("cat <<EOF | while read l; do echo \"L:$l\"; done\na\nb\nEOF\n").await;
    assert_eq!(out, "L:a\nL:b\n");
    assert_eq!(code, 0);
}

/// `sort < f > f` is refused on a command; the same holds on a compound.
#[tokio::test]
async fn one_file_as_compound_input_and_output_is_a_validation_error() {
    let kernel = kernel();
    let error = kernel
        .execute("while read l; do echo $l; done < /f > /f")
        .await
        .expect_err("must be refused");
    let codes = validation_codes(error);
    assert_eq!(codes.len(), 1, "{codes:?}");
    assert_eq!(codes[0].0, "E023");
}

#[tokio::test]
async fn one_file_as_compound_input_and_output_is_refused_at_runtime() {
    let kernel = unvalidated_kernel();
    let result = kernel
        .execute("printf 'a\\n' > /f; while read l; do echo $l; done < /f > $(echo /f); cat /f")
        .await
        .expect("runs");
    assert_eq!(result.text_out(), "a\n", "/f keeps its content");
    assert!(result.err.contains("/f is both input and output"), "{result:?}");
}
