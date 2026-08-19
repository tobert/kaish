//! A compound statement can feed a pipe: `for … done | cmd`.
//!
//! kaish rejected every compound form in pipeline position with one error,
//! because `for`/`while`/`if`/`case` sit ahead of `pipeline_parser()` in
//! `base_statement` and chumsky's choice is PEG-ordered — the compound parser
//! consumes through `done`, then the `&&`/`||` fold meets `|`:
//!
//! ```text
//! for f in a b; do echo $f; done | wc -l   →  found '|' expected '&&'
//! ```
//!
//! Expectations here are bash's, taken by running each row against bash.
//!
//! Ordering note: this depends on #367 and #368. Built on the pipeline path as
//! it stood before those, a compound stage would have passed a last-stage test
//! and emitted nothing at exit 0 — the feature would have shipped broken in
//! exactly the shape people use.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

use kaish_kernel::{Kernel, KernelConfig};
use rstest::rstest;

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::repl().with_trash(false)).expect("failed to create kernel")
}

/// Every compound form as the *first* stage. One grammar site serves all four,
/// so a fix that reaches only `for` would leave three rows red.
#[rstest]
#[case::for_into_wc("for f in a b; do echo $f; done | wc -l", "2\n")]
#[case::for_into_grep("for f in a b c; do echo $f; done | grep b", "b\n")]
#[case::while_no_iterations("while [[ -n \"\" ]]; do echo x; done | wc -l", "0\n")]
#[case::if_then("if true; then echo x; fi | cat", "x\n")]
#[case::if_else("if false; then echo x; else echo y; fi | cat", "y\n")]
#[case::case_stmt("case a in a) echo hit ;; esac | cat", "hit\n")]
// Two pipes deep: a compound must survive a middle stage as well as a first.
#[case::for_through_two_pipes("for f in a b; do echo $f; done | cat | cat", "a\nb\n")]
#[tokio::test]
async fn a_compound_statement_can_feed_a_pipe(#[case] source: &str, #[case] expected: &str) {
    let kernel = kernel();
    let result = kernel.execute(source).await.expect("execution failed");

    assert_eq!(
        result.text_out(),
        expected,
        "`{source}` (exit {}, stderr {:?})",
        result.code,
        result.err
    );
}

/// A compound in a *later* stage, which bash also allows. Modeling stages
/// uniformly (rather than a special first-position head) is what makes these
/// fall out of the same change instead of being a second feature later.
#[rstest]
#[case::while_consumes_a_pipe(
    "printf \"a\\nb\\n\" | while read l; do echo \"got $l\"; done",
    "got a\ngot b\n"
)]
#[case::if_in_last_stage("echo x | if true; then cat; fi", "x\n")]
#[case::case_in_last_stage("echo hit | case a in a) cat ;; esac", "hit\n")]
// Compound on both ends of the same pipe.
#[case::compound_both_ends(
    "for f in a b; do echo $f; done | while read l; do echo \"L:$l\"; done",
    "L:a\nL:b\n"
)]
#[tokio::test]
async fn a_compound_can_be_a_later_stage(#[case] source: &str, #[case] expected: &str) {
    let kernel = kernel();
    let result = kernel.execute(source).await.expect("execution failed");

    assert_eq!(
        result.text_out(),
        expected,
        "`{source}` (exit {}, stderr {:?})",
        result.code,
        result.err
    );
}

/// A compound stage buffers: its whole output is collected before the next
/// stage sees a byte. bash streams, so `… | head -1` over a long loop exits
/// early there and runs to completion here. Deliberate for now — streaming
/// means plumbing a stage's writer into nested statement execution, which is
/// the shared-slot machinery GH #369 is about. This row pins the *result*, not
/// the timing, so it stays true if streaming lands later.
#[tokio::test]
async fn a_buffered_compound_stage_still_produces_the_right_answer() {
    let kernel = kernel();
    let result = kernel
        .execute("for f in a b c; do echo $f; done | head -n 1")
        .await
        .expect("execution failed");

    assert_eq!(result.text_out(), "a\n");
}

/// A compound stage must not disturb the statement before it. This is the row
/// that fails if the grammar change makes the compound swallow too much.
#[tokio::test]
async fn a_compound_stage_leaves_the_previous_statement_alone() {
    let kernel = kernel();
    let result = kernel
        .execute("echo pre | cat; for f in a; do echo $f; done | cat")
        .await
        .expect("execution failed");

    assert_eq!(result.text_out(), "pre\na\n");
}

/// `case` patterns use `|` as alternation, at a grammar site the pipeline
/// position cannot reach. Widening the pipeline must not make `a|b)` ambiguous.
#[rstest]
#[case::case_alternation("case b in a|b) echo hit ;; esac", "hit\n")]
#[case::case_alternation_into_pipe("case b in a|b) echo hit ;; esac | cat", "hit\n")]
#[tokio::test]
async fn case_alternation_still_parses(#[case] source: &str, #[case] expected: &str) {
    let kernel = kernel();
    let result = kernel.execute(source).await.expect("execution failed");

    assert_eq!(result.text_out(), expected, "`{source}` regressed");
}

/// The controls: a plain pipeline and a bare compound both already worked and
/// must keep working. If either breaks, the grammar change went too wide.
#[rstest]
#[case::plain_pipeline("echo plain | cat", "plain\n")]
#[case::bare_compound("for f in a b; do echo $f; done", "a\nb\n")]
#[case::compound_with_and("for f in a; do echo $f; done && echo after", "a\nafter\n")]
#[tokio::test]
async fn the_forms_that_already_worked_keep_working(
    #[case] source: &str,
    #[case] expected: &str,
) {
    let kernel = kernel();
    let result = kernel.execute(source).await.expect("execution failed");

    assert_eq!(result.text_out(), expected, "`{source}` regressed");
}

/// The pipeline's exit code is its last stage's, whichever kind of stage that
/// is. `for … done | grep zzz` is 1 because `grep` found nothing, not because
/// the loop had an opinion.
#[rstest]
#[case::last_stage_command_succeeds("for f in a; do false; done | cat", 0)]
#[case::last_stage_command_fails("for f in a b; do echo $f; done | grep zzz", 1)]
#[case::last_stage_loop_takes_its_last_command("echo x | for f in a; do false; done", 1)]
#[case::last_stage_if_with_no_branch_taken("echo x | if false; then echo a; fi", 0)]
#[tokio::test]
async fn the_last_stage_sets_the_exit_code(#[case] source: &str, #[case] expected: i64) {
    let kernel = kernel();
    let result = kernel.execute(source).await.expect("execution failed");

    assert_eq!(result.code, expected, "`{source}` (stderr {:?})", result.err);
}

/// A non-last stage's session changes stay in that stage, compound or not.
/// bash runs every stage in a subshell; kaish syncs only the last stage back.
#[tokio::test]
async fn a_cd_in_a_non_last_compound_stage_does_not_leak() {
    let kernel = kernel();
    let before = kernel.cwd().await;
    kernel
        .execute("for f in a; do cd /; done | cat")
        .await
        .expect("execution failed");

    assert_eq!(kernel.cwd().await, before);
}

/// A compound stage nests: the inner loop's output reaches the pipe through
/// the outer one.
#[tokio::test]
async fn a_compound_stage_nests() {
    let kernel = kernel();
    let result = kernel
        .execute("for f in a b; do for g in 1 2; do echo \"$f$g\"; done; done | cat")
        .await
        .expect("execution failed");

    assert_eq!(result.text_out(), "a1\na2\nb1\nb2\n");
}

/// Scatter splits a pipeline across workers that run plain commands, so a
/// compound stage in the same pipeline has nowhere to run. Refuse it by name
/// rather than dropping the parallelism quietly.
#[tokio::test]
async fn scatter_gather_refuses_a_compound_stage_by_name() {
    let kernel = kernel();
    let result = kernel
        .execute("for f in a b; do echo $f; done | scatter | echo x | gather")
        .await
        .expect("execution failed");

    assert_eq!(result.code, 2, "stderr {:?}", result.err);
    assert!(
        result.err.contains("scatter/gather cannot share a pipeline"),
        "the error must name the condition, got {:?}",
        result.err
    );
}

/// The plan sees every command inside a compound stage. An embedder that gates
/// on `rm` must still see the `rm` in a loop body that feeds a pipe — this is
/// the row that fails if a compound stage becomes opaque to `plan_program`.
#[rstest]
#[case::first_stage("for f in a b; do rm $f; done | wc -l", vec!["rm", "wc"])]
#[case::last_stage("echo x | while read l; do rm $l; done", vec!["echo", "read", "rm"])]
#[test]
fn a_compound_stage_is_not_opaque_to_the_plan(#[case] source: &str, #[case] expected: Vec<&str>) {
    let plans = kaish_kernel::plan_program(source).expect("parses");
    let names: Vec<&str> = plans[0]
        .plan
        .commands
        .iter()
        .map(|c| c.name.as_str())
        .collect();

    assert_eq!(names, expected, "`{source}`");
}

/// A backgrounded compound is the one route that reaches `run_single` with a
/// compound stage — everywhere else `pipeline_into_stmt` unwraps a lone
/// compound back to a bare statement, and `&` is what keeps it a `Pipeline`.
/// So this pair covers a code path the foreground rows never touch.
#[rstest]
#[case::bare_compound_backgrounded("for f in a b; do echo $f; done", "a\nb\n")]
#[case::compound_into_pipe_backgrounded("for f in a b; do echo $f; done | wc -l", "2\n")]
#[tokio::test]
async fn a_backgrounded_compound_still_produces_its_output(
    #[case] body: &str,
    #[case] expected: &str,
) {
    let kernel = kernel();
    let announcement = kernel
        .execute(&format!("{body} &"))
        .await
        .expect("execution failed");
    assert!(
        announcement.err.contains("[1]"),
        "expected a job announcement, got {:?}",
        announcement.err
    );

    // Poll rather than sleep: the job is done when its status says so.
    let deadline = std::time::Instant::now() + std::time::Duration::from_secs(10);
    loop {
        let status = kernel
            .execute("cat /v/jobs/1/status")
            .await
            .expect("status check failed");
        if status.text_out().trim().starts_with("done:") {
            break;
        }
        assert!(std::time::Instant::now() < deadline, "job 1 never finished");
        tokio::time::sleep(std::time::Duration::from_millis(10)).await;
    }

    let out = kernel
        .execute("cat /v/jobs/1/stdout")
        .await
        .expect("reading job stdout failed");
    assert_eq!(out.text_out(), expected, "`{body} &`");
}

/// A compound in the *true middle* — piped stdin AND piped stdout on the same
/// stage. The earlier rows put a compound at index 0 or at the last index; this
/// is the only position where the runner wires both ends of one stage.
#[tokio::test]
async fn a_compound_can_be_a_middle_stage() {
    let kernel = kernel();
    let result = kernel
        .execute("printf \"a\\nb\\n\" | while read l; do echo \"got $l\"; done | cat")
        .await
        .expect("execution failed");

    assert_eq!(result.text_out(), "got a\ngot b\n");
}

/// Structured data reaching a compound stage. `seq` carries a JSON array on
/// `.data` alongside its text, and the sideband is a oneshot the stage hands
/// down — the exact machinery #368 was about. Text-only rows would not catch a
/// regression in it.
#[rstest]
#[case::structured_into_a_compound("seq 1 3 | while read l; do echo \"n=$l\"; done", "n=1\nn=2\nn=3\n")]
#[case::structured_through_a_compound("seq 1 3 | while read l; do echo $l; done | wc -l", "3\n")]
#[tokio::test]
async fn structured_data_reaches_a_compound_stage(#[case] source: &str, #[case] expected: &str) {
    let kernel = kernel();
    let result = kernel.execute(source).await.expect("execution failed");

    assert_eq!(result.text_out(), expected, "`{source}`");
}

/// Control flow stops at the stage boundary — bash draws the same line with a
/// subshell. Output produced before the signal still reaches the pipe.
#[tokio::test]
async fn continue_inside_a_compound_stage_skips_only_that_iteration() {
    let kernel = kernel();
    let result = kernel
        .execute("for f in a b c; do if [[ $f == b ]]; then continue; fi; echo $f; done | cat")
        .await
        .expect("execution failed");

    assert_eq!(result.text_out(), "a\nc\n");
}

/// `exit` in a non-last stage stops that stage, not the script. In the last
/// stage its code becomes the pipeline's, because the last stage always sets it.
#[rstest]
#[case::exit_in_a_non_last_stage("for f in a b; do exit 7; done | cat; echo after=$?", "after=0\n")]
#[case::exit_in_the_last_stage("echo x | for f in a; do exit 7; done; echo after=$?", "after=7\n")]
#[tokio::test]
async fn exit_inside_a_compound_stage_stops_at_the_stage(
    #[case] source: &str,
    #[case] expected: &str,
) {
    let kernel = kernel();
    let result = kernel.execute(source).await.expect("execution failed");

    assert_eq!(result.text_out(), expected, "`{source}`");
}

/// `return` in a compound stage returns from the STAGE, not from the enclosing
/// function — so the statement after the pipeline still runs. This is the row
/// that fails if the stage boundary ever starts leaking control flow outward.
#[tokio::test]
async fn return_inside_a_compound_stage_does_not_return_from_the_function() {
    let kernel = kernel();
    let result = kernel
        .execute("f() { for x in 1 2; do echo $x; return; done | cat; echo after; }; f")
        .await
        .expect("execution failed");

    assert_eq!(result.text_out(), "1\nafter\n");
}

/// The direct witnesses that the buffering seam holds. Each of these re-enters
/// the interpreter from inside a compound stage — the paths that would steal
/// the stage's pipe writer if it had been handed down instead of kept with the
/// runner. A stolen writer shows up as missing output, so an empty result here
/// is the failure.
#[rstest]
#[case::timeout_redispatch("for f in a b; do timeout 5 echo $f; done | cat", "a\nb\n")]
#[case::user_tool_body("g() { echo \"g:$1\"; }; for f in a b; do g $f; done | cat", "g:a\ng:b\n")]
#[case::nested_pipeline("for f in a b; do echo $f | cat; done | cat", "a\nb\n")]
#[case::command_substitution("for f in a b; do echo $(echo $f); done | cat", "a\nb\n")]
#[tokio::test]
async fn a_nested_dispatch_inside_a_compound_stage_cannot_steal_the_pipe(
    #[case] source: &str,
    #[case] expected: &str,
) {
    let kernel = kernel();
    let result = kernel.execute(source).await.expect("execution failed");

    assert_eq!(result.text_out(), expected, "`{source}`");
}

/// `source` is the fourth re-entry path, and the one that needs a real file.
/// It re-runs a whole statement list inside the compound stage, so it is the
/// closest analogue to the shape that broke in #367.
#[tokio::test]
async fn a_sourced_script_inside_a_compound_stage_cannot_steal_the_pipe() {
    let dir = tempfile::tempdir().expect("tempdir");
    let kernel = Kernel::new(
        KernelConfig::repl()
            .with_cwd(dir.path().to_path_buf())
            .with_trash(false),
    )
    .expect("failed to create kernel");

    kernel
        .execute("echo 'echo from-script' > inner.kai")
        .await
        .expect("writing the fixture failed");
    let result = kernel
        .execute("for f in a b; do source inner.kai; done | cat")
        .await
        .expect("execution failed");

    assert_eq!(result.text_out(), "from-script\nfrom-script\n");
}
