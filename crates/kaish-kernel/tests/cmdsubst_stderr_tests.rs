//! A command substitution's stderr reaches the caller.
//!
//! `x=$(cat /nope)` set the right exit code and lost the reason: the
//! `Expr::CommandSubst` arm took the block's stdout and dropped its `err`
//! on the floor. `echo $(cat /nope)` was worse — silent output *and* a
//! zero status, so nothing at all reported the failure.
//!
//! bash gives a substitution the shell's own fd 2, so its stderr is never
//! captured with its stdout. kaish captures everything into an `ExecResult`,
//! so the equivalent is: **a substitution's stderr becomes the enclosing
//! statement's stderr**, and never contaminates the substituted value.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use kaish_kernel::{Kernel, KernelConfig};

async fn setup() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated())
        .expect("failed to create kernel")
        .into_arc()
}

/// The reported repro: the exit code survived, the reason did not.
#[tokio::test]
async fn assignment_substitution_keeps_stderr() {
    let k = setup().await;
    let r = k.execute("x=$(cat /nope)").await.expect("execute");
    assert!(
        r.err.contains("/nope"),
        "the failing command's message must reach the caller, got err: {:?}",
        r.err
    );
}

/// The worse sibling: in argv position nothing reported the failure at all,
/// because an assignment is the only statement that adopts a substitution's
/// exit code.
#[tokio::test]
async fn argv_substitution_keeps_stderr() {
    let k = setup().await;
    let r = k.execute("echo $(cat /nope)").await.expect("execute");
    assert!(
        r.err.contains("/nope"),
        "a substitution in argv position must still report its stderr, got err: {:?}",
        r.err
    );
}

/// The quoted-interpolation path (`StringPart::CommandSubst`) is a second,
/// separate arm — it dropped `err` the same way and needs its own test.
#[tokio::test]
async fn quoted_substitution_keeps_stderr() {
    let k = setup().await;
    let r = k.execute("x=\"prefix $(cat /nope)\"").await.expect("execute");
    assert!(
        r.err.contains("/nope"),
        "the quoted `\"$(…)\"` arm must report stderr too, got err: {:?}",
        r.err
    );
}

/// Nesting composes: the inner substitution's stderr rides out through the
/// block result of the outer one, so it must arrive exactly once.
#[tokio::test]
async fn nested_substitution_stderr_arrives_once() {
    let k = setup().await;
    let r = k.execute("x=$(echo $(cat /nope))").await.expect("execute");
    let hits = r.err.matches("/nope").count();
    assert_eq!(
        hits, 1,
        "nested substitution stderr must arrive exactly once, got {hits} in {:?}",
        r.err
    );
}

/// Two substitutions in one statement: the second must not displace the
/// first. This is the test that catches a "last one wins" accumulator.
#[tokio::test]
async fn sibling_substitutions_both_keep_stderr() {
    let k = setup().await;
    let r = k
        .execute("x=\"$(cat /nope-one)$(cat /nope-two)\"")
        .await
        .expect("execute");
    assert!(
        r.err.contains("/nope-one") && r.err.contains("/nope-two"),
        "both substitutions' stderr must survive, got err: {:?}",
        r.err
    );
}

/// stderr must never contaminate the substituted *value* — that would be a
/// far worse bug than losing it, since it silently corrupts data.
#[tokio::test]
async fn stderr_does_not_leak_into_the_value() {
    let k = setup().await;
    let r = k
        .execute("x=$(cat /nope)\necho \"[$x]\"")
        .await
        .expect("execute");
    assert_eq!(
        r.text_out().trim(),
        "[]",
        "the substituted value must stay empty; stderr belongs on stderr"
    );
}

/// A redirect still suppresses it, exactly as in bash — the stderr is
/// consumed inside the substitution and never reaches the statement.
#[tokio::test]
async fn redirected_substitution_stderr_is_suppressed() {
    let k = setup().await;
    let r = k
        .execute("x=$(cat /nope 2>/dev/null)")
        .await
        .expect("execute");
    assert!(
        !r.err.contains("/nope"),
        "`2>/dev/null` inside the substitution must still suppress, got err: {:?}",
        r.err
    );
}

/// The control: a substitution that succeeds must add no stderr at all.
/// Without this, "always append something" would pass every test above.
#[tokio::test]
async fn successful_substitution_adds_no_stderr() {
    let k = setup().await;
    let r = k.execute("x=$(echo hi)\necho \"[$x]\"").await.expect("execute");
    assert!(
        r.err.is_empty(),
        "a successful substitution must produce no stderr, got: {:?}",
        r.err
    );
    assert_eq!(r.text_out().trim(), "[hi]", "and the value must be unchanged");
}

/// Two substitutions in one statement must not run together into one line.
/// Builtins are inconsistent about a trailing newline — `cat`'s failure
/// message has none — so without a terminator the two messages concatenate
/// and neither is readable.
#[tokio::test]
async fn sibling_substitution_stderr_stays_on_separate_lines() {
    let k = setup().await;
    let r = k
        .execute("x=\"$(cat /nope-one)$(cat /nope-two)\"")
        .await
        .expect("execute");
    let joined_up = r
        .err
        .lines()
        .any(|l| l.contains("/nope-one") && l.contains("/nope-two"));
    assert!(
        !joined_up,
        "the two messages must not share a line, got err: {:?}",
        r.err
    );
}

/// A substitution inside a pipeline stage. Pipeline-stage stderr already
/// travels through the same channel (`run_pipeline` flushes `result.err` into
/// it), so this pins that the two sources coexist without colliding or
/// double-counting.
#[tokio::test]
async fn substitution_in_a_pipeline_stage_keeps_stderr() {
    let k = setup().await;
    let r = k.execute("echo $(cat /nope) | cat").await.expect("execute");
    assert_eq!(
        r.err.matches("/nope").count(),
        1,
        "expected the substitution's stderr exactly once, got err: {:?}",
        r.err
    );
}

/// A substitution in a control-structure head is evaluated before the body
/// runs, so the enclosing statement's drain is the backstop. An empty body
/// removes every other route the stderr could take.
#[tokio::test]
async fn substitution_in_a_for_head_keeps_stderr() {
    let k = setup().await;
    let r = k
        .execute("for i in $(cat /nope); do :; done")
        .await
        .expect("execute");
    assert!(
        r.err.contains("/nope"),
        "a substitution in a `for` head must still report stderr, got err: {:?}",
        r.err
    );
}

/// Several statements inside one substitution: the value comes from stdout
/// only, and a later statement's stderr still escapes. This exercises
/// `execute_block_capturing`'s accumulation across sub-statements rather than
/// the single-statement path every other test here takes.
#[tokio::test]
async fn multi_statement_substitution_keeps_value_and_stderr() {
    let k = setup().await;
    let r = k
        .execute("x=$(echo hi; cat /nope)\necho \"[$x]\"")
        .await
        .expect("execute");
    assert_eq!(
        r.text_out().trim(),
        "[hi]",
        "the value must be the block's stdout alone"
    );
    assert!(
        r.err.contains("/nope"),
        "and the later statement's stderr must still escape, got err: {:?}",
        r.err
    );
}
