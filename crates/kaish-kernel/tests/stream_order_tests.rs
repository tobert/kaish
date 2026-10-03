//! `ExecResult::stream_order` records the order stdout and stderr were
//! produced, while the payloads keep their two blocks. Merges (`2>&1`,
//! `1>&2`, `&>`) still join the blocks stdout first; see
//! redirect_merge_order_tests.rs.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

mod common;

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::{ExecuteOptions, Kernel, KernelConfig};
use kaish_types::StreamKind;

const BOTH: &str = "both() { echo out; echo err >&2; echo out2; echo err2 >&2; }\n";

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::transient()).expect("kernel")
}

/// What a terminal shows when both streams write to it.
fn ordered(result: &ExecResult) -> String {
    let bytes: Vec<u8> = result.chunks().iter().flat_map(|chunk| chunk.bytes.iter().copied()).collect();
    String::from_utf8(bytes).unwrap()
}

fn streams(result: &ExecResult) -> Vec<StreamKind> {
    result.stream_order().expect("result has a stream order").iter().map(|span| span.stream).collect()
}

async fn run(script: &str) -> ExecResult {
    kernel().execute(script).await.expect("runs")
}

#[tokio::test]
async fn a_function_records_its_statement_order() {
    let result = run(&format!("{BOTH}both")).await;
    assert_eq!(ordered(&result), "out\nerr\nout2\nerr2\n");
    // The payloads are the same two blocks as before.
    assert_eq!(result.text_out(), "out\nout2\n");
    assert_eq!(result.err, "err\nerr2\n");
    use StreamKind::{Stderr, Stdout};
    assert_eq!(streams(&result), vec![Stdout, Stderr, Stdout, Stderr]);
}

#[tokio::test]
async fn sequence_numbers_increase_down_the_list() {
    let result = run(&format!("{BOTH}both; both")).await;
    let spans = result.stream_order().unwrap();
    assert_eq!(spans.len(), 8);
    assert!(spans.windows(2).all(|pair| pair[0].seq < pair[1].seq), "{spans:?}");
}

#[tokio::test]
async fn top_level_statements_record_their_order() {
    let result = run("echo a; echo b >&2; echo c").await;
    assert_eq!(ordered(&result), "a\nb\nc\n");
}

#[tokio::test]
async fn a_for_loop_records_each_iteration() {
    let result = run("for i in 1 2; do echo \"out$i\"; echo \"err$i\" >&2; done").await;
    assert_eq!(ordered(&result), "out1\nerr1\nout2\nerr2\n");
}

#[tokio::test]
async fn a_while_loop_records_each_iteration() {
    let result = run("i=0; while [[ $i -lt 2 ]]; do echo \"out$i\"; echo \"err$i\" >&2; i=$((i + 1)); done").await;
    assert_eq!(ordered(&result), "out0\nerr0\nout1\nerr1\n");
}

#[tokio::test]
async fn binary_condition_output_keeps_its_place() {
    let result = run(
        "i=0; c() { [[ $i -lt 2 ]] || return 1; echo /w== | base64 -d; }; \
         while c; do echo out; echo err >&2; i=$((i + 1)); done",
    )
    .await;
    assert!(result.out_bytes().is_some(), "stdout is binary: {result:?}");
    let bytes: Vec<u8> = result.chunks().iter().flat_map(|chunk| chunk.bytes.iter().copied()).collect();
    assert_eq!(bytes, b"\xffout\nerr\n\xffout\nerr\n".to_vec());
}

#[tokio::test]
async fn a_condition_is_its_stdout_then_its_stderr() {
    let kernel = kernel();
    kernel.execute("echo good > /good").await.unwrap();
    let result = kernel.execute("if cat /good /nosuch; then echo y; else echo n; fi").await.unwrap();
    let text = ordered(&result);
    assert!(text.starts_with("good\n"), "{text:?}");
    assert!(text.ends_with("n\n"), "{text:?}");
}

#[tokio::test]
async fn a_brace_group_records_its_statement_order() {
    let result = run("{ echo a; echo b >&2; echo c; }").await;
    assert_eq!(ordered(&result), "a\nb\nc\n");
}

#[tokio::test]
async fn an_if_body_records_its_statement_order() {
    let result = run("if true; then echo a; echo b >&2; echo c; fi").await;
    assert_eq!(ordered(&result), "a\nb\nc\n");
}

#[tokio::test]
async fn a_chain_records_its_operand_order() {
    let result = run("echo a && echo b >&2 && echo c").await;
    assert_eq!(ordered(&result), "a\nb\nc\n");
    let result = run("false || echo b >&2 || true; echo c").await;
    assert_eq!(ordered(&result), "b\nc\n");
}

#[tokio::test]
async fn stderr_from_an_earlier_pipeline_stage_keeps_its_place() {
    let result = run("f() { echo a; cat /nosuch | cat; echo b; }; f").await;
    let text = ordered(&result);
    let error = text.find("nosuch").expect("cat's error is in the output");
    assert!(text.starts_with("a\n"), "{text:?}");
    assert!(text.ends_with("b\n"), "{text:?}");
    assert!(error > 0 && error < text.len() - 2, "{text:?}");
}

#[tokio::test]
async fn stderr_from_a_substitution_keeps_its_place() {
    let result = run("f() { echo a; x=$(cat /nosuch); echo b; }; f").await;
    let text = ordered(&result);
    assert!(text.starts_with("a\n"), "{text:?}");
    assert!(text.ends_with("b\n"), "{text:?}");
    assert!(text.contains("nosuch"), "{text:?}");
}

#[tokio::test]
async fn stderr_from_an_argument_substitution_precedes_the_command() {
    // The substitution runs, and fails, before echo prints.
    let result = run("echo \"x$(cat /nosuch)\"").await;
    let text = ordered(&result);
    assert!(text.starts_with("cat: "), "{text:?}");
    assert!(text.ends_with("x\n"), "{text:?}");
}

#[tokio::test]
async fn an_argument_substitution_in_a_function_precedes_its_command() {
    let result = run("f() { echo a; echo \"x$(cat /nosuch)\"; echo b; }; f").await;
    let text = ordered(&result);
    let error = text.find("cat: ").expect("cat's error is in the output");
    assert!(text.starts_with("a\n"), "{text:?}");
    assert!(text[error..].contains("\nx\nb\n"), "the error precedes x: {text:?}");
}

#[tokio::test]
async fn an_argument_substitution_in_a_loop_precedes_its_command() {
    let result = run("for i in 1 2; do echo \"x$i$(cat /nosuch)\"; done").await;
    let text = ordered(&result);
    let first_error = text.find("cat: ").expect("cat's error is in the output");
    let x1 = text.find("x1").unwrap();
    let second_error = text.rfind("cat: ").unwrap();
    let x2 = text.find("x2").unwrap();
    assert!(first_error < x1 && x1 < second_error && second_error < x2, "{text:?}");
}

#[tokio::test]
async fn a_single_builtin_is_its_stdout_then_its_stderr() {
    // One command returns its two streams whole: no order inside them.
    let kernel = kernel();
    kernel.execute("echo good > /good").await.unwrap();
    let result = kernel.execute("cat /good /nosuch").await.unwrap();
    use StreamKind::{Stderr, Stdout};
    assert_eq!(streams(&result), vec![Stdout, Stderr]);
}

#[tokio::test]
async fn the_last_pipeline_stage_is_its_stdout_then_its_stderr() {
    let kernel = kernel();
    kernel.execute("echo good > /good").await.unwrap();
    let result = kernel.execute("echo x | cat /good /nosuch").await.unwrap();
    let text = ordered(&result);
    assert!(text.starts_with("good\n"), "{text:?}");
    assert!(text.contains("nosuch"), "{text:?}");
}

#[tokio::test]
async fn a_background_job_records_its_order() {
    let kernel = kernel();
    kernel.execute("echo \"x$(cat /nosuch)\" &").await.unwrap();
    let id = kaish_kernel::scheduler::JobId(1);
    let result = kernel.jobs().wait(id).await.unwrap();
    assert!(result.stream_order().is_some(), "{result:?}");
    let text = ordered(&result);
    assert!(text.starts_with("cat: "), "{text:?}");
    assert!(text.ends_with("x\n"), "{text:?}");
}

/// The merge is unchanged: stdout block, then stderr block. The unmerged
/// result records the order a later merge can follow.
#[tokio::test]
async fn a_merge_still_joins_two_blocks() {
    let merged = run(&format!("{BOTH}both 2>&1")).await;
    assert_eq!(merged.text_out(), "out\nout2\nerr\nerr2\n");
    assert_eq!(merged.err, "");
    assert_eq!(ordered(&merged), "out\nout2\nerr\nerr2\n");

    let merged = run(&format!("{BOTH}both 1>&2")).await;
    assert_eq!(merged.err, "out\nout2\nerr\nerr2\n");
    assert_eq!(ordered(&merged), "out\nout2\nerr\nerr2\n");

    let merged = run("for i in 1 2; do echo \"out$i\"; echo \"err$i\" >&2; done 2>&1").await;
    assert_eq!(ordered(&merged), "out1\nout2\nerr1\nerr2\n");
}

#[tokio::test]
async fn a_redirect_to_a_file_keeps_the_other_stream_in_order() {
    let result = run(&format!("{BOTH}both > /dev/null; echo after")).await;
    assert_eq!(ordered(&result), "err\nerr2\nafter\n");
    let result = run(&format!("{BOTH}both 2> /dev/null; echo after >&2")).await;
    assert_eq!(ordered(&result), "out\nout2\nafter\n");
}

#[tokio::test]
async fn the_streaming_callback_sees_each_statement_in_order() {
    let kernel = kernel();
    let mut seen = Vec::new();
    let mut on_output = |result: &ExecResult| seen.push(ordered(result));
    kernel
        .execute_with_options_streaming(&format!("{BOTH}both; echo last"), ExecuteOptions::new(), &mut on_output)
        .await
        .unwrap();
    assert!(seen.contains(&"out\nerr\nout2\nerr2\n".to_string()), "{seen:?}");
}

#[cfg(feature = "subprocess")]
mod external {
    use super::*;

    fn kernel(dir: &tempfile::TempDir) -> Kernel {
        let variables = std::collections::HashMap::from([(
            "PATH".to_string(),
            kaish_kernel::ast::Value::String(std::env::var("PATH").expect("PATH")),
        )]);
        Kernel::new(
            KernelConfig::repl()
                .with_cwd(dir.path().to_path_buf())
                .with_initial_vars(variables),
        )
        .unwrap()
    }

    /// Pauses let each write reach kaish before the next one, so read
    /// order is write order.
    #[tokio::test]
    async fn an_external_command_is_ordered_by_read_time() {
        let dir = tempfile::tempdir().unwrap();
        let result = kernel(&dir)
            .execute("sh -c 'echo out; sleep 0.2; echo err >&2; sleep 0.2; echo out2'")
            .await
            .unwrap();
        assert_eq!(result.text_out(), "out\nout2\n");
        assert_eq!(result.err, "err\n");
        assert_eq!(ordered(&result), "out\nerr\nout2\n");
    }

    #[tokio::test]
    async fn the_last_pipeline_stage_keeps_its_read_order() {
        let dir = tempfile::tempdir().unwrap();
        let result = kernel(&dir)
            .execute("echo x | sh -c 'echo out; sleep 0.2; echo err >&2; sleep 0.2; echo out2'")
            .await
            .unwrap();
        assert_eq!(ordered(&result), "out\nerr\nout2\n");
    }

    #[tokio::test]
    async fn stderr_on_both_sides_of_stdout_keeps_its_place_in_a_pipeline() {
        let dir = tempfile::tempdir().unwrap();
        let result = kernel(&dir)
            .execute("echo x | sh -c 'echo err >&2; sleep 0.2; echo out; sleep 0.2; echo err2 >&2'")
            .await
            .unwrap();
        assert_eq!(ordered(&result), "err\nout\nerr2\n");
    }

    #[tokio::test]
    async fn a_pipeline_in_a_function_keeps_its_place_and_its_order() {
        let dir = tempfile::tempdir().unwrap();
        let result = kernel(&dir)
            .execute("f() { echo a; echo x | sh -c 'echo out; sleep 0.2; echo err >&2'; echo b; }; f")
            .await
            .unwrap();
        assert_eq!(ordered(&result), "a\nout\nerr\nb\n");
    }

    #[tokio::test]
    async fn a_pipeline_in_a_loop_keeps_its_order() {
        let dir = tempfile::tempdir().unwrap();
        let result = kernel(&dir)
            .execute("for i in 1 2; do echo x | sh -c 'echo out; sleep 0.2; echo err >&2'; done")
            .await
            .unwrap();
        assert_eq!(ordered(&result), "out\nerr\nout\nerr\n");
    }

    #[tokio::test]
    async fn a_timeout_note_leads_the_order_as_it_leads_stderr() {
        let dir = tempfile::tempdir().unwrap();
        let result = kernel(&dir)
            .into_arc()
            .execute("timeout 1 sh -c 'echo err >&2; sleep 0.2; echo out; sleep 5'")
            .await
            .unwrap();
        assert_eq!(result.text_out(), "out\n", "{result:?}");
        assert!(result.err.starts_with("timeout: timed out"), "{:?}", result.err);
        let text = ordered(&result);
        assert!(text.starts_with("timeout: timed out"), "{text:?}");
        assert!(text.ends_with("err\nout\n"), "the child's own order holds: {text:?}");
    }
}
