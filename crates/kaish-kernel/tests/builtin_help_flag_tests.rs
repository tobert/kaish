//! `NAME --help` prints the builtin's help to stdout and exits 0, for
//! every argv binder: typed (`diff`), raw (`kill`, `test`), verbatim (`find`),
//! and tools that render their own output (`gather`, `scatter`).

#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use common::kernel_at;
use rstest::rstest;
use tempfile::tempdir;

#[rstest]
#[case::typed("ls", "List directory contents")]
#[case::typed_with_operand_check("diff", "Compare files line by line")]
#[case::verbatim("find", "Search for files")]
#[case::raw_argv_signal("kill", "Send a signal")]
#[case::raw_argv_expression("test", "Evaluate a conditional expression")]
#[case::owns_output_gather("gather", "Collect results")]
#[case::owns_output_scatter("scatter", "Fan out")]
#[tokio::test]
async fn help_flag_prints_help_and_exits_zero(#[case] name: &str, #[case] about: &str) {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute(&format!("{name} --help")).await.unwrap();
    assert_eq!(result.code, 0, "{name} --help: {}", result.err);
    assert!(result.text_out().contains(about), "{name} --help printed: {:?}", result.text_out());
    assert!(result.err.is_empty(), "{name} --help wrote stderr: {}", result.err);
}

#[tokio::test]
async fn help_flag_text_matches_help_command_for_a_typed_tool() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let flag = kernel.execute("diff --help").await.unwrap();
    let topic = kernel.execute("help diff").await.unwrap();
    assert_eq!(flag.text_out(), topic.text_out());
}

#[tokio::test]
async fn help_word_after_an_expression_operand_is_still_data_for_test() {
    // `test x = --help` compares strings; only a leading --help asks for help.
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("test x = --help").await.unwrap();
    assert_eq!(result.code, 1, "{}", result.err);
    assert_eq!(result.text_out(), "");
}
