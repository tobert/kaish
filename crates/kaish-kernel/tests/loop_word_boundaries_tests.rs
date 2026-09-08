//! Loop items obey the same quote-to-join rule as command arguments.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};
use kaish_kernel::parser::parse;
use rstest::rstest;

#[rstest]
#[case("$(echo foo)/b")]
#[case("prefix$(echo foo)")]
#[case("$a/b")]
#[case("$a$b")]
#[case("$(echo a)$(echo b)c")]
#[case("a\"b\"")]
#[case("'a'\"b\"")]
#[case("$(echo a)$(echo b)")]
#[case("$((1+2))suffix")]
fn adjacent_loop_items_are_rejected_with_their_own_span(#[case] word: &str) {
    let source = format!("for x in {word}; do echo $x; done");
    let errors = parse(&source).expect_err("adjacent fragments must not become iterations");
    let error = errors.iter().find(|e| e.message.contains("for-loop items")).unwrap();
    assert!(error.message.contains("quote the whole word"), "{error:?}");
    assert_eq!(&source[error.span.start..error.span.end], word);
}

#[tokio::test]
async fn rejected_loop_runs_neither_substitution_nor_body() {
    let kernel = Kernel::new(KernelConfig::isolated()).unwrap();
    let result = kernel.execute("for x in $(touch /substitution; echo foo)/b; do touch /body; done").await;
    assert!(result.is_err(), "must refuse before execution: {result:?}");
    for path in ["/substitution", "/body"] {
        let result = kernel.execute(&format!("test -e {path}")).await.unwrap();
        assert_eq!(result.code, 1, "{path} must not have been created");
    }
}

#[rstest]
#[case("for x in \"$(echo foo)/b\"; do echo \"$x\"; done", "foo/b\n")]
#[case("for x in a b; do echo \"$x\"; done", "a\nb\n")]
#[case("for x in $(printf 'a b\\nc d\\n'); do echo \"<$x>\"; done", "<a b>\n<c d>\n")]
#[case("for x in $(fromjson '[1,2]'); do echo \"$x\"; done", "1\n2\n")]
#[case("for x in .git/HEAD 1.0/report; do echo \"$x\"; done", ".git/HEAD\n1.0/report\n")]
#[tokio::test]
async fn separated_items_and_quoted_words_preserve_iteration(#[case] source: &str, #[case] output: &str) {
    let kernel = Kernel::new(KernelConfig::isolated()).unwrap();
    let result = kernel.execute(source).await.unwrap();
    assert!(result.ok(), "{result:?}");
    assert_eq!(result.text_out(), output);
}

#[rstest]
#[case("echo $(for x in $(echo foo)/b; do echo $x; done)")]
#[case("if true; then for x in $(echo foo)/b; do echo $x; done; fi")]
fn nested_loop_error_keeps_absolute_span(#[case] source: &str) {
    let errors = parse(source).unwrap_err();
    let error = errors.iter().find(|e| e.message.contains("for-loop items")).unwrap();
    assert_eq!(&source[error.span.start..error.span.end], "$(echo foo)/b");
}

#[test]
fn separate_adjacent_runs_get_separate_errors() {
    let source = "for x in $(echo a)$(echo b) $(echo c)$(echo d); do echo $x; done";
    let errors = parse(source).unwrap_err();
    let words: Vec<_> = errors.iter().filter(|e| e.message.contains("for-loop items"))
        .map(|e| &source[e.span.start..e.span.end]).collect();
    assert_eq!(words, ["$(echo a)$(echo b)", "$(echo c)$(echo d)"]);
}
