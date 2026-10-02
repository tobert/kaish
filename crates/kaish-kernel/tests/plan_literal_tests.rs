//! `PlannedValue::Literal`: a fully literal argument carries the word the
//! command receives, so an embedder never strips quotes from rendered text.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::plan_program;
use kaish_types::plan::PlannedValue;

fn args_of(source: &str) -> Vec<PlannedValue> {
    let plans = plan_program(source).expect("parses");
    plans[0].plan.commands[0].args.clone()
}

fn lit(text: &str, value: &str) -> PlannedValue {
    PlannedValue::literal(text, value)
}

#[test]
fn a_quoted_numeral_keeps_its_value_without_the_quotes() {
    assert_eq!(args_of(r#"cmd "0""#), vec![lit("'0'", "0")]);
}

#[test]
fn bare_and_quoted_words_round_trip() {
    assert_eq!(
        args_of("cmd x 'a b' \"c d\""),
        vec![lit("x", "x"), lit("'a b'", "a b"), lit("'c d'", "c d")]
    );
}

#[test]
fn numbers_and_booleans_are_what_the_command_receives() {
    assert_eq!(
        args_of("cmd 5 1.5 0.10 -0 true"),
        vec![
            lit("5", "5"),
            lit("1.5", "1.5"),
            lit("0.10", "0.10"),
            lit("-0", "-0"),
            lit("true", "true"),
        ]
    );
}

#[test]
fn an_expansion_anywhere_in_the_word_stays_plain() {
    for source in [
        "cmd ${VAR}",
        "cmd $VAR",
        "cmd $(date)",
        "cmd $((1+1))",
        "cmd *.rs",
        "cmd ~/x",
        r#"cmd "a$x""#,
        "cmd --key=$x",
        "cmd KEY=$x",
        "cmd --key=~/x",
    ] {
        let args = args_of(source);
        assert_eq!(args.len(), 1, "{source}");
        assert!(
            matches!(args[0], PlannedValue::Plain(_)),
            "{source} must stay Plain, got {:?}",
            args[0]
        );
        assert_eq!(args[0].literal_value(), None, "{source}");
    }
}

#[test]
fn flags_and_the_terminator_are_their_own_value() {
    assert_eq!(
        args_of("cmd -n --force -- --after"),
        vec![
            lit("-n", "-n"),
            lit("--force", "--force"),
            lit("--", "--"),
            lit("--after", "--after"),
        ]
    );
}

#[test]
fn a_long_flag_value_is_the_joined_word() {
    assert_eq!(
        args_of(r#"cmd --tail="5" --name=a"#),
        vec![lit("--tail='5'", "--tail=5"), lit("--name=a", "--name=a")]
    );
}

#[test]
fn a_flag_then_a_quoted_number_is_two_literals() {
    assert_eq!(
        args_of(r#"cmd --tail "5""#),
        vec![lit("--tail", "--tail"), lit("'5'", "5")]
    );
}

#[test]
fn a_word_assignment_is_the_joined_word() {
    assert_eq!(args_of("cmd KEY=1"), vec![lit("KEY=1", "KEY=1")]);
    assert_eq!(args_of(r#"cmd KEY="a b""#), vec![lit("KEY='a b'", "KEY=a b")]);
}

#[test]
fn a_literal_redirect_target_is_a_literal_path() {
    let plans = plan_program(r#"cmd > "out 1" 2> err.txt"#).expect("parses");
    let redirects = &plans[0].plan.commands[0].redirects;
    assert_eq!(redirects[0].target, lit("'out 1'", "out 1"));
    assert_eq!(redirects[1].target, lit("err.txt", "err.txt"));
}

#[test]
fn an_expanding_redirect_target_stays_plain() {
    let plans = plan_program("cmd > ${LOG}").expect("parses");
    assert_eq!(
        plans[0].plan.commands[0].redirects[0].target,
        PlannedValue::Plain("${LOG}".to_string())
    );
}

#[test]
fn display_and_literal_value_read_the_two_halves() {
    let value = lit("'0'", "0");
    assert_eq!(value.display(), "'0'");
    assert_eq!(value.literal_value(), Some("0"));
    assert_eq!(PlannedValue::Plain("${X}".into()).literal_value(), None);
}

#[test]
fn the_serialized_form_is_snake_case() {
    let json = serde_json::to_string(&lit("'0'", "0")).expect("serializes");
    assert_eq!(json, r#"{"literal":{"text":"'0'","value":"0"}}"#);
}

#[cfg(feature = "subprocess")]
fn external_kernel(cwd: Option<&std::path::Path>) -> kaish_kernel::Kernel {
    use std::collections::HashMap;

    let mut vars = HashMap::new();
    vars.insert(
        "PATH".to_string(),
        kaish_kernel::ast::Value::String(std::env::var("PATH").expect("PATH is configured")),
    );
    let mut config = kaish_kernel::KernelConfig::repl().with_initial_vars(vars);
    if let Some(cwd) = cwd {
        config = config.with_cwd(cwd.to_path_buf()).with_trash(false);
    }
    kaish_kernel::Kernel::new(config).expect("kernel")
}

/// The plan's `value` for each literal argument is the argv element an
/// external process really receives. The shell script prints every argument
/// after `sh` with a NUL terminator, so a value holding a newline survives.
#[cfg(feature = "subprocess")]
#[tokio::test]
async fn literal_values_match_the_argv_an_external_command_receives() {
    let kernel = external_kernel(None);

    let words = [
        r#"'%s\0'"#,
        "x",
        "'a b'",
        r#""0""#,
        "5",
        "1.5",
        "0.10",
        "-0",
        "true",
        "false",
        "007",
        "0x10",
        "1e3",
        r#""""#,
        "héllo→",
        r#""a\"b""#,
        r#""\\""#,
        r#""\$x""#,
        "'*.rs'",
        "'$x'",
        "'~/x'",
        r#""~/x""#,
        "'a\nb'",
        "-n",
        "--force",
        "--tail=\"5\"",
        "--name=a",
        r#"--key="""#,
        "--tail=0.10",
        "--name=1e3",
        "--zero=-0",
        "KEY=1",
        "KEY=\"a b\"",
        "KEY=0.10",
        "--",
    ];
    let source = format!(
        "/bin/sh -c 'for a; do printf \"%s\\0\" \"$a\"; done' sh {}",
        words.join(" ")
    );
    let plans = kernel.plan_program(&source).expect("parses");
    let planned: Vec<String> = plans[0].plan.commands[0]
        .args
        .iter()
        .skip(3)
        .map(|arg| arg.literal_value().expect("every argument is literal").to_string())
        .collect();
    assert_eq!(planned.len(), words.len());

    let result = kernel.execute(&source).await.expect("runs");
    assert!(result.ok(), "sh failed: {result:?}");
    let out = result.text_out();
    let received: Vec<String> = out
        .strip_suffix('\0')
        .expect("every word ends in NUL")
        .split('\0')
        .map(str::to_string)
        .collect();
    assert_eq!(planned, received);
}

/// A literal redirect target names the file the redirect creates, relative
/// to the working directory.
#[cfg(feature = "subprocess")]
#[rstest::rstest]
#[case::quoted_glob("'*.txt'", "*.txt")]
#[case::negative_zero("-0", "-0")]
#[case::trailing_zero("0.10", "0.10")]
#[case::quoted_space("\"out 1\"", "out 1")]
#[case::quoted_tilde("'~'", "~")]
#[tokio::test]
async fn a_literal_redirect_target_is_the_file_created(#[case] word: &str, #[case] expected: &str) {
    let dir = tempfile::Builder::new()
        .prefix("plan-literal-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir");
    let kernel = external_kernel(Some(dir.path()));
    let source = format!("echo hi > {word}");
    let plans = kernel.plan_program(&source).expect("parses");
    assert_eq!(
        plans[0].plan.commands[0].redirects[0].target.literal_value(),
        Some(expected)
    );

    let result = kernel.execute(&source).await.expect("runs");
    assert!(result.ok(), "redirect failed: {result:?}");
    let names: Vec<String> = std::fs::read_dir(dir.path())
        .expect("readdir")
        .map(|e| e.expect("entry").file_name().to_string_lossy().into_owned())
        .collect();
    assert_eq!(names, vec![expected.to_string()]);
}

#[rstest::rstest]
#[case("cmd '~'", "~")]
#[case("cmd '~/x'", "~/x")]
#[case(r#"cmd "~/x""#, "~/x")]
#[case("cmd --key='~/x'", "--key=~/x")]
#[case("cmd KEY='~/x'", "KEY=~/x")]
#[test]
fn a_quoted_tilde_is_a_known_literal_word(#[case] source: &str, #[case] expected: &str) {
    let args = args_of(source);
    assert_eq!(args.len(), 1);
    assert_eq!(args[0].literal_value(), Some(expected), "{source}");
}
