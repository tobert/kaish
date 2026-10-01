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
        r#"cmd "~/x""#,
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

/// The plan's `value` for each literal argument is the argv element an
/// external process really receives.
#[cfg(feature = "subprocess")]
#[tokio::test]
async fn literal_values_match_the_argv_an_external_command_receives() {
    use std::collections::HashMap;

    let mut vars = HashMap::new();
    vars.insert(
        "PATH".to_string(),
        kaish_kernel::ast::Value::String(std::env::var("PATH").unwrap_or_default()),
    );
    let kernel = kaish_kernel::Kernel::new(
        kaish_kernel::KernelConfig::repl().with_initial_vars(vars),
    )
    .expect("kernel");

    let source = r#"/usr/bin/printf '%s\n' x 'a b' "0" 5 1.5 0.10 -0 true -n --force --tail="5" --name=a KEY=1 KEY="a b""#;
    let plans = kernel.plan_program(source).expect("parses");
    let planned: Vec<String> = plans[0].plan.commands[0]
        .args
        .iter()
        .skip(1)
        .map(|arg| arg.literal_value().expect("every argument is literal").to_string())
        .collect();

    let result = kernel.execute(source).await.expect("runs");
    assert!(result.ok(), "printf failed: {result:?}");
    let received: Vec<String> = result.text_out().lines().map(str::to_string).collect();
    assert_eq!(planned, received);
}
