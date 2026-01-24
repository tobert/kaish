//! Parser tests using rstest for parameterization and insta for snapshots.
//!
//! These tests replace the custom statements.test file format with native Rust tests.

use insta::assert_snapshot;
use kaish_kernel::ast::sexpr::format_program;
use kaish_kernel::parser::parse;
use rstest::rstest;

/// Run a parser test that expects successful parsing and compare with snapshot.
fn parse_and_snapshot(name: &str, input: &str) {
    let program = parse(input).unwrap_or_else(|errors| {
        let error_msg = errors
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("; ");
        panic!("Parse error for '{}': {}", name, error_msg);
    });
    let sexpr = format_program(&program);
    assert_snapshot!(name, sexpr);
}

/// Run a parser test that expects a parse error.
fn expect_parse_error(input: &str) {
    let result = parse(input);
    assert!(result.is_err(), "Expected error for input: {:?}", input);
}

// =============================================================================
// ASSIGNMENTS
// =============================================================================

#[test]
fn parser_assign_int() {
    parse_and_snapshot("assign_int", "set X = 5");
}

#[test]
fn parser_assign_negative_int() {
    parse_and_snapshot("assign_negative_int", "set X = -42");
}

#[test]
fn parser_assign_float() {
    parse_and_snapshot("assign_float", "set PI = 3.14159");
}

#[test]
fn parser_assign_bool_true() {
    parse_and_snapshot("assign_bool_true", "set FLAG = true");
}

#[test]
fn parser_assign_bool_false() {
    parse_and_snapshot("assign_bool_false", "set FLAG = false");
}

#[test]
fn parser_assign_string() {
    parse_and_snapshot("assign_string", r#"set NAME = "alice""#);
}

#[test]
fn parser_assign_string_with_spaces() {
    parse_and_snapshot("assign_string_with_spaces", r#"set MSG = "hello world""#);
}

#[test]
fn parser_assign_string_with_escapes() {
    parse_and_snapshot("assign_string_with_escapes", r#"set MSG = "line\nbreak""#);
}

#[test]
fn parser_assign_varref() {
    parse_and_snapshot("assign_varref", "set Y = ${X}");
}

#[test]
fn parser_assign_simple_varref() {
    parse_and_snapshot("assign_simple_varref", "set Y = $X");
}

#[test]
fn parser_assign_interpolated() {
    parse_and_snapshot("assign_interpolated", r#"set MSG = "hello ${NAME}""#);
}

#[test]
fn parser_assign_interpolated_simple() {
    parse_and_snapshot("assign_interpolated_simple", r#"set MSG = "hello $NAME""#);
}

#[test]
fn parser_assign_single_quoted() {
    parse_and_snapshot("assign_single_quoted", "set MSG = 'hello $NAME'");
}

#[test]
fn parser_bash_assign_int() {
    parse_and_snapshot("bash_assign_int", "X=5");
}

#[test]
fn parser_bash_assign_string() {
    parse_and_snapshot("bash_assign_string", r#"NAME="alice""#);
}

#[test]
fn parser_local_assign_int() {
    parse_and_snapshot("local_assign_int", "local X = 5");
}

#[test]
fn parser_local_assign_string() {
    parse_and_snapshot("local_assign_string", r#"local MSG = "hello""#);
}

// =============================================================================
// COMMANDS
// =============================================================================

#[test]
fn parser_cmd_simple() {
    parse_and_snapshot("cmd_simple", "echo");
}

#[test]
fn parser_cmd_positional_string() {
    parse_and_snapshot("cmd_positional_string", r#"echo "hello""#);
}

#[test]
fn parser_cmd_positional_multiple() {
    parse_and_snapshot("cmd_positional_multiple", r#"echo "hello" "world""#);
}

#[test]
fn parser_cmd_named_int() {
    parse_and_snapshot("cmd_named_int", "fetch count=10");
}

#[test]
fn parser_cmd_named_string() {
    parse_and_snapshot("cmd_named_string", r#"search query="rust""#);
}

#[test]
fn parser_cmd_named_multiple() {
    parse_and_snapshot("cmd_named_multiple", r#"api endpoint="/users" limit=50 verbose=true"#);
}

#[test]
fn parser_cmd_mixed_args() {
    parse_and_snapshot("cmd_mixed_args", r#"grep "pattern" path="/src" context=3"#);
}

// =============================================================================
// PIPELINES
// =============================================================================

#[test]
fn parser_pipe_two() {
    parse_and_snapshot("pipe_two", "a | b");
}

#[test]
fn parser_pipe_background() {
    parse_and_snapshot("pipe_background", "slow-task &");
}

#[test]
fn parser_pipe_chain_background() {
    parse_and_snapshot("pipe_chain_background", "a | b | c &");
}

#[test]
fn parser_pipe_three() {
    parse_and_snapshot("pipe_three", r#"cat file | grep "pattern" | head 10"#);
}

// =============================================================================
// REDIRECTS (Known failures - slash in paths needs path token type)
// =============================================================================

#[rstest]
#[case::redirect_stdout(r#"echo "hello" > /tmp/out"#)]
#[case::redirect_append(r#"echo "more" >> /tmp/out"#)]
#[case::redirect_stdin("wc < /tmp/input")]
#[case::redirect_stderr("risky-cmd 2> /tmp/err")]
#[case::redirect_both("cmd &> /tmp/all")]
#[case::redirect_multiple("cmd < /in > /out 2> /err")]
#[case::redirect_in_pipeline("a | b > /out")]
fn parser_redirects(#[case] input: &str) {
    let name = format!("redirect_{}", input.chars().take(20).filter(|c| c.is_alphanumeric()).collect::<String>());
    parse_and_snapshot(&name, input);
}

// =============================================================================
// CONTROL FLOW
// =============================================================================

#[test]
fn parser_if_simple() {
    parse_and_snapshot("if_simple", "if ${?.ok}; then\n    echo \"yes\"\nfi");
}

#[test]
fn parser_if_else() {
    parse_and_snapshot("if_else", "if ${?.ok}; then\n    echo \"yes\"\nelse\n    echo \"no\"\nfi");
}

#[test]
#[ignore = "parses identifier as string instead of command"]
fn parser_if_command_condition() {
    parse_and_snapshot("if_command_condition", "if test-something; then\n    echo \"passed\"\nfi");
}

#[test]
#[ignore = "parenthesized conditions not parsed"]
fn parser_if_comparison() {
    parse_and_snapshot("if_comparison", "if (${X} > 5); then\n    echo \"big\"\nfi");
}

#[test]
fn parser_for_simple() {
    parse_and_snapshot("for_simple", "for X in ${LIST}; do\n    echo ${X}\ndone");
}


#[test]
fn parser_and_chain() {
    parse_and_snapshot("and_chain", "a && b && c");
}

#[test]
fn parser_or_chain() {
    parse_and_snapshot("or_chain", "a || b || c");
}

#[test]
fn parser_mixed_chain() {
    parse_and_snapshot("mixed_chain", "a && b || c");
}

// =============================================================================
// TOOL DEFINITIONS
// =============================================================================

#[test]
fn parser_tool_minimal() {
    parse_and_snapshot("tool_minimal", "tool noop {\n}");
}

#[test]
fn parser_tool_one_param() {
    parse_and_snapshot("tool_one_param", "tool greet name:string {\n    echo \"hello ${name}\"\n}");
}

#[test]
fn parser_tool_multiple_params() {
    parse_and_snapshot("tool_multiple_params", "tool fetch url:string timeout:int retries:int {\n    http-get url=${url} timeout=${timeout}\n}");
}

#[test]
fn parser_tool_default_values() {
    parse_and_snapshot("tool_default_values", "tool search query:string limit:int=10 offset:int=0 {\n    db-query q=${query} l=${limit} o=${offset}\n}");
}


// =============================================================================
// SCATTER/GATHER
// =============================================================================

#[test]
fn parser_scatter_basic() {
    parse_and_snapshot("scatter_basic", "cat input | scatter | process | gather");
}

#[test]
fn parser_scatter_with_as() {
    parse_and_snapshot("scatter_with_as", "cat input | scatter as=ITEM | process ${ITEM} | gather");
}

#[test]
fn parser_scatter_with_limit() {
    parse_and_snapshot("scatter_with_limit", "cat input | scatter as=X limit=4 | process ${X} | gather");
}

#[test]
fn parser_gather_with_options() {
    parse_and_snapshot("gather_with_options", r#"cat input | scatter | process | gather progress=true errors="/tmp/err""#);
}

// =============================================================================
// TEST EXPRESSIONS [[ ]]
// =============================================================================

#[test]
fn parser_test_string_empty() {
    parse_and_snapshot("test_string_empty", "[[ -z $VAR ]]");
}

#[test]
fn parser_test_string_nonempty() {
    parse_and_snapshot("test_string_nonempty", "[[ -n $VAR ]]");
}

#[test]
fn parser_test_comparison_eq() {
    parse_and_snapshot("test_comparison_eq", r#"[[ $X == "value" ]]"#);
}

#[test]
fn parser_test_comparison_neq() {
    parse_and_snapshot("test_comparison_neq", r#"[[ $X != "other" ]]"#);
}

#[test]
fn parser_test_comparison_gt() {
    parse_and_snapshot("test_comparison_gt", "[[ $NUM -gt 5 ]]");
}

#[test]
fn parser_test_comparison_lt() {
    parse_and_snapshot("test_comparison_lt", "[[ $NUM -lt 10 ]]");
}

#[test]
fn parser_test_comparison_ge() {
    parse_and_snapshot("test_comparison_ge", "[[ $NUM -ge 5 ]]");
}

#[test]
fn parser_test_comparison_le() {
    parse_and_snapshot("test_comparison_le", "[[ $NUM -le 10 ]]");
}

#[test]
fn parser_test_file_exists() {
    parse_and_snapshot("test_file_exists", "[[ -f /etc/hosts ]]");
}

#[test]
fn parser_test_file_dir() {
    parse_and_snapshot("test_file_dir", "[[ -d /tmp ]]");
}

#[test]
fn parser_test_file_exists_quoted() {
    parse_and_snapshot("test_file_exists_quoted", r#"[[ -e "/path" ]]"#);
}

#[test]
fn parser_test_file_is_file() {
    parse_and_snapshot("test_file_is_file", r#"[[ -f "/path/file" ]]"#);
}

#[test]
fn parser_test_file_is_dir() {
    parse_and_snapshot("test_file_is_dir", r#"[[ -d "/path/dir" ]]"#);
}

#[test]
fn parser_test_regex_match() {
    parse_and_snapshot("test_regex_match", r#"[[ $filename =~ "\.rs$" ]]"#);
}

#[test]
fn parser_test_regex_not_match() {
    parse_and_snapshot("test_regex_not_match", r#"[[ $name !~ "^test_" ]]"#);
}

// =============================================================================
// STATEMENT CHAINING
// =============================================================================

#[test]
fn parser_stmt_and_chain() {
    parse_and_snapshot("stmt_and_chain", "cmd1 && cmd2");
}

#[test]
fn parser_stmt_or_chain() {
    parse_and_snapshot("stmt_or_chain", "cmd1 || cmd2");
}

#[test]
fn parser_stmt_chain_three() {
    parse_and_snapshot("stmt_chain_three", "mkdir dir && cd dir && init");
}

#[test]
fn parser_stmt_chain_mixed() {
    parse_and_snapshot("stmt_chain_mixed", r#"try-primary || try-fallback || echo "failed""#);
}

// =============================================================================
// EDGE CASES: Keywords
// =============================================================================

#[test]
fn parser_non_keyword_works() {
    parse_and_snapshot("non_keyword_works", r#"myif="value""#);
}

#[test]
fn parser_keyword_at_stmt_start() {
    parse_and_snapshot("keyword_at_stmt_start", "if true; then echo; fi");
}

#[test]
fn parser_keyword_if_rejected() {
    expect_parse_error(r#"if="value""#);
}

#[test]
fn parser_keyword_while_rejected() {
    expect_parse_error("while=true");
}

#[test]
fn parser_keyword_then_rejected() {
    expect_parse_error(r#"then="next""#);
}

// =============================================================================
// EDGE CASES: Test Expressions
// =============================================================================

#[test]
fn parser_test_expr_empty_error() {
    expect_parse_error("[[ ]]");
}

// =============================================================================
// SET COMMAND VS ASSIGNMENT
// =============================================================================

#[test]
fn parser_set_command_with_flag_e() {
    parse_and_snapshot("set_command_with_flag_e", "set -e");
}

#[test]
fn parser_set_command_multiple_flags() {
    parse_and_snapshot("set_command_multiple_flags", "set -e -u");
}

#[test]
fn parser_set_command_no_args() {
    parse_and_snapshot("set_command_no_args", "set");
}

#[test]
fn parser_set_assignment_legacy() {
    parse_and_snapshot("set_assignment_legacy", "set X = 5");
}

#[test]
fn parser_set_command_with_plus_flag() {
    parse_and_snapshot("set_command_with_plus_flag", "set +e");
}

#[test]
fn parser_set_in_chain() {
    parse_and_snapshot("set_in_chain", r#"set -e && echo "strict mode""#);
}

// =============================================================================
// COMMAND NAMES
// =============================================================================

#[test]
fn parser_true_as_command() {
    parse_and_snapshot("true_as_command", "true");
}

#[test]
fn parser_false_as_command() {
    parse_and_snapshot("false_as_command", "false");
}

#[test]
fn parser_dot_as_source_alias() {
    parse_and_snapshot("dot_as_source_alias", ". script.kai");
}

#[test]
fn parser_source_command() {
    parse_and_snapshot("source_command", "source utils.kai");
}

#[test]
fn parser_true_in_condition() {
    parse_and_snapshot("true_in_condition", "if true; then echo \"yes\"; fi");
}

#[test]
fn parser_false_in_condition() {
    parse_and_snapshot("false_in_condition", "if false; then echo \"no\"; fi");
}

// =============================================================================
// ARGUMENT PARSING
// =============================================================================

#[test]
fn parser_named_arg_no_spaces() {
    parse_and_snapshot("named_arg_no_spaces", "cmd key=value");
}

#[test]
fn parser_named_arg_with_spaces_error() {
    expect_parse_error("cmd key = value");
}

#[test]
fn parser_long_flag_with_value() {
    parse_and_snapshot("long_flag_with_value", r#"git commit --message="hello""#);
}

#[test]
fn parser_short_flag_then_value() {
    parse_and_snapshot("short_flag_then_value", r#"git commit -m "msg""#);
}

#[test]
fn parser_double_dash_ends_flags() {
    // Note: -not-a-flag is split by the lexer into three tokens due to hyphens
    // After --, these become positional string arguments instead of flags
    parse_and_snapshot("double_dash_ends_flags", "cmd -- -not-a-flag");
}

// =============================================================================
// CASE STATEMENTS
// =============================================================================

#[test]
fn parser_case_simple() {
    parse_and_snapshot("case_simple", "case \"hello\" in\n    hello) echo \"matched\" ;;\nesac");
}

#[test]
fn parser_case_multiple_branches() {
    parse_and_snapshot("case_multiple_branches", "case ${X} in\n    foo) echo \"foo\" ;;\n    bar) echo \"bar\" ;;\nesac");
}

#[test]
fn parser_case_with_patterns() {
    parse_and_snapshot("case_with_patterns", "case \"test.rs\" in\n    \"*.py\") echo \"Python\" ;;\n    \"*.rs\") echo \"Rust\" ;;\nesac");
}

#[test]
fn parser_case_multiple_patterns() {
    parse_and_snapshot("case_multiple_patterns", "case \"y\" in\n    \"y\"|\"yes\") echo \"yes\" ;;\nesac");
}

#[test]
fn parser_case_with_default() {
    parse_and_snapshot("case_with_default", "case \"x\" in\n    \"*\") echo \"default\" ;;\nesac");
}

#[test]
fn parser_case_optional_lparen() {
    parse_and_snapshot("case_optional_lparen", "case \"x\" in\n    (foo) echo \"foo\" ;;\nesac");
}

// =============================================================================
// KNOWN FAILURES - Issues to be fixed
// =============================================================================

#[test]
fn parser_pipe_with_args() {
    parse_and_snapshot("pipe_with_args", r#"ls path="/src" | grep pattern="\.rs$" | wc"#);
}
