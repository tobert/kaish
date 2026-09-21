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

/// Regression: a bare `.` *argument* must stay part of its command, not start
/// a new `source` statement. `find .` used to parse as TWO statements —
/// `find` (defaulting to cwd) plus `.` (source, no file) — which silently
/// broke `find .`, `ls .`, `echo .`, etc.
#[rstest]
#[case("find .")]
#[case("ls .")]
#[case("echo .")]
#[case("find . -name x")]
fn bare_dot_argument_is_one_command(#[case] input: &str) {
    let program = parse(input).expect("should parse");
    assert_eq!(
        program.statements.len(),
        1,
        "`{input}` must be a single statement, got {}: {}",
        program.statements.len(),
        format_program(&program),
    );
}

/// The `.` source alias in *command* position must still parse as a single
/// command (with its file argument attached), not split or error.
#[rstest]
#[case(". script.kai", "script.kai")]
#[case(". /etc/profile", "/etc/profile")]
fn leading_dot_is_still_source(#[case] input: &str, #[case] expected_file: &str) {
    let program = parse(input).expect("should parse");
    assert_eq!(program.statements.len(), 1, "`{input}` should be one statement");
    let sexpr = format_program(&program);
    assert!(
        sexpr.contains(expected_file),
        "source's file argument should attach to the command: {sexpr}",
    );
}

/// Keyword tokens are accepted as the *key* of a `key=value` argv assignment,
/// so `dd if=…` parses (`if` is `Token::If`). The classic `dd` idiom and the
/// other operands must come through as one command.
#[rstest]
#[case("dd if=/dev/urandom of=/dev/null bs=1024 count=10")]
#[case("dd if=/dev/zero of=/tmp/z.bin bs=512 count=2")]
#[case("tool in=a do=b for=c")]
fn keyword_key_argv_assignment_parses(#[case] input: &str) {
    let program = parse(input).expect("should parse");
    assert_eq!(
        program.statements.len(),
        1,
        "`{input}` must be a single command: {}",
        format_program(&program),
    );
}

/// The `if=` key fix must NOT shadow a real `if` conditional — a leading `if`
/// with a space still starts an if-statement.
#[test]
fn leading_if_conditional_still_parses() {
    let program = parse("if true; then echo hi; fi").expect("should parse");
    assert_eq!(program.statements.len(), 1);
    let sexpr = format_program(&program);
    assert!(sexpr.contains("(if"), "should be an if-statement: {sexpr}");
}

/// A spaced `if = x` is still the "no spaces around =" error — only the
/// span-adjacent `if=x` is the assignment form.
#[test]
fn spaced_keyword_assignment_still_errors() {
    expect_parse_error("dd if = x");
}

// =============================================================================
// ASSIGNMENTS
// =============================================================================

#[test]
fn parser_assign_int() {
    parse_and_snapshot("assign_int", "X=5");
}

#[test]
fn parser_assign_negative_int() {
    parse_and_snapshot("assign_negative_int", "X=-42");
}

#[test]
fn parser_assign_float() {
    parse_and_snapshot("assign_float", "PI=3.14159");
}

#[test]
fn parser_assign_bool_true() {
    parse_and_snapshot("assign_bool_true", "FLAG=true");
}

#[test]
fn parser_assign_bool_false() {
    parse_and_snapshot("assign_bool_false", "FLAG=false");
}

#[test]
fn parser_assign_string() {
    parse_and_snapshot("assign_string", r#"NAME="alice""#);
}

#[test]
fn parser_assign_string_with_spaces() {
    parse_and_snapshot("assign_string_with_spaces", r#"MSG="hello world""#);
}

#[test]
fn parser_assign_string_with_escapes() {
    parse_and_snapshot("assign_string_with_escapes", r#"MSG="line\nbreak""#);
}

#[test]
fn parser_assign_varref() {
    parse_and_snapshot("assign_varref", "Y=${X}");
}

#[test]
fn parser_assign_simple_varref() {
    parse_and_snapshot("assign_simple_varref", "Y=$X");
}

#[test]
fn parser_assign_interpolated() {
    parse_and_snapshot("assign_interpolated", r#"MSG="hello ${NAME}""#);
}

#[test]
fn parser_assign_interpolated_simple() {
    parse_and_snapshot("assign_interpolated_simple", r#"MSG="hello $NAME""#);
}

#[test]
fn parser_assign_single_quoted() {
    parse_and_snapshot("assign_single_quoted", "MSG='hello $NAME'");
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
#[case::redirect_merge_stderr("cmd 2>&1")]
#[case::redirect_merge_pipe("cmd 2>&1 | tee /tmp/log")]
#[case::redirect_herestring_bare("cat <<< hi")]
#[case::redirect_herestring_interpolated(r#"cat <<< "$R""#)]
#[case::redirect_herestring_literal(r#"cat <<< 'raw $VAR'"#)]
#[case::redirect_herestring_with_stdout(r#"cat <<< hi > /tmp/out"#)]
#[case::redirect_herestring_in_pipeline(r#"jq '.x' <<< "$J" | wc -l"#)]
fn parser_redirects(#[case] input: &str) {
    let name = format!("redirect_{}", input.chars().take(20).filter(|c| c.is_alphanumeric()).collect::<String>());
    parse_and_snapshot(&name, input);
}

#[rstest]
#[case::herestring_no_operand("cat <<<")]
#[case::herestring_then_stdin("cat <<< hi < /in")]
#[case::stdin_then_herestring("cat < /in <<< hi")]
#[case::two_herestrings("cat <<< a <<< b")]
fn parser_herestring_errors(#[case] input: &str) {
    expect_parse_error(input);
}

/// When two stdin sources appear on one command, the actionable
/// "multiple stdin redirects" message must surface — not the generic
/// "expected '=', or '('" from a competing statement-level alternative.
/// Chumsky keeps the furthest-position error, so the custom error must be
/// anchored at the offending (second) redirect to win the merge.
#[rstest]
#[case("cat <<< a <<< b")]
#[case("cat < /in <<< hi")]
#[case("cat <<< hi < /in")]
fn ambiguous_stdin_surfaces_actionable_message(#[case] input: &str) {
    let errors = parse(input).expect_err("expected a parse error");
    let joined = errors
        .iter()
        .map(|e| e.to_string())
        .collect::<Vec<_>>()
        .join("; ");
    assert!(
        joined.contains("multiple stdin redirects"),
        "`{input}` should surface the multiple-stdin message, got: {joined}"
    );
}

// =============================================================================
// CONTROL FLOW
// =============================================================================

#[test]
fn parser_if_simple() {
    // POSIX-shaped: compare $? to 0
    parse_and_snapshot("if_simple", "if [[ $? -eq 0 ]]; then\n    echo \"yes\"\nfi");
}

#[test]
fn parser_if_else() {
    // POSIX-shaped: compare $? to 0
    parse_and_snapshot("if_else", "if [[ $? -eq 0 ]]; then\n    echo \"yes\"\nelse\n    echo \"no\"\nfi");
}

#[test]
fn parser_if_command_condition() {
    parse_and_snapshot("if_command_condition", "if test-something; then\n    echo \"passed\"\nfi");
}

#[test]
fn parser_if_comparison() {
    parse_and_snapshot("if_comparison", "if [[ ${X} -gt 5 ]]; then\n    echo \"big\"\nfi");
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

#[test]
fn parser_precedence_or_then_and() {
    // POSIX: `&&` and `||` are EQUAL precedence, left-associative, so this parses
    // as ((a || b) && c) — NOT a || (b && c).
    parse_and_snapshot("precedence_or_then_and", "a || b && c");
}

#[test]
fn parser_precedence_complex() {
    // Left-to-right, equal precedence: a && b || c && d → (((a && b) || c) && d).
    parse_and_snapshot("precedence_complex", "a && b || c && d");
}

#[test]
fn parser_precedence_deeply_chained() {
    // Left-to-right, equal precedence:
    // a || b || c && d && e → ((((a || b) || c) && d) && e).
    parse_and_snapshot("precedence_deeply_chained", "a || b || c && d && e");
}

// =============================================================================
// STATEMENT-LEVEL ! (PIPELINE NEGATION)
// =============================================================================
//
// `!` binds to the whole pipeline, below `&&`/`||` — bash's reading. See
// docs/LANGUAGE.md, "Shell Options" for the errexit exemption this carries.

#[test]
fn parser_stmt_not_command() {
    parse_and_snapshot("stmt_not_command", "! true");
}

#[test]
fn parser_stmt_not_pipeline() {
    // `!` negates the WHOLE pipeline's status, not just the first stage.
    parse_and_snapshot("stmt_not_pipeline", "! a | b");
}

#[test]
fn parser_stmt_not_and_chain() {
    // `!` binds tighter than `&&`: `! a && b` is `(! a) && b`, not `!(a && b)`.
    parse_and_snapshot("stmt_not_and_chain", "! a && b");
}

#[test]
fn parser_stmt_or_chain_not() {
    // Same precedence rule on the right of `||`.
    parse_and_snapshot("stmt_or_chain_not", "a || ! b");
}

#[test]
fn parser_stmt_not_double() {
    // `! !` double-negates, matching bash's `! ! true`.
    parse_and_snapshot("stmt_not_double", "! ! true");
}

#[test]
fn parser_stmt_not_test_expr() {
    // A `[[ ]]` is a compound command in bash's grammar too — `!` applies.
    parse_and_snapshot("stmt_not_test_expr", "! [[ -f /nonexistent ]]");
}

#[test]
fn parser_stmt_not_arith() {
    parse_and_snapshot("stmt_not_arith", "! (( 0 ))");
}

#[test]
fn parser_stmt_not_compound() {
    // `!` also negates a compound statement's own exit status (bash allows
    // `! for …; done`, `! if …; fi`).
    parse_and_snapshot("stmt_not_compound", "! for x in 1 2; do\n    echo ${x}\ndone");
}

// `!` also wraps the signal statements — bash accepts `! exit 3` and
// `! break`/`! continue`/`! return` syntactically (they fail at RUNTIME, not
// parse time, if the position doesn't apply — same as a bare `break` outside
// a loop). The interpreter passes their ControlFlow through `Stmt::Not`
// untouched, so this is a parser-only change; see shell_compat_tests.rs for
// the runtime-behavior rows.

#[test]
fn parser_stmt_not_exit() {
    parse_and_snapshot("stmt_not_exit", "! exit 3");
}

#[test]
fn parser_stmt_not_return() {
    parse_and_snapshot("stmt_not_return", "! return 2");
}

#[test]
fn parser_stmt_not_break() {
    parse_and_snapshot("stmt_not_break", "! break");
}

#[test]
fn parser_stmt_not_continue() {
    parse_and_snapshot("stmt_not_continue", "! continue");
}

// =============================================================================
// GLUED `!` IS REFUSED (statement, condition, and `[[ ]]` position)
// =============================================================================
//
// bash's `!` is a reserved word needing a token boundary on both sides:
// `!true` lexes as the single word `!true` (bash: `!true: command not
// found`), never as `!` negating `true`. kaish's lexer emits a standalone
// `Bang` token regardless of adjacency, so without a check `!true` and
// `!!true` would silently parse as negation — a real divergence from bash
// that reads as a working script until the exit code is wrong. `!` needs a
// space everywhere it negates: the statement position, the condition
// position (`if`/`while`), and inside `[[ ]]` (`TestExpr::Not`) — Amy's call
// was to refuse a glued `!` in general, not just where it was first caught.

#[rstest]
#[case("!true")]
#[case("!!true")]
#[case("! !true")]
#[case("!grep")]
#[case("!break")]
#[case("!continue")]
#[case("!return")]
#[case("!exit 3")]
#[case("if !true; then echo yes; fi")]
#[case("while !cmd; do :; done")]
#[case("[[ !-f x ]]")]
#[case("[[ !$x == y ]]")]
fn glued_bang_is_refused(#[case] input: &str) {
    expect_parse_error(input);
}

#[test]
fn glued_bang_error_names_the_fix() {
    let errors = parse("!true").expect_err("`!true` must be refused");
    assert!(
        errors[0].message.contains("! true"),
        "the error must name the fix (a spaced `! true`): {errors:?}"
    );
}

/// `! !true` is refused for the INNER glued pair, not the outer spaced one —
/// the first `!` (properly spaced from the second `!`) must not be blamed.
#[test]
fn glued_bang_after_a_spaced_bang_blames_the_right_one() {
    let errors = parse("! !true").expect_err("`! !true` must be refused");
    assert_eq!(errors.len(), 1, "exactly one glue, not two: {errors:?}");
    assert_eq!(
        errors[0].span.start, 2,
        "must blame the SECOND `!` (glued to `true`), not the first"
    );
}

/// `!!true` is ONE mistake (a run of two glued `!`s glued to `true`), not
/// two — only the first glued pair is reported. Fixing the first `!` and
/// re-running surfaces any real remaining glue on its own.
#[test]
fn glued_bang_run_reports_only_the_first_pair() {
    let errors = parse("!!true").expect_err("`!!true` must be refused");
    assert_eq!(errors.len(), 1, "expected exactly one error for a glued run: {errors:?}");
    assert!(
        errors[0].message.contains("!!"),
        "the first pair reported must be the `!!` glue itself: {errors:?}"
    );
}

// Forms that must keep parsing exactly as before: a properly spaced `!`
// (statement, condition, and `[[ ]]` position — including double negation),
// `!=` (a distinct token, not `!` followed by `=`), arithmetic's own `!` (a
// separate sub-lexer), `!` inside quotes, and an ordinary glued ARGUMENT
// (`echo hi!`), which keeps the pre-existing glued-argument error, unrelated
// to this one.

#[rstest]
#[case("! true")]
#[case("! ! true")]
#[case("if ! true; then echo yes; fi")]
#[case("while ! cmd; do :; done")]
#[case("[[ 1 != 2 ]]")]
#[case("(( ! 0 ))")]
#[case("[[ ! -f x ]]")]
#[case("[[ ! ! -f x ]]")]
#[case(r#"echo "!true""#)]
#[case("echo '!true'")]
fn unaffected_bang_forms_still_parse(#[case] input: &str) {
    parse(input).unwrap_or_else(|e| panic!("{input:?} must still parse: {e:?}"));
}

#[test]
fn ordinary_glued_argument_keeps_its_pre_existing_error() {
    // `echo hi!` and `echo !x` were already refused by the PRE-EXISTING
    // glued-ARGUMENT check (`reject_glued_args`) before this work — pin
    // that the message is still that one, not the new glued-`!` message,
    // proving the two checks are independent.
    for input in ["echo hi!", "echo !x"] {
        let errors = parse(input).expect_err("must still be refused (pre-existing behavior)");
        assert!(
            errors[0].message.contains("adjacent words with no space"),
            "{input:?} must keep the pre-existing glued-argument message: {errors:?}"
        );
    }
}

// ── A line continuation must not evade the glued-`!` guard ─────────────────
//
// `tokenize` drops `Token::LineContinuation` from the stream it hands the
// parser, but used to keep the original byte spans either side of it — so
// `!\<newline>true` measured a 2-byte gap between `!` and `true` and read as
// spaced, silently negating. bash removes a backslash-newline before it even
// tokenizes, so `!\<newline>true` IS `!true`: one glued word. Fixed at the
// lexer: a `LineContinuation` flush against the token just kept widens that
// token's span to swallow it, so the parser's span-adjacency check sees the
// same zero gap bash would.

#[test]
fn glued_bang_across_a_line_continuation_is_refused() {
    // "!" + "\" + "\n" + "true" — the continuation is flush against `!`,
    // so removing it (as bash does) leaves `!true`, fully glued.
    let errors = parse("!\\\ntrue").expect_err("a line continuation must not hide the glue");
    assert!(
        errors[0].message.contains("!true"),
        "must report the same glued text as `!true`: {errors:?}"
    );
}

/// Control: a REAL space before the continuation is still a real space —
/// this must keep parsing, proving the fix above checks for the continuation
/// specifically, not for any nonzero byte gap.
#[test]
fn a_real_space_before_a_line_continuation_still_negates() {
    // "!" + " " + "\" + "\n" + "true"
    parse("! \\\ntrue").expect("a real space before the continuation must still negate");
}

// ── A glued `!` inside `$(...)` must surface its own message ───────────────
//
// A purpose-built parse diagnosis raised while re-parsing a `$(...)` body
// can lose its own message to chumsky's `choice`/alternative bookkeeping in
// favor of a generic one — the same loss `validate_cmd_subst_bodies` exists
// to undo for a bare, unquoted `$(...)`. A glued `!` inside a QUOTED
// `"$(...)"` or a heredoc body took a different, still-lossy path: parsing
// the body recursively and, on failure, discarding the real error for a
// generic "syntax error in command substitution" wrapper. Fixed by naming
// the real error's message inside that wrapper instead of discarding it.

#[test]
fn glued_bang_inside_a_bare_command_substitution_surfaces_its_message() {
    // The bare, unquoted form already went through `validate_cmd_subst_bodies`,
    // which re-parses with the real grammar and propagates its own error —
    // a control proving the quoted/heredoc forms below were the exception,
    // not the rule.
    let errors = parse("echo $(!true)").expect_err("must be refused");
    assert!(
        errors[0].message.contains("!true") && errors[0].message.contains("needs a space"),
        "the bare form must surface the glued-`!` message, not a generic one: {errors:?}"
    );
}

#[test]
fn glued_bang_inside_a_quoted_command_substitution_surfaces_its_message() {
    let errors = parse(r#"echo "$(!true)""#).expect_err("must be refused");
    assert!(
        errors[0].message.contains("!true") && errors[0].message.contains("needs a space"),
        "a quoted $(...) must surface the glued-`!` message, not a generic \
         \"syntax error in command substitution\": {errors:?}"
    );
}

#[test]
fn glued_bang_inside_a_heredoc_command_substitution_surfaces_its_message() {
    let errors =
        parse("cat <<EOF\n$(!true)\nEOF\n").expect_err("must be refused");
    assert!(
        errors[0].message.contains("!true") && errors[0].message.contains("needs a space"),
        "a heredoc body's $(...) must surface the glued-`!` message: {errors:?}"
    );
}

#[test]
fn parser_if_command_with_args() {
    // Command with arguments as condition
    parse_and_snapshot("if_command_with_args", "if grep -q pattern file; then\n    echo \"found\"\nfi");
}

#[test]
fn parser_if_test_and_command() {
    // Mixed: test expression && command
    parse_and_snapshot("if_test_and_command", "if [[ -f file ]] && process-file; then\n    echo \"processed\"\nfi");
}

#[test]
fn parser_if_command_or_test() {
    // Mixed: command || test expression
    parse_and_snapshot("if_command_or_test", "if check-cache || [[ -f backup ]]; then\n    echo \"data available\"\nfi");
}

#[test]
fn parser_while_command_with_args() {
    // while with command that has arguments
    parse_and_snapshot("while_command_with_args", "while read-line input; do\n    echo ${line}\ndone");
}

// =============================================================================
// SHELL-STYLE FUNCTIONS (use $1, $2 positional params)
// =============================================================================

#[test]
fn parser_posix_function_minimal() {
    parse_and_snapshot("posix_function_minimal", "greet() {\n}");
}

#[test]
fn parser_posix_function_with_body() {
    parse_and_snapshot("posix_function_with_body", "greet() {\n    echo \"Hello, $1!\"\n}");
}

#[test]
fn parser_posix_function_single_line() {
    parse_and_snapshot("posix_function_single_line", "double() { echo $1 }");
}

#[test]
fn parser_bash_function_minimal() {
    parse_and_snapshot("bash_function_minimal", "function greet {\n}");
}

#[test]
fn parser_bash_function_with_body() {
    parse_and_snapshot("bash_function_with_body", "function greet {\n    echo \"Hello, $1!\"\n}");
}

#[test]
fn parser_bash_function_single_line() {
    parse_and_snapshot("bash_function_single_line", "function double { echo $1 }");
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
fn parser_test_shape_guard_list() {
    parse_and_snapshot("test_shape_guard_list", "[[ -list $x ]]");
}

#[test]
fn parser_test_shape_guard_record() {
    parse_and_snapshot("test_shape_guard_record", "[[ -record $x ]]");
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
// TEST EXPRESSIONS: Compound (&&, ||, !)
// =============================================================================

#[test]
fn parser_test_and() {
    parse_and_snapshot("test_and", "[[ -f file && -d dir ]]");
}

#[test]
fn parser_test_or() {
    parse_and_snapshot("test_or", r#"[[ -z "$VAR" || -n "$DEFAULT" ]]"#);
}

#[test]
fn parser_test_not() {
    parse_and_snapshot("test_not", "[[ ! -f /tmp/lock ]]");
}

#[test]
fn parser_test_not_double() {
    parse_and_snapshot("test_not_double", "[[ ! ! -f file ]]");
}

#[test]
fn parser_test_and_three() {
    parse_and_snapshot("test_and_three", "[[ -f a && -f b && -f c ]]");
}

#[test]
fn parser_test_and_or_precedence() {
    // Precedence: ! > && > ||, so this is: (-f a) || ((-d b) && (-e c))
    parse_and_snapshot("test_and_or_precedence", "[[ -f a || -d b && -e c ]]");
}

#[test]
fn parser_test_not_with_and() {
    parse_and_snapshot("test_not_with_and", "[[ ! -f a && -d b ]]");
}

#[test]
fn parser_test_complex_compound() {
    parse_and_snapshot("test_complex_compound", r#"[[ ! -z "$X" && $Y == "value" || -f /tmp/flag ]]"#);
}

// ── Collection membership: `in` / `not in` ─────────────────────────────────
// The right-hand side must be a collection; a string RHS is a loud error.
// See `docs/LANGUAGE.md`, "Membership".

#[test]
fn parser_test_in() {
    parse_and_snapshot("test_in", "[[ x in $c ]]");
}

#[test]
fn parser_test_not_in() {
    parse_and_snapshot("test_not_in", "[[ x not in $c ]]");
}

#[test]
fn parser_test_in_nested_path() {
    parse_and_snapshot("test_in_nested_path", "[[ 443 in ${servers[web]} ]]");
}

#[test]
fn parser_test_in_compound_and() {
    parse_and_snapshot("test_in_compound_and", "[[ apple in $fruits && web in $services ]]");
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
fn parser_colon_as_command() {
    parse_and_snapshot("colon_as_command", ":");
}

#[test]
fn parser_colon_with_operands() {
    parse_and_snapshot("colon_with_operands", ": one two");
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

/// Post-`test`-grammar-relief: a *spaced* `key = value` is no longer a
/// "no spaces around =" parse error. `=` is now a literal argv operator (so
/// POSIX `test a = b` can reach the `test` command), which makes
/// `cmd key = value` parse as `cmd` with three positional args — exactly like
/// bash. A *glued* `key=value` still binds as a WordAssign
/// (see `parser_named_arg_no_spaces`), so the useful distinction survives.
#[test]
fn parser_spaced_equals_is_literal_argv_operator() {
    assert_eq!(
        one_stmt_sexpr("cmd key = value"),
        r#"(cmd cmd (pos (string "key")) (pos (string "=")) (pos (string "value")))"#
    );
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
    // `-not-a-flag` is a single shell word (the lexer keeps internal hyphens),
    // so after `--` it is one positional string argument, not three.
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

#[test]
fn parser_case_with_path_pattern() {
    // Bug M: paths should work in case patterns
    parse_and_snapshot("case_path_pattern", "case $file in\n    /tmp/*) echo \"temp\" ;;\nesac");
}

#[test]
fn parser_case_with_varref_pattern() {
    // Bug M: variable refs should work in case patterns
    parse_and_snapshot("case_varref_pattern", "case $input in\n    $expected) echo \"match\" ;;\nesac");
}

// -----------------------------------------------------------------------------
// Dash/plus bare-word and flag-shaped patterns (GH #144)
// -----------------------------------------------------------------------------
// `pattern_part` had no arm for the lexer's flag-shaped tokens (`ShortFlag`,
// `LongFlag`, `PlusFlag`) or its dash/plus bare-word fallbacks
// (`DoubleDashBare`, `PlusBare`, `MinusBare`, `MinusAlone`, `DoubleDash`), so
// a case pattern that happened to look like a flag (or a dash/plus-only
// bareword) failed to parse at all.

#[test]
fn parser_case_triple_dash_pattern() {
    // `---` lexes as `DoubleDashBare`.
    parse_and_snapshot(
        "case_triple_dash_pattern",
        "case \"$x\" in\n    ---) echo \"match\" ;;\n    *) echo \"nope\" ;;\nesac",
    );
}

#[test]
fn parser_case_single_dash_pattern() {
    // A lone `-` lexes as `MinusAlone`.
    parse_and_snapshot(
        "case_single_dash_pattern",
        "case \"$x\" in\n    -) echo \"match\" ;;\n    *) echo \"nope\" ;;\nesac",
    );
}

#[test]
fn parser_case_double_dash_pattern() {
    // A standalone `--` lexes as `DoubleDash`, distinct from `DoubleDashBare`.
    parse_and_snapshot(
        "case_double_dash_pattern",
        "case \"$x\" in\n    --) echo \"match\" ;;\n    *) echo \"nope\" ;;\nesac",
    );
}

#[test]
fn parser_case_minus_bare_pattern() {
    // `-%` lexes as `MinusBare`.
    parse_and_snapshot(
        "case_minus_bare_pattern",
        "case \"$x\" in\n    -%) echo \"match\" ;;\n    *) echo \"nope\" ;;\nesac",
    );
}

#[test]
fn parser_case_plus_bare_pattern() {
    // `+%s` lexes as `PlusBare` (the `date +%s` format-string shape).
    parse_and_snapshot(
        "case_plus_bare_pattern",
        "case \"$x\" in\n    +%s) echo \"match\" ;;\n    *) echo \"nope\" ;;\nesac",
    );
}

#[test]
fn parser_case_plus_flag_pattern() {
    // `+foo` lexes as `PlusFlag` (letters after `+`), distinct from `PlusBare`.
    parse_and_snapshot(
        "case_plus_flag_pattern",
        "case \"$x\" in\n    +foo) echo \"match\" ;;\n    *) echo \"nope\" ;;\nesac",
    );
}

#[test]
fn parser_case_short_flag_pattern() {
    // `-x` lexes as `ShortFlag`, same as it would in argument position.
    parse_and_snapshot(
        "case_short_flag_pattern",
        "case \"$x\" in\n    -x) echo \"match\" ;;\n    *) echo \"nope\" ;;\nesac",
    );
}

#[test]
fn parser_case_dash_alternation_pattern() {
    // `-h|--help`: a `ShortFlag` and a `LongFlag` on either side of a pipe
    // alternation, the idiom this bug most visibly broke.
    parse_and_snapshot(
        "case_dash_alternation_pattern",
        "case \"$x\" in\n    -h|--help) echo \"match\" ;;\n    *) echo \"nope\" ;;\nesac",
    );
}

// =============================================================================
// KNOWN FAILURES - Issues to be fixed
// =============================================================================

#[test]
fn parser_pipe_with_args() {
    parse_and_snapshot("pipe_with_args", r#"ls path="/src" | grep pattern="\.rs$" | wc"#);
}

// =============================================================================
// Navigation: cd .., cd ~, cd ~/foo, cd ../bar
// =============================================================================

#[test]
fn parser_cd_dotdot() {
    parse_and_snapshot("cd_dotdot", "cd ..");
}

#[test]
fn parser_cd_tilde() {
    parse_and_snapshot("cd_tilde", "cd ~");
}

#[test]
fn parser_cd_tilde_path() {
    parse_and_snapshot("cd_tilde_path", "cd ~/foo");
}

#[test]
fn parser_cd_relative_path() {
    parse_and_snapshot("cd_relative_path", "cd ../bar");
}

#[test]
fn parser_bare_dotdot() {
    parse_and_snapshot("bare_dotdot", "echo ..");
}

#[test]
fn parser_cd_dot_slash() {
    parse_and_snapshot("cd_dot_slash", "cd ./crates");
}

#[test]
fn parser_dot_slash_exec() {
    parse_and_snapshot("dot_slash_exec", "./script.sh");
}

// =============================================================================
// COLON IN UNQUOTED WORDS
// =============================================================================

#[test]
fn parser_colon_double() {
    parse_and_snapshot("colon_double", "echo foo::bar");
}

#[test]
fn parser_colon_port() {
    parse_and_snapshot("colon_port", "echo host:8080");
}

#[test]
fn parser_colon_in_cargo_test() {
    parse_and_snapshot("colon_cargo_test", "cargo test -- ls::tests");
}

// =============================================================================
// Glob patterns
// =============================================================================

#[test]
fn parser_glob_star_txt() {
    parse_and_snapshot("glob_star_txt", "ls *.txt");
}

#[test]
fn parser_glob_cp_mixed() {
    parse_and_snapshot("glob_cp_mixed", "cp *.rs /tmp");
}

#[test]
fn parser_glob_bracket_class() {
    parse_and_snapshot("glob_bracket_class", "rm [a-z].txt");
}

#[test]
fn parser_glob_bare_star() {
    parse_and_snapshot("glob_bare_star", "echo *");
}

#[test]
fn parser_glob_quoted_stays_literal() {
    parse_and_snapshot("glob_quoted_literal", "ls \"*.txt\"");
}

#[test]
fn parser_glob_double_star() {
    parse_and_snapshot("glob_double_star", "ls **/*.rs");
}

#[test]
fn parser_glob_question_mark() {
    parse_and_snapshot("glob_question_mark", "ls file?.log");
}

#[test]
fn parser_glob_for_loop() {
    parse_and_snapshot("glob_for_loop", "for f in *.txt; do echo $f; done");
}

#[test]
fn parser_glob_bare_question() {
    parse_and_snapshot("glob_bare_question", "echo ?");
}

#[test]
fn parser_glob_in_case() {
    parse_and_snapshot("glob_in_case", "case $x in\n    *.txt) echo text ;;\nesac");
}

#[test]
fn parser_glob_in_named_arg() {
    parse_and_snapshot("glob_in_named_arg", "cmd file=*.txt");
}

#[test]
fn parser_glob_in_test() {
    parse_and_snapshot("glob_in_test", "[[ *.txt == foo ]]");
}

// ---------------------------------------------------------------------------
// Grammar relief for the `test` builtin (Phase 1). POSIX `test`/`[` is a
// *command*, so its comparison and negation operators (`=`, `==`, `!=`, `!`)
// must reach it as ordinary positional argv words. kaish's grammar treats
// these as shell-significant tokens, so they used to parse-error before ever
// reaching a command. This relief makes them literal positionals — but ONLY
// these four: `<` `>` `<=` `>=` stay redirection (see the redirect-unaffected
// test below) and remain `[[ ]]`-only. See signoff.md / project_test_builtin.
// ---------------------------------------------------------------------------

/// Parse `input`, require exactly one statement, and return its s-expr.
/// A parse error (or a split into multiple statements) fails loudly here —
/// that IS the pre-relief bug we're fixing.
fn one_stmt_sexpr(input: &str) -> String {
    let program = parse(input).unwrap_or_else(|errors| {
        let msg = errors
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("; ");
        panic!("parse error for {input:?}: {msg}");
    });
    assert_eq!(
        program.statements.len(),
        1,
        "{input:?} must be ONE statement, got {}: {}",
        program.statements.len(),
        format_program(&program),
    );
    format_program(&program)
}

#[rstest]
// The canonical string-equality idiom — THE reason a flag-form-only `test`
// was judged not-credible.
#[case(
    "test a = b",
    r#"(cmd test (pos (string "a")) (pos (string "=")) (pos (string "b")))"#
)]
// bash-refugee `==` alias.
#[case(
    "test a == b",
    r#"(cmd test (pos (string "a")) (pos (string "==")) (pos (string "b")))"#
)]
// Inequality.
#[case(
    "test a != b",
    r#"(cmd test (pos (string "a")) (pos (string "!=")) (pos (string "b")))"#
)]
// Leading `!` as a `test` OPERAND, not kaish's own statement-level `!`
// negation: `test`'s raw-argv binding takes it as a literal positional
// string (POSIX's `test ! -f x` negation operand), distinct from
// `! test -f x`, which negates the whole command's exit status via grammar.
#[case(
    "test ! -f x",
    r#"(cmd test (pos (string "!")) (shortflag f) (pos (string "x")))"#
)]
// Operators as ordinary argv on any command, not just `test` (the relief is
// name-agnostic — no fragile special-casing of the `test` command name).
#[case(
    "echo a = b",
    r#"(cmd echo (pos (string "a")) (pos (string "=")) (pos (string "b")))"#
)]
fn test_operators_reach_command_as_positionals(#[case] input: &str, #[case] expected: &str) {
    assert_eq!(one_stmt_sexpr(input), expected);
}

/// The most common real-world form: quoted variable operands around `=`.
/// We only assert the operator lands as a literal positional between them;
/// the operand rendering is exercised elsewhere.
#[test]
fn test_quoted_var_equality_parses() {
    let sexpr = one_stmt_sexpr(r#"test "$a" = "$b""#);
    assert!(
        sexpr.starts_with("(cmd test "),
        "expected a `test` command, got: {sexpr}"
    );
    assert!(
        sexpr.contains(r#"(pos (string "="))"#),
        "`=` must land as a literal positional, got: {sexpr}"
    );
}

// --- Regressions: the relief must not disturb assignment / argv-assign /
// long-flag-value / redirection parsing. ---

/// A glued `x=y` at statement level is still an ASSIGNMENT, not a command
/// `x` with a bare `=` argument.
#[test]
fn glued_assignment_still_assignment_not_command() {
    let sexpr = one_stmt_sexpr("x=y");
    assert!(
        !sexpr.starts_with("(cmd "),
        "`x=y` must stay an assignment, not become a command: {sexpr}"
    );
}

/// A glued `key=value` in argv position (`cat foo=bar`) stays the WordAssign
/// production (bash: opens a file literally named `foo=bar`), NOT split into
/// `foo` `=` `bar`.
#[test]
fn glued_argv_assign_stays_wordassign() {
    let sexpr = one_stmt_sexpr("cat foo=bar");
    assert!(
        sexpr.contains("(wordassign foo"),
        "glued `foo=bar` must stay a WordAssign, got: {sexpr}"
    );
    assert!(
        !sexpr.contains(r#"(pos (string "="))"#),
        "glued `foo=bar` must not fragment into a bare `=` positional: {sexpr}"
    );
}

/// A glued `--name=value` long flag still binds its value.
#[test]
fn long_flag_with_value_still_binds() {
    let sexpr = one_stmt_sexpr("grep --context=3 pat");
    assert!(
        sexpr.contains("(named context"),
        "`--context=3` must stay a named flag, got: {sexpr}"
    );
}

/// `<` / `>` stay REDIRECTION — the relief deliberately excludes the angle
/// brackets so it can't shadow redirects.
#[test]
fn angle_brackets_stay_redirection() {
    let sexpr = one_stmt_sexpr("echo x > f");
    assert!(
        sexpr.contains("redir") && !sexpr.contains(r#"(pos (string ">"))"#),
        "`>` must remain a redirect, not become a positional: {sexpr}"
    );
}

// ---------------------------------------------------------------------------
// GH #189 (arg-binding polish): the no-token-pasting guard used to cover
// pre-`--` positional args only. A glued redirect target fell through to a
// generic chumsky "expected ..." error with no quoting hint, and args glued
// together AFTER `--` (or a flag glued straight to a following fragment)
// silently split into separate args instead of erroring at all.
// ---------------------------------------------------------------------------

fn parse_err_message(input: &str) -> String {
    match parse(input) {
        Ok(program) => panic!(
            "expected a parse error for {input:?}, got: {}",
            format_program(&program)
        ),
        Err(errors) => errors.iter().map(|e| e.to_string()).collect::<Vec<_>>().join("; "),
    }
}

/// A redirect target that fragments into multiple glued lexical pieces
/// (`/tmp/$(echo x).txt`) used to fail with a generic, unhelpful chumsky
/// error. It now gets the same "quote it" hint the plain-positional glue
/// guard gives, worded for a redirect target.
#[test]
fn glued_redirect_target_hints_to_quote() {
    let msg = parse_err_message("echo > /tmp/$(echo x).txt");
    assert!(msg.contains("redirect target"), "should name the redirect target: {msg}");
    assert!(msg.to_lowercase().contains("quote"), "should hint to quote: {msg}");
}

/// The same guard applies to `<` (stdin) redirect targets, not just `>`.
#[test]
fn glued_stdin_redirect_target_hints_to_quote() {
    let msg = parse_err_message("cat < /tmp/$(echo x).txt");
    assert!(msg.contains("redirect target"), "got: {msg}");
}

/// A single-fragment redirect target (no glue) is unaffected.
#[test]
fn unglued_redirect_target_still_parses() {
    one_stmt_sexpr(r#"echo > "/tmp/$(echo x).txt""#);
    one_stmt_sexpr("echo > /tmp/out.txt");
}

/// The pre-`--` guard already rejected glued positionals; the post-`--` half
/// used to be unchecked entirely, silently splitting `/tmp/$(echo x).txt`
/// into three positionals instead of erroring.
#[test]
fn glued_positional_after_double_dash_is_rejected() {
    let msg = parse_err_message("echo hi -- /tmp/$(echo x).txt");
    assert!(msg.to_lowercase().contains("quote"), "should hint to quote: {msg}");
}

/// A spaced (non-glued) positional after `--` is unaffected.
#[test]
fn spaced_positional_after_double_dash_still_parses() {
    let sexpr = one_stmt_sexpr("echo hi -- --this-is-data");
    assert!(sexpr.contains("(doubledash)"), "got: {sexpr}");
    assert!(
        sexpr.contains(r#"(pos (string "--this-is-data"))"#),
        "got: {sexpr}"
    );
}

/// `--flag` has no glued-value idiom (only the explicit `--flag=value` form
/// does) — a long flag glued straight to a following expression fragment
/// with no `=` (`--flag$(echo x)`) is always a pasting accident, not a
/// feature, and must be rejected rather than silently splitting into a bare
/// `--flag` bool plus a stray positional.
#[test]
fn glued_long_flag_then_fragment_is_rejected() {
    let msg = parse_err_message("echo --flag$(echo x)");
    assert!(msg.to_lowercase().contains("quote"), "should hint to quote: {msg}");
}

/// The short-flag glued-value idiom (`cut -d,`, `grep -A1`, `head -c5`) must
/// keep working: a `ShortFlag` glued to exactly one following fragment is a
/// deliberate feature (`consume_flag_positionals`/`bind_glued_short_value`
/// in the kernel binder), not a pasting accident, so it must NOT trip the
/// guard above.
#[test]
fn glued_short_flag_then_single_fragment_still_parses() {
    one_stmt_sexpr("echo -f$(echo x)");
    one_stmt_sexpr("cut -d, -f2");
}
