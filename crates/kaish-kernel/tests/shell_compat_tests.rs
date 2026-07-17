//! Compat tests that run the same script through kaish AND bash, so we can
//! spot where the two diverge.
//!
//! Each `shell_compat!` invocation generates a submodule with two tests:
//!   `<name>::kaish` — always runs, executes the script via the kaish kernel.
//!   `<name>::bash`  — runs only when `KAISH_BASH_COMPAT=1` is set, executes
//!                     the script via `bash -c` and applies the matching
//!                     assertions to bash's stdout/exit code.
//!
//! Usage:
//!   cargo test --test shell_compat_tests                     # kaish only
//!   KAISH_BASH_COMPAT=1 cargo test --test shell_compat_tests # both sides
//!
//! `cargo test bash` filters to just the bash side and surfaces divergences.
//!
//! Available clauses (mix and match in any order):
//!   eq: "..."         — both sides: trimmed stdout equals the literal
//!   kaish_eq: "..."   — kaish-only stdout assertion (pair with bash_eq:
//!                       to capture an intended divergence)
//!   bash_eq: "..."    — bash-only stdout assertion
//!   contains: "..."   — both sides: stdout contains the substring
//!   absent: "..."     — both sides: stdout does not contain the substring
//!   exit: N           — both sides: process exit code equals N
//!
//! Skipped from this suite (kaish-only by design — no benefit comparing):
//!   - `local x = inner` (kaish syntax with spaces; bash uses `local x=inner`)
//!   - `--json` output, kaish-only builtins (jq, glob, write, kaish-trash)
//!   - VFS paths (`/v/...`)
//!   - find / split / seq structured-iteration behavior

mod common;

// ---- $() full statement grammar (docs/issues.md #5) -----------------------
// $() bodies accept the full statement grammar — &&/|| chains, ; sequences,
// multi-line bodies, and # comments — not just a single pipeline.

shell_compat! {
    name: cmdsubst_and_chain,
    script: "echo $(true && echo hi)",
    eq: "hi",
}

shell_compat! {
    name: cmdsubst_and_chain_short_circuits,
    script: "echo $(false && echo nope)",
    eq: "",
}

shell_compat! {
    name: cmdsubst_or_chain,
    script: "echo $(false || echo fallback)",
    eq: "fallback",
}

shell_compat! {
    name: cmdsubst_semicolon_sequence,
    script: "echo $(printf a; printf b)",
    eq: "ab",
}

shell_compat! {
    name: cmdsubst_multiline_body,
    script: "echo $(printf a
printf b)",
    eq: "ab",
}

shell_compat! {
    name: cmdsubst_comment_in_body,
    script: "echo $(printf a # trailing note
printf b)",
    eq: "ab",
}

shell_compat! {
    name: cmdsubst_chain_in_string_interpolation,
    script: r#"echo "result: $(false || echo recovered)""#,
    eq: "result: recovered",
}

// ---- $? and ${?}: exit-code variable forms --------------------------------

shell_compat! {
    name: braced_last_exit_code_after_success,
    script: "true; echo ${?}",
    eq: "0",
}

shell_compat! {
    name: braced_last_exit_code_after_failure,
    script: "false; echo ${?}",
    eq: "1",
}

shell_compat! {
    name: exit_code_in_arithmetic,
    script: "false; echo $(( $? + 10 ))",
    eq: "11",
}

shell_compat! {
    name: exit_code_in_arithmetic_after_success,
    script: "true; echo $(( $? * 5 ))",
    eq: "0",
}

shell_compat! {
    name: braced_exit_code_in_arithmetic,
    script: "false; echo $(( ${?} + 5 ))",
    eq: "6",
}

shell_compat! {
    name: exit_code_in_string_interpolation,
    script: r#"false; echo "exit code: $?""#,
    eq: "exit code: 1",
}

shell_compat! {
    name: braced_exit_code_in_string_interpolation,
    script: r#"false; echo "exit code: ${?}""#,
    eq: "exit code: 1",
}

// ---- Command substitution -------------------------------------------------

shell_compat! {
    name: nested_command_substitution,
    script: "echo $(echo $(echo hello))",
    eq: "hello",
}

shell_compat! {
    name: deeply_nested_command_substitution,
    script: "echo $(echo $(echo $(echo deep)))",
    eq: "deep",
}

shell_compat! {
    name: cmd_subst_with_true,
    script: "VAR=$(true); echo \"exit: $?\"",
    eq: "exit: 0",
}

shell_compat! {
    name: cmd_subst_with_false,
    script: "VAR=$(false); echo \"exit: $?\"",
    eq: "exit: 1",
}

shell_compat! {
    name: cmd_subst_in_string,
    script: r#"echo "inner: $(echo nested)""#,
    eq: "inner: nested",
}

shell_compat! {
    name: cmd_subst_in_string_with_var,
    script: r#"VAL=world; echo "hello $(echo $VAL)""#,
    eq: "hello world",
}

shell_compat! {
    name: var_in_default_with_cmd_subst,
    script: r#"echo "${UNSET:-$(echo computed)}""#,
    eq: "computed",
}

// ---- Functions: return / exit-code propagation ----------------------------

shell_compat! {
    name: return_does_not_leak_to_capture,
    script: "f() {\n    echo output\n    return 5\n}\nX=$(f)\necho \"captured: [$X]\"\n",
    contains: "captured: [output]",
    absent: "captured: [5",
}

shell_compat! {
    name: return_sets_exit_code,
    script: "f() { return 42; }\nf\necho $?\n",
    eq: "42",
}

shell_compat! {
    name: cmd_subst_in_return,
    script: "f() { return $(echo 42); }\nf\necho $?\n",
    eq: "42",
}

shell_compat! {
    name: return_without_value,
    script: "f() { echo hi; return; }\nX=$(f)\necho \"got: [$X]\"\n",
    eq: "got: [hi]",
}

// ---- Positional params in arithmetic --------------------------------------

shell_compat! {
    name: positional_params_arithmetic_add,
    script: "add() {\n    echo $(($1 + $2))\n}\nadd 3 4\n",
    eq: "7",
}

shell_compat! {
    name: positional_params_arithmetic_mul,
    script: "mul() {\n    echo $(($1 * $2))\n}\nmul 5 6\n",
    eq: "30",
}

// ---- export ---------------------------------------------------------------

shell_compat! {
    name: export_with_value,
    script: "export FOO=\"bar\"; echo $FOO",
    eq: "bar",
}

shell_compat! {
    name: export_multiple_vars,
    script: "export A=1; export B=2; echo \"$A $B\"",
    eq: "1 2",
}

// ---- Inline env prefix `FOO=bar cmd` (docs/issues.md #1) ------------------
// An inline assignment prefix is scoped to the one command — it does NOT leak
// into the rest of the script (the bash-divergence bug this fixes).

shell_compat! {
    name: env_prefix_does_not_leak,
    script: r#"FOO=bar echo hi; echo "[$FOO]""#,
    eq: "hi\n[]",
}

shell_compat! {
    name: env_prefix_multiple_do_not_leak,
    script: r#"A=1 B=2 echo hi; echo "[$A$B]""#,
    eq: "hi\n[]",
}

// A plain assignment (no command following) still persists — only the
// prefixed-command form is command-scoped.
shell_compat! {
    name: plain_assignment_still_persists,
    script: "FOO=bar; echo $FOO",
    eq: "bar",
}

// kaish divergence (documented): the prefixed variable is visible to the
// command itself, including its argument expansion, because kaish builtins read
// from scope. bash expands the command's args before applying the prefix, so it
// prints empty. The leak-free guarantee above is the shared, load-bearing part.
shell_compat! {
    name: env_prefix_visible_to_command_args,
    script: "FOO=bar echo $FOO",
    kaish_eq: "bar",
    bash_eq: "",
}

// ---- Nested ${VAR:-default} -----------------------------------------------

shell_compat! {
    name: nested_var_default_all_unset,
    script: r#"echo "${A:-${B:-deep}}""#,
    eq: "deep",
}

shell_compat! {
    name: nested_var_default_outer_set,
    script: r#"A=outer; echo "${A:-${B:-deep}}""#,
    eq: "outer",
}

shell_compat! {
    name: nested_var_default_middle_set,
    script: r#"B=middle; echo "${A:-${B:-deep}}""#,
    eq: "middle",
}

shell_compat! {
    name: deeply_nested_var_defaults,
    script: r#"echo "${A:-${B:-${C:-deepest}}}""#,
    eq: "deepest",
}

// ---- Command substitution in [[ ]], case ----------------------------------

shell_compat! {
    name: cmd_subst_in_test_eq,
    script: r#"[[ $(echo hello) == "hello" ]] && echo "match" || echo "nope""#,
    eq: "match",
}

shell_compat! {
    name: cmd_subst_in_test_neq,
    script: r#"[[ $(echo hello) != "world" ]] && echo "diff" || echo "same""#,
    eq: "diff",
}

shell_compat! {
    name: cmd_subst_in_case,
    script: "case $(echo hello) in\n    hello) echo \"matched\" ;;\n    *) echo \"nope\" ;;\nesac\n",
    eq: "matched",
}

// ---- Dash/plus bare-word patterns in case (GH #144) ------------------------
// `case_parser`'s `pattern_part` had no arm for the lexer's flag-shaped
// tokens or its dash/plus bare-word fallbacks, so a case pattern that
// happened to look like a flag (or a dash-only bareword) failed to parse.

shell_compat! {
    name: case_triple_dash_matches,
    script: "x=\"---\"; case $x in\n    ---) echo \"match\" ;;\n    *) echo \"nope\" ;;\nesac",
    eq: "match",
}

shell_compat! {
    name: case_dash_alternation_matches,
    script: "x=\"--help\"; case $x in\n    -h|--help) echo \"match\" ;;\n    *) echo \"nope\" ;;\nesac",
    eq: "match",
}

shell_compat! {
    name: case_dash_pattern_star_fallback_still_works,
    script: "x=\"other\"; case $x in\n    ---) echo \"dash\" ;;\n    *) echo \"fallback\" ;;\nesac",
    eq: "fallback",
}

// ---- Compound test expressions: [[ A && B ]], [[ A || B ]], [[ ! A ]] ----
//
// Use `/` and `/tmp` (universal on POSIX-ish systems) instead of
// per-test tempdirs so the scripts are pure and deterministic on both sides.

shell_compat! {
    name: compound_and_both_true,
    script: r#"[[ -d / && -d /tmp ]] && echo "both""#,
    eq: "both",
}

shell_compat! {
    name: compound_and_one_false,
    script: r#"[[ -d / && -f /nonexistent_kaish_test ]] && echo "both" || echo "failed""#,
    eq: "failed",
}

shell_compat! {
    name: compound_or_first_true,
    script: r#"[[ -d / || -f /nonexistent_kaish_test ]] && echo "one""#,
    eq: "one",
}

shell_compat! {
    name: compound_or_second_true,
    script: r#"[[ -f /nonexistent_kaish_test || -d / ]] && echo "one""#,
    eq: "one",
}

shell_compat! {
    name: compound_or_both_false,
    script: r#"[[ -f /nonexistent_kaish_test || -f /also_nonexistent_kaish ]] && echo "yes" || echo "no""#,
    eq: "no",
}

shell_compat! {
    name: compound_not_true,
    script: r#"[[ ! -f /nonexistent_kaish_test ]] && echo "correct""#,
    eq: "correct",
}

shell_compat! {
    name: compound_not_false,
    script: r#"[[ ! -d / ]] && echo "yes" || echo "no""#,
    eq: "no",
}

shell_compat! {
    name: compound_double_not,
    script: r#"[[ ! ! -d / ]] && echo "yes" || echo "no""#,
    eq: "yes",
}

shell_compat! {
    name: compound_string_tests,
    script: r#"VAR=""; [[ -z "$VAR" || -n "default" ]] && echo "ok""#,
    eq: "ok",
}

shell_compat! {
    name: compound_with_comparison,
    script: r#"X=5; [[ $X -gt 3 && $X -lt 10 ]] && echo "in range""#,
    eq: "in range",
}

shell_compat! {
    name: compound_in_if,
    script: "X=5\nif [[ $X -gt 0 && $X -lt 10 ]]; then\n    echo valid\nelse\n    echo invalid\nfi\n",
    eq: "valid",
}

// Numeric test operators (-eq/-ne/-gt/-lt/-ge/-le) coerce string operands
// to numbers. The lex-vs-numeric distinction matters once operands have
// more than one digit — `"15".cmp("2")` is Less, but 15 >= 2 numerically.

shell_compat! {
    name: numeric_ge_multidigit_unquoted,
    script: "X=15; [[ $X -ge 2 ]] && echo ok || echo nope",
    eq: "ok",
}

shell_compat! {
    name: numeric_ge_multidigit_quoted,
    script: r#"X=15; [[ "$X" -ge 2 ]] && echo ok || echo nope"#,
    eq: "ok",
}

shell_compat! {
    name: numeric_ge_string_literals,
    script: r#"[[ "15" -ge "2" ]] && echo ok || echo nope"#,
    eq: "ok",
}

shell_compat! {
    name: numeric_lt_multidigit_via_cmd_subst,
    script: r#"COUNT=$(echo 5); [[ "$COUNT" -lt 10 ]] && echo ok || echo nope"#,
    eq: "ok",
}

shell_compat! {
    name: numeric_gt_two_quoted_strings,
    script: r#"[[ "15" -gt "2" ]] && echo ok || echo nope"#,
    eq: "ok",
}

shell_compat! {
    name: numeric_le_two_quoted_strings,
    script: r#"[[ "2" -le "15" ]] && echo ok || echo nope"#,
    eq: "ok",
}

shell_compat! {
    name: numeric_eq_leading_zero_string,
    script: r#"[[ "01" -eq "1" ]] && echo ok || echo nope"#,
    eq: "ok",
}

shell_compat! {
    name: numeric_ne_quoted_strings,
    script: r#"[[ "2" -ne "15" ]] && echo ok || echo nope"#,
    eq: "ok",
}

// `==` does string equality in [[ ]] (bash-compat) and must agree across
// quoted/unquoted RHS — a leading-zero string is NOT equal to a bare
// numeric literal. Use `-eq` to compare numerically.

shell_compat! {
    name: string_eq_leading_zero_vs_int_literal,
    script: r#"X="01"; [[ "$X" == 1 ]] && echo same || echo diff"#,
    eq: "diff",
}

shell_compat! {
    name: string_eq_leading_zero_vs_quoted,
    script: r#"X="01"; [[ "$X" == "1" ]] && echo same || echo diff"#,
    eq: "diff",
}

shell_compat! {
    name: string_eq_quoted_vs_int_literal_agree,
    script: r#"X="1"; [[ "$X" == 1 ]] && [[ "$X" == "1" ]] && echo agree || echo split"#,
    eq: "agree",
}

shell_compat! {
    name: numeric_eq_handles_what_string_eq_does_not,
    script: r#"X="01"; [[ "$X" -eq 1 ]] && echo numeric || echo lex"#,
    eq: "numeric",
}

shell_compat! {
    name: compound_short_circuit_or,
    script: r#"[[ -d / || $(cat /nonexistent_file) == "x" ]] && echo "yes" || echo "no""#,
    eq: "yes",
}

shell_compat! {
    name: compound_short_circuit_and,
    script: r#"[[ -f /nonexistent_kaish_test && $(cat /nonexistent_file) == "x" ]] && echo "yes" || echo "no""#,
    eq: "no",
}

shell_compat! {
    name: compound_not_with_and,
    script: r#"[[ ! -f /nonexistent_kaish_test && -d / ]] && echo "both""#,
    eq: "both",
}

// Precedence: && binds tighter than ||
// [[ -f /nx || -d / && -d /tmp ]] = [[ -f /nx || (-d / && -d /tmp) ]]
// false || (true && true) = true
shell_compat! {
    name: compound_precedence,
    script: r#"[[ -f /nonexistent_kaish_test || -d / && -d /tmp ]] && echo "yes" || echo "no""#,
    eq: "yes",
}

// ---- Tokenization edge cases ----------------------------------------------

shell_compat! {
    name: colon_in_unquoted_arg,
    script: "echo foo::bar",
    eq: "foo::bar",
}

shell_compat! {
    name: colon_port_in_arg,
    script: "echo host:8080",
    eq: "host:8080",
}

shell_compat! {
    name: colon_in_variable_assignment,
    script: "PATH=/usr/bin:/usr/local/bin\necho $PATH",
    eq: "/usr/bin:/usr/local/bin",
}

// ---- Pipelines (large-output deadlock regression) -------------------------

shell_compat! {
    name: pipeline_large_output_no_deadlock,
    script: "seq 1 20000 | wc -l",
    contains: "20000",
}

shell_compat! {
    name: head_n_in_pipeline,
    script: "printf 'a\\nb\\nc\\nd\\ne\\n' | head -n 3",
    eq: "a\nb\nc",
}

shell_compat! {
    name: pipeline_three_stage_large_output_no_deadlock,
    script: "seq 1 20000 | grep '1' | wc -l",
    eq: "13439",
}

// ---- Command-substitution scope/cwd isolation -----------------------------

shell_compat! {
    name: cmd_subst_variable_isolation,
    script: "X=outer\nset_inner() { X=inner; echo $X; }\nY=$(set_inner)\necho $X\n",
    eq: "outer",
}

// ---- for-loop semantics ---------------------------------------------------

shell_compat! {
    name: for_loop_cleanup_after_body_failure,
    script: "for i in 1 2 3; do\n    false\ndone\nfor j in a b c; do\n    echo $j\ndone\n",
    eq: "a\nb\nc",
}

// ---- set -e (errexit) -----------------------------------------------------

shell_compat! {
    name: errexit_after_and_chain,
    script: "set -e\ntrue && true\nfalse\necho \"should not reach\"\n",
    absent: "should not reach",
}

shell_compat! {
    name: errexit_after_or_chain,
    script: "set -e\nfalse || true\nfalse\necho \"should not reach\"\n",
    absent: "should not reach",
}

shell_compat! {
    name: errexit_suppressed_inside_and_chain_left,
    script: "set -e\nfalse && echo right\necho reached\n",
    contains: "reached",
    absent: "right",
}

// ---- Exit-code assertions -------------------------------------------------

shell_compat! {
    name: false_exits_one,
    script: "false",
    exit: 1,
}

shell_compat! {
    name: explicit_exit_code,
    script: "exit 7",
    exit: 7,
}

// ---- Recorded divergences -------------------------------------------------
//
// kaish does NOT split $(...) on whitespace; bash does. Capturing both
// expected outputs in one entry lets the test pass on each side AND fail
// loudly if either side ever changes its behavior unexpectedly.

shell_compat! {
    name: command_subst_no_implicit_split,
    script: "for x in $(echo \"a b c\"); do echo \"[$x]\"; done",
    kaish_eq: "[a b c]",
    bash_eq: "[a]\n[b]\n[c]",
}

// kaish scopes the for-loop variable to the loop frame; bash leaks the
// last bound value into the surrounding scope.

shell_compat! {
    name: for_loop_variable_scoping,
    script: "for i in 1 2 3; do\n    true\ndone\necho \"after: [$i]\"\n",
    kaish_eq: "after: []",
    bash_eq: "after: [3]",
}

// =============================================================================
// `${VAR:-WORD}` default-word quote removal — the quotes are shell syntax,
// not data, so `${X:-"default"}` yields `default` (no literal quotes), same
// as bash. Regression for the 2026-06-09 finding.
// =============================================================================

shell_compat! {
    name: default_word_double_quoted_strips_quotes,
    script: r#"echo ${NAME:-"default"}"#,
    eq: "default",
}

shell_compat! {
    name: default_word_single_quoted_strips_quotes,
    script: "echo ${NAME:-'default'}",
    eq: "default",
}

shell_compat! {
    name: default_word_bare_unchanged,
    script: "echo ${NAME:-default}",
    eq: "default",
}

shell_compat! {
    name: default_word_double_quoted_with_spaces,
    script: r#"echo ${NAME:-"a b c"}"#,
    eq: "a b c",
}

shell_compat! {
    name: default_word_double_quoted_interpolates,
    script: r#"X=hi; echo ${NAME:-"$X there"}"#,
    eq: "hi there",
}

shell_compat! {
    name: default_word_single_quoted_suppresses_interpolation,
    script: "X=hi; echo ${NAME:-'$X there'}",
    eq: "$X there",
}

shell_compat! {
    name: default_word_set_value_wins_over_quoted_default,
    script: r#"NAME=actual; echo ${NAME:-"default"}"#,
    eq: "actual",
}

// `${VAR:-default}` default-word ESCAPED quotes inside DOUBLE quotes (GH #93
// item 5). A backslash-escaped quote inside a double-quoted default word must
// not toggle the quote-tracking state — it's literal data, unescaping to a
// bare quote character, exactly like bash's double-quote escape rule.

shell_compat! {
    name: default_word_double_quoted_escaped_quotes_literal,
    script: r#"echo ${NAME:-"hello \"world\""}"#,
    eq: "hello \"world\"",
}

shell_compat! {
    name: default_word_escaped_backslash_before_quote,
    script: r#"echo ${NAME:-"a\\"}"#,
    eq: "a\\",
}

shell_compat! {
    name: default_word_mixed_single_and_escaped_double_quotes,
    script: r#"echo ${NAME:-"it's \"quoted\""}"#,
    eq: "it's \"quoted\"",
}

// Inside DOUBLE quotes, `'` is not special, so a backslash before it is
// literal — only `\"`, `\$`, `\\`, `` \` `` are escapes in a double-quoted
// region. `${VAR:-"a\'b"}` therefore keeps the backslash (`a\'b`), matching
// bash (kaibo-caught divergence: the escape used to also fire on `\'` here).
shell_compat! {
    name: default_word_double_quoted_backslash_before_squote_literal,
    script: r#"echo ${NAME:-"a\'b"}"#,
    eq: "a\\'b",
}

// Single-quoted default words are a LITERAL region, per shell rules: zero
// interpolation AND zero escape processing. Only the delimiter quotes are
// stripped (syntax, not data); a backslash stays literal and a `'` always
// closes the region — it is never escaped.

shell_compat! {
    name: default_word_single_quoted_strips_delimiters,
    script: "echo ${NAME:-'x'}",
    eq: "x",
}

shell_compat! {
    name: default_word_single_quoted_no_interpolation,
    script: "echo ${NAME:-'$HOME'}",
    eq: "$HOME",
}

shell_compat! {
    name: default_word_single_quoted_backslash_literal,
    script: r#"echo ${NAME:-'a\b'}"#,
    eq: "a\\b",
}

// The shell-correct way to embed a single quote is the `'…'\''…'` idiom:
// close the single-quoted span, emit an UNQUOTED escaped `\'`, reopen. The
// escape fires only outside single quotes — which this fix keeps working.
shell_compat! {
    name: default_word_single_quote_embed_idiom,
    script: r#"echo ${NAME:-'it'\''s'}"#,
    eq: "it's",
}

// =============================================================================
// `break N` / `continue N` must not discard output printed before the signal.
// The signal used to replace the loop's accumulated result on its way up, so
// `break 2` swallowed everything the inner loop had printed. Regression for
// the 2026-06-09 finding.
// =============================================================================

shell_compat! {
    name: break_2_preserves_inner_output,
    script: "for i in 1 2; do for j in a b; do echo \"$j\"; break 2; done; done",
    eq: "a",
}

shell_compat! {
    name: break_1_preserves_output,
    script: "for i in 1 2 3; do echo \"$i\"; break; done",
    eq: "1",
}

shell_compat! {
    name: break_2_preserves_outer_and_inner_output,
    script: "for i in x y; do echo \"outer$i\"; for j in a b; do echo \"inner$j\"; break 2; done; done",
    eq: "outerx\ninnera",
}

shell_compat! {
    name: continue_2_preserves_inner_output,
    script: "for i in 1 2; do for j in a b; do echo \"i${i}j${j}\"; continue 2; done; done",
    eq: "i1ja\ni2ja",
}

// =============================================================================
// Statement-output joining inserts no artificial separator — `;` and `&&` both
// concatenate raw output, matching bash. `printf` (no trailing newline) is the
// discriminator; `echo` brings its own newline. Regression for the 2026-06-09
// finding that `;` gave `ab` while `&&` gave `a\nb`.
// =============================================================================

shell_compat! {
    name: semicolon_sequence_no_separator,
    script: r#"printf "a"; printf "b""#,
    eq: "ab",
}

shell_compat! {
    name: and_chain_no_separator,
    script: r#"printf "a" && printf "b""#,
    eq: "ab",
}

shell_compat! {
    name: semicolon_and_and_chain_agree,
    script: r#"printf "x" && printf "y"; printf "z""#,
    eq: "xyz",
}

shell_compat! {
    name: echo_sequence_keeps_own_newlines,
    script: "echo a; echo b",
    eq: "a\nb",
}

shell_compat! {
    name: printf_loop_no_separator,
    script: "for i in 1 2 3; do printf \"$i\"; done",
    eq: "123",
}

shell_compat! {
    name: unset_is_silent,
    script: "X=1; unset X; echo done",
    eq: "done",
}

// ---- standalone [[ ]] writes $? (docs/issues.md P1, found during PR-D) ----
// A bare test statement must store its result in $? like any command. It
// evaluated the test but never wrote $?, so a following read was stale.

shell_compat! {
    name: standalone_test_failure_writes_status,
    script: "[[ 1 = 2 ]]; echo $?",
    eq: "1",
}

shell_compat! {
    name: standalone_test_success_writes_status,
    script: "false; [[ 1 = 1 ]]; echo $?",
    eq: "0",
}

shell_compat! {
    name: standalone_test_status_captured_in_var,
    script: "[[ a = b ]]; ok=$?; echo $ok",
    eq: "1",
}

shell_compat! {
    name: failing_test_gates_and_chain,
    script: "[[ 1 = 2 ]] && echo skipped; echo done",
    eq: "done",
}

shell_compat! {
    name: failing_test_statement_trips_errexit,
    script: "set -e; [[ 1 = 2 ]]; echo unreachable",
    absent: "unreachable",
    exit: 1,
}
