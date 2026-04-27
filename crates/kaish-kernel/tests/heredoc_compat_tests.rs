//! Heredoc compat tests — same scripts run through kaish AND bash.
//!
//! Most heredoc behavior is POSIX-defined and bash-compatible, so these are
//! a high-yield place to compare. Uses `eq_exact:` (no trim) because heredoc
//! semantics turn on trailing newlines.
//!
//! Skipped from this suite (kaish behavior diverges from bash by design):
//! - CRLF normalization in heredoc bodies (kaish normalizes; bash preserves
//!   `\r` and rejects `EOF\r` as a non-match for the delimiter `EOF`).
//! - Unterminated heredocs (kaish uses whatever was collected; bash warns
//!   to stderr and appends a trailing newline).
//! - `$(cmd)` inside heredoc bodies (kaish has a known limitation around
//!   async redirect-target evaluation; tests in heredoc_tests.rs are
//!   `#[ignore]` until that lands).

mod common;

// ---- Literal vs interpolated bodies ---------------------------------------

shell_compat! {
    name: literal_body_no_interpolation,
    script: "cat <<'EOF'\n$NOT_A_VAR\nEOF",
    eq_exact: "$NOT_A_VAR\n",
}

shell_compat! {
    name: double_quoted_delimiter_is_literal,
    script: "cat <<\"EOF\"\n$NOT_A_VAR\nEOF",
    eq_exact: "$NOT_A_VAR\n",
}

shell_compat! {
    name: interpolated_body_var_substitution,
    script: "X=hello\ncat <<EOF\n${X}\nEOF",
    eq_exact: "hello\n",
}

shell_compat! {
    name: interpolated_body_simple_var,
    script: "X=world\ncat <<EOF\nhello $X\nEOF",
    eq_exact: "hello world\n",
}

shell_compat! {
    name: empty_body,
    script: "cat <<EOF\nEOF",
    eq_exact: "",
}

// ---- `<<-EOF` indented form: leading-tab stripping (POSIX) ----------------

shell_compat! {
    name: dash_form_strips_leading_tabs,
    script: "cat <<-EOF\n\thello\n\tworld\n\tEOF",
    eq_exact: "hello\nworld\n",
}

shell_compat! {
    name: dash_form_does_not_strip_leading_spaces,
    script: "cat <<-EOF\n   spaced\n\tEOF",
    eq_exact: "   spaced\n",
}

shell_compat! {
    name: dash_form_with_interpolation_strips_tabs,
    script: "X=value\ncat <<-EOF\n\t${X}\n\tEOF",
    eq_exact: "value\n",
}

shell_compat! {
    name: dash_form_literal_delimiter_strips_tabs,
    script: "cat <<-'EOF'\n\thi\n\tEOF",
    eq_exact: "hi\n",
}

shell_compat! {
    name: dash_form_strips_multiple_leading_tabs,
    script: "cat <<-EOF\n\t\t\tdeep\n\tEOF",
    eq_exact: "deep\n",
}

shell_compat! {
    name: dash_form_preserves_internal_tabs,
    script: "cat <<-EOF\n\thello\tworld\n\tEOF",
    eq_exact: "hello\tworld\n",
}

// ---- Pipeline interaction -------------------------------------------------

shell_compat! {
    name: trailing_text_after_delimiter_pipes,
    script: "cat <<EOF | tr a-z A-Z\nfoo\nEOF",
    eq_exact: "FOO\n",
}

shell_compat! {
    name: heredoc_body_with_arithmetic_expansion,
    script: "cat <<EOF\nresult: $((1+2))\nEOF",
    eq_exact: "result: 3\n",
}

// ---- Multi-line bodies ----------------------------------------------------

shell_compat! {
    name: multi_line_body_preserves_newlines,
    script: "cat <<EOF\nline1\nline2\nline3\nEOF",
    eq_exact: "line1\nline2\nline3\n",
}

shell_compat! {
    name: body_with_blank_line_in_middle,
    script: "cat <<EOF\nfirst\n\nthird\nEOF",
    eq_exact: "first\n\nthird\n",
}

shell_compat! {
    name: preserves_multiple_trailing_newlines,
    script: "cat <<EOF\nhello\n\n\nEOF",
    eq_exact: "hello\n\n\n",
}

// ---- Backslash escape processing inside unquoted heredocs (POSIX) --------

shell_compat! {
    name: dollar_escape_in_unquoted_heredoc_suppresses_expansion,
    script: "X=hello\ncat <<EOF\n\\$X\nEOF",
    eq_exact: "$X\n",
}

shell_compat! {
    name: dollar_escape_in_quoted_heredoc_is_literal_backslash,
    script: "X=hello\ncat <<'EOF'\n\\$X\nEOF",
    eq_exact: "\\$X\n",
}

shell_compat! {
    name: backslash_backslash_escape_collapses_to_one_backslash,
    script: "cat <<EOF\n\\\\\nEOF",
    eq_exact: "\\\n",
}

shell_compat! {
    name: backslash_other_char_kept_literally,
    script: "cat <<EOF\nfoo\\nbar\nEOF",
    eq_exact: "foo\\nbar\n",
}

shell_compat! {
    name: backslash_newline_is_line_continuation,
    script: "cat <<EOF\nfoo\\\nbar\nEOF",
    eq_exact: "foobar\n",
}

shell_compat! {
    name: backslash_dollar_then_unrelated_var_only_escapes_first,
    script: "X=foo\nY=bar\ncat <<EOF\n\\$X $Y\nEOF",
    eq_exact: "$X bar\n",
}

// ---- Heredocs nested inside control flow / functions ----------------------

shell_compat! {
    name: heredoc_inside_if_branch,
    script: "if true; then\ncat <<EOF\nin-then\nEOF\nfi",
    eq_exact: "in-then\n",
}

shell_compat! {
    name: heredoc_inside_for_loop_body,
    script: "for N in $(seq 1 2); do\ncat <<EOF\npass $N\nEOF\ndone",
    eq_exact: "pass 1\npass 2\n",
}

shell_compat! {
    name: heredoc_inside_user_function,
    script: "greet() {\ncat <<EOF\nhello from $1\nEOF\n}\ngreet Amy",
    eq_exact: "hello from Amy\n",
}

// ---- Body oddities --------------------------------------------------------

shell_compat! {
    name: body_line_looking_like_delimiter_but_longer_is_not_delimiter,
    script: "cat <<EOF\nEOFSUFFIX\ntrue body\nEOF",
    eq_exact: "EOFSUFFIX\ntrue body\n",
}

shell_compat! {
    name: body_with_leading_whitespace_before_delimiter_in_plain_form,
    script: "cat <<EOF\nbody\n  EOF\nEOF",
    eq_exact: "body\n  EOF\n",
}
