//! Parser for kaish source code.
//!
//! Transforms a token stream from the lexer into an Abstract Syntax Tree.
//! Uses chumsky for parser combinators with good error recovery.

use crate::ast::{
    Arg, Assignment, BinaryOp, CaseBranch, CaseStmt, Command, Expr, FileTestOp, ForLoop, IfStmt,
    ListElem, Pipeline, Program, RecordEntry, RecordKey, Redirect, RedirectKind, SpannedPart, Stmt,
    StringPart, StringTestOp, TestCmpOp, TestExpr, ToolDef, Value, VarPath, VarSegment, WhileLoop,
};
use crate::lexer::{self, HereDocData, Token};
use chumsky::{input::ValueInput, prelude::*};

/// Span type used throughout the parser.
pub type Span = SimpleSpan;

/// Parse a raw `${...}` string into an Expr.
///
/// Handles:
/// - Special variables: `${?}` → LastExitCode, `${$}` → CurrentPid
/// - Simple paths: `${VAR}`, `${VAR.field}`, `${VAR[0]}` → VarRef
/// - Default values: `${VAR:-default}` → VarWithDefault (with nested expansion support)
fn parse_var_expr(raw: &str) -> Expr {
    // Special case: ${?} is the last exit code (same as $?)
    if raw == "${?}" {
        return Expr::LastExitCode;
    }

    // Special case: ${$} is the current PID (same as $$)
    if raw == "${$}" {
        return Expr::CurrentPid;
    }

    // Check for default value syntax: ${VAR:-default}
    // Need to find :- that's not inside a nested ${...}
    if let Some(colon_idx) = find_default_separator(raw) {
        // Extract the variable path (between ${ and :-) — may carry subscripts.
        let path = parse_varpath(&format!("${{{}}}", &raw[2..colon_idx]));
        // Extract default value (between :- and }) and recursively parse it,
        // after stripping shell quoting from the word (quotes are syntax).
        let default_str = &raw[colon_idx + 2..raw.len() - 1];
        // This `${VAR:-WORD}` expr path is infallible; a malformed `$()` inside
        // the default word stays literal here (rare edge). The common quoted
        // `"$(…)"` path is the loud one (see `parse_interpolated_string`).
        let default_word = unquote_default_word(default_str);
        let default = parse_interpolated_string(&default_word)
            .unwrap_or_else(|_| vec![StringPart::Literal(default_word.clone())]);
        return Expr::VarWithDefault { path, default };
    }

    // Regular variable path
    Expr::VarRef(parse_varpath(raw))
}

/// Remove shell quoting from a `${VAR:-WORD}` default word, bash-style, before
/// the word is parsed for interpolation.
///
/// The quotes around a default word are syntax, not data: `${X:-"default"}`
/// yields `default`, not `"default"`. Double quotes are stripped but `$`-style
/// interpolation inside them stays active; single quotes are stripped and
/// suppress interpolation (their `$` becomes a literal, via the lexer's
/// `__KAISH_ESCAPED_DOLLAR__` marker that `parse_interpolated_string` turns
/// back into a bare `$`). Unquoted text passes through unchanged.
///
/// A backslash-escaped quote unescapes to a bare quote character without
/// toggling the quote-tracking state, but *which* quote is escapable depends on
/// context, matching bash (GH #93 item 5): OUTSIDE any quotes both `\"` and
/// `\'` escape (this is what makes the `'it'\''s'` → `it's` embedding idiom
/// resolve); INSIDE double quotes only `\"` escapes, since `'` is an ordinary
/// character there — a backslash before it stays literal (`"a\'b"` → `a\'b`). A
/// run of backslashes immediately before an escapable quote is judged by parity
/// (bash pairs them left-to-right): an odd run escapes the quote, an even run
/// doesn't, and either way the run collapses to half as many literal
/// backslashes. Backslashes not immediately followed by an escapable quote are
/// untouched — general backslash-escape processing (`\\`, `\n`, ...) outside
/// quote-adjacency is out of scope for this function.
///
/// Inside a single-quoted region shell rules apply verbatim: it is a LITERAL
/// span with zero escape processing and zero interpolation. A backslash is a
/// literal character and a `'` always closes the region (it is never escaped);
/// only `$` is marked (`__KAISH_ESCAPED_DOLLAR__`) so it can't interpolate
/// downstream. Only the delimiter quotes themselves are stripped — they are
/// syntax, not data.
fn unquote_default_word(word: &str) -> String {
    let mut out = String::with_capacity(word.len());
    let mut in_single = false;
    let mut in_double = false;
    let chars: Vec<char> = word.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        let ch = chars[i];
        // Backslash-escape processing applies only OUTSIDE single quotes. In a
        // single-quoted region a backslash is a literal character (handled by
        // the `_` arm below) and a `'` always closes the span, per shell rules.
        if ch == '\\' && !in_single {
            let run_start = i;
            while i < chars.len() && chars[i] == '\\' {
                i += 1;
            }
            let run_len = i - run_start;
            // Inside double quotes only `\"` escapes; `'` is an ordinary
            // character there, so a preceding backslash stays literal.
            let next_is_quote =
                chars.get(i).is_some_and(|c| *c == '"' || (*c == '\'' && !in_double));
            if next_is_quote {
                if run_len / 2 > 0 {
                    out.push_str(&"\\".repeat(run_len / 2));
                }
                if run_len % 2 == 1 {
                    // Odd run: the quote is escaped — literal quote, no
                    // toggle. Consume it here; the main loop below never
                    // sees it.
                    out.push(chars[i]);
                    i += 1;
                }
                // Even run: the quote at chars[i] is unescaped and falls
                // through to the normal toggle logic on the next iteration.
            } else {
                out.push_str(&"\\".repeat(run_len));
            }
            continue;
        }
        i += 1;
        match ch {
            // A quote delimiter toggles its mode and is itself dropped; the
            // other quote kind is literal data while inside one.
            '\'' if !in_double => in_single = !in_single,
            '"' if !in_single => in_double = !in_double,
            // `$` inside single quotes must not interpolate downstream.
            '$' if in_single => out.push_str("__KAISH_ESCAPED_DOLLAR__"),
            _ => out.push(ch),
        }
    }
    out
}

/// Find the position of :- in a ${VAR:-default} expression, accounting for nested ${...}.
fn find_default_separator(raw: &str) -> Option<usize> {
    let bytes = raw.as_bytes();
    let mut depth = 0;
    let mut bracket_depth = 0;
    let mut i = 0;

    while i < bytes.len() {
        if i + 1 < bytes.len() && bytes[i] == b'$' && bytes[i + 1] == b'{' {
            depth += 1;
            i += 2;
            continue;
        }
        if bytes[i] == b'}' && depth > 0 {
            depth -= 1;
            i += 1;
            continue;
        }
        // Track `[...]` so a `:-` inside a subscript (e.g. the negative slice end
        // in `${xs[0:-1]}`) is NOT mistaken for a default separator.
        if bytes[i] == b'[' {
            bracket_depth += 1;
        } else if bytes[i] == b']' && bracket_depth > 0 {
            bracket_depth -= 1;
        }
        // Only find :- at the top level (depth == 1 means we're inside the outer
        // ${...}) and outside any subscript.
        if depth == 1
            && bracket_depth == 0
            && i + 1 < bytes.len()
            && bytes[i] == b':'
            && bytes[i + 1] == b'-'
        {
            return Some(i);
        }
        i += 1;
    }
    None
}

/// Find the position of :- in variable content (without outer braces), accounting for nested ${...}.
fn find_default_separator_in_content(content: &str) -> Option<usize> {
    let bytes = content.as_bytes();
    let mut depth = 0;
    let mut bracket_depth = 0;
    let mut i = 0;

    while i < bytes.len() {
        if i + 1 < bytes.len() && bytes[i] == b'$' && bytes[i + 1] == b'{' {
            depth += 1;
            i += 2;
            continue;
        }
        if bytes[i] == b'}' && depth > 0 {
            depth -= 1;
            i += 1;
            continue;
        }
        // Track `[...]` so a `:-` inside a subscript (e.g. the negative slice end
        // in `${xs[0:-1]}`) is NOT mistaken for a default separator.
        if bytes[i] == b'[' {
            bracket_depth += 1;
        } else if bytes[i] == b']' && bracket_depth > 0 {
            bracket_depth -= 1;
        }
        // Find :- at the top level (depth == 0) and outside any subscript.
        if depth == 0
            && bracket_depth == 0
            && i + 1 < bytes.len()
            && bytes[i] == b':'
            && bytes[i + 1] == b'-'
        {
            return Some(i);
        }
        i += 1;
    }
    None
}

/// Parse a raw `${...}` string into a VarPath.
///
/// The first segment is the root variable name; each `[...]` segment the lexer
/// produced becomes the corresponding subscript (`Index`/`Key`/`Dynamic`/
/// `Slice`). A dotted segment (`${a.b}`) is kept as a non-root `Field` so
/// resolution can emit the brackets-only error — the lexer already split it out.
pub(crate) fn parse_varpath(raw: &str) -> VarPath {
    let segment_strs = lexer::parse_var_ref(raw).unwrap_or_default();
    let segments = segment_strs
        .into_iter()
        .enumerate()
        .map(|(i, s)| {
            if i == 0 {
                // The root name (or the special `?`).
                VarSegment::Field(s)
            } else if let Some(inner) = s.strip_prefix('[').and_then(|s| s.strip_suffix(']')) {
                parse_subscript(inner)
            } else {
                // A dotted `.field` — carried through as a Field so resolution
                // produces the "use ${name[field]}" error (brackets only).
                VarSegment::Field(s)
            }
        })
        .collect();
    VarPath { segments }
}

/// Parse the interior of a `[...]` subscript into a `VarSegment`.
///
/// Classification is syntactic (the container's runtime type decides list-vs-
/// record at resolution): `$var` → dynamic; a quoted string → literal key;
/// `int:int` (either side optional) → slice; a bare integer → index; anything
/// else → a literal bareword key.
fn parse_subscript(inner: &str) -> VarSegment {
    // Dynamic: `[$var]`.
    if let Some(var) = inner.strip_prefix('$') {
        return VarSegment::Dynamic(var.to_string());
    }
    // Quoted key: `["weird key"]` or `['weird key']`.
    if inner.len() >= 2
        && ((inner.starts_with('"') && inner.ends_with('"'))
            || (inner.starts_with('\'') && inner.ends_with('\'')))
    {
        return VarSegment::Key(inner[1..inner.len() - 1].to_string());
    }
    // Slice: `a:b` where each side is empty or a valid integer. A colon that
    // isn't a numeric slice falls through to a bareword key (`["a:b"]` covers
    // colon-bearing keys explicitly).
    if let Some((lhs, rhs)) = inner.split_once(':') {
        let bound = |s: &str| -> Option<Option<i64>> {
            if s.is_empty() {
                Some(None)
            } else {
                s.parse::<i64>().ok().map(Some)
            }
        };
        if let (Some(start), Some(end)) = (bound(lhs), bound(rhs)) {
            return VarSegment::Slice(start, end);
        }
    }
    // Integer index: `[0]`, `[-1]`.
    if let Ok(i) = inner.parse::<i64>() {
        return VarSegment::Index(i);
    }
    // Bareword literal key: `[name]`, `[content-type]`.
    VarSegment::Key(inner.to_string())
}

/// Drop `Stmt::Empty` (bare newlines/semicolons) from a parsed `$()` body so an
/// empty or whitespace-only substitution collapses to nothing runnable.
fn strip_empty_stmts(statements: Vec<Stmt>) -> Vec<Stmt> {
    statements
        .into_iter()
        .filter(|s| !matches!(s, Stmt::Empty))
        .collect()
}

/// Parse an unquoted heredoc body's interpolation while tracking each part's
/// byte offset in the source.
///
/// `base_offset` is added to every part's offset so callers can attribute
/// positions to a larger source (e.g., heredoc body inside the original
/// script). Returns parts in source order with offset+len populated.
///
/// **Heredoc-specific behaviour**: per POSIX, unquoted heredoc bodies process
/// three backslash escapes — `\$` (suppress expansion), `\\` (literal
/// backslash), and `\<newline>` (line continuation). All other backslashes
/// are kept verbatim. This differs from [`parse_interpolated_string`], which
/// is called on double-quoted string content where the lexer has already
/// processed escapes via `__KAISH_ESCAPED_DOLLAR__`.
///
/// This sibling of [`parse_interpolated_string`] duplicates parsing logic
/// for now; unifying them behind a position-tracking core is a follow-up
/// cleanup. Behaviour MUST stay aligned for the non-escape paths — bug fixes
/// for the shared interpolation logic here should land there as well.
fn parse_interpolated_string_spanned(s: &str, base_offset: usize) -> Vec<SpannedPart> {
    let s = s.replace("__KAISH_ESCAPED_DOLLAR__", "\x00DOLLAR\x00");

    let chars_vec: Vec<char> = s.chars().collect();
    let mut i = 0;
    let mut pos: usize = 0;

    let mut parts: Vec<SpannedPart> = Vec::new();
    let mut current_text = String::new();
    let mut current_text_start: usize = pos;

    let push_literal =
        |current_text: &mut String, start: &mut usize, end: usize, parts: &mut Vec<SpannedPart>| {
            if !current_text.is_empty() {
                parts.push(SpannedPart {
                    part: StringPart::Literal(std::mem::take(current_text)),
                    offset: base_offset + *start,
                    len: end - *start,
                });
                *start = end;
            }
        };

    while i < chars_vec.len() {
        let ch = chars_vec[i];

        if ch == '\x00' {
            // Escaped-dollar marker: \x00 DOLLAR \x00 → literal '$'
            let start = pos;
            i += 1;
            pos += 1;
            let mut marker = String::new();
            while let Some(&c) = chars_vec.get(i) {
                if c == '\x00' {
                    i += 1;
                    pos += 1;
                    break;
                }
                marker.push(c);
                i += 1;
                pos += c.len_utf8();
            }
            if marker == "DOLLAR" {
                if current_text.is_empty() {
                    current_text_start = start;
                }
                current_text.push('$');
            }
        } else if ch == '\\' {
            // POSIX heredoc-body escape processing for unquoted heredocs.
            // Only `\$`, `\\`, and `\<newline>` are escapes; everything else
            // keeps the backslash verbatim. Each case advances `pos` by the
            // bytes consumed from the source so subsequent part offsets stay
            // anchored to original-source coordinates.
            let next = chars_vec.get(i + 1).copied();
            match next {
                Some('$') => {
                    if current_text.is_empty() {
                        current_text_start = pos;
                    }
                    current_text.push('$');
                    i += 2;
                    pos += 2;
                }
                Some('\\') => {
                    if current_text.is_empty() {
                        current_text_start = pos;
                    }
                    current_text.push('\\');
                    i += 2;
                    pos += 2;
                }
                Some('\n') => {
                    // Line continuation: consume both bytes, emit nothing.
                    // The literal run resumes on the next line.
                    i += 2;
                    pos += 2;
                    if current_text.is_empty() {
                        current_text_start = pos;
                    }
                }
                Some('\r') => {
                    // \<CR> or \<CR><LF>: line continuation
                    i += 2;
                    pos += 2;
                    if chars_vec.get(i) == Some(&'\n') {
                        i += 1;
                        pos += 1;
                    }
                    if current_text.is_empty() {
                        current_text_start = pos;
                    }
                }
                _ => {
                    // Other backslash sequences: keep `\` literally,
                    // consume only the backslash. The next iteration will
                    // process the following char on its own merits.
                    if current_text.is_empty() {
                        current_text_start = pos;
                    }
                    current_text.push('\\');
                    i += 1;
                    pos += 1;
                }
            }
        } else if ch == '$' {
            // Possible expansion. Save current run before peeking ahead.
            let part_start = pos;
            let next = chars_vec.get(i + 1).copied();

            if next == Some('(') && chars_vec.get(i + 2) != Some(&'(') {
                // $(...) command substitution
                push_literal(&mut current_text, &mut current_text_start, pos, &mut parts);
                i += 2; // consume "$("
                pos += 2;
                let mut cmd_content = String::new();
                let mut depth = 1;
                while let Some(&c) = chars_vec.get(i) {
                    i += 1;
                    pos += c.len_utf8();
                    if c == '(' {
                        depth += 1;
                        cmd_content.push(c);
                    } else if c == ')' {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                        cmd_content.push(c);
                    } else {
                        cmd_content.push(c);
                    }
                }
                let inserted = if let Ok(program) = parse(&cmd_content) {
                    // The full statement block runs as the substitution body
                    // (pipelines, `&&`/`||`, `;`/newline sequences, comments).
                    let stmts = strip_empty_stmts(program.statements);
                    if stmts.is_empty() {
                        false
                    } else {
                        parts.push(SpannedPart {
                            part: StringPart::CommandSubst(stmts),
                            offset: base_offset + part_start,
                            len: pos - part_start,
                        });
                        true
                    }
                } else {
                    false
                };
                if inserted {
                    // Successfully pushed a CommandSubst; the next literal
                    // run will start after the closing ')'.
                    current_text_start = pos;
                } else {
                    // Fall back to literal text. The literal run starts at
                    // the leading '$' (set above only if current_text was
                    // empty); leave current_text_start alone otherwise so we
                    // don't lose an in-progress run.
                    if current_text.is_empty() {
                        current_text_start = part_start;
                    }
                    current_text.push_str("$(");
                    current_text.push_str(&cmd_content);
                    current_text.push(')');
                }
            } else if next == Some('{') {
                push_literal(&mut current_text, &mut current_text_start, pos, &mut parts);
                i += 2; // consume "${"
                pos += 2;
                let mut var_content = String::new();
                let mut depth = 1;
                while let Some(&c) = chars_vec.get(i) {
                    i += 1;
                    pos += c.len_utf8();
                    if c == '{' && var_content.ends_with('$') {
                        depth += 1;
                        var_content.push(c);
                    } else if c == '}' {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                        var_content.push(c);
                    } else {
                        var_content.push(c);
                    }
                }
                let part = if let Some(name) = var_content.strip_prefix('#') {
                    StringPart::VarLength(parse_varpath(&format!("${{{name}}}")))
                } else if var_content.starts_with("__ARITH:") && var_content.ends_with("__") {
                    let expr = var_content
                        .strip_prefix("__ARITH:")
                        .and_then(|s| s.strip_suffix("__"))
                        .unwrap_or("");
                    StringPart::Arithmetic(expr.to_string())
                } else if let Some(colon_idx) = find_default_separator_in_content(&var_content) {
                    let path = parse_varpath(&format!("${{{}}}", &var_content[..colon_idx]));
                    let default_str = &var_content[colon_idx + 2..];
                    // Default value spans recursively kept relative to the
                    // outer body — the inner parts get their own offsets via
                    // the recursive call when needed. For now, the default's
                    // parts are stored without spans (default is a Vec<StringPart>).
                    let default_word = unquote_default_word(default_str);
                    let default = parse_interpolated_string(&default_word)
                        .unwrap_or_else(|_| vec![StringPart::Literal(default_word.clone())]);
                    StringPart::VarWithDefault { path, default }
                } else {
                    StringPart::Var(parse_varpath(&format!("${{{}}}", var_content)))
                };
                parts.push(SpannedPart {
                    part,
                    offset: base_offset + part_start,
                    len: pos - part_start,
                });
                current_text_start = pos;
            } else if next.map(|c| c.is_ascii_digit()).unwrap_or(false) {
                push_literal(&mut current_text, &mut current_text_start, pos, &mut parts);
                i += 1; // consume '$'
                pos += 1;
                if let Some(&digit) = chars_vec.get(i) {
                    let n = digit.to_digit(10).unwrap_or(0) as usize;
                    i += 1;
                    pos += digit.len_utf8();
                    parts.push(SpannedPart {
                        part: StringPart::Positional(n),
                        offset: base_offset + part_start,
                        len: pos - part_start,
                    });
                }
                current_text_start = pos;
            } else if next == Some('@') {
                push_literal(&mut current_text, &mut current_text_start, pos, &mut parts);
                i += 2; // consume "$@"
                pos += 2;
                parts.push(SpannedPart {
                    part: StringPart::AllArgs,
                    offset: base_offset + part_start,
                    len: pos - part_start,
                });
                current_text_start = pos;
            } else if next == Some('#') {
                push_literal(&mut current_text, &mut current_text_start, pos, &mut parts);
                i += 2; // consume "$#"
                pos += 2;
                parts.push(SpannedPart {
                    part: StringPart::ArgCount,
                    offset: base_offset + part_start,
                    len: pos - part_start,
                });
                current_text_start = pos;
            } else if next == Some('?') {
                push_literal(&mut current_text, &mut current_text_start, pos, &mut parts);
                i += 2; // consume "$?"
                pos += 2;
                parts.push(SpannedPart {
                    part: StringPart::LastExitCode,
                    offset: base_offset + part_start,
                    len: pos - part_start,
                });
                current_text_start = pos;
            } else if next == Some('$') {
                push_literal(&mut current_text, &mut current_text_start, pos, &mut parts);
                i += 2; // consume "$$"
                pos += 2;
                parts.push(SpannedPart {
                    part: StringPart::CurrentPid,
                    offset: base_offset + part_start,
                    len: pos - part_start,
                });
                current_text_start = pos;
            } else if next.map(|c| c.is_ascii_alphabetic() || c == '_').unwrap_or(false) {
                push_literal(&mut current_text, &mut current_text_start, pos, &mut parts);
                i += 1; // consume '$'
                pos += 1;
                let mut var_name = String::new();
                while let Some(&c) = chars_vec.get(i) {
                    if c.is_ascii_alphanumeric() || c == '_' {
                        var_name.push(c);
                        i += 1;
                        pos += c.len_utf8();
                    } else {
                        break;
                    }
                }
                parts.push(SpannedPart {
                    part: StringPart::Var(VarPath::simple(var_name)),
                    offset: base_offset + part_start,
                    len: pos - part_start,
                });
                current_text_start = pos;
            } else {
                // Bare $ — treat as literal
                if current_text.is_empty() {
                    current_text_start = pos;
                }
                current_text.push(ch);
                i += 1;
                pos += 1;
            }
        } else {
            if current_text.is_empty() {
                current_text_start = pos;
            }
            current_text.push(ch);
            i += 1;
            pos += ch.len_utf8();
        }
    }

    push_literal(&mut current_text, &mut current_text_start, pos, &mut parts);

    parts
}

fn parse_interpolated_string(s: &str) -> Result<Vec<StringPart>, String> {
    // First, replace escaped dollar markers with a temporary placeholder
    // The lexer uses __KAISH_ESCAPED_DOLLAR__ for \$ to prevent re-interpretation
    let s = s.replace("__KAISH_ESCAPED_DOLLAR__", "\x00DOLLAR\x00");

    let mut parts = Vec::new();
    let mut current_text = String::new();
    let mut chars = s.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\x00' {
            // This is our escaped dollar marker - skip "DOLLAR" and the closing \x00
            let mut marker = String::new();
            while let Some(&c) = chars.peek() {
                if c == '\x00' {
                    chars.next(); // consume closing marker
                    break;
                }
                if let Some(c) = chars.next() {
                    marker.push(c);
                }
            }
            if marker == "DOLLAR" {
                current_text.push('$');
            }
        } else if ch == '$' {
            // Check for command substitution $(...)
            if chars.peek() == Some(&'(') {
                // Command substitution $(...)
                if !current_text.is_empty() {
                    parts.push(StringPart::Literal(std::mem::take(&mut current_text)));
                }

                // Consume the '('
                chars.next();

                // Collect until matching ')' accounting for nested parens
                let mut cmd_content = String::new();
                let mut paren_depth = 1;
                for c in chars.by_ref() {
                    if c == '(' {
                        paren_depth += 1;
                        cmd_content.push(c);
                    } else if c == ')' {
                        paren_depth -= 1;
                        if paren_depth == 0 {
                            break;
                        }
                        cmd_content.push(c);
                    } else {
                        cmd_content.push(c);
                    }
                }

                // Parse the command content as a full statement block
                // (pipelines, `&&`/`||` chains, `;`/newline sequences, comments).
                match parse(&cmd_content) {
                    Ok(program) => {
                        let stmts = strip_empty_stmts(program.statements);
                        if stmts.is_empty() {
                            // Nothing runnable (e.g. `$()` or only a comment) —
                            // bash treats this as the empty string. Keep literal.
                            current_text.push_str("$(");
                            current_text.push_str(&cmd_content);
                            current_text.push(')');
                        } else {
                            parts.push(StringPart::CommandSubst(stmts));
                        }
                    }
                    Err(_) => {
                        // A syntax error inside the substitution is loud, exactly
                        // like the unquoted `$(...)` form — never silently demoted
                        // to literal text.
                        return Err(format!(
                            "syntax error in command substitution: $({cmd_content})"
                        ));
                    }
                }
            } else if chars.peek() == Some(&'{') {
                // Braced variable reference ${...}
                if !current_text.is_empty() {
                    parts.push(StringPart::Literal(std::mem::take(&mut current_text)));
                }

                // Consume the '{'
                chars.next();

                // Collect until matching '}', tracking nesting depth
                let mut var_content = String::new();
                let mut depth = 1;
                for c in chars.by_ref() {
                    if c == '{' && var_content.ends_with('$') {
                        depth += 1;
                        var_content.push(c);
                    } else if c == '}' {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                        var_content.push(c);
                    } else {
                        var_content.push(c);
                    }
                }

                // Parse the content for special syntax
                let part = if let Some(name) = var_content.strip_prefix('#') {
                    // Variable length: ${#VAR} / ${#path[sub]}
                    StringPart::VarLength(parse_varpath(&format!("${{{name}}}")))
                } else if var_content.starts_with("__ARITH:") && var_content.ends_with("__") {
                    // Arithmetic expression: ${__ARITH:expr__}
                    let expr = var_content
                        .strip_prefix("__ARITH:")
                        .and_then(|s| s.strip_suffix("__"))
                        .unwrap_or("");
                    StringPart::Arithmetic(expr.to_string())
                } else if let Some(colon_idx) = find_default_separator_in_content(&var_content) {
                    // Variable with default: ${VAR:-default} - recursively parse the default
                    let path = parse_varpath(&format!("${{{}}}", &var_content[..colon_idx]));
                    let default_str = &var_content[colon_idx + 2..];
                    let default = parse_interpolated_string(&unquote_default_word(default_str))?;
                    StringPart::VarWithDefault { path, default }
                } else {
                    // Regular variable: ${VAR} or ${VAR.field}
                    StringPart::Var(parse_varpath(&format!("${{{}}}", var_content)))
                };
                parts.push(part);
            } else if chars.peek().map(|c| c.is_ascii_digit()).unwrap_or(false) {
                // Positional parameter $0-$9
                if !current_text.is_empty() {
                    parts.push(StringPart::Literal(std::mem::take(&mut current_text)));
                }
                if let Some(digit) = chars.next() {
                    let n = digit.to_digit(10).unwrap_or(0) as usize;
                    parts.push(StringPart::Positional(n));
                }
            } else if chars.peek() == Some(&'@') {
                // All arguments $@
                if !current_text.is_empty() {
                    parts.push(StringPart::Literal(std::mem::take(&mut current_text)));
                }
                chars.next(); // consume '@'
                parts.push(StringPart::AllArgs);
            } else if chars.peek() == Some(&'#') {
                // Argument count $#
                if !current_text.is_empty() {
                    parts.push(StringPart::Literal(std::mem::take(&mut current_text)));
                }
                chars.next(); // consume '#'
                parts.push(StringPart::ArgCount);
            } else if chars.peek() == Some(&'?') {
                // Last exit code $?
                if !current_text.is_empty() {
                    parts.push(StringPart::Literal(std::mem::take(&mut current_text)));
                }
                chars.next(); // consume '?'
                parts.push(StringPart::LastExitCode);
            } else if chars.peek() == Some(&'$') {
                // Current PID $$
                if !current_text.is_empty() {
                    parts.push(StringPart::Literal(std::mem::take(&mut current_text)));
                }
                chars.next(); // consume second '$'
                parts.push(StringPart::CurrentPid);
            } else if chars.peek().map(|c| c.is_ascii_alphabetic() || *c == '_').unwrap_or(false) {
                // Simple variable reference $NAME
                if !current_text.is_empty() {
                    parts.push(StringPart::Literal(std::mem::take(&mut current_text)));
                }

                // Collect identifier characters
                let mut var_name = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_alphanumeric() || c == '_' {
                        if let Some(c) = chars.next() {
                            var_name.push(c);
                        }
                    } else {
                        break;
                    }
                }

                parts.push(StringPart::Var(VarPath::simple(var_name)));
            } else {
                // Literal $ (not followed by { or identifier start)
                current_text.push(ch);
            }
        } else {
            current_text.push(ch);
        }
    }

    if !current_text.is_empty() {
        parts.push(StringPart::Literal(current_text));
    }

    Ok(parts)
}

/// Parse error with location and context.
#[derive(Debug, Clone)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}

impl ParseError {
    /// Format the error against the original source, emitting a 1-indexed
    /// `line:col [parse]: <message>` prefix and a snippet of the offending
    /// line. Mirrors `ValidationIssue::format` so error reporting feels
    /// consistent across pipeline phases.
    pub fn format(&self, source: &str) -> String {
        let start = self.span.start;
        let mut line = 1usize;
        let mut col = 1usize;
        for (i, ch) in source.char_indices() {
            if i >= start {
                break;
            }
            if ch == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }
        let line_content = {
            let line_start = source[..start.min(source.len())]
                .rfind('\n')
                .map_or(0, |i| i + 1);
            let line_end = source[start.min(source.len())..]
                .find('\n')
                .map_or(source.len(), |i| start + i);
            source.get(line_start..line_end).unwrap_or("")
        };
        if line_content.is_empty() {
            format!("{}:{} [parse]: {}", line, col, self.message)
        } else {
            format!(
                "{}:{} [parse]: {}\n  | {}",
                line, col, self.message, line_content
            )
        }
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {:?}", self.message, self.span)
    }
}

impl std::error::Error for ParseError {}

/// Parse kaish source code into a Program AST.
pub fn parse(source: &str) -> Result<Program, Vec<ParseError>> {
    // Tokenize with logos
    let tokens = lexer::tokenize(source).map_err(|errs| {
        errs.into_iter()
            .map(|e| ParseError {
                span: (e.span.start..e.span.end).into(),
                message: format!("lexer error: {}", e.token),
            })
            .collect::<Vec<_>>()
    })?;

    // Convert tokens to (Token, SimpleSpan) pairs
    let tokens: Vec<(Token, Span)> = tokens
        .into_iter()
        .map(|spanned| (spanned.token, (spanned.span.start..spanned.span.end).into()))
        .collect();

    // End-of-input span
    let end_span: Span = (source.len()..source.len()).into();

    // Parse using slice-based input (like nano_rust example)
    let parser = program_parser();
    let result = parser.parse(tokens.as_slice().map(end_span, |(t, s)| (t, s)));

    let program = result.into_result().map_err(|errs| {
        errs.into_iter()
            .map(|e| ParseError {
                span: *e.span(),
                message: e.to_string(),
            })
            .collect::<Vec<_>>()
    })?;

    // Structural well-formedness checks that chumsky's grammar can't surface a
    // clean message for. A command with two stdin sources (`<`/`<<`/`<<<`)
    // would silently depend on redirect ordering at execution time, so reject
    // it here — at parse time, which (unlike validation) can never be skipped.
    if first_ambiguous_stdin(&program.statements) {
        return Err(vec![ParseError {
            // Redirects carry no AST span, so anchor at the start of the
            // source; the message is the actionable part. Precise columns
            // would require spanning `Redirect` (deferred — see docs/issues.md).
            span: (0..0).into(),
            message: "multiple stdin redirects on one command are ambiguous; \
                      use exactly one of `<`, `<<`, or `<<<`"
                .to_string(),
        }]);
    }

    Ok(program)
}

/// Parse a single statement (useful for REPL).
pub fn parse_statement(source: &str) -> Result<Stmt, Vec<ParseError>> {
    let program = parse(source)?;
    program
        .statements
        .into_iter()
        .find(|s| !matches!(s, Stmt::Empty))
        .ok_or_else(|| {
            vec![ParseError {
                span: (0..source.len()).into(),
                message: "empty input".to_string(),
            }]
        })
}

// ═══════════════════════════════════════════════════════════════════════════
// Parser Combinators - generic over input type
// ═══════════════════════════════════════════════════════════════════════════

/// Top-level program parser.
fn program_parser<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Program, extra::Err<Rich<'tokens, Token, Span>>>
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    statement_parser()
        .repeated()
        .collect::<Vec<_>>()
        .map(|statements| Program { statements })
}

/// Statement parser - dispatches based on leading token.
/// Supports statement-level chaining with && and ||.
fn statement_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Stmt, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive(|stmt| {
        let terminator = choice((just(Token::Newline), just(Token::Semi))).repeated();

        // break [N] - break out of N levels of loops (default 1)
        let break_stmt = just(Token::Break)
            .ignore_then(
                select! { Token::Int(n) => n as usize }.or_not()
            )
            .map(Stmt::Break);

        // continue [N] - continue to next iteration, skipping N levels (default 1)
        let continue_stmt = just(Token::Continue)
            .ignore_then(
                select! { Token::Int(n) => n as usize }.or_not()
            )
            .map(Stmt::Continue);

        // return [expr] - return from a tool
        let return_stmt = just(Token::Return)
            .ignore_then(primary_expr_parser().or_not())
            .map(|e| Stmt::Return(e.map(Box::new)));

        // exit [code] - exit the script
        let exit_stmt = just(Token::Exit)
            .ignore_then(primary_expr_parser().or_not())
            .map(|e| Stmt::Exit(e.map(Box::new)));

        // set command: `set -e`, `set +e`, `set` (no args), `set -o pipefail`
        // This must come BEFORE assignment_parser to handle `set -e` vs `X=value`
        //
        // Strategy: Use lookahead to check what follows `set`:
        // - If followed by a flag (-e, --long, +e): parse as set command
        // - If followed by identifier NOT followed by =: parse as set command (e.g., `set pipefail`)
        // - If followed by nothing (end/newline/semi): parse as set command
        // - If followed by identifier then =: let assignment_parser handle it
        let set_flag_arg = choice((
            select! { Token::ShortFlag(f) => Arg::ShortFlag(f) },
            select! { Token::LongFlag(f) => Arg::LongFlag(f) },
            // PlusFlag for +e, +x etc. - convert to positional arg with + prefix
            select! { Token::PlusFlag(f) => Arg::Positional(Expr::Literal(Value::String(format!("+{}", f)))) },
        ));

        // Option value after `-o`/`+o`: a size literal (`8K`, `1M`) or raw
        // byte count. Stringified so `set.rs` can `parse_size` the
        // `output-limit=<value>` it reconstructs.
        let option_value_str = select! {
            Token::NumberIdent(s) => s,
            Token::Int(n) => n.to_string(),
            Token::Ident(s) => s,
        };

        // `-o output-limit=8K`: `name`, `=`, `value` are three tokens; fold
        // them back into a single `name=value` positional (the form `set.rs`
        // and bash both expect). Without this the `=` is a parse error.
        let set_option_assign = ident_parser()
            .then_ignore(just(Token::Eq))
            .then(option_value_str)
            .map(|(name, value)| {
                Arg::Positional(Expr::Literal(Value::String(format!("{name}={value}"))))
            });

        // Quoted option such as `set -o "output-limit=8K"`: the whole thing is
        // one string token. Accept it as a positional so the quoted form works
        // too (agents reach for it after the unquoted form trips a shell lint).
        let set_quoted_arg = select! {
            Token::String(s) => Arg::Positional(Expr::Literal(Value::String(s))),
            Token::SingleString(s) => Arg::Positional(Expr::Literal(Value::String(s))),
        };

        // set with flags: `set -e`, `set -e -u -o pipefail`
        let set_with_flags = just(Token::Set)
            .then(set_flag_arg)
            .then(
                choice((
                    set_flag_arg,
                    // `-o name=value` (try before the bare-ident arm).
                    set_option_assign,
                    set_quoted_arg,
                    // Identifiers like 'pipefail' after -o
                    ident_parser().map(|name| Arg::Positional(Expr::Literal(Value::String(name)))),
                ))
                .repeated()
                .collect::<Vec<_>>(),
            )
            .map(|((_, first_arg), mut rest_args)| {
                let mut args = vec![first_arg];
                args.append(&mut rest_args);
                Stmt::Command(Command {
                    name: "set".to_string(),
                    args,
                    redirects: vec![],
                })
            });

        // set with no args: `set` alone (shows settings)
        // Must be followed by newline, semicolon, end of input, or a chaining operator (&&, ||)
        let set_no_args = just(Token::Set)
            .then(
                choice((
                    just(Token::Newline).to(()),
                    just(Token::Semi).to(()),
                    just(Token::And).to(()),
                    just(Token::Or).to(()),
                    end(),
                ))
                .rewind(),
            )
            .map(|_| Stmt::Command(Command {
                name: "set".to_string(),
                args: vec![],
                redirects: vec![],
            }));

        // Try set_with_flags first (requires at least one flag)
        // Then try set_no_args (no args, followed by terminator)
        // If neither matches, fall through to assignment_parser
        let set_command = set_with_flags.or(set_no_args);

        // Inline env prefix: `NAME=value ... command`. One or more bash-style
        // assignments immediately followed by a command/pipeline scopes those
        // assignments to that command only (Stmt::EnvScoped). With no command
        // following, this alternative fails and we fall through to a plain,
        // persistent assignment. Must precede `assignment_parser` so the
        // prefixed-command form wins when a command follows.
        // Env-prefix assignment stays BARE-IDENT ONLY — a subscripted target
        // (`user[email]=x cmd`) is illegal here, not just unsupported:
        // structured values can't cross the process boundary anyway (see
        // docs/arrays-and-hashes.md, "Assignment lvalues"). Using
        // `ident_parser()` directly (not `lvalue_path_parser()`) means a
        // bracket run before `=` in this position never gets a chance to
        // parse as a path — it either isn't there (plain ident) or the
        // lexer's lvalue suppression fires and the stray `LBracket` fails
        // this parser, falling through to a real parse error instead of
        // silently being accepted.
        let env_prefix_assign = ident_parser()
            .then_ignore(just(Token::Eq))
            .then(value_expr_parser())
            .map(|(name, value)| Assignment { path: VarPath::simple(name), value, local: false });
        let env_scoped = env_prefix_assign
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .then(pipeline_parser().map(pipeline_into_stmt))
            .map(|(assignments, body)| Stmt::EnvScoped {
                assignments,
                body: Box::new(body),
            });

        // Base statement (without chaining)
        let base_statement = choice((
            just(Token::Newline).to(Stmt::Empty),
            set_command,
            env_scoped,
            assignment_parser().map(Stmt::Assignment),
            // Shell-style functions (use $1, $2 positional params)
            posix_function_parser(stmt.clone()).map(Stmt::ToolDef),  // name() { }
            bash_function_parser(stmt.clone()).map(Stmt::ToolDef),   // function name { }
            if_parser(stmt.clone()).map(Stmt::If),
            for_parser(stmt.clone()).map(Stmt::For),
            while_parser(stmt.clone()).map(Stmt::While),
            case_parser(stmt.clone()).map(Stmt::Case),
            break_stmt,
            continue_stmt,
            return_stmt,
            exit_stmt,
            test_expr_stmt_parser().map(Stmt::Test),
            // Note: 'true' and 'false' are handled by command_parser/pipeline_parser
            pipeline_parser().map(pipeline_into_stmt),
        ))
        .boxed();

        // Statement chaining: `&&` and `||` have EQUAL precedence and associate
        // left-to-right (POSIX), so `true || echo A && echo B` parses as
        // `((true || echo A) && echo B)` and prints B — NOT `&&`-binds-tighter.
        // A single left fold over a stream of (operator, statement) pairs gives
        // that: each operator wraps the accumulated left side with the next stmt.
        base_statement
            .clone()
            .foldl(
                choice((
                    just(Token::And).to(true), // true = &&
                    just(Token::Or).to(false), // false = ||
                ))
                .then(base_statement)
                .repeated(),
                |left, (is_and, right): (bool, Stmt)| {
                    if is_and {
                        Stmt::AndChain {
                            left: Box::new(left),
                            right: Box::new(right),
                        }
                    } else {
                        Stmt::OrChain {
                            left: Box::new(left),
                            right: Box::new(right),
                        }
                    }
                },
            )
            .then_ignore(terminator)
    })
}

/// One bracket subscript in an assignment LHS: `[0]`, `[email]`, `["a b"]`,
/// `[$k]`, `[0:2]`. Reached only via the lexer's lvalue suppression (see
/// `lexer::flush_glob_run`), which keeps a bracket run immediately followed by
/// `=` from fusing into a `GlobWord` — so this always sees primitive
/// `LBracket`/`RBracket` tokens around one of a handful of interior shapes.
/// Interior classification reuses [`parse_subscript`] (string-based) for the
/// `Ident` case (bareword key or colon-fused slice like `0:2`/`0:-1` — colon
/// merge already ran ahead of this parser) so read and write share one
/// subscript grammar; the other interior kinds map straight to their segment.
fn lvalue_subscript_parser<'tokens, I>(
) -> impl Parser<'tokens, I, VarSegment, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    let interior = choice((
        select! { Token::SimpleVarRef(name) => VarSegment::Dynamic(name) },
        select! { Token::String(s) => VarSegment::Key(s) },
        select! { Token::SingleString(s) => VarSegment::Key(s) },
        select! { Token::Int(n) => VarSegment::Index(n) },
        select! { Token::Ident(s) => parse_subscript(&s) },
    ));

    just(Token::LBracket)
        .ignore_then(interior)
        .then_ignore(just(Token::RBracket))
        .labelled("subscript")
}

/// An lvalue path: `NAME`, `NAME[sub]`, `NAME[sub][sub]…`. The root is a
/// plain identifier; zero or more bracket subscripts follow with no
/// whitespace expected between them (the lexer only suppresses fusion for a
/// bracket run immediately followed by `=`, so this is the only shape that
/// reaches here already split into primitive tokens).
fn lvalue_path_parser<'tokens, I>(
) -> impl Parser<'tokens, I, VarPath, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    ident_parser()
        .then(lvalue_subscript_parser().repeated().collect::<Vec<_>>())
        .map(|(name, subscripts)| {
            let mut segments = vec![VarSegment::Field(name)];
            segments.extend(subscripts);
            VarPath { segments }
        })
        .labelled("lvalue path")
}

/// Assignment: `NAME=value` / `NAME[sub]=value` (bash-style), or
/// `local NAME = value` (scoped). Bracket paths are lvalues here — see
/// `docs/arrays-and-hashes.md` ("Assignment lvalues") — and are resolved at
/// runtime by `Scope::walk_write`, sharing the read resolver's per-hop
/// classification.
fn assignment_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Assignment, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // local NAME = value (with spaces around =)
    let local_assignment = just(Token::Local)
        .ignore_then(lvalue_path_parser())
        .then_ignore(just(Token::Eq))
        .then(value_expr_parser())
        .map(|(path, value)| Assignment {
            path,
            value,
            local: true,
        });

    // Bash-style: NAME=value / NAME[sub]=value (no spaces around =)
    // The lexer produces IDENT (LBRACKET ... RBRACKET)* EQ EXPR, so we parse it here
    let bash_assignment = lvalue_path_parser()
        .then_ignore(just(Token::Eq))
        .then(value_expr_parser())
        .map(|(path, value)| Assignment {
            path,
            value,
            local: false,
        });

    choice((local_assignment, bash_assignment))
        .labelled("assignment")
        .boxed()
}

/// POSIX-style function: `name() { body }`
///
/// Produces a ToolDef with empty params - uses positional params ($1, $2, etc.)
fn posix_function_parser<'tokens, I, S>(
    stmt: S,
) -> impl Parser<'tokens, I, ToolDef, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
    S: Parser<'tokens, I, Stmt, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
{
    ident_parser()
        .then_ignore(just(Token::LParen))
        .then_ignore(just(Token::RParen))
        .then_ignore(just(Token::LBrace))
        .then_ignore(just(Token::Newline).repeated())
        .then(
            stmt.repeated()
                .collect::<Vec<_>>()
                .map(|stmts| stmts.into_iter().filter(|s| !matches!(s, Stmt::Empty)).collect()),
        )
        .then_ignore(just(Token::Newline).repeated())
        .then_ignore(just(Token::RBrace))
        .map(|(name, body)| ToolDef { name, params: vec![], body })
        .labelled("POSIX function")
        .boxed()
}

/// Bash-style function: `function name { body }` (without parens)
///
/// Produces a ToolDef with empty params - uses positional params ($1, $2, etc.)
fn bash_function_parser<'tokens, I, S>(
    stmt: S,
) -> impl Parser<'tokens, I, ToolDef, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
    S: Parser<'tokens, I, Stmt, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
{
    just(Token::Function)
        .ignore_then(ident_parser())
        .then_ignore(just(Token::LBrace))
        .then_ignore(just(Token::Newline).repeated())
        .then(
            stmt.repeated()
                .collect::<Vec<_>>()
                .map(|stmts| stmts.into_iter().filter(|s| !matches!(s, Stmt::Empty)).collect()),
        )
        .then_ignore(just(Token::Newline).repeated())
        .then_ignore(just(Token::RBrace))
        .map(|(name, body)| ToolDef { name, params: vec![], body })
        .labelled("bash function")
        .boxed()
}

/// If statement: `if COND; then STMTS [elif COND; then STMTS]* [else STMTS] fi`
///
/// elif clauses are desugared to nested if/else:
///   `if A; then X elif B; then Y else Z fi`
/// becomes:
///   `if A; then X else { if B; then Y else Z fi } fi`
fn if_parser<'tokens, I, S>(
    stmt: S,
) -> impl Parser<'tokens, I, IfStmt, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
    S: Parser<'tokens, I, Stmt, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
{
    // Parse a single branch: condition + then statements
    let branch = condition_parser()
        .then_ignore(just(Token::Semi).or_not())
        .then_ignore(just(Token::Newline).repeated())
        .then_ignore(just(Token::Then))
        .then_ignore(just(Token::Newline).repeated())
        .then(
            stmt.clone()
                .repeated()
                .collect::<Vec<_>>()
                .map(|stmts: Vec<Stmt>| {
                    stmts
                        .into_iter()
                        .filter(|s| !matches!(s, Stmt::Empty))
                        .collect::<Vec<_>>()
                }),
        );

    // Parse elif branches: `elif COND; then STMTS`
    let elif_branch = just(Token::Elif)
        .ignore_then(condition_parser())
        .then_ignore(just(Token::Semi).or_not())
        .then_ignore(just(Token::Newline).repeated())
        .then_ignore(just(Token::Then))
        .then_ignore(just(Token::Newline).repeated())
        .then(
            stmt.clone()
                .repeated()
                .collect::<Vec<_>>()
                .map(|stmts: Vec<Stmt>| {
                    stmts
                        .into_iter()
                        .filter(|s| !matches!(s, Stmt::Empty))
                        .collect::<Vec<_>>()
                }),
        );

    // Parse else branch: `else STMTS`
    let else_branch = just(Token::Else)
        .ignore_then(just(Token::Newline).repeated())
        .ignore_then(stmt.repeated().collect::<Vec<_>>())
        .map(|stmts: Vec<Stmt>| {
            stmts
                .into_iter()
                .filter(|s| !matches!(s, Stmt::Empty))
                .collect::<Vec<_>>()
        });

    just(Token::If)
        .ignore_then(branch)
        .then(elif_branch.repeated().collect::<Vec<_>>())
        .then(else_branch.or_not())
        .then_ignore(just(Token::Fi))
        .map(|(((condition, then_branch), elif_branches), else_branch)| {
            // Build nested if/else structure from elif branches
            build_if_chain(condition, then_branch, elif_branches, else_branch)
        })
        .labelled("if statement")
        .boxed()
}

/// Build a nested IfStmt chain from elif branches.
///
/// Transforms:
///   if A then X elif B then Y elif C then Z else W fi
/// Into:
///   IfStmt { cond: A, then: X, else: Some([IfStmt { cond: B, then: Y, else: Some([IfStmt { cond: C, then: Z, else: Some(W) }]) }]) }
fn build_if_chain(
    condition: Expr,
    then_branch: Vec<Stmt>,
    mut elif_branches: Vec<(Expr, Vec<Stmt>)>,
    else_branch: Option<Vec<Stmt>>,
) -> IfStmt {
    if elif_branches.is_empty() {
        // No elif, just if/else
        IfStmt {
            condition: Box::new(condition),
            then_branch,
            else_branch,
        }
    } else {
        // Pop the first elif and recursively build the rest
        let (elif_cond, elif_then) = elif_branches.remove(0);
        let nested_if = build_if_chain(elif_cond, elif_then, elif_branches, else_branch);
        IfStmt {
            condition: Box::new(condition),
            then_branch,
            else_branch: Some(vec![Stmt::If(nested_if)]),
        }
    }
}

/// For loop: `for VAR in ITEMS; do STMTS done`
fn for_parser<'tokens, I, S>(
    stmt: S,
) -> impl Parser<'tokens, I, ForLoop, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
    S: Parser<'tokens, I, Stmt, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
{
    just(Token::For)
        .ignore_then(ident_parser())
        .then_ignore(just(Token::In))
        .then(expr_parser().repeated().at_least(1).collect::<Vec<_>>())
        .then_ignore(just(Token::Semi).or_not())
        .then_ignore(just(Token::Newline).repeated())
        .then_ignore(just(Token::Do))
        .then_ignore(just(Token::Newline).repeated())
        .then(
            stmt.repeated()
                .collect::<Vec<_>>()
                .map(|stmts| stmts.into_iter().filter(|s| !matches!(s, Stmt::Empty)).collect()),
        )
        .then_ignore(just(Token::Done))
        .map(|((variable, items), body)| ForLoop {
            variable,
            items,
            body,
        })
        .labelled("for loop")
        .boxed()
}

/// While loop: `while condition; do ...; done`
fn while_parser<'tokens, I, S>(
    stmt: S,
) -> impl Parser<'tokens, I, WhileLoop, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
    S: Parser<'tokens, I, Stmt, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
{
    just(Token::While)
        .ignore_then(condition_parser())
        .then_ignore(just(Token::Semi).or_not())
        .then_ignore(just(Token::Newline).repeated())
        .then_ignore(just(Token::Do))
        .then_ignore(just(Token::Newline).repeated())
        .then(
            stmt.repeated()
                .collect::<Vec<_>>()
                .map(|stmts| stmts.into_iter().filter(|s| !matches!(s, Stmt::Empty)).collect()),
        )
        .then_ignore(just(Token::Done))
        .map(|(condition, body)| WhileLoop {
            condition: Box::new(condition),
            body,
        })
        .labelled("while loop")
        .boxed()
}

/// Case statement: `case expr in pattern) commands ;; esac`
///
/// Supports:
/// - Single patterns: `pattern) commands ;;`
/// - Multiple patterns: `pattern1|pattern2) commands ;;`
/// - Optional leading `(` before patterns: `(pattern) commands ;;`
fn case_parser<'tokens, I, S>(
    stmt: S,
) -> impl Parser<'tokens, I, CaseStmt, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
    S: Parser<'tokens, I, Stmt, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
{
    // Pattern part: individual tokens that make up a glob pattern
    // e.g., "*.rs" is Star + Dot + Ident("rs")
    let pattern_part = choice((
        select! { Token::GlobWord(s) => s },
        select! { Token::Ident(s) => s },
        select! { Token::NumberIdent(s) => s },
        select! { Token::DashNumWord(s) => s },
        select! { Token::AtWord(s) => s },
        select! { Token::DottedIdent(s) => s },
        select! { Token::String(s) => s },
        select! { Token::SingleString(s) => s },
        select! { Token::Int(n) => n.to_string() },
        select! { Token::Star => "*".to_string() },
        select! { Token::Question => "?".to_string() },
        select! { Token::Dot => ".".to_string() },
        select! { Token::DotDot => "..".to_string() },
        select! { Token::Tilde => "~".to_string() },
        select! { Token::TildePath(s) => s },
        select! { Token::RelativePath(s) => s },
        select! { Token::DotSlashPath(s) => s },
        select! { Token::Path(p) => p },
        select! { Token::VarRef(v) => v },
        select! { Token::SimpleVarRef(v) => format!("${}", v) },
        // Character class: [a-z], [!abc], [^abc], etc.
        just(Token::LBracket)
            .ignore_then(
                choice((
                    select! { Token::Ident(s) => s },
                    select! { Token::Int(n) => n.to_string() },
                    just(Token::Colon).to(":".to_string()),
                    // Negation: ! or ^ at start of char class
                    just(Token::Bang).to("!".to_string()),
                    // Range like a-z
                    select! { Token::ShortFlag(s) => format!("-{}", s) },
                ))
                .repeated()
                .at_least(1)
                .collect::<Vec<String>>()
            )
            .then_ignore(just(Token::RBracket))
            .map(|parts| format!("[{}]", parts.join(""))),
        // Brace expansion: {a,b,c} or {js,ts}
        just(Token::LBrace)
            .ignore_then(
                choice((
                    select! { Token::Ident(s) => s },
                    select! { Token::Int(n) => n.to_string() },
                ))
                .separated_by(just(Token::Comma))
                .at_least(1)
                .collect::<Vec<String>>()
            )
            .then_ignore(just(Token::RBrace))
            .map(|parts| format!("{{{}}}", parts.join(","))),
    ));

    // A complete pattern is one or more pattern parts joined together
    // e.g., "*.rs" = Star + Dot + Ident
    let pattern = pattern_part
        .repeated()
        .at_least(1)
        .collect::<Vec<String>>()
        .map(|parts| parts.join(""))
        .labelled("case pattern");

    // Multiple patterns separated by pipe: `pattern1 | pattern2`
    let patterns = pattern
        .separated_by(just(Token::Pipe))
        .at_least(1)
        .collect::<Vec<String>>()
        .labelled("case patterns");

    // Branch: `[( ] patterns ) commands ;;`
    let branch = just(Token::LParen)
        .or_not()
        .ignore_then(just(Token::Newline).repeated())
        .ignore_then(patterns)
        .then_ignore(just(Token::RParen))
        .then_ignore(just(Token::Newline).repeated())
        .then(
            stmt.clone()
                .repeated()
                .collect::<Vec<_>>()
                .map(|stmts| stmts.into_iter().filter(|s| !matches!(s, Stmt::Empty)).collect()),
        )
        .then_ignore(just(Token::DoubleSemi))
        .then_ignore(just(Token::Newline).repeated())
        .map(|(patterns, body)| CaseBranch { patterns, body })
        .labelled("case branch");

    just(Token::Case)
        .ignore_then(expr_parser())
        .then_ignore(just(Token::In))
        .then_ignore(just(Token::Newline).repeated())
        .then(branch.repeated().collect::<Vec<_>>())
        .then_ignore(just(Token::Esac))
        .map(|(expr, branches)| CaseStmt { expr, branches })
        .labelled("case statement")
        .boxed()
}

/// Pipeline: `cmd | cmd | cmd [&]`
fn pipeline_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Pipeline, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    command_parser()
        .separated_by(just(Token::Pipe))
        .at_least(1)
        .collect::<Vec<_>>()
        .then(just(Token::Amp).or_not())
        .map(|(commands, bg)| Pipeline {
            commands,
            background: bg.is_some(),
        })
        .labelled("pipeline")
        .boxed()
}

/// Command: `name args... [redirects...]`
/// Command names can be identifiers, 'true', 'false', or '.' (source alias).
fn command_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Command, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // Command name can be an identifier, path, 'true', 'false', '.' (source alias), or ./path
    let command_name = choice((
        ident_parser(),
        path_parser(),
        select! { Token::DotSlashPath(s) => s },
        just(Token::True).to("true".to_string()),
        just(Token::False).to("false".to_string()),
        just(Token::Dot).to(".".to_string()),
    ));

    // NB: the "at most one stdin source per command" rule is enforced by a
    // post-parse scan in `parse()` (see `first_ambiguous_stdin`), NOT here.
    // A `try_map` rejection at this level cannot surface its own message: a
    // command like `cat <<< a <<< b` also fails the competing statement-level
    // assignment/function alternative ("expected '=', or '('"), and chumsky's
    // `choice` merge keeps that alternative's error regardless of which span
    // our custom error carries. So we accept the command here and reject it
    // structurally after parsing, where the message is fully under our control
    // (verified empirically 2026-06-07; see docs/issues.md).
    command_name
        .then(args_list_parser())
        .then(redirect_parser(primary_expr_parser()).repeated().collect::<Vec<_>>())
        .map(|((name, args), redirects)| Command {
            name,
            args,
            redirects,
        })
        .labelled("command")
        .boxed()
}

/// Map a parsed `Pipeline` to a statement, unwrapping a single redirect-free
/// foreground command to `Stmt::Command` (the canonical shape used throughout
/// the parser). Shared by the top-level statement parser, `$()` bodies, and
/// inline env-prefix bodies so the unwrap rule lives in one place.
fn pipeline_into_stmt(p: Pipeline) -> Stmt {
    if p.commands.len() == 1 && !p.background && p.commands[0].redirects.is_empty() {
        match p.commands.into_iter().next() {
            Some(cmd) => Stmt::Command(cmd),
            None => Stmt::Empty, // unreachable (len checked) but safe
        }
    } else {
        Stmt::Pipeline(p)
    }
}

/// True if `cmd` has more than one stdin source (`<`, `<<`, `<<<`). Such a
/// command would silently depend on redirect ordering at execution time
/// (`setup_stdin_redirects` is last-wins), so `parse()` rejects it loudly.
fn command_has_ambiguous_stdin(cmd: &Command) -> bool {
    cmd.redirects
        .iter()
        .filter(|r| {
            matches!(
                r.kind,
                RedirectKind::Stdin | RedirectKind::HereDoc | RedirectKind::HereString
            )
        })
        .count()
        > 1
}

/// Find the first command anywhere in `stmts` (recursing into pipelines,
/// control-flow bodies, chains, and tool definitions) that has more than one
/// stdin source. Used by `parse()` to reject the ambiguity after parsing.
fn first_ambiguous_stdin(stmts: &[Stmt]) -> bool {
    stmts.iter().any(stmt_has_ambiguous_stdin)
}

fn stmt_has_ambiguous_stdin(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Command(c) => command_has_ambiguous_stdin(c),
        Stmt::Pipeline(p) => p.commands.iter().any(command_has_ambiguous_stdin),
        Stmt::If(i) => {
            first_ambiguous_stdin(&i.then_branch)
                || i.else_branch
                    .as_deref()
                    .is_some_and(first_ambiguous_stdin)
        }
        Stmt::For(f) => first_ambiguous_stdin(&f.body),
        Stmt::While(w) => first_ambiguous_stdin(&w.body),
        Stmt::Case(c) => c.branches.iter().any(|b| first_ambiguous_stdin(&b.body)),
        Stmt::ToolDef(t) => first_ambiguous_stdin(&t.body),
        Stmt::AndChain { left, right } | Stmt::OrChain { left, right } => {
            stmt_has_ambiguous_stdin(left) || stmt_has_ambiguous_stdin(right)
        }
        Stmt::EnvScoped { body, .. } => stmt_has_ambiguous_stdin(body),
        Stmt::Assignment(_)
        | Stmt::Break(_)
        | Stmt::Continue(_)
        | Stmt::Return(_)
        | Stmt::Exit(_)
        | Stmt::Test(_)
        | Stmt::Empty => false,
    }
}

/// True when `arg` is the bare-comma literal positional (`Expr::Literal(",")`),
/// produced by a lone `,` token in argument position.
fn is_comma_literal_arg(arg: &Arg) -> bool {
    matches!(arg, Arg::Positional(Expr::Literal(Value::String(s))) if s == ",")
}

/// Arguments list parser that handles `--` flag terminator.
///
/// After `--`, all subsequent flags are converted to positional string arguments.
fn args_list_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Vec<Arg>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // Arguments before `--` (normal parsing). Each arg is captured with its
    // source span so we can reject the silent argv-splat: two positional words
    // with no whitespace between them (`/tmp/$(echo x).txt` → 3 args). kaish does
    // no token pasting, so an unquoted interpolated word fragments into separate
    // args; the fix is to quote the whole word. Single-token words (`file.txt`,
    // `v1.2.3`) are one arg and never trigger this. See docs/issues.md #2.
    let pre_dash = arg_before_double_dash_parser()
        .map_with(|arg, e| -> (Arg, Span) { (arg, e.span()) })
        .repeated()
        .collect::<Vec<(Arg, Span)>>()
        .try_map(|args, _span| {
            for pair in args.windows(2) {
                let (prev, prev_span) = &pair[0];
                let (next, next_span) = &pair[1];
                if matches!(prev, Arg::Positional(_))
                    && matches!(next, Arg::Positional(_))
                    && prev_span.end == next_span.start
                {
                    // A bare `,` lexes as its own token, so a comma-bearing word
                    // (`cut -f1,3`, `sort -k2,2n`, `echo a,b`) trips this guard.
                    // It isn't token pasting — `,` is reserved (brace expansion,
                    // lists) — so give a comma-specific hint that teaches quoting.
                    let msg = if is_comma_literal_arg(prev) || is_comma_literal_arg(next) {
                        "an unquoted comma splits this into separate words — kaish reserves \
                         `,` (brace expansion, lists); quote a comma-bearing argument to keep \
                         it one word, e.g. cut -f \"1,3\", sort -k \"2,2n\", or echo \"a,b\""
                    } else {
                        "adjacent words with no space between them are not joined into one \
                         argument (kaish does no token pasting); quote the whole word, e.g. \
                         \"/tmp/$(echo x).txt\" or \"$dir/out.txt\""
                    };
                    return Err(Rich::custom(*next_span, msg));
                }
            }
            Ok(args.into_iter().map(|(arg, _)| arg).collect::<Vec<Arg>>())
        });

    // The `--` marker itself
    let double_dash = select! {
        Token::DoubleDash => Arg::DoubleDash,
    };

    // Arguments after `--` (flags become positional strings)
    let post_dash_arg = choice((
        // Flags become positional strings
        select! {
            Token::ShortFlag(name) => Arg::Positional(Expr::Literal(Value::String(format!("-{}", name)))),
            Token::LongFlag(name) => Arg::Positional(Expr::Literal(Value::String(format!("--{}", name)))),
        },
        // `name=value` — same WordAssign production used before `--`. Nothing
        // is special after `--` (standard shell behavior), but the
        // WordAssign→positional collapse already yields the literal
        // `"name=value"` string for commands that don't consume shell
        // assignments (like `echo`), so no separate literal-folding rule is
        // needed here.
        word_assign_arg_parser(),
        // `test`/`[` operators stay literal after `--` too (`test -- a = b`).
        test_operator_arg_parser(),
        // Everything else stays the same
        primary_expr_parser().map(Arg::Positional),
    ));

    let post_dash = post_dash_arg.repeated().collect::<Vec<_>>();

    // Combine: args_before ++ [--] ++ args_after
    pre_dash
        .then(double_dash.then(post_dash).or_not())
        .map(|(mut args, maybe_dd)| {
            if let Some((dd, post)) = maybe_dd {
                args.push(dd);
                args.extend(post);
            }
            args
        })
}

/// A statement keyword used as a plain word — its source spelling.
///
/// Lets keywords serve as the *key* of a `key=value` argv assignment, so
/// `dd if=/dev/urandom` works (`if` is `Token::If`, not an `Ident`). Safe
/// because: statement-level `if`/`for`/… are decided before arg parsing (their
/// productions precede `pipeline_parser`), `command_name` never accepts these
/// tokens, and the `key=value` rule requires the key span-adjacent to `=` — a
/// real `if <cond>` has a space and never matches. See docs/binary-data.md.
fn keyword_word<'tokens, I>(
) -> impl Parser<'tokens, I, String, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    select! {
        Token::Set => "set",
        Token::Local => "local",
        Token::If => "if",
        Token::Then => "then",
        Token::Else => "else",
        Token::Elif => "elif",
        Token::Fi => "fi",
        Token::For => "for",
        Token::While => "while",
        Token::In => "in",
        Token::Do => "do",
        Token::Done => "done",
        Token::Case => "case",
        Token::Esac => "esac",
        Token::Function => "function",
        Token::Break => "break",
        Token::Continue => "continue",
        Token::Return => "return",
        Token::Exit => "exit",
    }
    .map(|s| s.to_string())
}

/// Shell assignment in argv position: `name=value` (must not have spaces
/// around `=`). Produces `Arg::WordAssign`; the kernel routes it through
/// `tool_args.named` only for shell-assignment-accepting builtins (export,
/// alias). For every other command it materialises as a `"name=value"`
/// positional, matching bash semantics (`cat foo=bar` opens a file named
/// `foo=bar`). Shared by the pre-`--` and post-`--` argument grammars — the
/// `WordAssign`/positional collapse already gives `--`-following `a=b` the
/// literal-string behavior shell users expect, so it needs no special casing
/// after `--`.
fn word_assign_arg_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Arg, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        select! { Token::Ident(s) => s },
        keyword_word(),
    ))
    .map_with(|s, e| -> (String, Span) { (s, e.span()) })
    .then(just(Token::Eq).map_with(|_, e| -> Span { e.span() }))
    .then(primary_expr_parser().map_with(|expr, e| -> (Expr, Span) { (expr, e.span()) }))
    .try_map(|(((key, key_span), eq_span), (value, value_span)): (((String, Span), Span), (Expr, Span)), span| {
        // Check that key ends where = starts and = ends where value starts
        if key_span.end != eq_span.start || eq_span.end != value_span.start {
            Err(Rich::custom(
                span,
                "shell assignment must not have spaces around '=' (use 'key=value' not 'key = value')",
            ))
        } else {
            Ok(Arg::WordAssign { key, value })
        }
    })
}

/// The `test`/`[` comparison and negation operators (`=`, `==`, `!=`, `!`) as
/// ordinary positional argv words.
///
/// POSIX `test` is a *command*, so its operators must reach it flat as argv —
/// but kaish lexes `=`/`==`/`!=`/`!` as shell-significant tokens, so at
/// command-argument position they used to parse-error before ever reaching a
/// command (`test a = b`). This production makes each a literal-string
/// positional. It is name-agnostic: like bash, `echo a = b` prints `a = b` —
/// no command name is special-cased (that would be fragile under aliases).
///
/// Deliberately EXCLUDES the angle brackets `<` `>` `<=` `>=`: those stay
/// redirection (making them argv would shadow redirects) and remain
/// `[[ ]]`-only. Ordered after the flag/`word_assign` productions so a glued
/// `name=value` still binds as a `WordAssign` — this bare-operator rule only
/// fires once the current token IS the standalone operator (a spaced `a = b`,
/// where `word_assign`'s span-adjacency check has already declined).
fn test_operator_arg_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Arg, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    select! {
        Token::Eq => "=",
        Token::EqEq => "==",
        Token::NotEq => "!=",
        Token::Bang => "!",
    }
    .map(|s| Arg::Positional(Expr::Literal(Value::String(s.to_string()))))
}

/// Argument parser for arguments before `--` (normal flag handling).
fn arg_before_double_dash_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Arg, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // Long flag with value: --name=value
    let long_flag_with_value = select! {
        Token::LongFlag(name) => name,
    }
    .then_ignore(just(Token::Eq))
    .then(primary_expr_parser())
    .map(|(key, value)| Arg::Named { key, value });

    // Boolean long flag: --name
    let long_flag = select! {
        Token::LongFlag(name) => Arg::LongFlag(name),
    };

    // Boolean short flag: -x
    let short_flag = select! {
        Token::ShortFlag(name) => Arg::ShortFlag(name),
    };

    // Shell assignment in argv position: name=value (must not have spaces around =).
    let named = word_assign_arg_parser();

    // Positional argument
    let positional = primary_expr_parser().map(Arg::Positional);

    // The `test`/`[` operators (`=` `==` `!=` `!`) as literal positionals.
    // After the flag/`named` productions (so glued `name=value` stays a
    // WordAssign), before `positional` (which can't parse these tokens).
    let test_operator = test_operator_arg_parser();

    // Order matters: try more specific patterns first
    // Note: DoubleDash is NOT included here - it's handled by args_list_parser
    choice((
        long_flag_with_value,
        long_flag,
        short_flag,
        named,
        test_operator,
        positional,
    ))
    .boxed()
}

/// Redirect: `> file`, `>> file`, `< file`, `<< heredoc`, `2> file`, `&> file`, `2>&1`
///
/// `target` parses the file word (and here-string body). Callers pass the
/// expression parser appropriate to their context: the top-level command
/// grammar passes a fresh `primary_expr_parser()`, while `cmd_subst_parser`
/// passes its *already-recursive* `expr` handle. Threading it in (rather than
/// building `primary_expr_parser()` internally) is what lets `$(cmd > file)`
/// parse without an unbounded `cmd_subst → redirect → primary_expr → cmd_subst`
/// construction cycle.
fn redirect_parser<'tokens, I, T>(
    target: T,
) -> impl Parser<'tokens, I, Redirect, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
    T: Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
{
    // Regular redirects: >, >>, <, 2>, &>
    let regular_redirect = select! {
        Token::GtGt => RedirectKind::StdoutAppend,
        Token::Gt => RedirectKind::StdoutOverwrite,
        Token::Lt => RedirectKind::Stdin,
        Token::Stderr => RedirectKind::Stderr,
        Token::Both => RedirectKind::Both,
    }
    .then(target.clone())
    .map(|(kind, target)| Redirect { kind, target });

    // Here-doc redirect: << content
    // Quoted delimiters (<<'EOF' or <<"EOF") produce literal heredocs (no expansion).
    // Unquoted delimiters produce interpolated heredocs (variables are expanded).
    // For literal heredocs the `<<-EOF` tab stripping is applied here at parse
    // time (the body is fully known); for interpolated heredocs the stripping
    // is deferred to the interpreter so source byte offsets in `parts` stay
    // aligned with the original source for span reporting.
    let heredoc_redirect = just(Token::HereDocStart)
        .ignore_then(select! { Token::HereDoc(data) => data })
        .map(|data: HereDocData| {
            let target = if data.literal {
                let body = if data.strip_tabs {
                    crate::interpreter::strip_leading_tabs(&data.content)
                } else {
                    data.content
                };
                Expr::Literal(Value::String(body))
            } else {
                let parts = parse_interpolated_string_spanned(
                    &data.content,
                    data.body_start_offset,
                );
                // If there's only one literal part and no tab stripping is
                // needed, simplify to Expr::Literal — keeps the AST shape
                // identical to the pre-spans path for trivial bodies.
                if parts.len() == 1 && !data.strip_tabs {
                    if let StringPart::Literal(text) = &parts[0].part {
                        return Redirect {
                            kind: RedirectKind::HereDoc,
                            target: Expr::Literal(Value::String(text.clone())),
                        };
                    }
                }
                Expr::HereDocBody {
                    parts,
                    strip_tabs: data.strip_tabs,
                }
            };
            Redirect {
                kind: RedirectKind::HereDoc,
                target,
            }
        });

    // Here-string redirect: <<< word
    // The target is any single expression; kaish's existing Expr machinery
    // handles interpolation, single-quoted literals, and command substitution.
    let herestring_redirect = just(Token::HereString)
        .ignore_then(target.clone())
        .map(|target| Redirect {
            kind: RedirectKind::HereString,
            target,
        });

    // Merge stderr to stdout: 2>&1 (no target needed - implicit)
    let merge_stderr_redirect = just(Token::StderrToStdout)
        .map(|_| Redirect {
            kind: RedirectKind::MergeStderr,
            // Target is unused for MergeStderr, but we need something
            target: Expr::Literal(Value::Null),
        });

    // Merge stdout to stderr: 1>&2 or >&2 (no target needed - implicit)
    let merge_stdout_redirect = choice((
        just(Token::StdoutToStderr),
        just(Token::StdoutToStderr2),
    ))
    .map(|_| Redirect {
        kind: RedirectKind::MergeStdout,
        // Target is unused for MergeStdout, but we need something
        target: Expr::Literal(Value::Null),
    });

    choice((
        heredoc_redirect,
        herestring_redirect,
        merge_stderr_redirect,
        merge_stdout_redirect,
        regular_redirect,
    ))
    .labelled("redirect")
    .boxed()
}

/// Test expression parser for `[[ ... ]]` syntax.
///
/// Supports:
/// - File tests: `[[ -f path ]]`, `[[ -d path ]]`, etc.
/// - String tests: `[[ -z str ]]`, `[[ -n str ]]`
/// - Shape-guard tests: `[[ -list x ]]`, `[[ -record x ]]` (see
///   `docs/arrays-and-hashes.md`, decision F)
/// - Comparisons: `[[ $X == "value" ]]`, `[[ $NUM -gt 5 ]]`
/// - Compound: `[[ -f a && -d b ]]`, `[[ -z x || -n y ]]`, `[[ ! -f file ]]`
///
/// Precedence (highest to lowest): `!` > `&&` > `||`
fn test_expr_stmt_parser<'tokens, I>(
) -> impl Parser<'tokens, I, TestExpr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // File test operators: -e, -f, -d, -r, -w, -x
    let file_test_op = select! {
        Token::ShortFlag(s) if s == "e" => FileTestOp::Exists,
        Token::ShortFlag(s) if s == "f" => FileTestOp::IsFile,
        Token::ShortFlag(s) if s == "d" => FileTestOp::IsDir,
        Token::ShortFlag(s) if s == "r" => FileTestOp::Readable,
        Token::ShortFlag(s) if s == "w" => FileTestOp::Writable,
        Token::ShortFlag(s) if s == "x" => FileTestOp::Executable,
    };

    // String test operators: -z, -n, plus the shape-guard operators -list /
    // -record (value-typed tests, not path stats — same operand-evaluation
    // path as -z/-n, unlike the file_test_op family above).
    let string_test_op = select! {
        Token::ShortFlag(s) if s == "z" => StringTestOp::IsEmpty,
        Token::ShortFlag(s) if s == "n" => StringTestOp::IsNonEmpty,
        Token::ShortFlag(s) if s == "list" => StringTestOp::IsList,
        Token::ShortFlag(s) if s == "record" => StringTestOp::IsRecord,
    };

    // Comparison operators: =, ==, !=, =~, !~, >, <, >=, <=, -gt, -lt, -ge, -le, -eq, -ne
    // Note: = and == are equivalent inside [[ ]] (matching bash behavior)
    let cmp_op = choice((
        just(Token::EqEq).to(TestCmpOp::Eq),
        just(Token::Eq).to(TestCmpOp::Eq),
        just(Token::NotEq).to(TestCmpOp::NotEq),
        just(Token::Match).to(TestCmpOp::Match),
        just(Token::NotMatch).to(TestCmpOp::NotMatch),
        just(Token::Gt).to(TestCmpOp::Gt),
        just(Token::Lt).to(TestCmpOp::Lt),
        just(Token::GtEq).to(TestCmpOp::GtEq),
        just(Token::LtEq).to(TestCmpOp::LtEq),
        select! { Token::ShortFlag(s) if s == "eq" => TestCmpOp::NumEq },
        select! { Token::ShortFlag(s) if s == "ne" => TestCmpOp::NumNotEq },
        select! { Token::ShortFlag(s) if s == "gt" => TestCmpOp::NumGt },
        select! { Token::ShortFlag(s) if s == "lt" => TestCmpOp::NumLt },
        select! { Token::ShortFlag(s) if s == "ge" => TestCmpOp::NumGtEq },
        select! { Token::ShortFlag(s) if s == "le" => TestCmpOp::NumLtEq },
    ));

    // File test: -f path
    let file_test = file_test_op
        .then(primary_expr_parser())
        .map(|(op, path)| TestExpr::FileTest {
            op,
            path: Box::new(path),
        });

    // String test: -z str
    let string_test = string_test_op
        .then(primary_expr_parser())
        .map(|(op, value)| TestExpr::StringTest {
            op,
            value: Box::new(value),
        });

    // Comparison: $X == "value" or $NUM -gt 5
    let comparison = primary_expr_parser()
        .then(cmp_op)
        .then(primary_expr_parser())
        .map(|((left, op), right)| TestExpr::Comparison {
            left: Box::new(left),
            op,
            right: Box::new(right),
        });

    // Collection membership: `e in $coll` / `e not in $coll` (element-in-list,
    // key-in-record; see docs/arrays-and-hashes.md). There is no dedicated
    // `not` token — it lexes as a plain identifier, so `not_in` is matched as
    // the two-word sequence `Ident("not") In`. `not_in` must be tried before
    // `in` in the choice below so `e not in c` doesn't get parsed as `e` `in`
    // failing on the stray `not` bareword.
    let not_in = primary_expr_parser()
        .then_ignore(select! { Token::Ident(s) if s == "not" => () })
        .then_ignore(just(Token::In))
        .then(value_primary_parser())
        .map(|(left, right)| TestExpr::NotIn {
            left: Box::new(left),
            right: Box::new(right),
        });

    let in_ = primary_expr_parser()
        .then_ignore(just(Token::In))
        .then(value_primary_parser())
        .map(|(left, right)| TestExpr::In {
            left: Box::new(left),
            right: Box::new(right),
        });

    // Primary test expression (atomic - no compound operators)
    let primary_test = choice((file_test, string_test, not_in, in_, comparison));

    // Build compound expressions with proper precedence:
    // Grammar:
    //   test_expr = or_expr
    //   or_expr   = and_expr { "||" and_expr }
    //   and_expr  = unary_expr { "&&" unary_expr }
    //   unary_expr = "!" unary_expr | primary_test
    //
    // Precedence: ! (highest) > && > ||

    // Unary NOT binds tighter than `&&`/`||`, so it must recurse at the
    // unary level — `! A || B` is `(!A) || B`, NOT `!(A || B)`. The inner
    // `recursive` lets `!` chain (`! ! expr`) while bottoming out at a
    // primary test, so the bang never swallows a following `&&`/`||` operand.
    let unary = recursive(|unary| {
        let not_expr = just(Token::Bang)
            .ignore_then(unary)
            .map(|expr| TestExpr::Not { expr: Box::new(expr) });
        choice((not_expr, primary_test.clone()))
    });

    // AND level: unary && unary && ...
    let and_expr = unary.clone().foldl(
        just(Token::And).ignore_then(unary).repeated(),
        |left, right| TestExpr::And {
            left: Box::new(left),
            right: Box::new(right),
        },
    );

    // OR level: and_expr || and_expr || ...
    let compound_test = and_expr.clone().foldl(
        just(Token::Or).ignore_then(and_expr).repeated(),
        |left, right| TestExpr::Or {
            left: Box::new(left),
            right: Box::new(right),
        },
    );

    // [[ ]] is two consecutive bracket tokens (not a single TestStart token)
    // to avoid conflicts with nested array syntax like [[1, 2], [3, 4]]
    just(Token::LBracket)
        .then(just(Token::LBracket))
        .ignore_then(compound_test)
        .then_ignore(just(Token::RBracket).then(just(Token::RBracket)))
        .labelled("test expression")
        .boxed()
}

/// Condition parser: supports [[ ]] test expressions and commands with && / || chaining.
///
/// Shell semantics: conditions are commands whose exit codes determine truthiness.
/// - `if true; then` → runs `true` builtin, exit code 0 = truthy
/// - `if grep -q pattern file; then` → runs command, checks exit code
/// - `if a && b; then` → runs `a`, if exit 0, runs `b`
///
/// Use `[[ ]]` for comparisons: `if [[ $X -gt 5 ]]; then`
///
/// Grammar (with precedence - && binds tighter than ||):
///   condition = or_expr
///   or_expr   = and_expr { "||" and_expr }
///   and_expr  = base { "&&" base }
///   base      = test_expr | command
fn condition_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // [[ ]] test expression - wrap as Expr::Test
    let test_expr_condition = test_expr_stmt_parser().map(|test| Expr::Test(Box::new(test)));

    // Command as condition (includes true/false as command names)
    // The command's exit code determines truthiness (0 = true, non-zero = false)
    let command_condition = command_parser().map(Expr::Command);

    // Base: test expr OR command
    let base = choice((test_expr_condition, command_condition));

    // && has higher precedence than ||
    // First chain with && (higher precedence)
    let and_expr = base.clone().foldl(
        just(Token::And).ignore_then(base).repeated(),
        |left, right| Expr::BinaryOp {
            left: Box::new(left),
            op: BinaryOp::And,
            right: Box::new(right),
        },
    );

    // Then chain with || (lower precedence)
    and_expr
        .clone()
        .foldl(
            just(Token::Or).ignore_then(and_expr).repeated(),
            |left, right| Expr::BinaryOp {
                left: Box::new(left),
                op: BinaryOp::Or,
                right: Box::new(right),
            },
        )
        .labelled("condition")
        .boxed()
}

/// Expression parser - supports && and || binary operators.
///
/// Used by `for`-head items (among others), which must stay `$()`-only
/// (bare `$VAR` splice is rejected upstream by validator E012 — see
/// docs/LANGUAGE.md) and must NOT gain collection literals later. Do not
/// reroute this to the value seam.
fn expr_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // For now, just primary expressions. Can extend for && / || later if needed.
    primary_expr_parser()
}

/// Value-position expression parser (assignment RHS: bash-style, `local`,
/// and env-prefix). Adds collection literals on top of everything
/// `primary_expr_parser` covers, so they appear on assignment RHS but never
/// in argv or `for`-head items (`expr_parser`, above, stays untouched).
fn value_expr_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    value_literal_parser()
}

/// Value-position primary parser (`in`/`not in` RHS operand only — the
/// collection being tested for membership; the left needle stays on
/// `primary_expr_parser`). Same grammar as `value_expr_parser`; kept as a
/// separate name because the two seams are conceptually distinct call sites
/// (see PR-A) even though they currently share an implementation.
fn value_primary_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    value_literal_parser()
}

/// The value-position grammar: list/record literals (tried first, so a
/// `[`/`{` at value position is always a literal — never a bareword/glob),
/// falling back to everything `primary_expr_parser` covers ($(), `$VAR`,
/// scalars, …). `recursive` lets literal interiors reference this same
/// grammar, so nesting (`{tags: [a b], meta: {active: true}}`) and spread
/// (`[...$xs date]`) both parse.
///
/// The lexer guarantees a `[`/`{` reaching here at value position was never
/// fused into a `GlobWord`/colon-joined `Ident` (see
/// `lexer::compute_value_context`), so this choice never needs to "unfuse"
/// anything — it just sees primitive bracket/brace tokens.
fn value_literal_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive(|value| {
        choice((
            list_literal_parser(value.clone()),
            record_literal_parser(value.clone()),
            primary_expr_parser(),
        ))
    })
    .boxed()
}

/// List literal: `[a b c]`, `[]`, `[...$xs date]`. Elements may be separated
/// by whitespace alone, commas, newlines, or any mix — all optional and
/// interchangeable (see docs/arrays-and-hashes.md, "Commas optional in BOTH
/// lists and records") — and newlines are consumed rather than treated as
/// statement terminators, so a multi-line literal doesn't end the assignment
/// early. A bare element nests as ONE item; `...` flattens a list operand's
/// elements into this one (spread).
fn list_literal_parser<'tokens, I, V>(
    value: V,
) -> impl Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
    V: Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
{
    let spread_elem = just(Token::DotDotDot)
        .ignore_then(value.clone())
        .map(ListElem::Spread);
    let item_elem = value.map(ListElem::Item);
    let elem = choice((spread_elem, item_elem));

    let sep = choice((just(Token::Comma).to(()), just(Token::Newline).to(()))).repeated();

    just(Token::LBracket)
        .ignore_then(just(Token::Newline).repeated())
        .ignore_then(elem.then_ignore(sep).repeated().collect::<Vec<_>>())
        .then_ignore(just(Token::RBracket))
        .map(Expr::ListLiteral)
        .labelled("list literal")
}

/// Record literal: `{name: amy, role: maintainer}`, `{port:8080}` (the
/// colon-fusion exemption in the lexer means both spellings reach here as
/// the same three tokens). Keys are a bareword (`Ident`) or a quoted string
/// (for anything that isn't a bareword, e.g. `{"content-type": x}`); values
/// are the full recursive value grammar, so nested literals work. Entries
/// separate the same way list elements do (comma/newline/whitespace, all
/// optional) — including multi-line literals with a trailing comma.
fn record_literal_parser<'tokens, I, V>(
    value: V,
) -> impl Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
    V: Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
{
    let bare_key = select! { Token::Ident(s) => RecordKey::Bare(s) };
    // A double-quoted key interpolates like any double-quoted string ({"$k": v}
    // resolves $k at eval time — it used to silently create a literal "$k"
    // key); a pure-literal result folds back to Quoted so the common case
    // carries no eval overhead. Single quotes stay verbatim — the escape hatch
    // for a literal `$` in a key.
    let double_key = select! { Token::String(s) => s }.try_map(|s, span| {
        let parts = parse_interpolated_string(&s)
            .map_err(|e| Rich::custom(span, format!("record key: {e}")))?;
        Ok(match parts.as_slice() {
            [] => RecordKey::Quoted(String::new()),
            [StringPart::Literal(lit)] => RecordKey::Quoted(lit.clone()),
            _ => RecordKey::Interpolated(parts),
        })
    });
    let single_key = select! { Token::SingleString(s) => RecordKey::Quoted(s) };
    let key = choice((double_key, single_key, bare_key)).labelled("record key");

    let entry = key
        .then_ignore(just(Token::Colon))
        .then(value)
        .map(|(key, value)| RecordEntry { key, value });

    let sep = choice((just(Token::Comma).to(()), just(Token::Newline).to(()))).repeated();

    just(Token::LBrace)
        .ignore_then(just(Token::Newline).repeated())
        .ignore_then(entry.then_ignore(sep).repeated().collect::<Vec<_>>())
        .then_ignore(just(Token::RBrace))
        .map(Expr::RecordLiteral)
        .labelled("record literal")
}

/// Primary expression: literal, variable reference, command substitution, or bare identifier.
///
/// Uses `recursive` to support nested command substitution like `$(echo $(date))`.
fn primary_expr_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // Positional parameters: $0-$9, $@, $#, ${#VAR}, $?, $$
    let positional = select! {
        Token::Positional(n) => Expr::Positional(n),
        Token::AllArgs => Expr::AllArgs,
        Token::ArgCount => Expr::ArgCount,
        Token::VarLength(name) => Expr::VarLength(parse_varpath(&format!("${{{name}}}"))),
        Token::LastExitCode => Expr::LastExitCode,
        Token::CurrentPid => Expr::CurrentPid,
    };

    // Arithmetic expression: $((expr)) - preprocessed into Arithmetic token
    let arithmetic = select! {
        Token::Arithmetic(expr_str) => Expr::Arithmetic(expr_str),
    };

    // Keywords that can also be used as barewords in argument position
    // (e.g., `echo done` should work even though `done` is a keyword)
    let keyword_as_bareword = select! {
        Token::Done => "done",
        Token::Fi => "fi",
        Token::Then => "then",
        Token::Else => "else",
        Token::Elif => "elif",
        Token::In => "in",
        Token::Do => "do",
        Token::Esac => "esac",
        // `set` in argument position is the literal word (`echo set`,
        // `kaish-output-limit set 1K`); the `set` *builtin* is only matched
        // when `Token::Set` leads a statement (see `set_command`), so this
        // arm never shadows it.
        Token::Set => "set",
    }
    .map(|s| Expr::Literal(Value::String(s.to_string())));

    // Bare words starting with + or - (e.g., date +%s, cat -), and a
    // `--`-prefixed word that isn't a valid long flag (`echo ---`,
    // `echo --=x`, GH #137).
    let plus_minus_bare = select! {
        Token::PlusBare(s) => Expr::Literal(Value::String(s)),
        Token::MinusBare(s) => Expr::Literal(Value::String(s)),
        Token::MinusAlone => Expr::Literal(Value::String("-".to_string())),
        Token::DoubleDashBare(s) => Expr::Literal(Value::String(s)),
    };

    // Glob patterns: merged GlobWord tokens and bare Star/Question
    let glob_pattern = select! {
        Token::GlobWord(s) => Expr::GlobPattern(s),
        Token::Star => Expr::GlobPattern("*".to_string()),
        Token::Question => Expr::GlobPattern("?".to_string()),
    };

    recursive(|expr| {
        choice((
            positional,
            arithmetic,
            cmd_subst_parser(expr.clone()),
            var_expr_parser(),
            interpolated_string_parser(),
            literal_parser().map(Expr::Literal),
            // Glob patterns before ident (GlobWord is more specific)
            glob_pattern,
            // Bare identifiers become string literals (shell barewords)
            ident_parser().map(|s| Expr::Literal(Value::String(s))),
            // Absolute paths become string literals
            path_parser().map(|s| Expr::Literal(Value::String(s))),
            // Bare words starting with + or - (date +%s, cat -)
            // Shell navigation tokens
            select! {
                // Bare `.` in argument/expression position is the literal
                // current-directory path (`find .`, `ls .`, `echo .`). The
                // `source` alias is unaffected: `command_parser` consumes a
                // *leading* `.` as the command name before args are parsed,
                // so only a `.` that follows a command reaches here.
                Token::Dot => Expr::Literal(Value::String(".".into())),
                Token::DotDot => Expr::Literal(Value::String("..".into())),
                // Bare comma in argument position is the literal "," — the
                // `cut -d, -f2` / `tr -d ,` delimiter idiom. Brace expansion
                // consumes its separator commas inside `{…}` before reaching
                // here, and a run of comma-touching positionals (`echo 1,2,3`)
                // is still caught by the no-token-pasting guard in
                // `args_list_parser`. See docs/issues.md.
                Token::Comma => Expr::Literal(Value::String(",".into())),
                // Bare colon in argument position is the literal ":" — the
                // `awk -F: '{print $1}'` / `--field-separator=:` idiom and
                // the bash no-op `:` alias. In statement position the colon is
                // the no-op command (handled by `command_parser`); here it is
                // only reached after a command name has been parsed, so there
                // is no ambiguity with the statement form.
                Token::Colon => Expr::Literal(Value::String(":".into())),
                Token::Tilde => Expr::Literal(Value::String("~".into())),
                Token::TildePath(s) => Expr::Literal(Value::String(s)),
                Token::RelativePath(s) => Expr::Literal(Value::String(s)),
                Token::DotSlashPath(s) => Expr::Literal(Value::String(s)),
                // Digit-leading bareword (SHA prefix `019dda1c`, UUIDs).
                Token::NumberIdent(s) => Expr::Literal(Value::String(s)),
                // Hyphenated/minus-led numeric word (`2024-01-02`, `10-20`,
                // `1.5-2`, `cut -f 1-3`, `find -size -1k`) — one contiguous word.
                Token::DashNumWord(s) => Expr::Literal(Value::String(s)),
                // Leading-`@` bareword (`@scope/pkg`, `@0`, bare `@`).
                Token::AtWord(s) => Expr::Literal(Value::String(s)),
                // Dot-prefixed bareword (`.gitignore`, `.parent`, `.parent.parent`).
                // Distinct from `Token::Dot` (the source alias), which only
                // matches a bare `.` and requires whitespace before its file
                // argument.
                Token::DottedIdent(s) => Expr::Literal(Value::String(s)),
                // Job specifier `%1` for wait/kill — flows as the literal
                // string "%1"; the builtins interpret the leading `%`.
                Token::JobSpec(s) => Expr::Literal(Value::String(s)),
            },
            plus_minus_bare,
            // Keywords can be used as barewords in argument position
            keyword_as_bareword,
        ))
        .labelled("expression")
    })
    .boxed()
}

/// Variable reference: `${VAR}`, `${VAR.field}`, `${VAR:-default}`, or `$VAR` (simple form).
/// Returns Expr directly to support both VarRef and VarWithDefault.
fn var_expr_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    select! {
        Token::VarRef(raw) => parse_var_expr(&raw),
        Token::SimpleVarRef(name) => Expr::VarRef(VarPath::simple(name)),
    }
    .labelled("variable reference")
}

/// Command substitution: `$(pipeline)` - runs a pipeline and returns its result.
///
/// Accepts a recursive expression parser to support nested command substitution.
fn cmd_subst_parser<'tokens, I, E>(
    expr: E,
) -> impl Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
    E: Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
{
    // Argument parser using the recursive expression parser
    // Long flag with value: --name=value
    let long_flag_with_value = select! {
        Token::LongFlag(name) => name,
    }
    .then_ignore(just(Token::Eq))
    .then(expr.clone())
    .map(|(key, value)| Arg::Named { key, value });

    // Boolean long flag: --name
    let long_flag = select! {
        Token::LongFlag(name) => Arg::LongFlag(name),
    };

    // Boolean short flag: -x
    let short_flag = select! {
        Token::ShortFlag(name) => Arg::ShortFlag(name),
    };

    // Shell assignment in argv position: name=value (see arg_before_double_dash_parser).
    // Keyword keys (`if=`, `in=`, …) are accepted so `$(dd if=x)` parses.
    let named = choice((ident_parser(), keyword_word()))
        .then_ignore(just(Token::Eq))
        .then(expr.clone())
        .map(|(key, value)| Arg::WordAssign { key, value });

    // Positional argument
    let positional = expr.clone().map(Arg::Positional);

    let arg = choice((
        long_flag_with_value,
        long_flag,
        short_flag,
        named,
        positional,
    ));

    // Command name parser - accepts identifiers and boolean keywords (true/false are builtins)
    let command_name = choice((
        ident_parser(),
        just(Token::True).to("true".to_string()),
        just(Token::False).to("false".to_string()),
    ));

    // Command parser. Trailing redirects (`> file`, `2> file`, `>> file`, …)
    // reuse the same `redirect_parser` combinator the top-level
    // `command_parser` uses, so `$(cmd > file)` parses like any other command.
    // The redirect *target* threads the recursive `expr` handle (not a fresh
    // `primary_expr_parser()`) so the target may itself contain `$(...)` while
    // avoiding an unbounded parser-construction cycle.
    let command = command_name
        .then(arg.repeated().collect::<Vec<_>>())
        .then(
            redirect_parser(expr.clone())
                .repeated()
                .collect::<Vec<_>>(),
        )
        .map(|((name, args), redirects)| Command {
            name,
            args,
            redirects,
        });

    // Pipeline parser
    let pipeline = command
        .separated_by(just(Token::Pipe))
        .at_least(1)
        .collect::<Vec<_>>()
        .map(|commands| Pipeline {
            commands,
            background: false,
        });

    // A single pipeline becomes one statement (`$(echo x)` → one `Stmt::Command`),
    // keeping the AST shape uniform with the rest of the parser.
    let pipeline_stmt = pipeline.map(pipeline_into_stmt);

    // Statement chaining inside `$()`. `&&` and `||` have EQUAL precedence and
    // associate left-to-right (POSIX) — the same single left fold as the top
    // level (`statement_parser`), NOT `&&`-binds-tighter. This is the full
    // statement grammar a command substitution body accepts — pipelines,
    // `&&`/`||` chains, and (via the sequence below) `;`/newline separators and
    // `#` comments. Control structures (`if`/`for`/`while`/`case`) are
    // intentionally out of scope here (see docs/issues.md).
    let chained = pipeline_stmt.clone().foldl(
        choice((
            just(Token::And).to(true), // true = &&
            just(Token::Or).to(false), // false = ||
        ))
        .then(pipeline_stmt.clone())
        .repeated(),
        |left, (is_and, right): (bool, Stmt)| {
            if is_and {
                Stmt::AndChain {
                    left: Box::new(left),
                    right: Box::new(right),
                }
            } else {
                Stmt::OrChain {
                    left: Box::new(left),
                    right: Box::new(right),
                }
            }
        },
    );

    // `;` / newline separated sequence of chained statements, with optional
    // leading/trailing/interior separators (so multi-line bodies and a trailing
    // `;` or comment-induced newline parse cleanly). `#` comments lex to
    // newlines, so they are consumed here as ordinary separators.
    let separator = choice((just(Token::Newline), just(Token::Semi)));
    let body = separator
        .clone()
        .repeated()
        .ignore_then(
            chained
                .separated_by(separator.clone().repeated().at_least(1))
                .allow_trailing()
                .collect::<Vec<_>>(),
        )
        .then_ignore(separator.repeated());

    just(Token::CmdSubstStart)
        .ignore_then(body)
        .then_ignore(just(Token::RParen))
        .map(Expr::CommandSubst)
        .labelled("command substitution")
}

/// String parser - handles double-quoted strings (with interpolation) and single-quoted (literal).
fn interpolated_string_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // Double-quoted string: may contain $VAR or ${VAR} interpolation
    let double_quoted = select! {
        Token::String(s) => s,
    }
    .try_map(|s, span| {
        // Check if string contains interpolation markers (${} or $NAME) or escaped dollars
        if s.contains('$') || s.contains("__KAISH_ESCAPED_DOLLAR__") {
            // Parse interpolated parts. A syntax error inside a `$(…)` is loud
            // (Rich error at this string's span), not silently demoted to text.
            let parts = parse_interpolated_string(&s)
                .map_err(|msg| Rich::custom(span, msg))?;
            if parts.len() == 1
                && let StringPart::Literal(text) = &parts[0] {
                    return Ok(Expr::Literal(Value::String(text.clone())));
                }
            Ok(Expr::Interpolated(parts))
        } else {
            Ok(Expr::Literal(Value::String(s)))
        }
    });

    // Single-quoted string: literal, no interpolation
    let single_quoted = select! {
        Token::SingleString(s) => Expr::Literal(Value::String(s)),
    };

    choice((single_quoted, double_quoted)).labelled("string")
}

/// Literal value parser (excluding strings, which are handled by interpolated_string_parser).
fn literal_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Value, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        select! {
            Token::True => Value::Bool(true),
            Token::False => Value::Bool(false),
        },
        select! {
            Token::Int(n) => Value::Int(n),
            Token::Float(f) => Value::Float(f),
        },
    ))
    .labelled("literal")
    .boxed()
}

/// Identifier parser.
fn ident_parser<'tokens, I>(
) -> impl Parser<'tokens, I, String, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    select! {
        Token::Ident(s) => s,
    }
    .labelled("identifier")
}

/// Path parser: matches absolute paths like `/tmp/out`, `/etc/hosts`.
fn path_parser<'tokens, I>(
) -> impl Parser<'tokens, I, String, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    select! {
        Token::Path(s) => s,
    }
    .labelled("path")
}

#[cfg(test)]
#[allow(clippy::approx_constant)]
mod tests {
    use super::*;

    /// Extract the single `Command` from a one-statement `$(cmd)` body.
    fn subst_cmd(expr: &Expr) -> &Command {
        match expr {
            Expr::CommandSubst(stmts) => match stmts.as_slice() {
                [Stmt::Command(cmd)] => cmd,
                other => panic!("expected a single command in $(), got {other:?}"),
            },
            other => panic!("expected command subst, got {other:?}"),
        }
    }

    /// Extract the single `Pipeline` from a one-statement `$(a | b)` body.
    fn subst_pipeline(expr: &Expr) -> &Pipeline {
        match expr {
            Expr::CommandSubst(stmts) => match stmts.as_slice() {
                [Stmt::Pipeline(p)] => p,
                other => panic!("expected a single pipeline in $(), got {other:?}"),
            },
            other => panic!("expected command subst, got {other:?}"),
        }
    }

    #[test]
    fn parse_empty() {
        let result = parse("");
        assert!(result.is_ok());
        assert_eq!(result.expect("ok").statements.len(), 0);
    }

    #[test]
    fn parse_newlines_only() {
        let result = parse("\n\n\n");
        assert!(result.is_ok());
    }

    #[test]
    fn parse_simple_command() {
        let result = parse("echo");
        assert!(result.is_ok());
        let program = result.expect("ok");
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(&program.statements[0], Stmt::Command(_)));
    }

    #[test]
    fn parse_command_with_string_arg() {
        let result = parse(r#"echo "hello""#);
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Command(cmd) => assert_eq!(cmd.args.len(), 1),
            _ => panic!("expected Command"),
        }
    }

    #[test]
    fn parse_assignment() {
        let result = parse("X=5");
        assert!(result.is_ok());
        let program = result.expect("ok");
        assert!(matches!(&program.statements[0], Stmt::Assignment(_)));
    }

    #[test]
    fn parse_pipeline() {
        let result = parse("a | b | c");
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Pipeline(p) => assert_eq!(p.commands.len(), 3),
            _ => panic!("expected Pipeline"),
        }
    }

    #[test]
    fn parse_background_job() {
        let result = parse("cmd &");
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Pipeline(p) => assert!(p.background),
            _ => panic!("expected Pipeline with background"),
        }
    }

    #[test]
    fn parse_if_simple() {
        let result = parse("if true; then echo; fi");
        assert!(result.is_ok());
        let program = result.expect("ok");
        assert!(matches!(&program.statements[0], Stmt::If(_)));
    }

    #[test]
    fn parse_if_else() {
        let result = parse("if true; then echo; else echo; fi");
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::If(if_stmt) => assert!(if_stmt.else_branch.is_some()),
            _ => panic!("expected If"),
        }
    }

    #[test]
    fn parse_elif_simple() {
        let result = parse("if true; then echo a; elif false; then echo b; fi");
        assert!(result.is_ok(), "parse failed: {:?}", result);
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::If(if_stmt) => {
                // elif is desugared to nested if in else
                assert!(if_stmt.else_branch.is_some());
                let else_branch = if_stmt.else_branch.as_ref().unwrap();
                assert_eq!(else_branch.len(), 1);
                assert!(matches!(&else_branch[0], Stmt::If(_)));
            }
            _ => panic!("expected If"),
        }
    }

    #[test]
    fn parse_elif_with_else() {
        let result = parse("if true; then echo a; elif false; then echo b; else echo c; fi");
        assert!(result.is_ok(), "parse failed: {:?}", result);
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::If(outer_if) => {
                // Check nested structure: if -> elif -> else
                let else_branch = outer_if.else_branch.as_ref().expect("outer else");
                assert_eq!(else_branch.len(), 1);
                match &else_branch[0] {
                    Stmt::If(inner_if) => {
                        // The inner if (from elif) should have the final else
                        assert!(inner_if.else_branch.is_some());
                    }
                    _ => panic!("expected nested If from elif"),
                }
            }
            _ => panic!("expected If"),
        }
    }

    #[test]
    fn parse_multiple_elif() {
        // Shell-compatible: use [[ ]] for comparisons
        let result = parse(
            "if [[ ${X} == 1 ]]; then echo one; elif [[ ${X} == 2 ]]; then echo two; elif [[ ${X} == 3 ]]; then echo three; else echo other; fi",
        );
        assert!(result.is_ok(), "parse failed: {:?}", result);
    }

    #[test]
    fn parse_for_loop() {
        let result = parse("for X in items; do echo; done");
        assert!(result.is_ok());
        let program = result.expect("ok");
        assert!(matches!(&program.statements[0], Stmt::For(_)));
    }

    #[test]
    fn parse_brackets_not_array_literal() {
        // Array literals are no longer supported, [ is just a regular char
        let result = parse("cmd [1");
        // This should fail or parse unexpectedly - arrays are removed
        // Just verify we don't crash
        let _ = result;
    }

    #[test]
    fn parse_named_arg() {
        // Bareword key=value parses as WordAssign — the kernel decides per
        // command whether to route it to tool_args.named (export/alias) or
        // stringify to a positional (every other builtin).
        let result = parse("cmd foo=5");
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.args.len(), 1);
                assert!(matches!(&cmd.args[0], Arg::WordAssign { .. }));
            }
            _ => panic!("expected Command"),
        }
    }

    #[test]
    fn parse_short_flag() {
        let result = parse("ls -l");
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, "ls");
                assert_eq!(cmd.args.len(), 1);
                match &cmd.args[0] {
                    Arg::ShortFlag(name) => assert_eq!(name, "l"),
                    _ => panic!("expected ShortFlag"),
                }
            }
            _ => panic!("expected Command"),
        }
    }

    #[test]
    fn parse_long_flag() {
        let result = parse("git push --force");
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, "git");
                assert_eq!(cmd.args.len(), 2);
                match &cmd.args[0] {
                    Arg::Positional(Expr::Literal(Value::String(s))) => assert_eq!(s, "push"),
                    _ => panic!("expected Positional push"),
                }
                match &cmd.args[1] {
                    Arg::LongFlag(name) => assert_eq!(name, "force"),
                    _ => panic!("expected LongFlag"),
                }
            }
            _ => panic!("expected Command"),
        }
    }

    #[test]
    fn parse_long_flag_with_value() {
        let result = parse(r#"git commit --message="hello""#);
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, "git");
                assert_eq!(cmd.args.len(), 2);
                match &cmd.args[1] {
                    Arg::Named { key, value } => {
                        assert_eq!(key, "message");
                        match value {
                            Expr::Literal(Value::String(s)) => assert_eq!(s, "hello"),
                            _ => panic!("expected String value"),
                        }
                    }
                    _ => panic!("expected Named from --flag=value"),
                }
            }
            _ => panic!("expected Command"),
        }
    }

    #[test]
    fn parse_mixed_flags_and_args() {
        let result = parse(r#"git commit -m "message" --amend"#);
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, "git");
                assert_eq!(cmd.args.len(), 4);
                // commit (positional)
                assert!(matches!(&cmd.args[0], Arg::Positional(_)));
                // -m (short flag)
                match &cmd.args[1] {
                    Arg::ShortFlag(name) => assert_eq!(name, "m"),
                    _ => panic!("expected ShortFlag -m"),
                }
                // "message" (positional)
                assert!(matches!(&cmd.args[2], Arg::Positional(_)));
                // --amend (long flag)
                match &cmd.args[3] {
                    Arg::LongFlag(name) => assert_eq!(name, "amend"),
                    _ => panic!("expected LongFlag --amend"),
                }
            }
            _ => panic!("expected Command"),
        }
    }

    #[test]
    fn parse_redirect_stdout() {
        let result = parse("cmd > file");
        assert!(result.is_ok());
        let program = result.expect("ok");
        // Commands with redirects stay as Pipeline, not Command
        match &program.statements[0] {
            Stmt::Pipeline(p) => {
                assert_eq!(p.commands.len(), 1);
                let cmd = &p.commands[0];
                assert_eq!(cmd.redirects.len(), 1);
                assert!(matches!(cmd.redirects[0].kind, RedirectKind::StdoutOverwrite));
            }
            _ => panic!("expected Pipeline"),
        }
    }

    #[test]
    fn parse_var_ref() {
        let result = parse("echo ${VAR}");
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.args.len(), 1);
                assert!(matches!(&cmd.args[0], Arg::Positional(Expr::VarRef(_))));
            }
            _ => panic!("expected Command"),
        }
    }

    #[test]
    fn parse_multiple_statements() {
        let result = parse("a\nb\nc");
        assert!(result.is_ok());
        let program = result.expect("ok");
        let non_empty: Vec<_> = program.statements.iter().filter(|s| !matches!(s, Stmt::Empty)).collect();
        assert_eq!(non_empty.len(), 3);
    }

    #[test]
    fn parse_semicolon_separated() {
        let result = parse("a; b; c");
        assert!(result.is_ok());
        let program = result.expect("ok");
        let non_empty: Vec<_> = program.statements.iter().filter(|s| !matches!(s, Stmt::Empty)).collect();
        assert_eq!(non_empty.len(), 3);
    }

    #[test]
    fn parse_complex_pipeline() {
        let result = parse(r#"cat file | grep pattern="foo" | head count=10"#);
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Pipeline(p) => assert_eq!(p.commands.len(), 3),
            _ => panic!("expected Pipeline"),
        }
    }

    #[test]
    fn parse_json_as_string_arg() {
        // JSON arrays/objects should be passed as string arguments
        let result = parse(r#"cmd '[[1, 2], [3, 4]]'"#);
        assert!(result.is_ok());
    }

    #[test]
    fn parse_mixed_args() {
        let result = parse(r#"cmd pos1 key="val" pos2 num=42"#);
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Command(cmd) => assert_eq!(cmd.args.len(), 4),
            _ => panic!("expected Command"),
        }
    }

    #[test]
    fn error_unterminated_string() {
        let result = parse(r#"echo "hello"#);
        assert!(result.is_err());
    }

    #[test]
    fn error_unterminated_var_ref() {
        let result = parse("echo ${VAR");
        assert!(result.is_err());
    }

    #[test]
    fn error_missing_fi() {
        let result = parse("if true; then echo");
        assert!(result.is_err());
    }

    #[test]
    fn error_missing_done() {
        let result = parse("for X in items; do echo");
        assert!(result.is_err());
    }

    #[test]
    fn parse_lvalue_single_index() {
        let result = parse("xs[0]=9").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => {
                assert_eq!(a.name(), "xs");
                assert_eq!(
                    a.path.segments,
                    vec![VarSegment::Field("xs".into()), VarSegment::Index(0)]
                );
                assert!(!a.local);
            }
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn parse_lvalue_negative_index() {
        let result = parse("xs[-1]=7").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => assert_eq!(
                a.path.segments,
                vec![VarSegment::Field("xs".into()), VarSegment::Index(-1)]
            ),
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn parse_lvalue_bareword_key() {
        let result = parse("user[email]=x").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => assert_eq!(
                a.path.segments,
                vec![
                    VarSegment::Field("user".into()),
                    VarSegment::Key("email".into())
                ]
            ),
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn parse_lvalue_chained_keys() {
        let result = parse("s[web][port]=9000").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => assert_eq!(
                a.path.segments,
                vec![
                    VarSegment::Field("s".into()),
                    VarSegment::Key("web".into()),
                    VarSegment::Key("port".into())
                ]
            ),
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn parse_lvalue_dynamic_key() {
        let result = parse("r[$k]=v").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => assert_eq!(
                a.path.segments,
                vec![
                    VarSegment::Field("r".into()),
                    VarSegment::Dynamic("k".into())
                ]
            ),
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn parse_local_lvalue_spaced() {
        let result = parse("local xs[0] = 9").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => {
                assert!(a.local);
                assert_eq!(
                    a.path.segments,
                    vec![VarSegment::Field("xs".into()), VarSegment::Index(0)]
                );
            }
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn env_prefix_subscripted_target_is_not_captured_as_env_scoped() {
        // A subscripted target before a following command (`user[email]=x
        // echo hi`) must NOT become `Stmt::EnvScoped` — structured values
        // can't cross the process boundary, so env-prefix stays bare-ident
        // only. The lexer suppression + `env_prefix_assign` using
        // `ident_parser()` (not `lvalue_path_parser()`) means this falls
        // through to an ordinary subscripted assignment followed by an
        // independent statement — the SAME back-to-back-without-a-terminator
        // shape `X=1 Y=2` already has (kaish's `terminator` is
        // `.repeated()`, not `.at_least(1)`), not a new hazard.
        let result = parse("user={}\nuser[email]=x echo hi").unwrap();
        for stmt in &result.statements {
            assert!(
                !matches!(stmt, Stmt::EnvScoped { .. }),
                "a subscripted assignment must never be captured into EnvScoped: {stmt:?}"
            );
        }
        // Sanity: it really did parse as two independent statements.
        assert!(matches!(&result.statements[1], Stmt::Assignment(a) if a.name() == "user"));
        assert!(matches!(&result.statements[2], Stmt::Command(c) if c.name == "echo"));
    }

    #[test]
    fn parse_nested_cmd_subst() {
        // Nested command substitution is supported
        let result = parse("X=$(echo $(date))").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => {
                assert_eq!(a.name(), "X");
                let outer = subst_cmd(&a.value);
                assert_eq!(outer.name, "echo");
                // The argument should be another command substitution
                match &outer.args[0] {
                    Arg::Positional(inner_expr) => {
                        assert_eq!(subst_cmd(inner_expr).name, "date");
                    }
                    other => panic!("expected nested cmd subst arg, got {:?}", other),
                }
            }
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn parse_deeply_nested_cmd_subst() {
        // Three levels deep
        let result = parse("X=$(a $(b $(c)))").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => {
                let level1 = subst_cmd(&a.value);
                assert_eq!(level1.name, "a");
                match &level1.args[0] {
                    Arg::Positional(level2_expr) => {
                        let level2 = subst_cmd(level2_expr);
                        assert_eq!(level2.name, "b");
                        match &level2.args[0] {
                            Arg::Positional(level3_expr) => {
                                assert_eq!(subst_cmd(level3_expr).name, "c");
                            }
                            other => panic!("expected level3 cmd subst, got {:?}", other),
                        }
                    }
                    other => panic!("expected level2 cmd subst, got {:?}", other),
                }
            }
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Value Preservation Tests - These test that actual values are captured
    // ═══════════════════════════════════════════════════════════════════════════

    #[test]
    fn value_int_preserved() {
        let result = parse("X=42").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => {
                assert_eq!(a.name(), "X");
                match &a.value {
                    Expr::Literal(Value::Int(n)) => assert_eq!(*n, 42),
                    other => panic!("expected int literal, got {:?}", other),
                }
            }
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn value_negative_int_preserved() {
        let result = parse("X=-99").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::Literal(Value::Int(n)) => assert_eq!(*n, -99),
                other => panic!("expected int, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn value_float_preserved() {
        let result = parse("PI=3.14").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::Literal(Value::Float(f)) => assert!((*f - 3.14).abs() < 0.001),
                other => panic!("expected float, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn value_string_preserved() {
        let result = parse(r#"echo "hello world""#).unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, "echo");
                match &cmd.args[0] {
                    Arg::Positional(Expr::Literal(Value::String(s))) => {
                        assert_eq!(s, "hello world");
                    }
                    other => panic!("expected string arg, got {:?}", other),
                }
            }
            other => panic!("expected command, got {:?}", other),
        }
    }

    #[test]
    fn value_string_with_escapes_preserved() {
        let result = parse(r#"echo "line1\nline2""#).unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => match &cmd.args[0] {
                Arg::Positional(Expr::Literal(Value::String(s))) => {
                    assert_eq!(s, "line1\nline2");
                }
                other => panic!("expected string, got {:?}", other),
            },
            other => panic!("expected command, got {:?}", other),
        }
    }

    #[test]
    fn value_command_name_preserved() {
        let result = parse("my-command").unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => assert_eq!(cmd.name, "my-command"),
            other => panic!("expected command, got {:?}", other),
        }
    }

    #[test]
    fn value_assignment_name_preserved() {
        let result = parse("MY_VAR=1").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => assert_eq!(a.name(), "MY_VAR"),
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn value_for_variable_preserved() {
        let result = parse("for ITEM in items; do echo; done").unwrap();
        match &result.statements[0] {
            Stmt::For(f) => assert_eq!(f.variable, "ITEM"),
            other => panic!("expected for, got {:?}", other),
        }
    }

    #[test]
    fn value_varref_name_preserved() {
        let result = parse("echo ${MESSAGE}").unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => match &cmd.args[0] {
                Arg::Positional(Expr::VarRef(path)) => {
                    assert_eq!(path.segments.len(), 1);
                    let VarSegment::Field(name) = &path.segments[0] else {
                        panic!("expected root field, got {:?}", path.segments[0]);
                    };
                    assert_eq!(name, "MESSAGE");
                }
                other => panic!("expected varref, got {:?}", other),
            },
            other => panic!("expected command, got {:?}", other),
        }
    }

    #[test]
    fn value_varref_field_access_preserved() {
        let result = parse("echo ${RESULT.data}").unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => match &cmd.args[0] {
                Arg::Positional(Expr::VarRef(path)) => {
                    // A dotted `${RESULT.data}` keeps both as Field — the root
                    // and a dotted segment (resolution turns the latter into the
                    // brackets-only error).
                    assert_eq!(path.segments.len(), 2);
                    let VarSegment::Field(a) = &path.segments[0] else {
                        panic!("expected field, got {:?}", path.segments[0]);
                    };
                    let VarSegment::Field(b) = &path.segments[1] else {
                        panic!("expected field, got {:?}", path.segments[1]);
                    };
                    assert_eq!(a, "RESULT");
                    assert_eq!(b, "data");
                }
                other => panic!("expected varref, got {:?}", other),
            },
            other => panic!("expected command, got {:?}", other),
        }
    }

    #[test]
    fn value_varref_index_parsed() {
        // Bracket subscripts are now parsed into typed segments (native
        // collection access), not filtered out.
        let result = parse("echo ${ITEMS[0]}").unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => match &cmd.args[0] {
                Arg::Positional(Expr::VarRef(path)) => {
                    assert_eq!(path.segments.len(), 2);
                    let VarSegment::Field(name) = &path.segments[0] else {
                        panic!("expected root field, got {:?}", path.segments[0]);
                    };
                    assert_eq!(name, "ITEMS");
                    assert_eq!(path.segments[1], VarSegment::Index(0));
                }
                other => panic!("expected varref, got {:?}", other),
            },
            other => panic!("expected command, got {:?}", other),
        }
    }

    #[test]
    fn value_named_arg_preserved() {
        // Bareword key=value parses as WordAssign — the kernel decides per
        // command whether to route into args.named (export/alias) or
        // stringify as a positional.
        let result = parse("cmd count=42").unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, "cmd");
                match &cmd.args[0] {
                    Arg::WordAssign { key, value } => {
                        assert_eq!(key, "count");
                        match value {
                            Expr::Literal(Value::Int(n)) => assert_eq!(*n, 42),
                            other => panic!("expected int, got {:?}", other),
                        }
                    }
                    other => panic!("expected WordAssign arg, got {:?}", other),
                }
            }
            other => panic!("expected command, got {:?}", other),
        }
    }

    #[test]
    fn value_function_def_name_preserved() {
        let result = parse("greet() { echo }").unwrap();
        match &result.statements[0] {
            Stmt::ToolDef(t) => {
                assert_eq!(t.name, "greet");
                assert!(t.params.is_empty());
            }
            other => panic!("expected function def, got {:?}", other),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // New Feature Tests - Comparisons, Interpolation, Nested Structures
    // ═══════════════════════════════════════════════════════════════════════════

    #[test]
    fn parse_comparison_equals() {
        // Shell-compatible: use [[ ]] for comparisons
        let result = parse("if [[ ${X} == 5 ]]; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::Test(test) => match test.as_ref() {
                    TestExpr::Comparison { left, op, right } => {
                        assert!(matches!(left.as_ref(), Expr::VarRef(_)));
                        assert_eq!(*op, TestCmpOp::Eq);
                        match right.as_ref() {
                            Expr::Literal(Value::Int(n)) => assert_eq!(*n, 5),
                            other => panic!("expected int, got {:?}", other),
                        }
                    }
                    other => panic!("expected comparison, got {:?}", other),
                },
                other => panic!("expected test expr, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_comparison_not_equals() {
        let result = parse("if [[ ${X} != 0 ]]; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::Test(test) => match test.as_ref() {
                    TestExpr::Comparison { op, .. } => assert_eq!(*op, TestCmpOp::NotEq),
                    other => panic!("expected comparison, got {:?}", other),
                },
                other => panic!("expected test expr, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_comparison_less_than() {
        let result = parse("if [[ ${COUNT} -lt 10 ]]; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::Test(test) => match test.as_ref() {
                    TestExpr::Comparison { op, .. } => assert_eq!(*op, TestCmpOp::NumLt),
                    other => panic!("expected comparison, got {:?}", other),
                },
                other => panic!("expected test expr, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_comparison_greater_than() {
        let result = parse("if [[ ${COUNT} -gt 0 ]]; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::Test(test) => match test.as_ref() {
                    TestExpr::Comparison { op, .. } => assert_eq!(*op, TestCmpOp::NumGt),
                    other => panic!("expected comparison, got {:?}", other),
                },
                other => panic!("expected test expr, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_comparison_less_equal() {
        let result = parse("if [[ ${X} -le 100 ]]; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::Test(test) => match test.as_ref() {
                    TestExpr::Comparison { op, .. } => assert_eq!(*op, TestCmpOp::NumLtEq),
                    other => panic!("expected comparison, got {:?}", other),
                },
                other => panic!("expected test expr, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_comparison_greater_equal() {
        let result = parse("if [[ ${X} -ge 1 ]]; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::Test(test) => match test.as_ref() {
                    TestExpr::Comparison { op, .. } => assert_eq!(*op, TestCmpOp::NumGtEq),
                    other => panic!("expected comparison, got {:?}", other),
                },
                other => panic!("expected test expr, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_regex_match() {
        let result = parse(r#"if [[ ${NAME} =~ "^test" ]]; then echo; fi"#).unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::Test(test) => match test.as_ref() {
                    TestExpr::Comparison { op, .. } => assert_eq!(*op, TestCmpOp::Match),
                    other => panic!("expected comparison, got {:?}", other),
                },
                other => panic!("expected test expr, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_regex_not_match() {
        let result = parse(r#"if [[ ${NAME} !~ "^test" ]]; then echo; fi"#).unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::Test(test) => match test.as_ref() {
                    TestExpr::Comparison { op, .. } => assert_eq!(*op, TestCmpOp::NotMatch),
                    other => panic!("expected comparison, got {:?}", other),
                },
                other => panic!("expected test expr, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_string_interpolation() {
        let result = parse(r#"echo "Hello ${NAME}!""#).unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => match &cmd.args[0] {
                Arg::Positional(Expr::Interpolated(parts)) => {
                    assert_eq!(parts.len(), 3);
                    match &parts[0] {
                        StringPart::Literal(s) => assert_eq!(s, "Hello "),
                        other => panic!("expected literal, got {:?}", other),
                    }
                    match &parts[1] {
                        StringPart::Var(path) => {
                            assert_eq!(path.segments.len(), 1);
                            let VarSegment::Field(name) = &path.segments[0] else {
                                panic!("expected root field, got {:?}", path.segments[0]);
                            };
                            assert_eq!(name, "NAME");
                        }
                        other => panic!("expected var, got {:?}", other),
                    }
                    match &parts[2] {
                        StringPart::Literal(s) => assert_eq!(s, "!"),
                        other => panic!("expected literal, got {:?}", other),
                    }
                }
                other => panic!("expected interpolated, got {:?}", other),
            },
            other => panic!("expected command, got {:?}", other),
        }
    }

    #[test]
    fn parse_string_interpolation_multiple_vars() {
        let result = parse(r#"echo "${FIRST} and ${SECOND}""#).unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => match &cmd.args[0] {
                Arg::Positional(Expr::Interpolated(parts)) => {
                    // ${FIRST} + " and " + ${SECOND} = 3 parts
                    assert_eq!(parts.len(), 3);
                    assert!(matches!(&parts[0], StringPart::Var(_)));
                    assert!(matches!(&parts[1], StringPart::Literal(_)));
                    assert!(matches!(&parts[2], StringPart::Var(_)));
                }
                other => panic!("expected interpolated, got {:?}", other),
            },
            other => panic!("expected command, got {:?}", other),
        }
    }

    #[test]
    fn parse_empty_function_body() {
        let result = parse("empty() { }").unwrap();
        match &result.statements[0] {
            Stmt::ToolDef(t) => {
                assert_eq!(t.name, "empty");
                assert!(t.params.is_empty());
                assert!(t.body.is_empty());
            }
            other => panic!("expected function def, got {:?}", other),
        }
    }

    #[test]
    fn parse_bash_style_function() {
        let result = parse("function greet { echo hello }").unwrap();
        match &result.statements[0] {
            Stmt::ToolDef(t) => {
                assert_eq!(t.name, "greet");
                assert!(t.params.is_empty());
                assert_eq!(t.body.len(), 1);
            }
            other => panic!("expected function def, got {:?}", other),
        }
    }

    #[test]
    fn parse_comparison_string_values() {
        let result = parse(r#"if [[ ${STATUS} == "ok" ]]; then echo; fi"#).unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::Test(test) => match test.as_ref() {
                    TestExpr::Comparison { left, op, right } => {
                        assert!(matches!(left.as_ref(), Expr::VarRef(_)));
                        assert_eq!(*op, TestCmpOp::Eq);
                        match right.as_ref() {
                            Expr::Literal(Value::String(s)) => assert_eq!(s, "ok"),
                            other => panic!("expected string, got {:?}", other),
                        }
                    }
                    other => panic!("expected comparison, got {:?}", other),
                },
                other => panic!("expected test expr, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Command Substitution Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[test]
    fn parse_cmd_subst_simple() {
        let result = parse("X=$(echo)").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => {
                assert_eq!(a.name(), "X");
                assert_eq!(subst_cmd(&a.value).name, "echo");
            }
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn parse_cmd_subst_with_args() {
        let result = parse(r#"X=$(fetch url="http://example.com")"#).unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => {
                let cmd = subst_cmd(&a.value);
                assert_eq!(cmd.name, "fetch");
                assert_eq!(cmd.args.len(), 1);
                match &cmd.args[0] {
                    Arg::WordAssign { key, .. } => assert_eq!(key, "url"),
                    other => panic!("expected WordAssign arg, got {:?}", other),
                }
            }
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn parse_cmd_subst_pipeline() {
        let result = parse("X=$(cat file | grep pattern)").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => {
                let pipeline = subst_pipeline(&a.value);
                assert_eq!(pipeline.commands.len(), 2);
                assert_eq!(pipeline.commands[0].name, "cat");
                assert_eq!(pipeline.commands[1].name, "grep");
            }
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn parse_cmd_subst_with_redirect() {
        // Regression: `cmd_subst_parser` used to hardcode `redirects: vec![]`,
        // so a redirect inside `$()` was a parse error. A command carrying a
        // redirect stays a `Stmt::Pipeline` (`pipeline_into_stmt` only unwraps
        // redirect-free commands), so read it back through `subst_pipeline`.
        let result = parse("X=$(echo hi > out.txt)").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => {
                let pipeline = subst_pipeline(&a.value);
                assert_eq!(pipeline.commands.len(), 1);
                let cmd = &pipeline.commands[0];
                assert_eq!(cmd.name, "echo");
                assert_eq!(cmd.redirects.len(), 1);
                assert!(matches!(
                    cmd.redirects[0].kind,
                    RedirectKind::StdoutOverwrite
                ));
            }
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn parse_cmd_subst_redirect_target_with_nested_subst() {
        // The cycle-break's sharpest case: a `$(...)` in the redirect *target*,
        // inside a `$(...)`. This exercises cmd_subst → redirect → (recursive
        // expr) → cmd_subst, the path that used to recurse unboundedly during
        // parser construction (stack overflow). It must parse; the target is a
        // nested `CommandSubst`.
        let result = parse("X=$(echo hi > $(echo f))").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => {
                let pipeline = subst_pipeline(&a.value);
                assert_eq!(pipeline.commands.len(), 1);
                let cmd = &pipeline.commands[0];
                assert_eq!(cmd.name, "echo");
                assert_eq!(cmd.redirects.len(), 1);
                assert!(
                    matches!(cmd.redirects[0].target, Expr::CommandSubst(_)),
                    "redirect target should be a nested command substitution, got {:?}",
                    cmd.redirects[0].target
                );
            }
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn parse_cmd_subst_chain_with_redirect() {
        // A redirect in a chained `$()` body binds to its own command, not to
        // the chain: `$(a && b > f)` → AndChain{ left: a, right: (b > f) }, with
        // the redirect on `b` only.
        let result = parse("X=$(echo a && echo b > out.txt)").unwrap();
        let stmts = match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::CommandSubst(s) => s,
                other => panic!("expected command subst, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        };
        match stmts.as_slice() {
            [Stmt::AndChain { left, right }] => {
                // `echo a` is redirect-free → unwrapped to Stmt::Command.
                assert!(
                    matches!(**left, Stmt::Command(_)),
                    "left of && should be a bare command, got {:?}",
                    left
                );
                // `echo b > out.txt` carries a redirect → stays Stmt::Pipeline.
                match &**right {
                    Stmt::Pipeline(p) => {
                        assert_eq!(p.commands.len(), 1);
                        assert_eq!(p.commands[0].name, "echo");
                        assert_eq!(p.commands[0].redirects.len(), 1);
                    }
                    other => panic!("right should be a redirect-bearing pipeline, got {:?}", other),
                }
            }
            other => panic!("expected a single AndChain, got {:?}", other),
        }
    }

    #[test]
    fn parse_cmd_subst_in_condition() {
        // Shell-compatible: conditions are commands, not command substitutions
        let result = parse("if kaish-validate; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::Command(cmd) => {
                    assert_eq!(cmd.name, "kaish-validate");
                }
                other => panic!("expected command, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Inline env-prefix (`NAME=value command`) Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[test]
    fn parse_env_prefix_single() {
        let result = parse("FOO=bar echo hi").unwrap();
        match &result.statements[0] {
            Stmt::EnvScoped { assignments, body } => {
                assert_eq!(assignments.len(), 1);
                assert_eq!(assignments[0].name(), "FOO");
                assert!(!assignments[0].local);
                match body.as_ref() {
                    Stmt::Command(cmd) => assert_eq!(cmd.name, "echo"),
                    other => panic!("expected command body, got {other:?}"),
                }
            }
            other => panic!("expected env-scoped, got {other:?}"),
        }
    }

    #[test]
    fn parse_env_prefix_multiple() {
        let result = parse("A=1 B=2 run").unwrap();
        match &result.statements[0] {
            Stmt::EnvScoped { assignments, body } => {
                assert_eq!(assignments.len(), 2);
                assert_eq!(assignments[0].name(), "A");
                assert_eq!(assignments[1].name(), "B");
                assert!(matches!(body.as_ref(), Stmt::Command(c) if c.name == "run"));
            }
            other => panic!("expected env-scoped, got {other:?}"),
        }
    }

    #[test]
    fn parse_bare_assignment_is_not_env_scoped() {
        // No command follows — stays a plain (persistent) assignment.
        let result = parse("FOO=bar").unwrap();
        assert!(
            matches!(&result.statements[0], Stmt::Assignment(a) if a.name() == "FOO"),
            "got {:?}",
            result.statements[0]
        );
    }

    #[test]
    fn parse_assignment_then_and_chain_does_not_over_capture() {
        // `FOO=bar && echo` is a (persistent) assignment chained with `&&`, NOT
        // an env-prefixed command — the `&&` is not a command for the prefix.
        let result = parse("FOO=bar && echo hi").unwrap();
        match &result.statements[0] {
            Stmt::AndChain { left, right } => {
                assert!(matches!(left.as_ref(), Stmt::Assignment(a) if a.name() == "FOO"));
                assert!(matches!(right.as_ref(), Stmt::Command(c) if c.name == "echo"));
            }
            other => panic!("expected and-chain, got {other:?}"),
        }
    }

    #[test]
    fn parse_env_prefix_pipeline_body() {
        let result = parse("FOO=bar cat | grep x").unwrap();
        match &result.statements[0] {
            Stmt::EnvScoped { assignments, body } => {
                assert_eq!(assignments[0].name(), "FOO");
                match body.as_ref() {
                    Stmt::Pipeline(p) => assert_eq!(p.commands.len(), 2),
                    other => panic!("expected pipeline body, got {other:?}"),
                }
            }
            other => panic!("expected env-scoped, got {other:?}"),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Argv-splat rejection (adjacent unquoted words — docs/issues.md #2)
    // ═══════════════════════════════════════════════════════════════════════════

    fn parse_err_message(source: &str) -> String {
        parse(source)
            .expect_err("expected a parse error")
            .iter()
            .map(|e| e.message.clone())
            .collect::<Vec<_>>()
            .join(" ")
    }

    #[test]
    fn argv_splat_cmdsubst_glued_to_path_is_rejected() {
        // `/tmp/$(echo x).txt` lexes as 3 adjacent tokens; unquoted it would
        // silently splat into 3 args. Reject with a quote-it hint.
        let msg = parse_err_message("echo /tmp/$(echo x).txt");
        assert!(msg.contains("quote"), "expected quote hint, got: {msg}");
    }

    #[test]
    fn argv_splat_var_glued_to_path_is_rejected() {
        assert!(parse("echo $dir/out.txt").is_err());
    }

    #[test]
    fn argv_splat_three_way_glue_is_rejected() {
        assert!(parse("echo foo$(echo bar)baz").is_err());
    }

    #[test]
    fn argv_splat_quoted_word_is_accepted() {
        // The supported idiom: quote the whole interpolated word.
        assert!(parse(r#"echo "/tmp/$(echo x).txt""#).is_ok());
        assert!(parse(r#"echo "$dir/out.txt""#).is_ok());
    }

    #[test]
    fn argv_single_token_words_are_not_splat() {
        // These lex as a single token each — no adjacency, must still parse.
        assert!(parse("echo file.txt").is_ok(), "file.txt");
        assert!(parse("echo a.b.c").is_ok(), "a.b.c");
        assert!(parse("echo v1.2.3").is_ok(), "v1.2.3");
    }

    #[test]
    fn argv_spaced_words_are_not_splat() {
        assert!(parse("echo a b c").is_ok());
        assert!(parse("echo /tmp/x $(echo y)").is_ok());
    }

    #[test]
    fn parse_cmd_subst_in_command_arg() {
        let result = parse("echo $(whoami)").unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, "echo");
                match &cmd.args[0] {
                    Arg::Positional(expr) => {
                        assert_eq!(subst_cmd(expr).name, "whoami");
                    }
                    other => panic!("expected command subst, got {:?}", other),
                }
            }
            other => panic!("expected command, got {:?}", other),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Logical Operator Tests (&&, ||)
    // ═══════════════════════════════════════════════════════════════════════════

    #[test]
    fn parse_condition_and() {
        // Shell-compatible: commands chained with &&
        let result = parse("if check-a && check-b; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::BinaryOp { left, op, right } => {
                    assert_eq!(*op, BinaryOp::And);
                    assert!(matches!(left.as_ref(), Expr::Command(_)));
                    assert!(matches!(right.as_ref(), Expr::Command(_)));
                }
                other => panic!("expected binary op, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_condition_or() {
        let result = parse("if try-a || try-b; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::BinaryOp { left, op, right } => {
                    assert_eq!(*op, BinaryOp::Or);
                    assert!(matches!(left.as_ref(), Expr::Command(_)));
                    assert!(matches!(right.as_ref(), Expr::Command(_)));
                }
                other => panic!("expected binary op, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_condition_and_or_precedence() {
        // a && b || c should parse as (a && b) || c
        let result = parse("if cmd-a && cmd-b || cmd-c; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::BinaryOp { left, op, right } => {
                    // Top level should be ||
                    assert_eq!(*op, BinaryOp::Or);
                    // Left side should be && expression
                    match left.as_ref() {
                        Expr::BinaryOp { op: inner_op, .. } => {
                            assert_eq!(*inner_op, BinaryOp::And);
                        }
                        other => panic!("expected binary op (&&), got {:?}", other),
                    }
                    // Right side should be command
                    assert!(matches!(right.as_ref(), Expr::Command(_)));
                }
                other => panic!("expected binary op, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_condition_multiple_and() {
        let result = parse("if cmd-a && cmd-b && cmd-c; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::BinaryOp { left, op, .. } => {
                    assert_eq!(*op, BinaryOp::And);
                    // Left side should also be &&
                    match left.as_ref() {
                        Expr::BinaryOp { op: inner_op, .. } => {
                            assert_eq!(*inner_op, BinaryOp::And);
                        }
                        other => panic!("expected binary op, got {:?}", other),
                    }
                }
                other => panic!("expected binary op, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_condition_mixed_comparison_and_logical() {
        // Shell-compatible: use [[ ]] for comparisons, && to chain them
        let result = parse("if [[ ${X} == 5 ]] && [[ ${Y} -gt 0 ]]; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::BinaryOp { left, op, right } => {
                    assert_eq!(*op, BinaryOp::And);
                    // Left: [[ ${X} == 5 ]]
                    match left.as_ref() {
                        Expr::Test(test) => match test.as_ref() {
                            TestExpr::Comparison { op: left_op, .. } => {
                                assert_eq!(*left_op, TestCmpOp::Eq);
                            }
                            other => panic!("expected comparison, got {:?}", other),
                        },
                        other => panic!("expected test, got {:?}", other),
                    }
                    // Right: [[ ${Y} -gt 0 ]]
                    match right.as_ref() {
                        Expr::Test(test) => match test.as_ref() {
                            TestExpr::Comparison { op: right_op, .. } => {
                                assert_eq!(*right_op, TestCmpOp::NumGt);
                            }
                            other => panic!("expected comparison, got {:?}", other),
                        },
                        other => panic!("expected test, got {:?}", other),
                    }
                }
                other => panic!("expected binary op, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Integration Tests - Complete Scripts
    // ═══════════════════════════════════════════════════════════════════════════

    /// Level 1: Linear script using core features
    #[test]
    fn script_level1_linear() {
        let script = r#"
NAME="kaish"
VERSION=1
TIMEOUT=30
ITEMS="alpha beta gamma"

echo "Starting ${NAME} v${VERSION}"
cat "README.md" | grep pattern="install" | head count=5
fetch url="https://api.example.com/status" timeout=${TIMEOUT} > "/tmp/status.json"
echo "Items: ${ITEMS}"
"#;
        let result = parse(script).unwrap();
        let stmts: Vec<_> = result.statements.iter()
            .filter(|s| !matches!(s, Stmt::Empty))
            .collect();

        assert_eq!(stmts.len(), 8);
        assert!(matches!(stmts[0], Stmt::Assignment(_)));  // set NAME
        assert!(matches!(stmts[1], Stmt::Assignment(_)));  // set VERSION
        assert!(matches!(stmts[2], Stmt::Assignment(_)));  // set TIMEOUT
        assert!(matches!(stmts[3], Stmt::Assignment(_)));  // set ITEMS
        assert!(matches!(stmts[4], Stmt::Command(_)));     // echo "Starting..."
        assert!(matches!(stmts[5], Stmt::Pipeline(_)));    // cat | grep | head
        assert!(matches!(stmts[6], Stmt::Pipeline(_)));    // fetch (with redirect - Pipeline since it has redirects)
        assert!(matches!(stmts[7], Stmt::Command(_)));     // echo "Items: ${ITEMS}"
    }

    /// Level 2: Script with conditionals (shell-compatible syntax)
    #[test]
    fn script_level2_branching() {
        let script = r#"
RESULT=$(kaish-validate "input.json")

if [[ ${RESULT.ok} == true ]]; then
    echo "Validation passed"
    process "input.json" > "output.json"
else
    echo "Validation failed: ${RESULT.err}"
fi

if [[ ${COUNT} -gt 0 ]] && [[ ${COUNT} -le 100 ]]; then
    echo "Count in valid range"
fi

if check-network || check-cache; then
    fetch url=${URL}
fi
"#;
        let result = parse(script).unwrap();
        let stmts: Vec<_> = result.statements.iter()
            .filter(|s| !matches!(s, Stmt::Empty))
            .collect();

        assert_eq!(stmts.len(), 4);

        // First: assignment with command substitution
        match stmts[0] {
            Stmt::Assignment(a) => {
                assert_eq!(a.name(), "RESULT");
                assert!(matches!(&a.value, Expr::CommandSubst(_)));
            }
            other => panic!("expected assignment, got {:?}", other),
        }

        // Second: if/else
        match stmts[1] {
            Stmt::If(if_stmt) => {
                assert_eq!(if_stmt.then_branch.len(), 2);
                assert!(if_stmt.else_branch.is_some());
                assert_eq!(if_stmt.else_branch.as_ref().unwrap().len(), 1);
            }
            other => panic!("expected if, got {:?}", other),
        }

        // Third: if with && condition
        match stmts[2] {
            Stmt::If(if_stmt) => {
                match if_stmt.condition.as_ref() {
                    Expr::BinaryOp { op, .. } => assert_eq!(*op, BinaryOp::And),
                    other => panic!("expected && condition, got {:?}", other),
                }
            }
            other => panic!("expected if, got {:?}", other),
        }

        // Fourth: if with || of commands
        match stmts[3] {
            Stmt::If(if_stmt) => {
                match if_stmt.condition.as_ref() {
                    Expr::BinaryOp { op, left, right } => {
                        assert_eq!(*op, BinaryOp::Or);
                        assert!(matches!(left.as_ref(), Expr::Command(_)));
                        assert!(matches!(right.as_ref(), Expr::Command(_)));
                    }
                    other => panic!("expected || condition, got {:?}", other),
                }
            }
            other => panic!("expected if, got {:?}", other),
        }
    }

    /// Level 3: Script with loops and function definitions
    #[test]
    fn script_level3_loops_and_functions() {
        let script = r#"
greet() {
    echo "Hello, $1!"
}

fetch_all() {
    for URL in $@; do
        fetch url=${URL}
    done
}

USERS="alice bob charlie"

for USER in ${USERS}; do
    greet ${USER}
    if [[ ${USER} == "bob" ]]; then
        echo "Found Bob!"
    fi
done

long-running-task &
"#;
        let result = parse(script).unwrap();
        let stmts: Vec<_> = result.statements.iter()
            .filter(|s| !matches!(s, Stmt::Empty))
            .collect();

        assert_eq!(stmts.len(), 5);

        // First function def
        match stmts[0] {
            Stmt::ToolDef(t) => {
                assert_eq!(t.name, "greet");
                assert!(t.params.is_empty());
            }
            other => panic!("expected function def, got {:?}", other),
        }

        // Second function def with nested for loop
        match stmts[1] {
            Stmt::ToolDef(t) => {
                assert_eq!(t.name, "fetch_all");
                assert_eq!(t.body.len(), 1);
                assert!(matches!(&t.body[0], Stmt::For(_)));
            }
            other => panic!("expected function def, got {:?}", other),
        }

        // Assignment
        assert!(matches!(stmts[2], Stmt::Assignment(_)));

        // For loop with nested if
        match stmts[3] {
            Stmt::For(f) => {
                assert_eq!(f.variable, "USER");
                assert_eq!(f.body.len(), 2);
                assert!(matches!(&f.body[0], Stmt::Command(_)));
                assert!(matches!(&f.body[1], Stmt::If(_)));
            }
            other => panic!("expected for loop, got {:?}", other),
        }

        // Background job
        match stmts[4] {
            Stmt::Pipeline(p) => {
                assert!(p.background);
                assert_eq!(p.commands[0].name, "long-running-task");
            }
            other => panic!("expected pipeline (background), got {:?}", other),
        }
    }

    /// Level 4: Complex nested control flow (shell-compatible syntax)
    #[test]
    fn script_level4_complex_nesting() {
        let script = r#"
RESULT=$(cat "config.json" | jq query=".servers" | kaish-validate schema="server-schema.json")

if ping host=${HOST} && [[ ${RESULT} == true ]]; then
    for SERVER in "prod-1 prod-2"; do
        deploy target=${SERVER} port=8080
        if [[ $? -ne 0 ]]; then
            notify channel="ops" message="Deploy failed"
        fi
    done
fi
"#;
        let result = parse(script).unwrap();
        let stmts: Vec<_> = result.statements.iter()
            .filter(|s| !matches!(s, Stmt::Empty))
            .collect();

        assert_eq!(stmts.len(), 2);

        // Command substitution with pipeline
        match stmts[0] {
            Stmt::Assignment(a) => {
                assert_eq!(a.name(), "RESULT");
                assert_eq!(subst_pipeline(&a.value).commands.len(), 3);
            }
            other => panic!("expected assignment, got {:?}", other),
        }

        // If with && condition, containing for loop with nested if
        match stmts[1] {
            Stmt::If(if_stmt) => {
                match if_stmt.condition.as_ref() {
                    Expr::BinaryOp { op, .. } => assert_eq!(*op, BinaryOp::And),
                    other => panic!("expected && condition, got {:?}", other),
                }
                assert_eq!(if_stmt.then_branch.len(), 1);
                match &if_stmt.then_branch[0] {
                    Stmt::For(f) => {
                        assert_eq!(f.body.len(), 2);
                        assert!(matches!(&f.body[1], Stmt::If(_)));
                    }
                    other => panic!("expected for in if body, got {:?}", other),
                }
            }
            other => panic!("expected if, got {:?}", other),
        }
    }

    /// Level 5: Edge cases and parser stress test
    #[test]
    fn script_level5_edge_cases() {
        let script = r#"
echo ""
echo "quotes: \"nested\" here"
echo "escapes: \n\t\r\\"
echo "unicode: \u2764"

X=-99999
Y=3.14159265358979
Z=-0.001

cmd a=1 b="two" c=true d=false e=null

if true; then
    if false; then
        echo "inner"
    else
        echo "else"
    fi
fi

for I in "a b c"; do
    echo ${I}
done

no_params() {
    echo "no params"
}

function all_args {
    echo "args: $@"
}

a | b | c | d | e &
cmd 2> "errors.log"
cmd &> "all.log"
cmd >> "append.log"
cmd < "input.txt"
"#;
        let result = parse(script).unwrap();
        let stmts: Vec<_> = result.statements.iter()
            .filter(|s| !matches!(s, Stmt::Empty))
            .collect();

        // Verify it parses without error
        assert!(stmts.len() >= 10, "expected many statements, got {}", stmts.len());

        // Background pipeline
        let bg_stmt = stmts.iter().find(|s| matches!(s, Stmt::Pipeline(p) if p.background));
        assert!(bg_stmt.is_some(), "expected background pipeline");

        match bg_stmt.unwrap() {
            Stmt::Pipeline(p) => {
                assert_eq!(p.commands.len(), 5);
                assert!(p.background);
            }
            _ => unreachable!(),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Edge Case Tests: Ambiguity Resolution
    // ═══════════════════════════════════════════════════════════════════════════

    #[test]
    fn parse_keyword_as_variable_rejected() {
        // Keywords CANNOT be used as variable names - this is intentional
        // to avoid ambiguity. Use different names instead.
        let result = parse(r#"if="value""#);
        assert!(result.is_err(), "if= should fail - 'if' is a keyword");

        let result = parse("while=true");
        assert!(result.is_err(), "while= should fail - 'while' is a keyword");

        let result = parse(r#"then="next""#);
        assert!(result.is_err(), "then= should fail - 'then' is a keyword");
    }

    #[test]
    fn parse_set_command_with_flag() {
        let result = parse("set -e");
        assert!(result.is_ok(), "failed to parse set -e: {:?}", result);
        let program = result.unwrap();
        match &program.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, "set");
                assert_eq!(cmd.args.len(), 1);
                match &cmd.args[0] {
                    Arg::ShortFlag(f) => assert_eq!(f, "e"),
                    other => panic!("expected ShortFlag, got {:?}", other),
                }
            }
            other => panic!("expected Command, got {:?}", other),
        }
    }

    #[test]
    fn parse_set_command_no_args() {
        let result = parse("set");
        assert!(result.is_ok(), "failed to parse set: {:?}", result);
        let program = result.unwrap();
        match &program.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, "set");
                assert_eq!(cmd.args.len(), 0);
            }
            other => panic!("expected Command, got {:?}", other),
        }
    }

    #[test]
    fn parse_set_assignment_vs_command() {
        // X=5 should be assignment
        let result = parse("X=5");
        assert!(result.is_ok());
        let program = result.unwrap();
        assert!(matches!(&program.statements[0], Stmt::Assignment(_)));

        // set -e should be command
        let result = parse("set -e");
        assert!(result.is_ok());
        let program = result.unwrap();
        assert!(matches!(&program.statements[0], Stmt::Command(_)));
    }

    #[test]
    fn parse_true_as_command() {
        let result = parse("true");
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.statements[0] {
            Stmt::Command(cmd) => assert_eq!(cmd.name, "true"),
            other => panic!("expected Command(true), got {:?}", other),
        }
    }

    #[test]
    fn parse_false_as_command() {
        let result = parse("false");
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.statements[0] {
            Stmt::Command(cmd) => assert_eq!(cmd.name, "false"),
            other => panic!("expected Command(false), got {:?}", other),
        }
    }

    #[test]
    fn parse_dot_as_source_alias() {
        let result = parse(". script.kai");
        assert!(result.is_ok(), "failed to parse . script.kai: {:?}", result);
        let program = result.unwrap();
        match &program.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, ".");
                assert_eq!(cmd.args.len(), 1);
            }
            other => panic!("expected Command(.), got {:?}", other),
        }
    }

    #[test]
    fn parse_source_command() {
        let result = parse("source utils.kai");
        assert!(result.is_ok(), "failed to parse source: {:?}", result);
        let program = result.unwrap();
        match &program.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, "source");
                assert_eq!(cmd.args.len(), 1);
            }
            other => panic!("expected Command(source), got {:?}", other),
        }
    }

    #[test]
    fn parse_test_expr_file_test() {
        // Paths must be quoted strings in test expressions
        let result = parse(r#"[[ -f "/path/file" ]]"#);
        assert!(result.is_ok(), "failed to parse file test: {:?}", result);
    }

    #[test]
    fn parse_test_expr_comparison() {
        let result = parse(r#"[[ $X == "value" ]]"#);
        assert!(result.is_ok(), "failed to parse comparison test: {:?}", result);
    }

    #[test]
    fn parse_test_expr_single_eq() {
        // = and == are equivalent inside [[ ]] (matching bash behavior)
        let result = parse(r#"[[ $X = "value" ]]"#);
        assert!(result.is_ok(), "failed to parse single-= comparison: {:?}", result);
        let program = result.unwrap();
        match &program.statements[0] {
            Stmt::Test(TestExpr::Comparison { op, .. }) => {
                assert_eq!(op, &TestCmpOp::Eq);
            }
            other => panic!("expected Test(Comparison), got {:?}", other),
        }
    }

    #[test]
    fn parse_while_loop() {
        let result = parse("while true; do echo; done");
        assert!(result.is_ok(), "failed to parse while loop: {:?}", result);
        let program = result.unwrap();
        assert!(matches!(&program.statements[0], Stmt::While(_)));
    }

    #[test]
    fn parse_break_with_level() {
        let result = parse("break 2");
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.statements[0] {
            Stmt::Break(Some(n)) => assert_eq!(*n, 2),
            other => panic!("expected Break(2), got {:?}", other),
        }
    }

    #[test]
    fn parse_continue_with_level() {
        let result = parse("continue 3");
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.statements[0] {
            Stmt::Continue(Some(n)) => assert_eq!(*n, 3),
            other => panic!("expected Continue(3), got {:?}", other),
        }
    }

    #[test]
    fn parse_exit_with_code() {
        let result = parse("exit 1");
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.statements[0] {
            Stmt::Exit(Some(expr)) => {
                match expr.as_ref() {
                    Expr::Literal(Value::Int(n)) => assert_eq!(*n, 1),
                    other => panic!("expected Int(1), got {:?}", other),
                }
            }
            other => panic!("expected Exit(1), got {:?}", other),
        }
    }

    // ========================================================================
    // parse_interpolated_string_spanned — body-internal span tracking for
    // heredoc bodies. The byte offsets these tests pin become validator
    // issue spans via the HereDocBody → SpannedPart flow.
    // ========================================================================

    #[test]
    fn spanned_literal_only_records_byte_range() {
        let parts = parse_interpolated_string_spanned("hello world", 100);
        assert_eq!(parts.len(), 1);
        assert!(matches!(&parts[0].part, StringPart::Literal(s) if s == "hello world"));
        assert_eq!(parts[0].offset, 100, "base_offset must propagate to literals");
        assert_eq!(parts[0].len, 11);
    }

    #[test]
    fn spanned_braced_var_at_zero() {
        let parts = parse_interpolated_string_spanned("${X}", 50);
        assert_eq!(parts.len(), 1);
        assert!(matches!(&parts[0].part, StringPart::Var(_)));
        assert_eq!(parts[0].offset, 50);
        assert_eq!(parts[0].len, 4); // "${X}"
    }

    #[test]
    fn spanned_simple_var_then_literal() {
        let parts = parse_interpolated_string_spanned("$X end", 10);
        assert_eq!(parts.len(), 2);
        assert!(matches!(&parts[0].part, StringPart::Var(_)));
        assert_eq!(parts[0].offset, 10);
        assert_eq!(parts[0].len, 2); // "$X"
        assert!(matches!(&parts[1].part, StringPart::Literal(s) if s == " end"));
        assert_eq!(parts[1].offset, 12);
        assert_eq!(parts[1].len, 4);
    }

    #[test]
    fn spanned_mixed_literal_var_literal() {
        let parts = parse_interpolated_string_spanned("hi ${X} bye", 0);
        assert_eq!(parts.len(), 3);
        // "hi "
        assert!(matches!(&parts[0].part, StringPart::Literal(s) if s == "hi "));
        assert_eq!(parts[0].offset, 0);
        assert_eq!(parts[0].len, 3);
        // ${X}
        assert!(matches!(&parts[1].part, StringPart::Var(_)));
        assert_eq!(parts[1].offset, 3);
        assert_eq!(parts[1].len, 4);
        // " bye"
        assert!(matches!(&parts[2].part, StringPart::Literal(s) if s == " bye"));
        assert_eq!(parts[2].offset, 7);
        assert_eq!(parts[2].len, 4);
    }

    #[test]
    fn spanned_positional_param() {
        let parts = parse_interpolated_string_spanned("$1 done", 0);
        assert_eq!(parts.len(), 2);
        assert!(matches!(&parts[0].part, StringPart::Positional(1)));
        assert_eq!(parts[0].offset, 0);
        assert_eq!(parts[0].len, 2); // "$1"
    }

    #[test]
    fn spanned_special_dollar_dollar() {
        let parts = parse_interpolated_string_spanned("$$", 5);
        assert_eq!(parts.len(), 1);
        assert!(matches!(&parts[0].part, StringPart::CurrentPid));
        assert_eq!(parts[0].offset, 5);
        assert_eq!(parts[0].len, 2);
    }

    #[test]
    fn spanned_arithmetic_marker_recognised() {
        // The lexer wraps arithmetic markers as ${__ARITH:expr__} for
        // interpolated heredocs; the spanned parser must produce
        // StringPart::Arithmetic for that shape.
        let parts = parse_interpolated_string_spanned("${__ARITH:1+2__}", 0);
        assert_eq!(parts.len(), 1);
        assert!(matches!(&parts[0].part, StringPart::Arithmetic(e) if e == "1+2"));
    }

    #[test]
    fn spanned_default_separator_yields_var_with_default() {
        let parts = parse_interpolated_string_spanned("${X:-fallback}", 0);
        assert_eq!(parts.len(), 1);
        assert!(matches!(&parts[0].part, StringPart::VarWithDefault { .. }));
        assert_eq!(parts[0].offset, 0);
        assert_eq!(parts[0].len, 14); // "${X:-fallback}"
    }

    #[test]
    fn spanned_no_dollar_runs_one_literal() {
        let parts = parse_interpolated_string_spanned("plain text only", 7);
        assert_eq!(parts.len(), 1);
        assert!(matches!(&parts[0].part, StringPart::Literal(s) if s == "plain text only"));
        assert_eq!(parts[0].offset, 7);
        assert_eq!(parts[0].len, 15);
    }

    #[test]
    fn spanned_matches_unspanned_part_count() {
        // Spanned and spanless variants must agree on the part decomposition.
        // Bug fixes in one should land in the other.
        let cases = [
            "hello",
            "$X",
            "${X}",
            "${X:-d}",
            "hi $A and $B",
            "$0 $1 $2",
            "$$ $? $#",
        ];
        for s in &cases {
            let unspanned = parse_interpolated_string(s).expect("test input parses");
            let spanned = parse_interpolated_string_spanned(s, 0);
            assert_eq!(
                unspanned.len(),
                spanned.len(),
                "part count differs for {:?}",
                s
            );
        }
    }

    #[test]
    fn spanned_multibyte_utf8_before_var_uses_byte_offsets() {
        // 🚀 is 4 bytes in UTF-8 and a space is 1 byte, so the literal
        // prefix is 5 bytes total. `${X}` then sits at byte offset 5.
        // Right-by-luck for char-vs-byte indexing is precisely what this
        // test catches: if someone swaps .len_utf8() for 1, offset becomes 2.
        let parts = parse_interpolated_string_spanned("🚀 ${X}", 0);
        assert_eq!(parts.len(), 2);

        assert!(matches!(&parts[0].part, StringPart::Literal(s) if s == "🚀 "));
        assert_eq!(parts[0].offset, 0);
        assert_eq!(parts[0].len, 5, "literal len must be bytes, not chars");

        assert!(matches!(&parts[1].part, StringPart::Var(_)));
        assert_eq!(parts[1].offset, 5, "var offset must be bytes, not chars");
        assert_eq!(parts[1].len, 4);
    }

    #[test]
    fn spanned_multibyte_utf8_pure_literal_is_byte_length() {
        // "hello 世界 world": 5 + 1 + 6 (3 per CJK char) + 1 + 5 = 18 bytes,
        // 13 chars. The `len` field must report 18, not 13.
        let parts = parse_interpolated_string_spanned("hello 世界 world", 0);
        assert_eq!(parts.len(), 1);
        assert!(matches!(&parts[0].part, StringPart::Literal(s) if s == "hello 世界 world"));
        assert_eq!(parts[0].offset, 0);
        assert_eq!(parts[0].len, 18);
    }

    #[test]
    fn spanned_escape_dollar_consumes_two_bytes_emits_one_char() {
        // `\$` is 2 source bytes and resolves to a single literal `$`.
        // The literal part's `len` should reflect the SOURCE length (2).
        let parts = parse_interpolated_string_spanned("\\$", 0);
        assert_eq!(parts.len(), 1);
        assert!(matches!(&parts[0].part, StringPart::Literal(s) if s == "$"));
        assert_eq!(parts[0].offset, 0);
        assert_eq!(parts[0].len, 2, "len is source byte length, not rendered length");
    }

    #[test]
    fn spanned_escape_backslash_collapses_pair_to_one() {
        let parts = parse_interpolated_string_spanned("\\\\", 0);
        assert_eq!(parts.len(), 1);
        assert!(matches!(&parts[0].part, StringPart::Literal(s) if s == "\\"));
        assert_eq!(parts[0].len, 2);
    }

    #[test]
    fn spanned_standalone_cr_continuation_realigns_span_start() {
        // `\` + bare `\r` (old Mac line ending, no trailing `\n`) is a line
        // continuation: 2 source bytes, consumed with no output. Pins the
        // `current_text_start` update on that branch (parser.rs's `Some('\r')`
        // arm in `parse_interpolated_string_spanned`) — if it failed to
        // advance past the consumed `\`+`\r`, the following literal run would
        // be misreported starting at byte 0 instead of byte 2, corrupting
        // every subsequent span in the string (here, the `${x}` var's offset).
        let parts = parse_interpolated_string_spanned("\\\rCD${x}", 0);
        assert_eq!(parts.len(), 2);
        assert!(matches!(&parts[0].part, StringPart::Literal(s) if s == "CD"));
        assert_eq!(parts[0].offset, 2, "literal run must start after the consumed \\+CR");
        assert_eq!(parts[0].len, 2);
        assert!(matches!(&parts[1].part, StringPart::Var(_)));
        assert_eq!(parts[1].offset, 4);
        assert_eq!(parts[1].len, 4); // "${x}"
    }

    #[test]
    fn spanned_standalone_cr_continuation_mid_run_keeps_span_start() {
        // Same continuation, but hit mid-run (current_text already holds
        // "AB") — current_text_start must stay anchored to the run's true
        // start (0), not jump to the post-continuation position, so "AB"
        // and "CD" merge into one literal spanning the whole source run.
        let parts = parse_interpolated_string_spanned("AB\\\rCD${x}", 0);
        assert_eq!(parts.len(), 2);
        assert!(matches!(&parts[0].part, StringPart::Literal(s) if s == "ABCD"));
        assert_eq!(parts[0].offset, 0);
        assert_eq!(parts[0].len, 6); // "AB" + "\" + "\r" + "CD" = 6 source bytes
        assert!(matches!(&parts[1].part, StringPart::Var(_)));
        assert_eq!(parts[1].offset, 6);
        assert_eq!(parts[1].len, 4); // "${x}"
    }

    // ── Collection literals ─────────────────────────────────────────────

    /// Extract the RHS `Expr` from a one-statement `NAME=value` assignment.
    fn assignment_value(source: &str) -> Expr {
        let program = parse(source).unwrap_or_else(|e| panic!("parse {source:?}: {e:?}"));
        match program.statements.as_slice() {
            [Stmt::Assignment(a)] => a.value.clone(),
            other => panic!("expected a single assignment, got {other:?}"),
        }
    }

    #[test]
    fn list_literal_three_elements() {
        let expr = assignment_value("xs=[a b c]");
        match expr {
            Expr::ListLiteral(elems) => {
                assert_eq!(elems.len(), 3);
                assert!(elems.iter().all(|e| matches!(e, ListElem::Item(_))));
            }
            other => panic!("expected ListLiteral, got {other:?}"),
        }
    }

    #[test]
    fn list_literal_empty() {
        let expr = assignment_value("xs=[]");
        assert!(matches!(expr, Expr::ListLiteral(elems) if elems.is_empty()));
    }

    #[test]
    fn list_literal_single_glued_dog() {
        // `[dog]` is glued (no spaces) — the value-position glob-merge
        // suppression must still hand it to the parser as a one-element list,
        // not a fused GlobWord.
        let expr = assignment_value("xs=[dog]");
        match expr {
            Expr::ListLiteral(elems) => assert_eq!(elems.len(), 1),
            other => panic!("expected ListLiteral, got {other:?}"),
        }
    }

    #[test]
    fn list_literal_single_int() {
        let expr = assignment_value("xs=[1]");
        match expr {
            Expr::ListLiteral(elems) => match elems.as_slice() {
                [ListElem::Item(Expr::Literal(Value::Int(1)))] => {}
                other => panic!("expected one Int(1) item, got {other:?}"),
            },
            other => panic!("expected ListLiteral, got {other:?}"),
        }
    }

    #[test]
    fn record_literal_unspaced_colon_equals_spaced() {
        let spaced = assignment_value("x={port: 8080}");
        let unspaced = assignment_value("x={port:8080}");
        assert_eq!(spaced, unspaced, "{{port:8080}} must parse identically to {{port: 8080}}");
        match spaced {
            Expr::RecordLiteral(entries) => match entries.as_slice() {
                [RecordEntry { key: RecordKey::Bare(k), value: Expr::Literal(Value::Int(8080)) }] => {
                    assert_eq!(k, "port");
                }
                other => panic!("expected one port:8080 entry, got {other:?}"),
            },
            other => panic!("expected RecordLiteral, got {other:?}"),
        }
    }

    #[test]
    fn record_literal_name_role() {
        let expr = assignment_value("u={name: amy, role: maintainer}");
        match expr {
            Expr::RecordLiteral(entries) => assert_eq!(entries.len(), 2),
            other => panic!("expected RecordLiteral, got {other:?}"),
        }
    }

    #[test]
    fn record_literal_multiline_trailing_comma() {
        let source = "services={\n  web:    {port: 8080, replicas: 3, healthy: true},\n  api:    {port: 9000, replicas: 2, healthy: false},\n}";
        let expr = assignment_value(source);
        match expr {
            Expr::RecordLiteral(entries) => assert_eq!(entries.len(), 2, "web + api entries"),
            other => panic!("expected RecordLiteral, got {other:?}"),
        }
    }

    #[test]
    fn record_literal_quoted_key() {
        let expr = assignment_value(r#"r={"content-type": x}"#);
        match expr {
            Expr::RecordLiteral(entries) => match entries.as_slice() {
                [RecordEntry { key: RecordKey::Quoted(k), .. }] => assert_eq!(k, "content-type"),
                other => panic!("expected one quoted-key entry, got {other:?}"),
            },
            other => panic!("expected RecordLiteral, got {other:?}"),
        }
    }

    #[test]
    fn nested_list_and_record_in_record() {
        let expr = assignment_value("x={tags: [a b], meta: {active: true}}");
        match expr {
            Expr::RecordLiteral(entries) => {
                assert_eq!(entries.len(), 2);
                assert!(matches!(entries[0].value, Expr::ListLiteral(_)));
                assert!(matches!(entries[1].value, Expr::RecordLiteral(_)));
            }
            other => panic!("expected RecordLiteral, got {other:?}"),
        }
    }

    #[test]
    fn spread_and_item_elements() {
        let expr = assignment_value("new=[...$xs date]");
        match expr {
            Expr::ListLiteral(elems) => match elems.as_slice() {
                [ListElem::Spread(Expr::VarRef(_)), ListElem::Item(Expr::Literal(Value::String(s)))] => {
                    assert_eq!(s, "date");
                }
                other => panic!("expected [Spread($xs), Item(date)], got {other:?}"),
            },
            other => panic!("expected ListLiteral, got {other:?}"),
        }
    }

    #[test]
    fn spread_of_two_variables() {
        let expr = assignment_value("c=[...$a ...$b]");
        match expr {
            Expr::ListLiteral(elems) => {
                assert_eq!(elems.len(), 2);
                assert!(elems.iter().all(|e| matches!(e, ListElem::Spread(_))));
            }
            other => panic!("expected ListLiteral, got {other:?}"),
        }
    }

    #[test]
    fn in_rhs_accepts_a_list_literal() {
        let program = parse("if [[ $a not in [dog] ]]; then echo hit; fi")
            .unwrap_or_else(|e| panic!("parse: {e:?}"));
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn multiword_bareword_record_value_is_a_parse_error() {
        // Strict quoting inside literals: a record value must be exactly one
        // word or one quoted string — never silently split or joined.
        assert!(parse("x={msg: hello world}").is_err());
    }

    // ── Invariant guards: argv/for-head globs must be unaffected ────────

    #[test]
    fn argv_bracket_glob_stays_a_glob_pattern() {
        // `ls [dog]` is argv position — the glued `[dog]` run must still fuse
        // to a GlobWord (the value-position suppression only applies right
        // after `Eq`/a genuine membership `In`, not after a command name).
        let program = parse("ls [dog]").unwrap_or_else(|e| panic!("parse: {e:?}"));
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn brace_expansion_at_argv_position_is_unaffected() {
        // `*.{rs,go}` is glob/brace-expansion argv syntax (the glob-merge run
        // needs a wildcard char present to fuse at all — a bare `{a,b}` with
        // no `*`/`?`/`[...]` never fuses into a GlobWord, independent of this
        // PR). Value-position literal parsing must not leak into argv.
        let program = parse("cmd *.{rs,go}").unwrap_or_else(|e| panic!("parse: {e:?}"));
        assert_eq!(program.statements.len(), 1);
    }

    #[test]
    fn for_head_item_is_not_a_literal() {
        // `for x in [a]` stays argv (a GlobPattern word list), never a
        // ListLiteral — collection literals are value-position only.
        let program = parse("for x in [a]; do echo $x; done")
            .unwrap_or_else(|e| panic!("parse: {e:?}"));
        match program.statements.as_slice() {
            [Stmt::For(for_loop)] => {
                assert_eq!(for_loop.items.len(), 1);
                assert!(
                    !matches!(for_loop.items[0], Expr::ListLiteral(_)),
                    "for-head item must not be a ListLiteral: {:?}",
                    for_loop.items[0]
                );
            }
            other => panic!("expected a single For statement, got {other:?}"),
        }
    }
}
