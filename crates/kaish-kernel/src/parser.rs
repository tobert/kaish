//! Parser for kaish source code.
//!
//! Transforms a token stream from the lexer into an Abstract Syntax Tree.
//! Uses chumsky for parser combinators with good error recovery.

use crate::ast::{
    Arg, Assignment, BinaryOp, CaseBranch, CaseStmt, Command, Expr, FileTestOp, ForLoop,
    HereDocMeta, IfStmt, ListElem, Pipeline, PipelineStage, Program, RecordEntry, RecordKey,
    Redirect, RedirectKind, SpannedPart, Stmt, StringPart, StringTestOp, TestCmpOp, TestExpr,
    ToolDef, Value,
    VarPath, VarSegment, WhileLoop,
};
use crate::lexer::{self, HereDocData, Token};
use chumsky::error::RichReason;
use chumsky::input::{MappedInput, Stream, ValueInput};
use chumsky::prelude::*;

/// Span type used throughout the parser.
pub type Span = SimpleSpan;

/// The token stream a cached parser reads.
///
/// `Stream` **owns** its tokens, so this type borrows nothing and its input
/// lifetime is `'static` — which is the whole reason the grammar below can be
/// built once instead of per call. A slice input borrows, so a parser over one
/// carries the slice's lifetime and cannot outlive a single `parse`.
///
/// The `.map` is not decoration either. `Stream`'s own spans come from cursor
/// positions, so a bare `Stream` would report *token indices* where the rest of
/// kaish reports **byte offsets** — every diagnostic position and every
/// `PlannedHeredoc::body_offset` would silently change meaning. Mapping each
/// pair through keeps the lexer's byte spans.
type ParserInput = MappedInput<'static, Token, Span, Stream<std::vec::IntoIter<(Token, Span)>>, PairFn>;

/// The mapping above, as a function pointer rather than a closure: a closure's
/// type cannot be named, and [`ParserInput`] has to be nameable to appear in
/// the cached parser's type.
type PairFn = fn((Token, Span)) -> (Token, Span);

fn keep_pair(pair: (Token, Span)) -> (Token, Span) {
    pair
}

thread_local! {
    /// The whole combinator graph, built once per thread.
    ///
    /// `program_parser()` allocated ~840 times and ~163 KB **before reading a
    /// single token**, on every `parse()` — 62% of the allocations in an
    /// embedder's `execute()` round trip (GH #255). None of it depended on the
    /// input, so all of it was rebuilt to be thrown away.
    ///
    /// Per-thread rather than one shared static: chumsky's `Boxed` holds an
    /// `Rc`, so the built graph is not `Sync` and cannot live in a `OnceLock`.
    /// A thread-local also avoids the lock a shared one would need, and the
    /// kernel's worker threads each pay the build once.
    static CACHED_PARSER: Boxed<
        'static,
        'static,
        ParserInput,
        Program,
        extra::Err<Rich<'static, Token, Span>>,
    > = program_parser().boxed();
}

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
        // TODO: this discards a real error. `parse_interpolated_string` now
        // reports an unterminated `$(`, but this path returns `Expr` and has
        // nowhere to put a failure, so `echo ${x:-$(echo hi}` still exits 0
        // with the body kept as literal text — the same silent shape the
        // quoted path just stopped doing. Closing it needs the check on the
        // token stream, where `validate_interpolated_strings` already lives;
        // it only inspects `Token::String` today and would have to read a
        // `VarRef`'s default word too.
        let default_word = unquote_default_word(default_str);
        let default = parse_interpolated_string(&default_word)
            .unwrap_or_else(|_| vec![StringPart::Literal(default_word.clone())]);
        return Expr::VarWithDefault { path, default };
    }

    // Regular variable path
    Expr::VarRef(parse_varpath(raw))
}

/// Detect bash's `${VAR:offset:length}` substring form and explain the kaish
/// spelling; `None` if this is not that shape.
///
/// kaish slices with brackets — `${s[start:end]}`, end-exclusive, the same rule
/// as a list slice — so bash's colon form means something different here and
/// used to expand to nothing at all. Silently: `"${d:0:4}/file"` became
/// `/file`, pointing a destructive command at the wrong path.
///
/// `var_content` is the inside of `${…}`. A colon inside brackets is a slice
/// subscript (`${r[a:b]}`) and is left alone; only a colon at bracket depth 0
/// is the bash form. `${VAR:-default}` is matched earlier and never gets here.
pub(crate) fn bash_substring_hint(var_content: &str) -> Option<String> {
    let mut depth = 0usize;
    let colon = var_content.char_indices().find_map(|(i, c)| match c {
        '[' => {
            depth += 1;
            None
        }
        ']' => {
            depth = depth.saturating_sub(1);
            None
        }
        ':' if depth == 0 => Some(i),
        _ => None,
    })?;
    let (name, rest) = var_content.split_at(colon);
    // `rest` still carries the colon we split on; strip exactly that one.
    let after_offset = &rest[1..];
    // `${v:0:5}` → `${v[0:5]}`; `${v::5}` (bash: offset omitted, so 0) →
    // `${v[0:5]}`; a lone `${v:5}` (bash: from offset 5 to end) → `${v[5:]}`.
    let suggestion = if let Some(length) = after_offset.strip_prefix(':') {
        format!("${{{name}[0:{length}]}}")
    } else if after_offset.contains(':') {
        format!("${{{name}[{after_offset}]}}")
    } else {
        format!("${{{name}[{after_offset}:]}}")
    };
    Some(format!(
        "${{{var_content}}}: kaish slices with brackets, not `:offset:length` — \
         write {suggestion}. Brackets are start:end and end-exclusive, so \
         ${{{name}[0:5]}} is the first five characters and ${{{name}[-3:]}} the last three."
    ))
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
/// A character that may appear after the first in a variable name: ASCII
/// alphanumerics, `_`, or any non-ASCII scalar value. Mirrors the lexer's
/// `SimpleVarRef` class exactly — the unquoted and interpolated doors to a name
/// must agree on where the name ends, or `"$caf\u{e9}"` collects `caf` and
/// substitutes a different variable than `$caf\u{e9}` does.
fn is_name_char(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_' || !c.is_ascii()
}

/// A character that may start a variable name: as [`is_name_char`], minus the
/// digits, which belong to the positional parameters (`$0`..`$9`).
fn is_name_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_' || !c.is_ascii()
}


/// The variable name a token spells, and whether it is an assignment *target*.
///
/// Five tokens can carry a name: `$x`, `${x}`, `${#x}`, an `Ident` that is the
/// target of an assignment, and the `Ident` a `for` loop binds. The neighbors
/// are what tell them apart — the same `Ident` in argument position is
/// ordinary data and its bytes are its own, and `case subject in` puts a
/// data word in front of the very `In` that marks a `for` variable.
///
/// The target flag exists for one rule: a dotted or hashed target is refused by
/// the validator as `E017`/`E018`, which name the exact corrected spelling
/// (`user[email]=x`). This scan runs first and would report a blander message
/// for the same input, so it stands aside for that one shape and lets the
/// better error win. Every other door — `for` included, and the runtime doors
/// that never reach the validator at all — is refused here.
fn name_in_token_kind<'a>(
    tok: &'a Token,
    prev: Option<&Token>,
    next: Option<&Token>,
) -> Option<(&'a str, bool)> {
    match tok {
        Token::SimpleVarRef(name) => Some((name.as_str(), false)),
        Token::VarLength(inner) => Some((root_of(inner), false)),
        Token::VarRef(raw) => raw
            .strip_prefix("${")
            .and_then(|s| s.strip_suffix('}'))
            .map(|r| (root_of(r), false)),
        // An assignment target — but only where a statement can start. The
        // same `Ident`+`Eq` spelling is an ordinary argv `key=value` word in
        // argument position (`echo k=v`), and that word is data: its bytes are
        // its own, and refusing it for holding a character a *name* may not
        // hold rejects a valid program.
        Token::Ident(name)
            if matches!(next, Some(Token::Eq))
                && match prev {
                    None => true,
                    Some(p) => crate::lexer::is_statement_boundary(p) || matches!(p, Token::Local),
                } =>
        {
            Some((name.as_str(), true))
        }
        // `for x in …` binds `x`. Keyed on the `For` before it, not the `In`
        // after it: `case x in …` reads the same one token ahead, and that
        // `x` is a subject to match, not a name.
        Token::Ident(name) if matches!(prev, Some(Token::For)) => Some((name.as_str(), false)),
        _ => None,
    }
}

/// The first name inside an interpolated string that does not read as what it
/// is. A quoted `"$x"` never becomes a name-carrying token of its own — the
/// whole string is one `Token::String` — so without this the quoted spelling
/// of a name is the one door that reads an invisible character in silence.
fn bad_name_in_parts(parts: &[StringPart]) -> Option<crate::name::NameError> {
    fn root(path: &VarPath) -> Option<&str> {
        match path.segments.first() {
            Some(VarSegment::Field(name)) => Some(name.as_str()),
            _ => None,
        }
    }
    for part in parts {
        let bad = match part {
            StringPart::Var(path) | StringPart::VarLength(path) => {
                root(path).and_then(|n| crate::name::validate(n).err())
            }
            StringPart::VarWithDefault { path, default } => root(path)
                .and_then(|n| crate::name::validate(n).err())
                .or_else(|| bad_name_in_parts(default)),
            // A command substitution's own statements were parsed by `parse`,
            // which ran this same scan over them.
            _ => None,
        };
        if bad.is_some() {
            return bad;
        }
    }
    None
}

/// The root of a variable path — everything before the first subscript or
/// dotted field. Only the root is a name; a subscript is data.
fn root_of(inner: &str) -> &str {
    let end = inner.find(['[', '.', ':', '-']).unwrap_or(inner.len());
    &inner[..end]
}

pub(crate) fn parse_varpath(raw: &str) -> VarPath {
    let segment_strs = lexer::parse_var_ref(raw).unwrap_or_default();
    let segments = segment_strs
        .into_iter()
        .enumerate()
        .map(|(i, s)| {
            if i == 0 {
                // The root name (or the special `?`). Normalized like every
                // other door to a name; a subscript below is data and is not.
                VarSegment::Field(crate::ast::normalize_name(s))
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
    // Integer index: `[0]`, `[-1]`. A leading zero makes the word text, so
    // `[007]` falls through to the bareword key below — on a record it finds
    // the "007" key, and on a list it raises the loud error in `scope.rs`.
    if !lexer::is_leading_zero_numeral(inner)
        && let Ok(i) = inner.parse::<i64>()
    {
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
fn parse_interpolated_string_spanned(
    s: &str,
    base_offset: usize,
) -> Result<Vec<SpannedPart>, String> {
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
                let mut closed = false;
                while let Some(&c) = chars_vec.get(i) {
                    i += 1;
                    pos += c.len_utf8();
                    if c == '(' {
                        depth += 1;
                        cmd_content.push(c);
                    } else if c == ')' {
                        depth -= 1;
                        if depth == 0 {
                            closed = true;
                            break;
                        }
                        cmd_content.push(c);
                    } else {
                        cmd_content.push(c);
                    }
                }
                if !closed {
                    return Err("unterminated command substitution: missing `)`".to_string());
                }
                // Both silent fallbacks are closed here rather than by reusing
                // `parse_interpolated_string`: a heredoc body is not the inside
                // of a double-quoted string and may hold a raw `"`, so the
                // string scanner mis-reads `stamp = "$(date +%s)"` as
                // unterminated. The escape models genuinely differ, which is
                // why this sibling exists at all.
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
                    return Err(format!(
                        "syntax error in command substitution: $({cmd_content})"
                    ));
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
                    // `${#x:-y}` has no meaning: bash rejects it as a bad
                    // substitution, and the unquoted door here already refuses
                    // it. Without this the `#` strip wins and the whole
                    // `x:-y` becomes the path, which resolves to unset and
                    // reports 0 — a wrong length, silently, in the quoted
                    // spelling only.
                    if find_default_separator_in_content(name).is_some() {
                        return Err(format!(
                            "${{#{name}}}: a length cannot carry a default — \
                             ${{#NAME}} counts, ${{NAME:-default}} substitutes. \
                             Write ${{#NAME}} on a name you have set, or test it \
                             first."
                        ));
                    }
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
                    // Propagated, not discarded. The twin in
                    // `parse_interpolated_string` already uses `?` here; this
                    // copy swallowed a malformed `$(` in the default word and
                    // kept it as literal text, so a heredoc body carrying
                    // `${x:-$(echo hi}` ran with the substitution silently
                    // dropped.
                    let default = parse_interpolated_string(&unquote_default_word(default_str))?;
                    StringPart::VarWithDefault { path, default }
                } else if let Some(msg) = bash_substring_hint(&var_content) {
                    return Err(msg);
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
            } else if next.map(is_name_start).unwrap_or(false) {
                push_literal(&mut current_text, &mut current_text_start, pos, &mut parts);
                i += 1; // consume '$'
                pos += 1;
                let mut var_name = String::new();
                while let Some(&c) = chars_vec.get(i) {
                    if is_name_char(c) {
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

    Ok(parts)
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

                // Find the matching ')' the same way the unquoted `$(...)`
                // form does: tokenize what remains and walk it with
                // `find_cmd_subst_close` (the plain-slice twin of
                // `CmdSubstFrames`) instead of counting raw `(`/`)`
                // characters. A per-character count can't tell a
                // case-branch pattern's unpaired `)` (`case $x in a) …`)
                // from a real close, and it also miscounts a literal
                // `(`/`)` sitting inside a quoted argument of the
                // substitution itself (`$(echo "(")`).
                let remainder: String = chars.clone().collect();
                let close = lexer::tokenize(&remainder).ok().and_then(|toks| {
                    let toks: Vec<(Token, Span)> = toks
                        .into_iter()
                        .map(|sp| (sp.token, (sp.span.start..sp.span.end).into()))
                        .collect();
                    find_cmd_subst_close(&toks).map(|idx| toks[idx].1)
                });
                // No close — or a remainder that does not even tokenize —
                // means the substitution ran past the closing quote. Report
                // it before `parse` sees the body: the body can be a valid
                // program on its own (`echo hi`), so falling back to it runs
                // a substitution nobody closed, and the plan then renders a
                // `)` the writer never typed.
                let Some(rparen_span) = close else {
                    return Err("unterminated command substitution: missing `)`".to_string());
                };
                let (cmd_content, consume_bytes) =
                    (remainder[..rparen_span.start].to_string(), rparen_span.end);
                let mut consumed = 0usize;
                while consumed < consume_bytes {
                    match chars.next() {
                        Some(c) => consumed += c.len_utf8(),
                        None => break,
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
                    // `${#x:-y}` has no meaning: bash rejects it as a bad
                    // substitution, and the unquoted door here already refuses
                    // it. Without this the `#` strip wins and the whole
                    // `x:-y` becomes the path, which resolves to unset and
                    // reports 0 — a wrong length, silently, in the quoted
                    // spelling only.
                    if find_default_separator_in_content(name).is_some() {
                        return Err(format!(
                            "${{#{name}}}: a length cannot carry a default — \
                             ${{#NAME}} counts, ${{NAME:-default}} substitutes. \
                             Write ${{#NAME}} on a name you have set, or test it \
                             first."
                        ));
                    }
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
                } else if let Some(msg) = bash_substring_hint(&var_content) {
                    return Err(msg);
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
            } else if chars.peek().copied().map(is_name_start).unwrap_or(false) {
                // Simple variable reference $NAME
                if !current_text.is_empty() {
                    parts.push(StringPart::Literal(std::mem::take(&mut current_text)));
                }

                // Collect identifier characters
                let mut var_name = String::new();
                while let Some(&c) = chars.peek() {
                    if is_name_char(c) {
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

    // bash's `${VAR:offset:length}` is checked on the token stream, before the
    // grammar runs, for the reason documented on `command_parser`: a `try_map`
    // rejection inside `choice` loses its own message to a competing
    // alternative's. This shape needs to *teach* the bracket form, so it is
    // caught where nothing can outvote it.
    //
    // A name that does not read as what it is — one holding whitespace, a
    // bidi control, or a zero-width character — is caught in the same scan and
    // for the same reason: the message has to name a character the reader
    // cannot see, so it must not lose to a competing alternative's.
    for (i, (tok, span)) in tokens.iter().enumerate() {
        let prev = i.checked_sub(1).and_then(|j| tokens.get(j)).map(|(t, _)| t);
        if let Some((name, is_target)) =
            name_in_token_kind(tok, prev, tokens.get(i + 1).map(|(t, _)| t))
        {
            if let Err(bad) = crate::name::validate(name) {
                // `.` and `#` in an assignment target belong to the validator,
                // which refuses them as `E017`/`E018` and names the corrected
                // spelling (`user[email]=x`) where this scan can only describe
                // the shape. Those two codes are a published surface, so the
                // scan stands aside for exactly that case — and only that
                // case. A dot or hash anywhere the validator never looks
                // (`for`, `read`, `unset`, `push`, `scatter --as`) is refused
                // right here, which is the hole this rule exists to close.
                let defer = is_target && matches!(bad.ch, '.' | '#');
                if !defer {
                    return Err(vec![ParseError { span: *span, message: bad.to_string() }]);
                }
            }
        }
        // The quoted spelling of a read: `"$x"` arrives whole, so its names
        // have to be dug out rather than met as tokens. An all-ASCII string
        // cannot hold a name this rule refuses — every character the rule
        // rejects is non-ASCII — so the common string never pays for the
        // second parse.
        if let Token::String(s) = tok {
            if !s.is_ascii() && s.contains('$') {
                if let Ok(parts) = parse_interpolated_string(s) {
                    if let Some(bad) = bad_name_in_parts(&parts) {
                        return Err(vec![ParseError { span: *span, message: bad.to_string() }]);
                    }
                }
            }
        }
        let message = match tok {
            Token::VarRef(raw) => raw
                .strip_prefix("${")
                .and_then(|s| s.strip_suffix('}'))
                .filter(|_| find_default_separator(raw).is_none())
                .and_then(bash_substring_hint),
            // A quoted `"${d:0:4}/file"` arrives as one string token. The
            // guard keeps this off the hot path — an interpolation with a
            // top-level colon is the only thing worth re-scanning for.
            Token::String(s) if s.contains("${") && s.contains(':') => {
                parse_interpolated_string(s).err()
            }
            _ => None,
        };
        if let Some(message) = message {
            return Err(vec![ParseError {
                span: *span,
                message,
            }]);
        }
    }

    // End-of-input span
    let end_span: Span = (source.len()..source.len()).into();

    parse_tokens(tokens, end_span, (0..0).into())
}

/// Parse an already-tokenized slice into a `Program`, running the same
/// structural well-formedness checks [`parse`] runs on the top-level source.
///
/// Shared by [`parse`] and `cmd_subst_parser`'s route-C recursive descent into
/// an unquoted `$(...)` body (GH #194): the lexer's token spans are absolute
/// byte offsets into the original source in both cases, so a caller handing
/// in a sub-slice needs no span-rebasing — errors from this function already
/// point at the right place.
///
/// `stdin_anchor` is where the ambiguous-multiple-stdin-redirect diagnostic
/// (which carries no AST span of its own) points: the source start for the
/// top level, or the `$(...)` span for a nested body.
fn parse_tokens(
    tokens: Vec<(Token, Span)>,
    end_span: Span,
    stdin_anchor: Span,
) -> Result<Program, Vec<ParseError>> {
    // Parse with the per-thread parser, built once (see `CACHED_PARSER`). A
    // nested `$(...)` body reaches this from inside a `try_map` closure that
    // is itself running as part of a `CACHED_PARSER.with(...)` call on the
    // same thread — reentrant, but sound: `with` just hands out a shared
    // `&Boxed<...>` after the one-time init completes, and nothing here is a
    // `RefCell`, so a second concurrent `&` borrow on the same thread is
    // ordinary aliasing, not a conflict.
    //
    // `tokens.clone()` costs one extra token-vec copy on every call (paid
    // even on success) so `tokens` survives for `validate_cmd_subst_bodies`
    // below — see that function's doc comment for why a failure needs a
    // second look with the original tokens in hand.
    let input = Stream::from_iter(tokens.clone()).map(end_span, keep_pair as PairFn);
    let result = CACHED_PARSER.with(|parser| parser.parse(input));

    let program = result.into_result().map_err(|errs| {
        // A malformed unquoted `$(...)` body can lose its own precise error
        // to chumsky's choice/alt bookkeeping (see `validate_cmd_subst_bodies`'s
        // doc comment) in favor of a generic one from an unrelated sibling
        // `choice` alternative. Re-validate every `$(...)` body directly,
        // outside that machinery, so a body failure reports its own message.
        // Cheap on the common (successful) path — this only runs once the
        // grammar has already failed.
        if let Err(specific) = validate_cmd_subst_bodies(&tokens) {
            return specific;
        }
        // Same chumsky bookkeeping loss, for `$(...)` inside a double-quoted
        // string instead of the unquoted grammar (see
        // `validate_interpolated_strings`'s doc comment).
        if let Err(specific) = validate_interpolated_strings(&tokens) {
            return specific;
        }
        // And the same, for a heredoc body's own `$(...)`.
        if let Err(specific) = validate_heredoc_bodies(&tokens) {
            return specific;
        }
        // `break 007` / `continue 007`: the count grammar matches `Token::Int`
        // and a leading zero no longer produces one, so chumsky reports a
        // shape mismatch against the whole statement alternative set. Name
        // the leading zero instead.
        if let Err(specific) = validate_leading_zero_counts(&tokens) {
            return specific;
        }
        // `reject_glued_args` raises this from inside a `try_map` wrapping the
        // whole argv, where chumsky's alt bookkeeping swaps in a shallower
        // sibling's span — `git show HEAD:x.py` reported at `show`. Re-derive
        // the paste from the tokens instead and report the span from here.
        //
        // Only when the standing error IS that rejection: this corrects a span
        // and must never author a verdict. The scan is an approximation of the
        // argv grammar and finds adjacency the grammar accepts, so the gate is
        // load-bearing. Scanning from the grammar's own position forward keeps
        // it from blaming a legal run in an earlier clause.
        if errs.iter().all(is_glued_args_error)
            && let Some(from_offset) = errs.iter().map(|e| e.span().start).min()
            && let Err(specific) = validate_glued_args(&tokens, from_offset)
        {
            return specific;
        }
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
            // Redirects carry no AST span; the message is the actionable
            // part. Precise columns would require spanning `Redirect` —
            // deferred.
            span: stdin_anchor,
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
        // (`user[email]=x cmd`) is illegal here, not just unsupported: a
        // structured value cannot cross the process boundary into a child's
        // environment, so there is nothing correct to assign.
        //
        // Using `ident_parser()` directly, not `lvalue_path_parser()`, means a
        // bracket run before `=` never gets a chance to parse as a path here.
        // Either it is absent (plain ident), or the lexer's lvalue suppression
        // fires and the stray `LBracket` fails this parser. Both fall through
        // to a real parse error rather than being accepted silently.
        let env_prefix_assign = ident_parser()
            .then_ignore(just(Token::Eq))
            .then(value_expr_parser())
            .map(|(name, value)| Assignment { path: VarPath::simple(name), value, local: false });
        let env_scoped = env_prefix_assign
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .then(pipeline_parser(command_stage_parser()).map(pipeline_into_stmt))
            .map(|(assignments, body)| Stmt::EnvScoped {
                assignments,
                body: Box::new(body),
            });

        // The compound statements. They reach `base_statement` only through
        // `pipeline_parser`, which parses a lone compound and hands it back
        // unwrapped — a compound and a compound-headed pipeline are the same
        // alternative, so neither can shadow the other. Parsing them as
        // separate alternatives is what produced "found '|' expected '&&'":
        // `for_parser` sat ahead of the pipeline, consumed through `done`, and
        // the `&&`/`||` fold below then met the `|`.
        let compound = choice((
            if_parser(stmt.clone()).map(Stmt::If),
            for_parser(stmt.clone()).map(Stmt::For),
            while_parser(stmt.clone()).map(Stmt::While),
            case_parser(stmt.clone()).map(Stmt::Case),
        ))
        .boxed();

        // Base statement (without chaining)
        let base_statement = choice((
            just(Token::Newline).to(Stmt::Empty),
            set_command,
            env_scoped,
            assignment_parser().map(Stmt::Assignment),
            // Shell-style functions (use $1, $2 positional params)
            posix_function_parser(stmt.clone()).map(Stmt::ToolDef),  // name() { }
            bash_function_parser(stmt.clone()).map(Stmt::ToolDef),   // function name { }
            break_stmt,
            continue_stmt,
            return_stmt,
            exit_stmt,
            test_expr_stmt_parser().map(Stmt::Test),
            // Note: 'true' and 'false' are handled by command_parser/pipeline_parser
            pipeline_parser(choice((
                compound.map(|s| PipelineStage::Compound(Box::new(s))),
                command_stage_parser(),
            )))
            .map(pipeline_into_stmt),
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
        // A leading-zero numeral lexes as `NumberIdent`, so without this arm
        // `r[007]=v` was a parse error while `${r[007]}` read fine. Both are
        // the same text key now.
        select! { Token::NumberIdent(s) => parse_subscript(&s) },
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
            // Normalized like every other door to a name, so binding through
            // one spelling and reading through another reaches one variable.
            let mut segments = vec![VarSegment::Field(crate::ast::normalize_name(name))];
            segments.extend(subscripts);
            VarPath { segments }
        })
        .labelled("lvalue path")
}

/// Assignment: `NAME=value` / `NAME[sub]=value` (bash-style), or
/// `local NAME = value` (scoped). Bracket paths are lvalues here — see
/// `docs/LANGUAGE.md`, "Assignment — bracket-path lvalues". They resolve at
/// runtime in `Scope::walk_write`, which shares the read resolver's per-hop
/// classification so a read and a write disagree about no path.
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
        // Dash/plus bare words and flag-shaped tokens (GH #144): a case
        // pattern that happens to look like a flag (`-h`, `--help`, `+x`) or
        // an unrecognized dash/plus prefix (`---`, `-%`, `+%s`) is still a
        // literal glob pattern in case position, not a flag — the lexer
        // strips the leading dash/plus off `ShortFlag`/`LongFlag`/`PlusFlag`,
        // so put it back. Grouped in a nested `choice()` to stay under
        // chumsky's 26-element tuple limit for the outer `choice()`.
        choice((
            select! { Token::DoubleDashBare(s) => s },
            select! { Token::PlusBare(s) => s },
            select! { Token::MinusBare(s) => s },
            select! { Token::MinusAlone => "-".to_string() },
            select! { Token::DoubleDash => "--".to_string() },
            select! { Token::ShortFlag(s) => format!("-{}", s) },
            select! { Token::LongFlag(s) => format!("--{}", s) },
            select! { Token::PlusFlag(s) => format!("+{}", s) },
        )),
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

/// Pipeline: `stage | stage | stage [&]`.
///
/// `stage` is the caller's stage parser — `command_stage_parser()` alone where
/// only a command is legal, or a compound statement ahead of it in the
/// positions that host one. Taking it as a parameter keeps every compound
/// inside the single `recursive(|stmt| …)` in `statement_parser`, and lets one
/// alternative serve both a bare compound and a compound-headed pipeline:
/// `pipeline_into_stmt` unwraps a lone compound stage back to its statement.
fn pipeline_parser<'tokens, I, S>(
    stage: S,
) -> impl Parser<'tokens, I, Pipeline, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
    S: Parser<'tokens, I, PipelineStage, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
{
    stage
        .separated_by(just(Token::Pipe))
        .at_least(1)
        .collect::<Vec<_>>()
        .then(just(Token::Amp).or_not())
        .map(|(stages, bg)| Pipeline {
            stages,
            background: bg.is_some(),
        })
        .labelled("pipeline")
        .boxed()
}

/// A single command as a pipeline stage.
fn command_stage_parser<'tokens, I>(
) -> impl Parser<'tokens, I, PipelineStage, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    command_parser().map(PipelineStage::Command)
}

/// Command: `name args... [redirects...]`
/// Command names can be identifiers, 'true', 'false', ':' (null command), or
/// '.' (source alias).
fn command_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Command, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // Command name can be an identifier, path, 'true', 'false', ':' (null
    // command), '.' (source alias), or ./path. A bare `:` reaches here only
    // when nothing adjacent fused it into a word — inside brackets and braces
    // the colon is structural (record entries, slices, character classes) and
    // never reaches a command-name position.
    let command_name = choice((
        ident_parser(),
        path_parser(),
        select! { Token::DotSlashPath(s) => s },
        just(Token::True).to("true".to_string()),
        just(Token::False).to("false".to_string()),
        just(Token::Colon).to(":".to_string()),
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
    // (verified empirically 2026-06-07).
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
    if p.stages.len() == 1 && !p.background && p.stages[0].redirects().is_empty() {
        match p.stages.into_iter().next() {
            // A lone compound stage is just that statement — `for … done` on
            // its own parses to `Stmt::For`, exactly as it did before the
            // pipeline position learned to host one.
            Some(PipelineStage::Compound(stmt)) => *stmt,
            Some(PipelineStage::Command(cmd)) => Stmt::Command(cmd),
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
                RedirectKind::Stdin | RedirectKind::HereDoc(_) | RedirectKind::HereString
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
        Stmt::Pipeline(p) => p.stages.iter().any(|stage| match stage {
            PipelineStage::Command(cmd) => command_has_ambiguous_stdin(cmd),
            PipelineStage::Compound(inner) => stmt_has_ambiguous_stdin(inner),
        }),
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

/// True for the argv-fragment `Arg` shapes eligible for the glue check:
/// bareword/expr positionals and long flags.
///
/// `ShortFlag` is excluded: `cut -d,` and `grep -A$n` are the getopt glued-value
/// idiom the kernel binder already supports, and a punctuation or substitution
/// value genuinely arrives as two adjacent `Arg`s. `--flag` has no such idiom,
/// so a `LongFlag` glued to a fragment is always an accident.
///
/// `Named`/`WordAssign` ARE candidates: their own fusion covers the boundaries
/// inside the word, not a fragment glued to the END of the value, and
/// `--a=1--b=2` is pasting by any other name.
fn is_glue_candidate(arg: &Arg) -> bool {
    matches!(
        arg,
        Arg::Positional(_) | Arg::LongFlag(_) | Arg::Named { .. } | Arg::WordAssign { .. }
    )
}

/// Reject a run of argv fragments produced by glued (zero source-gap)
/// tokens — kaish does no token pasting, so an unquoted `/tmp/$(echo
/// x).txt` lexes into three fragments (`/tmp/`, the substitution, `.txt`)
/// that would otherwise silently bind as THREE separate args, and
/// `--flag$(echo x)` glues a flag straight to the next fragment with no
/// error at all. Shared by the pre-`--` and post-`--` argument grammars
/// (GH #189: the post-`--` half of this used to be unchecked entirely — a
/// script relying on `--` to end flag parsing got a silent argv-splat
/// instead of this same helpful error).
///
/// A comma-bearing word (`cut -f1,3`, `sort -k2,2n`, `echo a,b`) used to
/// trip this guard and get a comma-specific "kaish reserves `,`" hint —
/// that was never true outside a `[...]`/`{...}` literal or pattern, and
/// the lexer now folds a bare comma into the surrounding bareword before
/// the parser ever sees separate fragments (see `lexer::flush_glob_run`),
/// so a comma-bearing word no longer reaches this function as two glued
/// `Arg`s at all. Every remaining case is genuine token pasting.
fn reject_glued_args<'src>(
    args: Vec<(Arg, Span)>,
) -> Result<Vec<Arg>, Rich<'src, Token, Span>> {
    for pair in args.windows(2) {
        let (prev, prev_span) = &pair[0];
        let (next, next_span) = &pair[1];
        if is_glue_candidate(prev) && is_glue_candidate(next) && prev_span.end == next_span.start {
            return Err(Rich::custom(*next_span, GLUED_ARGS_MESSAGE));
        }
    }
    Ok(args.into_iter().map(|(arg, _)| arg).collect())
}

/// The message `reject_glued_args` and [`validate_glued_args`] both raise —
/// kept as one constant so the raw-token fallback can never drift from the
/// grammar-level check it exists to correct the span for.
const GLUED_ARGS_MESSAGE: &str = "adjacent words with no space between them are not joined into \
     one argument (kaish does no token pasting); quote the whole word, e.g. \
     \"/tmp/$(echo x).txt\" or \"$dir/out.txt\"";

/// True when `e` is `reject_glued_args`'s own rejection and nothing else —
/// the only case where [`validate_glued_args`] may restate the span.
///
/// Any other `Custom` is a purpose-built diagnosis, and `flat_merge` prefers
/// `Custom`, so it is the message standing here and must reach the caller
/// untouched. An `ExpectedFound` means the grammar never judged this a paste,
/// and inventing that verdict would change what kaish rejects. No skip list is
/// needed: a new guard is protected the day it is written.
fn is_glued_args_error(e: &Rich<'_, Token, Span>) -> bool {
    matches!(e.reason(), RichReason::Custom(msg) if msg.as_str() == GLUED_ARGS_MESSAGE)
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
    // source span so we can reject the silent argv-splat: two argv fragments
    // with no whitespace between them (`/tmp/$(echo x).txt` → 3 args,
    // `--flag$(echo x)` → a flag glued to one). kaish does no token pasting,
    // so an unquoted interpolated word fragments into separate args; the fix
    // is to quote the whole word. Single-token words (`file.txt`, `v1.2.3`)
    // are one arg and never trigger this. See `reject_glued_args`.
    let pre_dash = arg_before_double_dash_parser()
        .map_with(|arg, e| -> (Arg, Span) { (arg, e.span()) })
        .repeated()
        .collect::<Vec<(Arg, Span)>>()
        .try_map(|args, _span| reject_glued_args(args));

    // The `--` marker itself
    let double_dash = select! {
        Token::DoubleDash => Arg::DoubleDash,
    };

    // Arguments after `--` (flags become positional strings)
    let post_dash_arg = choice((
        // `--flag=value` — one operand, like `name=value` below. Long flags
        // only; see the production's own doc for why `-x=value` is not here.
        // This must precede the bare-flag rule, which would otherwise take
        // `--flag` and leave `=value` to be rejected as a glued word. Past
        // `--` there is no flag for a value to belong to, so the pair is
        // simply text; the binders stringify it the way they already
        // stringify a post-`--` `WordAssign`.
        post_dash_flag_value_parser(),
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

    // Same glue guard as `pre_dash` (GH #189): before this, a post-`--`
    // glued word silently split into separate positionals instead of
    // erroring — the pre-`--` guard never ran over these tokens at all.
    let post_dash = post_dash_arg
        .map_with(|arg, e| -> (Arg, Span) { (arg, e.span()) })
        .repeated()
        .collect::<Vec<(Arg, Span)>>()
        .try_map(|args, _span| reject_glued_args(args));

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
/// because: a statement-level `if`/`for`/… is decided before arg parsing (the
/// compound parsers are the pipeline's first stage alternative, tried ahead of
/// `command_parser`), `command_name` never accepts these tokens, and the
/// `key=value` rule requires the key span-adjacent to `=` — a real `if <cond>`
/// has a space and never matches. See docs/binary-data.md.
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
/// `--flag=value` after `--`, as a single `Arg::Named`.
///
/// The binders route a `Named` past `--` into a stringified
/// `"--flag=value"` positional, exactly as they do for `WordAssign` — so this
/// production only has to say "these three tokens are one word". Adjacency is
/// checked the same way `word_assign_arg_parser` checks it, which keeps
/// `-- --flag = value` (spaced) a separate-words error rather than silently
/// pasting.
///
/// Long flags only, because `Arg::Named` means a long flag everywhere else
/// and the binders reconstruct it with `--`. `-x=value` is refused on BOTH
/// sides of `--` today (`echo -n=1` is the same parse error), so accepting it
/// only after `--` would trade one asymmetry for another; that spelling is
/// its own question about the flag grammar.
fn post_dash_flag_value_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Arg, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    select! { Token::LongFlag(name) => name }
        .map_with(|s, e| -> (String, Span) { (s, e.span()) })
    .then(just(Token::Eq).map_with(|_, e| -> Span { e.span() }))
    .then(primary_expr_parser().map_with(|expr, e| -> (Expr, Span) { (expr, e.span()) }))
    .try_map(
        |(((key, key_span), eq_span), (value, value_span)): (((String, Span), Span), (Expr, Span)),
         span| {
            if key_span.end != eq_span.start || eq_span.end != value_span.start {
                Err(Rich::custom(
                    span,
                    "a flag and its value must not have spaces around '=' \
                     (use '--flag=value' not '--flag = value')",
                ))
            } else {
                Ok(Arg::Named { key, value })
            }
        },
    )
}

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
/// `target` parses the file word (and here-string body); the sole caller
/// (`command_parser`) passes a fresh `primary_expr_parser()`. `target` stays
/// a generic parameter rather than calling `primary_expr_parser()` directly
/// here for history, not necessity: `cmd_subst_parser` used to pass its own
/// recursive `expr` handle so a redirect inside `$(...)` could parse without
/// an unbounded `cmd_subst → redirect → primary_expr → cmd_subst` construction
/// cycle. Route C (GH #194) replaced that hand-rolled grammar with a
/// recursive descent through the full program grammar at parse time, so
/// `cmd_subst_parser` no longer calls this function at all — the cycle this
/// threading avoided no longer exists here, but the shape was left as-is
/// since a second caller could reintroduce the same need.
fn redirect_parser<'tokens, I, T>(
    target: T,
) -> impl Parser<'tokens, I, Redirect, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
    T: Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
{
    // `target` only ever parses ONE expression. An unquoted target that
    // spans multiple lexical fragments with no gap between them
    // (`/tmp/$(echo x).txt` lexes as three tokens: "/tmp/", the command
    // substitution, ".txt") only binds its first fragment as the target —
    // the rest dangle, and the surrounding statement grammar rejects them
    // with a generic chumsky "expected ..." message that never mentions
    // quoting (GH #189). Peek (`rewind`, consumes nothing) for an
    // immediately-adjacent further expr fragment and turn that into the same
    // "quote it" hint `reject_glued_args` gives positional args, worded for a
    // redirect target. The peek reuses the caller's own `target` clone
    // (never a fresh `primary_expr_parser()` built here) — see the
    // construction-cycle note in this function's doc comment above.
    let target = target
        .clone()
        .map_with(|expr, e| -> (Expr, Span) { (expr, e.span()) })
        .then(target.clone().map_with(|_, e| e.span()).rewind().or_not())
        .try_map(|((expr, span), glued), _| match glued {
            Some(next_span) if next_span.start == span.end => Err(Rich::custom(
                next_span,
                "adjacent words with no space between them are not joined into the redirect \
                 target (kaish does no token pasting); quote the whole target, e.g. \
                 \"/tmp/$(echo x).txt\"",
            )),
            _ => Ok(expr),
        })
        .boxed();

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
        .try_map(|data: HereDocData, span| {
            // How it was written, kept for the plan. The target below is what
            // executes: tab-stripped, or split into interpolation parts.
            let meta = HereDocMeta {
                delimiter: data.delimiter.clone(),
                literal: data.literal,
                strip_tabs: data.strip_tabs,
                body: data.source_body.clone(),
                body_offset: data.body_start_offset,
            };
            let target = if data.literal {
                let body = if data.strip_tabs {
                    crate::interpreter::strip_leading_tabs(&data.content)
                } else {
                    data.content
                };
                Expr::Literal(Value::String(body))
            } else {
                let parts =
                    parse_interpolated_string_spanned(&data.content, data.body_start_offset)
                        .map_err(|msg| Rich::custom(span, msg))?;
                // If there's only one literal part and no tab stripping is
                // needed, simplify to Expr::Literal — keeps the AST shape
                // identical to the pre-spans path for trivial bodies.
                if parts.len() == 1 && !data.strip_tabs {
                    if let StringPart::Literal(text) = &parts[0].part {
                        return Ok(Redirect {
                            kind: RedirectKind::HereDoc(meta),
                            target: Expr::Literal(Value::String(text.clone())),
                        });
                    }
                }
                Expr::HereDocBody {
                    parts,
                    strip_tabs: data.strip_tabs,
                }
            };
            Ok(Redirect {
                kind: RedirectKind::HereDoc(meta),
                target,
            })
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
///   `docs/LANGUAGE.md`, "Shape guards")
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
    // key-in-record; see docs/LANGUAGE.md, "Membership"). There is no dedicated
    // `not` token — it lexes as a plain identifier, so `not_in` matches the
    // two-word sequence `Ident("not") In`. Try `not_in` before `in` below, or
    // `e not in c` parses as `e in` and then fails on the stray `not` bareword.
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

    // Command as condition (includes true/false/: as command names)
    // The command's exit code determines truthiness (0 = true, non-zero = false)
    let command_condition = command_parser().map(Expr::Command);

    // Base: test expr OR command
    let base = choice((test_expr_condition, command_condition));

    // `!` negates the command that follows it, BEFORE `&&`/`||` fold below —
    // bash reads `! true && true` as `(! true) && true`. Repeated so `! ! x`
    // parses, which bash also accepts.
    let base = just(Token::Bang)
        .repeated()
        .foldr(base, |_, inner| Expr::Not(Box::new(inner)));

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
/// interchangeable (see docs/LANGUAGE.md, "Construction — list/record
/// literals"). Newlines are consumed rather than treated as statement
/// terminators, so a multi-line literal does not end the assignment early.
/// A bare element nests as ONE item; `...` flattens a list operand's elements
/// into this one (spread).
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

    // Guard against the classic unquoted multi-word value mistake
    // (`{msg: hello world}`): without this, "world" is consumed by the
    // NEXT `entry` attempt as a candidate key (kaish allows a bare
    // space — no comma — between entries, so `{a: 1 b: 2}` is legal), which
    // then fails at `}` expecting `:` — chumsky's generic message ("found
    // '}' expected ':'") without ever naming the actual mistake. Peeked via
    // `.rewind()` (consumes nothing — a legitimate following entry, comma
    // or not, is still parsed normally by the outer `repeated()`): an
    // `Ident` right after this value that ISN'T itself followed by `:` can
    // only be a stray unquoted word, since a real next entry always looks
    // like `Ident :` (or a quoted key) at this position.
    let stray_bareword_after_value = select! { Token::Ident(s) => s }
        .then(just(Token::Colon).or_not())
        .rewind()
        .or_not()
        .try_map(|maybe, span| match maybe {
            Some((word, None)) => Err(Rich::custom(
                span,
                format!(
                    "record value: unexpected word \"{word}\" after the value — a multi-word \
                     value must be quoted, e.g. {{key: \"hello world\"}}"
                ),
            )),
            _ => Ok(()),
        });

    let entry = key
        .then_ignore(just(Token::Colon))
        .then(value)
        .then_ignore(stray_bareword_after_value)
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

    // No longer `recursive()`: `cmd_subst_parser` used to need this closure's
    // own `expr` handle to parse `$(...)`'s body and redirect targets, which
    // is what created the `cmd_subst → primary_expr → cmd_subst` construction
    // cycle (see `cmd_subst_parser`'s doc comment). Route C (GH #194) parses
    // the `$(...)` body from raw captured tokens instead, so nothing in this
    // choice references itself anymore.
    choice((
        positional,
        arithmetic,
        cmd_subst_parser(),
        var_expr_parser(),
        interpolated_string_parser(),
        literal_parser().map(Expr::Literal),
        numeric_literal_parser(),
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
            // `cut -d, -f2` / `tr -d ,` delimiter idiom. This is reached
            // only by a comma with no adjacent bareword to fold into
            // (whitespace on both sides, e.g. `cut -d , -f2`, or a
            // neighbor the lexer doesn't fuse across, e.g. `,$VAR`) — a
            // comma glued to a bareword (`echo a,b`, `sort -k 2,2n`) is
            // already folded into ONE token before the parser runs (see
            // `lexer::flush_glob_run`), and a comma inside a
            // `[...]`/`{...}` literal or pattern is consumed there
            // instead (list/record literals, brace expansion — see
            // `docs/LANGUAGE.md`, "Construction").
            Token::Comma => Expr::Literal(Value::String(",".into())),
            // Bare colon in argument position is the literal ":" — the
            // `awk -F: '{print $1}'` / `--field-separator=:` idiom. In
            // command-name position the colon is the null command (see
            // `command_name` in `command_parser`); here it is only reached
            // after a command name has been parsed, so there is no
            // ambiguity with that form.
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
    .boxed()
}

/// Variable reference: `${VAR}`, `${VAR.field}`, `${VAR:-default}`, or `$VAR` (simple form).
/// Returns Expr directly to support both VarRef and VarWithDefault.
fn var_expr_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        select! { Token::VarRef(raw) => raw }.try_map(|raw, span| {
            // The unquoted twin of the check in `parse_interpolated_string`:
            // bash's `${v:0:5}` is a different slice convention here, and used
            // to expand to nothing at all.
            let inner = raw
                .strip_prefix("${")
                .and_then(|s| s.strip_suffix('}'))
                .unwrap_or(&raw);
            if !raw.starts_with("${?}")
                && !raw.starts_with("${$}")
                && find_default_separator(&raw).is_none()
                && let Some(msg) = bash_substring_hint(inner)
            {
                return Err(Rich::custom(span, msg));
            }
            // `${x:-WORD}`'s default word expands like a double-quoted string,
            // and `parse_var_expr` returns an `Expr` with nowhere to put a
            // failure — so a malformed `$(` inside the word was kept as
            // literal text and the whole statement ran. Checked here, at the
            // grammar, rather than on the token stream: a nested default word
            // (`$(echo ${x:-$(echo hi})`) is a `VarRef` at whatever depth it
            // occurs, so this one rule reaches every nesting.
            if let Some(colon) = find_default_separator(&raw)
                && raw.len() > colon + 3
                && let Err(msg) =
                    parse_interpolated_string(&unquote_default_word(&raw[colon + 2..raw.len() - 1]))
            {
                return Err(Rich::custom(span, msg));
            }
            Ok(parse_var_expr(&raw))
        }),
        select! { Token::SimpleVarRef(name) => Expr::VarRef(VarPath::simple(name)) },
    ))
    .labelled("variable reference")
}

/// Capture the token stream inside `$(...)`, consuming through the matching
/// closing `)`.
///
/// A `)` can close a nested `$(`, a plain `(`, or be a case-branch pattern
/// terminator with no matching open at all (`case $x in a) … ;; esac` is
/// legal with no leading `(`). Which one a given `)` means depends on what
/// is innermost at that point, so this is a stack of [`CmdSubstFrame`]s, not
/// a flat counter — see that type's doc comment for the rule.
///
/// Token spans are untouched — they stay the lexer's absolute byte offsets
/// into the original source. That is what lets `cmd_subst_parser` hand the
/// captured slice straight to [`parse_tokens`] and get diagnostics anchored
/// at their true position with no span-rebasing.
///
/// Returns the body tokens (the closing `)` is not included) and that `)`'s
/// own span, which the caller uses as the sub-parse's end-of-input point.
/// A `$(...)` body's captured tokens (the closing `)` not included) and that
/// `)`'s own span. Named so [`cmd_subst_body_tokens`]'s return type reads —
/// clippy's `type_complexity` flags the bare tuple spelled out inline.
type CmdSubstBody = (Vec<(Token, Span)>, Span);

fn cmd_subst_body_tokens<'tokens, I>(
) -> impl Parser<'tokens, I, CmdSubstBody, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    custom(|inp| {
        let mut tracker = CmdSubstFrames::default();
        let mut body: Vec<(Token, Span)> = Vec::new();
        loop {
            let before = inp.cursor();
            match inp.next() {
                None => {
                    let span = inp.span_since(&before);
                    return Err(Rich::custom(
                        span,
                        "unterminated command substitution: missing `)`",
                    ));
                }
                Some(tok) => {
                    let span = inp.span_since(&before);
                    // `inp.peek()` now shows the token AFTER `tok` — `next()`
                    // already advanced the cursor past it — giving `step`
                    // its one-token lookahead without consuming anything.
                    let next = inp.peek();
                    if tracker.step(&tok, next.as_ref()) {
                        return Ok((body, span));
                    }
                    body.push((tok, span));
                }
            }
        }
    })
}

/// One open construct on a [`CmdSubstFrames`] stack.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CmdSubstFrame {
    /// Opened by a nested `$(` (`Token::CmdSubstStart`); closed by a `)`.
    Subst,
    /// Opened by a literal `(` (`Token::LParen`) — e.g. a parenthesized
    /// case-branch pattern `(a)`; closed by a `)`. A `case`-pattern's
    /// leading `(` is popped by [`CmdSubstFrames::step`]'s `RParen` arm,
    /// which also clears the `Case` frame directly beneath it out of
    /// `awaiting_pattern` — see that arm's comment.
    Paren,
    /// Opened by `Token::Case`, unless the very next token is `Token::Eq`
    /// (`case=x` is a `key=value` argv key spelled with the keyword, not a
    /// case-statement opener — see [`CmdSubstFrames::step`]'s `Case` arm).
    /// `awaiting_pattern` is `true` right after `case … in` and right after
    /// a `;;` — a branch pattern (or `esac`) is expected next — and `false`
    /// once a pattern's `)` has been consumed, for the rest of that
    /// branch's body. `Esac` closes this frame only while `awaiting_pattern`
    /// is `true`; see [`CmdSubstFrames`].
    Case { awaiting_pattern: bool },
}

/// Tracks, one token at a time, the stack of open `$(`/`(`/`case` frames
/// while scanning a `$(...)` body for the `)` that closes it.
///
/// A flat depth counter can't tell a case-branch pattern's `)` apart from
/// one that really closes a nested `$(...)` or `(...)` once both are open
/// at once — `case $x in a) …` has no leading `(` at all, so its `)` has no
/// matching open on any counter, but a bare "depth > 0 decrements" rule
/// doesn't know that and consumes whatever counter happens to be nonzero.
/// Asking a stack instead — what's actually innermost right now — resolves
/// each `)` against the frame it belongs to:
///
/// - innermost is `Case` — the `)` is a branch pattern terminator; it's
///   part of the body and nothing pops (but `awaiting_pattern` flips to
///   `false` — see below).
/// - innermost is `Paren` — that frame closes; pop it. If a `Case` frame
///   awaiting its pattern is directly beneath, its `)` was also the
///   pattern's own closer (`(a)` is `a)` with an optional leading paren —
///   the same word either way), so clear that `Case`'s `awaiting_pattern`
///   too; otherwise this `Paren` closed something else (a POSIX function's
///   empty `()`, or a case-branch body's own paren once `awaiting_pattern`
///   is already `false`) and the frame beneath is untouched either way.
/// - innermost is `Subst` — that frame closes; pop it.
/// - stack empty — this is the substitution's own closing `)`; stop, and
///   the token is not part of the body.
///
/// `Esac` is ALSO the literal bareword `"esac"` in argument position
/// (`keyword_as_bareword`, same as `done`/`fi`), and that bareword can
/// appear anywhere inside a branch's own body (`case a in a) y=esac;;
/// b) …`) — a real, still-open case whose closer has not been reached yet.
/// Popping the `Case` frame on every `Esac` while it's innermost (rather
/// than only when one was never open) is not enough: it would treat that
/// bareword as the closer too, and then the branch's real `;;`/pattern/
/// `esac` tokens run with no `Case` frame protecting them, corrupting the
/// scan the same way the original flat counter did. `awaiting_pattern`
/// tracks the one thing that actually distinguishes them — position, not
/// spelling: `esac` closes only where a new pattern could otherwise start
/// (right after `case … in` or a `;;`); anywhere else in the body it's
/// just a word.
///
/// Shared by [`cmd_subst_body_tokens`] (the live chumsky capture that
/// actually bounds the body during a real parse) and
/// [`find_cmd_subst_close`] (a plain slice scan used only by
/// `validate_cmd_subst_bodies`'s error-path fallback) so the rule lives in
/// exactly one place.
#[derive(Default)]
struct CmdSubstFrames(Vec<CmdSubstFrame>);

impl CmdSubstFrames {
    /// Feed one token, plus the token right after it (`None` at end of
    /// input) so a bareword `case` can be told apart from a `case=value`
    /// argv key one token early — see the `Case` arm. Returns `true` when
    /// `tok` is the substitution's own closing `)` — the scan must stop, and
    /// `tok` itself is not part of the body. Returns `false` when `tok`
    /// belongs to the body and scanning continues.
    fn step(&mut self, tok: &Token, next: Option<&Token>) -> bool {
        match tok {
            Token::RParen => match self.0.last_mut() {
                None => return true,
                Some(CmdSubstFrame::Case { awaiting_pattern }) => {
                    *awaiting_pattern = false;
                }
                Some(CmdSubstFrame::Paren) => {
                    self.0.pop();
                    // The paren just closed may have been a case-branch
                    // pattern's optional leading `(` (`(a)` == `a)`) — if
                    // so, this same `)` also consumed the pattern.
                    if let Some(CmdSubstFrame::Case { awaiting_pattern }) = self.0.last_mut() {
                        *awaiting_pattern = false;
                    }
                }
                Some(CmdSubstFrame::Subst) => {
                    self.0.pop();
                }
            },
            Token::LParen => self.0.push(CmdSubstFrame::Paren),
            Token::CmdSubstStart => self.0.push(CmdSubstFrame::Subst),
            // `case` opens a case-statement frame UNLESS it's immediately
            // followed by `=` — kaish permits shell keywords as `key=value`
            // argv keys (`in=a`, `do=b`; see `keyword_word`), and `case` is
            // no exception. Pushing a frame for `case=x` would leave it
            // stuck open (nothing but a bareword `esac` or a stray `)`
            // would ever touch it again), corrupting the rest of the scan.
            Token::Case if !matches!(next, Some(Token::Eq)) => {
                self.0.push(CmdSubstFrame::Case { awaiting_pattern: true });
            }
            Token::Case => {}
            Token::DoubleSemi => {
                if let Some(CmdSubstFrame::Case { awaiting_pattern }) = self.0.last_mut() {
                    *awaiting_pattern = true;
                }
            }
            Token::Esac
                if matches!(
                    self.0.last(),
                    Some(CmdSubstFrame::Case { awaiting_pattern: true })
                ) =>
            {
                self.0.pop();
            }
            _ => {}
        }
        false
    }
}

/// Find the index in `tokens` of the `)` that closes a `$(...)` whose body
/// starts at `tokens[0]` (i.e. `tokens` must NOT include the leading
/// `CmdSubstStart`). `None` if `tokens` runs out first (unterminated).
///
/// Plain-slice twin of [`cmd_subst_body_tokens`]'s live chumsky capture, used
/// only by `validate_cmd_subst_bodies`'s error-path fallback — see that
/// function's doc comment for why a second, non-chumsky scan exists at all.
fn find_cmd_subst_close(tokens: &[(Token, Span)]) -> Option<usize> {
    let mut tracker = CmdSubstFrames::default();
    (0..tokens.len()).find(|&i| {
        let next = tokens.get(i + 1).map(|(t, _)| t);
        tracker.step(&tokens[i].0, next)
    })
}

/// Re-validate every unquoted `$(...)` body in `tokens` on its own, outside
/// chumsky's `choice`/`try_map` alternative machinery — called only after
/// [`parse_tokens`]'s main grammar pass has already failed.
///
/// Why this exists: chumsky's `TryMap::go` (see `combinator.rs` in the
/// `chumsky` crate) records a failed alternative's error at the cursor
/// position from *before* the wrapped parser ran, not at the error's own
/// span. `cmd_subst_parser`'s `try_map` wraps a body that can be many tokens
/// long, so a deep, specific error from well inside a malformed `$(...)`
/// body gets attributed to the shallow position right after `$(` for
/// purposes of chumsky's furthest-error bookkeeping — and can lose to a
/// shorter, more generic error from a sibling `choice` alternative that
/// never had a chance of matching. The user then sees "expected expression"
/// pointing at the `$(` itself instead of the real problem inside.
///
/// The fix already used elsewhere in this parser (`bash_substring_hint`,
/// `first_ambiguous_stdin`) is to step outside chumsky's alternative
/// machinery entirely for diagnostics we want full control over. This
/// function does that for `$(...)` bodies: it is not part of the grammar, so
/// nothing merges or discards the error it returns.
fn validate_cmd_subst_bodies(tokens: &[(Token, Span)]) -> Result<(), Vec<ParseError>> {
    let mut i = 0;
    while i < tokens.len() {
        if !matches!(tokens[i].0, Token::CmdSubstStart) {
            i += 1;
            continue;
        }
        let start_span = tokens[i].1;
        let rest = &tokens[i + 1..];
        let Some(close_rel) = find_cmd_subst_close(rest) else {
            return Err(vec![ParseError {
                span: start_span,
                message: "unterminated command substitution: missing `)`".to_string(),
            }]);
        };
        let body = &rest[..close_rel];
        let rparen_span = rest[close_rel].1;
        let end_span: Span = (rparen_span.start..rparen_span.start).into();
        // Recurse before moving on to the remainder of `tokens`, so a nested
        // `$(...)` reports its own (deeper, more specific) error rather than
        // this level's.
        parse_tokens(body.to_vec(), end_span, start_span)?;
        i += 1 + close_rel + 1;
    }
    Ok(())
}

/// Re-validate every double-quoted string's interpolation directly, outside
/// chumsky's `choice`/`try_map` alternative machinery — called only after
/// [`parse_tokens`]'s main grammar pass has already failed, the same way and
/// for the same reason as [`validate_cmd_subst_bodies`] (its doc comment has
/// the mechanism): a `$(...)` inside a `Token::String` fails deep inside
/// `interpolated_string_parser`'s `try_map`, so a `choice` alternative
/// elsewhere in the grammar that never had a chance of matching can still win
/// chumsky's furthest-error bookkeeping and bury the real message (an
/// unterminated or malformed quoted `$(...)` reporting a generic "expected
/// expression" pointing at the string's start, instead of naming the actual
/// problem).
fn validate_interpolated_strings(tokens: &[(Token, Span)]) -> Result<(), Vec<ParseError>> {
    for (tok, span) in tokens {
        let owned;
        let body = match tok {
            Token::String(s) => Some(s.as_str()),
            // `${x:-WORD}`'s default word fails in `var_expr_parser`'s
            // `try_map`, which is inside the same `choice` bookkeeping — so its
            // message needs surfacing here for the same reason a string's does.
            Token::VarRef(raw) => match find_default_separator(raw) {
                Some(colon) if raw.len() > colon + 3 => {
                    owned = unquote_default_word(&raw[colon + 2..raw.len() - 1]);
                    Some(owned.as_str())
                }
                _ => None,
            },
            _ => None,
        };
        if let Some(body) = body
            && let Err(message) = parse_interpolated_string(body)
        {
            return Err(vec![ParseError { span: *span, message }]);
        }
    }
    Ok(())
}

/// A heredoc body's own `$(...)` errors are raised inside
/// `parse_interpolated_string_spanned`, deep in a `try_map`, so chumsky's
/// alternative bookkeeping can bury them behind a generic "found `<<`" — the
/// same loss `validate_cmd_subst_bodies` exists to undo for the unquoted form.
///
/// Uses the heredoc's own parser, never the double-quoted string's: a body may
/// hold a raw `"`, and the string scanner reads `stamp = "$(date +%s)"` as
/// unterminated. A quoted delimiter (`<<'EOF'`) is literal and never expanded,
/// so its body is never inspected.
fn validate_heredoc_bodies(tokens: &[(Token, Span)]) -> Result<(), Vec<ParseError>> {
    for (tok, span) in tokens {
        if let Token::HereDoc(d) = tok
            && !d.literal
            && let Err(message) = parse_interpolated_string_spanned(&d.content, 0)
        {
            return Err(vec![ParseError { span: *span, message }]);
        }
    }
    Ok(())
}

/// True for a lexer token that becomes exactly one glue-candidate `Arg`
/// (`Positional` or a bare `LongFlag`) on its own.
///
/// Exhaustive over `Token` with no wildcard, so a new variant is a compile
/// error here rather than a silently un-improved span. That is completeness
/// against the ENUM, not the GRAMMAR: a new production built from a token
/// already listed `false` compiles fine and this scanner keeps missing it.
fn is_word_token(tok: &Token) -> bool {
    match tok {
        Token::True | Token::False | Token::EqEq | Token::NotEq | Token::Eq
        | Token::Colon | Token::Comma | Token::DotDot | Token::Dot | Token::TildePath(_)
        | Token::Tilde | Token::RelativePath(_) | Token::DotSlashPath(_)
        | Token::DottedIdent(_) | Token::Star | Token::Bang | Token::Question
        | Token::GlobWord(_) | Token::Arithmetic(_) | Token::LongFlag(_)
        | Token::DoubleDashBare(_) | Token::PlusBare(_) | Token::MinusBare(_)
        | Token::JobSpec(_) | Token::MinusAlone | Token::String(_)
        | Token::SingleString(_) | Token::VarRef(_) | Token::SimpleVarRef(_)
        | Token::Positional(_) | Token::AllArgs | Token::ArgCount | Token::LastExitCode
        | Token::CurrentPid | Token::VarLength(_) | Token::Int(_) | Token::Float(_)
        | Token::NumericLiteral(_)
        | Token::NumberIdent(_) | Token::DashNumWord(_) | Token::AtWord(_)
        | Token::Path(_) | Token::Ident(_) => true,

        // Known gap. These nine are keywords `keyword_as_bareword` accepts in
        // argument position, so `git checkout do:x` is a real paste this
        // scanner misses; it falls through to the grammar's own span. Flipping
        // them to `true` closes it, but changes error text outside this
        // change's scope. (`keyword_word`'s wider set is assignment KEYS — see
        // `is_assign_key_token`.)
        Token::Done | Token::Fi | Token::Then | Token::Else | Token::Elif | Token::In
        | Token::Do | Token::Esac | Token::Set => false,

        // Keywords that are only ever keywords in argument position.
        Token::Local | Token::If | Token::For | Token::While | Token::Case
        | Token::Function | Token::Break | Token::Continue | Token::Return | Token::Exit => false,

        // Type names, only meaningful in a declaration.
        Token::TypeString | Token::TypeInt | Token::TypeFloat | Token::TypeBool => false,

        // Operators: never a word, and each already breaks a run.
        Token::And | Token::Or | Token::Match | Token::NotMatch | Token::GtEq
        | Token::LtEq => false,

        // Redirect operators. A redirect's target is diagnosed by
        // `redirect_parser` with its own wording, and `is_glued_args_error`
        // keeps this scanner from ever running for those inputs.
        Token::Gt | Token::GtGt | Token::Lt | Token::Stderr | Token::Both
        | Token::HereString | Token::HereDocStart | Token::StderrToStdout
        | Token::StdoutToStderr | Token::StdoutToStderr2 => false,

        // Statement and collection structure.
        Token::Pipe | Token::Amp | Token::Semi | Token::DoubleSemi | Token::DotDotDot
        | Token::LBrace | Token::RBrace | Token::LBracket | Token::RBracket
        | Token::LParen | Token::RParen => false,

        // `$(` opens a balanced group, not a single-token word — `word_unit`
        // handles it by scanning to the matching `)`.
        Token::CmdSubstStart => false,

        // Excluded to match `is_glue_candidate`'s own exclusion of
        // `Arg::ShortFlag`/`Arg::DoubleDash` — see that function.
        Token::ShortFlag(_) | Token::PlusFlag(_) | Token::DoubleDash => false,

        // Heredoc bodies, lexer errors, trivia: never argv words.
        Token::HereDoc(_) | Token::InvalidFloatNoLeading | Token::InvalidFloatNoTrailing
        | Token::Comment | Token::Newline | Token::LineContinuation
        | Token::BacktickRejected => false,
    }
}

/// A key token `word_assign_arg_parser`/`long_flag_with_value` accept —
/// `Ident` for `key=value`, `LongFlag` for `--key=value`.
///
/// The `LongFlag` half fuses `--key=value` into ONE unit; without it a spaced
/// `--a=1 --b=2` would scan as adjacent fragments and be flagged as a paste it
/// is not. `keyword_word`'s wider key set is deliberately not reproduced: this
/// scanner re-derives common shapes, not the whole grammar.
fn is_assign_key_token(tok: &Token) -> bool {
    matches!(tok, Token::Ident(_) | Token::LongFlag(_))
}

/// If `tokens[i]` starts a single glue-candidate word — one `is_word_token`
/// token, or a balanced `$(...)` group — returns its span and the index
/// just past it. `None` when `tokens[i]` cannot start a word (an operator,
/// a `ShortFlag`, a keyword used as a keyword, an unterminated `$(...)`).
fn word_unit(tokens: &[(Token, Span)], i: usize) -> Option<(Span, usize)> {
    let (tok, span) = tokens.get(i)?;
    if matches!(tok, Token::CmdSubstStart) {
        let close_rel = find_cmd_subst_close(&tokens[i + 1..])?;
        let close_idx = i + 1 + close_rel;
        return Some(((span.start..tokens[close_idx].1.end).into(), close_idx + 1));
    }
    if is_word_token(tok) {
        return Some((*span, i + 1));
    }
    None
}

/// Walk `tokens` producing the glue-candidate units
/// `arg_before_double_dash_parser` would bind as `Arg`s: a word token, a
/// `key=value`/`--key=value` assignment, or a balanced `$(...)`. Anything
/// else breaks the chain and never merges across it.
///
/// Redirect targets need no handling here — `redirect_parser` diagnoses a
/// glued target in its own words, so `is_glued_args_error` keeps this
/// scanner from running for them at all.
fn glue_candidate_units(tokens: &[(Token, Span)]) -> Vec<Span> {
    let mut units = Vec::new();
    let mut i = 0;
    while i < tokens.len() {
        let (tok, span) = &tokens[i];

        if is_assign_key_token(tok)
            && let Some((Token::Eq, eq_span)) = tokens.get(i + 1)
            && eq_span.start == span.end
            && let Some((value_span, next_i)) = word_unit(tokens, i + 2)
            && eq_span.end == value_span.start
        {
            units.push((span.start..value_span.end).into());
            i = next_i;
            continue;
        }

        if let Some((unit_span, next_i)) = word_unit(tokens, i) {
            units.push(unit_span);
            i = next_i;
            continue;
        }

        i += 1;
    }
    units
}

/// Re-detect a glued-argv paste directly from `tokens`, after the main
/// grammar pass has already failed — the same shape as
/// [`validate_cmd_subst_bodies`], for the same reason.
///
/// `reject_glued_args` raises this rejection inside a `try_map` wrapping the
/// whole argv, and its span never survives: `flat_merge` keeps our message
/// while `Rich::merge` keeps a shallower sibling's span, so `git show
/// HEAD:x.py` reports at `show`.
///
/// Reached only once `is_glued_args_error` confirms the standing error is
/// this exact rejection, so it need not know what to avoid. Under-recognizing
/// a shape just leaves the grammar's span in place. The scope is "report the
/// right span for a rejection that already happened", never "change what is
/// rejected".
/// `break`/`continue` take a loop count, and a count is a number. A leading
/// zero makes the word text ([`lexer::is_leading_zero_numeral`]), so `break
/// 007` no longer matches the count grammar and chumsky reports the miss
/// against every statement alternative — a long message that never mentions
/// the zero.
///
/// Runs only after the grammar has already failed, and only for a leading-zero
/// numeral standing directly after `break`/`continue`, so it replaces a
/// confusing message for a statement kaish rejects either way. It never makes
/// a passing program fail.
fn validate_leading_zero_counts(tokens: &[(Token, Span)]) -> Result<(), Vec<ParseError>> {
    for pair in tokens.windows(2) {
        let keyword = match &pair[0].0 {
            Token::Break => "break",
            Token::Continue => "continue",
            _ => continue,
        };
        let Token::NumberIdent(word) = &pair[1].0 else {
            continue;
        };
        if !lexer::is_leading_zero_numeral(word) {
            continue;
        }
        let count = word.trim_start_matches('-').trim_start_matches('0');
        let count = if count.is_empty() { "1" } else { count };
        return Err(vec![ParseError {
            span: pair[1].1,
            message: format!(
                "`{keyword}` takes a loop count and `{word}` is text (leading zero) — write \
                 `{keyword} {count}`"
            ),
        }]);
    }
    Ok(())
}

fn validate_glued_args(
    tokens: &[(Token, Span)],
    from_offset: usize,
) -> Result<(), Vec<ParseError>> {
    let units = glue_candidate_units(tokens);
    for i in 0..units.len().saturating_sub(1) {
        if units[i].end != units[i + 1].start {
            continue;
        }
        let mut start_idx = i;
        while start_idx > 0 && units[start_idx - 1].end == units[start_idx].start {
            start_idx -= 1;
        }
        let mut end_idx = i + 1;
        while end_idx + 1 < units.len() && units[end_idx].end == units[end_idx + 1].start {
            end_idx += 1;
        }
        // The scan walks the whole token stream, so it also finds adjacency
        // in regions the grammar parsed happily — `for x in $a/b; do echo
        // /tmp/$(echo x).txt; done` has a legal `$a/b` in the loop head and
        // the real paste in the body. Take the first run at or after the
        // grammar's own position so the earlier legal run cannot win.
        if units[start_idx].start < from_offset {
            continue;
        }
        let span: Span = (units[start_idx].start..units[end_idx].end).into();
        return Err(vec![ParseError {
            span,
            message: GLUED_ARGS_MESSAGE.to_string(),
        }]);
    }
    // No run at or after the grammar's position: say nothing and let its own
    // span stand. Never worse than the message shipped in 0.16.0.
    Ok(())
}

/// Command substitution: `$(...)` - runs a statement sequence and returns its
/// result.
///
/// Route C (GH #194): the body is a token slice, balance-captured by
/// [`cmd_subst_body_tokens`], then parsed with the FULL program grammar
/// (`parse_tokens`, the same entry point [`parse`] uses) from inside this
/// `.try_map()` closure — at *parse* time, not build time. That is what lets
/// `if`/`for`/`while`/`case` appear inside an unquoted `$(...)`: closing the
/// cycle with a second `recursive()` call (`cmd_subst → primary_expr →
/// cmd_subst`) overflows the stack while the parser graph is being
/// CONSTRUCTED (see `CACHED_PARSER`'s doc comment), but a call made once that
/// graph already exists — from inside a closure that only runs when this
/// combinator actually matches a token — has nothing left to recurse through
/// at build time.
///
/// Before this, the body had its own hand-rolled pipeline/`&&`/`||` grammar
/// with control structures intentionally out of scope — a second, smaller
/// copy of `pipeline_parser`/`command_parser` kept in sync by hand. Route C
/// deleted that copy: a pipeline inside `$(...)` now goes through the same
/// `pipeline_parser` as everywhere else, and this function no longer needs
/// the caller's recursive `expr` handle at all (the redirect-target cycle
/// `redirect_parser` used to document is gone with it).
fn cmd_subst_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    just(Token::CmdSubstStart)
        .ignore_then(cmd_subst_body_tokens())
        .try_map(|(body_tokens, rparen_span), outer_span| {
            let end_span: Span = (rparen_span.start..rparen_span.start).into();
            parse_tokens(body_tokens, end_span, outer_span)
                .map(|program| Expr::CommandSubst(program.statements))
                .map_err(|errs| {
                    let first = errs.into_iter().next().unwrap_or_else(|| ParseError {
                        span: outer_span,
                        message: "command substitution failed to parse".to_string(),
                    });
                    Rich::custom(first.span, first.message)
                })
        })
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

/// A numeral whose source text does not round-trip through its own typed
/// `Display` — a negative zero, a leading zero, or a non-canonical trailing
/// fraction digit. See `lexer::Token::NumericLiteral`. Kept separate from
/// `literal_parser` because it produces `Expr::NumericLiteral` directly
/// (carrying `raw` alongside `value`), not a bare `Value` for `Expr::Literal`
/// to wrap.
fn numeric_literal_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    select! {
        Token::NumericLiteral(d) => Expr::NumericLiteral { value: d.value, raw: d.raw },
    }
    .labelled("literal")
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
    use proptest::strategy::Strategy;

    /// The commands of a command-only pipeline. Panics on a compound stage —
    /// every assertion below is about a pipeline of plain commands, and a
    /// compound appearing in one would be the bug, not a case to skip.
    fn pipeline_commands(p: &Pipeline) -> Vec<&Command> {
        p.stages
            .iter()
            .map(|stage| stage.as_command().expect("expected a command stage"))
            .collect()
    }

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
            Stmt::Pipeline(p) => assert_eq!(pipeline_commands(p).len(), 3),
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
                assert_eq!(pipeline_commands(p).len(), 1);
                let cmd = pipeline_commands(p)[0];
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
            Stmt::Pipeline(p) => assert_eq!(pipeline_commands(p).len(), 3),
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
                assert_eq!(pipeline_commands(pipeline).len(), 2);
                assert_eq!(pipeline_commands(pipeline)[0].name, "cat");
                assert_eq!(pipeline_commands(pipeline)[1].name, "grep");
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
                assert_eq!(pipeline_commands(pipeline).len(), 1);
                let cmd = pipeline_commands(pipeline)[0];
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
                assert_eq!(pipeline_commands(pipeline).len(), 1);
                let cmd = pipeline_commands(pipeline)[0];
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
                        assert_eq!(pipeline_commands(p).len(), 1);
                        assert_eq!(pipeline_commands(p)[0].name, "echo");
                        assert_eq!(pipeline_commands(p)[0].redirects.len(), 1);
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
    // GH #194: control structures inside an UNQUOTED `$(...)` (route C)
    //
    // Before this, `x="$(for f in a b; do echo $f; done)"` (quoted) worked
    // because `parse_interpolated_string` recursively calls the top-level
    // `parse()`, but `echo $(for f in a b; do echo $f; done)` (unquoted) was
    // a parse error: `cmd_subst_parser` had its own hand-rolled
    // pipeline/`&&`/`||` grammar with control structures intentionally out
    // of scope. Route C replaced that with a balance-captured token slice
    // parsed through the full program grammar from inside a `.try_map()`
    // closure at parse time — see `cmd_subst_parser`'s doc comment for why
    // it has to be parse time, not build time.
    // ═══════════════════════════════════════════════════════════════════════════

    #[test]
    fn parse_cmd_subst_unquoted_for_loop() {
        let result = parse("X=$(for f in a b; do echo $f; done)").unwrap();
        let stmts = match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::CommandSubst(s) => s,
                other => panic!("expected command subst, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        };
        match stmts.as_slice() {
            [Stmt::For(f)] => {
                assert_eq!(f.variable, "f");
                assert_eq!(f.items.len(), 2);
                assert!(matches!(f.body.as_slice(), [Stmt::Command(c)] if c.name == "echo"));
            }
            other => panic!("expected a single For statement, got {:?}", other),
        }
    }

    #[test]
    fn parse_cmd_subst_unquoted_while_loop() {
        let result = parse("X=$(while false; do echo x; done)").unwrap();
        let stmts = match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::CommandSubst(s) => s,
                other => panic!("expected command subst, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        };
        assert!(
            matches!(stmts.as_slice(), [Stmt::While(w)] if matches!(w.body.as_slice(), [Stmt::Command(c)] if c.name == "echo")),
            "expected a single While statement, got {stmts:?}"
        );
    }

    #[test]
    fn parse_cmd_subst_unquoted_if_else() {
        let result = parse("X=$(if true; then echo one; else echo two; fi)").unwrap();
        let stmts = match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::CommandSubst(s) => s,
                other => panic!("expected command subst, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        };
        match stmts.as_slice() {
            [Stmt::If(i)] => {
                assert!(i.else_branch.is_some(), "expected an else branch");
                assert!(matches!(i.then_branch.as_slice(), [Stmt::Command(c)] if c.name == "echo"));
            }
            other => panic!("expected a single If statement, got {:?}", other),
        }
    }

    #[test]
    fn parse_cmd_subst_unquoted_case() {
        // An unpaired case-branch pattern (`a)`, no leading `(`) is the
        // sharpest case for the balance tracker: its `)` has no matching
        // open on the depth counter, so it must not be read as the
        // substitution's own close (see `CmdSubstFrames`).
        let result = parse("X=$(case a in a) echo hit;; esac)").unwrap();
        let stmts = match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::CommandSubst(s) => s,
                other => panic!("expected command subst, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        };
        match stmts.as_slice() {
            [Stmt::Case(c)] => {
                assert_eq!(c.branches.len(), 1);
                assert_eq!(c.branches[0].patterns, vec!["a".to_string()]);
            }
            other => panic!("expected a single Case statement, got {:?}", other),
        }
    }

    #[test]
    fn parse_cmd_subst_unquoted_case_with_parenthesized_pattern() {
        // The *paired* form (`(a)`) — a `Paren` frame handles this one on
        // its own, since the `(` pushed it.
        let result = parse("X=$(case a in (a) echo hit;; esac)").unwrap();
        let stmts = match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::CommandSubst(s) => s,
                other => panic!("expected command subst, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        };
        assert!(
            matches!(stmts.as_slice(), [Stmt::Case(c)] if c.branches.len() == 1),
            "expected a single Case statement, got {stmts:?}"
        );
    }

    #[test]
    fn parse_cmd_subst_unquoted_case_parenthesized_pattern_with_bareword_esac_in_body() {
        // The parenthesized twin of
        // `parse_cmd_subst_unquoted_esac_as_bareword_inside_still_open_case`:
        // the FIRST branch's pattern is `(a)` instead of bare `a)`. Popping
        // the `Paren` frame the leading `(` pushed used to leave the `Case`
        // frame beneath stuck at `awaiting_pattern: true` — the contract
        // `CmdSubstFrame::Case`'s own docstring states ("false once a
        // pattern's `)` has been consumed") went unmet for this spelling —
        // so the bareword `esac` in `y=esac` (branch `a)`'s whole body) read
        // as the case's own closer, popping the frame early and corrupting
        // everything the tracker reads after it.
        let result = parse("X=$(case a in (a) y=esac;; b) echo two;; esac)").unwrap();
        let stmts = match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::CommandSubst(s) => s,
                other => panic!("expected command subst, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        };
        match stmts.as_slice() {
            [Stmt::Case(c)] => {
                assert_eq!(c.branches.len(), 2);
                assert_eq!(c.branches[0].patterns, vec!["a".to_string()]);
                assert_eq!(c.branches[1].patterns, vec!["b".to_string()]);
            }
            other => panic!("expected a single Case statement with two branches, got {:?}", other),
        }
    }

    #[test]
    fn parse_cmd_subst_unquoted_nested_case_parenthesized_pattern_esac_in_outer_body() {
        // Nesting: a `case` inside a `case` branch's body, both inside
        // `$(...)`, both patterns parenthesized. The inner case resolves
        // and closes cleanly (its own `;;` sets `awaiting_pattern` back to
        // `true` before its `esac`, independent of the bug), which masked
        // this defect in isolation — the outer `Case` frame's stuck
        // `awaiting_pattern: true` only surfaces once the inner case's
        // frame is popped and the outer frame is innermost again: the
        // bareword `esac` in the outer branch's own `y=esac`, reached AFTER
        // the inner case fully closes, must not read as the outer case's
        // closer either.
        let result = parse(
            "X=$(case a in (a) case b in (b) echo z;; esac; y=esac;; c) echo two;; esac)",
        )
        .unwrap();
        let stmts = match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::CommandSubst(s) => s,
                other => panic!("expected command subst, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        };
        match stmts.as_slice() {
            [Stmt::Case(c)] => {
                assert_eq!(c.branches.len(), 2);
                assert_eq!(c.branches[0].patterns, vec!["a".to_string()]);
                assert_eq!(c.branches[1].patterns, vec!["c".to_string()]);
            }
            other => panic!("expected a single Case statement with two branches, got {:?}", other),
        }
    }

    #[test]
    fn parse_cmd_subst_unquoted_case_eq_argv_key() {
        // `case` is a valid `key=value` argv key (same as `in=a`/`do=b` —
        // see `keyword_key_argv_assignment_parses` in parser_tests.rs), but
        // it's the only one of those keywords that pushes a structural
        // frame on the `$(...)` balance tracker. Pushing one unconditionally
        // reads `case=x` as a case-statement opener, then reads the
        // substitution's real closing `)` as a pattern terminator instead —
        // the tracker never sees a `)` to stop on and reports "unterminated"
        // even though the input is well-formed.
        let result = parse("X=$(echo case=x)").unwrap();
        let stmts = match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::CommandSubst(s) => s,
                other => panic!("expected command subst, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        };
        let cmd = match stmts.as_slice() {
            [Stmt::Command(c)] => c,
            other => panic!("expected a single echo command, got {:?}", other),
        };
        assert_eq!(cmd.name, "echo");
        match &cmd.args[0] {
            Arg::WordAssign { key, value } => {
                assert_eq!(key, "case");
                match value {
                    Expr::Literal(Value::String(s)) => assert_eq!(s, "x"),
                    other => panic!("expected string \"x\", got {:?}", other),
                }
            }
            other => panic!("expected WordAssign arg, got {:?}", other),
        }
    }

    #[test]
    fn parse_cmd_subst_unquoted_case_eq_argv_key_with_sibling_keyword_keys() {
        // `case=x` alongside the already-covered sibling keyword keys
        // (`do=y`), inside `$(...)` — the balance tracker must treat all of
        // them uniformly.
        let result = parse("X=$(tool case=x do=y)").unwrap();
        let stmts = match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::CommandSubst(s) => s,
                other => panic!("expected command subst, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        };
        let cmd = match stmts.as_slice() {
            [Stmt::Command(c)] => c,
            other => panic!("expected a single tool command, got {:?}", other),
        };
        assert_eq!(cmd.name, "tool");
        assert_eq!(cmd.args.len(), 2);
        assert!(matches!(&cmd.args[0], Arg::WordAssign { key, .. } if key == "case"));
        assert!(matches!(&cmd.args[1], Arg::WordAssign { key, .. } if key == "do"));
    }

    #[test]
    fn parse_cmd_subst_unquoted_case_inside_nested_subst() {
        // A flat depth counter conflates a case-branch pattern's unpaired
        // `)` with a nested `$(...)`'s own close once both are open at
        // once: `depth > 0` fires before the case check ever runs, so the
        // pattern terminator wrongly closes the inner substitution instead
        // of being consumed as body text (see `CmdSubstFrames`). The stack
        // asks each `)` about the frame it actually belongs to instead.
        let result = parse("X=$(echo $(case b in b) echo x;; esac))").unwrap();
        let outer_stmts = match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::CommandSubst(s) => s,
                other => panic!("expected command subst, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        };
        let outer_cmd = match outer_stmts.as_slice() {
            [Stmt::Command(c)] => c,
            other => panic!("expected a single echo command, got {:?}", other),
        };
        assert_eq!(outer_cmd.name, "echo");
        let inner_stmts = match &outer_cmd.args[0] {
            Arg::Positional(Expr::CommandSubst(s)) => s,
            other => panic!("expected nested command subst arg, got {:?}", other),
        };
        assert!(
            matches!(inner_stmts.as_slice(), [Stmt::Case(c)] if c.branches.len() == 1),
            "expected a single Case statement inside the inner $(), got {inner_stmts:?}"
        );
    }

    #[test]
    fn parse_cmd_subst_unquoted_esac_as_bareword() {
        // `Esac` is also the literal bareword "esac" in argument position
        // (`keyword_as_bareword`, same as `done`/`fi`). No case is open at
        // all here, so the tracker must not touch a `Case` frame it never
        // pushed.
        let result = parse("X=$(echo esac)").unwrap();
        let stmts = match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::CommandSubst(s) => s,
                other => panic!("expected command subst, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        };
        let cmd = match stmts.as_slice() {
            [Stmt::Command(c)] => c,
            other => panic!("expected a single echo command, got {:?}", other),
        };
        assert_eq!(cmd.name, "echo");
        assert!(
            matches!(&cmd.args[0], Arg::Positional(Expr::Literal(Value::String(s))) if s == "esac"),
            "expected \"esac\" as a literal argument, got {:?}",
            cmd.args[0]
        );
    }

    #[test]
    fn parse_cmd_subst_unquoted_esac_as_bareword_inside_still_open_case() {
        // The sharper form of the previous test: `esac` as a bareword
        // *inside a case that is genuinely still open* (its own closer
        // hasn't been reached yet) — `y=esac` is the first branch's whole
        // body. Popping the `Case` frame whenever it's merely innermost
        // (rather than only while `awaiting_pattern`) treats this bareword
        // as the closer too, and the branch's real `;;`/pattern/`esac`
        // tokens then run with no `Case` frame protecting them — the same
        // failure mode a flat counter has, just one level more specific.
        let result = parse("X=$(case a in a) y=esac;; b) echo two;; esac)").unwrap();
        let stmts = match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::CommandSubst(s) => s,
                other => panic!("expected command subst, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        };
        match stmts.as_slice() {
            [Stmt::Case(c)] => {
                assert_eq!(c.branches.len(), 2);
                assert_eq!(c.branches[0].patterns, vec!["a".to_string()]);
                assert_eq!(c.branches[1].patterns, vec!["b".to_string()]);
            }
            other => panic!("expected a single Case statement with two branches, got {:?}", other),
        }
    }

    #[test]
    fn parse_quoted_cmd_subst_case_pattern_paren_not_miscounted() {
        // `parse_interpolated_string`'s own `$(...)` scan is a THIRD site
        // with the same bug class as `CmdSubstFrames`, but at the character
        // level: it used to count raw `(`/`)` chars, so a case-branch
        // pattern's unpaired `)` truncated the substitution's captured
        // content at "case v in v" and the malformed remainder failed to
        // parse. It now tokenizes the remainder and reuses
        // `find_cmd_subst_close` — the same rule `CmdSubstFrames` uses —
        // instead of a second, independent counter.
        let result = parse(r#"X="pre $(case v in v) echo x;; esac) post""#).unwrap();
        let parts = match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::Interpolated(parts) => parts,
                other => panic!("expected an interpolated string, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        };
        let stmts = match parts.as_slice() {
            [StringPart::Literal(pre), StringPart::CommandSubst(stmts), StringPart::Literal(post)] =>
            {
                assert_eq!(pre, "pre ");
                assert_eq!(post, " post");
                stmts
            }
            other => panic!("expected [literal, command subst, literal], got {:?}", other),
        };
        assert!(
            matches!(stmts.as_slice(), [Stmt::Case(c)] if c.branches.len() == 1),
            "expected a single Case statement inside the quoted $(...), got {stmts:?}"
        );
    }

    #[test]
    fn parse_quoted_cmd_subst_case_parenthesized_pattern_esac_in_body_not_miscounted() {
        // `find_cmd_subst_close` (shared with `CmdSubstFrames::step`) drives
        // `parse_interpolated_string`'s own `$(...)` scan too — the
        // parenthesized-pattern defect must not resurface there either.
        let result = parse(r#"X="pre $(case a in (a) y=esac;; b) echo two;; esac) post""#).unwrap();
        let parts = match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::Interpolated(parts) => parts,
                other => panic!("expected an interpolated string, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        };
        let stmts = match parts.as_slice() {
            [StringPart::Literal(pre), StringPart::CommandSubst(stmts), StringPart::Literal(post)] =>
            {
                assert_eq!(pre, "pre ");
                assert_eq!(post, " post");
                stmts
            }
            other => panic!("expected [literal, command subst, literal], got {:?}", other),
        };
        match stmts.as_slice() {
            [Stmt::Case(c)] => {
                assert_eq!(c.branches.len(), 2);
                assert_eq!(c.branches[0].patterns, vec!["a".to_string()]);
                assert_eq!(c.branches[1].patterns, vec!["b".to_string()]);
            }
            other => panic!("expected a single Case statement with two branches, got {:?}", other),
        }
    }

    #[test]
    fn parse_quoted_cmd_subst_case_eq_argv_key_not_miscounted() {
        // Same shared-scan concern for the `case=x` defect: the quoted
        // `$(...)` form must accept it too.
        let result = parse(r#"X="pre $(echo case=x) post""#).unwrap();
        let parts = match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::Interpolated(parts) => parts,
                other => panic!("expected an interpolated string, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        };
        let stmts = match parts.as_slice() {
            [StringPart::Literal(pre), StringPart::CommandSubst(stmts), StringPart::Literal(post)] =>
            {
                assert_eq!(pre, "pre ");
                assert_eq!(post, " post");
                stmts
            }
            other => panic!("expected [literal, command subst, literal], got {:?}", other),
        };
        let cmd = match stmts.as_slice() {
            [Stmt::Command(c)] => c,
            other => panic!("expected a single echo command, got {:?}", other),
        };
        assert_eq!(cmd.name, "echo");
        assert!(matches!(&cmd.args[0], Arg::WordAssign { key, .. } if key == "case"));
    }

    #[test]
    fn parse_cmd_subst_unquoted_nested_with_control_structure() {
        // Nesting question #2 from the route-C verification list: a control
        // structure inside the INNER `$(...)`, reached through an outer one.
        let result = parse("X=$(echo $(for f in a; do echo $f; done))").unwrap();
        let outer_stmts = match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::CommandSubst(s) => s,
                other => panic!("expected command subst, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        };
        let outer_cmd = match outer_stmts.as_slice() {
            [Stmt::Command(c)] => c,
            other => panic!("expected a single echo command, got {:?}", other),
        };
        assert_eq!(outer_cmd.name, "echo");
        let inner_stmts = match &outer_cmd.args[0] {
            Arg::Positional(Expr::CommandSubst(s)) => s,
            other => panic!("expected nested command subst arg, got {:?}", other),
        };
        assert!(
            matches!(inner_stmts.as_slice(), [Stmt::For(f)] if f.variable == "f"),
            "expected a single For statement inside the inner $(), got {inner_stmts:?}"
        );
    }

    #[test]
    fn parse_cmd_subst_unquoted_pipeline() {
        // Verification question from GH #194's route-C plan: a pipeline
        // (the ordinary, non-compound kind) inside `$(...)` still works once
        // the body goes through the real `pipeline_parser` instead of its
        // own hand-rolled copy.
        let result = parse("X=$(cat f | grep pat | wc -l)").unwrap();
        let value = match &result.statements[0] {
            Stmt::Assignment(a) => a.value.clone(),
            other => panic!("expected assignment, got {:?}", other),
        };
        let pipeline = subst_pipeline(&value);
        assert_eq!(pipeline_commands(pipeline).len(), 3);
        assert_eq!(pipeline_commands(pipeline)[2].name, "wc");
    }

    #[test]
    fn parse_quoted_cmd_subst_with_for_loop_still_works() {
        // The quoted form was never broken (it goes through
        // `parse_interpolated_string`'s own recursive `parse()` call, not
        // `cmd_subst_parser`) — pinned so route C cannot regress it.
        let result = parse(r#"out="$(for f in a b; do echo $f; done)""#).unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => assert_eq!(a.name(), "out"),
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn parse_cmd_subst_body_error_reports_span_inside_body_not_at_dollar_paren() {
        // Route C's sharpest failure mode: a `try_map` rejection deep inside
        // a `$(...)` body can lose its span to chumsky's choice/alt
        // bookkeeping and surface as a generic error at the `$(` itself
        // (`validate_cmd_subst_bodies`'s doc comment has the mechanism).
        // `done` here is swallowed as a second positional arg to `echo`
        // (`keyword_as_bareword` accepts it as a bareword), so the `for`
        // loop's own `done` never arrives and the body runs out of tokens.
        let source = "echo $(for f in a; do echo $f done)";
        let errs = parse(source).expect_err("missing loop terminator must be a parse error");
        let dollar_paren = source.find("$(").expect("fixture contains $(");
        assert!(
            errs.iter().all(|e| e.span.start > dollar_paren + 1),
            "error span must point inside the $() body, not at '$(' itself: {errs:?}"
        );
        // The message names what's actually missing, not a generic
        // "expected expression" from an unrelated sibling `choice` arm.
        assert!(
            errs.iter().any(|e| e.message.contains("done")),
            "expected the missing-`done` diagnostic, got: {errs:?}"
        );
    }

    #[test]
    fn parse_cmd_subst_unterminated_reports_error() {
        let result = parse("echo $(for f in a; do echo $f; done");
        assert!(result.is_err(), "a missing `)` must be a parse error");
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
                    Stmt::Pipeline(p) => assert_eq!(pipeline_commands(p).len(), 2),
                    other => panic!("expected pipeline body, got {other:?}"),
                }
            }
            other => panic!("expected env-scoped, got {other:?}"),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Argv-splat rejection (adjacent unquoted words)
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
                assert_eq!(pipeline_commands(p)[0].name, "long-running-task");
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
                assert_eq!(pipeline_commands(subst_pipeline(&a.value)).len(), 3);
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
                assert_eq!(pipeline_commands(p).len(), 5);
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
        let parts = parse_interpolated_string_spanned("hello world", 100).unwrap();
        assert_eq!(parts.len(), 1);
        assert!(matches!(&parts[0].part, StringPart::Literal(s) if s == "hello world"));
        assert_eq!(parts[0].offset, 100, "base_offset must propagate to literals");
        assert_eq!(parts[0].len, 11);
    }

    #[test]
    fn spanned_braced_var_at_zero() {
        let parts = parse_interpolated_string_spanned("${X}", 50).unwrap();
        assert_eq!(parts.len(), 1);
        assert!(matches!(&parts[0].part, StringPart::Var(_)));
        assert_eq!(parts[0].offset, 50);
        assert_eq!(parts[0].len, 4); // "${X}"
    }

    #[test]
    fn spanned_simple_var_then_literal() {
        let parts = parse_interpolated_string_spanned("$X end", 10).unwrap();
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
        let parts = parse_interpolated_string_spanned("hi ${X} bye", 0).unwrap();
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
        let parts = parse_interpolated_string_spanned("$1 done", 0).unwrap();
        assert_eq!(parts.len(), 2);
        assert!(matches!(&parts[0].part, StringPart::Positional(1)));
        assert_eq!(parts[0].offset, 0);
        assert_eq!(parts[0].len, 2); // "$1"
    }

    #[test]
    fn spanned_special_dollar_dollar() {
        let parts = parse_interpolated_string_spanned("$$", 5).unwrap();
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
        let parts = parse_interpolated_string_spanned("${__ARITH:1+2__}", 0).unwrap();
        assert_eq!(parts.len(), 1);
        assert!(matches!(&parts[0].part, StringPart::Arithmetic(e) if e == "1+2"));
    }

    #[test]
    fn spanned_default_separator_yields_var_with_default() {
        let parts = parse_interpolated_string_spanned("${X:-fallback}", 0).unwrap();
        assert_eq!(parts.len(), 1);
        assert!(matches!(&parts[0].part, StringPart::VarWithDefault { .. }));
        assert_eq!(parts[0].offset, 0);
        assert_eq!(parts[0].len, 14); // "${X:-fallback}"
    }

    #[test]
    fn spanned_no_dollar_runs_one_literal() {
        let parts = parse_interpolated_string_spanned("plain text only", 7).unwrap();
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
            let spanned = parse_interpolated_string_spanned(s, 0).unwrap();
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
        let parts = parse_interpolated_string_spanned("🚀 ${X}", 0).unwrap();
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
        let parts = parse_interpolated_string_spanned("hello 世界 world", 0).unwrap();
        assert_eq!(parts.len(), 1);
        assert!(matches!(&parts[0].part, StringPart::Literal(s) if s == "hello 世界 world"));
        assert_eq!(parts[0].offset, 0);
        assert_eq!(parts[0].len, 18);
    }

    #[test]
    fn spanned_escape_dollar_consumes_two_bytes_emits_one_char() {
        // `\$` is 2 source bytes and resolves to a single literal `$`.
        // The literal part's `len` should reflect the SOURCE length (2).
        let parts = parse_interpolated_string_spanned("\\$", 0).unwrap();
        assert_eq!(parts.len(), 1);
        assert!(matches!(&parts[0].part, StringPart::Literal(s) if s == "$"));
        assert_eq!(parts[0].offset, 0);
        assert_eq!(parts[0].len, 2, "len is source byte length, not rendered length");
    }

    #[test]
    fn spanned_escape_backslash_collapses_pair_to_one() {
        let parts = parse_interpolated_string_spanned("\\\\", 0).unwrap();
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
        let parts = parse_interpolated_string_spanned("\\\rCD${x}", 0).unwrap();
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
        let parts = parse_interpolated_string_spanned("AB\\\rCD${x}", 0).unwrap();
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

    /// One layer a [`nested_compound_constructs_always_parse`] source can be
    /// wrapped in. Each variant takes the previous layer's source (always a
    /// complete, valid statement) and produces a new one, so folding a
    /// random sequence of these builds an arbitrarily nested — but always
    /// structurally valid — program.
    #[derive(Debug, Clone, Copy)]
    enum NestingLayer {
        /// Unquoted `$(...)`, Route C's own grammar (`cmd_subst_parser`).
        CmdSubst,
        /// Quoted `"$(...)"`, the separate `parse_interpolated_string` path.
        QuotedCmdSubst,
        /// `case ... in v) ...;; esac`, an unpaired pattern-terminator `)`.
        Case,
        If,
        For,
    }

    fn wrap_in_layer(inner: &str, layer: NestingLayer) -> String {
        match layer {
            NestingLayer::CmdSubst => format!("x=$({inner})"),
            NestingLayer::QuotedCmdSubst => format!("x=\"pre $({inner}) post\""),
            NestingLayer::Case => format!("case v in v) {inner};; esac"),
            NestingLayer::If => format!("if true; then {inner}; fi"),
            NestingLayer::For => format!("for f in a; do {inner}; done"),
        }
    }

    proptest::proptest! {
        /// This is the exact bug class the `CmdSubstFrames` fixes were found
        /// in: a structural nesting COMBINATION (a case pattern's unpaired
        /// `)` inside a nested/quoted `$(...)`) that no individually-passing
        /// hand-written test happened to cover. Rather than add more
        /// hand-picked combinations, generate a grammar-aware random one:
        /// fold 1..=4 random `NestingLayer`s onto the trivial leaf statement
        /// `echo x` and assert the result always parses. The payload stays
        /// trivial on purpose — this tests structural nesting, not
        /// expression content.
        ///
        /// At most one `QuotedCmdSubst` layer: two of them nests a `"$(...)"`
        /// inside another `"..."`, and the raw double-quoted-string token
        /// (`Token::String`'s lexer regex) has no `$(...)`-awareness at all —
        /// it matches to the first unescaped `"`, full stop. That is a real,
        /// pre-existing gap (confirmed on `main`, unrelated to any
        /// `CmdSubstFrames` frame — it fires before a frame stack ever sees a
        /// token), well outside this fix's scope; see the PR body.
        #[test]
        fn nested_compound_constructs_always_parse(
            layers in proptest::collection::vec(
                proptest::prop_oneof![
                    proptest::strategy::Just(NestingLayer::CmdSubst),
                    proptest::strategy::Just(NestingLayer::QuotedCmdSubst),
                    proptest::strategy::Just(NestingLayer::Case),
                    proptest::strategy::Just(NestingLayer::If),
                    proptest::strategy::Just(NestingLayer::For),
                ],
                1..=4,
            ).prop_filter("at most one QuotedCmdSubst layer", |layers| {
                layers.iter().filter(|l| matches!(l, NestingLayer::QuotedCmdSubst)).count() <= 1
            })
        ) {
            let source = layers
                .iter()
                .fold("echo x".to_string(), |inner, &layer| wrap_in_layer(&inner, layer));
            proptest::prop_assert!(
                parse(&source).is_ok(),
                "grammar-nested construct failed to parse: {source:?}"
            );
        }
    }
}
