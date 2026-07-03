//! Regex dialect helpers shared by the pattern-matching builtins.
//!
//! kaish's regex is ERE-always (Rust's `regex` crate, egrep-style). Commercial
//! LLMs, though, reflexively reach for the GNU BRE spellings — `foo\|bar` for
//! alternation, `a\+` for one-or-more, `\(...\)` for a group, `x\{2,5\}` for an
//! interval — because GNU grep/sed accept them as a BRE extension and the habit
//! is baked deep into how models write shell. Under bare ERE those backslash
//! escapes mean a *literal* `|`/`+`/`(`/`{`, so an agent's `grep 'a\|b'`
//! silently matches nothing (issue #60: measured as the single largest source of
//! wasted explorer tool calls).
//!
//! [`bre_metas_to_ere`] makes kaish a *superset*: it rewrites the GNU BRE
//! backslash-metas into their ERE equivalents so both spellings work. The
//! casualty is narrow and documented — a backslash before one of these metas is
//! now always the operator, never a literal; match the character itself with a
//! bracket class (`[+]`, `[|]`, `[{]`).

/// The GNU BRE backslash-metacharacters kaish rewrites to their bare ERE form.
/// `\|`→alternation, `\+`/`\?`→quantifiers, `\(`/`\)`→group, `\{`/`\}`→interval.
const BRE_METAS: &[char] = &['|', '+', '?', '(', ')', '{', '}'];

/// Append a dialect note to a regex compile error, when the rewrite is the
/// likely culprit: `rewrote` is true when [`bre_metas_to_ere`] changed the
/// pattern, so a formerly-literal escape like `:\)` became an operator and the
/// engine's error (`unopened group` on `:)`) describes a pattern the author
/// never wrote. `strict_flag` names the tool's strict-ERE escape hatch (`-E`,
/// `-E/-r`); awk has none and passes `None`.
pub(crate) fn append_dialect_hint(err: String, rewrote: bool, strict_flag: Option<&str>) -> String {
    if !rewrote {
        return err;
    }
    let escape_hatch = match strict_flag {
        Some(flag) => format!(", or pass {flag} for strict ERE"),
        None => String::new(),
    };
    format!(
        "{err} (note: a backslashed |+?(){{}} is a GNU BRE operator in the default \
         dialect — match the literal character with a bracket class like [)] or \
         [|]{escape_hatch})"
    )
}

/// Rewrite GNU BRE backslash-metas (`\| \+ \? \( \) \{ \}`) into the bare ERE
/// operators Rust's `regex` crate understands. Any other escape is passed
/// through verbatim, so `\.`, `\d`, `\b`, `\w`, and an escaped backslash `\\`
/// keep their meaning — `a\\|b` stays "literal backslash, then alternation",
/// never a stray BRE `\|`.
///
/// Bracket-expression interiors are deliberately *not* special-cased: for every
/// meta in [`BRE_METAS`], the escaped and bare forms denote the same literal
/// character inside a class (`[\|]` and `[|]` both match `|`), so rewriting
/// there yields an equivalent pattern and needs no class tracking.
pub(crate) fn bre_metas_to_ere(pattern: &str) -> String {
    let mut out = String::with_capacity(pattern.len());
    let mut chars = pattern.chars().peekable();
    while let Some(c) = chars.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }
        match chars.peek() {
            // `\<meta>` → drop the backslash, keep the operator.
            Some(&next) if BRE_METAS.contains(&next) => {
                out.push(next);
                chars.next();
            }
            // `\<other>` → preserve both chars (incl. `\\`) untouched.
            Some(&next) => {
                out.push('\\');
                out.push(next);
                chars.next();
            }
            // Trailing lone backslash: leave it for the regex engine to judge.
            None => out.push('\\'),
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    // Alternation — the issue's headline case.
    #[case(r"foo\|bar", "foo|bar")]
    #[case(r"a\|b\|c", "a|b|c")]
    // Quantifiers.
    #[case(r"a\+", "a+")]
    #[case(r"a\?", "a?")]
    // Groups and intervals.
    #[case(r"\(foo\)\+", "(foo)+")]
    #[case(r"x\{2,5\}", "x{2,5}")]
    // Non-meta escapes are preserved verbatim.
    #[case(r"\d\.\w\b", r"\d\.\w\b")]
    // Escaped backslash stays literal; a following bare `|` is already ERE.
    #[case(r"a\\|b", r"a\\|b")]
    // Escaped backslash then BRE alternation → literal backslash, then `|`.
    #[case(r"a\\\|b", r"a\\|b")]
    // Bare ERE forms pass straight through.
    #[case(r"foo|bar", "foo|bar")]
    #[case(r"a+b?", "a+b?")]
    // Inside a class the rewrite is equivalent (both match the literal char).
    #[case(r"[\|]", "[|]")]
    #[case(r"[\{]", "[{]")]
    // Trailing lone backslash is left alone.
    #[case(r"foo\", r"foo\")]
    fn rewrites_gnu_bre_metas(#[case] input: &str, #[case] expected: &str) {
        assert_eq!(bre_metas_to_ere(input), expected);
    }
}
