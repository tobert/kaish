//! Regex dialect helpers shared by the pattern-matching builtins.
//!
//! The engine is Rust's `regex` crate, which speaks an ERE-like syntax. Two
//! translations sit in front of it:
//!
//! - [`gnu_bre_to_regex`] is `grep`'s and `sed`'s default (no `-E`/`-r`) mode:
//!   a faithful GNU BRE. Bare `( ) { } | + ?` are literal, `\( \) \{ \} \| \+
//!   \?` are operators, and the context rules for `*`, `^`, `$`, and bracket
//!   expressions follow GNU grep and GNU sed, which agree on all of this.
//!   Models write both tools from GNU habit (`grep "fn consult("`, `sed
//!   's/a\|b/x/'`), so GNU is the specification; `tests/grep_gnu_bre_tests.rs`
//!   and `tests/sed_gnu_regex_tests.rs` record GNU's output. `tail` is the
//!   caller's own "you may have meant ERE" reminder, so `grep`'s and `sed`'s
//!   refusals never quote a flag the reader isn't running.
//! - [`bre_metas_to_ere`] is the older superset `awk` still uses: bare ERE
//!   operators AND the GNU BRE backslash spellings are both operators. A
//!   follow-up commit gives `awk` its own gawk-ERE translation and retires
//!   this one.
//! - [`rewrite_posix_classes`] is `grep -E`'s pass: only `[...]` interiors
//!   change, using the same [`posix_class_pattern`] table `gnu_bre_to_regex`
//!   uses, since strict ERE otherwise passes straight through.
//!
//! Both bracket-expression translators route `[:alpha:]` and its eleven
//! siblings through [`posix_class_pattern`], the regex engine's own
//! `[:alpha:]` being ASCII-only where GNU grep in a UTF-8 locale is not.

/// A GNU BRE rewritten into the regex engine's syntax, with the warnings GNU
/// grep prints for it (`stray \ before d`), without the `grep: warning: `
/// prefix.
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct BreTranslation {
    pub(crate) pattern: String,
    pub(crate) warnings: Vec<String>,
}

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


/// Format a BRE refusal: the value and the rule, plus `tail` — the caller's
/// own reminder that the reader may have meant ERE (`grep`'s and `sed`'s
/// wording differ: different flag, different example command).
fn refusal(tail: &str, what: impl std::fmt::Display) -> String {
    format!("invalid pattern: {what}. {tail}")
}

/// The POSIX class names GNU grep accepts inside `[...]`.
const POSIX_CLASSES: &[&str] = &[
    "alnum", "alpha", "blank", "cntrl", "digit", "graph", "lower", "print", "punct",
    "space", "upper", "xdigit",
];

/// The regex engine's Unicode-aware equivalent for a GNU POSIX bracket
/// class, self-contained (already wrapped in its own `[...]`) so it can sit
/// next to any other item in the caller's bracket expression. `None` for a
/// name GNU does not recognize.
///
/// Matched against `/usr/bin/grep` (GNU grep 3.12, `LC_ALL=C.UTF-8`), not the
/// regex engine's own `[:alpha:]` support, which is ASCII-only —
/// `[[:alpha:]]` must match `日本語テキスト`, not just `héllo wörld`.
/// `tests/grep_gnu_bre_tests.rs` records the corpus this was checked against.
///
/// Two real GNU/glibc quirks ride along, confirmed against 57 Unicode code
/// points spanning every general category before landing on this table:
///
/// - `alpha`/`alnum` also match a non-ASCII decimal digit (Arabic-Indic `٣`,
///   fullwidth `３`) — glibc classifies every Unicode `Nd` character but the
///   ASCII range as alphabetic. `digit` stays ASCII-only regardless.
/// - `space`/`blank` exclude three Unicode "no-break" spaces (U+00A0 NBSP,
///   U+2007 FIGURE SPACE, U+202F NARROW NO-BREAK SPACE), which glibc
///   classifies `[:punct:]` instead, even though Unicode's White_Space
///   property includes them.
///
/// Not chased: glibc additionally classifies U+2028 LINE SEPARATOR as
/// `[:cntrl:]` (Unicode calls it `Zl`, not `Cc`), and `-i` on `[:upper:]` or
/// `[:lower:]` becomes `[:alpha:]` in glibc — neither is in the corpus this
/// gap was found from, and the second would need the case-fold flag threaded
/// into this translation, which the sed/awk branch stacked on this one
/// depends on staying out of `gnu_bre_to_regex`'s signature.
fn posix_class_pattern(name: &str) -> Option<&'static str> {
    Some(match name {
        "alpha" => r"[\p{Alphabetic}[\p{Nd}--0-9]]",
        "alnum" => r"[\p{Alphabetic}\p{Nd}]",
        "upper" => r"[\p{Uppercase}]",
        "lower" => r"[\p{Lowercase}]",
        "digit" => r"[0-9]",
        "space" => r"[\p{White_Space}--[\u{00A0}\u{2007}\u{202F}]]",
        "blank" => r"[\t\p{Zs}--[\u{00A0}\u{2007}\u{202F}]]",
        "cntrl" => r"[\p{Cc}]",
        "print" => r"[\P{Cc}]",
        "graph" => r"[\P{Cc}--[\p{White_Space}--[\u{00A0}\u{2007}\u{202F}]]]",
        "punct" => r"[\P{Cc}--[\p{Alphabetic}\p{Nd}[\p{White_Space}--[\u{00A0}\u{2007}\u{202F}]]]]",
        "xdigit" => r"[0-9A-Fa-f]",
        _ => return None,
    })
}

/// Translate a GNU BRE (`grep` without `-E`/`-F`) into the regex engine's
/// syntax, following GNU grep:
///
/// - bare `( ) { } | + ?` are literal; `\( \) \{n,m\} \| \+ \?` are operators;
/// - `*` is literal at the start of the pattern, a group, or an alternative,
///   and after a leading `^`; `\+ \? \{` there are literal with a warning;
/// - `^` is an anchor only at those starts, `$` only at the end of the pattern
///   or before `\)` / `\|`; elsewhere both are literal;
/// - inside `[...]` every character is literal, backslash included, and
///   `[:class:]`, `[.c.]`, `[=c=]` are recognized;
/// - `\w \W \s \S \b \B \< \> \` \'` keep their GNU meaning; a backslash
///   before any other ordinary character is that character, with a warning.
///
/// Back-references (`\1`) have no engine support and are refused. `tail` is
/// appended to every refusal — the caller's own "you may have meant ERE"
/// reminder (`grep`'s names `-E`; `sed`'s names `-E`/`-r`) — so the error
/// never leaks a message written for a tool the reader isn't running.
pub(crate) fn gnu_bre_to_regex(pattern: &str, tail: &str) -> Result<BreTranslation, String> {
    BreTranslator::new(pattern, tail).run()
}

/// Where the translator stands: what the next `*`, `^`, or `\+` means.
#[derive(Clone, Copy, PartialEq, Eq)]
enum BrePosition {
    /// Start of the pattern, a group, or an alternative.
    Start,
    /// Right after a leading `^` anchor.
    AfterAnchor,
    /// After something a quantifier can repeat.
    Atom,
    /// After a zero-width assertion (`$`, `\b`, `\<`).
    Assertion,
}

/// One item of a bracket expression.
enum BracketItem {
    Char(char),
    Class(String),
}

struct BreTranslator {
    chars: Vec<char>,
    index: usize,
    out: String,
    warnings: Vec<String>,
    /// Output offsets of the open `\(` groups.
    groups: Vec<usize>,
    position: BrePosition,
    /// Output offset where the last atom starts.
    atom_start: usize,
    /// Whether the last atom already carries a quantifier.
    quantified: bool,
    /// The caller's "you may have meant ERE" reminder, appended to a refusal.
    tail: String,
}

impl BreTranslator {
    fn new(pattern: &str, tail: &str) -> Self {
        Self {
            chars: pattern.chars().collect(),
            index: 0,
            out: String::with_capacity(pattern.len() + 8),
            warnings: Vec::new(),
            groups: Vec::new(),
            position: BrePosition::Start,
            atom_start: 0,
            quantified: false,
            tail: tail.to_string(),
        }
    }

    fn refuse(&self, what: impl std::fmt::Display) -> String {
        refusal(&self.tail, what)
    }

    fn peek(&self, offset: usize) -> Option<char> {
        self.chars.get(self.index + offset).copied()
    }

    fn push_atom(&mut self, text: &str) {
        self.atom_start = self.out.len();
        self.out.push_str(text);
        self.position = BrePosition::Atom;
        self.quantified = false;
    }

    fn push_literal(&mut self, c: char) {
        let mut buffer = [0u8; 4];
        self.push_atom(&regex::escape(c.encode_utf8(&mut buffer)));
    }

    fn push_assertion(&mut self, text: &str) {
        self.out.push_str(text);
        self.position = BrePosition::Assertion;
    }

    /// Apply a quantifier to the last atom. GNU allows a second quantifier
    /// (`a**`, `a*\+`); the engine does not, so the quantified atom is
    /// wrapped in a group first.
    fn push_quantifier(&mut self, quantifier: &str) {
        if self.quantified {
            self.out.insert_str(self.atom_start, "(?:");
            self.out.push(')');
        }
        self.out.push_str(quantifier);
        self.quantified = true;
    }

    fn warn_stray(&mut self, c: char) {
        let what = if c.is_whitespace() {
            "white space".to_string()
        } else if c.is_control() {
            "unprintable character".to_string()
        } else {
            c.to_string()
        };
        self.warnings.push(format!("stray \\ before {what}"));
    }

    fn run(mut self) -> Result<BreTranslation, String> {
        while let Some(c) = self.peek(0) {
            self.index += 1;
            match c {
                '\\' => self.escape()?,
                '[' => {
                    let class = self.bracket()?;
                    self.push_atom(&class);
                }
                '*' if self.position == BrePosition::Atom => self.push_quantifier("*"),
                '^' if self.position == BrePosition::Start => {
                    self.out.push('^');
                    self.position = BrePosition::AfterAnchor;
                }
                '$' if self.dollar_is_anchor() => self.push_assertion("$"),
                '.' => self.push_atom("."),
                other => self.push_literal(other),
            }
        }
        if !self.groups.is_empty() {
            return Err(self.refuse(r"unmatched `\(` — close the group with `\)`, or write `(` to match a literal `(`"));
        }
        Ok(BreTranslation { pattern: self.out, warnings: self.warnings })
    }

    /// `$` anchors at the end of the pattern and before `\)` or `\|`.
    fn dollar_is_anchor(&self) -> bool {
        match self.peek(0) {
            None => true,
            Some('\\') => matches!(self.peek(1), Some(')') | Some('|')),
            Some(_) => false,
        }
    }

    /// Translate the escape after a backslash; `self.index` is on its second
    /// character.
    fn escape(&mut self) -> Result<(), String> {
        let Some(next) = self.peek(0) else {
            return Err(self.refuse(r"trailing backslash — write `\\` to match a literal backslash"));
        };
        self.index += 1;
        let quantifiable = self.position == BrePosition::Atom;
        match next {
            '(' => {
                self.groups.push(self.out.len());
                self.out.push('(');
                self.position = BrePosition::Start;
            }
            ')' => {
                let Some(start) = self.groups.pop() else {
                    return Err(self.refuse(r"unmatched `\)` — write `)` to match a literal `)`"));
                };
                self.out.push(')');
                self.atom_start = start;
                self.position = BrePosition::Atom;
                self.quantified = false;
            }
            '|' => {
                self.out.push('|');
                self.position = BrePosition::Start;
            }
            '{' if quantifiable => {
                let interval = self.interval()?;
                self.push_quantifier(&interval);
            }
            '+' | '?' if quantifiable => self.push_quantifier(&next.to_string()),
            '{' | '+' | '?' => {
                self.warn_stray(next);
                self.push_literal(next);
            }
            '}' | '.' | '*' | '[' | ']' | '^' | '$' | '\\' => self.push_literal(next),
            'w' | 'W' | 's' | 'S' => self.push_atom(&format!("\\{next}")),
            'b' | 'B' => self.push_assertion(&format!("\\{next}")),
            '<' => self.push_assertion(r"\b{start}"),
            '>' => self.push_assertion(r"\b{end}"),
            '`' => self.push_assertion(r"\A"),
            '\'' => self.push_assertion(r"\z"),
            '1'..='9' => {
                return Err(self.refuse(format!(
                    r"back-reference `\{next}` is not supported — the regex engine has no back-references"
                )));
            }
            other => {
                self.warn_stray(other);
                self.push_literal(other);
            }
        }
        Ok(())
    }

    /// Read an interval body up to `\}`; `self.index` is just past `\{`.
    fn interval(&mut self) -> Result<String, String> {
        let mut body = String::new();
        loop {
            match (self.peek(0), self.peek(1)) {
                (Some('\\'), Some('}')) => {
                    self.index += 2;
                    break;
                }
                (Some(c), _) => {
                    body.push(c);
                    self.index += 1;
                }
                (None, _) => {
                    return Err(self.refuse(
                        r"unmatched `\{` — close the interval with `\}`, or write `{` to match a literal `{`",
                    ));
                }
            }
        }
        let tail = self.tail.clone();
        let invalid = || {
            refusal(&tail, format!(
                r"invalid interval `\{{{body}\}}` — write `\{{2\}}`, `\{{2,5\}}`, `\{{2,\}}`, or `\{{,5\}}`, lower bound first"
            ))
        };
        let parse = |text: &str| -> Result<Option<u32>, String> {
            if text.is_empty() {
                return Ok(None);
            }
            if !text.chars().all(|c| c.is_ascii_digit()) {
                return Err(invalid());
            }
            text.parse::<u32>().map(Some).map_err(|_| invalid())
        };
        match body.split_once(',') {
            None => match parse(&body)? {
                Some(count) => Ok(format!("{{{count}}}")),
                None => Err(invalid()),
            },
            Some((low, high)) => {
                let low = parse(low)?.unwrap_or(0);
                match parse(high)? {
                    Some(high) if high < low => Err(invalid()),
                    Some(high) => Ok(format!("{{{low},{high}}}")),
                    None => Ok(format!("{{{low},}}")),
                }
            }
        }
    }

    /// Translate a bracket expression; `self.index` is just past `[`.
    fn bracket(&mut self) -> Result<String, String> {
        let tail = self.tail.clone();
        let unmatched = || refusal(&tail, r"unmatched `[` — close the bracket expression with `]`, or write `\[` to match a literal `[`");
        let mut out = String::from("[");
        if self.peek(0) == Some('^') {
            out.push('^');
            self.index += 1;
        }
        let mut first = true;
        loop {
            let c = self.peek(0).ok_or_else(unmatched)?;
            if c == ']' && !first {
                self.index += 1;
                out.push(']');
                break;
            }
            first = false;
            let item = self.bracket_item()?;
            let is_range = self.peek(0) == Some('-') && matches!(self.peek(1), Some(n) if n != ']');
            match item {
                BracketItem::Char(low) if is_range => {
                    self.index += 1;
                    let BracketItem::Char(high) = self.bracket_item()? else {
                        return Err(self.refuse("a range cannot end in a character class"));
                    };
                    if high < low {
                        return Err(self.refuse(format!(
                            "invalid range `{low}-{high}` — write the lower end first"
                        )));
                    }
                    push_class_char(&mut out, low);
                    out.push('-');
                    push_class_char(&mut out, high);
                }
                BracketItem::Char(c) => push_class_char(&mut out, c),
                BracketItem::Class(class) => out.push_str(&class),
            }
        }
        Ok(out)
    }

    /// Read one bracket item: `[:class:]`, `[.c.]`, `[=c=]`, or a character.
    fn bracket_item(&mut self) -> Result<BracketItem, String> {
        let tail = self.tail.clone();
        let unmatched = || refusal(&tail, r"unmatched `[` — close the bracket expression with `]`, or write `\[` to match a literal `[`");
        let c = self.peek(0).ok_or_else(unmatched)?;
        let delimiter = match (c, self.peek(1)) {
            ('[', Some(d @ (':' | '.' | '='))) => d,
            _ => {
                self.index += 1;
                return Ok(BracketItem::Char(c));
            }
        };
        let body_start = self.index + 2;
        let mut end = body_start;
        while !(self.chars.get(end) == Some(&delimiter) && self.chars.get(end + 1) == Some(&']')) {
            if end >= self.chars.len() {
                return Err(unmatched());
            }
            end += 1;
        }
        let body: String = self.chars[body_start..end].iter().collect();
        self.index = end + 2;
        if delimiter == ':' {
            return match posix_class_pattern(&body) {
                Some(pattern) => Ok(BracketItem::Class(pattern.to_string())),
                None => Err(self.refuse(format!(
                    "invalid character class `[:{body}:]` — use one of {}",
                    POSIX_CLASSES.join(", ")
                ))),
            };
        }
        let mut body_chars = body.chars();
        match (body_chars.next(), body_chars.next()) {
            (Some(single), None) => Ok(BracketItem::Char(single)),
            _ => Err(self.refuse(format!(
                "`[{delimiter}{body}{delimiter}]` is not supported — only a single character works inside `[{delimiter} {delimiter}]`"
            ))),
        }
    }
}

/// Rewrite POSIX bracket classes for strict ERE (`grep -E`): only `[...]`
/// interiors change, using [`posix_class_pattern`], the same translation
/// [`gnu_bre_to_regex`] applies in default mode — the regex engine's own
/// `[:alpha:]` is ASCII-only in both dialects. Every other ERE construct
/// (`(...)`, `a|b`, `x{2,5}`, a backslashed literal) passes through
/// untouched.
///
/// A bracket expression the translator cannot make sense of (an unmatched
/// `[`, an unrecognized class name) is left exactly as written, so the
/// engine's own error stands for it; this function never fails.
pub(crate) fn rewrite_posix_classes(pattern: &str) -> String {
    // Errors are discarded below, so `tail` (an "-E" reminder) never surfaces.
    let mut translator = BreTranslator::new(pattern, "");
    let mut out = String::with_capacity(pattern.len());
    while let Some(c) = translator.peek(0) {
        if c == '\\' {
            out.push(c);
            translator.index += 1;
            if let Some(next) = translator.peek(0) {
                out.push(next);
                translator.index += 1;
            }
            continue;
        }
        if c == '[' {
            let saved = translator.index;
            translator.index += 1;
            match translator.bracket() {
                Ok(text) => out.push_str(&text),
                Err(_) => {
                    translator.index = saved + 1;
                    out.push(c);
                }
            }
            continue;
        }
        out.push(c);
        translator.index += 1;
    }
    out
}

/// Write `c` as a literal inside an engine character class.
fn push_class_char(out: &mut String, c: char) {
    if matches!(c, '\\' | '[' | ']' | '&' | '~' | '-' | '^') {
        out.push('\\');
    }
    out.push(c);
}

/// The opener a pattern never closed, if there is exactly one to name.
///
/// Scans outside-in, honoring backslash escapes and the rule that `]` is a
/// literal when it opens a class body (`[]a]`). Returns the innermost opener
/// still waiting at end of input.
fn unbalanced_opener(pattern: &str) -> Option<(usize, char)> {
    let mut chars = pattern.char_indices().peekable();
    let mut open: Vec<(usize, char)> = Vec::new();
    let mut class_body_start: Option<usize> = None;
    let mut class_open_index = 0usize;

    while let Some((index, c)) = chars.next() {
        // A backslash escapes whatever follows, in or out of a class.
        if c == '\\' {
            chars.next();
            continue;
        }
        match class_body_start {
            // Inside `[...]`: only `]` closes, and not in first position,
            // where it is a literal.
            Some(start) => {
                if c == ']' && index != start {
                    class_body_start = None;
                }
            }
            None => match c {
                // `[^...]` keeps the caret out of the first-position rule.
                '[' => {
                    let body = match chars.peek() {
                        Some(&(next_index, '^')) => next_index + 1,
                        Some(&(next_index, _)) => next_index,
                        None => index + 1,
                    };
                    class_body_start = Some(body);
                    class_open_index = index;
                }
                '(' | '{' => open.push((index, c)),
                ')' if open.last().map(|&(_, o)| o) == Some('(') => {
                    open.pop();
                }
                '}' if open.last().map(|&(_, o)| o) == Some('{') => {
                    open.pop();
                }
                _ => {}
            },
        }
    }

    if class_body_start.is_some() {
        return Some((class_open_index, '['));
    }
    open.pop()
}

/// Name the spelling that fixes a pattern the regex engine refused.
///
/// The engine reports what is wrong ("unclosed character class"); an agent
/// needs to know what to write instead. An unbalanced opener is nearly always
/// a literal the author did not escape, so name the escape for the character
/// left open. `pattern` is strict ERE (`grep -E`), where a backslash before
/// any meta is its literal form.
///
/// Returns `None` when no single opener explains the failure, leaving the
/// engine's own message to stand alone.
pub(crate) fn regex_fix_hint(pattern: &str) -> Option<&'static str> {
    let (index, opener) = unbalanced_opener(pattern)?;
    let (spelling, hint) = match opener {
        '[' => (r"\[", r"write `\[` to match a literal `[`"),
        '(' => (r"\(", r"write `\(` to match a literal `(`"),
        '{' => (r"\{", r"write `\{` to match a literal `{`"),
        _ => return None,
    };

    // An unbalanced opener explains the failure only when escaping it is the
    // whole fix. `[)` opens a class AND leaves a group unopened, and naming
    // `\[` there would send the reader back with a pattern that still does not
    // compile. Apply the spelling at the site the scan found and keep the hint
    // only if the result compiles.
    let mut fixed = String::with_capacity(pattern.len() + spelling.len());
    fixed.push_str(&pattern[..index]);
    fixed.push_str(spelling);
    fixed.push_str(&pattern[index + opener.len_utf8()..]);
    regex::Regex::new(&fixed).ok().map(|_| hint)
}


#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    const TEST_TAIL: &str = "pass -E for ERE";

    #[rstest]
    // Strict ERE: a backslash is the literal form of every meta.
    #[case("[cast:", Some(r"write `\[` to match a literal `[`"))]
    #[case("[^abc", Some(r"write `\[` to match a literal `[`"))]
    // A `]` in first position is a literal, so the class is still open.
    #[case("[]", Some(r"write `\[` to match a literal `[`"))]
    #[case("(unclosed", Some(r"write `\(` to match a literal `(`"))]
    #[case("a{2", Some(r"write `\{` to match a literal `{`"))]
    // Balanced patterns have nothing to name.
    #[case("[cast:]", None)]
    #[case("[]]", None)]
    #[case("(a|b)", None)]
    #[case("a{2,5}", None)]
    // An escaped opener is a literal and never opens anything.
    #[case(r"\[cast:", None)]
    // Brackets inside a class are literal, not nested openers.
    #[case("[([{]", None)]
    fn fix_hint_names_the_dialect_correct_escape(
        #[case] pattern: &str,
        #[case] expected: Option<&str>,
    ) {
        assert_eq!(regex_fix_hint(pattern), expected);
    }

    #[rstest]
    // A second unescaped meta after the opener: escaping `[` leaves `)`
    // unopened, so there is no single spelling to name.
    #[case("[)")]
    #[case("[a(b")]
    // The scan reports the class and never reaches the unclosed group.
    #[case("(a[")]
    fn a_pattern_with_two_faults_gets_no_hint(#[case] pattern: &str) {
        assert!(
            regex::Regex::new(pattern).is_err(),
            "fixture must actually be a broken pattern",
        );
        assert_eq!(
            regex_fix_hint(pattern),
            None,
            "naming one fix for a two-fault pattern sends the reader back with \
             a pattern that still does not compile",
        );
    }

    #[rstest]
    #[case("[cast:")]
    #[case("[^abc")]
    #[case("[]")]
    #[case("(unclosed")]
    #[case("a{2")]
    #[case("x[0-9]+(")]
    #[case("日本[")]
    fn every_hint_it_gives_actually_compiles(#[case] pattern: &str) {
        // The hint is applied at the site the scan found, which is what
        // `regex_fix_hint` itself does — a test that searched for the first
        // occurrence of the character could pass while the real fix landed
        // somewhere else.
        let hint = regex_fix_hint(pattern).expect("an open pattern gets a hint");
        let spelling = hint
            .split('`')
            .nth(1)
            .expect("the hint quotes the spelling it recommends");
        let (index, opener) = unbalanced_opener(pattern).expect("an opener");
        let fixed = format!(
            "{}{}{}",
            &pattern[..index],
            spelling,
            &pattern[index + opener.len_utf8()..],
        );
        assert!(
            regex::Regex::new(&fixed).is_ok(),
            "hint {hint:?} produced {fixed:?}, which still does not compile",
        );
    }

    #[rstest]
    // Bare ERE operators are literal characters.
    #[case("fn consult(", r"fn consult\(")]
    #[case("KjCaller {", r"KjCaller \{")]
    #[case("(a|b)+?", r"\(a\|b\)\+\?")]
    // Backslashed operators.
    #[case(r"\(foo\|bar\)", "(foo|bar)")]
    #[case(r"x\{2,5\}", "x{2,5}")]
    #[case(r"x\{,5\}", "x{0,5}")]
    #[case(r"a\+b\?", "a+b?")]
    // `*` is literal at every expression start, and after a leading `^`.
    #[case("*a", r"\*a")]
    #[case(r"\(*a\)", r"(\*a)")]
    #[case(r"a\|*b", r"a|\*b")]
    #[case("^*a", r"^\*a")]
    // `^` and `$` anchor only at the ends of an expression.
    #[case("a^b$", r"a\^b$")]
    #[case(r"a$b", r"a\$b")]
    #[case(r"\(^a$\)", "(^a$)")]
    #[case(r"a$\|^b", "a$|^b")]
    // A second quantifier wraps the first in a group.
    #[case("a**", "(?:a*)*")]
    #[case(r"\(ab\)*\{2\}", "(?:(ab)*){2}")]
    // Every bracket character is literal, backslash included.
    #[case(r"[\d]", r"[\\d]")]
    #[case("[]a]", r"[\]a]")]
    #[case("[[:digit:]x-z]", "[[0-9]x-z]")]
    #[case("[a-]", r"[a\-]")]
    // GNU word and buffer escapes.
    #[case(r"\<a\>", r"\b{start}a\b{end}")]
    #[case(r"\w\s\b", r"\w\s\b")]
    fn translates_gnu_bre(#[case] input: &str, #[case] expected: &str) {
        let translation = gnu_bre_to_regex(input, TEST_TAIL).expect("valid GNU BRE");
        assert_eq!(translation.pattern, expected, "input {input:?}");
        assert!(translation.warnings.is_empty(), "input {input:?}");
    }

    #[rstest]
    #[case(r"\d", r"d", r"stray \ before d")]
    #[case(r"\+a", r"\+a", r"stray \ before +")]
    #[case(r"a\ b", "a b", r"stray \ before white space")]
    fn stray_backslash_is_a_literal_with_a_warning(
        #[case] input: &str,
        #[case] expected: &str,
        #[case] warning: &str,
    ) {
        let translation = gnu_bre_to_regex(input, TEST_TAIL).expect("valid GNU BRE");
        assert_eq!(translation.pattern, expected);
        assert_eq!(translation.warnings, vec![warning.to_string()]);
    }

    #[rstest]
    #[case(r"\(a")]
    #[case(r"a\)")]
    #[case(r"a\{2")]
    #[case(r"a\{2,1\}")]
    #[case(r"a\{x\}")]
    #[case("a\\")]
    #[case("[a")]
    #[case("[[:nope:]]")]
    #[case("[z-a]")]
    #[case(r"\(a\)\1")]
    fn refuses_what_gnu_grep_refuses(#[case] input: &str) {
        let message = gnu_bre_to_regex(input, TEST_TAIL).expect_err("GNU grep exits 2 on this");
        assert!(message.contains("-E"), "names the ERE override: {message}");
    }


    #[rstest]
    // `[:alpha:]` matches a Unicode letter and, per the glibc quirk this
    // mirrors, a non-ASCII decimal digit — but not an ASCII one.
    #[case('\u{00E9}', "alpha", true)] // é
    #[case('\u{65E5}', "alpha", true)] // 日
    #[case('\u{0663}', "alpha", true)] // Arabic-Indic ٣ — the glibc quirk
    #[case('0', "alpha", false)]
    #[case('0', "digit", true)]
    #[case('\u{0663}', "digit", false)] // [:digit:] stays ASCII-only
    #[case('\u{00A0}', "space", false)] // NBSP is [:punct:], not [:space:]
    #[case('\u{00A0}', "blank", false)]
    #[case('\u{00A0}', "punct", true)]
    #[case('\u{3000}', "space", true)] // ideographic space IS [:space:]
    #[case('\u{3000}', "blank", true)]
    #[case('\u{0301}', "punct", true)] // combining acute accent
    #[case('\u{00C9}', "upper", true)] // É
    #[case('\u{00E9}', "lower", true)] // é
    fn posix_class_pattern_matches_gnu_grep(
        #[case] c: char,
        #[case] class: &str,
        #[case] expect_match: bool,
    ) {
        let pattern = posix_class_pattern(class).expect("a recognized class");
        let re = regex::Regex::new(pattern).expect("every class formula compiles");
        assert_eq!(
            re.is_match(&c.to_string()),
            expect_match,
            "{c:?} against [:{class}:] ({pattern})",
        );
    }

    #[rstest]
    // Union with an explicit range and negation still work once a class
    // expands to a Unicode formula.
    #[case("[[:alpha:]0-9_]", 'a', true)]
    #[case("[[:alpha:]0-9_]", '_', true)]
    #[case("[[:alpha:]0-9_]", '\u{65E5}', true)]
    #[case("[^[:alpha:]]", 'a', false)]
    #[case("[^[:alpha:]]", '!', true)]
    fn posix_class_composes_with_ranges_and_negation(
        #[case] pattern: &str,
        #[case] c: char,
        #[case] expect_match: bool,
    ) {
        let translation = gnu_bre_to_regex(pattern, TEST_TAIL).expect("valid GNU BRE");
        let re = regex::Regex::new(&translation.pattern).expect("translation compiles");
        assert_eq!(re.is_match(&c.to_string()), expect_match, "{c:?} against {pattern:?}");
    }

    #[rstest]
    // `-E` gets the same Unicode-aware classes, everything else untouched.
    #[case("[[:alpha:]]", '\u{65E5}', true)]
    #[case("(foo|bar)", 'x', false)] // ERE syntax passes straight through
    fn rewrite_posix_classes_fixes_extended_mode(
        #[case] pattern: &str,
        #[case] c: char,
        #[case] expect_class_translated: bool,
    ) {
        let rewritten = rewrite_posix_classes(pattern);
        if expect_class_translated {
            assert_ne!(rewritten, pattern, "class should have translated: {pattern:?}");
            let re = regex::Regex::new(&rewritten).expect("translation compiles");
            assert!(re.is_match(&c.to_string()), "{c:?} against {rewritten:?}");
        } else {
            assert_eq!(rewritten, pattern, "non-class ERE syntax must pass through untouched");
        }
    }

    #[rstest]
    // A malformed or unrecognized class is left exactly as written, so the
    // engine's own error names it.
    #[case("[[:bogus:]]")]
    #[case("[unclosed")]
    fn rewrite_posix_classes_leaves_malformed_brackets_alone(#[case] pattern: &str) {
        assert_eq!(rewrite_posix_classes(pattern), pattern);
    }

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