//! Regex dialect helpers shared by the pattern-matching builtins.
//!
//! The engine is Rust's `regex` crate, which speaks an ERE-like syntax. Two
//! translations sit in front of it, one per dialect the builtins actually
//! read:
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
//! - [`gawk_ere_to_regex`] is `awk`'s only mode: gawk has no BRE, so bare
//!   `( ) { } | + ?` are already operators and `\( \) \{ \} \| \+ \?` are
//!   already literal in both gawk and the engine — the translation only
//!   touches the handful of spots where gawk's own ERE reads differently from
//!   the engine's (a bare `{` that isn't digit-led, `\b`, `\y`, `\<`, `\>`,
//!   `` \` ``, `\'`, `\d`, `\D`). `tests/awk_gnu_regex_tests.rs` records
//!   gawk's output.
//! - [`translate_strict_ere`] is `grep -E`'s and `sed -E`/`-r`'s pass:
//!   `[...]` interiors are rewritten using the same [`posix_class_pattern`]
//!   table `gnu_bre_to_regex` uses, and every backslash escape reads GNU's
//!   way too, via the same [`classify_gnu_escape`] table `gnu_bre_to_regex`
//!   reads — `grep -E '\d'` is the literal letter `d`, not the engine's own
//!   digit class, with GNU's "stray \" warning; `\( \) \{ \} \| \+ \?` and
//!   the rest of ERE's own metacharacters are already literal escaped in
//!   both GNU ERE and the engine, so those pass straight through. One rule
//!   does not travel with the rest: a bare `{`, `*`, `+`, or `?` with
//!   nothing before it to repeat is a literal character in `/usr/bin/grep
//!   -E` but a refusal in `/usr/bin/sed -E` — confirmed on the same corpus,
//!   not assumed. `lenient_operators` is the one parameter where the two
//!   callers differ; everything else about the pass is identical.
//!
//! Every bracket-expression translator — `gnu_bre_to_regex`,
//! `translate_strict_ere`, and `gawk_ere_to_regex`'s own bracket handling —
//! routes `[:alpha:]` and its eleven siblings through [`posix_class_pattern`],
//! the regex engine's own `[:alpha:]` being ASCII-only where GNU grep, GNU
//! sed, and gawk in a UTF-8 locale are not; all three agree on the same
//! glibc class table.

/// A GNU BRE rewritten into the regex engine's syntax, with the warnings GNU
/// grep prints for it (`stray \ before d`), without the `grep: warning: `
/// prefix.
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct BreTranslation {
    pub(crate) pattern: String,
    pub(crate) warnings: Vec<String>,
}

/// Format a regex refusal: the value and the rule, plus `tail` — the
/// caller's own reminder that the reader may have meant a different dialect
/// (`grep`'s and `sed`'s wording differ: different flag, different example
/// command). Strict ERE is already the dialect a `tail` would point at, so
/// its refusals pass `""` and get no trailing reminder.
fn refusal(tail: &str, what: impl std::fmt::Display) -> String {
    if tail.is_empty() {
        format!("invalid pattern: {what}")
    } else {
        format!("invalid pattern: {what}. {tail}")
    }
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
/// `/usr/bin/sed` 4.10 and gawk 5.4.1 read the same glibc locale data and
/// agree with grep on every class in the corpus — `tests/sed_gnu_regex_tests.rs`
/// and `tests/awk_gnu_regex_tests.rs` record the same table for them.
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
/// into this translation, which `sed`'s and `awk`'s own class rewrites
/// depend on staying out of `gnu_bre_to_regex`'s signature.
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

/// A bracket expression [`bracket_expression`]/[`bracket_item`] could not
/// translate. `Unmatched` covers a fault the engine's own error already
/// names reasonably (an unclosed `[`, an unsupported multi-character
/// collating symbol) — strict ERE may leave the bracket exactly as written
/// and let that error stand. `Invalid` covers a fault the engine would
/// otherwise accept silently, in a way GNU does not (an unrecognized class
/// name, `[:name:]` written without its own brackets) — this must always
/// refuse, in both dialects.
enum BracketFault {
    Unmatched(String),
    Invalid(String),
}

impl BracketFault {
    fn into_message(self) -> String {
        match self {
            BracketFault::Unmatched(m) | BracketFault::Invalid(m) => m,
        }
    }
}

/// What GNU gives `\c` for a letter or digit `c` — the one table
/// [`gnu_bre_to_regex`] and [`translate_strict_ere`] both read, so BRE and
/// ERE cannot drift apart on it. `None` means `c` is not one of these: the
/// caller's own "any other letter is stray" rule applies.
enum GnuEscape {
    /// `\w \W \s \S` — already the engine's own class syntax, and an atom a
    /// quantifier can repeat.
    WordClass,
    /// `\b \B` — already the engine's own boundary syntax, but zero-width:
    /// no quantifier follows it directly.
    Boundary,
    /// `` \< \> \` \' `` — GNU's word/buffer anchors; the engine spells them
    /// differently.
    Anchor(&'static str),
    /// `\1`-`\9` — a back-reference. The engine has none.
    Backreference,
}

fn classify_gnu_escape(c: char) -> Option<GnuEscape> {
    Some(match c {
        'w' | 'W' | 's' | 'S' => GnuEscape::WordClass,
        'b' | 'B' => GnuEscape::Boundary,
        '<' => GnuEscape::Anchor(r"\b{start}"),
        '>' => GnuEscape::Anchor(r"\b{end}"),
        '`' => GnuEscape::Anchor(r"\A"),
        '\'' => GnuEscape::Anchor(r"\z"),
        '1'..='9' => GnuEscape::Backreference,
        _ => return None,
    })
}

/// GNU's "stray \" warning text for a backslash before an ordinary
/// character, shared by BRE and strict ERE.
fn stray_warning(c: char) -> String {
    let what = if c.is_whitespace() {
        "white space".to_string()
    } else if c.is_control() {
        "unprintable character".to_string()
    } else {
        c.to_string()
    };
    format!("stray \\ before {what}")
}

/// GNU refuses `[:name:]` written without the class's own brackets —
/// `[[:alpha:]]` is the required spelling, not `[:alpha:]`. Confirmed
/// against `/usr/bin/grep -E`/`/usr/bin/sed -E`: the message names `space`
/// regardless of the identifier actually written, and fires even when that
/// identifier is not a real class name (`[:al pha:]`, `[^:alpha:]`);
/// `[::]` (nothing between the colons) and `[:alpha:0-9]` (more content
/// after the second colon) do not trigger it — GNU only reads this shape as
/// the mistake when the colon-to-colon span is the bracket's entire body.
/// `index` is just past the optional leading `^`.
fn missing_class_brackets(chars: &[char], index: usize) -> bool {
    if chars.get(index) != Some(&':') {
        return false;
    }
    let Some(close) = (index..chars.len()).find(|&i| chars[i] == ']') else {
        return false;
    };
    close >= index + 3 && chars[close - 1] == ':'
}

/// Translate a bracket expression, shared by BRE and strict ERE — GNU grep
/// and GNU sed read `[...]` the same way regardless of dialect: every
/// character inside is literal, backslash included, and `[:class:]`,
/// `[.c.]`, `[=c=]` are recognized. `index` is just past the opening `[`.
fn bracket_expression(chars: &[char], index: &mut usize, tail: &str) -> Result<String, BracketFault> {
    let unmatched = || BracketFault::Unmatched(refusal(tail, r"unmatched `[` — close the bracket expression with `]`, or write `\[` to match a literal `[`"));
    let mut out = String::from("[");
    if chars.get(*index) == Some(&'^') {
        out.push('^');
        *index += 1;
    }
    if missing_class_brackets(chars, *index) {
        return Err(BracketFault::Invalid(refusal(
            tail,
            "character class syntax is `[[:space:]]`, not `[:space:]` — wrap the class name in its own `[...]`",
        )));
    }
    let mut first = true;
    loop {
        let c = chars.get(*index).copied().ok_or_else(unmatched)?;
        if c == ']' && !first {
            *index += 1;
            out.push(']');
            break;
        }
        first = false;
        let item = bracket_item(chars, index, tail)?;
        let is_range =
            chars.get(*index) == Some(&'-') && matches!(chars.get(*index + 1), Some(n) if *n != ']');
        match item {
            BracketItem::Char(low) if is_range => {
                *index += 1;
                let BracketItem::Char(high) = bracket_item(chars, index, tail)? else {
                    return Err(BracketFault::Unmatched(refusal(tail, "a range cannot end in a character class")));
                };
                if high < low {
                    return Err(BracketFault::Unmatched(refusal(
                        tail,
                        format!("invalid range `{low}-{high}` — write the lower end first"),
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
fn bracket_item(chars: &[char], index: &mut usize, tail: &str) -> Result<BracketItem, BracketFault> {
    let unmatched = || BracketFault::Unmatched(refusal(tail, r"unmatched `[` — close the bracket expression with `]`, or write `\[` to match a literal `[`"));
    let c = chars.get(*index).copied().ok_or_else(unmatched)?;
    let delimiter = match (c, chars.get(*index + 1)) {
        ('[', Some(&d @ (':' | '.' | '='))) => d,
        _ => {
            *index += 1;
            return Ok(BracketItem::Char(c));
        }
    };
    let body_start = *index + 2;
    let mut end = body_start;
    while !(chars.get(end) == Some(&delimiter) && chars.get(end + 1) == Some(&']')) {
        if end >= chars.len() {
            return Err(unmatched());
        }
        end += 1;
    }
    let body: String = chars[body_start..end].iter().collect();
    *index = end + 2;
    if delimiter == ':' {
        return match posix_class_pattern(&body) {
            Some(pattern) => Ok(BracketItem::Class(pattern.to_string())),
            None => Err(BracketFault::Invalid(refusal(
                tail,
                format!("invalid character class `[:{body}:]` — use one of {}", POSIX_CLASSES.join(", ")),
            ))),
        };
    }
    let mut body_chars = body.chars();
    match (body_chars.next(), body_chars.next()) {
        (Some(single), None) => Ok(BracketItem::Char(single)),
        _ => Err(BracketFault::Unmatched(refusal(
            tail,
            format!(
                "`[{delimiter}{body}{delimiter}]` is not supported — only a single character works inside `[{delimiter} {delimiter}]`"
            ),
        ))),
    }
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
        self.warnings.push(stray_warning(c));
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
            _ => match classify_gnu_escape(next) {
                Some(GnuEscape::WordClass) => self.push_atom(&format!("\\{next}")),
                Some(GnuEscape::Boundary) => self.push_assertion(&format!("\\{next}")),
                Some(GnuEscape::Anchor(text)) => self.push_assertion(text),
                Some(GnuEscape::Backreference) => {
                    return Err(self.refuse(format!(
                        r"back-reference `\{next}` is not supported — the regex engine has no back-references"
                    )));
                }
                None => {
                    self.warn_stray(next);
                    self.push_literal(next);
                }
            },
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
        bracket_expression(&self.chars, &mut self.index, &self.tail).map_err(BracketFault::into_message)
    }
}

/// Translate a strict ERE (`grep -E`; `sed -E`/`-r`) into the regex engine's
/// syntax. `[...]` interiors are rewritten using [`posix_class_pattern`], the
/// same table [`gnu_bre_to_regex`] uses in default mode — the regex engine's
/// own `[:alpha:]` is ASCII-only in both dialects. Every backslash escape now
/// reads GNU's way too: `\w \W \s \S \b \B \< \> \` \'` keep their GNU
/// meaning, shared with [`gnu_bre_to_regex`] via [`classify_gnu_escape`] so
/// BRE and ERE cannot drift apart on it; `\( \) \{ \} \| \+ \? \. \* \^ \$
/// \[ \] \\` are already literal in both GNU ERE and the engine, so they pass
/// through unchanged; a backslash before any other ordinary character is
/// that character, with GNU's "stray \" warning — confirmed against
/// `/usr/bin/grep -E`: `grep -E -o '\d'` matches the letter `d`, not a
/// digit, which the regex engine's own `\d` would read as a digit class.
///
/// Back-references (`\1`) are refused — the same gap [`gnu_bre_to_regex`]
/// documents for BRE. GNU ERE runs one as a GNU extension when a group
/// precedes it; the regex engine has none in any dialect.
///
/// A bracket expression the translator cannot make sense of because the
/// engine's own error already names the fault (an unmatched `[`) is left
/// exactly as written; an unrecognized class name always refuses instead —
/// the engine would otherwise read `[[:foo:]]` as a plain set of the six
/// characters `:foo:`, matching in a way GNU never does. An escape-level
/// fault — a trailing backslash or a back-reference — fails outright too;
/// `tail` is appended to every refusal, empty for strict ERE, since ERE is
/// already the dialect a `tail` would point the reader at.
///
/// `a{` (unterminated), a bare `{2}` with nothing to its left to repeat, and
/// a leading `*`/`+`/`?` are three more spots GNU grep and GNU sed disagree,
/// confirmed against `/usr/bin/grep -E` and `/usr/bin/sed -E` on the same
/// corpus: grep reads an operator with nothing to repeat as a literal
/// character (`grep -E 'fn main() {'` is common, written from bash/Rust
/// habit); sed's regcomp refuses every one of them outright, unchanged from
/// before this leniency existed. `lenient_operators` is grep's opt-in; sed
/// passes `false` and keeps refusing exactly as it did. Both dialects still
/// gain GNU's `{,m}` shorthand for `{0,m}` regardless of `lenient_operators`
/// — the regex engine has no syntax for an omitted low bound, so a
/// legitimately GNU-shaped interval is rewritten either way.
pub(crate) fn translate_strict_ere(
    pattern: &str,
    tail: &str,
    lenient_operators: bool,
) -> Result<BreTranslation, String> {
    EreTranslator::new(pattern, tail, lenient_operators).run()
}

/// A backslash before this character is already literal in both GNU ERE and
/// the engine: no rewrite needed.
fn is_ere_meta(c: char) -> bool {
    matches!(c, '(' | ')' | '{' | '}' | '|' | '+' | '?' | '.' | '*' | '^' | '$' | '[' | ']' | '\\')
}

struct EreTranslator {
    chars: Vec<char>,
    index: usize,
    out: String,
    warnings: Vec<String>,
    tail: String,
    /// Whether the position just scanned can take a quantifier: right after
    /// a real atom (a literal, a bracket class, a closing group). `*`, `+`,
    /// `?`, and a digit-led `{` need one — without it there is nothing to
    /// repeat.
    quantifiable: bool,
    /// `grep -E`'s leniency for an operator with nothing to repeat — see
    /// [`translate_strict_ere`].
    lenient_operators: bool,
}

impl EreTranslator {
    fn new(pattern: &str, tail: &str, lenient_operators: bool) -> Self {
        Self {
            chars: pattern.chars().collect(),
            index: 0,
            out: String::with_capacity(pattern.len() + 8),
            warnings: Vec::new(),
            tail: tail.to_string(),
            quantifiable: false,
            lenient_operators,
        }
    }

    fn peek(&self, offset: usize) -> Option<char> {
        self.chars.get(self.index + offset).copied()
    }

    fn run(mut self) -> Result<BreTranslation, String> {
        while let Some(c) = self.peek(0) {
            match c {
                '\\' => {
                    self.index += 1;
                    self.escape()?;
                }
                '[' => {
                    let saved = self.index;
                    self.index += 1;
                    match bracket_expression(&self.chars, &mut self.index, &self.tail) {
                        Ok(text) => {
                            self.out.push_str(&text);
                            self.quantifiable = true;
                        }
                        Err(BracketFault::Invalid(message)) => return Err(message),
                        Err(BracketFault::Unmatched(_)) => {
                            // Malformed: leave it exactly as written, so the
                            // engine's own error names it.
                            self.index = saved + 1;
                            self.out.push(c);
                            self.quantifiable = true;
                        }
                    }
                }
                '{' if self.quantifiable => self.interval_or_literal_brace(),
                '{' => self.literal_brace(),
                // `(?s)`/`(?:...)` are the engine's own inline-flag and
                // non-capturing-group syntax, not GNU ERE at all — kaish's
                // `-U` multiline grep documents `(?s).` as the way to span
                // newlines. A `?` straight after `(` passes through
                // untouched rather than reading it as a bare, unquantifiable
                // operator, so that escape hatch keeps working.
                '?' if !self.quantifiable && self.out.ends_with('(') => {
                    self.out.push('?');
                    self.index += 1;
                }
                '*' | '+' | '?' if self.quantifiable => {
                    self.out.push(c);
                    self.index += 1;
                }
                '*' | '+' | '?' => self.literal_operator(c),
                '(' => {
                    self.out.push('(');
                    self.index += 1;
                    self.quantifiable = false;
                }
                ')' => {
                    self.out.push(')');
                    self.index += 1;
                    self.quantifiable = true;
                }
                '|' => {
                    self.out.push('|');
                    self.index += 1;
                    self.quantifiable = false;
                }
                '^' => {
                    self.out.push('^');
                    self.index += 1;
                    self.quantifiable = false;
                }
                '$' => {
                    self.out.push('$');
                    self.index += 1;
                    self.quantifiable = false;
                }
                other => {
                    self.out.push(other);
                    self.index += 1;
                    self.quantifiable = true;
                }
            }
        }
        Ok(BreTranslation { pattern: self.out, warnings: self.warnings })
    }

    /// `{` with nothing before it to repeat, or a body that is not a valid
    /// interval — `grep -E`'s leniency reads it as one literal character;
    /// `sed -E`'s does not, so the raw `{` passes to the engine unchanged
    /// and its own refusal stands, exactly as before this leniency existed.
    fn literal_brace(&mut self) {
        if self.lenient_operators {
            self.out.push_str(r"\{");
        } else {
            self.out.push('{');
        }
        self.index += 1;
        self.quantifiable = true;
    }

    /// `*`, `+`, or `?` with nothing before it to repeat. Same leniency
    /// split as [`Self::literal_brace`].
    fn literal_operator(&mut self, c: char) {
        if self.lenient_operators {
            let mut buffer = [0u8; 4];
            self.out.push_str(&regex::escape(c.encode_utf8(&mut buffer)));
        } else {
            self.out.push(c);
        }
        self.index += 1;
        self.quantifiable = true;
    }

    /// `self.index` is on `{` and a real atom precedes it. Reads ahead for a
    /// GNU-shaped interval body (digits, at most one comma, closed by `}`);
    /// a valid body with an omitted low bound (`{,5}`) is GNU's shorthand
    /// for `{0,5}`, which the engine has no syntax for, so it is rewritten —
    /// every other valid body passes through unchanged, already engine
    /// syntax. A body that is not this shape falls back to
    /// [`Self::literal_brace`]/[`Self::literal_operator`]'s leniency split,
    /// the same as an unquantifiable operator — GNU reads `a{x}` and `a{1`
    /// as the literal text `a{x}` and `a{1` too.
    fn interval_or_literal_brace(&mut self) {
        let start = self.index + 1;
        let mut end = start;
        while matches!(self.chars.get(end), Some(c) if c.is_ascii_digit() || *c == ',') {
            end += 1;
        }
        let commas = self.chars[start..end].iter().filter(|&&c| c == ',').count();
        let valid_shape = self.chars.get(end) == Some(&'}') && end > start && commas <= 1;
        if !valid_shape {
            self.literal_brace();
            return;
        }
        let body: String = self.chars[start..end].iter().collect();
        match body.strip_prefix(',') {
            Some(rest) => {
                self.out.push_str(&format!("{{0,{rest}}}"));
            }
            None => {
                self.out.push('{');
                self.out.push_str(&body);
                self.out.push('}');
            }
        }
        self.index = end + 1;
        self.quantifiable = true;
    }

    /// Translate the escape after a backslash; `self.index` is on its second
    /// character.
    fn escape(&mut self) -> Result<(), String> {
        let Some(next) = self.peek(0) else {
            return Err(refusal(
                &self.tail,
                r"trailing backslash — write `\\` to match a literal backslash",
            ));
        };
        self.index += 1;
        if is_ere_meta(next) {
            self.out.push('\\');
            self.out.push(next);
            self.quantifiable = true;
            return Ok(());
        }
        match classify_gnu_escape(next) {
            Some(GnuEscape::WordClass) => {
                self.out.push('\\');
                self.out.push(next);
                self.quantifiable = true;
            }
            Some(GnuEscape::Boundary) => {
                self.out.push('\\');
                self.out.push(next);
                self.quantifiable = false;
            }
            Some(GnuEscape::Anchor(text)) => {
                self.out.push_str(text);
                self.quantifiable = false;
            }
            Some(GnuEscape::Backreference) => {
                return Err(refusal(
                    &self.tail,
                    format!(
                        r"back-reference `\{next}` is not supported — the regex engine has no back-references"
                    ),
                ));
            }
            None => {
                let mut buffer = [0u8; 4];
                self.out.push_str(&regex::escape(next.encode_utf8(&mut buffer)));
                self.warnings.push(stray_warning(next));
                self.quantifiable = true;
            }
        }
        Ok(())
    }
}

/// Translate a gawk ERE (every regex `awk` reads: `/re/`, a dynamic string,
/// `FS`, `split()`'s separator) into the regex engine's syntax.
///
/// gawk's ERE is already close to the engine's native syntax — bare
/// `( ) { } | + ?` are operators, an escaped one is literal, exactly like the
/// engine — so this only touches the spots where gawk's regex reads
/// differently:
///
/// - a bare `{` is literal unless a digit follows it immediately (gawk only
///   commits to interval parsing when it sees one; the engine would otherwise
///   refuse `a{` or `a{x}` as a bad repetition, where gawk reads a literal
///   brace — once gawk commits, an invalid count (`a{2,1}`) or a `{n}` with
///   nothing before it to repeat is a refusal in both);
/// - `\<` `\>` are GNU word-start/end anchors, `` \` `` `\'` are GNU
///   buffer-start/end anchors, and `\y` is a word boundary — gawk's spelling
///   for what the engine calls `\b{start}`, `\b{end}`, `\A`, `\z`, `\b`;
/// - `\b` itself is gawk's backspace character, not a word boundary (the
///   engine's `\b` is the word boundary — `\y` is gawk's spelling for that);
/// - `\d` `\D` are not gawk regexp operators, so they read as the literal
///   letter, not the engine's Perl-style digit class;
/// - `\1`-`\9` are refused: real gawk reads a backslash-digit as an octal
///   escape, which the engine has no way to run (same gap as a BRE
///   back-reference in `sed`/`grep`; unlike sed's/grep's back-reference
///   reading, `\8`/`\9` are gawk's plain digits, since they're not valid
///   octal — kaish refuses those two as well, a narrow, documented gap).
///
/// Everything else — `\n \t \r \a \f \v`, `\w \W \s \S \B`, every other
/// escaped punctuation character, and a `[...]` bracket expression — already
/// reads the same in gawk and the engine, so it passes through unchanged.
/// Bracket-expression interiors are one narrow exception: gawk applies its
/// own escape rules there too (`[\d]` is a class containing the literal
/// letter `d`), but this translator does not — see the "known gaps" awk
/// regex tests.
pub(crate) fn gawk_ere_to_regex(pattern: &str) -> Result<String, String> {
    GawkEreTranslator::new(pattern).run()
}

struct GawkEreTranslator {
    chars: Vec<char>,
    index: usize,
    out: String,
}

impl GawkEreTranslator {
    fn new(pattern: &str) -> Self {
        Self {
            chars: pattern.chars().collect(),
            index: 0,
            out: String::with_capacity(pattern.len() + 8),
        }
    }

    fn peek(&self, offset: usize) -> Option<char> {
        self.chars.get(self.index + offset).copied()
    }

    fn run(mut self) -> Result<String, String> {
        while let Some(c) = self.peek(0) {
            self.index += 1;
            match c {
                '\\' => self.escape()?,
                '[' => self.bracket()?,
                // gawk commits to interval parsing only when a digit follows
                // `{` immediately; anything else is a literal brace.
                '{' if !matches!(self.peek(0), Some(d) if d.is_ascii_digit()) => {
                    self.out.push_str(r"\{");
                }
                other => self.out.push(other),
            }
        }
        Ok(self.out)
    }

    /// Translate the escape after a backslash; `self.index` is on its second
    /// character.
    fn escape(&mut self) -> Result<(), String> {
        let Some(next) = self.peek(0) else {
            return Err("trailing backslash — write `\\\\` to match a literal backslash".to_string());
        };
        self.index += 1;
        match next {
            'y' => self.out.push_str(r"\b"),
            '<' => self.out.push_str(r"\b{start}"),
            '>' => self.out.push_str(r"\b{end}"),
            '`' => self.out.push_str(r"\A"),
            '\'' => self.out.push_str(r"\z"),
            // gawk's backslash-b is a literal backspace, not a word boundary.
            'b' => self.out.push('\u{8}'),
            '0'..='9' => {
                return Err(format!(
                    "`\\{next}` is not supported — kaish's awk regex has no octal \
                     escapes or back-references; match the character directly"
                ));
            }
            // gawk and the engine already agree on these control and class escapes.
            'n' | 't' | 'r' | 'a' | 'f' | 'v' | 'w' | 'W' | 's' | 'S' | 'B' => {
                self.out.push('\\');
                self.out.push(next);
            }
            // Any other letter is not a gawk regexp operator — the literal letter
            // (`\d`, `\D`, `\q`, …), never the engine's own meaning for it.
            c if c.is_ascii_alphabetic() => self.out.push(c),
            // Punctuation: literal in both dialects.
            other => {
                self.out.push('\\');
                self.out.push(other);
            }
        }
        Ok(())
    }

    /// Copy a bracket expression `[...]` through onto `self.out`: gawk's
    /// bracket rules already match the engine's (ranges, `[.c.]`, `[=c=]`,
    /// and a backslash still escapes the next character — unlike POSIX
    /// BRE/ERE, `[\]abc]` is one class matching `]`, `a`, `b`, `c`, a GNU
    /// extension), except `[:class:]`, rewritten through
    /// [`posix_class_pattern`] the same as `gnu_bre_to_regex` and
    /// `translate_strict_ere` — the regex engine's own `[:alpha:]` is
    /// ASCII-only, where gawk in a UTF-8 locale is not. An unrecognized class
    /// name refuses outright — confirmed against gawk 5.4.1:
    /// `gawk '$0 ~ /[[:foo:]]/'` is a fatal "invalid character class name",
    /// not a silent match against the six characters `:foo:`.  gawk's own
    /// leniency for `[:name:]` written without its own brackets (a warning,
    /// not a refusal — `grep`'s and `sed`'s shared [`bracket_expression`]
    /// disagrees, see [`missing_class_brackets`]) is untouched: this
    /// translator never reads that shape as anything but a plain bracket
    /// set, matching gawk. `self.index` is just past `[`.
    fn bracket(&mut self) -> Result<(), String> {
        let unmatched = || {
            "unmatched `[` — close the bracket expression with `]`, or write \
             `\\[` to match a literal `[`"
                .to_string()
        };
        self.out.push('[');
        if self.peek(0) == Some('^') {
            self.out.push('^');
            self.index += 1;
        }
        let mut first = true;
        loop {
            let c = self.peek(0).ok_or_else(unmatched)?;
            if c == '[' && matches!(self.peek(1), Some(':' | '.' | '=')) {
                let delimiter = self.peek(1).unwrap_or(':');
                let body_start = self.index + 2;
                let mut end = body_start;
                loop {
                    match (self.chars.get(end), self.chars.get(end + 1)) {
                        (Some(&a), Some(&b)) if a == delimiter && b == ']' => break,
                        (Some(_), _) => end += 1,
                        (None, _) => return Err(unmatched()),
                    }
                }
                if delimiter == ':' {
                    let body: String = self.chars[body_start..end].iter().collect();
                    match posix_class_pattern(&body) {
                        Some(pattern) => self.out.push_str(pattern),
                        None => {
                            return Err(format!(
                                "invalid character class `[:{body}:]` — use one of {}",
                                POSIX_CLASSES.join(", "),
                            ));
                        }
                    }
                } else {
                    for &item in &self.chars[self.index..end + 2] {
                        self.out.push(item);
                    }
                }
                self.index = end + 2;
                first = false;
                continue;
            }
            if c == ']' && !first {
                self.out.push(']');
                self.index += 1;
                break;
            }
            first = false;
            if c == '\\' && self.peek(1).is_some() {
                self.out.push('\\');
                self.out.push(self.peek(1).unwrap_or('\\'));
                self.index += 2;
            } else {
                self.out.push(c);
                self.index += 1;
            }
        }
        Ok(())
    }
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
    fn strict_ere_fixes_bracket_classes(
        #[case] pattern: &str,
        #[case] c: char,
        #[case] expect_class_translated: bool,
    ) {
        let rewritten = translate_strict_ere(pattern, TEST_TAIL, true).expect("valid ERE").pattern;
        if expect_class_translated {
            assert_ne!(rewritten, pattern, "class should have translated: {pattern:?}");
            let re = regex::Regex::new(&rewritten).expect("translation compiles");
            assert!(re.is_match(&c.to_string()), "{c:?} against {rewritten:?}");
        } else {
            assert_eq!(rewritten, pattern, "non-class ERE syntax must pass through untouched");
        }
    }

    #[rstest]
    // A malformed bracket the engine's own error already names (here, an
    // unclosed `[`) is left exactly as written.
    #[case("[unclosed")]
    fn strict_ere_leaves_malformed_brackets_alone(#[case] pattern: &str) {
        assert_eq!(translate_strict_ere(pattern, TEST_TAIL, true).expect("valid ERE").pattern, pattern);
    }

    /// An unrecognized class name always refuses — the engine would
    /// otherwise read `[[:bogus:]]` as a plain set of the seven characters
    /// `:bogus:`, matching text GNU refuses to compile at all.
    #[test]
    fn strict_ere_refuses_unrecognized_class_name() {
        let message =
            translate_strict_ere("[[:bogus:]]", TEST_TAIL, true).expect_err("GNU refuses this class name");
        assert!(message.contains("bogus"), "{message}");
    }

    #[rstest]
    // GNU ERE's own metacharacters are already literal escaped, in both GNU
    // and the engine: no rewrite, no warning.
    #[case(r"fn\(x\)", r"fn\(x\)")]
    #[case(r"a\{2,5\}", r"a\{2,5\}")]
    #[case(r"a\|b", r"a\|b")]
    #[case(r"a\+b\?\.c\*", r"a\+b\?\.c\*")]
    #[case(r"\^a\$", r"\^a\$")]
    // GNU word/class/anchor escapes already read like the engine's, or are
    // rewritten to the engine's spelling for them.
    #[case(r"\w\W\s\S\b\B", r"\w\W\s\S\b\B")]
    #[case(r"\<a\>", r"\b{start}a\b{end}")]
    #[case(r"\`a\'", r"\Aa\z")]
    fn strict_ere_leaves_gnu_extensions_alone(#[case] input: &str, #[case] expected: &str) {
        let translation = translate_strict_ere(input, TEST_TAIL, true).expect("valid ERE");
        assert_eq!(translation.pattern, expected, "input {input:?}");
        assert!(translation.warnings.is_empty(), "input {input:?}");
    }

    #[rstest]
    // A backslash before an ordinary letter is that letter, with GNU's
    // "stray \" warning — the regex engine's own Perl-style class meaning
    // for `\d` never applies. Confirmed against `/usr/bin/grep -E`.
    #[case(r"\d", "d", r"stray \ before d")]
    #[case(r"\D", "D", r"stray \ before D")]
    #[case(r"\A", "A", r"stray \ before A")]
    #[case(r"\z", "z", r"stray \ before z")]
    #[case(r"\Z", "Z", r"stray \ before Z")]
    #[case(r"\p", "p", r"stray \ before p")]
    #[case(r"\x", "x", r"stray \ before x")]
    #[case(r"\h", "h", r"stray \ before h")]
    #[case(r"\v", "v", r"stray \ before v")]
    #[case(r"\R", "R", r"stray \ before R")]
    #[case(r"\K", "K", r"stray \ before K")]
    #[case(r"\-", "-", r"stray \ before -")]
    #[case(r"\/", "/", r"stray \ before /")]
    fn strict_ere_stray_backslash_is_a_literal_with_a_warning(
        #[case] input: &str,
        #[case] expected: &str,
        #[case] warning: &str,
    ) {
        let translation = translate_strict_ere(input, TEST_TAIL, true).expect("valid ERE");
        let re = regex::Regex::new(&translation.pattern).expect("translation compiles");
        assert!(re.is_match(expected), "{input:?} -> {:?} should match {expected:?}", translation.pattern);
        assert_eq!(translation.warnings, vec![warning.to_string()]);
    }

    #[rstest]
    #[case(r"\1")]
    #[case(r"(a)\1")]
    fn strict_ere_refuses_back_references(#[case] input: &str) {
        let message = translate_strict_ere(input, TEST_TAIL, true).expect_err("no back-reference support");
        assert!(message.contains("back-reference"), "{message}");
    }

    #[test]
    fn strict_ere_refuses_trailing_backslash() {
        let message = translate_strict_ere(r"a\", TEST_TAIL, true).expect_err("trailing backslash");
        assert!(message.contains("trailing backslash"), "{message}");
    }
}