//! What may spell a variable name.
//!
//! A name is an identifier under [UAX #31] — `XID_Start` then `XID_Continue`,
//! plus `_` — widened with emoji, and closed against characters that do not
//! show themselves.
//!
//! The rule behind all three parts is that a reader must be able to see what
//! the name is. `café` and `名前` are visible. `😁` is visible. A non-breaking
//! space is not: `a\u{a0}b` renders as `a b` and is one name that looks like
//! two words. A zero-width space is worse — `a\u{200b}b` renders as `ab` and is
//! a different variable from `ab`. A right-to-left override reorders the text
//! around it, so the source shows an order the parser does not see. Each of
//! those is rejected, and the error names the character.
//!
//! [UAX #31]: https://www.unicode.org/reports/tr31/

use std::fmt;

/// Why a name was refused, carrying the character that caused it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NameError {
    /// The offending character.
    pub ch: char,
    /// What class it fell into.
    pub kind: NameErrorKind,
}

/// The class of character that made a name unreadable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NameErrorKind {
    /// Whitespace: the name looks like more than one word.
    Whitespace,
    /// A format or bidi control: the name does not render as it parses.
    Invisible,
    /// Not an identifier character in any script, and not an emoji.
    NotAnIdentifier,
    /// ASCII punctuation a *word* may hold but a name may not, because it does
    /// not read back through every spelling of a reference.
    AmbiguousAscii,
    /// A dot, which reads as collection access rather than as part of a name.
    DottedName,
}

impl fmt::Display for NameError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.kind == NameErrorKind::DottedName {
            return write!(
                f,
                "variable name contains `.` (U+002E) — kaish reads a dot as collection \
                 access, not as part of a name, so write `name[key]` instead. Quote the \
                 word to use it as a literal string instead"
            );
        }
        if self.kind == NameErrorKind::AmbiguousAscii {
            return write!(
                f,
                "variable name contains `{}` (U+{:04X}) — an ASCII name is letters, \
                 digits, and `_`, because anything else fails to read back through \
                 some spelling of a reference: `$a-b` reads `$a` and then the \
                 literal text. Quote the word to use it as a literal string instead",
                self.ch, self.ch as u32
            );
        }
        let what = match self.kind {
            NameErrorKind::Whitespace => "whitespace",
            NameErrorKind::Invisible => "an invisible character",
            NameErrorKind::AmbiguousAscii | NameErrorKind::DottedName => {
                unreachable!("handled above")
            }
            NameErrorKind::NotAnIdentifier => "a character that is not a letter, digit, or emoji",
        };
        write!(
            f,
            "variable name contains {what} (U+{:04X}) — a name has to read as \
             what it is; quote the word to use it as a literal string instead",
            self.ch as u32
        )
    }
}

/// Format and bidirectional controls that change how text renders without
/// occupying a column. Listed rather than derived from the `Cf` category so
/// the set is reviewable, and so the two emoji joiners below can be excluded
/// from it deliberately.
const INVISIBLE: &[char] = &[
    '\u{00ad}', // SOFT HYPHEN
    '\u{061c}', // ARABIC LETTER MARK
    '\u{180e}', // MONGOLIAN VOWEL SEPARATOR
    '\u{200b}', // ZERO WIDTH SPACE
    '\u{200c}', // ZERO WIDTH NON-JOINER
    '\u{200e}', // LEFT-TO-RIGHT MARK
    '\u{200f}', // RIGHT-TO-LEFT MARK
    '\u{2028}', // LINE SEPARATOR
    '\u{2029}', // PARAGRAPH SEPARATOR
    '\u{202a}', // LEFT-TO-RIGHT EMBEDDING
    '\u{202b}', // RIGHT-TO-LEFT EMBEDDING
    '\u{202c}', // POP DIRECTIONAL FORMATTING
    '\u{202d}', // LEFT-TO-RIGHT OVERRIDE
    '\u{202e}', // RIGHT-TO-LEFT OVERRIDE
    '\u{2060}', // WORD JOINER
    '\u{2066}', // LEFT-TO-RIGHT ISOLATE
    '\u{2067}', // RIGHT-TO-LEFT ISOLATE
    '\u{2068}', // FIRST STRONG ISOLATE
    '\u{2069}', // POP DIRECTIONAL ISOLATE
    '\u{feff}', // ZERO WIDTH NO-BREAK SPACE
];

/// ZERO WIDTH JOINER — invisible alone, but it is what fuses `👨` and `👩`
/// into one glyph. Permitted only between emoji (see [`validate`]), which is
/// the only place it earns its keep.
const ZWJ: char = '\u{200d}';

/// Variation selectors 15 and 16, which pick the text or emoji rendering of
/// the character before them. Like [`ZWJ`], only meaningful after an emoji.
const VARIATION_SELECTORS: [char; 2] = ['\u{fe0e}', '\u{fe0f}'];

/// Emoji, as the blocks that are predominantly pictographic.
///
/// A range list rather than the Unicode `Emoji` property, because the question
/// here is only "a picture a reader can see" versus "a control they cannot",
/// and that boundary does not move with the emoji spec. Blocks that are mostly
/// typography or mathematics are deliberately out even where they hold a few
/// characters with emoji presentation — the Arrows block would otherwise make
/// `a→b` a legal name, and Miscellaneous Technical would admit `⌘`. The cost
/// is that `⌚` and `⏰` are not name characters; the benefit is that the rule
/// can be stated in one sentence.
fn is_emoji(c: char) -> bool {
    matches!(c as u32,
        0x1F000..=0x1FAFF   // pictographs, faces, flags, supplemental, extended-A
        | 0x2600..=0x27BF   // miscellaneous symbols and dingbats
        | 0x2B00..=0x2BFF   // stars and heavy shapes
    )
}

/// May this character begin a name?
pub fn is_name_start(c: char) -> bool {
    c == '_' || unicode_ident::is_xid_start(c) || is_emoji(c)
}

/// May this character continue a name? Joiners are accepted here and checked
/// for context by [`validate`] — a character class alone cannot see what came
/// before it.
pub fn is_name_continue(c: char) -> bool {
    unicode_ident::is_xid_continue(c)
        || is_emoji(c)
        || c == ZWJ
        || VARIATION_SELECTORS.contains(&c)
}

/// Check a whole name, returning the first character that makes it unreadable.
///
/// Runs over the name rather than per character because the joiners are only
/// legitimate after an emoji: `👨‍👩` is one glyph, while `a‍b` renders as `ab`
/// and is a different variable from `ab`.
pub fn validate(name: &str) -> Result<(), NameError> {
    // A one-character ASCII punctuation name is a special parameter — `$$`,
    // `$?`, `$@`, `$#` — never a name a user wrote. The `Ident` token cannot
    // start with punctuation, so no assignment can create one, and the ASCII
    // class below would otherwise refuse `${$}`. Restricted to ASCII on
    // purpose: a lone zero-width character IS lexable as a name start, and
    // still has to be refused.
    if name.len() == 1 && name.is_ascii() && !name.starts_with(|c: char| c.is_ascii_alphanumeric() || c == '_') {
        return Ok(());
    }

    let mut previous: Option<char> = None;
    for (i, c) in name.chars().enumerate() {
        if c.is_ascii() {
            // An ASCII name is letters, digits, and `_`. The `Ident` token
            // admits `-`, `@`, `.`, and `#` so that words, paths, hostnames,
            // and ids keep them, but none of the four reads back through every
            // spelling of a reference — `$a-b` reads `$a` and then the literal
            // `-b`, and `a:b` has no read spelling at all. A name that binds
            // one way and cannot be read another is the silent write this rule
            // removes.
            //
            // `.` and `#` are refused here too, not left to the validator. The
            // validator writes a better message — it knows the exact spelling
            // to suggest — but it only ever sees an assignment, and `read`,
            // `unset`, `push`, and `scatter --as` take a name at runtime with
            // no validator pass in front of them. Leaving the two characters
            // out left `read a.b` binding a name no read could reach, which is
            // the whole defect.
            if c == '.' {
                return Err(NameError { ch: c, kind: NameErrorKind::DottedName });
            }
            if !(c.is_ascii_alphanumeric() || c == '_') {
                return Err(NameError { ch: c, kind: NameErrorKind::AmbiguousAscii });
            }
            previous = Some(c);
            continue;
        }
        if c.is_whitespace() {
            return Err(NameError { ch: c, kind: NameErrorKind::Whitespace });
        }
        if INVISIBLE.contains(&c) {
            return Err(NameError { ch: c, kind: NameErrorKind::Invisible });
        }
        if c == ZWJ || VARIATION_SELECTORS.contains(&c) {
            // Only after an emoji, and never leading.
            match previous {
                Some(p) if is_emoji(p) => {}
                _ => return Err(NameError { ch: c, kind: NameErrorKind::Invisible }),
            }
            previous = Some(c);
            continue;
        }
        let legal = if i == 0 { is_name_start(c) } else { is_name_continue(c) };
        if !legal {
            return Err(NameError { ch: c, kind: NameErrorKind::NotAnIdentifier });
        }
        previous = Some(c);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn visible_names_in_any_script_are_accepted() {
        for name in ["v", "_x", "café", "名前", "Ω", "переменная", "x1", "😁", "x😁", "👨\u{200d}👩", "❤\u{fe0f}"] {
            assert!(validate(name).is_ok(), "{name:?} should be a legal name");
        }
    }

    #[test]
    fn whitespace_that_looks_like_a_word_break_is_refused() {
        for (name, ch) in [("a\u{a0}b", '\u{a0}'), ("a\u{3000}b", '\u{3000}')] {
            let err = validate(name).expect_err("should be refused");
            assert_eq!(err.ch, ch);
            assert_eq!(err.kind, NameErrorKind::Whitespace);
        }
    }

    #[test]
    fn invisible_characters_are_refused() {
        for name in ["a\u{200b}b", "a\u{202e}b", "a\u{200c}b", "a\u{feff}b", "a\u{ad}b"] {
            let err = validate(name).expect_err("{name:?} should be refused");
            assert_eq!(err.kind, NameErrorKind::Invisible, "for {name:?}");
        }
    }

    /// The joiners are the one deliberate exception, and it is narrow: they
    /// carry an emoji sequence and nothing else.
    #[test]
    fn joiners_are_refused_away_from_emoji() {
        for name in ["a\u{200d}b", "\u{200d}x", "a\u{fe0f}"] {
            let err = validate(name).expect_err("should be refused");
            assert_eq!(err.kind, NameErrorKind::Invisible, "for {name:?}");
        }
    }

    /// Typography and mathematics are not names, even when the block next
    /// door is full of emoji.
    #[test]
    fn punctuation_and_symbols_are_not_identifiers() {
        for name in ["a«b", "a→b", "a⌘b", "a▪b"] {
            assert!(validate(name).is_err(), "{name:?} should be refused");
        }
    }

    /// The message has to name the character, since the whole problem is that
    /// the reader cannot see it.
    #[test]
    fn the_message_names_the_codepoint() {
        let err = validate("a\u{a0}b").expect_err("refused");
        let text = err.to_string();
        assert!(text.contains("U+00A0"), "got: {text}");
        assert!(text.contains("quote"), "the message must say what to do: {text}");
    }
}
