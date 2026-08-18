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
//! A name that mixes scripts is a different problem: every character shows
//! itself, and the name still reads as something it is not. `PАTH` — with
//! CYRILLIC CAPITAL LETTER A where Latin `A` belongs — binds a second variable
//! and leaves `$PATH` alone. That one is a warning, not a refusal ([`mixed_script`]),
//! because refusing it would refuse `変数x` and every other name a writing
//! system spells in two scripts.
//!
//! [UAX #31]: https://www.unicode.org/reports/tr31/
//! [UAX #39]: https://www.unicode.org/reports/tr39/

use std::fmt;

use unicode_script::{Script, UnicodeScript};
use unicode_security::mixed_script::AugmentedScriptSet;
use unicode_security::skeleton;

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
    // `${$}` and `${?}` are the braced spellings of the session identifier and
    // the last exit code, and their name is literally that one character. They
    // are the *only* two: every other special parameter (`$@`, `$#`, `$0`-`$9`)
    // is its own token and never reaches this function.
    //
    // Listed rather than derived as "any single punctuation character". That
    // wider rule looked equivalent — no assignment can create such a name,
    // because the `Ident` token cannot start with punctuation — but the runtime
    // doors do not take names from `Ident`: `read .`, `read @`, and `read -`
    // are ordinary argument words, and each bound a variable no read could
    // reach. The narrow list has no such hole.
    if name == "$" || name == "?" {
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

/// A name spelled in more than one script, and the character that shows it.
///
/// Reported by [`mixed_script`], and a warning at every door — a mixed-script
/// name still binds.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MixedScript {
    /// The name as written.
    pub name: String,
    /// The first character that does not belong to the name's own script.
    pub ch: char,
    /// The script the rest of the name is written in.
    pub script: &'static str,
    /// The script [`MixedScript::ch`] belongs to.
    pub other_script: &'static str,
    /// The all-ASCII spelling the name reads as, from [UAX #39]'s confusables
    /// data. `None` when the plain reading is itself not ASCII — `Ωmega`
    /// reduces to `Ωrnega`, which teaches nothing.
    ///
    /// [UAX #39]: https://www.unicode.org/reports/tr39/
    pub reads_as: Option<String>,
}

impl MixedScript {
    /// What to do about it. Pairs with the message as a suggestion.
    pub fn suggestion(&self) -> String {
        match &self.reads_as {
            Some(plain) => format!("write the name in one script, e.g. `{plain}`"),
            None => "write the name in one script".to_string(),
        }
    }
}

impl fmt::Display for MixedScript {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "`{}` mixes {} and {}", self.name, self.script, self.other_script)?;
        if let Some(plain) = &self.reads_as {
            write!(f, " and reads as `{plain}`")?;
        }
        write!(
            f,
            // "names", not "binds": this message reaches `unset` too, which
            // removes a variable rather than creating one.
            " — `{}` (U+{:04X}) is {}, so this names a different variable",
            self.ch, self.ch as u32, self.other_script
        )
    }
}

/// Is this name spelled in more than one script?
///
/// The rule is [UAX #39]'s Highly Restrictive profile: a name whose characters
/// resolve to one script is fine, and so are the three script sets a writing
/// system needs — Latin with Japanese, with Chinese, or with Korean. `café`,
/// `名前`, `переменная`, and `変数x` all pass. `PАTH` does not.
///
/// Separate from [`validate`] on purpose. `validate` returns an `Err` its
/// callers refuse on, and this is a warning: the name binds either way.
///
/// [UAX #39]: https://www.unicode.org/reports/tr39/
pub fn mixed_script(name: &str) -> Option<MixedScript> {
    // `Common` and `Inherited` characters — ASCII digits, `_`, emoji, and the
    // joiners — intersect every script, so they leave the arithmetic alone
    // without being named here. A character with no script at all (an
    // unassigned code point inside an emoji block) carries no evidence either
    // way and is skipped; folding it in would empty every set and report every
    // emoji name.
    let mut resolved = AugmentedScriptSet::default();
    let mut without_latin = AugmentedScriptSet::default();
    for c in name.chars() {
        let set = AugmentedScriptSet::for_char(c);
        if set.is_empty() {
            continue;
        }
        resolved.intersect_with(set);
        if !set.base.contains_script(Script::Latin) {
            without_latin.intersect_with(set);
        }
    }
    // One script covers the name.
    if !resolved.is_empty() {
        return None;
    }
    // Latin beside Japanese, Chinese, or Korean — the augmented sets Highly
    // Restrictive admits, and the reason this is not simply "one script".
    if without_latin.jpan || without_latin.hanb || without_latin.kore {
        return None;
    }

    // Name the character that stands out rather than the one that happens to
    // break a left-to-right intersection: in `Аbc` the Cyrillic letter comes
    // first, and blaming `b` would point at the characters spelled correctly.
    let spelled: Vec<(char, Script)> = name
        .chars()
        .map(|c| (c, c.script()))
        .filter(|(_, s)| !matches!(s, Script::Common | Script::Inherited | Script::Unknown))
        .collect();
    let mut tally: Vec<(Script, usize)> = Vec::new();
    for (_, s) in &spelled {
        match tally.iter_mut().find(|(t, _)| t == s) {
            Some(entry) => entry.1 += 1,
            None => tally.push((*s, 1)),
        }
    }
    // An empty resolved set with fewer than two scripts present is not
    // reachable — one script always resolves to itself — so a `None` here
    // would be a bug in the walk above rather than a name to report.
    let (mut main_script, mut best) = *tally.first()?;
    for &(script, count) in &tally[1..] {
        if count > best {
            main_script = script;
            best = count;
        }
    }
    let (ch, other) = spelled.into_iter().find(|&(_, s)| s != main_script)?;

    let plain: String = skeleton(name).collect();
    let reads_as = (plain.is_ascii() && plain != name).then_some(plain);

    Some(MixedScript {
        name: name.to_string(),
        ch,
        script: main_script.full_name(),
        other_script: other.full_name(),
        reads_as,
    })
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

    /// Every name kaish accepts today is spelled in one script, and stays
    /// quiet. `Common` and `Inherited` characters — digits, `_`, emoji, and
    /// the joiners — must drop out of the rule on their own; if this goes red,
    /// the arithmetic is wrong, not the list.
    #[test]
    fn single_script_names_are_not_mixed() {
        for name in [
            "v", "_x", "x1", "café", "名前", "Ω", "переменная", "😁", "x😁", "👨\u{200d}👩",
            "❤\u{fe0f}", "$", "?",
        ] {
            assert_eq!(mixed_script(name), None, "{name:?} is one script");
        }
    }

    /// Latin beside Han, Hiragana, or Katakana is a writing system, not a
    /// confusable — UAX #39's Highly Restrictive profile admits it.
    #[test]
    fn latin_with_japanese_is_not_mixed() {
        for name in ["変数x", "x変数", "カタカナ1", "名前_v2"] {
            assert_eq!(mixed_script(name), None, "{name:?} is Highly Restrictive");
        }
    }

    /// The defect this rule exists for.
    #[test]
    fn latin_with_cyrillic_is_mixed() {
        let found = mixed_script("PАTH").expect("PАTH mixes scripts");
        assert_eq!(found.ch, '\u{0410}');
        assert_eq!(found.script, "Latin");
        assert_eq!(found.other_script, "Cyrillic");
        assert_eq!(found.reads_as.as_deref(), Some("PATH"));

        let text = found.to_string();
        assert!(text.contains("U+0410"), "got: {text}");
        assert!(text.contains("Cyrillic"), "got: {text}");
        assert!(text.contains("`PATH`"), "got: {text}");
    }

    /// The odd character out is named even when it comes first — `Аbc` is
    /// three correct letters and one wrong one, not the other way round.
    #[test]
    fn the_minority_script_is_the_one_named() {
        let found = mixed_script("Аbc").expect("Аbc mixes scripts");
        assert_eq!(found.ch, '\u{0410}');
        assert_eq!(found.other_script, "Cyrillic");
    }

    /// Greek beside Latin mixes too, and its plain reading is noise
    /// (`Ωmega` reduces to `Ωrnega`), so the message leaves it out.
    #[test]
    fn latin_with_greek_is_mixed_without_a_plain_reading() {
        let found = mixed_script("Ωmega").expect("Ωmega mixes scripts");
        assert_eq!(found.ch, 'Ω');
        assert_eq!(found.other_script, "Greek");
        assert_eq!(found.reads_as, None);
        assert!(!found.to_string().contains("reads as"), "{found}");
    }

    /// A mixed-script name is still a name — this rule never refuses.
    #[test]
    fn a_mixed_script_name_still_validates() {
        assert!(validate("PАTH").is_ok());
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
