use super::{LexerError, Spanned};

/// One word with its quoted characters retained for pattern contexts.
#[derive(Debug, Clone, PartialEq)]
pub struct EscapedWord {
    pub source: String,
    pub literal: String,
    pub glob_pattern: String,
    pub regex_pattern: String,
    pub has_unquoted_glob: bool,
    pub expands_tilde: bool,
}

impl EscapedWord {
    pub(crate) fn glob_error(&self) -> String {
        format!(
            "{} contains a backslash escape and an unquoted glob; quote the whole word: {}",
            self.source,
            quote_literal(&self.literal),
        )
    }
}

fn quote_literal(text: &str) -> String {
    if !text.contains('\'') {
        return format!("'{text}'");
    }
    let text = text
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('$', "\\$");
    format!("\"{text}\"")
}

/// Encode a quoted case pattern as a glob that matches only its literal text.
pub(crate) fn literal_glob_pattern(text: &str) -> String {
    let mut pattern = String::with_capacity(text.len());
    for character in text.chars() {
        if character.is_whitespace() || "*?[]{}\\'\"$;|&<>()=,:#".contains(character) {
            pattern.push('\\');
        }
        pattern.push(character);
    }
    pattern
}

pub(super) enum ScannedWord {
    Plain(usize),
    Escaped { end: usize, word: EscapedWord },
}

/// Read a literal word, leaving unquoted shell separators in the scanner.
pub(super) fn scan_word(
    source: &str,
    characters: &[(usize, char)],
    start: usize,
) -> Result<Option<ScannedWord>, Spanned<LexerError>> {
    let initial = characters[start].1;
    if initial.is_whitespace() || "'\"$;|&<>()={}[],:!#".contains(initial) || initial == '\u{60}' {
        return Ok(None);
    }

    let mut end = start;
    let mut bracket_depth = 0usize;
    let mut first_escape = None;
    let mut has_unquoted_glob = false;
    while end < characters.len() {
        let (position, character) = characters[end];
        if character == '\\' {
            let Some(&(_, quoted)) = characters.get(end + 1) else {
                return Err(Spanned::new(
                    LexerError::TrailingBackslash,
                    position..source.len(),
                ));
            };
            if matches!(quoted, '\n' | '\r') {
                break;
            }
            first_escape.get_or_insert(end);
            end += 2;
            continue;
        }
        if character.is_whitespace()
            || "'\"$;|&<>()={},:!".contains(character)
            || character == '\u{60}'
            || (character == ']' && bracket_depth == 0)
        {
            break;
        }
        if character == '[' {
            bracket_depth += 1;
        } else if character == ']' {
            bracket_depth -= 1;
        }
        has_unquoted_glob |= matches!(character, '*' | '?' | '[');
        end += 1;
    }

    if end == start {
        return Ok(None);
    }
    let Some(first_escape) = first_escape else {
        return Ok(Some(ScannedWord::Plain(end)));
    };
    let prefix = &source[characters[start].0..characters[first_escape].0];
    if prefix.starts_with(['-', '+'])
        && prefix
            .as_bytes()
            .get(1)
            .is_some_and(u8::is_ascii_alphabetic)
    {
        if prefix.len() != 2 {
            return Err(Spanned::new(
                LexerError::EscapedCombinedFlag,
                characters[start].0..characters[end - 1].0 + characters[end - 1].1.len_utf8(),
            ));
        }
        return Ok(Some(ScannedWord::Plain(first_escape)));
    }
    let byte_end = characters
        .get(end)
        .map_or(source.len(), |&(position, _)| position);
    let mut literal = String::new();
    let mut glob_pattern = String::new();
    let mut regex_pattern = String::new();
    let mut cursor = start;
    while cursor < end {
        let character = characters[cursor].1;
        if character == '\\' {
            let quoted = characters[cursor + 1].1;
            literal.push(quoted);
            glob_pattern.push('\\');
            glob_pattern.push(quoted);
            regex_pattern.push_str(&regex::escape(&quoted.to_string()));
            cursor += 2;
        } else {
            literal.push(character);
            glob_pattern.push(character);
            regex_pattern.push(character);
            cursor += 1;
        }
    }
    Ok(Some(ScannedWord::Escaped {
        end,
        word: EscapedWord {
            source: source[characters[start].0..byte_end].to_string(),
            literal,
            glob_pattern,
            regex_pattern,
            has_unquoted_glob,
            expands_tilde: prefix.starts_with('~') && prefix.contains('/'),
        },
    }))
}
