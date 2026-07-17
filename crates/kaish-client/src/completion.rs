//! Completion context detection — the frontend-neutral half of tab
//! completion.
//!
//! Every kaish frontend (the rustyline REPL, the browser playground in
//! kaish-extras, future embedders) answers the same first question on Tab:
//! *what kind of thing is under the cursor?* That's pure text analysis, so
//! it lives here, once. What each frontend does with the answer — which
//! candidate sources it queries (`KernelClient::tool_schemas`,
//! `KernelClient::list_vars`, a real or virtual filesystem) and how it
//! renders them — stays frontend-local.

/// What kind of completion to offer based on cursor context.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompletionContext {
    /// Start of line, after |, ;, &&, || → complete command names
    Command,
    /// After $ or within ${ → complete variable names
    Variable,
    /// Everything else → complete file paths
    Path,
}

/// Characters that delimit words for completion purposes.
pub fn is_word_delimiter(c: char) -> bool {
    c.is_whitespace() || matches!(c, '|' | ';' | '(' | ')')
}

/// Byte offset where the word under the cursor begins.
pub fn word_start(line: &str, pos: usize) -> usize {
    line[..pos]
        .rfind(is_word_delimiter)
        .map(|i| i + 1)
        .unwrap_or(0)
}

/// Detect the completion context by scanning backwards from cursor position.
pub fn detect_completion_context(line: &str, pos: usize) -> CompletionContext {
    let before = &line[..pos];

    // Check for variable completion: look for $ before cursor
    // Walk backwards to find if we're in a $VAR or ${VAR context
    // But NOT $( which is command substitution
    let bytes = before.as_bytes();
    let mut i = pos;
    while i > 0 {
        i -= 1;
        let b = bytes[i];
        if b == b'$' {
            // $( is command substitution, not variable
            if i + 1 < pos && bytes[i + 1] == b'(' {
                break;
            }
            return CompletionContext::Variable;
        }
        if b == b'{' && i > 0 && bytes[i - 1] == b'$' {
            return CompletionContext::Variable;
        }
        // Stop scanning if we hit a non-identifier character
        if !b.is_ascii_alphanumeric() && b != b'_' && b != b'{' {
            break;
        }
    }

    // Check for command position: start of line, or after pipe/semicolon/logical operators/$(
    let trimmed = before.trim();
    if trimmed.is_empty()
        || trimmed.ends_with('|')
        || trimmed.ends_with(';')
        || trimmed.ends_with("&&")
        || trimmed.ends_with("||")
        || trimmed.ends_with("$(")
    {
        return CompletionContext::Command;
    }

    // Find start of current "word" (using delimiters that include parentheses)
    let word_start = before.rfind(is_word_delimiter);
    match word_start {
        None => CompletionContext::Command, // First word on the line
        Some(idx) => {
            // Check what's before the word
            let prefix = before[..=idx].trim();
            if prefix.is_empty()
                || prefix.ends_with('|')
                || prefix.ends_with(';')
                || prefix.ends_with("&&")
                || prefix.ends_with("||")
                || prefix.ends_with("$(")
                || prefix.ends_with("then")
                || prefix.ends_with("else")
                || prefix.ends_with("do")
            {
                CompletionContext::Command
            } else {
                CompletionContext::Path
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_detect_context_command_start() {
        assert_eq!(detect_completion_context("", 0), CompletionContext::Command);
        assert_eq!(detect_completion_context("ec", 2), CompletionContext::Command);
    }

    #[test]
    fn test_detect_context_after_pipe() {
        assert_eq!(
            detect_completion_context("echo hello | gr", 15),
            CompletionContext::Command
        );
    }

    #[test]
    fn test_detect_context_variable() {
        assert_eq!(
            detect_completion_context("echo $HO", 8),
            CompletionContext::Variable
        );
        assert_eq!(
            detect_completion_context("echo ${HO", 9),
            CompletionContext::Variable
        );
    }

    #[test]
    fn test_detect_context_path() {
        assert_eq!(
            detect_completion_context("cat /etc/hos", 12),
            CompletionContext::Path
        );
    }

    #[test]
    fn test_detect_context_command_substitution() {
        // $(cmd should complete commands, not variables
        assert_eq!(
            detect_completion_context("echo $(ca", 9),
            CompletionContext::Command
        );
        assert_eq!(
            detect_completion_context("X=$(ec", 6),
            CompletionContext::Command
        );
    }

    #[test]
    fn test_word_start() {
        assert_eq!(word_start("", 0), 0);
        assert_eq!(word_start("uname", 5), 0);
        assert_eq!(word_start("cat /src/ka", 11), 4);
        assert_eq!(word_start("a | gr", 6), 4);
    }
}

// ── Flag completion ─────────────────────────────────────────────────

/// Byte range of the command word governing the statement that contains
/// `pos`, or `None` when the cursor is still inside (or before) that word —
/// in which case command-name completion applies, not argument completion.
///
/// Scans back to the nearest statement boundary (`|`, `;`, `&`, `(`,
/// newline), then walks forward past leading `VAR=…` assignments and the
/// shell keywords that prefix a command position.
pub fn current_command(line: &str, pos: usize) -> Option<(usize, usize)> {
    const KEYWORDS: [&str; 7] = ["if", "then", "elif", "else", "while", "until", "do"];

    let before = &line[..pos];
    let stmt_start = before
        .rfind(['|', ';', '&', '(', '\n'])
        .map(|i| i + 1)
        .unwrap_or(0);

    let mut word_from = stmt_start;
    loop {
        // Skip whitespace to the next word.
        let rest = &line[word_from..pos];
        let skip = rest.len() - rest.trim_start().len();
        let start = word_from + skip;
        if start >= pos {
            return None; // nothing but whitespace before the cursor
        }
        let end = line[start..pos]
            .find(char::is_whitespace)
            .map(|i| start + i)
            .unwrap_or(pos);
        let word = &line[start..end];

        // `FOO=bar cmd` and `if cmd` — the command is still ahead.
        if word.contains('=') || KEYWORDS.contains(&word) {
            word_from = end;
            continue;
        }
        // The cursor sitting in the command word itself is command
        // completion's business.
        if end >= pos {
            return None;
        }
        return Some((start, end));
    }
}

/// Flag spellings from a tool's params that extend `word` (which starts
/// with `-`): the canonical `--long` for every non-positional param, `-x`
/// for single-char aliases, `--alias` for visible long aliases. Snake-case
/// field-id aliases (they contain `_`) are reachable in scripts but not
/// offered — completion promotes canonical spellings. Sorted, deduped;
/// display and replacement are the same string.
pub fn flag_candidates(
    params: &[kaish_types::tool::ParamSchema],
    word: &str,
) -> Vec<String> {
    let mut out = Vec::new();
    for p in params.iter().filter(|p| !p.positional) {
        let long = format!("--{}", p.name);
        if long.starts_with(word) {
            out.push(long);
        }
        for alias in &p.aliases {
            if alias.contains('_') {
                continue;
            }
            let spelled = if alias.chars().count() == 1 {
                format!("-{alias}")
            } else {
                format!("--{alias}")
            };
            if spelled.starts_with(word) {
                out.push(spelled);
            }
        }
    }
    out.sort();
    out.dedup();
    out
}

#[cfg(test)]
mod flag_tests {
    use super::*;
    use kaish_types::tool::ParamSchema;

    fn param(name: &str, aliases: &[&str], positional: bool) -> ParamSchema {
        let mut p = ParamSchema::new(name.to_string(), "bool".to_string());
        p.aliases = aliases.iter().map(|s| s.to_string()).collect();
        p.positional = positional;
        p
    }

    #[test]
    fn test_current_command_basics() {
        assert_eq!(current_command("uname -", 7), Some((0, 5)));
        assert_eq!(current_command("echo hi | grep -i", 17), Some((10, 14)));
        // Cursor still inside the command word → command completion's turf.
        assert_eq!(current_command("unam", 4), None);
        assert_eq!(current_command("", 0), None);
    }

    #[test]
    fn test_current_command_skips_assignments_and_keywords() {
        assert_eq!(current_command("FOO=1 grep -", 12), Some((6, 10)));
        assert_eq!(current_command("if grep -q x", 12), Some((3, 7)));
        // Nothing after the keyword/assignment yet.
        assert_eq!(current_command("FOO=1 ", 6), None);
    }

    #[test]
    fn test_flag_candidates_spellings() {
        let params = vec![
            param("no-newline", &["n", "no_newline"], false),
            param("paths", &[], true),
        ];
        // Long + short offered; snake alias and positionals never.
        assert_eq!(flag_candidates(&params, "-"), vec!["--no-newline", "-n"]);
        assert_eq!(flag_candidates(&params, "--no"), vec!["--no-newline"]);
        assert_eq!(flag_candidates(&params, "--x"), Vec::<String>::new());
    }
}
