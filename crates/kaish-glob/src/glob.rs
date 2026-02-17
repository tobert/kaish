//! Glob pattern matching for case statements and file paths.
//!
//! Implements shell-style glob patterns:
//! - `*` matches zero or more characters
//! - `?` matches exactly one character
//! - `[abc]` matches any character in the set
//! - `[a-z]` matches any character in the range
//! - `[!abc]` or `[^abc]` matches any character NOT in the set
//! - `{a,b,c}` brace expansion (matches any of the alternatives)

/// Maximum number of recursive calls for glob matching. Protects against
/// adversarial patterns like `*a*a*a*...*a` that cause O(n^k) backtracking.
/// Counted as total work (calls), not stack depth, to bound actual CPU cost.
const MAX_MATCH_CALLS: usize = 100_000;

/// Match a string against a glob pattern.
///
/// Returns true if the pattern matches the entire input string.
/// Supports brace expansion: `*.{rs,go}` matches `main.rs` or `main.go`.
///
/// # Examples
/// ```
/// use kaish_glob::glob_match;
///
/// assert!(glob_match("*.rs", "main.rs"));
/// assert!(glob_match("test?", "test1"));
/// assert!(glob_match("[abc]", "b"));
/// assert!(glob_match("*.{rs,go}", "main.rs"));
/// assert!(!glob_match("*.txt", "main.rs"));
/// ```
/// Check if a string contains glob metacharacters (`*`, `?`, `[`).
///
/// Useful for builtins that want to detect when a path argument is a glob
/// pattern and switch to pattern-matching mode.
///
/// ```
/// use kaish_glob::contains_glob;
/// assert!(contains_glob("*.rs"));
/// assert!(contains_glob("src/[ab]*.txt"));
/// assert!(!contains_glob("src/main.rs"));
/// ```
pub fn contains_glob(s: &str) -> bool {
    s.contains('*') || s.contains('?') || s.contains('[')
}

pub fn glob_match(pattern: &str, input: &str) -> bool {
    use std::cell::Cell;

    // Expand braces first, then match each expanded pattern
    let expanded = expand_braces(pattern);
    let calls = Cell::new(0usize);
    for pat in expanded {
        let pat_chars: Vec<char> = pat.chars().collect();
        let input_chars: Vec<char> = input.chars().collect();
        if match_bounded(&pat_chars, 0, &input_chars, 0, &calls) {
            return true;
        }
    }
    false
}

/// Expand brace expressions in a pattern.
///
/// `{a,b,c}` expands to multiple patterns. Supports nested braces.
/// Returns a vector of all expanded patterns.
///
/// # Examples
/// ```
/// use kaish_glob::expand_braces;
///
/// assert_eq!(expand_braces("simple"), vec!["simple"]);
/// assert_eq!(expand_braces("{a,b}"), vec!["a", "b"]);
/// ```
pub fn expand_braces(pattern: &str) -> Vec<String> {
    let chars: Vec<char> = pattern.chars().collect();

    // Find the first top-level brace group
    let mut depth = 0;
    let mut brace_start = None;
    let mut brace_end = None;

    for (i, &c) in chars.iter().enumerate() {
        match c {
            '{' => {
                if depth == 0 {
                    brace_start = Some(i);
                }
                depth += 1;
            }
            '}' => {
                depth -= 1;
                if depth == 0 && brace_start.is_some() {
                    brace_end = Some(i);
                    break;
                }
            }
            _ => {}
        }
    }

    // No braces found - return pattern as-is
    let (start, end) = match (brace_start, brace_end) {
        (Some(s), Some(e)) => (s, e),
        _ => return vec![pattern.to_string()],
    };

    // Extract prefix, alternatives, and suffix
    let prefix: String = chars[..start].iter().collect();
    let suffix: String = chars[end + 1..].iter().collect();
    let brace_content: String = chars[start + 1..end].iter().collect();

    // Split alternatives (respecting nested braces)
    let alternatives = split_brace_alternatives(&brace_content);

    // Recursively expand each alternative combined with prefix/suffix
    let mut results = Vec::new();
    for alt in alternatives {
        let combined = format!("{}{}{}", prefix, alt, suffix);
        // Recursively expand in case there are more braces
        results.extend(expand_braces(&combined));
    }

    results
}

/// Split brace content by commas, respecting nested braces.
fn split_brace_alternatives(content: &str) -> Vec<String> {
    let mut alternatives = Vec::new();
    let mut current = String::new();
    let mut depth = 0;

    for c in content.chars() {
        match c {
            '{' => {
                depth += 1;
                current.push(c);
            }
            '}' => {
                depth -= 1;
                current.push(c);
            }
            ',' if depth == 0 => {
                alternatives.push(current);
                current = String::new();
            }
            _ => current.push(c),
        }
    }

    // Don't forget the last alternative
    alternatives.push(current);

    alternatives
}

/// Work-bounded recursive matching with backtracking for `*`.
///
/// Returns `false` (non-match) if total recursive calls exceed `MAX_MATCH_CALLS`,
/// preventing ReDoS from adversarial patterns.
fn match_bounded(
    pattern: &[char],
    pi: usize,
    input: &[char],
    ii: usize,
    calls: &std::cell::Cell<usize>,
) -> bool {
    let count = calls.get() + 1;
    calls.set(count);
    if count > MAX_MATCH_CALLS {
        return false;
    }

    // Both exhausted - match!
    if pi >= pattern.len() && ii >= input.len() {
        return true;
    }

    // Pattern exhausted but input remains - no match
    if pi >= pattern.len() {
        return false;
    }

    match pattern[pi] {
        '*' => {
            // Skip consecutive stars
            let mut next_pi = pi;
            while next_pi < pattern.len() && pattern[next_pi] == '*' {
                next_pi += 1;
            }

            // Star at end matches everything remaining
            if next_pi >= pattern.len() {
                return true;
            }

            // Try matching star with 0, 1, 2, ... characters
            for skip in 0..=(input.len() - ii) {
                if match_bounded(pattern, next_pi, input, ii + skip, calls) {
                    return true;
                }
            }
            false
        }

        '?' => {
            // Must have at least one character to match
            if ii >= input.len() {
                return false;
            }
            match_bounded(pattern, pi + 1, input, ii + 1, calls)
        }

        '[' => {
            // Must have at least one character to match
            if ii >= input.len() {
                return false;
            }

            // Parse character class
            let (matches, end_idx) = parse_char_class(&pattern[pi..], input[ii]);
            if matches {
                match_bounded(pattern, pi + end_idx, input, ii + 1, calls)
            } else {
                false
            }
        }

        // Escape next character
        '\\' if pi + 1 < pattern.len() => {
            if ii >= input.len() {
                return false;
            }
            if pattern[pi + 1] == input[ii] {
                match_bounded(pattern, pi + 2, input, ii + 1, calls)
            } else {
                false
            }
        }

        c => {
            // Literal character match
            if ii >= input.len() {
                return false;
            }
            if c == input[ii] {
                match_bounded(pattern, pi + 1, input, ii + 1, calls)
            } else {
                false
            }
        }
    }
}

/// Parse a character class `[...]` and return whether the character matches.
///
/// Returns (matches, length) where length is how many pattern chars were consumed.
fn parse_char_class(pattern: &[char], ch: char) -> (bool, usize) {
    if pattern.is_empty() || pattern[0] != '[' {
        return (false, 0);
    }

    let mut idx = 1;
    let mut negate = false;

    // Check for negation
    if idx < pattern.len() && (pattern[idx] == '!' || pattern[idx] == '^') {
        negate = true;
        idx += 1;
    }

    // Special case: ] as first char is literal
    let first_char = idx;
    let mut matched = false;

    while idx < pattern.len() {
        let c = pattern[idx];

        // End of character class
        if c == ']' && idx > first_char {
            idx += 1;
            break;
        }

        // Range a-z
        if idx + 2 < pattern.len() && pattern[idx + 1] == '-' && pattern[idx + 2] != ']' {
            let start = c;
            let end = pattern[idx + 2];
            if ch >= start && ch <= end {
                matched = true;
            }
            idx += 3;
            continue;
        }

        // Single character
        if c == ch {
            matched = true;
        }
        idx += 1;
    }

    // Handle unclosed bracket - treat as literal
    if idx >= pattern.len() && (pattern.len() < 2 || pattern[pattern.len() - 1] != ']') {
        return (pattern[0] == ch, 1);
    }

    (if negate { !matched } else { matched }, idx)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literal_matches() {
        assert!(glob_match("hello", "hello"));
        assert!(glob_match("", ""));
        assert!(!glob_match("hello", "world"));
        assert!(!glob_match("hello", "hell"));
        assert!(!glob_match("hello", "helloo"));
    }

    #[test]
    fn star_wildcard() {
        assert!(glob_match("*", ""));
        assert!(glob_match("*", "anything"));
        assert!(glob_match("*.rs", "main.rs"));
        assert!(glob_match("*.rs", ".rs"));
        assert!(glob_match("test*", "test"));
        assert!(glob_match("test*", "testing"));
        assert!(glob_match("*test*", "mytestfile"));
        assert!(glob_match("a*b*c", "abc"));
        assert!(glob_match("a*b*c", "aXXXbYYYc"));
        assert!(!glob_match("*.rs", "main.txt"));
        assert!(!glob_match("test*", "mytest"));
    }

    #[test]
    fn question_wildcard() {
        assert!(glob_match("?", "a"));
        assert!(glob_match("???", "abc"));
        assert!(glob_match("test?", "test1"));
        assert!(glob_match("?est", "test"));
        assert!(!glob_match("?", ""));
        assert!(!glob_match("?", "ab"));
        assert!(!glob_match("???", "ab"));
    }

    #[test]
    fn char_class_simple() {
        assert!(glob_match("[abc]", "a"));
        assert!(glob_match("[abc]", "b"));
        assert!(glob_match("[abc]", "c"));
        assert!(!glob_match("[abc]", "d"));
        assert!(!glob_match("[abc]", ""));
    }

    #[test]
    fn char_class_range() {
        assert!(glob_match("[a-z]", "m"));
        assert!(glob_match("[a-z]", "a"));
        assert!(glob_match("[a-z]", "z"));
        assert!(!glob_match("[a-z]", "A"));
        assert!(!glob_match("[a-z]", "0"));
        assert!(glob_match("[0-9]", "5"));
        assert!(glob_match("[a-zA-Z]", "M"));
    }

    #[test]
    fn char_class_negated() {
        assert!(glob_match("[!abc]", "d"));
        assert!(glob_match("[^abc]", "d"));
        assert!(!glob_match("[!abc]", "a"));
        assert!(!glob_match("[^abc]", "b"));
    }

    #[test]
    fn escape_sequence() {
        assert!(glob_match("\\*", "*"));
        assert!(glob_match("\\?", "?"));
        assert!(glob_match("test\\*", "test*"));
        assert!(!glob_match("\\*", "a"));
    }

    #[test]
    fn combined_patterns() {
        assert!(glob_match("*.tar.gz", "archive.tar.gz"));
        assert!(glob_match("file[0-9].txt", "file5.txt"));
        assert!(glob_match("test_?_*.rs", "test_a_foo.rs"));
        assert!(!glob_match("file[0-9].txt", "filea.txt"));
    }

    #[test]
    fn case_statement_patterns() {
        assert!(glob_match("*.rs", "main.rs"));
        assert!(glob_match("*.py", "script.py"));
        assert!(glob_match("y", "y"));
        assert!(glob_match("yes", "yes"));
        assert!(glob_match("[Yy]*", "Yes"));
        assert!(glob_match("[Yy]*", "yes"));
        assert!(glob_match("[Yy]*", "y"));
        assert!(glob_match("[Nn]*", "no"));
        assert!(glob_match("*", "anything"));
    }

    #[test]
    fn consecutive_stars() {
        assert!(glob_match("**", "anything"));
        assert!(glob_match("a**b", "ab"));
        assert!(glob_match("a**b", "aXXXb"));
    }

    #[test]
    fn edge_cases() {
        assert!(glob_match("", ""));
        assert!(!glob_match("", "a"));
        assert!(glob_match("a*", "a"));
        assert!(glob_match("*a", "a"));
        assert!(glob_match("*/*", "foo/bar"));
        assert!(!glob_match("*/*", "foobar"));
    }

    #[test]
    fn char_class_literal_dash() {
        assert!(glob_match("[-abc]", "-"));
        assert!(glob_match("[-abc]", "a"));
        assert!(glob_match("[abc-]", "-"));
        assert!(glob_match("[abc-]", "c"));
        assert!(glob_match("[a-c]", "b"));
        assert!(!glob_match("[a-c]", "-"));
    }

    #[test]
    fn char_class_literal_bracket() {
        assert!(glob_match("[]abc]", "]"));
        assert!(glob_match("[]abc]", "a"));
        assert!(glob_match("[!]abc]", "x"));
        assert!(!glob_match("[!]abc]", "]"));
    }

    #[test]
    fn char_class_multiple_ranges() {
        assert!(glob_match("[a-zA-Z0-9]", "m"));
        assert!(glob_match("[a-zA-Z0-9]", "M"));
        assert!(glob_match("[a-zA-Z0-9]", "5"));
        assert!(!glob_match("[a-zA-Z0-9]", "_"));
        assert!(!glob_match("[a-zA-Z0-9]", " "));
    }

    #[test]
    fn char_class_with_wildcards() {
        assert!(glob_match("[abc]*", "aXXX"));
        assert!(glob_match("[abc]*", "a"));
        assert!(!glob_match("[abc]*", "dXXX"));
        assert!(glob_match("*[0-9]", "test5"));
        assert!(glob_match("*[0-9]", "5"));
        assert!(!glob_match("*[0-9]", "test"));
        assert!(glob_match("[abc]?", "a1"));
        assert!(!glob_match("[abc]?", "a"));
        assert!(!glob_match("[abc]?", "a12"));
    }

    #[test]
    fn multiple_char_classes() {
        assert!(glob_match("[abc][123]", "a1"));
        assert!(glob_match("[abc][123]", "c3"));
        assert!(!glob_match("[abc][123]", "a4"));
        assert!(!glob_match("[abc][123]", "d1"));
        assert!(glob_match("[a-z][A-Z][0-9]", "xY9"));
    }

    #[test]
    fn backtracking_stress() {
        assert!(glob_match("a*a*a*a*a*a*a*a", "aaaaaaaaaaaaaaaa"));
        assert!(!glob_match("a*a*a*a*a*a*a*ab", "aaaaaaaaaaaaaaaa"));
        assert!(glob_match("*a*b*c", "XXXaYYYbZZZc"));
        assert!(glob_match("*a*b*c", "abc"));
        assert!(!glob_match("*a*b*c", "XXXaYYYcZZZb"));
        assert!(glob_match("*.*.txt", "file.backup.txt"));
        assert!(!glob_match("*.*.txt", "file.txt"));
    }

    #[test]
    fn real_world_file_patterns() {
        assert!(glob_match("*.rs", "main.rs"));
        assert!(glob_match("*.rs", "lib.rs"));
        assert!(glob_match("*_test.rs", "parser_test.rs"));
        assert!(!glob_match("*_test.rs", "parser.rs"));
        assert!(glob_match(".*", ".gitignore"));
        assert!(glob_match(".*", ".env"));
        assert!(!glob_match(".*", "visible"));
        assert!(glob_match("*.tar.gz", "archive.tar.gz"));
        assert!(glob_match("*.tar.gz", "backup.tar.gz"));
        assert!(!glob_match("*.tar.gz", "archive.tar"));
        assert!(!glob_match("*.tar.gz", "archive.gz"));
        assert!(glob_match("app.log.[0-9]", "app.log.1"));
        assert!(glob_match("app.log.[0-9]", "app.log.9"));
        assert!(!glob_match("app.log.[0-9]", "app.log.10"));
        assert!(glob_match("*.{json,yaml,toml}", "config.json"));
        assert!(glob_match("*.{json,yaml,toml}", "config.yaml"));
        assert!(glob_match("*.{json,yaml,toml}", "config.toml"));
        assert!(!glob_match("*.{json,yaml,toml}", "config.xml"));
    }

    #[test]
    fn special_characters_in_input() {
        assert!(glob_match("test", "test"));
        assert!(!glob_match("test", "te*t"));
        assert!(!glob_match("test", "te?t"));
        assert!(glob_match("file\\[1\\]", "file[1]"));
        assert!(glob_match("test\\?", "test?"));
    }

    #[test]
    fn whitespace_handling() {
        assert!(glob_match("hello world", "hello world"));
        assert!(glob_match("hello*world", "hello   world"));
        assert!(glob_match("* *", "hello world"));
        assert!(glob_match("*\t*", "hello\tworld"));
    }

    #[test]
    fn case_sensitivity() {
        assert!(glob_match("Hello", "Hello"));
        assert!(!glob_match("Hello", "hello"));
        assert!(!glob_match("hello", "Hello"));
        assert!(glob_match("[Hh]ello", "Hello"));
        assert!(glob_match("[Hh]ello", "hello"));
    }

    #[test]
    fn long_strings() {
        let long_str = "a".repeat(1000);
        assert!(glob_match("*", &long_str));
        assert!(glob_match("a*", &long_str));
        assert!(glob_match("*a", &long_str));
        let mixed = format!("{}X{}", "a".repeat(500), "a".repeat(500));
        assert!(glob_match("*X*", &mixed));
        assert!(!glob_match("*Y*", &mixed));
    }

    #[test]
    fn unicode_basic() {
        assert!(glob_match("héllo", "héllo"));
        assert!(glob_match("*ñ*", "español"));
        assert!(glob_match("?", "ü"));
        assert!(glob_match("[αβγ]", "β"));
    }

    #[test]
    fn negated_char_class_edge_cases() {
        assert!(glob_match("[!a-z]", "A"));
        assert!(glob_match("[!a-z]", "5"));
        assert!(!glob_match("[!a-z]", "m"));
        assert!(glob_match("[!a-zA-Z]", "5"));
        assert!(!glob_match("[!a-zA-Z]", "x"));
        assert!(!glob_match("[!a-zA-Z]", "X"));
    }

    #[test]
    fn path_like_patterns() {
        assert!(glob_match("src/*.rs", "src/main.rs"));
        assert!(!glob_match("src/*.rs", "test/main.rs"));
        assert!(glob_match("*/*/*.rs", "src/foo/bar.rs"));
        assert!(!glob_match("*/*/*.rs", "src/bar.rs"));
        assert!(glob_match("v?.0", "v1.0"));
        assert!(glob_match("v?.0", "v2.0"));
        assert!(!glob_match("v?.0", "v10.0"));
    }

    #[test]
    fn brace_expansion_basic() {
        assert!(glob_match("{foo,bar}", "foo"));
        assert!(glob_match("{foo,bar}", "bar"));
        assert!(!glob_match("{foo,bar}", "baz"));
        assert!(glob_match("test_{a,b,c}", "test_a"));
        assert!(glob_match("test_{a,b,c}", "test_b"));
        assert!(glob_match("test_{a,b,c}", "test_c"));
        assert!(!glob_match("test_{a,b,c}", "test_d"));
        assert!(glob_match("{debug,release}.exe", "debug.exe"));
        assert!(glob_match("{debug,release}.exe", "release.exe"));
        assert!(glob_match("lib{foo,bar}.so", "libfoo.so"));
        assert!(glob_match("lib{foo,bar}.so", "libbar.so"));
    }

    #[test]
    fn brace_expansion_with_wildcards() {
        assert!(glob_match("*.{rs,go,py}", "main.rs"));
        assert!(glob_match("*.{rs,go,py}", "server.go"));
        assert!(glob_match("*.{rs,go,py}", "script.py"));
        assert!(!glob_match("*.{rs,go,py}", "style.css"));
        assert!(glob_match("file{1,2,3}.txt", "file1.txt"));
        assert!(glob_match("test?.{log,txt}", "test1.log"));
        assert!(glob_match("test?.{log,txt}", "testA.txt"));
        assert!(glob_match("[abc].{x,y}", "a.x"));
        assert!(glob_match("[abc].{x,y}", "b.y"));
    }

    #[test]
    fn brace_expansion_multiple_braces() {
        assert!(glob_match("{a,b}{1,2}", "a1"));
        assert!(glob_match("{a,b}{1,2}", "a2"));
        assert!(glob_match("{a,b}{1,2}", "b1"));
        assert!(glob_match("{a,b}{1,2}", "b2"));
        assert!(!glob_match("{a,b}{1,2}", "c1"));
        assert!(glob_match("{a,b}{1,2}{x,y}", "a1x"));
        assert!(glob_match("{a,b}{1,2}{x,y}", "b2y"));
    }

    #[test]
    fn brace_expansion_nested() {
        assert!(glob_match("{a,{b,c}}", "a"));
        assert!(glob_match("{a,{b,c}}", "b"));
        assert!(glob_match("{a,{b,c}}", "c"));
        assert!(glob_match("{{a,b},{c,d}}", "a"));
        assert!(glob_match("{{a,b},{c,d}}", "b"));
        assert!(glob_match("{{a,b},{c,d}}", "c"));
        assert!(glob_match("{{a,b},{c,d}}", "d"));
    }

    #[test]
    fn brace_expansion_empty_alternatives() {
        assert!(glob_match("{,un}do", "do"));
        assert!(glob_match("{,un}do", "undo"));
        assert!(glob_match("test{,s}", "test"));
        assert!(glob_match("test{,s}", "tests"));
    }

    #[test]
    fn brace_expansion_single_item() {
        assert!(glob_match("{foo}", "foo"));
        assert!(!glob_match("{foo}", "bar"));
        assert!(glob_match("test_{only}.rs", "test_only.rs"));
    }

    #[test]
    fn brace_expansion_real_world() {
        assert!(glob_match("src/**/*.{ts,tsx,js,jsx}", "src/**/*.ts"));
        assert!(glob_match("{M,m}akefile", "Makefile"));
        assert!(glob_match("{M,m}akefile", "makefile"));
        assert!(glob_match("README{,.md,.txt}", "README"));
        assert!(glob_match("README{,.md,.txt}", "README.md"));
        assert!(glob_match("README{,.md,.txt}", "README.txt"));
        assert!(glob_match("{LICENSE,LICENCE}{,.md,.txt}", "LICENSE"));
        assert!(glob_match("{LICENSE,LICENCE}{,.md,.txt}", "LICENCE.md"));
        assert!(glob_match("{,.}config{,.json,.yaml}", "config"));
        assert!(glob_match("{,.}config{,.json,.yaml}", ".config"));
        assert!(glob_match("{,.}config{,.json,.yaml}", "config.json"));
        assert!(glob_match("{,.}config{,.json,.yaml}", ".config.yaml"));
        assert!(glob_match("{D,d}ocker{file,-compose.yml}", "Dockerfile"));
        assert!(glob_match("{D,d}ocker{file,-compose.yml}", "dockerfile"));
        assert!(glob_match("{D,d}ocker{file,-compose.yml}", "Docker-compose.yml"));
    }

    #[test]
    fn brace_expansion_no_braces() {
        assert!(glob_match("simple", "simple"));
        assert!(glob_match("*.rs", "main.rs"));
        assert!(glob_match("[abc]", "b"));
    }

    #[test]
    fn brace_expansion_unclosed() {
        assert!(glob_match("{abc", "{abc"));
        assert!(glob_match("test{", "test{"));
        assert!(glob_match("abc}", "abc}"));
    }

    #[test]
    fn expand_braces_unit() {
        assert_eq!(expand_braces("simple"), vec!["simple"]);
        assert_eq!(expand_braces("{a,b}"), vec!["a", "b"]);
        assert_eq!(expand_braces("x{a,b}y"), vec!["xay", "xby"]);
        let mut result = expand_braces("{a,b}{1,2}");
        result.sort();
        assert_eq!(result, vec!["a1", "a2", "b1", "b2"]);
    }

    #[test]
    fn redos_protection() {
        // Adversarial pattern: *a*a*a*...*a causes O(n^k) backtracking without depth limits.
        // With MAX_MATCH_DEPTH protection, this must complete quickly (non-match is acceptable).
        let pattern = format!("{}b", "*a".repeat(50));
        let input = "a".repeat(100);
        // The important thing is that this returns in bounded time, not that it matches.
        let _result = glob_match(&pattern, &input);
    }
}
