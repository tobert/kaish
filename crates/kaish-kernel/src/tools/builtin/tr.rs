//! tr — Translate or delete characters.

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Tr tool: translate, squeeze, or delete characters.
pub struct Tr;

#[async_trait]
impl Tool for Tr {
    fn name(&self) -> &str {
        "tr"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("tr", "Translate or delete characters")
            .param(ParamSchema::required(
                "set1",
                "string",
                "Characters to translate from (or delete with -d)",
            ))
            .param(ParamSchema::optional(
                "set2",
                "string",
                Value::Null,
                "Characters to translate to",
            ))
            .param(ParamSchema::optional(
                "delete",
                "bool",
                Value::Bool(false),
                "Delete characters in SET1 (-d)",
            ))
            .param(ParamSchema::optional(
                "squeeze",
                "bool",
                Value::Bool(false),
                "Squeeze repeated characters (-s)",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let set1 = match args.get_string("set1", 0) {
            Some(s) => s,
            None => return ExecResult::failure(1, "tr: missing SET1 argument"),
        };

        let set2 = args.get_string("set2", 1);
        let delete = args.has_flag("delete") || args.has_flag("d");
        let squeeze = args.has_flag("squeeze") || args.has_flag("s");

        let input = ctx.take_stdin().unwrap_or_default();

        // Expand character classes and ranges
        let chars1 = expand_char_set(&set1);
        let chars2 = set2.as_ref().map(|s| expand_char_set(s));

        let output = if delete {
            // Delete mode: remove all characters in set1
            input
                .chars()
                .filter(|c| !chars1.contains(c))
                .collect::<String>()
        } else if let Some(ref c2) = chars2 {
            // Translate mode
            let translated: String = input
                .chars()
                .map(|c| translate_char(c, &chars1, c2))
                .collect();

            if squeeze {
                squeeze_chars(&translated, c2)
            } else {
                translated
            }
        } else if squeeze {
            // Squeeze-only mode
            squeeze_chars(&input, &chars1)
        } else {
            return ExecResult::failure(1, "tr: SET2 required for translation");
        };

        ExecResult::with_output(OutputData::text(output))
    }
}

/// Expand a character set specification.
/// Supports: literal characters, ranges (a-z), and classes ([:alpha:], [:digit:], etc.)
fn expand_char_set(spec: &str) -> Vec<char> {
    let mut chars = Vec::new();
    let mut iter = spec.chars().peekable();

    while let Some(c) = iter.next() {
        if c == '[' && iter.peek() == Some(&':') {
            // Character class like [:alpha:]
            iter.next(); // consume ':'
            let mut class_name = String::new();
            while let Some(&ch) = iter.peek() {
                if ch == ':' {
                    iter.next(); // consume ':'
                    if iter.peek() == Some(&']') {
                        iter.next(); // consume ']'
                    }
                    break;
                }
                class_name.push(ch);
                iter.next();
            }

            match class_name.as_str() {
                "alpha" => chars.extend('a'..='z'),
                "upper" => chars.extend('A'..='Z'),
                "lower" => chars.extend('a'..='z'),
                "digit" => chars.extend('0'..='9'),
                "alnum" => {
                    chars.extend('a'..='z');
                    chars.extend('A'..='Z');
                    chars.extend('0'..='9');
                }
                "space" => chars.extend([' ', '\t', '\n', '\r', '\x0b', '\x0c']),
                "blank" => chars.extend([' ', '\t']),
                _ => {} // Unknown class, ignore
            }
        } else if iter.peek() == Some(&'-') {
            // Range like a-z
            iter.next(); // consume '-'
            if let Some(&end) = iter.peek() {
                iter.next(); // consume end char
                let start = c as u32;
                let end = end as u32;
                if start <= end {
                    for code in start..=end {
                        if let Some(ch) = char::from_u32(code) {
                            chars.push(ch);
                        }
                    }
                }
            } else {
                chars.push(c);
                chars.push('-');
            }
        } else {
            chars.push(c);
        }
    }

    chars
}

/// Translate a character using set1 -> set2 mapping.
fn translate_char(c: char, set1: &[char], set2: &[char]) -> char {
    if let Some(pos) = set1.iter().position(|&x| x == c) {
        // Use corresponding char from set2, or last char if set2 is shorter
        *set2.get(pos).or_else(|| set2.last()).unwrap_or(&c)
    } else {
        c
    }
}

/// Squeeze repeated characters from a set.
fn squeeze_chars(input: &str, squeeze_set: &[char]) -> String {
    let mut result = String::new();
    let mut prev: Option<char> = None;

    for c in input.chars() {
        let should_squeeze = squeeze_set.contains(&c) && prev == Some(c);
        if !should_squeeze {
            result.push(c);
        }
        prev = Some(c);
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_tr_basic_translate() {
        let mut ctx = make_ctx();
        ctx.set_stdin("hello".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("aeiou".into()));
        args.positional.push(Value::String("12345".into()));

        let result = Tr.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "h2ll4");
    }

    #[tokio::test]
    async fn test_tr_lowercase_to_uppercase() {
        let mut ctx = make_ctx();
        ctx.set_stdin("hello world".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("a-z".into()));
        args.positional.push(Value::String("A-Z".into()));

        let result = Tr.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "HELLO WORLD");
    }

    #[tokio::test]
    async fn test_tr_delete() {
        let mut ctx = make_ctx();
        ctx.set_stdin("hello world".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("aeiou".into()));
        args.flags.insert("d".to_string());

        let result = Tr.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "hll wrld");
    }

    #[tokio::test]
    async fn test_tr_squeeze() {
        let mut ctx = make_ctx();
        ctx.set_stdin("heeello   woooorld".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("eo ".into()));
        args.positional.push(Value::String("eo ".into()));
        args.flags.insert("s".to_string());

        let result = Tr.execute(args, &mut ctx).await;
        assert!(result.ok());
        // After translate (eo -> eo, identity), squeeze removes consecutive chars in set2
        assert_eq!(result.out, "hello world");
    }

    #[tokio::test]
    async fn test_tr_char_class_digit() {
        let mut ctx = make_ctx();
        ctx.set_stdin("abc123def456".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("[:digit:]".into()));
        args.flags.insert("d".to_string());

        let result = Tr.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "abcdef");
    }

    #[tokio::test]
    async fn test_tr_missing_set1() {
        let mut ctx = make_ctx();
        ctx.set_stdin("hello".to_string());

        let args = ToolArgs::new();
        let result = Tr.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("SET1"));
    }

    #[tokio::test]
    async fn test_tr_missing_set2_without_delete() {
        let mut ctx = make_ctx();
        ctx.set_stdin("hello".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("abc".into()));

        let result = Tr.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("SET2"));
    }

    #[test]
    fn test_expand_char_set() {
        assert_eq!(expand_char_set("abc"), vec!['a', 'b', 'c']);
        assert_eq!(expand_char_set("a-c"), vec!['a', 'b', 'c']);
        assert_eq!(expand_char_set("0-2"), vec!['0', '1', '2']);
        assert!(expand_char_set("[:digit:]").len() == 10);
        assert!(expand_char_set("[:alpha:]").contains(&'m'));
    }

    // --- Additional tests for common patterns ---

    #[tokio::test]
    async fn test_tr_delete_and_squeeze() {
        // tr -ds (delete + squeeze - common for cleanup)
        let mut ctx = make_ctx();
        ctx.set_stdin("hello   world!!!".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("!".into()));
        args.flags.insert("d".to_string());

        let result = Tr.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "hello   world");
    }

    #[tokio::test]
    async fn test_tr_newline_to_space() {
        // Common pattern: tr '\n' ' '
        let mut ctx = make_ctx();
        ctx.set_stdin("line1\nline2\nline3".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("\n".into()));
        args.positional.push(Value::String(" ".into()));

        let result = Tr.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "line1 line2 line3");
    }

    #[tokio::test]
    async fn test_tr_delete_non_printable() {
        // Delete control characters except newline
        let mut ctx = make_ctx();
        ctx.set_stdin("hello\x00\x01world\n".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("\x00\x01".into()));
        args.flags.insert("d".to_string());

        let result = Tr.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "helloworld\n");
    }

    #[tokio::test]
    async fn test_tr_squeeze_spaces() {
        // tr -s ' ' (squeeze multiple spaces)
        let mut ctx = make_ctx();
        ctx.set_stdin("hello     world".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String(" ".into()));
        args.positional.push(Value::String(" ".into()));
        args.flags.insert("s".to_string());

        let result = Tr.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "hello world");
    }

    #[tokio::test]
    async fn test_tr_rot13() {
        // ROT13 encoding
        let mut ctx = make_ctx();
        ctx.set_stdin("hello".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("a-zA-Z".into()));
        args.positional.push(Value::String("n-za-mN-ZA-M".into()));

        let result = Tr.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "uryyb");
    }

    #[tokio::test]
    async fn test_tr_delete_digits() {
        let mut ctx = make_ctx();
        ctx.set_stdin("abc123def456".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("0-9".into()));
        args.flags.insert("d".to_string());

        let result = Tr.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "abcdef");
    }

    #[tokio::test]
    async fn test_tr_empty_input() {
        let mut ctx = make_ctx();
        ctx.set_stdin("".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("a-z".into()));
        args.positional.push(Value::String("A-Z".into()));

        let result = Tr.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.is_empty());
    }

    #[tokio::test]
    async fn test_tr_no_matches() {
        let mut ctx = make_ctx();
        ctx.set_stdin("hello world".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("xyz".into()));
        args.positional.push(Value::String("XYZ".into()));

        let result = Tr.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "hello world");
    }

    #[tokio::test]
    async fn test_tr_char_class_space() {
        // Delete whitespace
        let mut ctx = make_ctx();
        ctx.set_stdin("hello\t world\n".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("[:space:]".into()));
        args.flags.insert("d".to_string());

        let result = Tr.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "helloworld");
    }
}
