//! sed — Stream editor for filtering and transforming text.
//!
//! A Bourne-lite sed implementation focused on the 80% use case.
//! Uses ERE (extended regex) syntax like egrep, not BRE.

use async_trait::async_trait;
use regex::{Regex, RegexBuilder};
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Sed tool: stream editor for text transformations.
pub struct Sed;

#[async_trait]
impl Tool for Sed {
    fn name(&self) -> &str {
        "sed"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("sed", "Stream editor for filtering and transforming text")
            .param(ParamSchema::optional(
                "expression",
                "string",
                Value::Null,
                "Sed expression to execute (-e)",
            ))
            .param(ParamSchema::optional(
                "path",
                "string",
                Value::Null,
                "File to process (reads stdin if not provided)",
            ))
            .param(ParamSchema::optional(
                "quiet",
                "bool",
                Value::Bool(false),
                "Suppress automatic printing (-n)",
            ))
            .example("Basic substitution", "sed 's/old/new/' file.txt")
            .example("Global substitution", "sed 's/old/new/g' file.txt")
            .example("Case-insensitive", "sed 's/hello/hi/gi' file.txt")
            .example("Delete lines matching pattern", "sed '/error/d' log.txt")
            .example("Print only matching lines", "sed -n '/pattern/p' file.txt")
            .example("Multiple expressions", "sed -e 's/a/b/' -e 's/c/d/' file.txt")
            .example("Line range", "sed '2,5d' file.txt")
            .example("Alternative delimiter", "sed 's|/usr|/opt|g' file.txt")
            .example("Capture groups", "sed 's/(\\w+) (\\w+)/\\2 \\1/' file.txt")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let quiet = args.has_flag("quiet") || args.has_flag("n");

        // Collect expressions: from -e flags or first positional
        let expressions = collect_expressions(&args);
        if expressions.is_empty() {
            return ExecResult::failure(1, "sed: missing expression");
        }

        // Parse all expressions upfront (fail early)
        let parsed: Result<Vec<SedExpression>, String> = expressions
            .iter()
            .map(|expr| parse_expression(expr))
            .collect();

        let parsed = match parsed {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(1, format!("sed: {}", e)),
        };

        // Get input: file or stdin
        // Expression is at position 0, file at position 1 (unless using -e)
        let file_pos = if args.flags.contains("e") || args.flags.contains("expression") {
            0
        } else {
            1
        };

        let input = match args.get_string("path", file_pos) {
            Some(path) => {
                let resolved = ctx.resolve_path(&path);
                match ctx.backend.read(Path::new(&resolved), None).await {
                    Ok(data) => match String::from_utf8(data) {
                        Ok(s) => s,
                        Err(_) => {
                            return ExecResult::failure(1, format!("sed: {}: invalid UTF-8", path))
                        }
                    },
                    Err(e) => return ExecResult::failure(1, format!("sed: {}: {}", path, e)),
                }
            }
            None => ctx.take_stdin().unwrap_or_default(),
        };

        // Execute
        let output = execute_sed(&input, &parsed, quiet);
        ExecResult::success(output)
    }
}

/// Collect all expressions from args (supports multiple -e flags).
fn collect_expressions(args: &ToolArgs) -> Vec<String> {
    let mut exprs = Vec::new();

    // Check for -e/--expression flag with values
    if let Some(Value::String(e)) = args.named.get("e") {
        exprs.push(e.clone());
    }
    if let Some(Value::String(e)) = args.named.get("expression") {
        exprs.push(e.clone());
    }

    // First positional is expression if no -e flag was used
    if exprs.is_empty() {
        if let Some(Value::String(e)) = args.positional.first() {
            exprs.push(e.clone());
        }
    }

    exprs
}

// ============================================================================
// Data Structures
// ============================================================================

/// Address specifies which lines a command applies to.
#[derive(Debug, Clone)]
enum Address {
    /// Apply to all lines (no address specified).
    All,
    /// Single line number (1-indexed).
    Line(usize),
    /// Last line of input.
    LastLine,
    /// Range from start to end (inclusive).
    Range(Box<Address>, Box<Address>),
    /// Lines matching a regex pattern.
    Pattern(Regex),
}

/// A sed command to execute.
#[derive(Debug, Clone)]
enum Command {
    /// Substitute pattern with replacement.
    Substitute {
        pattern: Regex,
        replacement: String,
        global: bool,
        print: bool,
    },
    /// Delete the pattern space.
    Delete,
    /// Print the pattern space.
    Print,
    /// Quit processing.
    Quit,
}

/// A complete sed expression: address + command.
#[derive(Debug, Clone)]
struct SedExpression {
    address: Address,
    command: Command,
}

// ============================================================================
// Parser
// ============================================================================

/// Parse a complete sed expression (address + command).
fn parse_expression(expr: &str) -> Result<SedExpression, String> {
    let expr = expr.trim();
    if expr.is_empty() {
        return Err("empty expression".to_string());
    }

    let (address, rest) = parse_address(expr)?;
    let command = parse_command(rest.trim())?;

    Ok(SedExpression { address, command })
}

/// Parse an optional address prefix, returning (Address, remaining).
fn parse_address(expr: &str) -> Result<(Address, &str), String> {
    let expr = expr.trim();

    if expr.is_empty() {
        return Ok((Address::All, ""));
    }

    // Check for pattern address: /regex/
    if expr.starts_with('/') {
        let (pattern, rest) = parse_pattern_address(expr)?;
        // Check for range
        if rest.starts_with(',') {
            let (end_addr, final_rest) = parse_address(&rest[1..])?;
            return Ok((
                Address::Range(Box::new(Address::Pattern(pattern)), Box::new(end_addr)),
                final_rest,
            ));
        }
        return Ok((Address::Pattern(pattern), rest));
    }

    // Check for $ (last line)
    if expr.starts_with('$') {
        let rest = &expr[1..];
        if rest.starts_with(',') {
            let (end_addr, final_rest) = parse_address(&rest[1..])?;
            return Ok((
                Address::Range(Box::new(Address::LastLine), Box::new(end_addr)),
                final_rest,
            ));
        }
        return Ok((Address::LastLine, rest));
    }

    // Check for line number
    if expr.chars().next().is_some_and(|c| c.is_ascii_digit()) {
        let num_end = expr
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(expr.len());
        let num: usize = expr[..num_end]
            .parse()
            .map_err(|_| "invalid line number")?;
        let rest = &expr[num_end..];

        // Check for range
        if rest.starts_with(',') {
            let (end_addr, final_rest) = parse_address(&rest[1..])?;
            return Ok((
                Address::Range(Box::new(Address::Line(num)), Box::new(end_addr)),
                final_rest,
            ));
        }
        return Ok((Address::Line(num), rest));
    }

    // No address, return all
    Ok((Address::All, expr))
}

/// Parse a /pattern/ address, returning the compiled regex and remaining input.
fn parse_pattern_address(expr: &str) -> Result<(Regex, &str), String> {
    debug_assert!(expr.starts_with('/'));

    let chars: Vec<char> = expr.chars().collect();
    let mut i = 1; // Skip opening /
    let mut pattern = String::new();

    while i < chars.len() {
        let c = chars[i];
        if c == '\\' && i + 1 < chars.len() {
            // Escaped character
            let next = chars[i + 1];
            if next == '/' {
                pattern.push('/');
            } else {
                pattern.push('\\');
                pattern.push(next);
            }
            i += 2;
        } else if c == '/' {
            // End of pattern
            i += 1;
            break;
        } else {
            pattern.push(c);
            i += 1;
        }
    }

    let regex = RegexBuilder::new(&pattern)
        .build()
        .map_err(|e| format!("invalid regex in address: {}", e))?;

    // Calculate byte offset from char offset
    let consumed: usize = chars[..i].iter().map(|c| c.len_utf8()).sum();
    Ok((regex, &expr[consumed..]))
}

/// Parse a sed command (s, d, p, q).
fn parse_command(cmd: &str) -> Result<Command, String> {
    if cmd.is_empty() {
        return Err("missing command".to_string());
    }

    let first = cmd.chars().next().unwrap();

    match first {
        's' => parse_substitute(&cmd[1..]),
        'd' => Ok(Command::Delete),
        'p' => Ok(Command::Print),
        'q' => Ok(Command::Quit),
        _ => Err(format!("unknown command: {}", first)),
    }
}

/// Parse a substitution command: /pattern/replacement/flags
fn parse_substitute(expr: &str) -> Result<Command, String> {
    if expr.is_empty() {
        return Err("s command requires delimiter".to_string());
    }

    let chars: Vec<char> = expr.chars().collect();
    let delimiter = chars[0];

    // Parse pattern
    let (pattern_str, after_pattern) = parse_delimited(&chars[1..], delimiter)?;

    // Parse replacement
    let (replacement, after_replacement) = parse_delimited(after_pattern, delimiter)?;

    // Parse flags
    let flags: String = after_replacement.iter().collect();
    let global = flags.contains('g');
    let case_insensitive = flags.contains('i');
    let print = flags.contains('p');

    // Build regex
    let regex = RegexBuilder::new(&pattern_str)
        .case_insensitive(case_insensitive)
        .build()
        .map_err(|e| format!("invalid pattern: {}", e))?;

    Ok(Command::Substitute {
        pattern: regex,
        replacement,
        global,
        print,
    })
}

/// Parse a delimited section, handling escapes.
fn parse_delimited(chars: &[char], delimiter: char) -> Result<(String, &[char]), String> {
    let mut result = String::new();
    let mut i = 0;

    while i < chars.len() {
        let c = chars[i];
        if c == '\\' && i + 1 < chars.len() {
            let next = chars[i + 1];
            if next == delimiter {
                result.push(delimiter);
                i += 2;
            } else {
                // Keep the backslash for regex or replacement escapes
                result.push('\\');
                result.push(next);
                i += 2;
            }
        } else if c == delimiter {
            return Ok((result, &chars[i + 1..]));
        } else {
            result.push(c);
            i += 1;
        }
    }

    Err("unterminated expression".to_string())
}

// ============================================================================
// Execution
// ============================================================================

/// Execute sed expressions on input text.
fn execute_sed(input: &str, expressions: &[SedExpression], quiet: bool) -> String {
    let lines: Vec<&str> = input.lines().collect();
    let total_lines = lines.len();
    let mut output = String::new();
    let mut range_active: Vec<bool> = vec![false; expressions.len()];

    for (line_num, line) in lines.iter().enumerate() {
        let one_indexed = line_num + 1;
        let is_last = line_num + 1 == total_lines;

        let mut pattern_space = line.to_string();
        let mut deleted = false;
        let mut printed_by_p = false;
        let mut quit = false;

        for (expr_idx, expr) in expressions.iter().enumerate() {
            // Check if address matches
            let matches = address_matches(
                &expr.address,
                one_indexed,
                is_last,
                &pattern_space,
                &mut range_active[expr_idx],
            );

            if !matches {
                continue;
            }

            // Execute command
            match &expr.command {
                Command::Substitute {
                    pattern,
                    replacement,
                    global,
                    print,
                } => {
                    let new_text = if *global {
                        substitute_all(pattern, &pattern_space, replacement)
                    } else {
                        substitute_first(pattern, &pattern_space, replacement)
                    };

                    let changed = new_text != pattern_space;
                    pattern_space = new_text;

                    if *print && changed {
                        output.push_str(&pattern_space);
                        output.push('\n');
                        printed_by_p = true;
                    }
                }
                Command::Delete => {
                    deleted = true;
                    break;
                }
                Command::Print => {
                    output.push_str(&pattern_space);
                    output.push('\n');
                    printed_by_p = true;
                }
                Command::Quit => {
                    quit = true;
                    break;
                }
            }
        }

        // Auto-print unless quiet or deleted
        if !deleted && !quiet && !printed_by_p {
            output.push_str(&pattern_space);
            output.push('\n');
        }

        if quit {
            break;
        }
    }

    output
}

/// Check if an address matches the current line.
fn address_matches(
    addr: &Address,
    line_num: usize,
    is_last: bool,
    pattern_space: &str,
    range_active: &mut bool,
) -> bool {
    match addr {
        Address::All => true,
        Address::Line(n) => line_num == *n,
        Address::LastLine => is_last,
        Address::Pattern(regex) => regex.is_match(pattern_space),
        Address::Range(start, end) => {
            if *range_active {
                // Check if end matches
                let end_matches = match end.as_ref() {
                    Address::Line(n) => line_num >= *n,
                    Address::LastLine => is_last,
                    Address::Pattern(regex) => regex.is_match(pattern_space),
                    _ => false,
                };
                if end_matches {
                    *range_active = false;
                }
                true
            } else {
                // Check if start matches
                let start_matches = match start.as_ref() {
                    Address::Line(n) => line_num == *n,
                    Address::LastLine => is_last,
                    Address::Pattern(regex) => regex.is_match(pattern_space),
                    _ => false,
                };
                if start_matches {
                    *range_active = true;
                    true
                } else {
                    false
                }
            }
        }
    }
}

/// Substitute first match, handling capture groups.
fn substitute_first(pattern: &Regex, text: &str, replacement: &str) -> String {
    if let Some(captures) = pattern.captures(text) {
        let mat = captures.get(0).unwrap();
        let expanded = expand_replacement(replacement, &captures);
        format!("{}{}{}", &text[..mat.start()], expanded, &text[mat.end()..])
    } else {
        text.to_string()
    }
}

/// Substitute all matches, handling capture groups.
fn substitute_all(pattern: &Regex, text: &str, replacement: &str) -> String {
    let mut result = String::new();
    let mut last_end = 0;

    for captures in pattern.captures_iter(text) {
        let mat = captures.get(0).unwrap();
        result.push_str(&text[last_end..mat.start()]);
        result.push_str(&expand_replacement(replacement, &captures));
        last_end = mat.end();
    }

    result.push_str(&text[last_end..]);
    result
}

/// Expand replacement string with capture groups (\1-\9, &).
fn expand_replacement(replacement: &str, captures: &regex::Captures) -> String {
    let mut result = String::new();
    let chars: Vec<char> = replacement.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        let c = chars[i];
        if c == '\\' && i + 1 < chars.len() {
            let next = chars[i + 1];
            if next.is_ascii_digit() {
                let group_num = (next as u8 - b'0') as usize;
                if let Some(m) = captures.get(group_num) {
                    result.push_str(m.as_str());
                }
                i += 2;
            } else {
                // Other escapes: \n, \t, \\
                match next {
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    '\\' => result.push('\\'),
                    _ => {
                        result.push('\\');
                        result.push(next);
                    }
                }
                i += 2;
            }
        } else if c == '&' {
            // & expands to entire match
            if let Some(m) = captures.get(0) {
                result.push_str(m.as_str());
            }
            i += 1;
        } else {
            result.push(c);
            i += 1;
        }
    }

    result
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(
            Path::new("test.txt"),
            b"hello world\nHELLO WORLD\nfoo bar\nbaz",
        )
        .await
        .unwrap();
        mem.write(
            Path::new("lines.txt"),
            b"line one\nline two\nline three\nline four\nline five",
        )
        .await
        .unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    // === Parser Tests ===

    #[test]
    fn test_parse_basic_substitute() {
        let expr = parse_expression("s/foo/bar/").unwrap();
        assert!(matches!(expr.address, Address::All));
        assert!(matches!(expr.command, Command::Substitute { .. }));
    }

    #[test]
    fn test_parse_substitute_flags() {
        let expr = parse_expression("s/foo/bar/gip").unwrap();
        if let Command::Substitute {
            global,
            print,
            pattern,
            ..
        } = &expr.command
        {
            assert!(*global);
            assert!(*print);
            // Case insensitive is baked into the regex
            assert!(pattern.is_match("FOO"));
        } else {
            panic!("expected Substitute");
        }
    }

    #[test]
    fn test_parse_alternative_delimiter() {
        let expr = parse_expression("s|/usr|/opt|").unwrap();
        if let Command::Substitute {
            pattern,
            replacement,
            ..
        } = &expr.command
        {
            assert!(pattern.is_match("/usr"));
            assert_eq!(replacement, "/opt");
        } else {
            panic!("expected Substitute");
        }
    }

    #[test]
    fn test_parse_escaped_delimiter() {
        let expr = parse_expression(r"s/foo\/bar/baz/").unwrap();
        if let Command::Substitute { pattern, .. } = &expr.command {
            assert!(pattern.is_match("foo/bar"));
        } else {
            panic!("expected Substitute");
        }
    }

    #[test]
    fn test_parse_line_address() {
        let expr = parse_expression("5d").unwrap();
        assert!(matches!(expr.address, Address::Line(5)));
        assert!(matches!(expr.command, Command::Delete));
    }

    #[test]
    fn test_parse_last_line_address() {
        let expr = parse_expression("$d").unwrap();
        assert!(matches!(expr.address, Address::LastLine));
    }

    #[test]
    fn test_parse_range_address() {
        let expr = parse_expression("1,5d").unwrap();
        if let Address::Range(start, end) = &expr.address {
            assert!(matches!(start.as_ref(), Address::Line(1)));
            assert!(matches!(end.as_ref(), Address::Line(5)));
        } else {
            panic!("expected Range");
        }
    }

    #[test]
    fn test_parse_range_to_last() {
        let expr = parse_expression("3,$d").unwrap();
        if let Address::Range(start, end) = &expr.address {
            assert!(matches!(start.as_ref(), Address::Line(3)));
            assert!(matches!(end.as_ref(), Address::LastLine));
        } else {
            panic!("expected Range");
        }
    }

    #[test]
    fn test_parse_pattern_address() {
        let expr = parse_expression("/error/d").unwrap();
        assert!(matches!(expr.address, Address::Pattern(_)));
        assert!(matches!(expr.command, Command::Delete));
    }

    #[test]
    fn test_parse_pattern_range() {
        let expr = parse_expression("/start/,/end/d").unwrap();
        if let Address::Range(start, end) = &expr.address {
            assert!(matches!(start.as_ref(), Address::Pattern(_)));
            assert!(matches!(end.as_ref(), Address::Pattern(_)));
        } else {
            panic!("expected Range");
        }
    }

    #[test]
    fn test_parse_delete_command() {
        let expr = parse_expression("d").unwrap();
        assert!(matches!(expr.command, Command::Delete));
    }

    #[test]
    fn test_parse_print_command() {
        let expr = parse_expression("p").unwrap();
        assert!(matches!(expr.command, Command::Print));
    }

    #[test]
    fn test_parse_quit_command() {
        let expr = parse_expression("q").unwrap();
        assert!(matches!(expr.command, Command::Quit));
    }

    #[test]
    fn test_parse_invalid_regex() {
        let result = parse_expression("s/[invalid/bar/");
        assert!(result.is_err());
    }

    // === Execution Tests ===

    #[test]
    fn test_basic_substitution() {
        let input = "hello world\nhello there";
        let expr = parse_expression("s/hello/hi/").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "hi world\nhi there\n");
    }

    #[test]
    fn test_global_substitution() {
        let input = "foo bar foo baz foo";
        let expr = parse_expression("s/foo/XXX/g").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "XXX bar XXX baz XXX\n");
    }

    #[test]
    fn test_case_insensitive() {
        let input = "Hello HELLO hello";
        let expr = parse_expression("s/hello/hi/gi").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "hi hi hi\n");
    }

    #[test]
    fn test_print_on_change() {
        let input = "hello world\nfoo bar";
        let expr = parse_expression("s/hello/hi/p").unwrap();
        let output = execute_sed(input, &[expr], true); // quiet mode
        assert_eq!(output, "hi world\n");
    }

    #[test]
    fn test_capture_groups() {
        let input = "John Smith";
        let expr = parse_expression(r"s/(\w+) (\w+)/\2, \1/").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "Smith, John\n");
    }

    #[test]
    fn test_ampersand_expansion() {
        let input = "hello world";
        let expr = parse_expression("s/hello/[&]/").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "[hello] world\n");
    }

    #[test]
    fn test_delete_command() {
        let input = "keep\ndelete this\nkeep";
        let expr = parse_expression("/delete/d").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "keep\nkeep\n");
    }

    #[test]
    fn test_quiet_mode() {
        let input = "line 1\npattern here\nline 3";
        let expr = parse_expression("/pattern/p").unwrap();
        let output = execute_sed(input, &[expr], true);
        assert_eq!(output, "pattern here\n");
    }

    #[test]
    fn test_line_number_address() {
        let input = "line 1\nline 2\nline 3";
        let expr = parse_expression("2s/line/LINE/").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "line 1\nLINE 2\nline 3\n");
    }

    #[test]
    fn test_range_address() {
        let input = "line 1\nline 2\nline 3\nline 4";
        let expr = parse_expression("2,3d").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "line 1\nline 4\n");
    }

    #[test]
    fn test_quit_command() {
        let input = "line 1\nline 2\nline 3\nline 4";
        let expr = parse_expression("2q").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "line 1\nline 2\n");
    }

    #[test]
    fn test_multiple_expressions() {
        let input = "abc 123";
        let e1 = parse_expression("s/a/X/").unwrap();
        let e2 = parse_expression("s/1/Y/").unwrap();
        let output = execute_sed(input, &[e1, e2], false);
        assert_eq!(output, "Xbc Y23\n");
    }

    #[test]
    fn test_empty_replacement() {
        let input = "hello world";
        let expr = parse_expression("s/hello //").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "world\n");
    }

    #[test]
    fn test_empty_input() {
        let input = "";
        let expr = parse_expression("s/foo/bar/").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert!(output.is_empty());
    }

    #[test]
    fn test_no_matches_passthrough() {
        let input = "hello world";
        let expr = parse_expression("s/xyz/abc/").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "hello world\n");
    }

    #[test]
    fn test_last_line_address() {
        let input = "line 1\nline 2\nline 3";
        let expr = parse_expression("$s/line/LAST/").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "line 1\nline 2\nLAST 3\n");
    }

    #[test]
    fn test_pattern_range() {
        let input = "before\nSTART\nmiddle\nEND\nafter";
        let expr = parse_expression("/START/,/END/d").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "before\nafter\n");
    }

    #[test]
    fn test_escaped_backslash_replacement() {
        let input = "hello";
        let expr = parse_expression(r"s/hello/a\\b/").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "a\\b\n");
    }

    #[test]
    fn test_newline_in_replacement() {
        let input = "hello world";
        let expr = parse_expression(r"s/ /\n/").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "hello\nworld\n");
    }

    // === Integration Tests ===

    #[tokio::test]
    async fn test_sed_from_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hello world".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("s/world/kaish/".into()));

        let result = Sed.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "hi kaish\n".replace("hi", "hello"));
    }

    #[tokio::test]
    async fn test_sed_from_file() {
        let mut ctx = make_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("s/hello/hi/".into()));
        args.positional.push(Value::String("/test.txt".into()));

        let result = Sed.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("hi world"));
    }

    #[tokio::test]
    async fn test_sed_quiet_mode() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("line 1\npattern\nline 3".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/pattern/p".into()));
        args.flags.insert("n".to_string());

        let result = Sed.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "pattern\n");
    }

    #[tokio::test]
    async fn test_sed_missing_expression() {
        let mut ctx = make_ctx().await;
        let args = ToolArgs::new();

        let result = Sed.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("missing expression"));
    }

    #[tokio::test]
    async fn test_sed_invalid_expression() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hello".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("s/[invalid/bar/".into()));

        let result = Sed.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("sed:"));
    }

    #[tokio::test]
    async fn test_sed_file_not_found() {
        let mut ctx = make_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("s/foo/bar/".into()));
        args.positional.push(Value::String("/nonexistent".into()));

        let result = Sed.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_sed_unicode() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("こんにちは 世界".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("s/世界/kaish/".into()));

        let result = Sed.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "こんにちは kaish\n");
    }
}
