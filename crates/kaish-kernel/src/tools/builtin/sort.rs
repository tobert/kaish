//! sort — Sort lines of text.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::cmp::Ordering;
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Sort tool: sort lines of text files or stdin.
pub struct Sort;

/// clap-derived argv layer for sort. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "sort", about = "Sort lines of text")]
struct SortArgs {
    /// Sort numerically (-n)
    #[arg(short = 'n', long = "numeric")]
    numeric: bool,

    /// Sort by version (natural numeric runs), e.g. v1.2 < v1.10 (-V)
    #[arg(short = 'V', long = "version-sort")]
    version: bool,

    /// Reverse the sort order (-r)
    #[arg(short = 'r', long = "reverse")]
    reverse: bool,

    /// Output only unique lines (-u)
    #[arg(short = 'u', long = "unique")]
    unique: bool,

    /// Sort by field number, 1-indexed (-k)
    #[arg(short = 'k', long = "key")]
    key: Option<String>,

    /// Field delimiter (-t)
    #[arg(short = 't', long = "delimiter")]
    delimiter: Option<String>,

    #[command(flatten)]
    global: GlobalFlags,

    /// Files to sort; reads stdin when none are given.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Sort {
    fn name(&self) -> &str {
        "sort"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &SortArgs::command(),
            "sort",
            "Sort lines of text",
            [
                ("Alphabetical sort", "sort names.txt"),
                ("Numeric sort", "sort -n numbers.txt"),
                ("Version sort (v1.2 < v1.10)", "sort -V tags.txt"),
                ("Reverse sort", "sort -r file.txt"),
                ("Sort by second field", "sort -k 2 data.txt"),
                // A `from,to` key range must be quoted — an unquoted comma splits.
                ("Sort by a key range", "sort -k \"2,2\" data.txt"),
                ("Unique lines only", "sort -u file.txt"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let parsed = match SortArgs::try_parse_from(
            std::iter::once("sort".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("sort: {e}")),
        };
        parsed.global.apply(ctx);

        // POSIX: sort accepts multiple files and merge-sorts their contents.
        // When no file is given, read stdin.
        let input = if args.positional.is_empty() {
            match ctx.read_stdin_to_text().await {
                Ok(s) => s.unwrap_or_default(),
                Err(e) => return ExecResult::failure(2, format!("sort: {e}")),
            }
        } else {
            let mut acc = String::new();
            for value in &args.positional {
                let path = crate::interpreter::value_to_string(value);
                let resolved = ctx.resolve_path(&path);
                match ctx.backend.read(Path::new(&resolved), None).await {
                    Ok(data) => match String::from_utf8(data) {
                        Ok(s) => acc.push_str(&s),
                        Err(_) => return ExecResult::failure(1, format!("sort: {}: invalid UTF-8", path)),
                    },
                    Err(e) => return ExecResult::failure(1, format!("sort: {}: {}", path, e)),
                }
            }
            acc
        };

        let numeric = parsed.numeric;
        let version = parsed.version;
        let reverse = parsed.reverse;
        let unique = parsed.unique;

        let key_field = args.get("key", usize::MAX).and_then(|v| match v {
            Value::Int(i) => Some(*i as usize),
            Value::String(s) => s.parse().ok(),
            _ => None,
        });

        let delimiter = parsed.delimiter.clone()
            .or_else(|| args.get_string("delimiter", usize::MAX))
            .or_else(|| args.get_string("t", usize::MAX));

        let mut lines: Vec<&str> = input.lines().collect();

        // Sort with the appropriate comparator
        lines.sort_by(|a, b| {
            let cmp = compare_lines(a, b, numeric, version, key_field, delimiter.as_deref());
            if reverse {
                cmp.reverse()
            } else {
                cmp
            }
        });

        // Handle unique flag
        if unique {
            lines.dedup();
        }

        if lines.is_empty() {
            ExecResult::success("")
        } else {
            ExecResult::with_output(OutputData::text(format!("{}\n", lines.join("\n"))))
        }
    }
}

/// Compare two lines for sorting.
fn compare_lines(
    a: &str,
    b: &str,
    numeric: bool,
    version: bool,
    key_field: Option<usize>,
    delimiter: Option<&str>,
) -> Ordering {
    let (val_a, val_b) = match key_field {
        Some(k) if k > 0 => {
            let delim = delimiter.unwrap_or(" \t");
            let delim_char = delim.chars().next();
            let split_a: Vec<&str> = match delim_char {
                Some(ch) if delim.len() == 1 => a.split(ch).collect(),
                _ => a.split_whitespace().collect(),
            };
            let split_b: Vec<&str> = match delim_char {
                Some(ch) if delim.len() == 1 => b.split(ch).collect(),
                _ => b.split_whitespace().collect(),
            };

            let field_a = split_a.get(k - 1).copied().unwrap_or("");
            let field_b = split_b.get(k - 1).copied().unwrap_or("");
            (field_a, field_b)
        }
        _ => (a, b),
    };

    if version {
        compare_version(val_a, val_b)
    } else if numeric {
        let num_a = extract_leading_number(val_a);
        let num_b = extract_leading_number(val_b);
        num_a.partial_cmp(&num_b).unwrap_or(Ordering::Equal)
    } else {
        val_a.cmp(val_b)
    }
}

/// Compare two strings as versions (GNU `sort -V` family). The strings are
/// walked in lock-step as alternating runs of digits and non-digits: digit runs
/// are compared by numeric value (leading zeros ignored, then longer wins),
/// non-digit runs byte-by-byte. This is the common-case subset that orders
/// version tags correctly (`v1.2 < v1.9 < v1.10`); it does not implement every
/// `filevercmp` tie-break rule.
fn compare_version(a: &str, b: &str) -> Ordering {
    let (a, b) = (a.as_bytes(), b.as_bytes());
    let (mut i, mut j) = (0, 0);
    while i < a.len() && j < b.len() {
        let (da, db) = (a[i].is_ascii_digit(), b[j].is_ascii_digit());
        if da && db {
            let ni = i + a[i..].iter().take_while(|c| c.is_ascii_digit()).count();
            let nj = j + b[j..].iter().take_while(|c| c.is_ascii_digit()).count();
            let cmp = compare_numeric_run(&a[i..ni], &b[j..nj]);
            if cmp != Ordering::Equal {
                return cmp;
            }
            i = ni;
            j = nj;
        } else if !da && !db {
            let cmp = a[i].cmp(&b[j]);
            if cmp != Ordering::Equal {
                return cmp;
            }
            i += 1;
            j += 1;
        } else {
            // One side is a digit, the other isn't: fall back to a byte compare.
            return a[i].cmp(&b[j]);
        }
    }
    // The longer string sorts after when one is a prefix of the other.
    a.len().cmp(&b.len())
}

/// Compare two runs of ASCII digits by numeric value: ignore leading zeros,
/// then the longer run is larger, then lexically (handles equal-length runs).
fn compare_numeric_run(a: &[u8], b: &[u8]) -> Ordering {
    let a = strip_leading_zeros(a);
    let b = strip_leading_zeros(b);
    a.len().cmp(&b.len()).then_with(|| a.cmp(b))
}

fn strip_leading_zeros(s: &[u8]) -> &[u8] {
    let nz = s.iter().take_while(|&&c| c == b'0').count();
    // Keep one digit if the run is all zeros.
    &s[nz.min(s.len().saturating_sub(1))..]
}

/// Extract the leading numeric prefix from a string, like GNU sort -n.
/// "33 foo" → 33.0, "  -12.5xyz" → -12.5, "abc" → 0.0
fn extract_leading_number(s: &str) -> f64 {
    let s = s.trim_start();
    if s.is_empty() {
        return 0.0;
    }

    let mut end = 0;
    let chars: Vec<char> = s.chars().collect();

    // Optional sign
    if end < chars.len() && (chars[end] == '-' || chars[end] == '+') {
        end += 1;
    }

    // Digits before decimal
    while end < chars.len() && chars[end].is_ascii_digit() {
        end += 1;
    }

    // Optional decimal point + digits
    if end < chars.len() && chars[end] == '.' {
        end += 1;
        while end < chars.len() && chars[end].is_ascii_digit() {
            end += 1;
        }
    }

    if end == 0 || (end == 1 && (chars[0] == '-' || chars[0] == '+' || chars[0] == '.')) {
        return 0.0;
    }

    let num_str: String = chars[..end].iter().collect();
    num_str.parse().unwrap_or(0.0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("alpha.txt"), b"banana\napple\ncherry\napple")
            .await
            .unwrap();
        mem.write(Path::new("numbers.txt"), b"10\n2\n1\n20\n3")
            .await
            .unwrap();
        mem.write(Path::new("fields.txt"), b"bob 30\nalice 25\ncharlie 35")
            .await
            .unwrap();
        mem.write(Path::new("csv.txt"), b"bob,30\nalice,25\ncharlie,35")
            .await
            .unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_sort_alphabetical() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/alpha.txt".into()));

        let result = Sort.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines[0], "apple");
        assert_eq!(lines[1], "apple");
        assert_eq!(lines[2], "banana");
        assert_eq!(lines[3], "cherry");
    }

    #[tokio::test]
    async fn test_sort_reverse() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/alpha.txt".into()));
        args.flags.insert("r".to_string());

        let result = Sort.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines[0], "cherry");
        assert_eq!(lines[3], "apple");
    }

    #[tokio::test]
    async fn test_sort_numeric() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/numbers.txt".into()));
        args.flags.insert("n".to_string());

        let result = Sort.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["1", "2", "3", "10", "20"]);
    }

    #[tokio::test]
    async fn test_sort_unique() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/alpha.txt".into()));
        args.flags.insert("u".to_string());

        let result = Sort.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines.len(), 3); // apple appears only once
    }

    #[tokio::test]
    async fn test_sort_by_key() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/fields.txt".into()));
        args.named.insert("key".to_string(), Value::Int(2));
        args.flags.insert("n".to_string());

        let result = Sort.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines[0], "alice 25");
        assert_eq!(lines[1], "bob 30");
        assert_eq!(lines[2], "charlie 35");
    }

    #[tokio::test]
    async fn test_sort_with_delimiter() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/csv.txt".into()));
        args.named.insert("key".to_string(), Value::Int(2));
        args.named
            .insert("delimiter".to_string(), Value::String(",".into()));
        args.flags.insert("n".to_string());

        let result = Sort.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines[0], "alice,25");
        assert_eq!(lines[2], "charlie,35");
    }

    #[tokio::test]
    async fn test_sort_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("zebra\naardvark\nmouse\n".to_string());

        let args = ToolArgs::new();
        let result = Sort.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines[0], "aardvark");
        assert_eq!(lines[2], "zebra");
    }

    // --- Flag combinations and edge cases ---

    #[tokio::test]
    async fn test_sort_numeric_reverse() {
        // Common pattern: sort -nr
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/numbers.txt".into()));
        args.flags.insert("n".to_string());
        args.flags.insert("r".to_string());

        let result = Sort.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["20", "10", "3", "2", "1"]);
    }

    #[tokio::test]
    async fn test_sort_key_numeric_reverse() {
        // sort -k2 -nr (sort by second field, numeric, reverse)
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/fields.txt".into()));
        args.named.insert("key".to_string(), Value::Int(2));
        args.flags.insert("n".to_string());
        args.flags.insert("r".to_string());

        let result = Sort.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines[0], "charlie 35");
        assert_eq!(lines[2], "alice 25");
    }

    #[tokio::test]
    async fn test_sort_unique_numeric() {
        // sort -nu (unique numeric)
        let mut ctx = make_ctx().await;
        ctx.set_stdin("5\n3\n5\n1\n3\n".to_string());

        let mut args = ToolArgs::new();
        args.flags.insert("n".to_string());
        args.flags.insert("u".to_string());

        let result = Sort.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["1", "3", "5"]);
    }

    #[tokio::test]
    async fn test_sort_unicode() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("日本\nアメリカ\n中国\n".to_string());

        let args = ToolArgs::new();
        let result = Sort.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Just verify it doesn't crash and produces output
        assert_eq!(result.text_out().lines().count(), 3);
    }

    #[tokio::test]
    async fn test_sort_empty_lines() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("b\n\na\n\nc\n".to_string());

        let args = ToolArgs::new();
        let result = Sort.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        // Empty strings sort before non-empty
        assert_eq!(lines[0], "");
        assert_eq!(lines[1], "");
    }

    #[tokio::test]
    async fn test_sort_numeric_with_non_numbers() {
        // Numeric sort with non-numeric entries (should treat as 0)
        let mut ctx = make_ctx().await;
        ctx.set_stdin("10\nabc\n5\nxyz\n1\n".to_string());

        let mut args = ToolArgs::new();
        args.flags.insert("n".to_string());

        let result = Sort.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        // Non-numeric values parse as 0, so abc and xyz come first
        assert!(lines[0] == "abc" || lines[0] == "xyz");
        assert_eq!(lines[4], "10");
    }

    #[tokio::test]
    async fn test_sort_empty_input() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let args = ToolArgs::new();
        let result = Sort.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().is_empty());
    }

    #[tokio::test]
    async fn test_sort_numeric_with_suffix() {
        // Bug 2: sort -n should extract leading numeric prefix, not parse whole line
        let mut ctx = make_ctx().await;
        ctx.set_stdin("33 foo\n11 bar\n22 baz\n".to_string());
        let mut args = ToolArgs::new();
        args.flags.insert("n".to_string());
        let result = Sort.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["11 bar", "22 baz", "33 foo"]);
    }

    #[tokio::test]
    async fn test_sort_numeric_with_suffix_reverse() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("33 a\n89 b\n24 c\n".to_string());
        let mut args = ToolArgs::new();
        args.flags.insert("n".to_string());
        args.flags.insert("r".to_string());
        let result = Sort.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["89 b", "33 a", "24 c"]);
    }

    #[tokio::test]
    async fn test_sort_single_line() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("only one line".to_string());

        let args = ToolArgs::new();
        let result = Sort.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "only one line");
    }
}
