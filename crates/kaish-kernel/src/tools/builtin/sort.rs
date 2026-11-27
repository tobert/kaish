//! sort — Sort lines of text.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::cmp::Ordering;
use std::path::Path;

use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{exec_context, schema_from_clap, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

// `Value` is used only in the test module (positional fixtures); import it
// there to avoid a dead-code warning in non-test builds.
#[cfg(test)]
use crate::ast::Value;

/// Sort tool: sort lines of text files or stdin.
pub struct Sort;

/// clap-derived argv layer for sort.
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

    /// Sort by field key spec, 1-indexed, repeatable (-k)
    #[arg(short = 'k', long = "key")]
    key: Vec<String>,

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
                // Comma is significant only inside a `[...]`/`{...}` literal
                // or pattern; a `from,to` key range needs no quoting.
                ("Sort by a key range", "sort -k 2,2 data.txt"),
                ("Unique lines only", "sort -u file.txt"),
            ],
        )
            .with_effect(crate::tools::ToolEffect::ReadOnly)
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let ctx = exec_context(ctx);
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("sort: {e}")),
        };
        let parsed = match SortArgs::try_parse_from(
            std::iter::once("sort".to_string()).chain(argv),
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
            let paths = match ctx.positional_paths(&args) {
                Ok(p) => p,
                Err(e) => return ExecResult::failure(1, format!("sort: {e}")),
            };
            let mut acc = String::new();
            for path in paths {
                let resolved = ctx.resolve_path(&path);
                match ctx
                    .backend
                    .read_with_purpose(Path::new(&resolved), None, crate::backend::ReadPurpose::Content)
                    .await
                {
                    Ok(data) => match String::from_utf8(data) {
                        Ok(s) => {
                            if !acc.is_empty() && !acc.ends_with('\n') && !s.is_empty() {
                                acc.push('\n');
                            }
                            acc.push_str(&s);
                        }
                        Err(_) => return ExecResult::failure(2, format!("sort: {}: invalid UTF-8", path)),
                    },
                    Err(e) => return ExecResult::failure(2, format!("sort: {}: {}", path, e)),
                }
            }
            acc
        };

        let numeric = parsed.numeric;
        let version = parsed.version;
        let reverse = parsed.reverse;
        let unique = parsed.unique;

        // Read key specs from the parsed clap struct (parsed.key), not the raw
        // ToolArgs map. The raw map cannot be trusted: glued flag values like
        // `-k2n` land with the correct value ("2n") in the clap struct.
        let mut key_specs: Vec<KeySpec> = Vec::with_capacity(parsed.key.len());
        for s in &parsed.key {
            match KeySpec::parse(s) {
                Ok(ks) => key_specs.push(ks),
                Err(e) => {
                    return ExecResult::failure(2, format!("sort: invalid key spec {s:?}: {e}"))
                }
            }
        }

        let delimiter = parsed.delimiter.clone();

        // `Vec::sort_by` below is one long synchronous call with no yield
        // point of its own, so once it starts a script timeout cannot stop
        // it. Collecting via a checkpointed loop rather than `.lines().collect()`
        // means large input large enough to make the sort itself slow is
        // already caught here — before the uninterruptible sort ever starts.
        let mut lines: Vec<&str> = Vec::new();
        for line in input.lines() {
            if ctx.checkpoint().await.is_err() {
                return kaish_tool_api::Interrupted.result("sort");
            }
            lines.push(line);
        }

        // Sort with the appropriate comparator. When `-u` is active, skip the
        // last-resort whole-line tiebreak so Rust's stable sort keeps the first
        // line in input order for equal keys, matching GNU `sort -u`.
        lines.sort_by(|a, b| {
            compare_lines(
                a,
                b,
                numeric,
                version,
                reverse,
                &key_specs,
                delimiter.as_deref(),
                !unique,
            )
        });

        // Handle unique flag: GNU `sort -u` drops lines that compare EQUAL under
        // the ACTIVE keys/comparator without the last-resort whole-line tiebreak.
        if unique {
            let delim = delimiter.as_deref();
            lines.dedup_by(|b, a| {
                // `dedup_by` visits (later=b, earlier=a) pairs; keep `a` (first
                // seen) when the two compare equal.
                compare_lines(a, b, numeric, version, reverse, &key_specs, delim, false)
                    == Ordering::Equal
            });
        }

        if lines.is_empty() {
            ExecResult::success("")
        } else {
            ExecResult::with_output(OutputData::text(format!("{}\n", lines.join("\n"))))
        }
    }
}

/// A parsed GNU-style `-k` key specification.
///
/// Grammar: `start_field[.start_char][modifiers][,stop_field[.stop_char][modifiers]]`
///
/// Fields and character offsets are 1-indexed. When `stop_field` is absent the
/// key extends to the end of the line (GNU default). When `stop_field < start_field`
/// the key is empty, matching GNU `sort`.
///
/// Supported per-key modifiers: `n` (numeric), `V` (version), `r` (reverse),
/// `b` (ignore leading blanks), `f` (fold lowercase to uppercase).
#[derive(Debug, Clone)]
struct KeySpec {
    /// 1-indexed start field.
    start_field: usize,
    /// Optional 1-indexed start character offset within `start_field`.
    start_char: Option<usize>,
    /// 1-indexed stop field, or `None` to mean "to end of line".
    stop_field: Option<usize>,
    /// Optional 1-indexed stop character offset within `stop_field` (0 = end of field).
    stop_char: Option<usize>,
    /// Numeric sort on this key.
    numeric: bool,
    /// Version sort on this key.
    version: bool,
    /// Reverse this key's comparison.
    reverse: bool,
    /// Ignore leading blanks (`b`) on start position.
    start_ignore_blanks: bool,
    /// Ignore leading blanks (`b`) on stop position.
    stop_ignore_blanks: bool,
    /// Case-insensitive ASCII folding (`f`).
    ignore_case: bool,
    /// Whether this key declared any local ordering modifiers (`n`, `V`, `r`, `f`, `b`).
    /// Per GNU sort, a key with local ordering modifiers overrides global `-n`/`-V`/`-r`.
    has_local_modifiers: bool,
}

impl KeySpec {
    /// Parse a key spec string such as `"2"`, `"2n"`, `"2,2"`, `"1.2,1.4"`, `"2nr"`.
    fn parse(s: &str) -> Result<Self, String> {
        let (start_part, stop_part) = match s.split_once(',') {
            Some((start, stop)) => (start, Some(stop)),
            None => (s, None),
        };

        let (start_field, start_char, start_mods) = parse_field_and_modifiers(start_part, false)?;
        let (stop_field, stop_char, stop_mods) = match stop_part {
            Some(p) => {
                let (f, c, m) = parse_field_and_modifiers(p, true)?;
                (Some(f), c, m)
            }
            None => (None, None, ModifierFlags::default()),
        };

        let numeric = start_mods.numeric || stop_mods.numeric;
        let version = start_mods.version || stop_mods.version;
        let reverse = start_mods.reverse || stop_mods.reverse;
        let ignore_case = start_mods.ignore_case || stop_mods.ignore_case;
        let start_ignore_blanks =
            start_mods.ignore_leading_blanks || stop_mods.ignore_leading_blanks;
        let stop_ignore_blanks = start_ignore_blanks;
        let has_local_modifiers = numeric
            || version
            || reverse
            || ignore_case
            || start_ignore_blanks
            || stop_ignore_blanks;

        Ok(KeySpec {
            start_field,
            start_char,
            stop_field,
            stop_char,
            numeric,
            version,
            reverse,
            start_ignore_blanks,
            stop_ignore_blanks,
            ignore_case,
            has_local_modifiers,
        })
    }
}

/// Modifier flags extracted from a field spec.
#[derive(Default)]
struct ModifierFlags {
    numeric: bool,
    version: bool,
    reverse: bool,
    ignore_leading_blanks: bool,
    ignore_case: bool,
}

/// Parse `"F[.C][modifiers]"` into `(field_number, Option<char_offset>, ModifierFlags)`.
fn parse_field_and_modifiers(
    s: &str,
    allow_zero_char: bool,
) -> Result<(usize, Option<usize>, ModifierFlags), String> {
    if s.is_empty() {
        return Err("empty field spec".to_string());
    }

    // Read leading digits (the field number).
    let digit_end = s.bytes().take_while(|b| b.is_ascii_digit()).count();
    if digit_end == 0 {
        return Err(format!("expected field number, got {s:?}"));
    }
    let field: usize = s[..digit_end].parse().map_err(|e| format!("{e}"))?;
    if field == 0 {
        return Err("field numbers are 1-indexed; 0 is not valid".to_string());
    }

    // Parse optional `.C` character offset.
    let rest = &s[digit_end..];
    let (char_offset, rest) = if let Some(after_dot) = rest.strip_prefix('.') {
        let char_digit_end = after_dot.bytes().take_while(|b| b.is_ascii_digit()).count();
        if char_digit_end == 0 {
            return Err(format!("expected character offset after '.', got {after_dot:?}"));
        }
        let offset: usize = after_dot[..char_digit_end]
            .parse()
            .map_err(|e| format!("{e}"))?;
        if offset == 0 && !allow_zero_char {
            return Err("character offset is 1-indexed; 0 is not valid on start field".to_string());
        }
        (Some(offset), &after_dot[char_digit_end..])
    } else {
        (None, rest)
    };

    let mut flags = ModifierFlags::default();
    for ch in rest.chars() {
        match ch {
            'n' => flags.numeric = true,
            'V' => flags.version = true,
            'r' => flags.reverse = true,
            'b' => flags.ignore_leading_blanks = true,
            'f' => flags.ignore_case = true,
            // d, i, M, h, R — accepted without error for compatibility.
            _ => {}
        }
    }

    Ok((field, char_offset, flags))
}

/// Byte offset after advancing `char_count` Unicode characters inside `s`.
fn char_offset_to_byte(s: &str, char_count: usize) -> usize {
    if char_count == 0 {
        return 0;
    }
    s.char_indices()
        .nth(char_count)
        .map(|(byte_idx, _)| byte_idx)
        .unwrap_or(s.len())
}

/// Extract the comparison key from `line` according to `spec` and `delimiter`.
fn extract_key<'a>(line: &'a str, spec: &KeySpec, delimiter: Option<&str>) -> &'a str {
    if let Some(stop_f) = spec.stop_field
        && stop_f < spec.start_field
    {
        return "";
    }

    let fields = split_fields(line, delimiter);
    let start = spec.start_field.saturating_sub(1); // 0-indexed
    if fields.is_empty() || start >= fields.len() {
        return "";
    }

    let line_start = line.as_ptr() as usize;
    let start_field_slice = fields[start];
    let start_field_trimmed = if spec.start_ignore_blanks {
        start_field_slice.trim_start_matches([' ', '\t'])
    } else {
        start_field_slice
    };
    let start_char_skip = spec.start_char.map_or(0, |c| c.saturating_sub(1));
    let start_within = char_offset_to_byte(start_field_trimmed, start_char_skip);
    let byte_start =
        (start_field_trimmed.as_ptr() as usize - line_start) + start_within;

    let byte_end = match spec.stop_field {
        None => line.len(),
        Some(stop_f) => {
            let stop = stop_f.saturating_sub(1);
            if stop >= fields.len() {
                line.len()
            } else {
                let stop_field_slice = fields[stop];
                let stop_field_base = if spec.stop_ignore_blanks {
                    stop_field_slice.trim_start_matches([' ', '\t'])
                } else {
                    stop_field_slice
                };
                match spec.stop_char {
                    Some(c) if c > 0 => {
                        let end_within = char_offset_to_byte(stop_field_base, c);
                        (stop_field_base.as_ptr() as usize - line_start) + end_within
                    }
                    _ => (stop_field_slice.as_ptr() as usize - line_start) + stop_field_slice.len(),
                }
            }
        }
    };

    if byte_start >= byte_end || byte_end > line.len() {
        return "";
    }
    &line[byte_start..byte_end]
}

/// Split `line` into fields using the given delimiter.
///
/// With no delimiter, splits on runs of whitespace (GNU sort default).
/// With a single-char delimiter, splits on every occurrence (even empty fields).
fn split_fields<'a>(line: &'a str, delimiter: Option<&str>) -> Vec<&'a str> {
    match delimiter {
        None => line.split_ascii_whitespace().collect(),
        Some(d) if d.len() == 1 => {
            // The match guard ensures d is a non-empty single-char string.
            if let Some(ch) = d.chars().next() {
                line.split(ch).collect()
            } else {
                vec![line]
            }
        }
        Some(d) => line.split(d).collect(),
    }
}

/// Compare two strings with ASCII case folding (`f` modifier).
fn cmp_ignore_ascii_case(a: &str, b: &str) -> Ordering {
    for (ca, cb) in a.bytes().zip(b.bytes()) {
        let ord = ca.to_ascii_uppercase().cmp(&cb.to_ascii_uppercase());
        if ord != Ordering::Equal {
            return ord;
        }
    }
    a.len().cmp(&b.len())
}

/// Compare two lines for sorting across zero or more `-k` key specs.
#[allow(clippy::too_many_arguments)]
fn compare_lines(
    a: &str,
    b: &str,
    numeric: bool,
    version: bool,
    reverse: bool,
    key_specs: &[KeySpec],
    delimiter: Option<&str>,
    tiebreak_whole_line: bool,
) -> Ordering {
    if key_specs.is_empty() {
        let cmp = if version {
            compare_version(a, b)
        } else if numeric {
            let num_a = extract_leading_number(a);
            let num_b = extract_leading_number(b);
            num_a.partial_cmp(&num_b).unwrap_or(Ordering::Equal)
        } else {
            a.cmp(b)
        };
        return if reverse { cmp.reverse() } else { cmp };
    }

    for ks in key_specs {
        let val_a = extract_key(a, ks, delimiter);
        let val_b = extract_key(b, ks, delimiter);

        let use_numeric = if ks.has_local_modifiers {
            ks.numeric
        } else {
            numeric
        };
        let use_version = if ks.has_local_modifiers {
            ks.version
        } else {
            version
        };
        let use_reverse = if ks.has_local_modifiers {
            ks.reverse
        } else {
            reverse
        };

        let cmp = if use_version {
            compare_version(val_a, val_b)
        } else if use_numeric {
            let num_a = extract_leading_number(val_a);
            let num_b = extract_leading_number(val_b);
            num_a.partial_cmp(&num_b).unwrap_or(Ordering::Equal)
        } else if ks.ignore_case {
            cmp_ignore_ascii_case(val_a, val_b)
        } else {
            val_a.cmp(val_b)
        };

        let cmp = if use_reverse { cmp.reverse() } else { cmp };
        if cmp != Ordering::Equal {
            return cmp;
        }
    }

    if tiebreak_whole_line {
        let tb = a.cmp(b);
        if reverse { tb.reverse() } else { tb }
    } else {
        Ordering::Equal
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
    use crate::tools::ExecContext;
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
