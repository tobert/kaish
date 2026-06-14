//! Shared format string parser for printf and awk sprintf.
//!
//! Handles `%[flags][width][.precision]conversion` specifiers.
//! Both `Value` (printf) and `AwkValue` (awk) implement `FormatArg`
//! so the same parser serves both builtins.

/// Trait for values that can be formatted by printf-style specifiers.
pub trait FormatArg {
    fn as_format_string(&self) -> String;
    fn as_format_int(&self) -> i64;
    fn as_format_float(&self) -> f64;
    fn as_format_char(&self) -> Option<char>;
}

/// Parsed format specifier: `%[flags][width][.precision]conversion`.
struct FormatSpec {
    left_align: bool,
    zero_pad: bool,
    plus_sign: bool,
    space_sign: bool,
    alt_form: bool,
    width: Option<usize>,
    precision: Option<usize>,
    conversion: char,
}

/// Format a printf-style format string with the given arguments.
///
/// Supports: `%s`, `%d`, `%i`, `%f`, `%g`, `%e`, `%x`, `%X`, `%o`, `%c`, `%%`
/// With flags: `-` (left-align), `0` (zero-pad), `+`, ` `, `#`
/// With width and `.precision`.
///
/// Backslash escapes: `\n`, `\t`, `\r`, `\\`, `\0`
///
/// This is a single pass: each conversion consumes one argument in order, and
/// missing arguments fall back to defaults (`""`, `0`, `0.0`). awk's `sprintf`
/// uses this directly. POSIX `printf` reuses the format until all operands are
/// consumed — see [`format_string_cycling`].
pub fn format_string<A: FormatArg>(format: &str, args: &[A]) -> String {
    let mut output = String::new();
    format_pass(format, args, &mut output);
    output
}

/// POSIX `printf` cycling: reuse the format string until all operands are
/// consumed. `printf '%s\n' a b c` → `a\nb\nc\n`.
///
/// Each pass consumes as many operands as the format has conversion
/// specifiers. A format with no conversions is printed exactly once (extra
/// operands are ignored, matching bash) — this also guards against an infinite
/// loop. The final pass may run short on operands; the missing ones default.
pub fn format_string_cycling<A: FormatArg>(format: &str, args: &[A]) -> String {
    let mut output = String::new();
    // The first pass always runs, so a zero-operand call still prints the
    // literal text and an all-default conversion line.
    let per_pass = format_pass(format, args, &mut output);
    if per_pass == 0 {
        return output;
    }
    let mut start = per_pass;
    while start < args.len() {
        let end = (start + per_pass).min(args.len());
        format_pass(format, &args[start..end], &mut output);
        start = end;
    }
    output
}

/// Run one formatting pass, appending to `output`. Returns the number of
/// conversion specifiers applied (i.e. operand slots consumed this pass).
fn format_pass<A: FormatArg>(format: &str, args: &[A], output: &mut String) -> usize {
    let mut arg_index = 0;
    let mut chars = format.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '%' {
            match parse_specifier(&mut chars) {
                Some(spec) => {
                    let arg = args.get(arg_index);
                    apply_specifier(&spec, arg, output);
                    arg_index += 1;
                }
                None => {
                    // Was %% → literal %
                    output.push('%');
                }
            }
        } else if c == '\\' {
            match chars.next() {
                Some('n') => output.push('\n'),
                Some('t') => output.push('\t'),
                Some('r') => output.push('\r'),
                Some('\\') => output.push('\\'),
                Some('0') => output.push('\0'),
                Some(ch) => {
                    output.push('\\');
                    output.push(ch);
                }
                None => output.push('\\'),
            }
        } else {
            output.push(c);
        }
    }

    arg_index
}

/// Parse a format specifier after the initial `%`.
/// Returns `None` for `%%` (literal percent).
fn parse_specifier(chars: &mut std::iter::Peekable<std::str::Chars<'_>>) -> Option<FormatSpec> {
    // Check for %%
    if chars.peek() == Some(&'%') {
        chars.next();
        return None;
    }

    let mut spec = FormatSpec {
        left_align: false,
        zero_pad: false,
        plus_sign: false,
        space_sign: false,
        alt_form: false,
        width: None,
        precision: None,
        conversion: 's',
    };

    // Parse flags
    loop {
        match chars.peek() {
            Some('-') => { spec.left_align = true; chars.next(); }
            Some('0') => { spec.zero_pad = true; chars.next(); }
            Some('+') => { spec.plus_sign = true; chars.next(); }
            Some(' ') => { spec.space_sign = true; chars.next(); }
            Some('#') => { spec.alt_form = true; chars.next(); }
            _ => break,
        }
    }

    // Parse width
    let mut width_str = String::new();
    while let Some(&c) = chars.peek() {
        if c.is_ascii_digit() {
            width_str.push(c);
            chars.next();
        } else {
            break;
        }
    }
    if !width_str.is_empty() {
        spec.width = width_str.parse().ok();
    }

    // Parse precision
    if chars.peek() == Some(&'.') {
        chars.next();
        let mut prec_str = String::new();
        while let Some(&c) = chars.peek() {
            if c.is_ascii_digit() {
                prec_str.push(c);
                chars.next();
            } else {
                break;
            }
        }
        spec.precision = Some(prec_str.parse().unwrap_or(0));
    }

    // Parse conversion character
    if let Some(&c) = chars.peek() {
        match c {
            's' | 'd' | 'i' | 'f' | 'g' | 'e' | 'x' | 'X' | 'o' | 'c' => {
                spec.conversion = c;
                chars.next();
            }
            _ => {
                // Unknown specifier — treat as literal
                spec.conversion = c;
                chars.next();
            }
        }
    }

    Some(spec)
}

/// Apply a parsed format specifier to an argument, writing to `output`.
fn apply_specifier<A: FormatArg>(spec: &FormatSpec, arg: Option<&A>, output: &mut String) {
    match spec.conversion {
        's' => {
            let val = arg.map(|a| a.as_format_string()).unwrap_or_default();
            apply_string_padding(spec, &val, output);
        }
        'd' | 'i' => {
            let val = arg.map(|a| a.as_format_int()).unwrap_or(0);
            apply_int_format(spec, val, output, IntBase::Decimal);
        }
        'f' => {
            let val = arg.map(|a| a.as_format_float()).unwrap_or(0.0);
            let precision = spec.precision.unwrap_or(6);
            let formatted = format!("{:.prec$}", val, prec = precision);
            apply_string_padding(spec, &formatted, output);
        }
        'g' => {
            let val = arg.map(|a| a.as_format_float()).unwrap_or(0.0);
            let precision = spec.precision.unwrap_or(6);
            // %g uses the shorter of %e and %f, removing trailing zeros
            let formatted = format!("{:.prec$}", val, prec = precision);
            let trimmed = formatted.trim_end_matches('0').trim_end_matches('.');
            apply_string_padding(spec, trimmed, output);
        }
        'e' => {
            let val = arg.map(|a| a.as_format_float()).unwrap_or(0.0);
            let precision = spec.precision.unwrap_or(6);
            let formatted = format!("{:.prec$e}", val, prec = precision);
            apply_string_padding(spec, &formatted, output);
        }
        'x' => {
            let val = arg.map(|a| a.as_format_int()).unwrap_or(0);
            apply_int_format(spec, val, output, IntBase::LowerHex);
        }
        'X' => {
            let val = arg.map(|a| a.as_format_int()).unwrap_or(0);
            apply_int_format(spec, val, output, IntBase::UpperHex);
        }
        'o' => {
            let val = arg.map(|a| a.as_format_int()).unwrap_or(0);
            apply_int_format(spec, val, output, IntBase::Octal);
        }
        'c' => {
            if let Some(ch) = arg.and_then(|a| a.as_format_char()) {
                output.push(ch);
            }
        }
        other => {
            // Unknown conversion — output literally
            output.push('%');
            output.push(other);
        }
    }
}

enum IntBase {
    Decimal,
    LowerHex,
    UpperHex,
    Octal,
}

fn apply_int_format(spec: &FormatSpec, val: i64, output: &mut String, base: IntBase) {
    let raw = match base {
        IntBase::Decimal => format!("{}", val),
        IntBase::LowerHex => format!("{:x}", val),
        IntBase::UpperHex => format!("{:X}", val),
        IntBase::Octal => format!("{:o}", val),
    };

    let width = spec.width.unwrap_or(0);
    if width > raw.len() {
        let pad_count = width - raw.len();
        if spec.left_align {
            output.push_str(&raw);
            for _ in 0..pad_count { output.push(' '); }
        } else if spec.zero_pad {
            // Handle sign with zero padding
            if val < 0 && matches!(base, IntBase::Decimal) {
                output.push('-');
                for _ in 0..(pad_count) { output.push('0'); }
                output.push_str(&raw[1..]); // skip the '-'
            } else {
                for _ in 0..pad_count { output.push('0'); }
                output.push_str(&raw);
            }
        } else {
            for _ in 0..pad_count { output.push(' '); }
            output.push_str(&raw);
        }
    } else {
        output.push_str(&raw);
    }
}

fn apply_string_padding(spec: &FormatSpec, val: &str, output: &mut String) {
    let width = spec.width.unwrap_or(0);
    if width > val.len() {
        let pad_count = width - val.len();
        if spec.left_align {
            output.push_str(val);
            for _ in 0..pad_count { output.push(' '); }
        } else if spec.zero_pad {
            for _ in 0..pad_count { output.push('0'); }
            output.push_str(val);
        } else {
            for _ in 0..pad_count { output.push(' '); }
            output.push_str(val);
        }
    } else {
        output.push_str(val);
    }
}

#[cfg(test)]
#[allow(clippy::approx_constant)]
mod tests {
    use super::*;

    /// Test adapter: simple string/int/float values.
    enum TestVal {
        Str(String),
        Int(i64),
        Float(f64),
    }

    impl FormatArg for TestVal {
        fn as_format_string(&self) -> String {
            match self {
                TestVal::Str(s) => s.clone(),
                TestVal::Int(i) => i.to_string(),
                TestVal::Float(f) => f.to_string(),
            }
        }
        fn as_format_int(&self) -> i64 {
            match self {
                TestVal::Int(i) => *i,
                TestVal::Float(f) => *f as i64,
                TestVal::Str(s) => s.parse().unwrap_or(0),
            }
        }
        fn as_format_float(&self) -> f64 {
            match self {
                TestVal::Float(f) => *f,
                TestVal::Int(i) => *i as f64,
                TestVal::Str(s) => s.parse().unwrap_or(0.0),
            }
        }
        fn as_format_char(&self) -> Option<char> {
            match self {
                TestVal::Str(s) => s.chars().next(),
                TestVal::Int(i) => char::from_u32(*i as u32),
                _ => None,
            }
        }
    }

    #[test]
    fn test_bare_specifiers() {
        let args = vec![TestVal::Str("hello".into()), TestVal::Int(42)];
        assert_eq!(format_string("%s %d", &args), "hello 42");
    }

    #[test]
    fn test_left_align() {
        let args = vec![TestVal::Str("hi".into())];
        assert_eq!(format_string("%-10s|", &args), "hi        |");
    }

    #[test]
    fn test_right_align() {
        let args = vec![TestVal::Str("hi".into())];
        assert_eq!(format_string("%10s|", &args), "        hi|");
    }

    #[test]
    fn test_zero_pad_int() {
        let args = vec![TestVal::Int(42)];
        assert_eq!(format_string("%08d", &args), "00000042");
    }

    #[test]
    fn test_zero_pad_hex() {
        let args = vec![TestVal::Int(255)];
        assert_eq!(format_string("%08x", &args), "000000ff");
    }

    #[test]
    fn test_precision_float() {
        let args = vec![TestVal::Float(3.14159)];
        assert_eq!(format_string("%.2f", &args), "3.14");
    }

    #[test]
    fn test_width_and_precision_float() {
        let args = vec![TestVal::Float(3.14)];
        assert_eq!(format_string("%10.2f", &args), "      3.14");
    }

    #[test]
    fn test_percent_escape() {
        let args: Vec<TestVal> = vec![];
        assert_eq!(format_string("100%%", &args), "100%");
    }

    #[test]
    fn test_backslash_escapes() {
        let args: Vec<TestVal> = vec![];
        assert_eq!(format_string("a\\nb\\tc", &args), "a\nb\tc");
    }

    #[test]
    fn test_width_int() {
        let args = vec![TestVal::Int(42)];
        assert_eq!(format_string("%6d", &args), "    42");
    }

    #[test]
    fn test_left_align_int() {
        let args = vec![TestVal::Int(42)];
        assert_eq!(format_string("%-6d|", &args), "42    |");
    }

    #[test]
    fn test_cycling_single_conversion() {
        // POSIX: `printf '%s\n' a b c` reuses the format per operand.
        let args = vec![
            TestVal::Str("a".into()),
            TestVal::Str("b".into()),
            TestVal::Str("c".into()),
        ];
        assert_eq!(format_string_cycling("%s\\n", &args), "a\nb\nc\n");
    }

    #[test]
    fn test_cycling_two_conversions_per_pass() {
        // `printf '%s-%s ' a b c d` → "a-b c-d "
        let args = vec![
            TestVal::Str("a".into()),
            TestVal::Str("b".into()),
            TestVal::Str("c".into()),
            TestVal::Str("d".into()),
        ];
        assert_eq!(format_string_cycling("%s-%s ", &args), "a-b c-d ");
    }

    #[test]
    fn test_cycling_partial_final_pass_defaults() {
        // `printf '%s-%s ' a b c` → final pass runs short; missing operand defaults to "".
        let args = vec![
            TestVal::Str("a".into()),
            TestVal::Str("b".into()),
            TestVal::Str("c".into()),
        ];
        assert_eq!(format_string_cycling("%s-%s ", &args), "a-b c- ");
    }

    #[test]
    fn test_cycling_no_conversions_prints_once() {
        // No conversions: print the format exactly once, ignore extra operands.
        // (Also guards against an infinite loop.)
        let args = vec![TestVal::Str("a".into()), TestVal::Str("b".into())];
        assert_eq!(format_string_cycling("hello\\n", &args), "hello\n");
    }

    #[test]
    fn test_cycling_single_pass_when_args_match() {
        let args = vec![TestVal::Str("Alice".into()), TestVal::Int(30)];
        assert_eq!(
            format_string_cycling("%s is %d", &args),
            "Alice is 30"
        );
    }

    #[test]
    fn test_cycling_no_args_still_runs_once() {
        let args: Vec<TestVal> = vec![];
        assert_eq!(format_string_cycling("%s\\n", &args), "\n");
    }
}
