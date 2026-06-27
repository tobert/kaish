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
/// Supports: `%s`, `%d`, `%i`, `%u`, `%f`, `%g`, `%G`, `%e`, `%E`, `%x`, `%X`, `%o`, `%c`, `%b`, `%%`
/// With flags: `-` (left-align), `0` (zero-pad), `+`, ` `, `#`
/// With width and `.precision`.
///
/// Backslash escapes: `\n`, `\t`, `\r`, `\\`, `\0`, `\NNN` (octal)
///
/// This is a single pass: each conversion consumes one argument in order, and
/// missing arguments fall back to defaults (`""`, `0`, `0.0`). awk's `sprintf`
/// uses this directly. POSIX `printf` reuses the format until all operands are
/// consumed — see [`format_string_cycling`].
pub fn format_string<A: FormatArg>(format: &str, args: &[A]) -> String {
    let mut output = String::new();
    let _ = format_pass(format, args, &mut output);
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
    let (per_pass, stop) = format_pass(format, args, &mut output);
    if stop || per_pass == 0 {
        return output;
    }
    let mut start = per_pass;
    while start < args.len() {
        let end = (start + per_pass).min(args.len());
        let (_, stop) = format_pass(format, &args[start..end], &mut output);
        if stop {
            break;
        }
        start = end;
    }
    output
}

/// Run one formatting pass, appending to `output`. Returns the number of
/// conversion specifiers applied (i.e. operand slots consumed this pass) and
/// whether output should stop entirely (a `\c` was reached, in the format
/// literal or via a `%b` argument).
fn format_pass<A: FormatArg>(format: &str, args: &[A], output: &mut String) -> (usize, bool) {
    let mut arg_index = 0;
    let mut chars = format.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '%' {
            match parse_specifier(&mut chars) {
                Some(spec) => {
                    let arg = args.get(arg_index);
                    let stop = apply_specifier(&spec, arg, output);
                    arg_index += 1;
                    if stop {
                        return (arg_index, true);
                    }
                }
                None => {
                    // Was %% → literal %
                    output.push('%');
                }
            }
        } else if c == '\\' {
            // `\c` in the format literal stops all output (GNU printf).
            if chars.peek() == Some(&'c') {
                return (arg_index, true);
            }
            parse_backslash_escape(&mut chars, output);
        } else {
            output.push(c);
        }
    }

    (arg_index, false)
}

/// Parse and emit a backslash escape sequence, starting after the `\`.
///
/// Handles: `\n`, `\t`, `\r`, `\\`, `\0`, `\0NNN` (octal, `\0` prefix),
/// and `\NNN` (octal without leading zero). Unknown escapes pass through
/// literally as `\X`.
fn parse_backslash_escape(
    chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
    output: &mut String,
) {
    match chars.peek().copied() {
        Some('n') => { chars.next(); output.push('\n'); }
        Some('t') => { chars.next(); output.push('\t'); }
        Some('r') => { chars.next(); output.push('\r'); }
        Some('\\') => { chars.next(); output.push('\\'); }
        Some('0') => {
            chars.next();
            // \0NNN: GNU printf reads the leading `0` as the FIRST of up to 3
            // octal digits, so at most 2 more follow (`\0101` → octal `010` = BS,
            // then a literal `1`; `\0377` → octal `037`, then `7`). Reading 3
            // more here would make `\0101` = octal 101 = 'A', diverging from
            // GNU/bash/dash. Bare `\NNN` (no leading 0, below) takes the full 3.
            let mut octal = String::new();
            while octal.len() < 2 {
                match chars.peek().copied() {
                    Some(d) if d.is_ascii_digit() && d != '8' && d != '9' => {
                        octal.push(d);
                        chars.next();
                    }
                    _ => break,
                }
            }
            if octal.is_empty() {
                output.push('\0');
            } else {
                let codepoint = u32::from_str_radix(&octal, 8).unwrap_or(0);
                output.push(char::from_u32(codepoint).unwrap_or('\0'));
            }
        }
        Some(d) if d.is_ascii_digit() && d != '8' && d != '9' => {
            // \NNN octal (no leading zero required — GNU printf supports both)
            let mut octal = String::new();
            while octal.len() < 3 {
                match chars.peek().copied() {
                    Some(d2) if d2.is_ascii_digit() && d2 != '8' && d2 != '9' => {
                        octal.push(d2);
                        chars.next();
                    }
                    _ => break,
                }
            }
            let codepoint = u32::from_str_radix(&octal, 8).unwrap_or(0);
            output.push(char::from_u32(codepoint).unwrap_or('\0'));
        }
        Some(ch) => {
            chars.next();
            output.push('\\');
            output.push(ch);
        }
        None => output.push('\\'),
    }
}

/// Interpret backslash escapes in an argument string, as done by `%b`.
///
/// Same escape set as the format string itself, but applied to the argument
/// value rather than the format template. Returns the interpreted string and
/// whether a `\c` was seen — `\c` stops *all* further printf output (like GNU
/// printf), not just the rest of this argument, so the caller propagates it.
fn interpret_backslash_escapes(s: &str) -> (String, bool) {
    let mut output = String::new();
    let mut chars = s.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.peek().copied() {
                Some('c') => {
                    // \c: suppress this and all further output.
                    return (output, true);
                }
                _ => parse_backslash_escape(&mut chars, &mut output),
            }
        } else {
            output.push(c);
        }
    }
    (output, false)
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
            's' | 'd' | 'i' | 'u' | 'f' | 'g' | 'G' | 'e' | 'E' | 'x' | 'X' | 'o' | 'c' | 'b' => {
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
///
/// Returns `true` if output should stop entirely (a `%b` argument contained
/// `\c`), so the caller can abandon the rest of the format and any cycling.
fn apply_specifier<A: FormatArg>(spec: &FormatSpec, arg: Option<&A>, output: &mut String) -> bool {
    match spec.conversion {
        's' => {
            let val = arg.map(|a| a.as_format_string()).unwrap_or_default();
            // Precision truncates the string.
            let val = if let Some(prec) = spec.precision {
                // Truncate to at most `prec` characters (Unicode-aware).
                let truncated: String = val.chars().take(prec).collect();
                truncated
            } else {
                val
            };
            apply_string_padding(spec, &val, output);
        }
        'd' | 'i' => {
            let val = arg.map(|a| a.as_format_int()).unwrap_or(0);
            apply_int_format(spec, val, output, IntBase::Decimal);
        }
        'u' => {
            // Unsigned decimal: reinterpret the i64 bits as u64.
            let val = arg.map(|a| a.as_format_int()).unwrap_or(0);
            let unsigned = val as u64;
            let raw = format!("{unsigned}");
            let with_sign = apply_sign_and_prefix(spec, false, false, &raw);
            apply_padded(spec, false, &with_sign, spec.precision, false, output);
        }
        'f' => {
            let val = arg.map(|a| a.as_format_float()).unwrap_or(0.0);
            let precision = spec.precision.unwrap_or(6);
            let formatted = format!("{:.prec$}", val, prec = precision);
            let negative = val.is_sign_negative() && val != 0.0 || formatted.starts_with('-');
            let body = formatted.trim_start_matches('-');
            let with_sign = apply_sign_and_prefix(spec, negative, false, body);
            apply_padded(spec, false, &with_sign, None, false, output);
        }
        'e' => {
            let val = arg.map(|a| a.as_format_float()).unwrap_or(0.0);
            let precision = spec.precision.unwrap_or(6);
            let formatted = format_scientific(val, precision, false);
            let negative = val.is_sign_negative();
            let body = formatted.trim_start_matches('-');
            let with_sign = apply_sign_and_prefix(spec, negative, false, body);
            apply_padded(spec, false, &with_sign, None, false, output);
        }
        'E' => {
            let val = arg.map(|a| a.as_format_float()).unwrap_or(0.0);
            let precision = spec.precision.unwrap_or(6);
            let formatted = format_scientific(val, precision, true);
            let negative = val.is_sign_negative();
            let body = formatted.trim_start_matches('-');
            let with_sign = apply_sign_and_prefix(spec, negative, false, body);
            apply_padded(spec, false, &with_sign, None, false, output);
        }
        'g' => {
            let val = arg.map(|a| a.as_format_float()).unwrap_or(0.0);
            let precision = spec.precision.unwrap_or(6).max(1);
            let formatted = format_g(val, precision, false);
            let negative = val.is_sign_negative();
            let body = formatted.trim_start_matches('-');
            let with_sign = apply_sign_and_prefix(spec, negative, false, body);
            apply_padded(spec, false, &with_sign, None, false, output);
        }
        'G' => {
            let val = arg.map(|a| a.as_format_float()).unwrap_or(0.0);
            let precision = spec.precision.unwrap_or(6).max(1);
            let formatted = format_g(val, precision, true);
            let negative = val.is_sign_negative();
            let body = formatted.trim_start_matches('-');
            let with_sign = apply_sign_and_prefix(spec, negative, false, body);
            apply_padded(spec, false, &with_sign, None, false, output);
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
            // %c honors width and the left-align flag (`printf '%5c' x` → `    x`).
            if let Some(ch) = arg.and_then(|a| a.as_format_char()) {
                apply_string_padding(spec, &ch.to_string(), output);
            }
        }
        'b' => {
            // %b: interpret backslash escapes in the argument string. A `\c`
            // stops all further output, so propagate the stop signal upward.
            let raw = arg.map(|a| a.as_format_string()).unwrap_or_default();
            let (val, stop) = interpret_backslash_escapes(&raw);
            apply_string_padding(spec, &val, output);
            return stop;
        }
        other => {
            // Unknown conversion — output literally
            output.push('%');
            output.push(other);
        }
    }
    false
}

enum IntBase {
    Decimal,
    LowerHex,
    UpperHex,
    Octal,
}

/// Format an integer with flags, width, and precision.
///
/// For integers, precision sets the minimum digit count (zero-pads the digits).
/// When precision is set, the `0` flag is ignored (POSIX). Width then pads the
/// whole result. Sign/space/alt-form prefixes are applied before width padding.
fn apply_int_format(spec: &FormatSpec, val: i64, output: &mut String, base: IntBase) {
    let negative = val < 0 && matches!(base, IntBase::Decimal);
    // Format the absolute value (or unsigned reinterpretation for hex/octal).
    let raw_digits: String = match base {
        IntBase::Decimal => {
            if val < 0 {
                // Format the absolute value — we handle the sign separately.
                format!("{}", val.unsigned_abs())
            } else {
                format!("{val}")
            }
        }
        IntBase::LowerHex => format!("{:x}", val as u64),
        IntBase::UpperHex => format!("{:X}", val as u64),
        IntBase::Octal => format!("{:o}", val as u64),
    };

    // Precision zero-pads the digits themselves. When precision is set, the
    // `0` flag is ignored (POSIX/GNU behavior).
    let min_digits = spec.precision.unwrap_or(0);
    let precision_set = spec.precision.is_some();

    let digit_str = if min_digits > raw_digits.len() {
        let pad = min_digits - raw_digits.len();
        let mut s = String::with_capacity(min_digits);
        for _ in 0..pad {
            s.push('0');
        }
        s.push_str(&raw_digits);
        s
    } else {
        raw_digits
    };

    // Alternate form prefix (#). Special case: 0 with # stays "0" for hex/oct.
    let alt_prefix: &str = if spec.alt_form && val != 0 {
        match base {
            IntBase::LowerHex => "0x",
            IntBase::UpperHex => "0X",
            IntBase::Octal => {
                // Only add "0" if the digit string doesn't already start with "0".
                if digit_str.starts_with('0') { "" } else { "0" }
            }
            IntBase::Decimal => "",
        }
    } else {
        ""
    };

    let body = format!("{alt_prefix}{digit_str}");
    let with_sign = apply_sign_and_prefix(spec, negative, false, &body);
    // precision_set suppresses the `0` flag for width padding.
    apply_padded(spec, precision_set, &with_sign, None, negative, output);
}

/// Prepend sign/space to `body` based on flags and whether the value is negative.
///
/// Returns the sign-prefixed string. The sign is NOT counted again by
/// `apply_padded` — the whole returned string is the body for width purposes.
fn apply_sign_and_prefix(spec: &FormatSpec, negative: bool, _is_float: bool, body: &str) -> String {
    if negative {
        format!("-{body}")
    } else if spec.plus_sign {
        format!("+{body}")
    } else if spec.space_sign {
        format!(" {body}")
    } else {
        body.to_string()
    }
}

/// Apply width padding to `with_sign` (which already has its sign/prefix).
///
/// `precision_set`: when true, suppresses zero-padding (POSIX: precision
/// overrides the `0` flag for integer conversions).
/// `negative`: used to place the `-` sign before zero-padding.
fn apply_padded(
    spec: &FormatSpec,
    precision_set: bool,
    with_sign: &str,
    _precision: Option<usize>,
    _negative: bool,
    output: &mut String,
) {
    let width = spec.width.unwrap_or(0);
    let len = with_sign.len();
    if width <= len {
        output.push_str(with_sign);
        return;
    }
    let pad_count = width - len;
    if spec.left_align {
        output.push_str(with_sign);
        for _ in 0..pad_count {
            output.push(' ');
        }
    } else if spec.zero_pad && !precision_set {
        // Zero-pad: sign comes first, then the zeros.
        let (sign_part, digit_part) = if with_sign.starts_with(['-', '+', ' ']) {
            (&with_sign[..1], &with_sign[1..])
        } else {
            ("", with_sign)
        };
        output.push_str(sign_part);
        for _ in 0..pad_count {
            output.push('0');
        }
        output.push_str(digit_part);
    } else {
        for _ in 0..pad_count {
            output.push(' ');
        }
        output.push_str(with_sign);
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

/// Format `val` in scientific notation matching GNU printf's `%e`/`%E`.
///
/// GNU always uses at least 2 exponent digits and an explicit sign: `1.000000e+03`.
/// Rust's `{:.6e}` produces `1.000000e3` — we normalise the exponent part.
fn format_scientific(val: f64, precision: usize, uppercase: bool) -> String {
    // Use Rust's built-in formatter, then fix the exponent.
    let raw = if uppercase {
        format!("{:.prec$E}", val, prec = precision)
    } else {
        format!("{:.prec$e}", val, prec = precision)
    };
    normalise_exponent(&raw, uppercase)
}

/// Reformat a Rust scientific-notation string to GNU printf's exponent format.
///
/// Rust: `1.000000e3` or `1.000000e-3`
/// GNU:  `1.000000e+03` or `1.000000e-03`
fn normalise_exponent(raw: &str, uppercase: bool) -> String {
    let e_char = if uppercase { 'E' } else { 'e' };
    if let Some(e_pos) = raw.rfind(e_char) {
        let mantissa = &raw[..e_pos];
        let exp_str = &raw[e_pos + 1..];
        // exp_str is like "3", "-3", "+3" (Rust omits leading zeros and the '+')
        let (sign, digits) = if let Some(rest) = exp_str.strip_prefix('-') {
            ("-", rest)
        } else if let Some(rest) = exp_str.strip_prefix('+') {
            ("+", rest)
        } else {
            ("+", exp_str)
        };
        // At least 2 exponent digits.
        let exp_num: i32 = digits.parse().unwrap_or(0);
        format!("{}{}{}{:02}", mantissa, e_char, sign, exp_num)
    } else {
        raw.to_string()
    }
}

/// Format `val` using `%g`/`%G` semantics.
///
/// Uses the shorter of `%e` and `%f` representations, with `precision`
/// significant digits (not decimal places). Trailing zeros are removed.
/// If the exponent is < -4 or >= precision, uses scientific notation.
fn format_g(val: f64, precision: usize, uppercase: bool) -> String {
    if val == 0.0 {
        return "0".to_string();
    }
    let exp = val.abs().log10().floor() as i32;
    // GNU %g uses scientific notation when exp < -4 or exp >= precision.
    if exp < -4 || exp >= precision as i32 {
        // Scientific notation with (precision - 1) decimal places.
        let sci_prec = if precision > 0 { precision - 1 } else { 0 };
        let raw = if uppercase {
            format!("{:.prec$E}", val, prec = sci_prec)
        } else {
            format!("{:.prec$e}", val, prec = sci_prec)
        };
        let normalised = normalise_exponent(&raw, uppercase);
        // Remove trailing zeros from the mantissa.
        trim_trailing_zeros_sci(&normalised, uppercase)
    } else {
        // Fixed notation with enough decimal places to show `precision` sig figs.
        let decimal_places = if precision as i32 - 1 - exp > 0 {
            (precision as i32 - 1 - exp) as usize
        } else {
            0
        };
        let raw = format!("{:.prec$}", val, prec = decimal_places);
        // Remove trailing zeros after the decimal point.
        trim_trailing_zeros_fixed(&raw)
    }
}

/// Remove trailing zeros from a fixed-point string like "3.140000" → "3.14".
/// If no decimal point, return as-is.
fn trim_trailing_zeros_fixed(s: &str) -> String {
    if !s.contains('.') {
        return s.to_string();
    }
    let trimmed = s.trim_end_matches('0').trim_end_matches('.');
    trimmed.to_string()
}

/// Remove trailing zeros from the mantissa of a scientific notation string.
/// E.g. `1.230000e+03` → `1.23e+03`, `1.000000e+03` → `1e+03`.
fn trim_trailing_zeros_sci(s: &str, uppercase: bool) -> String {
    let e_char = if uppercase { 'E' } else { 'e' };
    if let Some(e_pos) = s.rfind(e_char) {
        let mantissa = &s[..e_pos];
        let exp_part = &s[e_pos..];
        let trimmed = mantissa.trim_end_matches('0').trim_end_matches('.');
        format!("{trimmed}{exp_part}")
    } else {
        s.to_string()
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

    // --- new tests covering previously broken cases --------------------------

    #[test]
    fn test_plus_flag() {
        let args = vec![TestVal::Int(5)];
        assert_eq!(format_string("%+d", &args), "+5");
    }

    #[test]
    fn test_space_flag() {
        let args = vec![TestVal::Int(5)];
        assert_eq!(format_string("% d", &args), " 5");
    }

    #[test]
    fn test_hash_hex() {
        let args = vec![TestVal::Int(255)];
        assert_eq!(format_string("%#x", &args), "0xff");
    }

    #[test]
    fn test_hash_octal() {
        let args = vec![TestVal::Int(8)];
        assert_eq!(format_string("%#o", &args), "010");
    }

    #[test]
    fn test_precision_string() {
        let args = vec![TestVal::Str("abcdef".into())];
        assert_eq!(format_string("%.3s", &args), "abc");
    }

    #[test]
    fn test_precision_decimal() {
        let args = vec![TestVal::Int(5)];
        assert_eq!(format_string("%.3d", &args), "005");
    }

    #[test]
    fn test_precision_overrides_zero_flag() {
        // POSIX: precision for integers suppresses the 0 flag.
        let args = vec![TestVal::Int(5)];
        assert_eq!(format_string("%05.3d", &args), "  005");
    }

    #[test]
    fn test_u_conversion() {
        let args = vec![TestVal::Int(5)];
        assert_eq!(format_string("%u", &args), "5");
    }

    #[test]
    fn test_big_e_conversion() {
        let args = vec![TestVal::Float(1000.0)];
        assert_eq!(format_string("%E", &args), "1.000000E+03");
    }

    #[test]
    fn test_little_e_conversion() {
        let args = vec![TestVal::Float(1000.0)];
        assert_eq!(format_string("%e", &args), "1.000000e+03");
    }

    #[test]
    fn test_big_g_large() {
        let args = vec![TestVal::Float(1234567.0)];
        assert_eq!(format_string("%G", &args), "1.23457E+06");
    }

    #[test]
    fn test_little_g_large() {
        let args = vec![TestVal::Float(1234567.0)];
        assert_eq!(format_string("%g", &args), "1.23457e+06");
    }

    #[test]
    fn test_b_tab() {
        let args = vec![TestVal::Str("\\t".into())];
        assert_eq!(format_string("%b", &args), "\t");
    }

    #[test]
    fn test_octal_escape_format() {
        // \0NNN: leading 0 is the first of up to 3 octal digits (≤2 more), like
        // GNU/bash/dash. \0101 → octal 010 (BS) + literal '1', NOT 'A'.
        let args: Vec<TestVal> = vec![];
        assert_eq!(format_string("\\0101", &args), "\u{8}1");
        assert_eq!(format_string("\\012", &args), "\n");
    }

    #[test]
    fn test_octal_escape_no_zero_prefix() {
        // \101 in format → 'A'
        let args: Vec<TestVal> = vec![];
        assert_eq!(format_string("\\101", &args), "A");
    }
}
