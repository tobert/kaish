//! Duration parsing for shell-style time strings.
//!
//! Used by the `timeout` builtin and `scatter --timeout` to parse durations
//! like `30`, `30s`, `500ms`, `5m`, `1h`.

use std::time::Duration;

/// Reject leading zeros on integer parts (`010`, `00`, `00.5`, `+010`, `00e5`).
/// `"0"`, `"0.5"`, and `"0e5"` are valid; `"00"` and `"010"` are refused.
fn has_leading_zero(num: &str) -> bool {
    let digits = num.strip_prefix('+').unwrap_or(num);
    let int_part = digits.split(['.', 'e', 'E']).next().unwrap_or(digits);
    int_part.len() > 1 && int_part.starts_with('0')
}

/// Parse a non-negative finite float duration value, rejecting leading zeros.
fn parse_non_neg_f64(num: &str) -> Option<f64> {
    let num = num.trim();
    if num.is_empty() || has_leading_zero(num) {
        return None;
    }
    let val = num.parse::<f64>().ok()?;
    if val.is_finite() && val >= 0.0 {
        Some(val)
    } else {
        None
    }
}

/// Parse a duration string: `30` (seconds), `30s`, `500ms`, `5m`, `1h`.
///
/// Returns `None` for invalid input (negative, leading zero like `010`,
/// non-finite, unrecognized suffix, non-numeric).
pub fn parse_duration(s: &str) -> Option<Duration> {
    let s = s.trim();

    if let Some(secs) = parse_non_neg_f64(s) {
        return Duration::try_from_secs_f64(secs).ok();
    }

    if let Some(num) = s.strip_suffix("ms") {
        let num = num.trim();
        if num.is_empty() || has_leading_zero(num) {
            return None;
        }
        let ms: u64 = num.parse().ok()?;
        return Some(Duration::from_millis(ms));
    }
    if let Some(num) = s.strip_suffix('s') {
        let secs = parse_non_neg_f64(num)?;
        return Duration::try_from_secs_f64(secs).ok();
    }
    if let Some(num) = s.strip_suffix('m') {
        let mins = parse_non_neg_f64(num)?;
        return Duration::try_from_secs_f64(mins * 60.0).ok();
    }
    if let Some(num) = s.strip_suffix('h') {
        let hours = parse_non_neg_f64(num)?;
        return Duration::try_from_secs_f64(hours * 3600.0).ok();
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn seconds() {
        assert_eq!(parse_duration("30"), Some(Duration::from_secs(30)));
        assert_eq!(parse_duration("0"), Some(Duration::from_secs(0)));
        assert_eq!(parse_duration("1.5"), Some(Duration::from_secs_f64(1.5)));
    }

    #[test]
    fn suffixes() {
        assert_eq!(parse_duration("500ms"), Some(Duration::from_millis(500)));
        assert_eq!(parse_duration("5s"), Some(Duration::from_secs(5)));
        assert_eq!(parse_duration("2m"), Some(Duration::from_secs(120)));
        assert_eq!(parse_duration("1h"), Some(Duration::from_secs(3600)));
    }

    #[test]
    fn invalid() {
        assert_eq!(parse_duration("abc"), None);
        assert_eq!(parse_duration(""), None);
        assert_eq!(parse_duration("-5"), None);
        assert_eq!(parse_duration("5x"), None);
        assert_eq!(parse_duration("010"), None);
        assert_eq!(parse_duration("00"), None);
        assert_eq!(parse_duration("010ms"), None);
        assert_eq!(parse_duration("010s"), None);
        assert_eq!(parse_duration("010m"), None);
        assert_eq!(parse_duration("010h"), None);
        assert_eq!(parse_duration("00.5"), None);
        assert_eq!(parse_duration("+010"), None);
        assert_eq!(parse_duration("00e5"), None);
        assert_eq!(parse_duration("inf"), None);
        assert_eq!(parse_duration("NaN"), None);
        assert_eq!(parse_duration("0e5"), Some(Duration::from_secs(0)));
        assert_eq!(parse_duration("0.5"), Some(Duration::from_secs_f64(0.5)));
        assert_eq!(parse_duration("0ms"), Some(Duration::from_millis(0)));
    }
}
