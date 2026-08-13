//! RFC 3339 UTC timestamps for the wire.
//!
//! kaish pins every `SystemTime` on its serde surface (`JobInfo`) to one
//! spelling: `YYYY-MM-DDTHH:MM:SS.mmmZ` — UTC only, exactly three fractional
//! digits, truncated never rounded. Fixed width keeps string order equal to
//! time order, and `Date()` / `datetime.fromisoformat()` parse it natively.
//! Parsing accepts 0–9 fractional digits but only the `Z` zone: kaish never
//! emits an offset, so it does not accept one.
//!
//! Hand-rolled on purpose: kaish-types is a pure-data leaf crate and this is a
//! screen of arithmetic (Howard Hinnant's civil-calendar algorithms); a
//! calendar dependency buys nothing here. No OS calls, so it works on
//! `wasm32-unknown-unknown` (see [`crate::clock`] for how "now" is acquired
//! there).

use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// Format as `YYYY-MM-DDTHH:MM:SS.mmmZ` (UTC, millisecond precision,
/// truncated). Errors on a time before 1970-01-01T00:00:00Z — kaish never
/// produces one.
pub fn format(t: SystemTime) -> Result<String, String> {
    let d = t
        .duration_since(UNIX_EPOCH)
        .map_err(|_| "timestamp before 1970-01-01T00:00:00Z cannot be formatted".to_string())?;
    let secs = d.as_secs();
    let millis = d.subsec_millis();
    let (year, month, day) = civil_from_days((secs / 86_400) as i64);
    let tod = secs % 86_400;
    Ok(std::format!(
        "{year:04}-{month:02}-{day:02}T{:02}:{:02}:{:02}.{millis:03}Z",
        tod / 3600,
        (tod % 3600) / 60,
        tod % 60
    ))
}

/// Parse `YYYY-MM-DDTHH:MM:SS[.f{1,9}]Z` back to a `SystemTime`.
///
/// Strict on purpose — one spelling, one meaning: uppercase `T` and `Z`, a
/// real calendar date, and nothing before the epoch. The error names the
/// expected shape so an agent can fix its input without reading docs.
pub fn parse(s: &str) -> Result<SystemTime, String> {
    let fail = |why: &str| {
        Err(std::format!(
            "invalid RFC 3339 timestamp {s:?}: {why}; expected YYYY-MM-DDTHH:MM:SS[.fff]Z (UTC `Z` only)"
        ))
    };

    let b = s.as_bytes();
    // Fixed head: YYYY-MM-DDTHH:MM:SS is 19 bytes; the tail is `Z` or `.f…Z`.
    if b.len() < 20 {
        return fail("too short");
    }
    if b[4] != b'-' || b[7] != b'-' || b[13] != b':' || b[16] != b':' {
        return fail("misplaced separator");
    }
    if b[10] != b'T' {
        return fail("expected `T` between date and time");
    }
    if b[b.len() - 1] != b'Z' {
        return fail("expected the `Z` zone");
    }

    let num = |range: std::ops::Range<usize>, what: &str| -> Result<u64, String> {
        let text = &s[range];
        // The emptiness check is load-bearing, not just a nicer error: an
        // empty string passes the all-digits check vacuously and then panics
        // `str::parse` under the expect below.
        if text.is_empty() || !text.bytes().all(|c| c.is_ascii_digit()) {
            return Err(std::format!(
                "invalid RFC 3339 timestamp {s:?}: non-digit {what}; expected YYYY-MM-DDTHH:MM:SS[.fff]Z (UTC `Z` only)"
            ));
        }
        // Non-empty digits-only with length <= 9 cannot fail or overflow u64.
        #[allow(clippy::expect_used)]
        Ok(text.parse::<u64>().expect("non-empty digit-checked"))
    };

    let year = num(0..4, "year")?;
    let month = num(5..7, "month")?;
    let day = num(8..10, "day")?;
    let hour = num(11..13, "hour")?;
    let minute = num(14..16, "minute")?;
    let second = num(17..19, "second")?;

    if !(1..=12).contains(&month) {
        return fail("month out of range");
    }
    if day < 1 || day > days_in_month(year, month as u32) as u64 {
        return fail("day out of range for that month");
    }
    if hour > 23 || minute > 59 || second > 59 {
        return fail("time of day out of range (leap seconds are not representable)");
    }

    // Fractional seconds: `.` then 1–9 digits, right-padded to nanoseconds.
    let nanos: u32 = match b.len() {
        20 => 0,
        _ => {
            if b[19] != b'.' {
                return fail("expected `.` before fractional seconds");
            }
            let digits = &s[20..s.len() - 1];
            if digits.is_empty() || digits.len() > 9 {
                return fail("fractional seconds need 1-9 digits");
            }
            let value = num(20..s.len() - 1, "fractional seconds")?;
            (value * 10u64.pow(9 - digits.len() as u32)) as u32
        }
    };

    if year < 1970 {
        return fail("before 1970-01-01T00:00:00Z");
    }
    let days = days_from_civil(year as i64, month as u32, day as u32) as u64;
    let secs = days * 86_400 + hour * 3600 + minute * 60 + second;
    Ok(UNIX_EPOCH + Duration::new(secs, nanos))
}

fn days_in_month(year: u64, month: u32) -> u32 {
    match month {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        _ => {
            if year % 4 == 0 && (year % 100 != 0 || year % 400 == 0) {
                29
            } else {
                28
            }
        }
    }
}

// Howard Hinnant's civil-calendar algorithms (public domain), specialized to
// the post-1970 range the parser admits.

fn civil_from_days(z: i64) -> (i64, u32, u32) {
    let z = z + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = (z - era * 146_097) as u64;
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let day = (doy - (153 * mp + 2) / 5 + 1) as u32;
    let month = if mp < 10 { mp + 3 } else { mp - 9 } as u32;
    let year = yoe as i64 + era * 400 + i64::from(month <= 2);
    (year, month, day)
}

fn days_from_civil(year: i64, month: u32, day: u32) -> i64 {
    let year = if month <= 2 { year - 1 } else { year };
    let era = if year >= 0 { year } else { year - 399 } / 400;
    let yoe = (year - era * 400) as u64;
    let mp = u64::from(if month > 2 { month - 3 } else { month + 9 });
    let doy = (153 * mp + 2) / 5 + u64::from(day) - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146_097 + doe as i64 - 719_468
}

/// serde adapter for a required `SystemTime` field:
/// `#[serde(with = "crate::rfc3339::system_time")]`.
pub mod system_time {
    use super::*;
    use serde::{Deserialize, Deserializer, Serializer};

    pub fn serialize<S: Serializer>(t: &SystemTime, s: S) -> Result<S::Ok, S::Error> {
        let text = super::format(*t).map_err(serde::ser::Error::custom)?;
        s.serialize_str(&text)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<SystemTime, D::Error> {
        let text = String::deserialize(d)?;
        super::parse(&text).map_err(serde::de::Error::custom)
    }
}

/// serde adapter for an `Option<SystemTime>` field; pair it with
/// `default, skip_serializing_if = "Option::is_none"`:
/// `#[serde(with = "crate::rfc3339::opt_system_time")]`.
pub mod opt_system_time {
    use super::*;
    use serde::{Deserialize, Deserializer, Serializer};

    pub fn serialize<S: Serializer>(t: &Option<SystemTime>, s: S) -> Result<S::Ok, S::Error> {
        match t {
            Some(t) => super::system_time::serialize(t, s),
            None => s.serialize_none(),
        }
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<Option<SystemTime>, D::Error> {
        match Option::<String>::deserialize(d)? {
            Some(text) => super::parse(&text).map(Some).map_err(serde::de::Error::custom),
            None => Ok(None),
        }
    }
}

/// Schema for a required RFC 3339 field (`schemars(schema_with = ...)`).
#[cfg(feature = "schema")]
pub fn schema(_: &mut schemars::SchemaGenerator) -> schemars::Schema {
    schemars::json_schema!({ "type": "string", "format": "date-time" })
}

/// Schema for an optional RFC 3339 field (`schemars(schema_with = ...)`).
#[cfg(feature = "schema")]
pub fn opt_schema(_: &mut schemars::SchemaGenerator) -> schemars::Schema {
    schemars::json_schema!({ "type": ["string", "null"], "format": "date-time" })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn epoch_formats_as_fixed_width() {
        assert_eq!(format(UNIX_EPOCH).unwrap(), "1970-01-01T00:00:00.000Z");
    }

    #[test]
    fn submillisecond_precision_truncates_never_rounds() {
        let t = UNIX_EPOCH + Duration::new(0, 999_999_999);
        assert_eq!(format(t).unwrap(), "1970-01-01T00:00:00.999Z");
    }

    #[test]
    fn pre_epoch_formatting_is_a_loud_error() {
        let t = UNIX_EPOCH - Duration::from_secs(1);
        assert!(format(t).unwrap_err().contains("1970"));
    }

    #[test]
    fn format_parse_round_trips_across_month_and_year_boundaries() {
        for secs in [
            0u64,          // epoch
            86_399,        // last second of a day
            951_782_399,   // 2000-02-28T23:59:59 (century leap year eve)
            951_782_400,   // 2000-02-29 (400-rule leap day)
            1_704_067_199, // 2023-12-31T23:59:59
            1_704_067_200, // 2024-01-01
            4_102_444_799, // 2099-12-31T23:59:59
        ] {
            let t = UNIX_EPOCH + Duration::new(secs, 123_000_000);
            let text = format(t).unwrap();
            assert_eq!(parse(&text).unwrap(), t, "{text}");
        }
    }
}
