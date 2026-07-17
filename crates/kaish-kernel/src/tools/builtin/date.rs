//! date — Display the date and time.
//!
//! Re-specced against the **empirical** `date` — what language models actually
//! type when they reach for it — rather than an imagined one. See
//! `docs/designing-syntax-with-llms.md` for the fleet-survey method behind this. The headline
//! behaviors:
//!
//! - **GNU dialect, deliberately.** 9/9 surveyed models were GNU-shaped, so we
//!   pick the GNU spellings (`-d`, `-I`, `-R`, `-r`) and say so. No BSD `-v`.
//! - **`-d` / `--date`** parses the convergent subset the fleet types:
//!   `@N` epoch, `now`/`today`/`yesterday`/`tomorrow`, `N units ago` / `±N
//!   units`, `next`/`last <weekday>`, an absolute ISO date/datetime, and the
//!   nested idiom `<absolute> ± <offset>` (`2026-06-01 -1 day`). Anything
//!   outside the subset fails loud — no half-supported open grammar.
//! - **Format path is hardened.** `%N` (and `%3N`/`%6N`/`%9N`) translate to
//!   chrono's nanosecond formatters so `date +%s%N` *works*; any genuinely
//!   unknown specifier (`%Q`) returns `exit 2` instead of panicking the worker.
//! - **`TZ` is honored, not silently ignored.** The effective zone is `-u` →
//!   UTC, else `--tz ZONE`, else the exported `TZ` var (resolved via
//!   `chrono-tz`), else host local. An unknown zone is a loud error.
//! - **`-r FILE`** reads the file's mtime through the VFS (read-only fit).
//! - **`--json`** emits every field at once for model consumers.
//!
//! The wall clock is injected via the [`Clock`] trait (real `Utc::now` in
//! production, a fixed instant in tests) so the `-d`/`-r`/`--json`/format paths
//! get assertions that can and will fail.

use std::path::Path;
use std::sync::Arc;

use async_trait::async_trait;
use chrono::{
    DateTime, Datelike, Days, Duration, FixedOffset, Local, Months, NaiveDate, NaiveDateTime,
    NaiveTime, SecondsFormat, TimeZone, Timelike, Utc, Weekday,
};
use chrono_tz::Tz;
use clap::{CommandFactory, Parser};

use crate::interpreter::{value_to_string, ExecResult, OutputData};
use crate::tools::{
    schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema,
};

/// The wall clock, behind a trait so tests can pin "now".
pub trait Clock: Send + Sync {
    fn now_utc(&self) -> DateTime<Utc>;
}

/// Production clock — reads the real system time.
///
/// Acquisition goes through `kaish_types::clock::system_now()` so this also
/// works on `wasm32-unknown-unknown`, where `Utc::now()` reaches std's
/// unsupported clock and panics; the conversion into chrono is pure math.
struct SystemClock;
impl Clock for SystemClock {
    fn now_utc(&self) -> DateTime<Utc> {
        DateTime::from(kaish_types::clock::system_now())
    }
}

/// Date tool: display the date and time.
pub struct Date {
    clock: Arc<dyn Clock>,
}

impl Default for Date {
    fn default() -> Self {
        Self {
            clock: Arc::new(SystemClock),
        }
    }
}

impl Date {
    pub fn new() -> Self {
        Self::default()
    }

    #[cfg(test)]
    fn with_clock(clock: Arc<dyn Clock>) -> Self {
        Self { clock }
    }
}

/// clap-derived argv layer for date.
#[derive(Parser, Debug)]
#[command(name = "date", about = "Display the date and time")]
struct DateArgs {
    /// Use UTC instead of local time.
    #[arg(short = 'u', long = "utc")]
    utc: bool,

    /// Display the time described by STRING (e.g. "yesterday", "2 days ago",
    /// "@1700000000", "2026-06-01 -1 day").
    #[arg(short = 'd', long = "date", value_name = "STRING")]
    date: Option<String>,

    /// Display the last-modified time of FILE.
    #[arg(short = 'r', long = "reference", value_name = "FILE")]
    reference: Option<String>,

    /// Render in the given IANA timezone (e.g. Asia/Tokyo). Overridden by -u.
    #[arg(long = "tz", value_name = "ZONE")]
    tz: Option<String>,

    /// Output ISO 8601; FMT ∈ date,hours,minutes,seconds,ns (default date).
    /// Value form requires `=`: `--iso-8601=seconds`.
    #[arg(
        short = 'I',
        long = "iso-8601",
        value_name = "FMT",
        num_args = 0..=1,
        default_missing_value = "date",
        require_equals = true,
    )]
    iso_8601: Option<String>,

    /// Output RFC 2822 format (e.g. Sun, 14 Jun 2026 09:03:15 -0400).
    #[arg(short = 'R', long = "rfc-2822")]
    rfc_2822: bool,

    /// Output RFC 3339; FMT ∈ date,seconds,ns.
    #[arg(long = "rfc-3339", value_name = "FMT")]
    rfc_3339: Option<String>,

    /// (hidden) legacy alias for --iso-8601=seconds. Nobody types it.
    #[arg(long = "iso", hide = true)]
    iso: bool,

    /// (hidden) legacy: print Unix epoch seconds. Use `+%s`.
    #[arg(long = "unix", hide = true)]
    unix: bool,

    /// (hidden) legacy: strftime format via flag. Use `+FORMAT`.
    #[arg(long = "format", hide = true, value_name = "FMT")]
    format: Option<String>,

    #[command(flatten)]
    global: GlobalFlags,

    /// Optional `+FORMAT` (strftime) and/or `@TIMESTAMP` argument.
    args: Vec<String>,
}

/// The effective timezone the output is rendered in.
enum Zone {
    Utc,
    Local,
    Named(Tz),
}

impl Zone {
    /// Render a UTC instant as a fixed-offset datetime in this zone. Used for
    /// the no-arithmetic paths (`now`, `-r FILE`); `-d` math is DST-aware and
    /// runs in the concrete zone via [`resolve_instant`].
    fn instant(&self, utc: DateTime<Utc>) -> DateTime<FixedOffset> {
        match self {
            Zone::Utc => utc.fixed_offset(),
            Zone::Local => utc.with_timezone(&Local).fixed_offset(),
            Zone::Named(tz) => utc.with_timezone(tz).fixed_offset(),
        }
    }
}

#[async_trait]
impl Tool for Date {
    fn name(&self) -> &str {
        "date"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &DateArgs::command(),
            "date",
            "Display the date and time",
            [
                ("Current date and time", "date"),
                ("UTC", "date -u"),
                ("Epoch seconds", "date +%s"),
                ("ISO 8601 date", "date -I"),
                ("Relative", "date -d \"2 days ago\""),
                ("Decode an epoch", "date -d \"@1700000000\""),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("date: {e}")),
        };
        self.execute_argv(argv, ctx).await
    }
}

impl Date {
    /// Parse a flat argv and run. Split out so tests can drive a clean argv
    /// (the production path arrives here via `to_argv()`).
    async fn execute_argv(&self, argv: Vec<String>, ctx: &mut ExecContext) -> ExecResult {
        let parsed = match DateArgs::try_parse_from(
            std::iter::once("date".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("date: {e}")),
        };
        parsed.global.apply(ctx);

        match self.run(&parsed, ctx).await {
            Ok(result) => result,
            Err(msg) => ExecResult::failure(2, msg),
        }
    }

    async fn run(&self, parsed: &DateArgs, ctx: &mut ExecContext) -> Result<ExecResult, String> {
        if parsed.date.is_some() && parsed.reference.is_some() {
            return Err("date: cannot combine --date and --reference".to_string());
        }

        let zone = resolve_zone(parsed.utc, parsed.tz.as_deref(), scope_tz(ctx))?;

        // The format positional is the first `+...` operand; any bare operand
        // that is neither `+FORMAT` nor `@TIMESTAMP` is an error (GNU rejects
        // stray operands, and a bare date is almost certainly a missing -d).
        let mut fmt_positional: Option<&str> = None;
        let mut at_positional: Option<&str> = None;
        for operand in &parsed.args {
            if let Some(stripped) = operand.strip_prefix('+') {
                if fmt_positional.is_none() {
                    fmt_positional = Some(stripped);
                }
            } else if operand.starts_with('@') {
                at_positional = Some(operand);
            } else {
                return Err(format!(
                    "date: unrecognized operand '{operand}' (did you mean -d \"{operand}\"?)"
                ));
            }
        }

        let now_utc = self.clock.now_utc();
        let dt: DateTime<FixedOffset> = if let Some(file) = &parsed.reference {
            file_mtime(ctx, &zone, file).await?
        } else if let Some(spec) = &parsed.date {
            resolve_instant(spec, &zone, now_utc)?
        } else if let Some(at) = at_positional {
            resolve_instant(at, &zone, now_utc)?
        } else {
            zone.instant(now_utc)
        };

        let body = format!("{}\n", render(&dt, parsed, fmt_positional)?);
        // Keep both the text body and the OutputData: `with_output` alone would
        // collapse a simple-text node back to a plain result and drop the
        // `rich_json`, leaving `--json` with nothing structured to emit.
        let output = OutputData::text(body.clone()).with_rich_json(build_json(&dt));
        Ok(ExecResult::with_output_and_text(output, body))
    }
}

/// Read the exported `TZ` variable from the hermetic scope, if non-empty.
fn scope_tz(ctx: &ExecContext) -> Option<String> {
    let raw = value_to_string(ctx.scope.get("TZ")?);
    if raw.is_empty() {
        None
    } else {
        Some(raw)
    }
}

/// Determine the effective zone. `-u` wins; then `--tz`; then `TZ`; then local.
fn resolve_zone(utc: bool, tz_flag: Option<&str>, tz_var: Option<String>) -> Result<Zone, String> {
    if utc {
        return Ok(Zone::Utc);
    }
    let name = tz_flag.map(str::to_string).or(tz_var);
    match name {
        Some(zone) => zone
            .parse::<Tz>()
            .map(Zone::Named)
            .map_err(|_| format!("date: unknown timezone '{zone}' (use an IANA name like Asia/Tokyo)")),
        None => Ok(Zone::Local),
    }
}

/// Stat `file` through the VFS and return its mtime in the effective zone.
async fn file_mtime(
    ctx: &mut ExecContext,
    zone: &Zone,
    file: &str,
) -> Result<DateTime<FixedOffset>, String> {
    let resolved = ctx.resolve_path(file);
    let info = ctx
        .backend
        .stat(Path::new(&resolved))
        .await
        .map_err(|e| format!("date: {file}: {e}"))?;
    let mtime = info
        .modified
        .ok_or_else(|| format!("date: {file}: no modification time available"))?;
    Ok(zone.instant(DateTime::<Utc>::from(mtime)))
}

// ---------------------------------------------------------------------------
// `-d` / `--date` parsing — the empirical subset, fail-loud outside it.
//
// All arithmetic runs in the concrete effective timezone (generic over `Tz`)
// and only collapses to `FixedOffset` at the end, so calendar shifts
// (days/weeks/months/years) stay DST-correct: chrono adjusts the offset and
// preserves the wall-clock time across a transition. Collapsing to a fixed
// offset *before* the math would silently shift the wall clock by an hour.
// ---------------------------------------------------------------------------

/// Resolve a `-d` string to a concrete instant, doing the math in `zone`'s
/// real timezone (DST-aware) and rendering the result as a fixed offset.
fn resolve_instant(
    spec: &str,
    zone: &Zone,
    now_utc: DateTime<Utc>,
) -> Result<DateTime<FixedOffset>, String> {
    match zone {
        Zone::Utc => parse_in_tz(spec, &Utc, now_utc),
        Zone::Local => parse_in_tz(spec, &Local, now_utc),
        Zone::Named(tz) => parse_in_tz(spec, tz, now_utc),
    }
}

fn parse_in_tz<Tz: TimeZone>(
    spec: &str,
    tz: &Tz,
    now_utc: DateTime<Utc>,
) -> Result<DateTime<FixedOffset>, String> {
    let base = now_utc.with_timezone(tz);
    parse_date_string(spec, base, tz).map(|dt| dt.fixed_offset())
}

/// Parse a `-d` string into a concrete instant in `tz`. `base` is "now" in
/// `tz`, the anchor for relative forms.
fn parse_date_string<Tz: TimeZone>(
    spec: &str,
    base: DateTime<Tz>,
    tz: &Tz,
) -> Result<DateTime<Tz>, String> {
    let trimmed = spec.trim();
    if trimmed.is_empty() {
        return Err("date: empty --date string".to_string());
    }

    // @N — epoch seconds. Absolute, but rendered in the effective zone.
    if let Some(rest) = trimmed.strip_prefix('@') {
        let secs: i64 = rest
            .trim()
            .parse()
            .map_err(|_| format!("date: invalid epoch timestamp '{trimmed}'"))?;
        let utc = DateTime::from_timestamp(secs, 0)
            .ok_or_else(|| format!("date: epoch timestamp out of range '{trimmed}'"))?;
        return Ok(utc.with_timezone(tz));
    }

    let lower = trimmed.to_ascii_lowercase();
    match lower.as_str() {
        "now" | "today" => return Ok(base),
        "yesterday" => return base.checked_sub_days(Days::new(1)).ok_or_else(overflow),
        "tomorrow" => return base.checked_add_days(Days::new(1)).ok_or_else(overflow),
        _ => {}
    }

    // next/last <weekday> — resets to midnight, like GNU.
    let lower_tokens: Vec<&str> = lower.split_whitespace().collect();
    if lower_tokens.len() == 2 {
        if let Some(weekday) = parse_weekday(lower_tokens[1]) {
            if lower_tokens[0] == "next" {
                return shift_to_weekday(base, weekday, true);
            } else if lower_tokens[0] == "last" {
                return shift_to_weekday(base, weekday, false);
            }
        }
    }

    // General form: an optional absolute-date prefix, then optional offset
    // terms. Covers `2 weeks ago`, `+3 days`, `2026-01-01`, and the nested
    // `2026-06-01 -1 day` idiom. Original case is preserved for the date parse
    // (the literal `T` in `2026-01-01T09:00` is case-sensitive).
    let tokens: Vec<&str> = trimmed.split_whitespace().collect();
    let (anchor, consumed) = match parse_absolute_prefix(&tokens, tz)? {
        Some((dt, n)) => (dt, n),
        None => (base, 0),
    };
    let rest = &tokens[consumed..];

    if rest.is_empty() {
        if consumed > 0 {
            return Ok(anchor); // pure absolute date
        }
        return Err(format!("date: cannot parse date string '{trimmed}'"));
    }

    let rest_lower: Vec<String> = rest.iter().map(|t| t.to_ascii_lowercase()).collect();
    apply_offset_terms(&rest_lower, anchor)
        .map_err(|_| format!("date: cannot parse date string '{trimmed}'"))
}

/// chrono date-math overflow — should never happen for plausible inputs.
fn overflow() -> String {
    "date: resulting time is out of range".to_string()
}

/// Interpret a naive wall-clock time as local to `tz`. `None` for times that
/// don't exist / are ambiguous (DST gaps/folds).
fn localize<Tz: TimeZone>(tz: &Tz, naive: NaiveDateTime) -> Option<DateTime<Tz>> {
    tz.from_local_datetime(&naive).single()
}

/// Try to consume a leading absolute date/datetime from `tokens`. Returns the
/// parsed instant and how many tokens it consumed (1 or 2), or `None` if the
/// front of `tokens` isn't an absolute date.
fn parse_absolute_prefix<Tz: TimeZone>(
    tokens: &[&str],
    tz: &Tz,
) -> Result<Option<(DateTime<Tz>, usize)>, String> {
    if tokens.is_empty() {
        return Ok(None);
    }

    // Two-token wall-clock datetime: `2026-06-01 09:00[:00]`.
    if tokens.len() >= 2 {
        let joined = format!("{} {}", tokens[0], tokens[1]);
        for fmt in ["%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M"] {
            if let Ok(naive) = NaiveDateTime::parse_from_str(&joined, fmt) {
                let dt = localize(tz, naive)
                    .ok_or_else(|| format!("date: ambiguous or invalid local time '{joined}'"))?;
                return Ok(Some((dt, 2)));
            }
        }
    }

    // One-token forms.
    let head = tokens[0];
    if let Ok(dt) = DateTime::parse_from_rfc3339(head) {
        // Carries its own offset; re-express in the effective zone.
        return Ok(Some((dt.with_timezone(tz), 1)));
    }
    for fmt in ["%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M"] {
        if let Ok(naive) = NaiveDateTime::parse_from_str(head, fmt) {
            let dt = localize(tz, naive)
                .ok_or_else(|| format!("date: ambiguous or invalid local time '{head}'"))?;
            return Ok(Some((dt, 1)));
        }
    }
    if let Ok(date) = NaiveDate::parse_from_str(head, "%Y-%m-%d") {
        let naive = date.and_time(NaiveTime::MIN);
        let dt = localize(tz, naive)
            .ok_or_else(|| format!("date: ambiguous or invalid local time '{head}'"))?;
        return Ok(Some((dt, 1)));
    }

    Ok(None)
}

/// Apply a sequence of `[in] N unit [N unit ...] [ago]` offset terms to `base`.
/// Errors (rather than no-ops) on anything that isn't a clean offset.
fn apply_offset_terms<Tz: TimeZone>(
    tokens: &[String],
    base: DateTime<Tz>,
) -> Result<DateTime<Tz>, String> {
    let mut slice = tokens;

    // Optional `in ...` prefix and `... ago` suffix; `ago` negates direction.
    if slice.first().map(String::as_str) == Some("in") {
        slice = &slice[1..];
    }
    let mut ago = false;
    if slice.last().map(String::as_str) == Some("ago") {
        ago = true;
        slice = &slice[..slice.len() - 1];
    }

    if slice.is_empty() || slice.len() % 2 != 0 {
        return Err("malformed".to_string());
    }
    // The first term must be a number for this to be an offset at all.
    if slice[0].parse::<i64>().is_err() {
        return Err("not an offset".to_string());
    }

    let mut dt = base;
    let mut i = 0;
    while i < slice.len() {
        let amount: i64 = slice[i].parse().map_err(|_| "bad number".to_string())?;
        let signed = if ago { -amount } else { amount };
        dt = apply_unit(dt, signed, &slice[i + 1]).map_err(|_| "bad unit".to_string())?;
        i += 2;
    }
    Ok(dt)
}

/// Shift `dt` by `n` of `unit` (negative `n` moves into the past). Sub-day
/// units add a true physical `Duration`; day/week/month/year are calendar
/// shifts so they stay DST-correct in a zone with transitions.
fn apply_unit<Tz: TimeZone>(dt: DateTime<Tz>, n: i64, unit: &str) -> Result<DateTime<Tz>, ()> {
    // Singularize a trailing plural `s` (days → day, secs → sec).
    let unit = unit.strip_suffix('s').unwrap_or(unit);
    let duration = match unit {
        "second" | "sec" => Some(Duration::seconds(n)),
        "minute" | "min" => Some(Duration::minutes(n)),
        "hour" | "hr" => Some(Duration::hours(n)),
        _ => None,
    };
    if let Some(d) = duration {
        return dt.checked_add_signed(d).ok_or(());
    }
    match unit {
        "day" => shift_days(dt, n),
        "week" | "wk" => shift_days(dt, n.checked_mul(7).ok_or(())?),
        "month" | "mon" => shift_months(dt, n),
        "year" | "yr" => shift_months(dt, n.checked_mul(12).ok_or(())?),
        _ => Err(()),
    }
}

/// Calendar-aware day shift (handles negative `n`).
fn shift_days<Tz: TimeZone>(dt: DateTime<Tz>, n: i64) -> Result<DateTime<Tz>, ()> {
    let magnitude = n.unsigned_abs();
    if n >= 0 {
        dt.checked_add_days(Days::new(magnitude)).ok_or(())
    } else {
        dt.checked_sub_days(Days::new(magnitude)).ok_or(())
    }
}

/// Calendar-aware month shift (handles negative `n`).
fn shift_months<Tz: TimeZone>(dt: DateTime<Tz>, n: i64) -> Result<DateTime<Tz>, ()> {
    let magnitude = u32::try_from(n.unsigned_abs()).map_err(|_| ())?;
    if n >= 0 {
        dt.checked_add_months(Months::new(magnitude)).ok_or(())
    } else {
        dt.checked_sub_months(Months::new(magnitude)).ok_or(())
    }
}

/// Parse a weekday name (full or three-letter abbreviation), case-insensitive.
fn parse_weekday(token: &str) -> Option<Weekday> {
    Some(match token {
        "mon" | "monday" => Weekday::Mon,
        "tue" | "tues" | "tuesday" => Weekday::Tue,
        "wed" | "weds" | "wednesday" => Weekday::Wed,
        "thu" | "thur" | "thurs" | "thursday" => Weekday::Thu,
        "fri" | "friday" => Weekday::Fri,
        "sat" | "saturday" => Weekday::Sat,
        "sun" | "sunday" => Weekday::Sun,
        _ => return None,
    })
}

/// The next/last occurrence of `target`, strictly off `base`'s day, at
/// midnight. "next friday" when today is friday is +7 days (GNU semantics).
fn shift_to_weekday<Tz: TimeZone>(
    base: DateTime<Tz>,
    target: Weekday,
    next: bool,
) -> Result<DateTime<Tz>, String> {
    let current = base.weekday().num_days_from_monday() as i64;
    let goal = target.num_days_from_monday() as i64;
    let days = if next {
        let d = (goal - current).rem_euclid(7);
        if d == 0 {
            7
        } else {
            d
        }
    } else {
        let d = (current - goal).rem_euclid(7);
        -(if d == 0 { 7 } else { d })
    };
    let shifted = shift_days(base, days).map_err(|()| overflow())?;
    shifted
        .with_hour(0)
        .and_then(|d| d.with_minute(0))
        .and_then(|d| d.with_second(0))
        .and_then(|d| d.with_nanosecond(0))
        .ok_or_else(overflow)
}

// ---------------------------------------------------------------------------
// Rendering — output mode selection + the hardened format path.
// ---------------------------------------------------------------------------

/// Build the human/text output for the resolved instant, honoring the output
/// mode flags. Precedence: explicit format > rfc-2822 > rfc-3339 > iso-8601 >
/// legacy --iso > legacy --unix > default.
fn render(
    dt: &DateTime<FixedOffset>,
    parsed: &DateArgs,
    fmt_positional: Option<&str>,
) -> Result<String, String> {
    if let Some(fmt) = fmt_positional.or(parsed.format.as_deref()) {
        return format_strftime(dt, fmt);
    }
    if parsed.rfc_2822 {
        return Ok(dt.to_rfc2822());
    }
    if let Some(fmt) = &parsed.rfc_3339 {
        return rfc_3339_variant(dt, fmt);
    }
    if let Some(fmt) = &parsed.iso_8601 {
        return iso_8601_variant(dt, fmt);
    }
    if parsed.iso {
        return iso_8601_variant(dt, "seconds");
    }
    if parsed.unix {
        return Ok(dt.timestamp().to_string());
    }
    Ok(dt.format("%Y-%m-%d %H:%M:%S").to_string())
}

/// Format via a strftime string, after translating GNU `%N` and validating
/// against unknown specifiers — so the path can never panic the worker.
fn format_strftime(dt: &DateTime<FixedOffset>, fmt: &str) -> Result<String, String> {
    let translated = translate_format(fmt);
    validate_format(&translated)?;
    Ok(dt.format(&translated).to_string())
}

/// Translate GNU nanosecond specifiers chrono lacks into chrono's `%<n>f`:
/// `%N`→`%9f`, `%3N`→`%3f`, `%6N`→`%6f`, `%9N`→`%9f`. `%%` is preserved.
fn translate_format(fmt: &str) -> String {
    let mut out = String::with_capacity(fmt.len());
    let mut chars = fmt.chars().peekable();
    while let Some(c) = chars.next() {
        if c != '%' {
            out.push(c);
            continue;
        }
        if chars.peek() == Some(&'%') {
            out.push('%');
            out.push('%');
            chars.next();
            continue;
        }
        let mut digits = String::new();
        while let Some(d) = chars.peek() {
            if d.is_ascii_digit() {
                digits.push(*d);
                chars.next();
            } else {
                break;
            }
        }
        if chars.peek() == Some(&'N') {
            chars.next();
            let width = if digits.is_empty() { "9" } else { &digits };
            out.push('%');
            out.push_str(width);
            out.push('f');
        } else {
            // Not a %N spec — re-emit `%` and any digits; the following
            // specifier char is handled by the next loop iteration.
            out.push('%');
            out.push_str(&digits);
        }
    }
    out
}

/// Reject a format string containing an unknown specifier, loudly.
fn validate_format(fmt: &str) -> Result<(), String> {
    use chrono::format::{Item, StrftimeItems};
    for item in StrftimeItems::new(fmt) {
        if matches!(item, Item::Error) {
            return Err(format!("date: unknown format specifier in '{fmt}'"));
        }
    }
    Ok(())
}

fn iso_8601_variant(dt: &DateTime<FixedOffset>, fmt: &str) -> Result<String, String> {
    let pattern = match fmt {
        "date" => "%Y-%m-%d",
        "hours" => "%Y-%m-%dT%H%:z",
        "minutes" => "%Y-%m-%dT%H:%M%:z",
        "seconds" => "%Y-%m-%dT%H:%M:%S%:z",
        "ns" => "%Y-%m-%dT%H:%M:%S,%9f%:z",
        other => {
            return Err(format!(
                "date: invalid argument '{other}' for --iso-8601 (date|hours|minutes|seconds|ns)"
            ))
        }
    };
    Ok(dt.format(pattern).to_string())
}

fn rfc_3339_variant(dt: &DateTime<FixedOffset>, fmt: &str) -> Result<String, String> {
    let pattern = match fmt {
        "date" => "%Y-%m-%d",
        "seconds" => "%Y-%m-%d %H:%M:%S%:z",
        "ns" => "%Y-%m-%d %H:%M:%S.%9f%:z",
        other => {
            return Err(format!(
                "date: invalid argument '{other}' for --rfc-3339 (date|seconds|ns)"
            ))
        }
    };
    Ok(dt.format(pattern).to_string())
}

/// The `--json` field bundle — every component at once for model consumers.
fn build_json(dt: &DateTime<FixedOffset>) -> serde_json::Value {
    serde_json::json!({
        "iso": dt.to_rfc3339_opts(SecondsFormat::Secs, false),
        "epoch": dt.timestamp(),
        "utc": dt.with_timezone(&Utc).to_rfc3339_opts(SecondsFormat::Secs, true),
        "local": dt.format("%Y-%m-%d %H:%M:%S").to_string(),
        "weekday": dt.format("%A").to_string(),
        "tz": dt.format("%:z").to_string(),
        "offset_seconds": dt.offset().local_minus_utc(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::interpreter::{apply_output_format, OutputFormat};
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};

    /// A clock pinned to a fixed instant so the new paths are exactly testable.
    struct FixedClock(DateTime<Utc>);
    impl Clock for FixedClock {
        fn now_utc(&self) -> DateTime<Utc> {
            self.0
        }
    }

    /// 2023-11-14T22:13:20Z — epoch 1700000000, a Tuesday.
    const FIXED_EPOCH: i64 = 1_700_000_000;

    fn fixed_date() -> Date {
        let instant = DateTime::from_timestamp(FIXED_EPOCH, 0).unwrap();
        Date::with_clock(Arc::new(FixedClock(instant)))
    }

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    fn argv(parts: &[&str]) -> Vec<String> {
        parts.iter().map(|s| s.to_string()).collect()
    }

    async fn run(date: &Date, parts: &[&str]) -> ExecResult {
        let mut ctx = make_ctx();
        date.execute_argv(argv(parts), &mut ctx).await
    }

    // --- Footgun regressions (these would fail against the old builtin) ------

    #[tokio::test]
    async fn unknown_specifier_errors_not_panics() {
        // %Q is genuinely unknown — must be a clean exit 2, never a panic.
        let result = run(&fixed_date(), &["+%Q"]).await;
        assert_eq!(result.code, 2, "unknown specifier should exit 2");
    }

    #[tokio::test]
    async fn nanos_specifier_is_translated() {
        // %N must WORK (translate to nanoseconds), not panic. At a whole-second
        // fixed clock the nanos are zero-padded.
        let result = run(&fixed_date(), &["-u", "+%s%N"]).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "1700000000000000000");
    }

    #[tokio::test]
    async fn at_timestamp_decodes_not_echoes() {
        // Quoted @N positional, no -d: decode to the known UTC instant.
        let result = run(&fixed_date(), &["-u", "@1700000000"]).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "2023-11-14 22:13:20");
    }

    #[tokio::test]
    async fn at_timestamp_via_dash_d() {
        let result = run(&fixed_date(), &["-u", "-d", "@1700000000", "+%Y-%m-%dT%H:%M:%SZ"]).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "2023-11-14T22:13:20Z");
    }

    #[tokio::test]
    async fn malformed_at_is_loud_error() {
        let result = run(&fixed_date(), &["-d", "@notanumber"]).await;
        assert_eq!(result.code, 2);
    }

    #[tokio::test]
    async fn tz_flag_is_honored() {
        // 22:13:20Z in Tokyo (+09:00) is 07:13 the next day.
        let result = run(&fixed_date(), &["--tz", "Asia/Tokyo", "-d", "@1700000000", "+%H:%M"]).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "07:13");
    }

    #[tokio::test]
    async fn tz_var_is_honored_and_differs_from_utc() {
        let date = fixed_date();
        let mut ctx = make_ctx();
        ctx.scope.set_exported("TZ", Value::String("Asia/Tokyo".into()));
        let result = date.execute_argv(argv(&["-d", "@1700000000", "+%H"]), &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "07", "TZ var must be honored");

        // And it must differ from the UTC rendering (the footgun was silent equality).
        let utc = run(&date, &["-u", "-d", "@1700000000", "+%H"]).await;
        assert_eq!(utc.text_out().trim(), "22");
    }

    #[tokio::test]
    async fn unknown_timezone_is_loud_error() {
        let result = run(&fixed_date(), &["--tz", "Mars/Olympus", "-d", "now"]).await;
        assert_eq!(result.code, 2);
    }

    // --- `-d` grammar -------------------------------------------------------

    #[tokio::test]
    async fn relative_yesterday_tomorrow() {
        let y = run(&fixed_date(), &["-u", "-d", "yesterday", "+%Y-%m-%d"]).await;
        assert_eq!(y.text_out().trim(), "2023-11-13");
        let t = run(&fixed_date(), &["-u", "-d", "tomorrow", "+%Y-%m-%d"]).await;
        assert_eq!(t.text_out().trim(), "2023-11-15");
    }

    #[tokio::test]
    async fn relative_n_units_ago_and_future() {
        let ago = run(&fixed_date(), &["-u", "-d", "2 weeks ago", "+%Y-%m-%d"]).await;
        assert_eq!(ago.text_out().trim(), "2023-10-31");
        let future = run(&fixed_date(), &["-u", "-d", "+3 days", "+%Y-%m-%d"]).await;
        assert_eq!(future.text_out().trim(), "2023-11-17");
        let bare_future = run(&fixed_date(), &["-u", "-d", "1 month", "+%Y-%m-%d"]).await;
        assert_eq!(bare_future.text_out().trim(), "2023-12-14");
    }

    #[tokio::test]
    async fn relative_next_last_weekday() {
        // 2023-11-14 is a Tuesday.
        let next_fri = run(&fixed_date(), &["-u", "-d", "next friday", "+%Y-%m-%d"]).await;
        assert_eq!(next_fri.text_out().trim(), "2023-11-17");
        let last_fri = run(&fixed_date(), &["-u", "-d", "last friday", "+%Y-%m-%d"]).await;
        assert_eq!(last_fri.text_out().trim(), "2023-11-10");
        // "next tuesday" off a Tuesday is +7 days.
        let next_tue = run(&fixed_date(), &["-u", "-d", "next tuesday", "+%Y-%m-%d"]).await;
        assert_eq!(next_tue.text_out().trim(), "2023-11-21");
    }

    #[tokio::test]
    async fn absolute_date_and_epoch() {
        let abs = run(&fixed_date(), &["-u", "-d", "2026-01-01", "+%s"]).await;
        assert!(abs.ok());
        let expected = NaiveDate::from_ymd_opt(2026, 1, 1)
            .unwrap()
            .and_hms_opt(0, 0, 0)
            .unwrap()
            .and_utc()
            .timestamp();
        assert_eq!(abs.text_out().trim(), expected.to_string());
    }

    #[tokio::test]
    async fn absolute_plus_offset() {
        // The nested last-day-of-previous-month idiom.
        let r = run(&fixed_date(), &["-u", "-d", "2026-06-01 -1 day", "+%Y-%m-%d"]).await;
        assert_eq!(r.text_out().trim(), "2026-05-31");
    }

    #[tokio::test]
    async fn garbage_date_string_fails_loud() {
        let r = run(&fixed_date(), &["-d", "the day before the war"]).await;
        assert_eq!(r.code, 2);
    }

    // --- Aliases & formats --------------------------------------------------

    #[tokio::test]
    async fn iso_8601_default_is_date() {
        let r = run(&fixed_date(), &["-u", "-I"]).await;
        assert!(r.ok());
        assert_eq!(r.text_out().trim(), "2023-11-14");
    }

    #[tokio::test]
    async fn iso_8601_seconds() {
        let r = run(&fixed_date(), &["-u", "--iso-8601=seconds"]).await;
        assert!(r.ok());
        assert_eq!(r.text_out().trim(), "2023-11-14T22:13:20+00:00");
    }

    #[tokio::test]
    async fn rfc_2822() {
        let r = run(&fixed_date(), &["-u", "-R"]).await;
        assert!(r.ok());
        assert_eq!(r.text_out().trim(), "Tue, 14 Nov 2023 22:13:20 +0000");
    }

    #[tokio::test]
    async fn default_and_utc() {
        let r = run(&fixed_date(), &["-u"]).await;
        assert!(r.ok());
        assert_eq!(r.text_out().trim(), "2023-11-14 22:13:20");
    }

    #[tokio::test]
    async fn epoch_via_format() {
        let r = run(&fixed_date(), &["-u", "+%s"]).await;
        assert_eq!(r.text_out().trim(), "1700000000");
    }

    #[tokio::test]
    async fn stray_operand_is_error() {
        let r = run(&fixed_date(), &["2023-11-14"]).await;
        assert_eq!(r.code, 2, "bare date operand should hint -d");
    }

    // --- -r FILE ------------------------------------------------------------

    #[tokio::test]
    async fn reference_reads_file_mtime() {
        // The fixed clock is pinned to 2023; the file is written "now" (real,
        // 2026+), so -r must read the file's mtime, not the clock.
        let date = fixed_date();
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("file.txt"), b"hi").await.unwrap();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let result = date.execute_argv(argv(&["-u", "-r", "/file.txt", "+%s"]), &mut ctx).await;
        assert!(result.ok());
        let file_epoch: i64 = result.text_out().trim().parse().unwrap();
        assert!(
            file_epoch > FIXED_EPOCH,
            "-r should read the file mtime ({file_epoch}), not the clock ({FIXED_EPOCH})"
        );
    }

    #[tokio::test]
    async fn reference_missing_file_errors() {
        let r = run(&fixed_date(), &["-r", "/nonexistent"]).await;
        assert_eq!(r.code, 2);
    }

    // --- --json -------------------------------------------------------------

    /// Parse the `--json` object emitted for `argv` under the fixed clock.
    async fn json_for(parts: &[&str]) -> serde_json::Value {
        let mut ctx = make_ctx();
        let result = fixed_date().execute_argv(argv(parts), &mut ctx).await;
        let json = apply_output_format(result, OutputFormat::Json);
        serde_json::from_str(json.text_out().trim()).expect("date --json is JSON")
    }

    #[tokio::test]
    async fn json_shape_is_consistent_utc() {
        let p = json_for(&["-u"]).await;
        // Every field pinned at the fixed instant.
        assert_eq!(p["epoch"], FIXED_EPOCH);
        assert_eq!(p["iso"], "2023-11-14T22:13:20+00:00");
        assert_eq!(p["utc"], "2023-11-14T22:13:20Z");
        assert_eq!(p["local"], "2023-11-14 22:13:20");
        assert_eq!(p["weekday"], "Tuesday");
        assert_eq!(p["tz"], "+00:00");
        assert_eq!(p["offset_seconds"], 0);
    }

    #[tokio::test]
    async fn json_carries_zone_offset() {
        // A named zone must show through every field, not collapse to UTC.
        let p = json_for(&["--tz", "Asia/Tokyo", "-d", "@1700000000"]).await;
        assert_eq!(p["epoch"], 1_700_000_000_i64);
        assert_eq!(p["iso"], "2023-11-15T07:13:20+09:00");
        assert_eq!(p["utc"], "2023-11-14T22:13:20Z");
        assert_eq!(p["local"], "2023-11-15 07:13:20");
        assert_eq!(p["weekday"], "Wednesday");
        assert_eq!(p["tz"], "+09:00");
        assert_eq!(p["offset_seconds"], 32_400);
    }

    // --- DST correctness (the fixed-offset-math footgun) --------------------

    #[tokio::test]
    async fn calendar_math_is_dst_correct() {
        // US DST springs forward 2026-03-08 02:00 (EST -05:00 → EDT -04:00).
        // Base: 2026-03-06 12:00 New York (EST) = 17:00 UTC. +3 calendar days
        // lands 2026-03-09 12:00 — which is EDT. The wall clock must stay 12:00
        // and the offset must flip to -04:00; fixed-offset math would keep
        // -05:00 (a wrong instant + wrong wall time).
        let instant = Utc.with_ymd_and_hms(2026, 3, 6, 17, 0, 0).unwrap();
        let date = Date::with_clock(Arc::new(FixedClock(instant)));
        let mut ctx = make_ctx();
        let r = date
            .execute_argv(
                argv(&["--tz", "America/New_York", "-d", "+3 days", "+%Y-%m-%dT%H:%M%:z"]),
                &mut ctx,
            )
            .await;
        assert!(r.ok());
        assert_eq!(r.text_out().trim(), "2026-03-09T12:00-04:00");
    }

    // --- format path: %N translation + validation ---------------------------

    #[tokio::test]
    async fn nanos_width_variants_translate() {
        // %3N/%6N/%9N → ms/µs/ns; whole-second clock → zero-padded.
        assert_eq!(run(&fixed_date(), &["-u", "+%3N"]).await.text_out().trim(), "000");
        assert_eq!(run(&fixed_date(), &["-u", "+%6N"]).await.text_out().trim(), "000000");
        assert_eq!(run(&fixed_date(), &["-u", "+%9N"]).await.text_out().trim(), "000000000");
        assert_eq!(run(&fixed_date(), &["-u", "+%N"]).await.text_out().trim(), "000000000");
    }

    #[tokio::test]
    async fn escaped_percent_n_is_literal() {
        // %%N is a literal percent then a literal N — NOT a nanos specifier.
        let r = run(&fixed_date(), &["-u", "+%%N"]).await;
        assert!(r.ok());
        assert_eq!(r.text_out().trim(), "%N");
    }

    #[tokio::test]
    async fn nanos_specifier_mid_string() {
        let r = run(&fixed_date(), &["-u", "+%H:%N"]).await;
        assert!(r.ok());
        assert_eq!(r.text_out().trim(), "22:000000000");
    }

    #[tokio::test]
    async fn trailing_bare_percent_errors() {
        let r = run(&fixed_date(), &["-u", "+%"]).await;
        assert_eq!(r.code, 2, "a dangling % must not panic; exit 2");
    }

    // --- output-mode aliases ------------------------------------------------

    #[tokio::test]
    async fn iso_8601_all_variants() {
        let v = |fmt: &'static str| async move {
            run(&fixed_date(), &["-u", fmt]).await.text_out().trim().to_string()
        };
        assert_eq!(v("--iso-8601=hours").await, "2023-11-14T22+00:00");
        assert_eq!(v("--iso-8601=minutes").await, "2023-11-14T22:13+00:00");
        assert_eq!(v("--iso-8601=ns").await, "2023-11-14T22:13:20,000000000+00:00");
    }

    #[tokio::test]
    async fn iso_8601_bad_fmt_errors() {
        let r = run(&fixed_date(), &["-u", "--iso-8601=bananas"]).await;
        assert_eq!(r.code, 2);
    }

    #[tokio::test]
    async fn rfc_3339_variants_and_bad_fmt() {
        assert_eq!(
            run(&fixed_date(), &["-u", "--rfc-3339=date"]).await.text_out().trim(),
            "2023-11-14"
        );
        assert_eq!(
            run(&fixed_date(), &["-u", "--rfc-3339=seconds"]).await.text_out().trim(),
            "2023-11-14 22:13:20+00:00"
        );
        assert_eq!(
            run(&fixed_date(), &["-u", "--rfc-3339=ns"]).await.text_out().trim(),
            "2023-11-14 22:13:20.000000000+00:00"
        );
        assert_eq!(run(&fixed_date(), &["-u", "--rfc-3339=nope"]).await.code, 2);
    }

    #[tokio::test]
    async fn legacy_hidden_aliases_still_work() {
        assert_eq!(run(&fixed_date(), &["-u", "--unix"]).await.text_out().trim(), "1700000000");
        // --iso maps to seconds-resolution rfc3339.
        assert_eq!(
            run(&fixed_date(), &["-u", "--iso"]).await.text_out().trim(),
            "2023-11-14T22:13:20+00:00"
        );
        assert_eq!(
            run(&fixed_date(), &["-u", "--format", "%Y"]).await.text_out().trim(),
            "2023"
        );
    }

    // --- -d grammar depth ---------------------------------------------------

    #[tokio::test]
    async fn relative_unit_spellings() {
        let f = |spec: &'static str| async move {
            run(&fixed_date(), &["-u", "-d", spec, "+%Y-%m-%dT%H:%M:%S"]).await.text_out().trim().to_string()
        };
        // base = 2023-11-14T22:13:20Z
        assert_eq!(f("1 second").await, "2023-11-14T22:13:21");
        assert_eq!(f("2 secs").await, "2023-11-14T22:13:22");
        assert_eq!(f("1 minute").await, "2023-11-14T22:14:20");
        assert_eq!(f("3 mins").await, "2023-11-14T22:16:20");
        assert_eq!(f("1 hour").await, "2023-11-14T23:13:20");
        assert_eq!(f("2 hrs").await, "2023-11-15T00:13:20");
        assert_eq!(f("1 day").await, "2023-11-15T22:13:20");
        assert_eq!(f("1 wk").await, "2023-11-21T22:13:20");
        assert_eq!(f("1 yr").await, "2024-11-14T22:13:20");
        assert_eq!(f("2 years").await, "2025-11-14T22:13:20");
    }

    #[tokio::test]
    async fn relative_explicit_negative_and_in_prefix() {
        // A hyphen-leading value must use the `=` form so clap doesn't read it
        // as a flag — this is exactly what the kernel's `to_argv()` emits
        // (`-d=-5 days`), so it mirrors production. Verified live through the
        // kernel too: `date -u -d "-5 days"` works.
        assert_eq!(
            run(&fixed_date(), &["-u", "-d=-5 days", "+%Y-%m-%d"]).await.text_out().trim(),
            "2023-11-09"
        );
        assert_eq!(
            run(&fixed_date(), &["-u", "-d", "in 3 weeks", "+%Y-%m-%d"]).await.text_out().trim(),
            "2023-12-05"
        );
    }

    #[tokio::test]
    async fn relative_multi_term() {
        // base 22:13:20 + 1 hour 30 minutes.
        assert_eq!(
            run(&fixed_date(), &["-u", "-d", "1 hour 30 minutes", "+%H:%M"]).await.text_out().trim(),
            "23:43"
        );
        // 1 day 2 hours ago → both terms negated.
        assert_eq!(
            run(&fixed_date(), &["-u", "-d", "1 day 2 hours ago", "+%Y-%m-%dT%H:%M"]).await.text_out().trim(),
            "2023-11-13T20:13"
        );
    }

    #[tokio::test]
    async fn absolute_datetime_with_time() {
        assert_eq!(
            run(&fixed_date(), &["-u", "-d", "2026-01-02T09:30:00", "+%Y-%m-%dT%H:%M:%S"]).await.text_out().trim(),
            "2026-01-02T09:30:00"
        );
        assert_eq!(
            run(&fixed_date(), &["-u", "-d", "2026-01-02 09:30", "+%H:%M"]).await.text_out().trim(),
            "09:30"
        );
    }

    #[tokio::test]
    async fn weekday_full_names() {
        // base Tuesday 2023-11-14
        assert_eq!(
            run(&fixed_date(), &["-u", "-d", "next monday", "+%Y-%m-%d"]).await.text_out().trim(),
            "2023-11-20"
        );
        assert_eq!(
            run(&fixed_date(), &["-u", "-d", "last sunday", "+%Y-%m-%d"]).await.text_out().trim(),
            "2023-11-12"
        );
    }

    #[tokio::test]
    async fn now_and_today_equal_clock() {
        let now = run(&fixed_date(), &["-u", "-d", "now", "+%s"]).await;
        let today = run(&fixed_date(), &["-u", "-d", "today", "+%s"]).await;
        assert_eq!(now.text_out().trim(), FIXED_EPOCH.to_string());
        assert_eq!(today.text_out().trim(), FIXED_EPOCH.to_string());
    }

    // --- negative / error paths ---------------------------------------------

    #[tokio::test]
    async fn date_and_reference_conflict() {
        let r = run(&fixed_date(), &["-d", "yesterday", "-r", "/tmp/x"]).await;
        assert_eq!(r.code, 2, "--date and --reference must not combine");
    }

    #[tokio::test]
    async fn empty_date_string_errors() {
        assert_eq!(run(&fixed_date(), &["-d", ""]).await.code, 2);
        assert_eq!(run(&fixed_date(), &["-d", "   "]).await.code, 2);
    }

    #[tokio::test]
    async fn bad_weekday_errors() {
        let r = run(&fixed_date(), &["-d", "next blursday"]).await;
        assert_eq!(r.code, 2);
    }

    #[tokio::test]
    async fn overflow_year_errors_not_panics() {
        let r = run(&fixed_date(), &["-d", "999999999999 years ago"]).await;
        assert_eq!(r.code, 2, "absurd offset must error cleanly, not panic");
    }

    #[tokio::test]
    async fn utc_flag_overrides_tz_var() {
        let date = fixed_date();
        let mut ctx = make_ctx();
        ctx.scope.set_exported("TZ", Value::String("Asia/Tokyo".into()));
        // -u must win over $TZ.
        let r = date.execute_argv(argv(&["-u", "-d", "@1700000000", "+%H"]), &mut ctx).await;
        assert_eq!(r.text_out().trim(), "22");
    }
}
