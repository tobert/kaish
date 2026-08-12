//! Job identification and status types.

use std::path::PathBuf;
use std::time::SystemTime;

use serde::{Deserialize, Serialize};

use crate::clock;

/// Unique identifier for a background job.
///
/// `Ord`/`PartialOrd` order by the wrapped id — job ids are minted in
/// increasing order (`JobManager`'s `next_id` counter), so sorting by
/// `JobId` gives spawn order (GH #247: `JobManager::list`/`list_ids`
/// previously iterated a `HashMap` in arbitrary order).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(transparent)]
pub struct JobId(pub u64);

impl std::fmt::Display for JobId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Status of a background job.
///
/// Wire spelling (via `Serialize`/`Deserialize`) is lowercase — `"running"`,
/// `"stopped"`, `"done"`, `"gated"`, `"killed"`, `"failed"` — matching the existing
/// `/v/jobs/N/status` text vocabulary (`Job::status_string`), not the
/// capitalized `Display` impl (which stays capitalized for human-facing
/// text: the `jobs` table, `[N]+ Done ...` notifications). This is now the
/// pinned wire shape for `jobs --json` and any embedder that deserializes
/// `JobInfo` — see the round-trip tests below.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
#[serde(rename_all = "lowercase")]
pub enum JobStatus {
    /// Job is currently running.
    Running,
    /// Job was stopped by a signal (e.g., Ctrl-Z / SIGTSTP).
    Stopped,
    /// Job completed successfully.
    Done,
    /// Job was terminated by `kill %N` (or an embedder's cancel) and has
    /// unwound. Terminal, like `Failed`, but distinguishes "someone killed
    /// it" from "it errored on its own" — before this variant existed the
    /// killed job was deleted outright, so "I killed job 1" and "job 1 never
    /// existed" were indistinguishable (GH #244). The job's cached result and
    /// output stay readable until the job is reaped.
    Killed,
    /// Job failed with an error.
    Failed,
}

impl std::fmt::Display for JobStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JobStatus::Running => write!(f, "Running"),
            JobStatus::Stopped => write!(f, "Stopped"),
            JobStatus::Done => write!(f, "Done"),
            JobStatus::Killed => write!(f, "Killed"),
            JobStatus::Failed => write!(f, "Failed"),
        }
    }
}

/// Information about a job for listing.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
pub struct JobInfo {
    /// Job ID.
    pub id: JobId,
    /// Command description.
    pub command: String,
    /// Current status.
    pub status: JobStatus,
    /// Path to output file (if available).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub output_file: Option<PathBuf>,
    /// OS process ID (if this is a stopped/foreground process). Only ever
    /// set for a Ctrl-Z-stopped foreground job — an embedder (no TTY) will
    /// never see this populated; see [`Self::pgids`] for the surface that
    /// actually covers embedder-spawned externals.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub pid: Option<u32>,
    /// The job's exit code, once finished. `None` while `Running`/`Stopped`.
    /// GH #243: previously the only way to learn *how* a job failed was to
    /// string-parse `failed:{code}` off `/v/jobs/N/status` or block on
    /// `JobManager::wait`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub exit_code: Option<i64>,
    /// Wall-clock time the job started running (spawn/registration time).
    /// Acquired via [`crate::clock::system_now`], not `SystemTime::now()`
    /// directly, so this stays valid on `wasm32-unknown-unknown`. On the wire
    /// this is an RFC 3339 UTC string with millisecond precision
    /// (`"2026-08-02T14:29:00.123Z"`) — see [`crate::rfc3339`].
    #[serde(with = "crate::rfc3339::system_time")]
    #[cfg_attr(feature = "schema", schemars(schema_with = "crate::rfc3339::schema"))]
    pub started_at: SystemTime,
    /// Wall-clock time the job finished, if it has. `None` while
    /// `Running`/`Stopped`. Same RFC 3339 wire format as [`Self::started_at`].
    #[serde(
        default,
        skip_serializing_if = "Option::is_none",
        with = "crate::rfc3339::opt_system_time"
    )]
    #[cfg_attr(feature = "schema", schemars(schema_with = "crate::rfc3339::opt_schema"))]
    pub finished_at: Option<SystemTime>,
    /// OS process groups spawned by this job's external children (so
    /// `kill -<sig> %N` can signal them, and an embedder can see what's
    /// actually running). Empty for a pure-builtin job. GH #243: the real
    /// surface for "what is this job doing", since `pid` above almost never
    /// applies to an embedder-created job.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub pgids: Vec<u32>,
}

impl JobInfo {
    /// Create a `JobInfo` with the required fields; `output_file`/`pid`/
    /// `exit_code`/`finished_at` default to `None`, `pgids` to empty,
    /// and `started_at` to now (callers that track a job's real start time —
    /// i.e. `JobManager` — override it via [`Self::with_started_at`]). Chain
    /// the `with_*` setters to fill in the rest.
    ///
    /// `#[non_exhaustive]` blocks struct-literal construction from outside this
    /// crate — this constructor plus the setters below are the replacement.
    pub fn new(id: JobId, command: impl Into<String>, status: JobStatus) -> Self {
        Self {
            id,
            command: command.into(),
            status,
            output_file: None,
            pid: None,
            exit_code: None,
            started_at: clock::system_now(),
            finished_at: None,
            pgids: Vec::new(),
        }
    }

    /// Set the output file path.
    pub fn with_output_file(mut self, output_file: Option<PathBuf>) -> Self {
        self.output_file = output_file;
        self
    }

    /// Set the OS process ID.
    pub fn with_pid(mut self, pid: Option<u32>) -> Self {
        self.pid = pid;
        self
    }

    /// Set the exit code (see [`Self::exit_code`]).
    pub fn with_exit_code(mut self, exit_code: Option<i64>) -> Self {
        self.exit_code = exit_code;
        self
    }

    /// Set the job's real start time (see [`Self::started_at`]).
    pub fn with_started_at(mut self, started_at: SystemTime) -> Self {
        self.started_at = started_at;
        self
    }

    /// Set the job's finish time (see [`Self::finished_at`]).
    pub fn with_finished_at(mut self, finished_at: Option<SystemTime>) -> Self {
        self.finished_at = finished_at;
        self
    }

    /// Set the recorded process groups (see [`Self::pgids`]).
    pub fn with_pgids(mut self, pgids: Vec<u32>) -> Self {
        self.pgids = pgids;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_defaults_optional_fields_to_none() {
        let before = std::time::SystemTime::now();
        let info = JobInfo::new(JobId(1), "echo hi", JobStatus::Running);
        assert_eq!(info.id, JobId(1));
        assert_eq!(info.command, "echo hi");
        assert_eq!(info.status, JobStatus::Running);
        assert!(info.output_file.is_none());
        assert!(info.pid.is_none());
        assert!(info.exit_code.is_none());
        assert!(info.finished_at.is_none());
        assert!(info.pgids.is_empty());
        // started_at defaults to "now" — bounded sanity check, not exact.
        assert!(
            info.started_at >= before,
            "started_at should default to roughly now"
        );
        assert!(
            info.started_at.duration_since(before).unwrap_or_default() < std::time::Duration::from_secs(5),
            "started_at default drifted too far from now"
        );
    }

    #[test]
    fn with_setters_chain_and_override_defaults() {
        let started = std::time::SystemTime::UNIX_EPOCH + std::time::Duration::from_secs(1_000);
        let finished = std::time::SystemTime::UNIX_EPOCH + std::time::Duration::from_secs(1_010);
        let info = JobInfo::new(JobId(2), "sleep 1", JobStatus::Done)
            .with_output_file(Some(PathBuf::from("job-output.txt")))
            .with_pid(Some(1234))
            .with_exit_code(Some(0))
            .with_started_at(started)
            .with_finished_at(Some(finished))
            .with_pgids(vec![4242, 4243]);
        assert_eq!(info.output_file, Some(PathBuf::from("job-output.txt")));
        assert_eq!(info.pid, Some(1234));
        assert_eq!(info.exit_code, Some(0));
        assert_eq!(info.started_at, started);
        assert_eq!(info.finished_at, Some(finished));
        assert_eq!(info.pgids, vec![4242, 4243]);
    }

    // ── serde: JobId ──

    #[test]
    fn job_id_serializes_transparent() {
        assert_eq!(serde_json::to_value(JobId(42)).unwrap(), serde_json::json!(42));
        let back: JobId = serde_json::from_value(serde_json::json!(42)).unwrap();
        assert_eq!(back, JobId(42));
    }

    // ── serde: JobStatus wire spelling (pinned — API once kaijutsu depends on it) ──

    #[test]
    fn job_status_json_spelling_is_lowercase() {
        // Pin the exact wire spelling. Deliberately lowercase, matching the
        // existing `/v/jobs/N/status` vocabulary (`running`/`done:0`/`gated`/
        // `failed:N`) rather than the capitalized `Display` impl — `Display`
        // stays capitalized for human-facing text (the `jobs` table).
        assert_eq!(serde_json::to_string(&JobStatus::Running).unwrap(), "\"running\"");
        assert_eq!(serde_json::to_string(&JobStatus::Stopped).unwrap(), "\"stopped\"");
        assert_eq!(serde_json::to_string(&JobStatus::Done).unwrap(), "\"done\"");
        assert_eq!(serde_json::to_string(&JobStatus::Killed).unwrap(), "\"killed\"");
        assert_eq!(serde_json::to_string(&JobStatus::Failed).unwrap(), "\"failed\"");
    }

    #[test]
    fn job_status_round_trips_through_serde() {
        for status in [
            JobStatus::Running,
            JobStatus::Stopped,
            JobStatus::Done,
            JobStatus::Killed,
            JobStatus::Failed,
        ] {
            let json = serde_json::to_string(&status).unwrap();
            let back: JobStatus = serde_json::from_str(&json).unwrap();
            assert_eq!(back, status);
        }
    }

    // ── serde: JobInfo round-trip, including the approval payload ──

    #[test]
    fn job_info_omits_unset_optional_fields_from_the_wire() {
        // A plain running job (the common case) must not carry dead weight:
        // no output_file/pid/approval/exit_code/finished_at, no pgids array.
        let info = JobInfo::new(JobId(4), "sleep 5", JobStatus::Running);
        let json = serde_json::to_value(&info).unwrap();
        let obj = json.as_object().unwrap();
        assert!(!obj.contains_key("output_file"), "{json}");
        assert!(!obj.contains_key("pid"), "{json}");
        assert!(!obj.contains_key("approval"), "{json}");
        assert!(!obj.contains_key("exit_code"), "{json}");
        assert!(!obj.contains_key("finished_at"), "{json}");
        assert!(!obj.contains_key("pgids"), "{json}");
        // Required fields always present.
        assert!(obj.contains_key("started_at"), "{json}");
        assert!(obj.contains_key("status"), "{json}");
    }

    // ── serde: timestamps are RFC 3339 UTC strings (wire format pinned) ──

    #[test]
    fn job_info_timestamps_serialize_as_rfc3339_utc_strings() {
        // 1_700_000_000s past the epoch is 2023-11-14T22:13:20Z. Exactly three
        // fractional digits, truncated never rounded, `Z` only — fixed width
        // keeps string order equal to time order.
        let started = std::time::UNIX_EPOCH + std::time::Duration::new(1_700_000_000, 123_456_789);
        let finished = std::time::UNIX_EPOCH + std::time::Duration::from_secs(1_700_000_005);
        let info = JobInfo::new(JobId(6), "sleep 5", JobStatus::Done)
            .with_started_at(started)
            .with_finished_at(Some(finished));
        let json = serde_json::to_value(&info).unwrap();
        assert_eq!(json["started_at"], "2023-11-14T22:13:20.123Z", "{json}");
        assert_eq!(json["finished_at"], "2023-11-14T22:13:25.000Z", "{json}");
    }

    #[test]
    fn job_info_timestamps_parse_second_through_nanosecond_precision() {
        let base = serde_json::json!({
            "id": 7, "command": "x", "status": "done",
            "started_at": "2023-11-14T22:13:20Z",
        });
        let back: JobInfo = serde_json::from_value(base).unwrap();
        assert_eq!(
            back.started_at,
            std::time::UNIX_EPOCH + std::time::Duration::from_secs(1_700_000_000)
        );

        let nanos = serde_json::json!({
            "id": 7, "command": "x", "status": "done",
            "started_at": "2023-11-14T22:13:20.123456789Z",
        });
        let back: JobInfo = serde_json::from_value(nanos).unwrap();
        assert_eq!(
            back.started_at,
            std::time::UNIX_EPOCH + std::time::Duration::new(1_700_000_000, 123_456_789)
        );
    }

    #[test]
    fn job_info_timestamps_reject_junk_loud() {
        // One spelling on the wire: `Z` only (kaish never emits an offset, so
        // it does not accept one), `T` separator, real calendar dates, nothing
        // before the epoch.
        for bad in [
            "2023-11-14 22:13:20Z",      // space separator
            "2023-11-14T22:13:20",       // missing zone
            "2023-11-14T22:13:20+00:00", // offset instead of Z
            "1969-12-31T23:59:59Z",      // before the epoch
            "2023-13-01T00:00:00Z",      // month 13
            "2023-02-29T00:00:00Z",      // not a leap year
            "2023-11-14T24:00:00Z",      // hour 24
            "not-a-time",
        ] {
            let v = serde_json::json!({
                "id": 8, "command": "x", "status": "done", "started_at": bad,
            });
            let r: Result<JobInfo, _> = serde_json::from_value(v);
            assert!(r.is_err(), "{bad:?} must be rejected");
            let msg = r.unwrap_err().to_string();
            assert!(
                msg.contains("RFC 3339"),
                "error for {bad:?} must name the expected format: {msg}"
            );
        }
    }

    #[test]
    fn job_info_leap_day_round_trips() {
        let leap = std::time::UNIX_EPOCH + std::time::Duration::from_secs(1_709_164_800);
        let info = JobInfo::new(JobId(9), "x", JobStatus::Done).with_started_at(leap);
        let json = serde_json::to_value(&info).unwrap();
        assert_eq!(json["started_at"], "2024-02-29T00:00:00.000Z", "{json}");
        let back: JobInfo = serde_json::from_value(json).unwrap();
        assert_eq!(back.started_at, leap);
    }

    #[test]
    fn job_info_exit_code_present_when_job_failed() {
        // GH #243(a): a job that exited 42 must surface the code, not just
        // "Failed" — this is the exact bug the audit verified against
        // `jobs --json`.
        let info = JobInfo::new(JobId(5), "sh -c 'exit 42'", JobStatus::Failed)
            .with_exit_code(Some(42));
        let json = serde_json::to_value(&info).unwrap();
        assert_eq!(json["exit_code"], 42);
        assert_eq!(json["status"], "failed");
    }
}
