//! State persistence for kaish kernels.
//!
//! Simplified to history + checkpoints only:
//! - History: REPL command recall, debugging, and agent context
//! - Checkpoints: History distillation for agent context windows
//!
//! Variables, CWD, and $? are kept in-memory per session.
//!
//! State is stored at `$XDG_DATA_HOME/kaish/kernels/{id}.db`.

pub mod paths;

use std::path::Path;

use anyhow::{Context, Result};
use rusqlite::{params, Connection, OpenFlags};

use crate::ast::Value;
use crate::interpreter::ExecResult;

/// Schema SQL embedded from schema/state.sql.
const SCHEMA_SQL: &str = include_str!("../../../../schema/state.sql");

/// Persistent state store backed by SQLite.
///
/// Stores history and checkpoints only. Variables and other runtime
/// state are kept in-memory per session.
pub struct StateStore {
    conn: Connection,
}

impl StateStore {
    /// Open or create a state database at the given path.
    ///
    /// Creates parent directories and initializes schema if needed.
    pub fn open(path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();

        // Ensure parent directory exists
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)
                .with_context(|| format!("creating state directory: {}", parent.display()))?;
        }

        let conn = Connection::open_with_flags(
            path,
            OpenFlags::SQLITE_OPEN_READ_WRITE
                | OpenFlags::SQLITE_OPEN_CREATE
                | OpenFlags::SQLITE_OPEN_NO_MUTEX,
        )
        .with_context(|| format!("opening state database: {}", path.display()))?;

        let store = Self { conn };
        store.init_schema()?;
        Ok(store)
    }

    /// Create an in-memory state store (for testing or ephemeral kernels).
    pub fn in_memory() -> Result<Self> {
        let conn = Connection::open_in_memory()
            .context("creating in-memory state database")?;
        let store = Self { conn };
        store.init_schema()?;
        Ok(store)
    }

    /// Initialize the database schema.
    fn init_schema(&self) -> Result<()> {
        self.conn
            .execute_batch(SCHEMA_SQL)
            .context("initializing state schema")?;
        Ok(())
    }

    // ================================================================
    // Metadata
    // ================================================================

    /// Get a metadata value.
    pub fn get_meta(&self, key: &str) -> Result<Option<String>> {
        let result = self.conn.query_row(
            "SELECT value FROM meta WHERE key = ?1",
            params![key],
            |row| row.get(0),
        );

        match result {
            Ok(value) => Ok(Some(value)),
            Err(rusqlite::Error::QueryReturnedNoRows) => Ok(None),
            Err(e) => Err(e).context(format!("loading meta: {}", key)),
        }
    }

    /// Set a metadata value.
    pub fn set_meta(&self, key: &str, value: &str) -> Result<()> {
        self.conn.execute(
            "INSERT OR REPLACE INTO meta (key, value) VALUES (?1, ?2)",
            params![key, value],
        ).with_context(|| format!("saving meta: {}", key))?;
        Ok(())
    }

    /// Get the session ID.
    pub fn session_id(&self) -> Result<String> {
        self.get_meta("session_id")
            .map(|opt| opt.unwrap_or_else(|| "unknown".to_string()))
    }

    // ================================================================
    // History
    // ================================================================

    /// Record an execution in history.
    pub fn record_history(&self, entry: &HistoryEntry) -> Result<i64> {
        let data_json = entry.result_data.as_ref().map(|v| {
            let json = value_to_json(v);
            serde_json::to_string(&json).unwrap_or_default()
        });

        self.conn.execute(
            "INSERT INTO history (code, code_hash, result_code, result_ok, result_out, result_err, result_data_json, duration_ms)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)",
            params![
                entry.code,
                entry.code_hash,
                entry.result_code,
                entry.result_ok as i32,
                entry.result_out,
                entry.result_err,
                data_json,
                entry.duration_ms,
            ],
        ).context("recording history")?;

        Ok(self.conn.last_insert_rowid())
    }

    /// Get recent history entries.
    pub fn get_history(&self, limit: usize) -> Result<Vec<HistoryEntry>> {
        let mut stmt = self.conn.prepare(
            "SELECT id, code, code_hash, result_code, result_ok, result_out, result_err, result_data_json, duration_ms, created_at
             FROM history ORDER BY id DESC LIMIT ?1"
        )?;

        let results = stmt.query_map(params![limit as i64], |row| {
            Ok(HistoryRow {
                id: row.get(0)?,
                code: row.get(1)?,
                code_hash: row.get(2)?,
                result_code: row.get(3)?,
                result_ok: row.get(4)?,
                result_out: row.get(5)?,
                result_err: row.get(6)?,
                result_data_json: row.get(7)?,
                duration_ms: row.get(8)?,
                created_at: row.get(9)?,
            })
        })?;

        let mut entries = Vec::new();
        for result in results {
            let row = result?;
            let result_data = row.result_data_json.and_then(|s| {
                serde_json::from_str::<serde_json::Value>(&s)
                    .ok()
                    .map(|json| json_to_value(&json))
            });

            entries.push(HistoryEntry {
                id: Some(row.id),
                code: row.code,
                code_hash: row.code_hash,
                result_code: row.result_code,
                result_ok: row.result_ok != 0,
                result_out: row.result_out,
                result_err: row.result_err,
                result_data,
                duration_ms: row.duration_ms,
                created_at: row.created_at,
            });
        }

        // Reverse to get chronological order
        entries.reverse();
        Ok(entries)
    }

    /// Get history entries in reverse chronological order (most recent first).
    /// This is useful for REPL history where you want newest first.
    pub fn get_history_reverse(&self, limit: usize) -> Result<Vec<HistoryEntry>> {
        let mut stmt = self.conn.prepare(
            "SELECT id, code, code_hash, result_code, result_ok, result_out, result_err, result_data_json, duration_ms, created_at
             FROM history ORDER BY id DESC LIMIT ?1"
        )?;

        let results = stmt.query_map(params![limit as i64], |row| {
            Ok(HistoryRow {
                id: row.get(0)?,
                code: row.get(1)?,
                code_hash: row.get(2)?,
                result_code: row.get(3)?,
                result_ok: row.get(4)?,
                result_out: row.get(5)?,
                result_err: row.get(6)?,
                result_data_json: row.get(7)?,
                duration_ms: row.get(8)?,
                created_at: row.get(9)?,
            })
        })?;

        let mut entries = Vec::new();
        for result in results {
            let row = result?;
            let result_data = row.result_data_json.and_then(|s| {
                serde_json::from_str::<serde_json::Value>(&s)
                    .ok()
                    .map(|json| json_to_value(&json))
            });

            entries.push(HistoryEntry {
                id: Some(row.id),
                code: row.code,
                code_hash: row.code_hash,
                result_code: row.result_code,
                result_ok: row.result_ok != 0,
                result_out: row.result_out,
                result_err: row.result_err,
                result_data,
                duration_ms: row.duration_ms,
                created_at: row.created_at,
            });
        }

        Ok(entries)
    }

    /// Get all history codes (command strings) for REPL history integration.
    /// Returns commands in chronological order (oldest first).
    pub fn get_history_codes(&self, limit: usize) -> Result<Vec<String>> {
        let mut stmt = self.conn.prepare(
            "SELECT code FROM history ORDER BY id ASC LIMIT ?1"
        )?;

        let results = stmt.query_map(params![limit as i64], |row| {
            row.get::<_, String>(0)
        })?;

        let codes = results.collect::<std::result::Result<Vec<_>, _>>()?;
        Ok(codes)
    }

    /// Get history count.
    pub fn history_count(&self) -> Result<i64> {
        let count: i64 = self.conn.query_row(
            "SELECT COUNT(*) FROM history",
            [],
            |row| row.get(0),
        )?;
        Ok(count)
    }

    /// Get the latest history ID.
    pub fn latest_history_id(&self) -> Result<Option<i64>> {
        let result = self.conn.query_row(
            "SELECT MAX(id) FROM history",
            [],
            |row| row.get::<_, Option<i64>>(0),
        )?;
        Ok(result)
    }

    /// Clear all history.
    pub fn clear_history(&self) -> Result<()> {
        self.conn.execute("DELETE FROM history", [])
            .context("clearing history")?;
        Ok(())
    }

    // ================================================================
    // Checkpoints
    // ================================================================

    /// Create a checkpoint that covers history up to the given ID.
    pub fn create_checkpoint(&self, checkpoint: &Checkpoint) -> Result<i64> {
        let variables_snapshot = checkpoint.variables_snapshot.as_ref().map(|v| {
            serde_json::to_string(v).unwrap_or_default()
        });

        let metadata_json = checkpoint.metadata.as_ref().map(|v| {
            serde_json::to_string(v).unwrap_or_default()
        });

        self.conn.execute(
            "INSERT INTO checkpoints (name, summary, up_to_history_id, variables_snapshot, metadata_json)
             VALUES (?1, ?2, ?3, ?4, ?5)",
            params![
                checkpoint.name,
                checkpoint.summary,
                checkpoint.up_to_history_id,
                variables_snapshot,
                metadata_json,
            ],
        ).context("creating checkpoint")?;

        Ok(self.conn.last_insert_rowid())
    }

    /// Get the latest checkpoint.
    pub fn latest_checkpoint(&self) -> Result<Option<Checkpoint>> {
        let mut stmt = self.conn.prepare(
            "SELECT id, name, summary, up_to_history_id, variables_snapshot, metadata_json, created_at
             FROM checkpoints ORDER BY id DESC LIMIT 1"
        )?;

        let result = stmt.query_row([], |row| {
            Ok(CheckpointRow {
                id: row.get(0)?,
                name: row.get(1)?,
                summary: row.get(2)?,
                up_to_history_id: row.get(3)?,
                variables_snapshot: row.get(4)?,
                metadata_json: row.get(5)?,
                created_at: row.get(6)?,
            })
        });

        match result {
            Ok(row) => Ok(Some(Checkpoint {
                id: Some(row.id),
                name: row.name,
                summary: row.summary,
                up_to_history_id: row.up_to_history_id,
                variables_snapshot: row.variables_snapshot.and_then(|s| serde_json::from_str(&s).ok()),
                metadata: row.metadata_json.and_then(|s| serde_json::from_str(&s).ok()),
                created_at: row.created_at,
            })),
            Err(rusqlite::Error::QueryReturnedNoRows) => Ok(None),
            Err(e) => Err(e).context("loading latest checkpoint"),
        }
    }

    /// List all checkpoints.
    pub fn list_checkpoints(&self) -> Result<Vec<Checkpoint>> {
        let mut stmt = self.conn.prepare(
            "SELECT id, name, summary, up_to_history_id, variables_snapshot, metadata_json, created_at
             FROM checkpoints ORDER BY id ASC"
        )?;

        let results = stmt.query_map([], |row| {
            Ok(CheckpointRow {
                id: row.get(0)?,
                name: row.get(1)?,
                summary: row.get(2)?,
                up_to_history_id: row.get(3)?,
                variables_snapshot: row.get(4)?,
                metadata_json: row.get(5)?,
                created_at: row.get(6)?,
            })
        })?;

        let mut checkpoints = Vec::new();
        for result in results {
            let row = result?;
            checkpoints.push(Checkpoint {
                id: Some(row.id),
                name: row.name,
                summary: row.summary,
                up_to_history_id: row.up_to_history_id,
                variables_snapshot: row.variables_snapshot.and_then(|s| serde_json::from_str(&s).ok()),
                metadata: row.metadata_json.and_then(|s| serde_json::from_str(&s).ok()),
                created_at: row.created_at,
            });
        }

        Ok(checkpoints)
    }

    /// Get history entries since the last checkpoint.
    pub fn history_since_checkpoint(&self) -> Result<Vec<HistoryEntry>> {
        let last_checkpoint_id = self.latest_checkpoint()?
            .and_then(|c| c.up_to_history_id)
            .unwrap_or(0);

        let mut stmt = self.conn.prepare(
            "SELECT id, code, code_hash, result_code, result_ok, result_out, result_err, result_data_json, duration_ms, created_at
             FROM history WHERE id > ?1 ORDER BY id ASC"
        )?;

        let results = stmt.query_map(params![last_checkpoint_id], |row| {
            Ok(HistoryRow {
                id: row.get(0)?,
                code: row.get(1)?,
                code_hash: row.get(2)?,
                result_code: row.get(3)?,
                result_ok: row.get(4)?,
                result_out: row.get(5)?,
                result_err: row.get(6)?,
                result_data_json: row.get(7)?,
                duration_ms: row.get(8)?,
                created_at: row.get(9)?,
            })
        })?;

        let mut entries = Vec::new();
        for result in results {
            let row = result?;
            let result_data = row.result_data_json.and_then(|s| {
                serde_json::from_str::<serde_json::Value>(&s)
                    .ok()
                    .map(|json| json_to_value(&json))
            });

            entries.push(HistoryEntry {
                id: Some(row.id),
                code: row.code,
                code_hash: row.code_hash,
                result_code: row.result_code,
                result_ok: row.result_ok != 0,
                result_out: row.result_out,
                result_err: row.result_err,
                result_data,
                duration_ms: row.duration_ms,
                created_at: row.created_at,
            });
        }

        Ok(entries)
    }
}

// ================================================================
// History Types
// ================================================================

/// A history entry representing one execution.
#[derive(Debug, Clone)]
pub struct HistoryEntry {
    pub id: Option<i64>,
    pub code: String,
    pub code_hash: Option<String>,
    pub result_code: i64,
    pub result_ok: bool,
    pub result_out: Option<String>,
    pub result_err: Option<String>,
    pub result_data: Option<Value>,
    pub duration_ms: Option<i64>,
    pub created_at: Option<String>,
}

impl HistoryEntry {
    /// Create a new history entry from an execution.
    pub fn from_exec(code: &str, result: &ExecResult, duration_ms: Option<i64>) -> Self {
        Self {
            id: None,
            code: code.to_string(),
            code_hash: None, // Could compute SHA256 here
            result_code: result.code,
            result_ok: result.ok(),
            result_out: if result.out.is_empty() { None } else { Some(result.out.clone()) },
            result_err: if result.err.is_empty() { None } else { Some(result.err.clone()) },
            result_data: result.data.clone(),
            duration_ms,
            created_at: None,
        }
    }
}

/// Internal row type for history queries.
struct HistoryRow {
    id: i64,
    code: String,
    code_hash: Option<String>,
    result_code: i64,
    result_ok: i32,
    result_out: Option<String>,
    result_err: Option<String>,
    result_data_json: Option<String>,
    duration_ms: Option<i64>,
    created_at: Option<String>,
}

// ================================================================
// Checkpoint Types
// ================================================================

/// A checkpoint that distills history into a summary.
#[derive(Debug, Clone)]
pub struct Checkpoint {
    pub id: Option<i64>,
    pub name: Option<String>,
    pub summary: String,
    pub up_to_history_id: Option<i64>,
    pub variables_snapshot: Option<serde_json::Value>,
    pub metadata: Option<serde_json::Value>,
    pub created_at: Option<String>,
}

impl Checkpoint {
    /// Create a new checkpoint.
    pub fn new(summary: impl Into<String>, up_to_history_id: Option<i64>) -> Self {
        Self {
            id: None,
            name: None,
            summary: summary.into(),
            up_to_history_id,
            variables_snapshot: None,
            metadata: None,
            created_at: None,
        }
    }

    /// Add a name to the checkpoint.
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    /// Add a variables snapshot.
    pub fn with_variables(mut self, vars: serde_json::Value) -> Self {
        self.variables_snapshot = Some(vars);
        self
    }

    /// Add metadata.
    pub fn with_metadata(mut self, meta: serde_json::Value) -> Self {
        self.metadata = Some(meta);
        self
    }
}

/// Internal row type for checkpoint queries.
struct CheckpointRow {
    id: i64,
    name: Option<String>,
    summary: String,
    up_to_history_id: Option<i64>,
    variables_snapshot: Option<String>,
    metadata_json: Option<String>,
    created_at: Option<String>,
}

// ================================================================
// Value Serialization (for history data)
// ================================================================

/// Convert a kaish Value to serde_json::Value.
fn value_to_json(value: &Value) -> serde_json::Value {
    match value {
        Value::Null => serde_json::Value::Null,
        Value::Bool(b) => serde_json::Value::Bool(*b),
        Value::Int(i) => serde_json::Value::Number((*i).into()),
        Value::Float(f) => {
            serde_json::Number::from_f64(*f)
                .map(serde_json::Value::Number)
                .unwrap_or(serde_json::Value::Null)
        }
        Value::String(s) => serde_json::Value::String(s.clone()),
    }
}

/// Convert serde_json::Value to a kaish Value.
///
/// Arrays and objects are stored as JSON strings.
fn json_to_value(json: &serde_json::Value) -> Value {
    match json {
        serde_json::Value::Null => Value::Null,
        serde_json::Value::Bool(b) => Value::Bool(*b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::Int(i)
            } else if let Some(f) = n.as_f64() {
                Value::Float(f)
            } else {
                Value::Int(0)
            }
        }
        serde_json::Value::String(s) => Value::String(s.clone()),
        // Arrays and objects are stored as JSON strings
        serde_json::Value::Array(_) | serde_json::Value::Object(_) => {
            Value::String(json.to_string())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_open_in_memory() {
        let store = StateStore::in_memory().expect("should create in-memory store");
        let session_id = store.session_id().expect("should get session_id");
        assert!(!session_id.is_empty());
    }

    #[test]
    fn test_metadata() {
        let store = StateStore::in_memory().expect("store");

        let session_id = store.session_id().expect("session_id");
        assert!(!session_id.is_empty());

        store.set_meta("custom", "value").expect("set");
        let custom = store.get_meta("custom").expect("get").expect("exists");
        assert_eq!(custom, "value");
    }

    // ================================================================
    // History Tests
    // ================================================================

    #[test]
    fn test_record_and_get_history() {
        let store = StateStore::in_memory().expect("store");

        let entry = HistoryEntry {
            id: None,
            code: "echo hello".to_string(),
            code_hash: None,
            result_code: 0,
            result_ok: true,
            result_out: Some("hello".to_string()),
            result_err: None,
            result_data: None,
            duration_ms: Some(5),
            created_at: None,
        };

        let id = store.record_history(&entry).expect("record");
        assert!(id > 0);

        let history = store.get_history(10).expect("get");
        assert_eq!(history.len(), 1);
        assert_eq!(history[0].code, "echo hello");
        assert_eq!(history[0].result_code, 0);
    }

    #[test]
    fn test_history_from_exec() {
        let store = StateStore::in_memory().expect("store");

        let result = ExecResult::success("output");
        let entry = HistoryEntry::from_exec("ls -la", &result, Some(10));

        store.record_history(&entry).expect("record");

        let history = store.get_history(10).expect("get");
        assert_eq!(history.len(), 1);
        assert_eq!(history[0].code, "ls -la");
        assert!(history[0].result_ok);
    }

    #[test]
    fn test_history_count() {
        let store = StateStore::in_memory().expect("store");

        assert_eq!(store.history_count().expect("count"), 0);

        let entry = HistoryEntry::from_exec("cmd1", &ExecResult::success(""), None);
        store.record_history(&entry).expect("record");

        let entry = HistoryEntry::from_exec("cmd2", &ExecResult::success(""), None);
        store.record_history(&entry).expect("record");

        assert_eq!(store.history_count().expect("count"), 2);
    }

    #[test]
    fn test_latest_history_id() {
        let store = StateStore::in_memory().expect("store");

        assert_eq!(store.latest_history_id().expect("id"), None);

        let entry = HistoryEntry::from_exec("cmd1", &ExecResult::success(""), None);
        let id1 = store.record_history(&entry).expect("record");

        let entry = HistoryEntry::from_exec("cmd2", &ExecResult::success(""), None);
        let id2 = store.record_history(&entry).expect("record");

        assert_eq!(store.latest_history_id().expect("id"), Some(id2));
        assert!(id2 > id1);
    }

    #[test]
    fn test_get_history_codes() {
        let store = StateStore::in_memory().expect("store");

        let entry = HistoryEntry::from_exec("echo first", &ExecResult::success(""), None);
        store.record_history(&entry).expect("record");

        let entry = HistoryEntry::from_exec("echo second", &ExecResult::success(""), None);
        store.record_history(&entry).expect("record");

        let codes = store.get_history_codes(100).expect("get");
        assert_eq!(codes.len(), 2);
        assert_eq!(codes[0], "echo first");
        assert_eq!(codes[1], "echo second");
    }

    #[test]
    fn test_clear_history() {
        let store = StateStore::in_memory().expect("store");

        let entry = HistoryEntry::from_exec("cmd1", &ExecResult::success(""), None);
        store.record_history(&entry).expect("record");

        assert_eq!(store.history_count().expect("count"), 1);

        store.clear_history().expect("clear");
        assert_eq!(store.history_count().expect("count"), 0);
    }

    // ================================================================
    // Checkpoint Tests
    // ================================================================

    #[test]
    fn test_create_and_get_checkpoint() {
        let store = StateStore::in_memory().expect("store");

        // Record some history first
        let entry = HistoryEntry::from_exec("cmd1", &ExecResult::success(""), None);
        let history_id = store.record_history(&entry).expect("record");

        // Create checkpoint
        let checkpoint = Checkpoint::new("Summary of session", Some(history_id))
            .with_name("session-1");

        let id = store.create_checkpoint(&checkpoint).expect("create");
        assert!(id > 0);

        let loaded = store.latest_checkpoint().expect("get").expect("exists");
        assert_eq!(loaded.name, Some("session-1".to_string()));
        assert_eq!(loaded.summary, "Summary of session");
        assert_eq!(loaded.up_to_history_id, Some(history_id));
    }

    #[test]
    fn test_list_checkpoints() {
        let store = StateStore::in_memory().expect("store");

        let c1 = Checkpoint::new("First checkpoint", None);
        store.create_checkpoint(&c1).expect("create");

        let c2 = Checkpoint::new("Second checkpoint", None);
        store.create_checkpoint(&c2).expect("create");

        let checkpoints = store.list_checkpoints().expect("list");
        assert_eq!(checkpoints.len(), 2);
        assert_eq!(checkpoints[0].summary, "First checkpoint");
        assert_eq!(checkpoints[1].summary, "Second checkpoint");
    }

    #[test]
    fn test_checkpoint_with_metadata() {
        let store = StateStore::in_memory().expect("store");

        let checkpoint = Checkpoint::new("Test", None)
            .with_metadata(serde_json::json!({
                "model": "claude-3",
                "token_count": 1500
            }));

        store.create_checkpoint(&checkpoint).expect("create");

        let loaded = store.latest_checkpoint().expect("get").expect("exists");
        assert!(loaded.metadata.is_some());
        let meta = loaded.metadata.expect("metadata");
        assert_eq!(meta["model"], "claude-3");
    }

    #[test]
    fn test_history_since_checkpoint() {
        let store = StateStore::in_memory().expect("store");

        // Record some history
        let e1 = HistoryEntry::from_exec("cmd1", &ExecResult::success(""), None);
        let id1 = store.record_history(&e1).expect("record");

        let e2 = HistoryEntry::from_exec("cmd2", &ExecResult::success(""), None);
        store.record_history(&e2).expect("record");

        // Create checkpoint covering up to id1
        let checkpoint = Checkpoint::new("Checkpoint 1", Some(id1));
        store.create_checkpoint(&checkpoint).expect("create");

        // Record more history after checkpoint
        let e3 = HistoryEntry::from_exec("cmd3", &ExecResult::success(""), None);
        store.record_history(&e3).expect("record");

        let e4 = HistoryEntry::from_exec("cmd4", &ExecResult::success(""), None);
        store.record_history(&e4).expect("record");

        // Get history since checkpoint
        let since = store.history_since_checkpoint().expect("since");
        assert_eq!(since.len(), 3); // cmd2, cmd3, cmd4 (cmd1 is covered by checkpoint)

        // Verify we got the right entries
        let codes: Vec<&str> = since.iter().map(|e| e.code.as_str()).collect();
        assert!(codes.contains(&"cmd2"));
        assert!(codes.contains(&"cmd3"));
        assert!(codes.contains(&"cmd4"));
    }

    #[test]
    fn test_history_since_checkpoint_no_checkpoint() {
        let store = StateStore::in_memory().expect("store");

        // Record history with no checkpoints
        let e1 = HistoryEntry::from_exec("cmd1", &ExecResult::success(""), None);
        store.record_history(&e1).expect("record");

        let e2 = HistoryEntry::from_exec("cmd2", &ExecResult::success(""), None);
        store.record_history(&e2).expect("record");

        // Should return all history
        let since = store.history_since_checkpoint().expect("since");
        assert_eq!(since.len(), 2);
    }
}
