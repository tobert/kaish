-- kaish Kernel State Schema (SQLite)
--
-- Simplified schema: history + checkpoints only.
-- Variables, cwd, and $? are kept in-memory per session.
-- History enables REPL recall and debugging.
-- Checkpoints enable agent context distillation.

PRAGMA journal_mode = WAL;           -- Crash recovery
PRAGMA foreign_keys = ON;            -- Referential integrity
PRAGMA user_version = 2;             -- Schema version (bumped from 1)

-- ============================================================
-- Metadata
-- ============================================================

CREATE TABLE IF NOT EXISTS meta (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL
);

-- Initialize metadata
INSERT OR IGNORE INTO meta (key, value) VALUES
    ('schema_version', '2'),
    ('created_at', strftime('%Y-%m-%dT%H:%M:%SZ', 'now')),
    ('session_id', lower(hex(randomblob(16))));

-- ============================================================
-- Execution History
-- ============================================================

-- Raw history of all executed commands/code
CREATE TABLE IF NOT EXISTS history (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    code TEXT NOT NULL,                 -- The code/command that was executed
    code_hash TEXT,                     -- SHA256 for dedup/lookup
    result_code INTEGER NOT NULL,       -- Exit code
    result_ok INTEGER NOT NULL,         -- 1 = success
    result_out TEXT,                    -- stdout
    result_err TEXT,                    -- stderr
    result_data_json TEXT,              -- Structured data if any
    duration_ms INTEGER,                -- Execution time in milliseconds
    created_at TEXT DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ', 'now'))
);

CREATE INDEX IF NOT EXISTS idx_history_created ON history(created_at);
CREATE INDEX IF NOT EXISTS idx_history_hash ON history(code_hash);

-- ============================================================
-- Checkpoints (History Distillation)
-- ============================================================

-- Checkpoints compress history into summaries for context generation.
-- Each checkpoint covers history up to a certain point.
CREATE TABLE IF NOT EXISTS checkpoints (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT,                          -- Optional human-readable name
    summary TEXT NOT NULL,              -- Distilled summary of covered history
    up_to_history_id INTEGER,           -- History entries covered (inclusive)
    variables_snapshot TEXT,            -- JSON snapshot of variables at checkpoint
    metadata_json TEXT,                 -- Additional metadata (model used, token count, etc.)
    created_at TEXT DEFAULT (strftime('%Y-%m-%dT%H:%M:%SZ', 'now')),
    FOREIGN KEY (up_to_history_id) REFERENCES history(id)
);

CREATE INDEX IF NOT EXISTS idx_checkpoints_created ON checkpoints(created_at);
