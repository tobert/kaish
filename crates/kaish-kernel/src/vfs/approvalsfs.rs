//! ApprovalsFs — the approval ledger, projected as a read-only filesystem
//! (`docs/approval-ledger.md` §D.3).
//!
//! ```text
//! /v/approvals/
//! ├── pending                  ← JSON array of pending requests
//! ├── standing                 ← JSON array of live standing grants
//! ├── log                      ← NDJSON of the retained log, seq-ordered
//! └── {request-id}/
//!     ├── request              ← the request as pretty JSON
//!     ├── state                ← "requested" | "granted" | "expired" | …
//!     ├── attempts             ← JSON array of attempts and their outcomes
//!     └── grant                ← the grant as pretty JSON, empty until decided
//! ```
//!
//! **Read-only, and every write path returns `Unsupported`.** Granting by
//! writing a file would make "the agent can write files" equivalent to "the
//! agent can approve its own operations", which is the hole the ledger exists
//! to close. Approval decisions have exactly one door — the `approvals`
//! builtin, and only in a session that holds an `ApproverHandle`.
//!
//! **No projection carries a credential**, because no projected type has a
//! credential field: `ApprovalRequestView`, `Grant`, `StandingGrant`, and
//! `LedgerEntry` are all tokenless by construction (spec §A.2). There is no
//! redaction pass here, and there is nothing for one to do.

use async_trait::async_trait;
use std::io;
use std::path::Path;
use std::time::SystemTime;

use kaish_types::approval::RequestId;

use super::{DirEntry, DirEntryKind, Filesystem};
use crate::ledger::Approvals;

/// The three files at the mount root.
const ROOT_FILES: [&str; 3] = ["pending", "standing", "log"];

/// The four files inside each `{request-id}/` directory.
const REQUEST_FILES: [&str; 4] = ["request", "state", "attempts", "grant"];

/// Virtual filesystem projecting the approval ledger's read side.
///
/// Mounted at `/v/approvals` by every kernel, so an embedder or a script can
/// read what is pending without holding any approval capability — reading is
/// not deciding.
pub struct ApprovalsFs {
    approvals: Approvals,
}

impl ApprovalsFs {
    /// Project `approvals` — the ledger's read side, which grants nothing.
    pub fn new(approvals: Approvals) -> Self {
        Self { approvals }
    }

    /// Split a path into `(request directory, file)`.
    ///
    /// - `""` / `"/"` → the mount root
    /// - `"pending"` → a root file
    /// - `"{id}"` → a request directory
    /// - `"{id}/state"` → a file inside one
    fn parse_path(path: &Path) -> Option<Node<'_>> {
        let path_str = path.to_str()?.trim_start_matches('/');
        if path_str.is_empty() {
            return Some(Node::Root);
        }
        let parts: Vec<&str> = path_str.split('/').collect();
        match parts.as_slice() {
            [name] if ROOT_FILES.contains(name) => Some(Node::RootFile(name)),
            [id] => Some(Node::RequestDir(id)),
            [id, file] => Some(Node::RequestFile(id, file)),
            _ => None,
        }
    }

    /// The chain for `id`, or a `NotFound` naming the id that missed. A
    /// retained-out or never-issued id reads the same way: the ledger has no
    /// record, and saying so beats an empty body that reads like "no grant".
    /// A malformed id is `InvalidInput` and names the format, because a typo
    /// and a since-evicted record are different problems.
    fn chain(&self, id: &str) -> io::Result<crate::ledger::RequestChain> {
        let parsed = RequestId::parse(id).map_err(|e| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("{e} — ids are full-form, e.g. req_9c1a4f2e_42"),
            )
        })?;
        self.approvals.get(&parsed).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                format!("no approval request {id} in this ledger"),
            )
        })
    }

    /// Every request id the ledger still holds, as directory names.
    fn request_ids(&self) -> Vec<String> {
        self.approvals
            .ids()
            .into_iter()
            .map(|id| id.as_str().to_string())
            .collect()
    }

    fn root_file(&self, name: &str) -> io::Result<Vec<u8>> {
        match name {
            "pending" => pretty(&self.approvals.pending()),
            "standing" => pretty(&self.approvals.standing()),
            "log" => {
                // NDJSON, sequence-ordered: one versioned record per line, so
                // a consumer can tail it and parse incrementally rather than
                // re-reading a growing array. Each line is a `LedgerRecord`
                // (spec §A.5) — `schema_version`, `sequence`, `at`, `scope`,
                // and the entry itself under `entry` — so a reader knows what
                // it is holding and whose it is.
                let mut out = String::new();
                for record in self.approvals.log(0) {
                    out.push_str(&serialize(&record)?);
                    out.push('\n');
                }
                Ok(out.into_bytes())
            }
            _ => Err(io::Error::new(io::ErrorKind::NotFound, format!("unknown file: {name}"))),
        }
    }

    fn request_file(&self, id: &str, name: &str) -> io::Result<Vec<u8>> {
        let chain = self.chain(id)?;
        match name {
            "request" => pretty(&chain.request),
            "state" => {
                let state = serialize(&chain.state)?;
                // The enum serializes as a JSON string; the node is the bare
                // word, so `cat` and `[ "$(cat state)" = granted ]` both read
                // naturally.
                Ok(format!("{}\n", state.trim_matches('"')).into_bytes())
            }
            "attempts" => {
                let attempts: Vec<serde_json::Value> = chain
                    .attempts
                    .iter()
                    .map(|a| {
                        serde_json::json!({
                            "attempt": a.attempt,
                            "state": a.state,
                            "outcome": a.outcome,
                        })
                    })
                    .collect();
                pretty(&attempts)
            }
            // Empty body until the request is decided — a consumer reads,
            // then parses only if non-empty, the same contract
            // `/v/jobs/{id}/approval` already uses.
            "grant" => match &chain.grant {
                Some(grant) => pretty(grant),
                None => Ok(Vec::new()),
            },
            _ => Err(io::Error::new(io::ErrorKind::NotFound, format!("unknown file: {name}"))),
        }
    }
}

/// What a path under the mount names.
enum Node<'a> {
    Root,
    RootFile(&'a str),
    RequestDir(&'a str),
    RequestFile(&'a str, &'a str),
}

fn serialize<T: serde::Serialize>(value: &T) -> io::Result<String> {
    serde_json::to_string(value).map_err(|e| io::Error::other(format!("approvals serialize: {e}")))
}

fn pretty<T: serde::Serialize>(value: &T) -> io::Result<Vec<u8>> {
    let json = serde_json::to_string_pretty(value)
        .map_err(|e| io::Error::other(format!("approvals serialize: {e}")))?;
    Ok(format!("{json}\n").into_bytes())
}

/// The one refusal every mutating path returns. `Unsupported`, not
/// `PermissionDenied` — no permission would make it work.
fn read_only(operation: &str) -> io::Error {
    io::Error::new(
        io::ErrorKind::Unsupported,
        format!("/v/approvals is read-only: {operation} is not supported — decide with the `approvals` builtin"),
    )
}

fn file(name: &str) -> DirEntry {
    DirEntry {
        name: name.to_string(),
        kind: DirEntryKind::File,
        modified: None,
        permissions: None,
        size: 0, // Synthesized on read.
        symlink_target: None,
    }
}

fn directory(name: &str) -> DirEntry {
    DirEntry {
        name: name.to_string(),
        kind: DirEntryKind::Directory,
        modified: None,
        permissions: None,
        size: 0,
        symlink_target: None,
    }
}

impl std::fmt::Debug for ApprovalsFs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ApprovalsFs").finish()
    }
}

#[async_trait]
impl Filesystem for ApprovalsFs {
    async fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        match ApprovalsFs::parse_path(path) {
            Some(Node::RootFile(name)) => self.root_file(name),
            Some(Node::RequestFile(id, name)) => self.request_file(id, name),
            Some(Node::Root) | Some(Node::RequestDir(_)) => Err(io::Error::new(
                io::ErrorKind::IsADirectory,
                "cannot read directory",
            )),
            None => Err(io::Error::new(io::ErrorKind::InvalidInput, "invalid approvals path")),
        }
    }

    async fn write(&self, _path: &Path, _data: &[u8]) -> io::Result<()> {
        Err(read_only("write"))
    }

    async fn list(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        match ApprovalsFs::parse_path(path) {
            Some(Node::Root) => {
                let mut entries: Vec<DirEntry> = ROOT_FILES.iter().map(|name| file(name)).collect();
                entries.extend(self.request_ids().iter().map(|id| directory(id)));
                Ok(entries)
            }
            Some(Node::RequestDir(id)) => {
                self.chain(id)?;
                Ok(REQUEST_FILES.iter().map(|name| file(name)).collect())
            }
            Some(Node::RootFile(_)) | Some(Node::RequestFile(..)) => Err(io::Error::new(
                io::ErrorKind::NotADirectory,
                "not a directory",
            )),
            None => Err(io::Error::new(io::ErrorKind::InvalidInput, "invalid approvals path")),
        }
    }

    async fn stat(&self, path: &Path) -> io::Result<DirEntry> {
        let name = path
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| "/".to_string());
        match ApprovalsFs::parse_path(path) {
            Some(Node::Root) => Ok(DirEntry::directory(name)),
            Some(Node::RootFile(_)) => Ok(DirEntry::file(name, 0)),
            Some(Node::RequestDir(id)) => {
                self.chain(id)?;
                Ok(DirEntry::directory(name))
            }
            Some(Node::RequestFile(id, file_name)) => {
                self.chain(id)?;
                if !REQUEST_FILES.contains(&file_name) {
                    return Err(io::Error::new(
                        io::ErrorKind::NotFound,
                        format!("unknown file: {file_name}"),
                    ));
                }
                Ok(DirEntry::file(name, 0))
            }
            None => Err(io::Error::new(io::ErrorKind::InvalidInput, "invalid approvals path")),
        }
    }

    async fn mkdir(&self, _path: &Path) -> io::Result<()> {
        Err(read_only("mkdir"))
    }

    async fn remove(&self, _path: &Path) -> io::Result<()> {
        Err(read_only("remove"))
    }

    async fn set_mtime(&self, _path: &Path, _mtime: SystemTime) -> io::Result<()> {
        Err(read_only("set_mtime"))
    }

    async fn rename(&self, _from: &Path, _to: &Path) -> io::Result<()> {
        Err(read_only("rename"))
    }

    async fn symlink(&self, _target: &Path, _link: &Path) -> io::Result<()> {
        Err(read_only("symlink"))
    }

    fn read_only(&self) -> bool {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// An empty ledger still lists its three root files — a consumer polling
    /// `pending` must not have to special-case "nothing has happened yet".
    #[tokio::test]
    async fn root_lists_the_three_files_with_no_requests() {
        let fs = ApprovalsFs::new(Approvals::empty());
        let entries = fs.list(Path::new("")).await.expect("root lists");
        let names: Vec<&str> = entries.iter().map(|e| e.name.as_str()).collect();
        assert_eq!(names, vec!["pending", "standing", "log"]);
    }

    #[tokio::test]
    async fn pending_on_an_empty_ledger_is_an_empty_json_array() {
        let fs = ApprovalsFs::new(Approvals::empty());
        let body = fs.read(Path::new("pending")).await.expect("pending reads");
        assert_eq!(String::from_utf8_lossy(&body).trim(), "[]");
    }

    /// `log` is NDJSON, so an empty log is zero bytes rather than `[]`.
    #[tokio::test]
    async fn log_on_an_empty_ledger_is_empty() {
        let fs = ApprovalsFs::new(Approvals::empty());
        let body = fs.read(Path::new("log")).await.expect("log reads");
        assert!(body.is_empty(), "expected no lines, got {body:?}");
    }

    #[tokio::test]
    async fn an_unknown_request_id_is_not_found() {
        let fs = ApprovalsFs::new(Approvals::empty());
        let err = fs
            .read(Path::new("req_00000000_1/state"))
            .await
            .expect_err("an unknown id must not read");
        assert_eq!(err.kind(), io::ErrorKind::NotFound);
    }

    #[tokio::test]
    async fn reading_a_directory_says_so() {
        let fs = ApprovalsFs::new(Approvals::empty());
        let err = fs.read(Path::new("")).await.expect_err("the root is a directory");
        assert_eq!(err.kind(), io::ErrorKind::IsADirectory);
    }
}
