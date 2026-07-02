//! Variable scope management for kaish.
//!
//! Scopes provide variable bindings with:
//! - Nested scope frames (push/pop for loops, tool calls)
//! - The special `$?` variable holding the last command's exit code
//! - Path resolution for nested access (`${VAR.field[0]}`)

use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use kaish_types::json_to_value_no_envelope;

use crate::ast::{Value, VarPath, VarSegment};

use super::eval::value_to_string;
use super::result::ExecResult;

/// Why a variable path failed to resolve.
///
/// The three-way split is load-bearing for `${path:-default}` (decision A): the
/// default fires on **absence** but never on a **shape** error — a wrong-typed
/// access is a bug, not a missing value. All three are loud for a bare access;
/// they diverge only when `:-` is present (see the default handling), where
/// `UndefinedRoot`/`Absence` yield the default and `Shape` still shouts.
///
/// - `UndefinedRoot` — the root variable (or an unset dynamic `$k` subscript) is
///   not in scope. Soft in string interpolation (expands to empty, matching
///   bash), loud in expression position.
/// - `Absence` — the path is well-shaped but the target isn't there: a missing
///   record key or an out-of-bounds list index.
/// - `Shape` — the access is wrong for the value: a string key on a list, an
///   integer index on a record, subscripting a scalar, a dotted (non-bracket)
///   segment, or slicing a record. Never suppressed by `:-`.
///
/// A loud error is surfaced everywhere, including inside strings; it is NEVER
/// silently swallowed to an empty expansion.
#[derive(Debug, Clone, PartialEq)]
pub enum PathError {
    /// The root variable is not in scope (or an unset dynamic `$k` subscript).
    UndefinedRoot(String),
    /// A missing key or out-of-bounds index — absence, not misuse.
    Absence(String),
    /// A wrong-for-the-shape access. Message ready to display.
    Shape(String),
}

/// A human-readable type name for a value, for path error messages.
fn type_name(value: &Value) -> &'static str {
    match value {
        Value::Null => "null",
        Value::Bool(_) => "a boolean",
        Value::Int(_) => "an integer",
        Value::Float(_) => "a float",
        Value::String(_) => "a string",
        Value::Json(serde_json::Value::Array(_)) => "a list",
        Value::Json(serde_json::Value::Object(_)) => "a record",
        Value::Json(_) => "a scalar",
        Value::Bytes(_) => "binary data",
    }
}

/// A dynamic subscript value usable as a list index: an integer, or a string
/// that parses as one (`k=1; ${xs[$k]}`).
fn value_as_index(value: &Value) -> Option<i64> {
    match value {
        Value::Int(i) => Some(*i),
        Value::String(s) => s.parse::<i64>().ok(),
        _ => None,
    }
}

/// A concrete, container-resolved subscript. [`resolve_step`] produces one per
/// hop *after* seeing the container — negative indices normalized, bounds
/// checked, dynamic keys looked up — so read traversal and (next phase) lvalue
/// writes share one classification and can never drift. Record-key *presence*
/// is deliberately NOT decided here: that is per-hop walk policy (a read errors
/// on a missing key; a write leaf inserts it).
#[derive(Debug, Clone, PartialEq)]
enum Step {
    /// A validated, in-bounds list index.
    Index(usize),
    /// A record key (existence unchecked — see the type doc).
    Key(String),
    /// A normalized, end-exclusive slice range (`s <= e <= len`).
    Slice(usize, usize),
}

/// Classify a list index against an array. Negative indices count from the end;
/// out of bounds is a loud error, and an integer subscript on a record is an
/// error (keys are strings — the design's "integers index lists" rule). The
/// container is a collection by [`resolve_step`]'s guard, so the scalar arm is
/// unreachable.
fn classify_index(json: &serde_json::Value, i: i64, path: &str) -> Result<Step, PathError> {
    let arr = match json {
        serde_json::Value::Array(a) => a,
        serde_json::Value::Object(_) => {
            return Err(PathError::Shape(format!(
                "${{{path}[{i}]}}: integer index on a record — record keys are strings, use ${{{path}[\"{i}\"]}}"
            )))
        }
        _ => unreachable!("resolve_step guards non-collection containers"),
    };
    let len = arr.len() as i64;
    let idx = if i < 0 { len + i } else { i };
    if idx < 0 || idx >= len {
        return Err(PathError::Absence(format!(
            "${{{path}[{i}]}}: index out of bounds (list length {len})"
        )));
    }
    Ok(Step::Index(idx as usize))
}

/// Classify a record key against an object. A bareword/string key on a list is
/// an error; key *presence* is checked when the step is applied ([`descend`]),
/// not here — the read/write split lives in that leaf policy.
fn classify_key(json: &serde_json::Value, key: &str, path: &str) -> Result<Step, PathError> {
    match json {
        serde_json::Value::Object(_) => Ok(Step::Key(key.to_string())),
        serde_json::Value::Array(_) => Err(PathError::Shape(format!(
            "${{{path}[{key}]}}: string key on a list — use an integer index"
        ))),
        _ => unreachable!("resolve_step guards non-collection containers"),
    }
}

/// Classify a slice against an array, end-exclusive. Bounds clamp; negatives
/// count from the end; an inverted or empty range yields an empty range.
/// Slicing a record is an error.
fn classify_slice(
    json: &serde_json::Value,
    start: Option<i64>,
    end: Option<i64>,
    path: &str,
) -> Result<Step, PathError> {
    let arr = match json {
        serde_json::Value::Array(a) => a,
        serde_json::Value::Object(_) => {
            return Err(PathError::Shape(format!(
                "${{{path}[..]}}: cannot slice a record"
            )))
        }
        _ => unreachable!("resolve_step guards non-collection containers"),
    };
    let len = arr.len() as i64;
    let norm = |b: i64| -> i64 {
        let b = if b < 0 { len + b } else { b };
        b.clamp(0, len)
    };
    let s = start.map(norm).unwrap_or(0);
    let e = end.map(norm).unwrap_or(len);
    let (s, e) = if s >= e {
        (s as usize, s as usize)
    } else {
        (s as usize, e as usize)
    };
    Ok(Step::Slice(s, e))
}

/// A dotted `.field` access. Brackets-only: always a loud error, with the
/// bracket fix in the message. Shared by the root pre-check and `resolve_step`
/// so a dotted segment reports identically at any hop.
fn dotted_access_error(path: &str, field: &str) -> PathError {
    PathError::Shape(format!(
        "${{{path}…}}: kaish uses bracket access, not dots — write the key as a subscript: [{field}]"
    ))
}

/// Render a subscript as the user wrote it, for building the path prefix that
/// error messages carry (`a` → `a[b]` → `a[b][0]`) so a nested failure names the
/// real path, not just the root.
fn render_segment(seg: &VarSegment) -> String {
    match seg {
        VarSegment::Index(i) => format!("[{i}]"),
        VarSegment::Key(k) => format!("[{k}]"),
        VarSegment::Dynamic(v) => format!("[${v}]"),
        VarSegment::Slice(a, b) => format!(
            "[{}:{}]",
            a.map(|n| n.to_string()).unwrap_or_default(),
            b.map(|n| n.to_string()).unwrap_or_default()
        ),
        VarSegment::Field(f) => format!(".{f}"),
    }
}

/// Classify one subscript against its container — the shared per-hop unit that
/// keeps read traversal and (next phase) lvalue writes from diverging. Only the
/// dynamic-key arm needs the scope (to look up `$k`); everything else is a pure
/// function of the container and segment. A non-collection container is caught
/// here once, so the `classify_*` helpers never see a scalar.
fn resolve_step(
    container: &serde_json::Value,
    seg: &VarSegment,
    scope: &Scope,
    path: &str,
) -> Result<Step, PathError> {
    // A non-root `Field` is a dotted `.field` access — checked before the
    // container guard so a dotted segment always wins over a "not a collection"
    // message (the per-hop precedence the old walker had).
    if let VarSegment::Field(name) = seg {
        return Err(dotted_access_error(path, name));
    }

    // Every remaining subscript needs a collection container.
    if !matches!(
        container,
        serde_json::Value::Array(_) | serde_json::Value::Object(_)
    ) {
        return Err(PathError::Shape(format!(
            "${{{path}…}}: cannot subscript {} — it is not a collection",
            type_name(&json_to_value_no_envelope(container.clone()))
        )));
    }

    match seg {
        VarSegment::Index(i) => classify_index(container, *i, path),
        VarSegment::Key(k) => classify_key(container, k, path),
        VarSegment::Slice(start, end) => classify_slice(container, *start, *end, path),
        VarSegment::Dynamic(var) => {
            // The variable's value is the subscript; the container type decides
            // whether it's an index or a key. An unset `$k` is UndefinedRoot,
            // not Absence — the *variable* is missing, so `${r[$k]:-d}` defaults.
            let key_val = scope.get(var).ok_or_else(|| {
                PathError::UndefinedRoot(format!("${{{path}[${var}]}}: ${var} is not set"))
            })?;
            match container {
                serde_json::Value::Array(_) => {
                    let idx = value_as_index(key_val).ok_or_else(|| {
                        PathError::Shape(format!(
                            "${{{path}[${var}]}}: a list index must be an integer, got \"{}\"",
                            value_to_string(key_val)
                        ))
                    })?;
                    classify_index(container, idx, path)
                }
                serde_json::Value::Object(_) => Ok(Step::Key(value_to_string(key_val))),
                _ => unreachable!("non-collection container guarded above"),
            }
        }
        VarSegment::Field(_) => unreachable!("dotted segment handled above"),
    }
}

/// Apply one classified step, descending the borrowed JSON tree. Borrowed input
/// stays borrowed for index/key (no clone); a slice always allocates a new list,
/// and once owned (post-slice) descent clones the selected child. A missing
/// record key is a loud read error here — the write-leaf insert is the next
/// phase, and lives in the walk, not in [`resolve_step`].
fn descend<'a>(
    current: Cow<'a, serde_json::Value>,
    step: Step,
    path: &str,
) -> Result<Cow<'a, serde_json::Value>, PathError> {
    match step {
        Step::Slice(s, e) => {
            let Some(arr) = current.as_array() else {
                unreachable!("slice classified against an array")
            };
            Ok(Cow::Owned(serde_json::Value::Array(arr[s..e].to_vec())))
        }
        Step::Index(i) => match current {
            Cow::Borrowed(j) => {
                let Some(arr) = j.as_array() else {
                    unreachable!("index classified against an array")
                };
                Ok(Cow::Borrowed(&arr[i]))
            }
            Cow::Owned(j) => {
                let Some(arr) = j.as_array() else {
                    unreachable!("index classified against an array")
                };
                Ok(Cow::Owned(arr[i].clone()))
            }
        },
        Step::Key(k) => match current {
            Cow::Borrowed(j) => match j.as_object().and_then(|m| m.get(&k)) {
                Some(child) => Ok(Cow::Borrowed(child)),
                None => Err(PathError::Absence(format!("${{{path}[{k}]}}: no such key"))),
            },
            Cow::Owned(j) => match j.as_object().and_then(|m| m.get(&k)) {
                Some(child) => Ok(Cow::Owned(child.clone())),
                None => Err(PathError::Absence(format!("${{{path}[{k}]}}: no such key"))),
            },
        },
    }
}

/// Variable scope with nested frames and last-result tracking.
///
/// Variables are looked up from innermost to outermost frame.
/// The `?` variable always refers to the last command result.
///
/// The `frames` field is wrapped in `Arc` for copy-on-write (COW) semantics.
/// Cloning a Scope is O(1) — just bumps the Arc refcount. Mutations use
/// `Arc::make_mut` to clone the inner data only when shared. This matters
/// because `execute_pipeline` snapshots the scope into ExecContext (clone)
/// and syncs it back (clone) on every command.
#[derive(Debug, Clone)]
pub struct Scope {
    /// Stack of variable frames. Last element is the innermost scope.
    /// Wrapped in Arc for copy-on-write: clone is O(1), mutation clones on demand.
    frames: Arc<Vec<HashMap<String, Value>>>,
    /// Variables marked for export to child processes.
    exported: HashSet<String>,
    /// The result of the last command execution.
    last_result: ExecResult,
    /// Script or tool name ($0).
    script_name: String,
    /// Positional arguments ($1-$9, $@, $#).
    positional: Vec<String>,
    /// Error exit mode (set -e): exit on any command failure.
    error_exit: bool,
    /// Counter for temporarily suppressing errexit (e.g. inside && / || left side).
    /// When > 0, error_exit_enabled() returns false even if error_exit is true.
    errexit_suppressed: usize,
    /// AST display mode (kaish-ast -on/-off): show AST instead of executing.
    show_ast: bool,
    /// Latch mode (set -o latch): gate dangerous operations behind nonce confirmation.
    latch_enabled: bool,
    /// Trash mode (set -o trash): move deleted files to freedesktop.org Trash.
    trash_enabled: bool,
    /// Maximum file size (bytes) for trash. Files larger than this bypass trash.
    /// Default: 10 MB.
    trash_max_size: u64,
    /// Glob expansion mode (set -o glob): expand bare glob patterns in arguments.
    glob_enabled: bool,
    /// Kaish session identifier ($$). A monotonic counter assigned at Kernel
    /// construction (see `KERNEL_COUNTER` in kernel.rs) — *not* the OS PID.
    /// Subshells / forks inherit the parent's value (Scope clone copies it).
    /// 0 is a sentinel meaning "this scope was constructed outside a Kernel"
    /// (e.g. arithmetic unit tests, kaish-clear before its setter runs).
    pid: u64,
}

impl Scope {
    /// Create a new scope with one empty frame.
    ///
    /// `pid` defaults to 0 (sentinel). The owning Kernel calls `set_pid()`
    /// during construction to assign the real session identifier.
    pub fn new() -> Self {
        Self {
            frames: Arc::new(vec![HashMap::new()]),
            exported: HashSet::new(),
            last_result: ExecResult::default(),
            script_name: String::new(),
            positional: Vec::new(),
            error_exit: false,
            errexit_suppressed: 0,
            show_ast: false,
            latch_enabled: false,
            trash_enabled: false,
            trash_max_size: 10 * 1024 * 1024, // 10 MB
            glob_enabled: true,
            pid: 0,
        }
    }

    /// Get the kaish session identifier ($$).
    pub fn pid(&self) -> u64 {
        self.pid
    }

    /// Set the kaish session identifier ($$). Called by the Kernel during
    /// construction to thread the assigned counter value into the scope.
    /// Also used by `kaish-clear` to preserve $$ across a session reset.
    pub fn set_pid(&mut self, pid: u64) {
        self.pid = pid;
    }

    /// Push a new scope frame (for entering a loop, tool call, etc.)
    pub fn push_frame(&mut self) {
        Arc::make_mut(&mut self.frames).push(HashMap::new());
    }

    /// Pop the innermost scope frame.
    ///
    /// Panics if attempting to pop the last frame.
    pub fn pop_frame(&mut self) {
        if self.frames.len() > 1 {
            Arc::make_mut(&mut self.frames).pop();
        } else {
            panic!("cannot pop the root scope frame");
        }
    }

    /// Set a variable in the current (innermost) frame.
    ///
    /// Use this for `local` variable declarations.
    pub fn set(&mut self, name: impl Into<String>, value: Value) {
        if let Some(frame) = Arc::make_mut(&mut self.frames).last_mut() {
            frame.insert(name.into(), value);
        }
    }

    /// Set a variable with global semantics (shell default).
    ///
    /// If the variable exists in any frame, update it there.
    /// Otherwise, create it in the outermost (root) frame.
    /// Use this for non-local variable assignments.
    pub fn set_global(&mut self, name: impl Into<String>, value: Value) {
        let name = name.into();

        // Search from innermost to outermost to find existing variable
        let frames = Arc::make_mut(&mut self.frames);
        for frame in frames.iter_mut().rev() {
            if let std::collections::hash_map::Entry::Occupied(mut e) = frame.entry(name.clone()) {
                e.insert(value);
                return;
            }
        }

        // Variable doesn't exist - create in root frame (index 0)
        if let Some(frame) = frames.first_mut() {
            frame.insert(name, value);
        }
    }

    /// Get a variable by name, searching from innermost to outermost frame.
    pub fn get(&self, name: &str) -> Option<&Value> {
        for frame in self.frames.iter().rev() {
            if let Some(value) = frame.get(name) {
                return Some(value);
            }
        }
        None
    }

    /// Remove a variable, searching from innermost to outermost frame.
    ///
    /// Returns the removed value if found, None otherwise.
    pub fn remove(&mut self, name: &str) -> Option<Value> {
        for frame in Arc::make_mut(&mut self.frames).iter_mut().rev() {
            if let Some(value) = frame.remove(name) {
                return Some(value);
            }
        }
        None
    }

    /// Set the last command result (accessible via `$?`).
    pub fn set_last_result(&mut self, result: ExecResult) {
        self.last_result = result;
    }

    /// Get the last command result.
    pub fn last_result(&self) -> &ExecResult {
        &self.last_result
    }

    /// Set the positional parameters ($0, $1-$9, $@, $#).
    ///
    /// The script_name becomes $0, and args become $1, $2, etc.
    pub fn set_positional(&mut self, script_name: impl Into<String>, args: Vec<String>) {
        self.script_name = script_name.into();
        self.positional = args;
    }

    /// Save current positional parameters for later restoration.
    ///
    /// Returns (script_name, args) tuple that can be passed to set_positional.
    pub fn save_positional(&self) -> (String, Vec<String>) {
        (self.script_name.clone(), self.positional.clone())
    }

    /// Get a positional parameter by index ($0-$9).
    ///
    /// $0 returns the script name, $1-$9 return arguments.
    pub fn get_positional(&self, n: usize) -> Option<&str> {
        if n == 0 {
            if self.script_name.is_empty() {
                None
            } else {
                Some(&self.script_name)
            }
        } else {
            self.positional.get(n - 1).map(|s| s.as_str())
        }
    }

    /// Get all positional arguments as a slice ($@).
    pub fn all_args(&self) -> &[String] {
        &self.positional
    }

    /// Get the count of positional arguments ($#).
    pub fn arg_count(&self) -> usize {
        self.positional.len()
    }

    /// Check if error-exit mode is active (set -e and not suppressed).
    ///
    /// Returns false when inside the left side of `&&` or `||` chains,
    /// matching bash behavior where those operators handle failure themselves.
    pub fn error_exit_enabled(&self) -> bool {
        self.error_exit && self.errexit_suppressed == 0
    }

    /// Set error-exit mode (set -e / set +e).
    pub fn set_error_exit(&mut self, enabled: bool) {
        self.error_exit = enabled;
    }

    /// Suppress errexit temporarily (for `&&`/`||` left side).
    pub fn suppress_errexit(&mut self) {
        self.errexit_suppressed += 1;
    }

    /// Unsuppress errexit (after `&&`/`||` left side completes).
    pub fn unsuppress_errexit(&mut self) {
        self.errexit_suppressed = self.errexit_suppressed.saturating_sub(1);
    }

    /// Check if AST display mode is enabled (kaish-ast -on).
    pub fn show_ast(&self) -> bool {
        self.show_ast
    }

    /// Set AST display mode (kaish-ast -on / kaish-ast -off).
    pub fn set_show_ast(&mut self, enabled: bool) {
        self.show_ast = enabled;
    }

    /// Check if latch mode is enabled (set -o latch).
    pub fn latch_enabled(&self) -> bool {
        self.latch_enabled
    }

    /// Set latch mode (set -o latch / set +o latch).
    pub fn set_latch_enabled(&mut self, enabled: bool) {
        self.latch_enabled = enabled;
    }

    /// Check if trash mode is enabled (set -o trash).
    pub fn trash_enabled(&self) -> bool {
        self.trash_enabled
    }

    /// Set trash mode (set -o trash / set +o trash).
    pub fn set_trash_enabled(&mut self, enabled: bool) {
        self.trash_enabled = enabled;
    }

    /// Get the maximum file size for trash (bytes).
    pub fn trash_max_size(&self) -> u64 {
        self.trash_max_size
    }

    /// Set the maximum file size for trash (bytes).
    pub fn set_trash_max_size(&mut self, size: u64) {
        self.trash_max_size = size;
    }

    /// Check if glob expansion is enabled (set -o glob, default true).
    pub fn glob_enabled(&self) -> bool {
        self.glob_enabled
    }

    /// Set glob expansion mode (set -o glob / set +o glob).
    pub fn set_glob_enabled(&mut self, enabled: bool) {
        self.glob_enabled = enabled;
    }

    /// Mark a variable as exported (visible to child processes).
    ///
    /// The variable doesn't need to exist yet; it will be exported when set.
    pub fn export(&mut self, name: impl Into<String>) {
        self.exported.insert(name.into());
    }

    /// Check if a variable is marked for export.
    pub fn is_exported(&self, name: &str) -> bool {
        self.exported.contains(name)
    }

    /// Set a variable in the **innermost** frame and mark it as exported.
    ///
    /// Used for frame-scoped overlays (`execute_with_vars`, `FOO=bar cmd`) and
    /// for seeding root-frame exports at construction. For the `export`
    /// builtin's assignment form use [`set_exported_global`](Self::set_exported_global)
    /// so the value survives a function return (shared-scope semantics).
    pub fn set_exported(&mut self, name: impl Into<String>, value: Value) {
        let name = name.into();
        self.set(&name, value);
        self.export(name);
    }

    /// Set a variable with **global** (shared-scope) semantics and mark it as
    /// exported. This is `export NAME=VALUE`: like a plain assignment, the value
    /// updates an existing variable wherever it lives or lands in the root frame,
    /// so it persists past a function return rather than dying with the
    /// function's frame.
    pub fn set_exported_global(&mut self, name: impl Into<String>, value: Value) {
        let name = name.into();
        self.set_global(&name, value);
        self.export(name);
    }

    /// Unmark a variable from export.
    pub fn unexport(&mut self, name: &str) {
        self.exported.remove(name);
    }

    /// Get all exported variables with their values.
    ///
    /// Only returns variables that exist and are marked for export.
    pub fn exported_vars(&self) -> Vec<(String, Value)> {
        let mut result = Vec::new();
        for name in &self.exported {
            if let Some(value) = self.get(name) {
                result.push((name.clone(), value.clone()));
            }
        }
        result.sort_by(|(a, _), (b, _)| a.cmp(b));
        result
    }

    /// Get all exported variable names.
    pub fn exported_names(&self) -> Vec<&str> {
        let mut names: Vec<&str> = self.exported.iter().map(|s| s.as_str()).collect();
        names.sort();
        names
    }

    /// Resolve a variable path: `${VAR}`, `${xs[0]}`, `${r[key]}`, `${a[b][c]}`.
    ///
    /// The first segment is the root name; the rest are bracket subscripts,
    /// walked left to right into the root's `Value::Json`. A subscript landing
    /// on a JSON scalar unwraps to a native `Value` (envelope-free); a subscript
    /// landing on a collection stays `Value::Json`. `$?` resolves to the
    /// previous command's exit code (bare only).
    ///
    /// Traversal borrows into the root's JSON tree and clones only the selected
    /// leaf (a slice builds a new list); the whole-root clone is never taken, so
    /// repeated `${u[$k]}` in a loop stays O(depth), not O(root size). The
    /// per-hop classification lives in [`resolve_step`], shared with the future
    /// lvalue-write walk so read and write can never diverge.
    ///
    /// Errors distinguish an undefined root (soft) from a loud path error (see
    /// [`PathError`]).
    pub fn resolve_path(&self, path: &VarPath) -> Result<Value, PathError> {
        let Some(VarSegment::Field(root_name)) = path.segments.first() else {
            // Empty path, or a first segment the parser never emits as root.
            return Err(PathError::UndefinedRoot(String::new()));
        };

        // Special case: $? (last result) — bare only.
        if root_name == "?" {
            if path.segments.len() == 1 {
                return Ok(Value::Int(self.last_result.code));
            }
            return Err(PathError::Shape(
                "$? is the POSIX exit code, not a collection — use `kaish-last` for structured data"
                    .to_string(),
            ));
        }

        let root = self
            .get(root_name)
            .ok_or_else(|| PathError::UndefinedRoot(root_name.clone()))?;

        // Bare `${VAR}`: return the stored value unchanged — no subscript, no
        // envelope unwrap.
        let subscripts = &path.segments[1..];
        if subscripts.is_empty() {
            return Ok(root.clone());
        }

        // A leading dotted segment is brackets-only regardless of the root's
        // type (matches the per-hop Field-before-container precedence in
        // `resolve_step`, which the root-collection check below would otherwise
        // preempt on a scalar root).
        if let Some(VarSegment::Field(name)) = subscripts.first() {
            return Err(dotted_access_error(root_name, name));
        }

        // Subscripted: the root must be a collection to descend into. A scalar
        // root reports the same "not a collection" message a mid-path scalar
        // would (via `resolve_step`).
        let root_json = match root {
            Value::Json(j) => j,
            other => {
                return Err(PathError::Shape(format!(
                    "${{{root_name}…}}: cannot subscript {} — it is not a collection",
                    type_name(other)
                )))
            }
        };

        // Walk the subscripts, borrowing into the tree; only a slice (which
        // builds a new list) and the terminal unwrap allocate. `prefix`
        // accumulates the path walked so far so a nested failure names the real
        // path (`${a[b][9]}`, not `${a[9]}`).
        let mut current = Cow::Borrowed(root_json);
        let mut prefix = root_name.clone();
        for seg in subscripts {
            let step = resolve_step(&current, seg, self, &prefix)?;
            current = descend(current, step, &prefix)?;
            prefix.push_str(&render_segment(seg));
        }
        Ok(json_to_value_no_envelope(current.into_owned()))
    }

    /// Check if a variable exists in any frame.
    pub fn contains(&self, name: &str) -> bool {
        self.get(name).is_some()
    }

    /// Get all variable names in scope (for debugging/introspection).
    pub fn all_names(&self) -> Vec<&str> {
        let mut names: Vec<&str> = self
            .frames
            .iter()
            .flat_map(|f| f.keys().map(|s| s.as_str()))
            .collect();
        names.sort();
        names.dedup();
        names
    }

    /// Get all variables as (name, value) pairs.
    ///
    /// Variables are deduplicated, with inner frames shadowing outer ones.
    pub fn all(&self) -> Vec<(String, Value)> {
        let mut result = std::collections::HashMap::new();
        // Iterate outer to inner so inner frames override
        for frame in self.frames.iter() {
            for (name, value) in frame {
                result.insert(name.clone(), value.clone());
            }
        }
        let mut pairs: Vec<_> = result.into_iter().collect();
        pairs.sort_by(|(a, _), (b, _)| a.cmp(b));
        pairs
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_scope_has_one_frame() {
        let scope = Scope::new();
        assert_eq!(scope.frames.len(), 1);
    }

    #[test]
    fn set_and_get_variable() {
        let mut scope = Scope::new();
        scope.set("X", Value::Int(42));
        assert_eq!(scope.get("X"), Some(&Value::Int(42)));
    }

    #[test]
    fn get_nonexistent_returns_none() {
        let scope = Scope::new();
        assert_eq!(scope.get("MISSING"), None);
    }

    #[test]
    fn inner_frame_shadows_outer() {
        let mut scope = Scope::new();
        scope.set("X", Value::Int(1));
        scope.push_frame();
        scope.set("X", Value::Int(2));
        assert_eq!(scope.get("X"), Some(&Value::Int(2)));
        scope.pop_frame();
        assert_eq!(scope.get("X"), Some(&Value::Int(1)));
    }

    #[test]
    fn inner_frame_can_see_outer_vars() {
        let mut scope = Scope::new();
        scope.set("OUTER", Value::String("visible".into()));
        scope.push_frame();
        assert_eq!(scope.get("OUTER"), Some(&Value::String("visible".into())));
    }

    #[test]
    fn resolve_simple_path() {
        let mut scope = Scope::new();
        scope.set("NAME", Value::String("Alice".into()));

        let path = VarPath::simple("NAME");
        assert_eq!(
            scope.resolve_path(&path),
            Ok(Value::String("Alice".into()))
        );
    }

    #[test]
    fn resolve_bare_last_result_returns_exit_code() {
        let mut scope = Scope::new();
        scope.set_last_result(ExecResult::failure(127, "not found"));

        let path = VarPath {
            segments: vec![VarSegment::Field("?".into())],
        };
        assert_eq!(scope.resolve_path(&path), Ok(Value::Int(127)));
    }

    #[test]
    fn resolve_last_result_field_access_is_rejected() {
        // Field access on $? was removed — use `kaish-last` for structured data.
        // The resolver now returns a loud error; the validator also catches it
        // earlier with a specific error code for actionable diagnostics.
        let mut scope = Scope::new();
        scope.set_last_result(ExecResult::success_with_data(
            "1",
            Value::Json(serde_json::json!({"count": 5})),
        ));

        let path = VarPath {
            segments: vec![
                VarSegment::Field("?".into()),
                VarSegment::Field("data".into()),
            ],
        };
        assert!(matches!(
            scope.resolve_path(&path),
            Err(PathError::Shape(_))
        ));
    }

    #[test]
    fn resolve_dotted_access_on_scalar_is_a_loud_error() {
        let mut scope = Scope::new();
        scope.set("X", Value::Int(42));

        // Dotted access `${X.invalid}` — brackets-only, so it's a loud error.
        let path = VarPath {
            segments: vec![
                VarSegment::Field("X".into()),
                VarSegment::Field("invalid".into()),
            ],
        };
        assert!(matches!(
            scope.resolve_path(&path),
            Err(PathError::Shape(_))
        ));
    }

    #[test]
    fn resolve_undefined_root_is_soft() {
        let scope = Scope::new();
        let path = VarPath::simple("NOPE");
        assert!(matches!(
            scope.resolve_path(&path),
            Err(PathError::UndefinedRoot(_))
        ));
    }

    // ── PathError classification (Absence vs Shape) ─────────────────────────
    // Pins the three-way split: `${path:-default}` (a later commit) leans on
    // Absence-vs-Shape, so a misclassification here is a real semantic bug, not
    // cosmetics. All three stay loud for a bare access.

    /// Build `${root[seg]}` with one bracket subscript.
    fn subscripted(scope: &mut Scope, root: &str, value: serde_json::Value, seg: VarSegment) -> Result<Value, PathError> {
        scope.set(root, Value::Json(value));
        let path = VarPath {
            segments: vec![VarSegment::Field(root.into()), seg],
        };
        scope.resolve_path(&path)
    }

    #[test]
    fn out_of_bounds_index_is_absence() {
        let mut scope = Scope::new();
        let r = subscripted(&mut scope, "xs", serde_json::json!([1, 2]), VarSegment::Index(9));
        assert!(matches!(r, Err(PathError::Absence(_))), "got: {r:?}");
    }

    #[test]
    fn missing_record_key_is_absence() {
        let mut scope = Scope::new();
        let r = subscripted(&mut scope, "u", serde_json::json!({"name": "amy"}), VarSegment::Key("nope".into()));
        assert!(matches!(r, Err(PathError::Absence(_))), "got: {r:?}");
    }

    #[test]
    fn string_key_on_a_list_is_shape() {
        let mut scope = Scope::new();
        let r = subscripted(&mut scope, "xs", serde_json::json!([1, 2]), VarSegment::Key("web".into()));
        assert!(matches!(r, Err(PathError::Shape(_))), "got: {r:?}");
    }

    #[test]
    fn integer_index_on_a_record_is_shape() {
        let mut scope = Scope::new();
        let r = subscripted(&mut scope, "u", serde_json::json!({"name": "amy"}), VarSegment::Index(0));
        assert!(matches!(r, Err(PathError::Shape(_))), "got: {r:?}");
    }

    #[test]
    fn subscripting_a_scalar_is_shape() {
        let mut scope = Scope::new();
        scope.set("s", Value::String("hello".into()));
        let path = VarPath {
            segments: vec![VarSegment::Field("s".into()), VarSegment::Index(0)],
        };
        assert!(matches!(scope.resolve_path(&path), Err(PathError::Shape(_))));
    }

    #[test]
    fn unset_dynamic_key_is_undefined_root_not_absence() {
        // `${r[$k]}` with `$k` unset: the *variable* is missing, so it's
        // UndefinedRoot-class (which `:-` treats as absence), not a Shape error.
        let mut scope = Scope::new();
        let r = subscripted(
            &mut scope,
            "r",
            serde_json::json!({"name": "amy"}),
            VarSegment::Dynamic("k".into()),
        );
        assert!(matches!(r, Err(PathError::UndefinedRoot(_))), "got: {r:?}");
    }

    #[test]
    fn contains_finds_variable() {
        let mut scope = Scope::new();
        scope.set("EXISTS", Value::Bool(true));
        assert!(scope.contains("EXISTS"));
        assert!(!scope.contains("MISSING"));
    }

    #[test]
    fn all_names_lists_variables() {
        let mut scope = Scope::new();
        scope.set("A", Value::Int(1));
        scope.set("B", Value::Int(2));
        scope.push_frame();
        scope.set("C", Value::Int(3));

        let names = scope.all_names();
        assert!(names.contains(&"A"));
        assert!(names.contains(&"B"));
        assert!(names.contains(&"C"));
    }

    #[test]
    #[should_panic(expected = "cannot pop the root scope frame")]
    fn pop_root_frame_panics() {
        let mut scope = Scope::new();
        scope.pop_frame();
    }

    #[test]
    fn positional_params_basic() {
        let mut scope = Scope::new();
        scope.set_positional("my_tool", vec!["arg1".into(), "arg2".into(), "arg3".into()]);

        // $0 is the script/tool name
        assert_eq!(scope.get_positional(0), Some("my_tool"));
        // $1, $2, $3 are the arguments
        assert_eq!(scope.get_positional(1), Some("arg1"));
        assert_eq!(scope.get_positional(2), Some("arg2"));
        assert_eq!(scope.get_positional(3), Some("arg3"));
        // $4 doesn't exist
        assert_eq!(scope.get_positional(4), None);
    }

    #[test]
    fn positional_params_empty() {
        let scope = Scope::new();
        // No positional params set
        assert_eq!(scope.get_positional(0), None);
        assert_eq!(scope.get_positional(1), None);
        assert_eq!(scope.arg_count(), 0);
        assert!(scope.all_args().is_empty());
    }

    #[test]
    fn all_args_returns_slice() {
        let mut scope = Scope::new();
        scope.set_positional("test", vec!["a".into(), "b".into(), "c".into()]);

        let args = scope.all_args();
        assert_eq!(args, &["a", "b", "c"]);
    }

    #[test]
    fn arg_count_returns_count() {
        let mut scope = Scope::new();
        scope.set_positional("test", vec!["one".into(), "two".into()]);

        assert_eq!(scope.arg_count(), 2);
    }

    #[test]
    fn export_marks_variable() {
        let mut scope = Scope::new();
        scope.set("X", Value::Int(42));

        assert!(!scope.is_exported("X"));
        scope.export("X");
        assert!(scope.is_exported("X"));
    }

    #[test]
    fn set_exported_sets_and_exports() {
        let mut scope = Scope::new();
        scope.set_exported("PATH", Value::String("/usr/bin".into()));

        assert!(scope.is_exported("PATH"));
        assert_eq!(scope.get("PATH"), Some(&Value::String("/usr/bin".into())));
    }

    #[test]
    fn unexport_removes_export_marker() {
        let mut scope = Scope::new();
        scope.set_exported("VAR", Value::Int(1));
        assert!(scope.is_exported("VAR"));

        scope.unexport("VAR");
        assert!(!scope.is_exported("VAR"));
        // Variable still exists, just not exported
        assert!(scope.get("VAR").is_some());
    }

    #[test]
    fn exported_vars_returns_only_exported_with_values() {
        let mut scope = Scope::new();
        scope.set_exported("A", Value::Int(1));
        scope.set_exported("B", Value::Int(2));
        scope.set("C", Value::Int(3)); // Not exported
        scope.export("D"); // Exported but no value

        let exported = scope.exported_vars();
        assert_eq!(exported.len(), 2);
        assert_eq!(exported[0], ("A".to_string(), Value::Int(1)));
        assert_eq!(exported[1], ("B".to_string(), Value::Int(2)));
    }

    #[test]
    fn exported_names_returns_sorted_names() {
        let mut scope = Scope::new();
        scope.export("Z");
        scope.export("A");
        scope.export("M");

        let names = scope.exported_names();
        assert_eq!(names, vec!["A", "M", "Z"]);
    }
}
