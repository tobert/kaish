//! ExecResult — the structured result of every command execution.
//!
//! After every command in kaish, the special variable `$?` contains an ExecResult.

use std::borrow::Cow;
use std::collections::BTreeMap;

use crate::output::OutputData;
use crate::stream_order::{OutputChunk, OutputSequence, OutputSpan, StreamKind, StreamOrder};
use crate::value::Value;

/// A command's stdout payload: text, or raw bytes.
///
/// `Text` xor `Bytes` — the enum makes the invalid both-set state
/// unrepresentable (an earlier draft used two sibling fields; see
/// `docs/binary-data.md`). Serializes wire-compatibly: `Text` is a bare JSON
/// string (unchanged from when `out` was a `String`), `Bytes` is the base64
/// envelope from [`crate::bytes`].
#[derive(Debug, Clone, PartialEq)]
pub enum OutputPayload {
    /// UTF-8 text — the common case, canonical for pipes.
    Text(String),
    /// Raw bytes — binary output (set by binary-aware builtins). Until the
    /// Phase-2 pipe/consumption rework, no builtin produces this in practice.
    Bytes(Vec<u8>),
}

impl Default for OutputPayload {
    fn default() -> Self {
        OutputPayload::Text(String::new())
    }
}

impl serde::Serialize for OutputPayload {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            // Bare string keeps the historical `"out":"…"` wire shape.
            OutputPayload::Text(t) => serializer.serialize_str(t),
            OutputPayload::Bytes(b) => crate::bytes::bytes_to_envelope(b).serialize(serializer),
        }
    }
}

impl<'de> serde::Deserialize<'de> for OutputPayload {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let v = serde_json::Value::deserialize(deserializer)?;
        match v {
            serde_json::Value::String(s) => Ok(OutputPayload::Text(s)),
            other => match crate::bytes::envelope_to_bytes(&other) {
                Some(b) => Ok(OutputPayload::Bytes(b)),
                None => Err(serde::de::Error::custom(
                    "ExecResult.out: expected a string or a base64 bytes envelope",
                )),
            },
        }
    }
}

/// Returned when a binary result is asked to behave as text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryNotText {
    /// Number of binary bytes that could not be coerced.
    pub len: usize,
}

impl std::fmt::Display for BinaryNotText {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "output is binary ({} bytes), not text — pipe through base64/xxd or redirect to a file",
            self.len
        )
    }
}

impl std::error::Error for BinaryNotText {}

/// The result of executing a command or pipeline.
///
/// `$?` in script syntax is the POSIX exit code (an integer). To read the
/// previous command's structured `.data` (or its captured stdout) from
/// inside a script, use the `kaish-last` builtin and pipe / capture its
/// output. Inside Rust callers, read `.data`, `.text_out()`, etc. directly.
///
/// Notes on the fields:
/// - `code` — exit code (0 = success)
/// - `err` — error message if failed
/// - `out` — raw stdout as string
/// - `data` — structured data; only set by builtins/tools that opt in
///   (e.g. `seq`, `jq`, `cut`, `find`, `glob`, `split`). External commands
///   never populate this — pipe their stdout through `jq` to get it.
#[derive(Debug, Clone, Default, PartialEq, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct ExecResult {
    /// Exit code. 0 means success.
    pub code: i64,
    /// Whether [`Self::data`] is this result's VALUE rather than a structured
    /// view of the text it printed. Stamped by the dispatcher from the tool's
    /// [`crate::tool::ToolSchema::typed_substitution`]; a command substitution binds `data`
    /// only when this is set, while `--json` and the pipeline's structured
    /// sideband read `data` regardless.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub data_is_value: bool,
    /// Standard output payload — text (canonical for pipes) or raw bytes.
    out: OutputPayload,
    /// Raw standard error as a string.
    ///
    /// Line contract: empty, or ends with exactly one `\n` — every diagnostic
    /// kaish mints ends its own line (#363), so renderers print `err`
    /// verbatim. Two deliberate exceptions carry unterminated text: a
    /// `read -p` prompt and stdout folded into stderr by `1>&2` — both stay
    /// byte-faithful, as bash does. External-command stderr is pass-through
    /// data too; the contract covers kaish's own messages.
    pub err: String,
    /// Structured data — only populated when a builtin/tool sets it explicitly.
    /// Stdout is *never* sniffed; this stays `None` for external commands.
    pub data: Option<Value>,
    /// Structured output data for rendering.
    ///
    /// Boxed because `OutputData` is ~120 B and `ExecResult` (and
    /// the `ControlFlow` that wraps it) is returned up every level of deep
    /// `$()`/pipeline recursion, so an inline `Option<OutputData>` fattened every
    /// frame. The box is allocated only when a builtin sets structured output,
    /// and it serializes identically (Box is transparent to serde). Private —
    /// the public accessors below hand back plain `OutputData`/`&OutputData`, so
    /// the boxing never leaks (GH #48, item 5).
    output: Option<Box<OutputData>>,
    // Avoid nesting a JSON failure envelope when a runner forwards it.
    #[serde(skip)]
    pub(crate) json_failure_formatted: bool,
    /// True if output was capped and lost data. Either the output limiter
    /// spilled the overflow to disk (the `out` message carries the path),
    /// truncated it in memory (Memory spill mode — head+tail only, no
    /// recoverable file), or an external command's stdout overflowed its
    /// fixed-size capture ring with output limiting off (GH #191) — the
    /// capture buffer evicted its head with no spill file at all. All cases
    /// remap the exit code to 3.
    pub did_spill: bool,
    /// The command could not decide, as opposed to deciding `false`.
    ///
    /// A non-numeric operand in `[[ ]]`, `test`, or `(( ))` is a fault: there
    /// is no true or false to report, only a malformed comparison. Where
    /// nothing consumes the result as a boolean this rides along with exit 2
    /// and is ignored. Where something DOES — an `if`/`while` condition, a
    /// `!`, or the left operand of `&&`/`||` — the kernel aborts rather than
    /// coerce a fault into a boolean, which would let a wrong conclusion be
    /// drawn from a comparison that never happened.
    ///
    /// A command that ran and failed is NOT a fault. `grep` matching nothing
    /// and a missing file still select `else` and still drive `||`.
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub fault: bool,
    /// The command's original exit code before spill logic overwrote it with 2 or 3.
    /// Present only when `did_spill` is true and `code` was changed.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub original_code: Option<i64>,
    /// MIME content type hint (e.g., "text/markdown", "image/svg+xml").
    /// When set, downstream consumers can use this instead of sniffing content.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub content_type: Option<String>,
    /// Opaque key-value context propagated from tools through execution.
    /// Intermediaries (kaish) carry but don't interpret. Consumers read known keys.
    /// Follows W3C Baggage semantics — useful for OTel trace propagation,
    /// application-level hints, etc.
    #[serde(default, skip_serializing_if = "BTreeMap::is_empty")]
    pub baggage: BTreeMap<String, String>,
    /// How many leading bytes of `err` a background job's `/v/jobs/N/stderr`
    /// stream already holds. Publishing writes `err[stderr_published_len..]`
    /// and advances this to `err.len()`, so text appended after a publish is
    /// published once and text published before is never sent again.
    /// It counts bytes, not order: the stream holds them in the order they
    /// were produced. An external's overflow markers lead `err` but follow
    /// its live bytes on the stream.
    /// Internal plumbing, not part of the wire contract: never serialized.
    #[serde(skip)]
    pub stderr_published_len: usize,
    /// How stdout and stderr interleave; see [`Self::stream_order`].
    ///
    /// Boxed so a result without spans pays 8 bytes. Held as a
    /// [`StreamOrder`] so a loop that takes it, appends, and puts it back
    /// keeps its merge progress and grows in amortized time.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    stream_order: Option<Box<StreamOrder>>,
}

/// How a tool's run ends: normally, or by ending the script.
///
/// `Normal` is the case for nearly every tool. `Exit` is for a tool that runs
/// a user function or script on the caller's behalf (`timeout` running a
/// function that calls `exit`): the script stops, and the result's `code` is
/// its exit status. A tool built on `Tool::execute` alone never produces it.
#[derive(Debug, Clone, PartialEq)]
#[non_exhaustive]
pub enum ToolFlow {
    /// The tool finished; the script goes on.
    Normal(ExecResult),
    /// The tool finished and the script ends with `result.code`.
    Exit(ExecResult),
}

impl ToolFlow {
    /// The result, whichever way the tool ended. An `Exit` keeps its code.
    pub fn into_result(self) -> ExecResult {
        match self {
            ToolFlow::Normal(result) | ToolFlow::Exit(result) => result,
        }
    }
}

impl ExecResult {
    /// End a kaish diagnostic on its own line: empty stays empty; anything
    /// else ends with exactly one `\n`.
    ///
    /// stderr is line-oriented. A diagnostic kaish mints must terminate its
    /// own line, or whatever renders next fuses onto it (#363). The
    /// constructors call this; apply it to any message assigned to `err`
    /// directly.
    pub fn terminate_diagnostic(message: impl Into<String>) -> String {
        let mut message = message.into();
        if message.is_empty() {
            return message;
        }
        let terminated = message.trim_end_matches('\n').len();
        message.truncate(terminated);
        message.push('\n');
        message
    }

    /// Create a successful result with output.
    pub fn success(out: impl Into<String>) -> Self {
        Self {
            code: 0,
            data_is_value: false,
            out: OutputPayload::Text(out.into()),
            err: String::new(),
            data: None,
            output: None,
            json_failure_formatted: false,
            did_spill: false,
            fault: false,
            original_code: None,
            content_type: None,
            baggage: BTreeMap::new(),
            stderr_published_len: 0,
            stream_order: None,
        }
    }

    /// Create a successful result with structured output data.
    ///
    /// The `OutputData` is the source of truth. Text is materialized lazily
    /// via `text_out()` when needed (pipes, redirects, command substitution).
    pub fn with_output(output: OutputData) -> Self {
        // Simple text: move string into .out directly for efficient Cow::Borrowed.
        // Structured output: store in .output, materialize lazily.
        match output.into_text() {
            Ok(text) => Self::success(text),
            Err(output) => Self {
                code: 0,
                data_is_value: false,
                out: OutputPayload::Text(String::new()),
                err: String::new(),
                data: None,
                output: Some(Box::new(output)),
                json_failure_formatted: false,
                did_spill: false,
                fault: false,
                original_code: None,
                content_type: None,
                baggage: BTreeMap::new(),
                stderr_published_len: 0,
                stream_order: None,
            },
        }
    }

    /// Create a successful result whose stdout is raw bytes (binary payload).
    pub fn success_bytes(bytes: Vec<u8>) -> Self {
        let mut r = Self::success("");
        r.out = OutputPayload::Bytes(bytes);
        r
    }

    /// Create a successful result from bytes, applying the coercion rule at the
    /// producer: valid UTF-8 becomes a text result, anything else a binary
    /// `Bytes` result. This is the single place pass-through/decoder builtins
    /// (`cat`, `head -c`, `base64 -d`, `xxd -r`, `tee`, …) decide text-vs-binary,
    /// so text workflows stay text and only real binary flows as bytes.
    pub fn success_text_or_bytes(bytes: Vec<u8>) -> Self {
        match String::from_utf8(bytes) {
            Ok(text) => Self::success(text),
            Err(e) => Self::success_bytes(e.into_bytes()),
        }
    }

    /// Create a successful result with structured data.
    pub fn success_data(data: Value) -> Self {
        let out = value_to_json(&data).to_string();
        Self {
            code: 0,
            data_is_value: false,
            out: OutputPayload::Text(out),
            err: String::new(),
            data: Some(data),
            output: None,
            json_failure_formatted: false,
            did_spill: false,
            fault: false,
            original_code: None,
            content_type: None,
            baggage: BTreeMap::new(),
            stderr_published_len: 0,
            stream_order: None,
        }
    }

    /// Create a successful result with both text output and structured data.
    ///
    /// Use this when a command should have:
    /// - Text output for pipes and traditional shell usage
    /// - Structured data for iteration and programmatic access
    ///
    /// The data field takes precedence for command substitution in contexts
    /// like `for i in $(cmd)` where the structured data can be iterated.
    pub fn success_with_data(out: impl Into<String>, data: Value) -> Self {
        Self {
            code: 0,
            data_is_value: false,
            out: OutputPayload::Text(out.into()),
            err: String::new(),
            data: Some(data),
            output: None,
            json_failure_formatted: false,
            did_spill: false,
            fault: false,
            original_code: None,
            content_type: None,
            baggage: BTreeMap::new(),
            stderr_published_len: 0,
            stream_order: None,
        }
    }

    /// Create a failed result with an error message.
    ///
    /// The message is normalized to the stderr line contract: it ends with
    /// exactly one newline (unless empty), so renderers print it verbatim.
    /// Mark this result as a fault: it could not decide, rather than
    /// deciding `false`. See [`Self::fault`].
    #[must_use]
    pub fn into_fault(mut self) -> Self {
        self.fault = true;
        self
    }

    pub fn failure(code: i64, err: impl Into<String>) -> Self {
        Self {
            code,
            data_is_value: false,
            out: OutputPayload::Text(String::new()),
            err: Self::terminate_diagnostic(err),
            data: None,
            output: None,
            json_failure_formatted: false,
            did_spill: false,
            fault: false,
            original_code: None,
            content_type: None,
            baggage: BTreeMap::new(),
            stderr_published_len: 0,
            stream_order: None,
        }
    }

    /// Create a result from raw output streams.
    ///
    /// `data` is left empty — kaish does not sniff stdout for JSON. To get
    /// structured iteration from an external command, pipe through `jq`:
    /// `for i in $(curl ... | jq .); do ...`.
    pub fn from_output(code: i64, stdout: impl Into<String>, stderr: impl Into<String>) -> Self {
        Self {
            data_is_value: false,
            code,
            out: OutputPayload::Text(stdout.into()),
            err: stderr.into(),
            data: None,
            output: None,
            json_failure_formatted: false,
            did_spill: false,
            fault: false,
            original_code: None,
            content_type: None,
            baggage: BTreeMap::new(),
            stderr_published_len: 0,
            stream_order: None,
        }
    }

    /// Create a successful result with structured output and explicit pipe text.
    ///
    /// Use this when a builtin needs custom text formatting that differs from
    /// the canonical `OutputData::to_canonical_string()` representation.
    pub fn with_output_and_text(output: OutputData, text: impl Into<String>) -> Self {
        Self {
            code: 0,
            data_is_value: false,
            out: OutputPayload::Text(text.into()),
            err: String::new(),
            data: None,
            output: Some(Box::new(output)),
            json_failure_formatted: false,
            did_spill: false,
            fault: false,
            original_code: None,
            content_type: None,
            baggage: BTreeMap::new(),
            stderr_published_len: 0,
            stream_order: None,
        }
    }

    /// Create a result from parts — for kernel struct literal sites.
    pub fn from_parts(
        code: i64,
        out: String,
        err: String,
        data: Option<Value>,
    ) -> Self {
        Self {
            data_is_value: false,
            code,
            out: OutputPayload::Text(out),
            err: Self::terminate_diagnostic(err),
            data,
            output: None,
            json_failure_formatted: false,
            did_spill: false,
            fault: false,
            original_code: None,
            content_type: None,
            baggage: BTreeMap::new(),
            stderr_published_len: 0,
            stream_order: None,
        }
    }

    /// Builder: set the exit code, returning self for chaining.
    pub fn with_code(mut self, code: i64) -> Self {
        self.code = code;
        self
    }

    // ── Read accessors ──

    /// Get text output, materializing from OutputData on demand.
    ///
    /// Returns the text payload if non-empty, otherwise falls back to
    /// `OutputData::to_canonical_string()`. This is the canonical way to
    /// get text for pipes, command substitution, and file redirects.
    ///
    /// **Binary payloads** decode lossily here (`U+FFFD` for invalid UTF-8).
    /// Several builtins already produce a `Bytes` payload (`cat`/`head`/`tail`/
    /// `base64 -d`/`xxd -r`/`dd`/`tee`/external commands), so this lossy path
    /// IS reachable — callers that need to catch binary rather than silently
    /// mangle it should use [`Self::try_text_out`] instead, which loud-errors
    /// with [`BinaryNotText`] on invalid UTF-8. See `docs/binary-data.md`.
    pub fn text_out(&self) -> Cow<'_, str> {
        match &self.out {
            OutputPayload::Text(s) if !s.is_empty() => Cow::Borrowed(s),
            OutputPayload::Bytes(b) => match std::str::from_utf8(b) {
                Ok(s) => Cow::Borrowed(s),
                Err(_) => Cow::Owned(String::from_utf8_lossy(b).into_owned()),
            },
            // Empty text → fall back to structured output's canonical string.
            _ => match self.output {
                Some(ref output) => Cow::Owned(output.to_canonical_string()),
                None => Cow::Borrowed(""),
            },
        }
    }

    /// Get text output, or a [`BinaryNotText`] error if the payload is binary
    /// and not valid UTF-8. This is the boundary guard for text sinks (`echo`,
    /// interpolation, `$()` capture) — adopted as those paths grow byte
    /// awareness (Phase 2). Valid-UTF-8 bytes coerce; everything else is loud.
    pub fn try_text_out(&self) -> Result<Cow<'_, str>, BinaryNotText> {
        match &self.out {
            OutputPayload::Bytes(b) => std::str::from_utf8(b)
                .map(Cow::Borrowed)
                .map_err(|_| BinaryNotText { len: b.len() }),
            _ => Ok(self.text_out()),
        }
    }

    /// Raw bytes if this result carries a binary payload, else `None`.
    pub fn out_bytes(&self) -> Option<&[u8]> {
        match &self.out {
            OutputPayload::Bytes(b) => Some(b),
            OutputPayload::Text(_) => None,
        }
    }

    /// True if the stdout payload is raw bytes rather than text.
    pub fn is_bytes(&self) -> bool {
        matches!(self.out, OutputPayload::Bytes(_))
    }

    /// Get a reference to structured output data.
    pub fn output(&self) -> Option<&OutputData> {
        self.output.as_deref()
    }

    /// True if structured output data is present.
    pub fn has_output(&self) -> bool {
        self.output.is_some()
    }

    // ── Mutation accessors ──

    /// Replace `.out` with text.
    ///
    /// Drops the stdout spans of [`Self::stream_order`].
    pub fn set_out(&mut self, s: String) {
        self.out = OutputPayload::Text(s);
        self.remove_stream_spans(StreamKind::Stdout);
    }

    /// Replace `.out` with raw bytes (binary payload).
    ///
    /// Drops the stdout spans of [`Self::stream_order`].
    pub fn set_out_bytes(&mut self, b: Vec<u8>) {
        self.out = OutputPayload::Bytes(b);
        self.remove_stream_spans(StreamKind::Stdout);
    }

    /// Append text to `.out`. A binary payload is appended to as raw UTF-8 bytes.
    pub fn push_out(&mut self, s: &str) {
        match &mut self.out {
            OutputPayload::Text(t) => t.push_str(s),
            OutputPayload::Bytes(b) => b.extend_from_slice(s.as_bytes()),
        }
    }

    /// Clear `.out` back to empty text, with its stdout spans.
    pub fn clear_out(&mut self) {
        self.out = OutputPayload::Text(String::new());
        self.remove_stream_spans(StreamKind::Stdout);
    }

    /// Drop every representation of stdout: the text `.out`, the structured
    /// `.output`, and the data-plane `.data` sideband. Used when a stdout
    /// redirect (`> file`, `>> file`, `&> file`, `1>&2`) has consumed the
    /// command's output — the bytes went to the file (or stderr), so nothing
    /// flows onward to a pipe, a `$(...)` capture, or the `.data` sideband.
    /// Clearing all three together keeps them from drifting: a redirect that
    /// cleared `.out`/`.output` but left `.data` would leak structured data past
    /// its own redirect (`x=$(fromjson … > file)` capturing the value instead
    /// of `""`).
    ///
    /// stdout, so a redirect can't drop it — `rm precious > log` still
    /// gates.
    pub fn clear_stdout(&mut self) {
        self.out = OutputPayload::Text(String::new());
        self.output = None;
        self.data = None;
        self.remove_stream_spans(StreamKind::Stdout);
    }

    /// Replace `.output`.
    ///
    /// When `.out` is empty text, `.output` is the stdout, so its stdout
    /// spans are dropped.
    pub fn set_output(&mut self, o: Option<OutputData>) {
        self.output = o.map(Box::new);
        if self.out_is_empty_text() {
            self.remove_stream_spans(StreamKind::Stdout);
        }
    }

    /// Take `.output`, leaving None.
    ///
    /// When `.out` is empty text, `.output` was the stdout, so its stdout
    /// spans are dropped.
    pub fn take_output(&mut self) -> Option<OutputData> {
        if self.out_is_empty_text() {
            self.remove_stream_spans(StreamKind::Stdout);
        }
        self.output.take().map(|o| *o)
    }

    /// Materialize: if `.out` is empty and `.output` is present,
    /// populate `.out` from canonical string and clear `.output`.
    pub fn materialize(&mut self) {
        if matches!(&self.out, OutputPayload::Text(s) if s.is_empty()) {
            if let Some(ref output) = self.output {
                self.out = OutputPayload::Text(output.to_canonical_string());
            }
        }
        self.output = None;
    }

    /// Take `.output` only if `.out` is empty (no custom text),
    /// so caller can stream directly without materializing.
    pub fn take_output_for_stream(&mut self) -> Option<OutputData> {
        if self.out_is_empty_text() {
            self.remove_stream_spans(StreamKind::Stdout);
            self.output.take().map(|o| *o)
        } else {
            None
        }
    }

    /// True if the command succeeded (exit code 0).
    pub fn ok(&self) -> bool {
        self.code == 0
    }

    /// Set content type hint, returning self for chaining.
    pub fn with_content_type(mut self, ct: impl Into<String>) -> Self {
        self.content_type = Some(ct.into());
        self
    }

    // ── Stream order ──

    /// How this result's stdout and stderr bytes interleave, in the order
    /// kaish saw them.
    ///
    /// Each span consumes the next `len` bytes of its stream: stdout is
    /// [`Self::text_out`] (or [`Self::out_bytes`] for a binary payload), and
    /// stderr is `err`. Sequence numbers strictly increase down the list.
    ///
    /// `None` means the order is the stdout block followed by the stderr
    /// block. That is the case for a result with no spans, and for one whose
    /// spans no longer match its payloads because `err` or stdout was
    /// changed after they were recorded. A spilled or truncated result has
    /// no spans.
    ///
    /// Exactness depends on the producer: a builtin reads as its whole
    /// stdout, then its whole stderr; an external command's chunks are
    /// ordered by when kaish read them from its two pipes; and statements in
    /// a function, loop, or group are ordered statement by statement.
    /// Numbers come from one counter per kernel (shared with its forks), so
    /// they order spans across results from the same kernel only.
    pub fn stream_order(&self) -> Option<&[OutputSpan]> {
        let spans = self.stream_order.as_deref()?.spans();
        crate::stream_order::spans_describe(spans, self.stdout_len(), self.err.len() as u64)
            .then_some(spans)
    }

    /// Stdout and stderr as runs of bytes in the order of
    /// [`Self::stream_order`], or the stdout block then the stderr block when
    /// that is `None`. Empty streams contribute no chunk.
    ///
    /// Concatenating the chunks gives what a terminal shows when both streams
    /// write to it. A chunk boundary can split a UTF-8 character.
    pub fn chunks(&self) -> Vec<OutputChunk<'_>> {
        let stdout = self.stdout_bytes();
        let stderr = self.err.as_bytes();
        let spans = self.stream_order.as_deref().map(StreamOrder::spans).filter(|spans| {
            crate::stream_order::spans_describe(spans, stdout.len() as u64, stderr.len() as u64)
        });
        let slice_stdout = |start: usize, end: usize| -> Cow<'_, [u8]> {
            match &stdout {
                Cow::Borrowed(bytes) => Cow::Borrowed(&bytes[start..end]),
                Cow::Owned(bytes) => Cow::Owned(bytes[start..end].to_vec()),
            }
        };
        let Some(spans) = spans else {
            let mut chunks = Vec::with_capacity(2);
            if !stdout.is_empty() {
                chunks.push(OutputChunk { stream: StreamKind::Stdout, seq: None, bytes: slice_stdout(0, stdout.len()) });
            }
            if !stderr.is_empty() {
                chunks.push(OutputChunk { stream: StreamKind::Stderr, seq: None, bytes: Cow::Borrowed(stderr) });
            }
            return chunks;
        };
        let mut stdout_at = 0usize;
        let mut stderr_at = 0usize;
        let mut chunks = Vec::with_capacity(spans.len());
        for span in spans {
            // spans_describe bounded every sum by a payload length, a usize.
            let len = span.len as usize;
            let bytes = match span.stream {
                StreamKind::Stdout => {
                    stdout_at += len;
                    slice_stdout(stdout_at - len, stdout_at)
                }
                StreamKind::Stderr => {
                    stderr_at += len;
                    Cow::Borrowed(&stderr[stderr_at - len..stderr_at])
                }
            };
            chunks.push(OutputChunk { stream: span.stream, seq: Some(span.seq), bytes });
        }
        chunks
    }

    /// Record how stdout and stderr interleave. An empty order clears it.
    ///
    /// The order is stored as given; [`Self::stream_order`] reports it only
    /// while it matches the payloads.
    pub fn set_stream_order(&mut self, order: StreamOrder) {
        self.stream_order = if order.is_empty() { None } else { Some(Box::new(order)) };
    }

    /// Take the order out, with every byte numbered (see
    /// [`StreamOrder::of`]), leaving none. Putting it back with
    /// [`Self::set_stream_order`] after appending costs only what was
    /// appended.
    pub fn take_stream_order(&mut self, sequence: &OutputSequence) -> StreamOrder {
        let mut order = self.stream_order.take().map_or_else(StreamOrder::new, |order| *order);
        order.cover(self.stdout_len(), self.err.len() as u64, sequence);
        order
    }

    /// Forget how stdout and stderr interleave.
    pub fn clear_stream_order(&mut self) {
        self.stream_order = None;
    }

    /// Number every byte not yet in a span, so [`Self::stream_order`]
    /// describes the whole result. See [`StreamOrder::of`].
    ///
    /// Stdout held only as structured output is rendered to measure it.
    pub fn stamp_stream_order(&mut self, sequence: &OutputSequence) {
        let order = StreamOrder::of(self, sequence);
        self.set_stream_order(order);
    }

    /// The spans as stored, whether or not they match the payloads.
    pub(crate) fn recorded_spans(&self) -> &[OutputSpan] {
        self.stream_order.as_deref().map_or(&[], StreamOrder::spans)
    }

    /// Length in bytes of the stdout that spans describe.
    pub(crate) fn stdout_len(&self) -> u64 {
        match &self.out {
            OutputPayload::Bytes(bytes) => bytes.len() as u64,
            OutputPayload::Text(text) if !text.is_empty() => text.len() as u64,
            OutputPayload::Text(_) => {
                self.output.as_ref().map_or(0, |output| output.to_canonical_string().len() as u64)
            }
        }
    }

    /// Stdout as bytes: a binary payload unchanged, text as `text_out`.
    fn stdout_bytes(&self) -> Cow<'_, [u8]> {
        match &self.out {
            OutputPayload::Bytes(bytes) => Cow::Borrowed(bytes),
            OutputPayload::Text(_) => match self.text_out() {
                Cow::Borrowed(text) => Cow::Borrowed(text.as_bytes()),
                Cow::Owned(text) => Cow::Owned(text.into_bytes()),
            },
        }
    }

    fn out_is_empty_text(&self) -> bool {
        matches!(&self.out, OutputPayload::Text(s) if s.is_empty())
    }

    fn remove_stream_spans(&mut self, stream: StreamKind) {
        if let Some(mut order) = self.stream_order.take() {
            order.remove_stream(stream);
            self.set_stream_order(*order);
        }
    }
}

/// Convert serde_json::Value to our AST Value.
///
/// Primitives are mapped to their corresponding Value variants.
/// Arrays and objects are preserved as `Value::Json` - use `jq` to query them.
pub fn json_to_value(json: serde_json::Value) -> Value {
    match json {
        serde_json::Value::Null => Value::Null,
        serde_json::Value::Bool(b) => Value::Bool(b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::Int(i)
            } else if let Some(f) = n.as_f64() {
                Value::Float(f)
            } else {
                Value::String(n.to_string())
            }
        }
        serde_json::Value::String(s) => Value::String(s),
        // A base64 byte envelope round-trips back to inline Bytes; any other
        // object/array stays structured Json.
        serde_json::Value::Object(_) => match crate::bytes::envelope_to_bytes(&json) {
            Some(bytes) => Value::Bytes(bytes),
            None => Value::Json(json),
        },
        serde_json::Value::Array(_) => Value::Json(json),
    }
}

/// Convert serde_json::Value to our AST Value **without** bytes-envelope sniffing.
///
/// External JSON (from `fromjson`, or native access traversal) must never
/// silently become a `Value::Bytes` just because an object happens to match the
/// base64 envelope shape (`{"_type":"bytes",…}`). That auto-decode is a feature
/// of *internal* round-tripping only — it would be a silent, surprising
/// conversion on untrusted input. Otherwise this is the same unwrap law as
/// [`json_to_value`]: JSON scalars unwrap to native `Value` variants, and only
/// objects/arrays stay `Value::Json`.
pub fn json_to_value_no_envelope(json: serde_json::Value) -> Value {
    match json {
        serde_json::Value::Null => Value::Null,
        serde_json::Value::Bool(b) => Value::Bool(b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::Int(i)
            } else if let Some(f) = n.as_f64() {
                Value::Float(f)
            } else {
                Value::String(n.to_string())
            }
        }
        serde_json::Value::String(s) => Value::String(s),
        // Objects and arrays stay structured — an envelope-shaped object is a
        // plain record here, not decoded to bytes.
        serde_json::Value::Object(_) | serde_json::Value::Array(_) => Value::Json(json),
    }
}

/// Convert our AST Value to serde_json::Value for serialization.
pub fn value_to_json(value: &Value) -> serde_json::Value {
    match value {
        Value::Null => serde_json::Value::Null,
        Value::Bool(b) => serde_json::Value::Bool(*b),
        Value::Int(i) => serde_json::Value::Number((*i).into()),
        Value::Float(f) => {
            // JSON has no NaN/Infinity. Rather than silently collapse them to
            // null (data loss), serialize the non-finite value to its string
            // form ("NaN", "inf", "-inf") so the information survives the trip.
            serde_json::Number::from_f64(*f)
                .map(serde_json::Value::Number)
                .unwrap_or_else(|| serde_json::Value::String(f.to_string()))
        }
        Value::String(s) => serde_json::Value::String(s.clone()),
        Value::Json(json) => json.clone(),
        Value::Bytes(data) => crate::bytes::bytes_to_envelope(data),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn success_creates_ok_result() {
        let result = ExecResult::success("hello world");
        assert!(result.ok());
        assert_eq!(result.code, 0);
        assert_eq!(&*result.text_out(),"hello world");
        assert!(result.err.is_empty());
    }

    #[test]
    fn value_to_json_finite_float_is_number() {
        assert_eq!(value_to_json(&Value::Float(3.5)), serde_json::json!(3.5));
    }

    #[test]
    fn value_to_json_non_finite_float_serializes_to_string() {
        // JSON has no NaN/Infinity — preserve the info as a string, never null.
        assert_eq!(value_to_json(&Value::Float(f64::NAN)), serde_json::json!("NaN"));
        assert_eq!(value_to_json(&Value::Float(f64::INFINITY)), serde_json::json!("inf"));
        assert_eq!(
            value_to_json(&Value::Float(f64::NEG_INFINITY)),
            serde_json::json!("-inf")
        );
        // Crucially: not null (the old data-losing behavior).
        assert_ne!(value_to_json(&Value::Float(f64::NAN)), serde_json::Value::Null);
    }

    #[test]
    fn failure_creates_non_ok_result() {
        let result = ExecResult::failure(1, "command not found");
        assert!(!result.ok());
        assert_eq!(result.code, 1);
        assert_eq!(result.err, "command not found\n");
    }

    #[test]
    fn failure_ends_exactly_one_newline() {
        assert_eq!(ExecResult::failure(1, "msg").err, "msg\n");
        assert_eq!(ExecResult::failure(1, "msg\n").err, "msg\n");
        assert_eq!(ExecResult::failure(1, "msg\n\n\n").err, "msg\n");
    }

    #[test]
    fn failure_empty_message_stays_empty() {
        // Callers branch on `err.is_empty()`; an empty failure must not mint
        // a bare blank line.
        assert_eq!(ExecResult::failure(1, "").err, "");
    }

    #[test]
    fn terminate_diagnostic_keeps_multiline_interior_newlines() {
        let msg = "wc: a: not found\nwc: b: not found";
        assert_eq!(
            ExecResult::terminate_diagnostic(msg),
            "wc: a: not found\nwc: b: not found\n"
        );
    }

    #[test]
    fn from_parts_ends_the_diagnostic_line() {
        assert_eq!(ExecResult::from_parts(1, String::new(), "boom".into(), None).err, "boom\n");
        assert_eq!(ExecResult::from_parts(1, String::new(), String::new(), None).err, "");
    }

    #[test]
    fn from_output_keeps_external_stderr_byte_faithful() {
        // A program that dies mid-line stays mid-line, as in bash: the
        // pass-through constructor must not add bytes.
        let result = ExecResult::from_output(1, "", "died mid-line");
        assert_eq!(result.err, "died mid-line");
    }

    #[test]
    fn success_does_not_sniff_json_stdout() {
        // External-command stdout is never sniffed for JSON. Tools that want
        // structured data must call success_with_data() / success_data().
        let result = ExecResult::success(r#"{"count": 42, "items": ["a", "b"]}"#);
        assert!(result.data.is_none());
        assert_eq!(&*result.text_out(),r#"{"count": 42, "items": ["a", "b"]}"#);
    }

    #[test]
    fn from_output_does_not_sniff_json_stdout() {
        let result = ExecResult::from_output(0, r#"[1, 2, 3]"#, "");
        assert!(result.data.is_none());
        assert_eq!(&*result.text_out(),"[1, 2, 3]");
    }

    #[test]
    fn non_json_stdout_has_no_data() {
        let result = ExecResult::success("just plain text");
        assert!(result.data.is_none());
    }

    #[test]
    fn success_data_creates_result_with_value() {
        let value = Value::String("test data".into());
        let result = ExecResult::success_data(value.clone());
        assert!(result.ok());
        assert_eq!(result.data, Some(value));
    }

    #[test]
    fn did_spill_defaults_to_false() {
        assert!(!ExecResult::success("hi").did_spill);
        assert!(!ExecResult::failure(1, "err").did_spill);
        assert!(!ExecResult::from_output(0, "out", "err").did_spill);
    }

    #[test]
    fn did_spill_is_serialized() {
        let mut result = ExecResult::success("hi");
        result.did_spill = true;
        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("\"did_spill\":true"));
    }

    #[test]
    fn original_code_omitted_when_none() {
        let result = ExecResult::success("hi");
        let json = serde_json::to_string(&result).unwrap();
        assert!(!json.contains("original_code"));
    }

    #[test]
    fn original_code_present_when_set() {
        let mut result = ExecResult::success("hi");
        result.original_code = Some(0);
        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("\"original_code\":0"));
    }

    #[test]
    fn default_is_empty_success() {
        let result = ExecResult::default();
        assert!(result.ok());
        assert!(result.text_out().is_empty());
        assert!(result.data.is_none());
        assert!(result.content_type.is_none());
        assert!(result.baggage.is_empty());
    }

    #[test]
    fn from_parts_creates_result() {
        let result = ExecResult::from_parts(42, "out".into(), "err".into(), None);
        assert_eq!(result.code, 42);
        assert_eq!(&*result.text_out(),"out");
        assert_eq!(result.err, "err\n");
        assert!(result.data.is_none());
        assert!(result.output.is_none());
    }

    #[test]
    fn with_code_sets_code() {
        let result = ExecResult::success("hi").with_code(42);
        assert_eq!(result.code, 42);
        assert_eq!(&*result.text_out(),"hi");
    }

    #[test]
    fn output_getter() {
        use crate::output::{OutputData, OutputNode};
        // Use structured (non-text) output so with_output preserves .output
        let nodes = OutputData::nodes(vec![OutputNode::new("a"), OutputNode::new("b")]);
        let result = ExecResult::with_output(nodes);
        assert!(result.output().is_some());
        assert!(result.has_output());

        // Simple text now routes to .out, so output is None
        let text_result = ExecResult::with_output(OutputData::text("test"));
        assert!(!text_result.has_output());
        assert_eq!(&*text_result.text_out(), "test");

        let plain = ExecResult::success("text");
        assert!(plain.output().is_none());
        assert!(!plain.has_output());
    }

    #[test]
    fn set_out_and_push_out_and_clear_out() {
        let mut result = ExecResult::success("");
        result.set_out("hello".into());
        assert_eq!(&*result.text_out(),"hello");
        result.push_out(" world");
        assert_eq!(&*result.text_out(),"hello world");
        result.clear_out();
        assert!(result.text_out().is_empty());
    }

    #[test]
    fn set_output_and_take_output() {
        use crate::output::OutputData;
        let mut result = ExecResult::success("");
        assert!(result.take_output().is_none());

        result.set_output(Some(OutputData::text("data")));
        assert!(result.has_output());

        let taken = result.take_output();
        assert!(taken.is_some());
        assert!(!result.has_output());
    }

    #[test]
    fn materialize_populates_out_from_output() {
        use crate::output::{OutputData, OutputNode};
        // Use structured output to test materialization
        let nodes = OutputData::nodes(vec![OutputNode::new("a"), OutputNode::new("b")]);
        let mut result = ExecResult::with_output(nodes);
        // Raw text payload is empty before materialize (text_out() would
        // already fall back to the OutputData canonical string).
        assert!(matches!(&result.out, OutputPayload::Text(s) if s.is_empty()));
        assert!(result.has_output());
        result.materialize();
        assert_eq!(&*result.text_out(),"a\nb\n");
        assert!(result.output.is_none());
    }

    #[test]
    fn value_bytes_round_trips_through_envelope() {
        let v = Value::Bytes(vec![0u8, 1, 2, 255, 128]);
        let json = value_to_json(&v);
        assert_eq!(json["_type"], "bytes");
        assert_eq!(json["len"], 5);
        // json_to_value recognizes the envelope and reconstructs Bytes.
        assert_eq!(json_to_value(json), v);
        // A plain object is NOT mistaken for bytes.
        let obj = serde_json::json!({"name": "amy"});
        assert!(matches!(json_to_value(obj), Value::Json(_)));
    }

    #[test]
    fn no_envelope_never_decodes_bytes() {
        // The envelope-free path is what external JSON (fromjson, access) uses:
        // an object matching the byte-envelope shape stays a plain record, it is
        // NOT silently decoded to Value::Bytes.
        let envelope = crate::bytes::bytes_to_envelope(&[1u8, 2, 3]);
        // The sniffing path DOES decode it (internal round-trip).
        assert!(matches!(json_to_value(envelope.clone()), Value::Bytes(_)));
        // The envelope-free path leaves it structured.
        assert!(matches!(
            json_to_value_no_envelope(envelope),
            Value::Json(serde_json::Value::Object(_))
        ));
    }

    #[test]
    fn no_envelope_shares_unwrap_law_for_scalars() {
        // Scalars unwrap identically to json_to_value; only the object arm differs.
        assert_eq!(json_to_value_no_envelope(serde_json::json!(42)), Value::Int(42));
        assert_eq!(json_to_value_no_envelope(serde_json::json!(1.5)), Value::Float(1.5));
        assert_eq!(json_to_value_no_envelope(serde_json::json!(true)), Value::Bool(true));
        assert_eq!(json_to_value_no_envelope(serde_json::json!("hi")), Value::String("hi".into()));
        assert_eq!(json_to_value_no_envelope(serde_json::json!(null)), Value::Null);
        assert!(matches!(
            json_to_value_no_envelope(serde_json::json!([1, 2])),
            Value::Json(serde_json::Value::Array(_))
        ));
    }

    #[test]
    fn output_payload_text_serializes_as_bare_string() {
        // Wire compatibility: a text result's `out` stays a plain JSON string,
        // exactly as when `out` was a `String`.
        let r = ExecResult::success("hello");
        let json: serde_json::Value = serde_json::from_str(&serde_json::to_string(&r).unwrap()).unwrap();
        assert_eq!(json["out"], "hello");
        // Round-trips back to a Text payload.
        let back: ExecResult = serde_json::from_value(json).unwrap();
        assert_eq!(&*back.text_out(), "hello");
        assert!(!back.is_bytes());
    }

    #[test]
    fn success_bytes_carries_binary_and_round_trips() {
        let r = ExecResult::success_bytes(vec![0u8, 159, 146, 150]); // invalid UTF-8
        assert!(r.is_bytes());
        assert_eq!(r.out_bytes(), Some(&[0u8, 159, 146, 150][..]));
        // try_text_out is the loud guard: invalid UTF-8 → error, not mangling.
        assert!(r.try_text_out().is_err());
        // text_out (infallible) decodes lossily — Phase-1 fallback.
        assert!(r.text_out().contains('\u{fffd}'));
        // Serializes as a base64 envelope and round-trips back to bytes.
        let json: serde_json::Value = serde_json::to_value(&r).unwrap();
        assert_eq!(json["out"]["_type"], "bytes");
        let back: ExecResult = serde_json::from_value(json).unwrap();
        assert_eq!(back.out_bytes(), Some(&[0u8, 159, 146, 150][..]));
    }

    #[test]
    fn valid_utf8_bytes_coerce_to_text() {
        let r = ExecResult::success_bytes(b"plain text".to_vec());
        assert!(r.is_bytes());
        assert_eq!(r.try_text_out().unwrap(), "plain text");
        assert_eq!(&*r.text_out(), "plain text");
    }

    #[test]
    fn materialize_preserves_existing_out() {
        use crate::output::OutputData;
        let mut result = ExecResult::with_output_and_text(OutputData::text("ignored"), "custom");
        result.materialize();
        assert_eq!(&*result.text_out(),"custom");
    }

    #[test]
    fn take_output_for_stream_when_out_empty() {
        use crate::output::{OutputData, OutputNode};
        // Use structured output — text now goes to .out directly
        let nodes = OutputData::nodes(vec![OutputNode::new("a")]);
        let mut result = ExecResult::with_output(nodes);
        let taken = result.take_output_for_stream();
        assert!(taken.is_some());
        assert!(!result.has_output());
    }

    #[test]
    fn with_output_simple_text_populates_out_directly() {
        use crate::output::OutputData;
        let result = ExecResult::with_output(OutputData::text("hello"));
        // Simple text should go to .out, not .output
        assert!(!result.has_output());
        assert_eq!(&*result.text_out(), "hello");
        // Even JSON-shaped text is NOT auto-parsed — .data stays None.
        let json_result = ExecResult::with_output(OutputData::text(r#"{"key": 1}"#));
        assert!(json_result.data.is_none());
    }


    #[test]
    fn clear_stdout_drops_data() {
        // A stdout redirect clears the data-plane .data unconditionally.
        let mut result = ExecResult::success_data(Value::Json(serde_json::json!([1, 2, 3])));
        result.clear_stdout();
        assert!(result.data.is_none(), "data-plane .data must clear");
    }

    fn interleaved(sequence: &OutputSequence) -> ExecResult {
        // out, err, out2 — the payloads hold them as two blocks.
        let mut order = StreamOrder::new();
        order.push(StreamKind::Stdout, 4, sequence);
        order.push(StreamKind::Stderr, 4, sequence);
        order.push(StreamKind::Stdout, 5, sequence);
        let mut result = ExecResult::from_output(0, "out\nout2\n", "err\n");
        result.set_stream_order(order);
        result
    }

    fn joined(result: &ExecResult) -> String {
        let bytes: Vec<u8> = result.chunks().iter().flat_map(|chunk| chunk.bytes.iter().copied()).collect();
        String::from_utf8(bytes).unwrap()
    }

    #[test]
    fn a_result_without_spans_reads_as_two_blocks() {
        let result = ExecResult::from_output(0, "out\n", "err\n");
        assert_eq!(result.stream_order(), None);
        let chunks = result.chunks();
        assert_eq!(chunks.len(), 2);
        assert_eq!((chunks[0].stream, chunks[0].seq), (StreamKind::Stdout, None));
        assert_eq!((chunks[1].stream, chunks[1].seq), (StreamKind::Stderr, None));
        assert_eq!(joined(&result), "out\nerr\n");
        assert!(ExecResult::success("").chunks().is_empty());
    }

    #[test]
    fn chunks_follow_the_recorded_order() {
        let sequence = OutputSequence::new();
        let result = interleaved(&sequence);
        let spans = result.stream_order().unwrap();
        assert_eq!(spans.len(), 3);
        assert_eq!(joined(&result), "out\nerr\nout2\n");
        // The payloads themselves are unchanged.
        assert_eq!(&*result.text_out(), "out\nout2\n");
        assert_eq!(result.err, "err\n");
        let seqs: Vec<Option<u64>> = result.chunks().iter().map(|chunk| chunk.seq).collect();
        assert_eq!(seqs, spans.iter().map(|span| Some(span.seq)).collect::<Vec<_>>());
    }

    #[test]
    fn spans_that_no_longer_match_the_payloads_are_not_reported() {
        let sequence = OutputSequence::new();
        let mut result = interleaved(&sequence);
        result.err.push_str("more\n");
        assert_eq!(result.stream_order(), None);
        assert_eq!(joined(&result), "out\nout2\nerr\nmore\n");
        // Stamping numbers the new bytes after the old ones.
        result.stamp_stream_order(&sequence);
        assert_eq!(joined(&result), "out\nerr\nout2\nmore\n");
    }

    #[test]
    fn replacing_stdout_drops_only_its_spans() {
        let sequence = OutputSequence::new();
        for replace in [
            (|r: &mut ExecResult| r.set_out("new\n".into())) as fn(&mut ExecResult),
            |r| r.set_out_bytes(b"new\n".to_vec()),
            |r| r.clear_out(),
            |r| r.clear_stdout(),
        ] {
            let mut result = interleaved(&sequence);
            replace(&mut result);
            let order = StreamOrder::of(&result, &sequence);
            let kinds: Vec<StreamKind> = order.spans().iter().map(|span| span.stream).collect();
            let stdout = result.stdout_len() > 0;
            let expected = if stdout {
                vec![StreamKind::Stderr, StreamKind::Stdout]
            } else {
                vec![StreamKind::Stderr]
            };
            assert_eq!(kinds, expected, "stderr span kept, stdout renumbered after it");
        }
    }

    #[test]
    fn appending_to_stdout_keeps_the_recorded_prefix() {
        let sequence = OutputSequence::new();
        let mut result = interleaved(&sequence);
        result.push_out("tail\n");
        result.stamp_stream_order(&sequence);
        assert_eq!(joined(&result), "out\nerr\nout2\ntail\n");
    }

    #[test]
    fn clearing_stderr_cuts_its_spans_when_stamped() {
        let sequence = OutputSequence::new();
        let mut result = interleaved(&sequence);
        result.err.clear();
        result.stamp_stream_order(&sequence);
        assert_eq!(result.stream_order().map(<[_]>::len), Some(2));
        assert_eq!(joined(&result), "out\nout2\n");
    }

    #[test]
    fn stamping_structured_output_measures_its_rendering() {
        use crate::output::{OutputData, OutputNode};
        let sequence = OutputSequence::new();
        let mut result = ExecResult::with_output(OutputData::nodes(vec![OutputNode::new("a"), OutputNode::new("b")]));
        result.err = "warn\n".into();
        result.stamp_stream_order(&sequence);
        let spans = result.stream_order().unwrap();
        assert_eq!(spans[0].len, 4, "rendered as a\\nb\\n");
        assert_eq!(joined(&result), "a\nb\nwarn\n");
        // Taking the structured output takes the stdout with it.
        result.take_output_for_stream();
        assert_eq!(result.stream_order().map(<[_]>::len), Some(1));
    }

    #[test]
    fn binary_stdout_chunks_are_raw_bytes() {
        let sequence = OutputSequence::new();
        let mut result = ExecResult::success_bytes(vec![0xff, 0x00]);
        result.err = "e".into();
        let mut order = StreamOrder::new();
        order.push(StreamKind::Stdout, 1, &sequence);
        order.push(StreamKind::Stderr, 1, &sequence);
        order.push(StreamKind::Stdout, 1, &sequence);
        result.set_stream_order(order);
        let bytes: Vec<u8> = result.chunks().iter().flat_map(|chunk| chunk.bytes.iter().copied()).collect();
        assert_eq!(bytes, vec![0xff, b'e', 0x00]);
    }

    #[test]
    fn stream_order_stays_off_the_wire_when_absent() {
        let json = serde_json::to_string(&ExecResult::from_output(0, "out\n", "err\n")).unwrap();
        assert!(!json.contains("stream_order"), "{json}");
    }

    #[test]
    fn stream_order_round_trips_through_serde() {
        let sequence = OutputSequence::new();
        let result = interleaved(&sequence);
        let json = serde_json::to_value(&result).unwrap();
        assert_eq!(json["stream_order"][1]["stream"], "stderr");
        let back: ExecResult = serde_json::from_value(json).unwrap();
        assert_eq!(back.stream_order(), result.stream_order());
        assert_eq!(joined(&back), "out\nerr\nout2\n");
    }

    #[test]
    fn take_output_for_stream_when_out_populated() {
        use crate::output::OutputData;
        let mut result = ExecResult::with_output_and_text(OutputData::text("x"), "custom");
        let taken = result.take_output_for_stream();
        assert!(taken.is_none());
        assert!(result.has_output()); // not taken
    }
}
