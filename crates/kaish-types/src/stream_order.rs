//! The order in which a result's stdout and stderr bytes were produced.
//!
//! An [`ExecResult`](crate::ExecResult) holds stdout and stderr as two
//! separate payloads. [`OutputSpan`]s record how those payloads interleave:
//! each span names a stream and a byte count, and the spans in list order
//! consume each stream's payload from its start. A kernel-wide
//! [`OutputSequence`] numbers the spans, so the list order is also the order
//! in which kaish saw the bytes.
//!
//! ```
//! use kaish_types::{ExecResult, OutputSequence, StreamKind, StreamOrder};
//!
//! let sequence = OutputSequence::new();
//! let mut order = StreamOrder::new();
//! order.push(StreamKind::Stdout, 4, &sequence);
//! order.push(StreamKind::Stderr, 4, &sequence);
//! order.push(StreamKind::Stdout, 5, &sequence);
//!
//! let mut result = ExecResult::from_output(0, "out\nout2\n", "err\n");
//! result.set_stream_order(order);
//! let text: Vec<u8> = result.chunks().iter().flat_map(|c| c.bytes.iter().copied()).collect();
//! assert_eq!(text, b"out\nerr\nout2\n");
//! ```
//!
//! Exactness depends on the producer. A single builtin returns its stdout and
//! stderr whole, with no order inside the command. An external command's two
//! pipes are ordered by when kaish read each chunk, not by when the program
//! wrote it. The result where output spills or is truncated has no spans. A
//! result without spans reads as its stdout block followed by its stderr
//! block.

use std::borrow::Cow;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

/// One of a result's two output streams.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum StreamKind {
    /// Standard output.
    Stdout,
    /// Standard error.
    Stderr,
}

/// A run of bytes from one stream, numbered by the kernel's [`OutputSequence`].
///
/// `len` counts bytes of the stream's payload: for stdout, the bytes of
/// [`ExecResult::text_out`](crate::ExecResult::text_out) (or of
/// [`ExecResult::out_bytes`](crate::ExecResult::out_bytes) for a binary
/// payload); for stderr, the bytes of `err`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
#[non_exhaustive]
pub struct OutputSpan {
    /// Position in the kernel's output sequence. Higher means later.
    pub seq: u64,
    /// The stream these bytes belong to.
    pub stream: StreamKind,
    /// Number of bytes in this run.
    pub len: u64,
}

impl OutputSpan {
    /// Create a span.
    pub fn new(seq: u64, stream: StreamKind, len: u64) -> Self {
        Self { seq, stream, len }
    }
}

/// A kernel's output counter: one number per run of output, never reused.
///
/// Clones share the counter, so a kernel, its forks, and its pipeline stages
/// number their output in one sequence. Numbers are unique and increase in
/// the order they are taken. They are not comparable between kernels.
#[derive(Debug, Clone, Default)]
pub struct OutputSequence(Arc<AtomicU64>);

impl OutputSequence {
    /// Create a counter that starts at 0.
    pub fn new() -> Self {
        Self::default()
    }

    /// Take the next number.
    pub fn next(&self) -> u64 {
        // Relaxed: every fetch_add on one atomic is totally ordered, which is
        // the only guarantee the numbers carry.
        self.0.fetch_add(1, Ordering::Relaxed)
    }
}

/// A run of output bytes in production order, from [`ExecResult::chunks`](crate::ExecResult::chunks).
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct OutputChunk<'a> {
    /// The stream the bytes belong to.
    pub stream: StreamKind,
    /// The span's sequence number, or `None` when the result has no spans.
    pub seq: Option<u64>,
    /// The bytes.
    pub bytes: Cow<'a, [u8]>,
}

/// A list of [`OutputSpan`]s under construction.
///
/// Every operation keeps the list valid: no empty spans and sequence numbers
/// strictly increasing. Each run keeps its own span until [`Self::append`]
/// adds spans numbered after it; then adjacent runs on one stream merge into
/// one span that keeps the first number, since nothing can be placed between
/// them any more.
///
/// Serializes as its list of spans.
#[derive(Debug, Clone, Default)]
pub struct StreamOrder {
    spans: Vec<OutputSpan>,
    /// Bytes recorded per stream, indexed by [`stream_index`].
    totals: [u64; 2],
    /// `spans[..merged]` have had adjacent same-stream runs merged.
    merged: usize,
}

impl PartialEq for StreamOrder {
    fn eq(&self, other: &Self) -> bool {
        self.spans == other.spans
    }
}

impl Eq for StreamOrder {}

impl serde::Serialize for StreamOrder {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.spans.serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for StreamOrder {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        Ok(Self::from_recorded(Vec::deserialize(deserializer)?))
    }
}

fn stream_index(stream: StreamKind) -> usize {
    match stream {
        StreamKind::Stdout => 0,
        StreamKind::Stderr => 1,
    }
}

impl StreamOrder {
    /// An empty order.
    pub fn new() -> Self {
        Self::default()
    }

    /// The order `result` describes, with every byte numbered.
    ///
    /// Spans are read as describing a prefix of each stream. Spans beyond a
    /// stream's length are cut back, and bytes past the spans (text appended
    /// to `err` directly, for example) get new numbers: stdout's first, then
    /// stderr's. A result without spans becomes its stdout block, then its
    /// stderr block.
    pub fn of(result: &crate::ExecResult, sequence: &OutputSequence) -> Self {
        let mut order = Self::recorded(result, sequence);
        order.cover(result.stdout_len(), result.err.len() as u64, sequence);
        order
    }

    /// The spans `result` holds, without measuring its payloads.
    ///
    /// For a caller that knows the payload lengths and calls [`Self::cover`]
    /// itself, which spares rendering structured stdout to measure it.
    pub fn recorded(result: &crate::ExecResult, sequence: &OutputSequence) -> Self {
        let mut order = Self::default();
        for span in result.recorded_spans() {
            order.push_span(*span, sequence);
        }
        order
    }

    /// The spans, in order.
    pub fn spans(&self) -> &[OutputSpan] {
        &self.spans
    }

    /// True when no span is recorded.
    pub fn is_empty(&self) -> bool {
        self.spans.is_empty()
    }

    /// Total bytes recorded for `stream`.
    pub fn len_of(&self, stream: StreamKind) -> u64 {
        self.totals[stream_index(stream)]
    }

    /// Append `len` bytes of `stream`, numbered now.
    pub fn push(&mut self, stream: StreamKind, len: u64, sequence: &OutputSequence) {
        if len == 0 {
            return;
        }
        self.totals[stream_index(stream)] += len;
        self.spans.push(OutputSpan::new(sequence.next(), stream, len));
    }

    /// Append a span that already has a number.
    ///
    /// A number not above the last span's is replaced by a new one: the list
    /// order follows the payloads, so a run that arrives after a later-numbered
    /// one (from a concurrent pipeline stage, say) is placed after it.
    pub fn push_span(&mut self, span: OutputSpan, sequence: &OutputSequence) {
        if span.len == 0 {
            return;
        }
        self.totals[stream_index(span.stream)] += span.len;
        match self.spans.last() {
            Some(last) if last.seq >= span.seq => {
                self.spans.push(OutputSpan::new(sequence.next(), span.stream, span.len));
            }
            _ => self.spans.push(span),
        }
    }

    /// Add `other`'s spans, for a payload that appends `other`'s stdout to
    /// this stdout and `other`'s stderr to this stderr.
    ///
    /// Each stream keeps its payload order: these spans of a stream come
    /// before `other`'s spans of that stream. Across streams the lower number
    /// goes first, so stderr collected after a command, but written while it
    /// ran, keeps its place among the command's stdout. Spans numbered below
    /// all of `other`'s are not moved, so appending a later result costs only
    /// its own length.
    pub fn append(&mut self, other: StreamOrder, sequence: &OutputSequence) {
        let Some(first) = other.spans.iter().min_by_key(|span| span.seq) else {
            return;
        };
        let split = self.spans.partition_point(|span| span.seq < first.seq);
        let split = self.merge_runs_before(split);
        let tail: Vec<OutputSpan> = self.spans.drain(split..).collect();
        for span in &tail {
            self.totals[stream_index(span.stream)] -= span.len;
        }
        // One queue per stream, each in payload order: my spans, then theirs.
        let queue = |stream: StreamKind| {
            tail.iter()
                .chain(other.spans.iter())
                .filter(move |span| span.stream == stream)
                .copied()
                .peekable()
        };
        let mut stdout = queue(StreamKind::Stdout);
        let mut stderr = queue(StreamKind::Stderr);
        loop {
            let next = match (stdout.peek(), stderr.peek()) {
                (None, None) => break,
                (Some(_), None) => stdout.next(),
                (None, Some(_)) => stderr.next(),
                (Some(out), Some(err)) if out.seq < err.seq => stdout.next(),
                (Some(_), Some(_)) => stderr.next(),
            };
            if let Some(span) = next {
                self.push_span(span, sequence);
            }
        }
    }

    /// Add `other`'s spans after all of these, in `other`'s order, for text
    /// placed ahead of `other` whatever its number. Spans that would break
    /// the number order are renumbered.
    pub fn extend(&mut self, other: StreamOrder, sequence: &OutputSequence) {
        let end = self.spans.len();
        self.merge_runs_before(end);
        for span in other.spans {
            self.push_span(span, sequence);
        }
    }

    /// Merge adjacent same-stream runs in `spans[..end]`, returning where
    /// `spans[end]` now sits. Work already done is not repeated.
    fn merge_runs_before(&mut self, end: usize) -> usize {
        let start = self.merged.min(end);
        let mut write = start;
        for read in start..end {
            let span = self.spans[read];
            match write.checked_sub(1).map(|previous| &mut self.spans[previous]) {
                Some(previous) if previous.stream == span.stream => previous.len += span.len,
                _ => {
                    self.spans[write] = span;
                    write += 1;
                }
            }
        }
        self.spans.drain(write..end);
        self.merged = write;
        write
    }

    /// Make the spans cover exactly `stdout_len` and `stderr_len` bytes.
    ///
    /// Spans past a stream's length are cut back; missing bytes are appended
    /// with new numbers, stdout first.
    pub fn cover(&mut self, stdout_len: u64, stderr_len: u64, sequence: &OutputSequence) {
        self.truncate_stream(StreamKind::Stdout, stdout_len);
        self.truncate_stream(StreamKind::Stderr, stderr_len);
        let stdout_missing = stdout_len - self.len_of(StreamKind::Stdout);
        let stderr_missing = stderr_len - self.len_of(StreamKind::Stderr);
        self.push(StreamKind::Stdout, stdout_missing, sequence);
        self.push(StreamKind::Stderr, stderr_missing, sequence);
    }

    /// Drop every span of `stream`.
    pub fn remove_stream(&mut self, stream: StreamKind) {
        self.truncate_stream(stream, 0);
    }

    /// Keep the first `keep` bytes of `stream`'s spans and drop the rest.
    fn truncate_stream(&mut self, stream: StreamKind, keep: u64) {
        if self.len_of(stream) <= keep {
            return;
        }
        let mut remaining = keep;
        let mut kept = Vec::with_capacity(self.spans.len());
        for mut span in self.spans.drain(..) {
            if span.stream == stream {
                span.len = span.len.min(remaining);
                remaining -= span.len;
            }
            if span.len > 0 {
                kept.push(span);
            }
        }
        self.spans = kept;
        self.totals[stream_index(stream)] = keep;
        self.merged = 0;
    }

    /// Spans as stored on a result, which may not satisfy this type's rules.
    pub(crate) fn from_recorded(spans: Vec<OutputSpan>) -> Self {
        let mut totals = [0u64; 2];
        for span in &spans {
            let total = &mut totals[stream_index(span.stream)];
            *total = total.saturating_add(span.len);
        }
        Self { spans, totals, merged: 0 }
    }

}

/// Check that `spans` describe payloads of `stdout_len` and `stderr_len` bytes.
pub(crate) fn spans_describe(spans: &[OutputSpan], stdout_len: u64, stderr_len: u64) -> bool {
    let mut stdout = 0u64;
    let mut stderr = 0u64;
    let mut last_seq: Option<u64> = None;
    for span in spans {
        if span.len == 0 || last_seq.is_some_and(|last| last >= span.seq) {
            return false;
        }
        last_seq = Some(span.seq);
        match span.stream {
            StreamKind::Stdout => stdout = stdout.saturating_add(span.len),
            StreamKind::Stderr => stderr = stderr.saturating_add(span.len),
        }
    }
    stdout == stdout_len && stderr == stderr_len
}

#[cfg(test)]
mod tests {
    use super::*;

    fn kinds(order: &StreamOrder) -> Vec<(StreamKind, u64)> {
        order.spans().iter().map(|span| (span.stream, span.len)).collect()
    }

    #[test]
    fn sequence_numbers_increase_and_clones_share_them() {
        let sequence = OutputSequence::new();
        let clone = sequence.clone();
        assert_eq!(sequence.next(), 0);
        assert_eq!(clone.next(), 1);
        assert_eq!(sequence.next(), 2);
    }

    #[test]
    fn runs_merge_once_later_spans_are_appended() {
        let sequence = OutputSequence::new();
        let mut order = StreamOrder::new();
        order.push(StreamKind::Stdout, 2, &sequence);
        order.push(StreamKind::Stdout, 3, &sequence);
        order.push(StreamKind::Stderr, 0, &sequence);
        order.push(StreamKind::Stderr, 4, &sequence);
        assert_eq!(order.spans().len(), 3, "runs stay apart while something may still go between");
        let mut later = StreamOrder::new();
        later.push(StreamKind::Stderr, 1, &sequence);
        order.append(later, &sequence);
        assert_eq!(
            kinds(&order),
            vec![(StreamKind::Stdout, 5), (StreamKind::Stderr, 4), (StreamKind::Stderr, 1)]
        );
        assert_eq!(order.spans()[0].seq, 0, "a merged span keeps the first number");
        assert_eq!(order.len_of(StreamKind::Stdout), 5);
        assert_eq!(order.len_of(StreamKind::Stderr), 5);
    }

    #[test]
    fn push_span_renumbers_a_span_that_arrives_out_of_order() {
        let sequence = OutputSequence::new();
        let early = OutputSpan::new(sequence.next(), StreamKind::Stderr, 3);
        let mut order = StreamOrder::new();
        order.push(StreamKind::Stdout, 2, &sequence);
        order.push_span(early, &sequence);
        let spans = order.spans();
        assert_eq!(kinds(&order), vec![(StreamKind::Stdout, 2), (StreamKind::Stderr, 3)]);
        assert!(spans[1].seq > spans[0].seq, "list order and numbers agree: {spans:?}");
    }

    #[test]
    fn push_span_keeps_a_number_that_is_already_later() {
        let sequence = OutputSequence::new();
        let mut order = StreamOrder::new();
        order.push(StreamKind::Stdout, 2, &sequence);
        let later = OutputSpan::new(sequence.next() + 10, StreamKind::Stderr, 1);
        order.push_span(later, &sequence);
        assert_eq!(order.spans()[1], later);
    }

    fn numbered(spans: &[(u64, StreamKind, u64)]) -> StreamOrder {
        let mut order = StreamOrder::new();
        let sequence = OutputSequence::new();
        for &(seq, stream, len) in spans {
            order.push_span(OutputSpan::new(seq, stream, len), &sequence);
        }
        order
    }

    fn seqs(order: &StreamOrder) -> Vec<(u64, StreamKind)> {
        order.spans().iter().map(|span| (span.seq, span.stream)).collect()
    }

    #[test]
    fn append_interleaves_by_number_across_streams() {
        use StreamKind::{Stderr, Stdout};
        let sequence = OutputSequence::new();
        // Stderr collected late, but written at 3 and 9; stdout read at 5.
        let mut order = numbered(&[(3, Stderr, 4), (9, Stderr, 4)]);
        order.append(numbered(&[(5, Stdout, 4)]), &sequence);
        assert_eq!(seqs(&order), vec![(3, Stderr), (5, Stdout), (9, Stderr)]);
    }

    #[test]
    fn append_keeps_each_stream_in_payload_order() {
        use StreamKind::{Stderr, Stdout};
        let sequence = OutputSequence::new();
        for _ in 0..20 {
            sequence.next();
        }
        // The appended stderr follows the existing stderr in the payload,
        // whatever its number.
        let mut order = numbered(&[(10, Stderr, 1)]);
        order.append(numbered(&[(2, Stderr, 1), (4, Stdout, 1)]), &sequence);
        let spans = order.spans();
        assert_eq!(spans[0].stream, Stdout, "stdout may move ahead: {spans:?}");
        assert_eq!((spans[1].seq, spans[1].stream), (10, Stderr));
        assert!(spans.windows(2).all(|pair| pair[0].seq < pair[1].seq), "{spans:?}");
        assert_eq!(order.len_of(Stderr), 2);
    }

    #[test]
    fn append_leaves_an_earlier_prefix_alone() {
        use StreamKind::{Stderr, Stdout};
        let sequence = OutputSequence::new();
        let mut order = numbered(&[(1, Stdout, 1), (2, Stderr, 1), (3, Stdout, 1)]);
        order.append(numbered(&[(7, Stderr, 1), (8, Stdout, 1)]), &sequence);
        assert_eq!(
            seqs(&order),
            vec![(1, Stdout), (2, Stderr), (3, Stdout), (7, Stderr), (8, Stdout)]
        );
    }

    #[test]
    fn extend_keeps_the_added_order_whatever_the_numbers() {
        use StreamKind::{Stderr, Stdout};
        let sequence = OutputSequence::new();
        for _ in 0..20 {
            sequence.next();
        }
        let mut order = numbered(&[(10, Stderr, 1)]);
        order.extend(numbered(&[(2, Stderr, 1), (4, Stdout, 1)]), &sequence);
        let kinds: Vec<StreamKind> = order.spans().iter().map(|span| span.stream).collect();
        assert_eq!(kinds, vec![Stderr, Stderr, Stdout]);
        assert!(order.spans().windows(2).all(|pair| pair[0].seq < pair[1].seq));
    }

    #[test]
    fn cover_cuts_back_and_fills_in() {
        let sequence = OutputSequence::new();
        let mut order = StreamOrder::new();
        order.push(StreamKind::Stdout, 4, &sequence);
        order.push(StreamKind::Stderr, 4, &sequence);
        order.push(StreamKind::Stdout, 4, &sequence);
        // stderr cleared, stdout grew by 2.
        order.cover(10, 0, &sequence);
        assert_eq!(kinds(&order), vec![(StreamKind::Stdout, 4), (StreamKind::Stdout, 4), (StreamKind::Stdout, 2)]);
        order.cover(3, 2, &sequence);
        assert_eq!(kinds(&order), vec![(StreamKind::Stdout, 3), (StreamKind::Stderr, 2)]);
    }

    #[test]
    fn spans_describe_checks_lengths_numbers_and_empty_spans() {
        let ok = [OutputSpan::new(1, StreamKind::Stdout, 2), OutputSpan::new(4, StreamKind::Stderr, 3)];
        assert!(spans_describe(&ok, 2, 3));
        assert!(!spans_describe(&ok, 2, 4), "stderr length differs");
        assert!(!spans_describe(&ok, 1, 3), "stdout length differs");
        let backwards = [OutputSpan::new(4, StreamKind::Stdout, 2), OutputSpan::new(1, StreamKind::Stderr, 3)];
        assert!(!spans_describe(&backwards, 2, 3), "numbers must increase");
        let empty = [OutputSpan::new(1, StreamKind::Stdout, 0)];
        assert!(!spans_describe(&empty, 0, 0), "a span holds at least one byte");
    }

    #[test]
    fn spans_serialize_with_lowercase_stream_names() {
        let span = OutputSpan::new(7, StreamKind::Stderr, 3);
        let json = serde_json::to_value(span).unwrap();
        assert_eq!(json, serde_json::json!({"seq": 7, "stream": "stderr", "len": 3}));
        assert_eq!(serde_json::from_value::<OutputSpan>(json).unwrap(), span);
    }
}
