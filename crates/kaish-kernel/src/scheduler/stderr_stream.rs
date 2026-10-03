//! Kernel-level stderr stream for real-time error output from pipeline stages.
//!
//! In bash, stderr from ALL pipeline stages streams to the terminal simultaneously.
//! kaish achieves this with `StderrStream` — a cloneable, Send+Sync handle backed
//! by an unbounded mpsc channel. Pipeline stages and external commands write to it
//! concurrently; the kernel drains it to the actual stderr sink.
//!
//! The channel carries raw bytes (`Vec<u8>`), not strings. This avoids UTF-8
//! corruption when multi-byte characters are split across chunk boundaries in
//! external command stderr reads. Lossy decode happens once at the kernel drain
//! site — the presentation boundary.
//!
//! ```text
//!   Stage 1 ──┐
//!   Stage 2 ──┼──▶ StderrStream (mpsc, bytes) ──▶ drain ──▶ from_utf8_lossy ──▶ output
//!   Stage 3 ──┘
//! ```

use kaish_types::{ExecResult, OutputSequence, OutputSpan, StreamKind, StreamOrder};
use tokio::sync::mpsc;

/// Cloneable handle to the kernel's stderr output stream.
///
/// Multiple pipeline stages can write concurrently. The receiver side
/// is drained by the kernel after each statement (or by a background task).
///
/// Uses `UnboundedSender` which is `Clone + Send + Sync` — safe across
/// `tokio::spawn` boundaries without `Arc<Mutex<..>>`.
#[derive(Clone, Debug)]
pub struct StderrStream {
    sender: mpsc::UnboundedSender<StderrChunk>,
    /// Numbers each write, so a drain can place it among the output it
    /// joins (`ExecResult::stream_order`).
    sequence: OutputSequence,
}

/// One write to the stream, with how many of its leading bytes a background
/// job's stderr stream already holds, and its output sequence number.
#[derive(Debug)]
pub(crate) struct StderrChunk {
    pub(crate) bytes: Vec<u8>,
    pub(crate) published_len: usize,
    pub(crate) seq: u64,
}

/// Receiving end of the stderr stream.
///
/// Owned by the kernel. Call `drain_lossy()` to collect all pending bytes
/// as a UTF-8 string (with lossy decode at this boundary).
pub struct StderrReceiver {
    receiver: mpsc::UnboundedReceiver<StderrChunk>,
}

/// Create a new stderr stream pair.
///
/// Writes are numbered by a counter of the stream's own; a kernel uses
/// [`numbered_stderr_stream`] to number them in its output sequence.
pub fn stderr_stream() -> (StderrStream, StderrReceiver) {
    numbered_stderr_stream(OutputSequence::new())
}

/// Create a stderr stream pair whose writes take numbers from `sequence`,
/// for placing them in `ExecResult::stream_order`.
pub fn numbered_stderr_stream(sequence: OutputSequence) -> (StderrStream, StderrReceiver) {
    let (sender, receiver) = mpsc::unbounded_channel();
    (
        StderrStream { sender, sequence },
        StderrReceiver { receiver },
    )
}

impl StderrStream {
    /// Write raw bytes to the stderr stream.
    ///
    /// Non-blocking. If the receiver has been dropped, the data is silently
    /// discarded (same as writing to a closed pipe).
    pub fn write(&self, data: &[u8]) {
        self.write_partly_published(data, 0);
    }

    /// Write bytes whose first `published_len` already reached the job's
    /// stderr stream, so the drain site publishes only the rest.
    pub(crate) fn write_partly_published(&self, data: &[u8], published_len: usize) {
        if !data.is_empty() {
            self.write_numbered(data, published_len, self.sequence.next());
        }
    }

    /// Write a result's stderr, one chunk per stderr span of its stream
    /// order, so each run keeps the number it was produced under. Bytes
    /// without a span are numbered now.
    pub(crate) fn write_result_stderr(&self, result: &ExecResult) {
        let err = result.err.as_bytes();
        assert!(
            result.stderr_published_len <= err.len(),
            "stderr claims {} published bytes of {}",
            result.stderr_published_len,
            err.len()
        );
        let mut published = result.stderr_published_len;
        let mut at = 0usize;
        for span in StreamOrder::of(result, &self.sequence).spans() {
            if span.stream != StreamKind::Stderr {
                continue;
            }
            // `of` covers exactly `err.len()` stderr bytes.
            let end = at + span.len as usize;
            let chunk_published = published.min(end - at);
            published -= chunk_published;
            self.write_numbered(&err[at..end], chunk_published, span.seq);
            at = end;
        }
    }

    fn write_numbered(&self, data: &[u8], published_len: usize, seq: u64) {
        assert!(
            published_len <= data.len(),
            "stderr chunk claims {published_len} published bytes of {}",
            data.len()
        );
        if !data.is_empty() {
            // Ignore send errors — receiver dropped means nobody is listening
            let _ = self.sender.send(StderrChunk { bytes: data.to_vec(), published_len, seq });
        }
    }

    /// Write a UTF-8 string to the stderr stream.
    ///
    /// Convenience for builtins that produce string stderr.
    pub fn write_str(&self, msg: &str) {
        self.write(msg.as_bytes());
    }
}

impl StderrReceiver {
    /// Drain all pending bytes and decode as UTF-8 (lossy).
    ///
    /// This is the single presentation boundary where bytes become a String.
    /// Multi-byte characters that were split across chunks are reassembled
    /// here because all chunks are concatenated before decoding.
    ///
    /// Returns an empty string if no messages are pending.
    /// Non-blocking — returns immediately with whatever is available.
    ///
    /// Text only: how much of it a job's stream already holds is dropped.
    /// The kernel drains with `drain_chunks`, which keeps that count.
    pub fn drain_lossy(&mut self) -> String {
        lossy_text(&self.drain_chunks())
    }

    /// Drain all pending chunks, keeping each one's published length.
    pub(crate) fn drain_chunks(&mut self) -> Vec<StderrChunk> {
        let mut chunks = Vec::new();
        while let Ok(chunk) = self.receiver.try_recv() {
            chunks.push(chunk);
        }
        chunks
    }
}

/// The stream order of `chunks` decoded into `text` by [`lossy_text`]: one
/// stderr span per chunk, with the number it was written under. When the
/// decode changed the bytes, per-chunk lengths no longer apply, and the text
/// is one span under the earliest number.
///
/// `leading` bytes placed before the text (a line separator) join the first
/// span.
pub(crate) fn drained_order(chunks: &[StderrChunk], text: &str, leading: usize, sequence: &OutputSequence) -> StreamOrder {
    let mut order = StreamOrder::new();
    let unchanged = chunks.iter().map(|chunk| chunk.bytes.len()).sum::<usize>() == text.len()
        && chunks.iter().flat_map(|chunk| chunk.bytes.iter()).eq(text.as_bytes().iter());
    if unchanged {
        // One stream, so the channel's delivery order is the payload order;
        // a chunk delivered after a later-numbered one is renumbered.
        let mut leading = leading;
        for chunk in chunks {
            let len = chunk.bytes.len() + std::mem::take(&mut leading);
            order.push_span(OutputSpan::new(chunk.seq, StreamKind::Stderr, len as u64), sequence);
        }
    } else if let Some(first) = chunks.iter().map(|chunk| chunk.seq).min() {
        order.push_span(OutputSpan::new(first, StreamKind::Stderr, (leading + text.len()) as u64), sequence);
    }
    order
}

/// Concatenate chunks and decode once, so a character split across two
/// chunks is reassembled.
pub(crate) fn lossy_text(chunks: &[StderrChunk]) -> String {
    let mut buf = Vec::new();
    for chunk in chunks {
        buf.extend_from_slice(&chunk.bytes);
    }
    if buf.is_empty() {
        String::new()
    } else {
        String::from_utf8_lossy(&buf).into_owned()
    }
}
