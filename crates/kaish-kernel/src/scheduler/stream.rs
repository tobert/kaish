//! Bounded streams for external command output capture.
//!
//! Provides ring-buffer-backed streams that:
//! - Bound memory usage (prevents OOM from large output)
//! - Evict oldest data when capacity is exceeded
//! - Support concurrent writes from async tasks
//! - Provide snapshot reads for observability

use std::collections::VecDeque;
use std::sync::Arc;
use tokio::sync::RwLock;

/// Default maximum size for bounded streams (10MB).
pub const DEFAULT_STREAM_MAX_SIZE: usize = 10 * 1024 * 1024;

/// A bounded stream backed by a ring buffer.
///
/// When writes exceed capacity, the oldest data is evicted to make room.
/// This prevents unbounded memory growth from chatty commands while still
/// keeping recent output available for inspection.
///
/// # Example
///
/// ```ignore
/// use kaish_kernel::scheduler::BoundedStream;
///
/// let stream = BoundedStream::new(100); // 100 byte max
///
/// stream.write(b"hello ").await;
/// stream.write(b"world").await;
///
/// let snapshot = stream.read().await;
/// assert_eq!(&snapshot, b"hello world");
/// ```
#[derive(Clone)]
pub struct BoundedStream {
    inner: Arc<RwLock<BoundedStreamInner>>,
    /// Fires on every accepted write and on close, so a reader can await new
    /// data instead of poll-looping — see [`BoundedStream::changed_since`].
    /// Held beside the lock, not inside it, so a waiter can register without
    /// contending with the writer it is waiting for.
    notify: Arc<tokio::sync::Notify>,
}

struct BoundedStreamInner {
    /// Ring buffer holding the data.
    buffer: VecDeque<u8>,
    /// Maximum buffer size in bytes.
    max_size: usize,
    /// Total bytes written (lifetime counter, for diagnostics).
    total_written: u64,
    /// Number of bytes evicted due to overflow.
    bytes_evicted: u64,
    /// Whether the stream has been closed (no more writes expected).
    closed: bool,
}

impl BoundedStream {
    /// Create a new bounded stream with the specified maximum size.
    pub fn new(max_size: usize) -> Self {
        Self {
            inner: Arc::new(RwLock::new(BoundedStreamInner {
                buffer: VecDeque::with_capacity(max_size.min(8192)), // Don't preallocate huge buffers
                max_size,
                total_written: 0,
                bytes_evicted: 0,
                closed: false,
            })),
            notify: Arc::new(tokio::sync::Notify::new()),
        }
    }

    /// Create a new bounded stream with the default max size (10MB).
    pub fn default_size() -> Self {
        Self::new(DEFAULT_STREAM_MAX_SIZE)
    }

    /// Write data to the stream.
    ///
    /// If the write would exceed capacity, the oldest data is evicted first.
    /// Writing to a closed stream is silently ignored.
    pub async fn write(&self, data: &[u8]) {
        {
            let mut inner = self.inner.write().await;

            if inner.closed {
                return;
            }

            inner.total_written += data.len() as u64;

            // If data itself is larger than max_size, only keep the tail
            if data.len() >= inner.max_size {
                let start = data.len() - inner.max_size;
                inner.bytes_evicted += inner.buffer.len() as u64 + start as u64;
                inner.buffer.clear();
                inner.buffer.extend(&data[start..]);
            } else {
                // Evict oldest data if needed to make room
                let needed = data.len();
                let available = inner.max_size.saturating_sub(inner.buffer.len());

                if needed > available {
                    let to_evict = needed - available;
                    let actual_evict = to_evict.min(inner.buffer.len());
                    inner.buffer.drain(..actual_evict);
                    inner.bytes_evicted += actual_evict as u64;
                }

                // Append new data
                inner.buffer.extend(data);
            }
        }
        // Outside the lock: a woken waiter reads `stats()`, which takes it.
        self.notify.notify_waiters();
    }

    /// Read a snapshot of the current buffer contents.
    ///
    /// Returns a copy of all data currently in the buffer.
    /// The buffer is not modified.
    pub async fn read(&self) -> Vec<u8> {
        let inner = self.inner.read().await;
        inner.buffer.iter().copied().collect()
    }

    /// Read the current buffer as a string (lossy UTF-8 conversion).
    pub async fn read_string(&self) -> String {
        let data = self.read().await;
        String::from_utf8_lossy(&data).into_owned()
    }

    /// Close the stream, indicating no more writes are expected.
    ///
    /// Subsequent writes will be silently ignored.
    pub async fn close(&self) {
        {
            let mut inner = self.inner.write().await;
            inner.closed = true;
        }
        // A waiter blocked on `changed_since` must not park forever on a
        // stream that will never produce another byte.
        self.notify.notify_waiters();
    }

    /// Check if the stream has been closed.
    pub async fn is_closed(&self) -> bool {
        let inner = self.inner.read().await;
        inner.closed
    }

    /// Get the current buffer size in bytes.
    pub async fn len(&self) -> usize {
        let inner = self.inner.read().await;
        inner.buffer.len()
    }

    /// Check if the buffer is empty.
    pub async fn is_empty(&self) -> bool {
        self.len().await == 0
    }

    /// Whether this stream has ever evicted data due to overflow.
    ///
    /// `write` silently drops the oldest bytes once the ring fills — this is
    /// the hot-path check capture sites use to detect that loss so they can
    /// surface it instead of reporting clean success (GH #191). Equivalent to
    /// `stats().await.bytes_evicted > 0`, but avoids building the full
    /// `StreamStats` when the caller only needs the boolean.
    pub async fn has_overflowed(&self) -> bool {
        let inner = self.inner.read().await;
        inner.bytes_evicted > 0
    }

    /// Wait until this stream has written more than `seen_total_written`
    /// lifetime bytes, or has closed. Returns the stats that ended the wait,
    /// so the caller's next call passes back `stats.total_written`.
    ///
    /// This is the alternative to a poll loop for an embedder tailing a
    /// running job's output. Pass `0` on the first call to wake on the first
    /// byte. **A closed stream returns immediately, every time** — the caller
    /// checks `stats.closed` and stops, rather than looping on a stream that
    /// can never change again.
    ///
    /// The registration happens before the read, not after: `Notify` only
    /// reaches waiters that are already registered, so reading first would
    /// drop a write that landed in between and park until the *next* one.
    pub async fn changed_since(&self, seen_total_written: u64) -> StreamStats {
        loop {
            let notified = self.notify.notified();
            tokio::pin!(notified);
            notified.as_mut().enable();

            let stats = self.stats().await;
            if stats.closed || stats.total_written > seen_total_written {
                return stats;
            }

            notified.await;
        }
    }

    /// Get stream statistics.
    pub async fn stats(&self) -> StreamStats {
        let inner = self.inner.read().await;
        StreamStats {
            current_size: inner.buffer.len(),
            max_size: inner.max_size,
            total_written: inner.total_written,
            bytes_evicted: inner.bytes_evicted,
            closed: inner.closed,
        }
    }
}

impl std::fmt::Debug for BoundedStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BoundedStream")
            .field("inner", &"<locked>")
            .finish()
    }
}

/// Statistics about a bounded stream.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "schema", derive(schemars::JsonSchema))]
pub struct StreamStats {
    /// Current bytes in buffer.
    pub current_size: usize,
    /// Maximum buffer size.
    pub max_size: usize,
    /// Total bytes written (lifetime).
    pub total_written: u64,
    /// Bytes evicted due to overflow.
    pub bytes_evicted: u64,
    /// Whether the stream is closed.
    pub closed: bool,
}

impl StreamStats {
    /// Build a loud marker describing this stream's overflow. Call only when
    /// `bytes_evicted > 0` — the caller is expected to gate on
    /// [`BoundedStream::has_overflowed`] first.
    ///
    /// `label` names the stream ("stdout"/"stderr") in the marker text.
    /// Centralized here — not hand-written at each capture site — so the two
    /// external-command spawn sites that must stay in sync
    /// (`kernel.rs::try_execute_external` and the test-only twin
    /// `dispatch.rs::BackendDispatcher::try_external`, see CLAUDE.md's "two
    /// spawn sites" gotcha) can't drift in wording (GH #191).
    pub fn overflow_marker(&self, label: &str) -> String {
        let max_mb = self.max_size as f64 / (1024.0 * 1024.0);
        format!(
            "[{label} truncated: output exceeded the {max_mb:.0}MB capture buffer \
             — first {} bytes lost ({} bytes total written); enable output-limit \
             to spill to disk]\n",
            self.bytes_evicted, self.total_written,
        )
    }
}

/// Drain an async reader into a bounded stream.
///
/// This is useful for capturing process output without blocking the pipe.
/// The function reads until EOF, then closes the stream.
pub async fn drain_to_stream<R>(reader: R, stream: Arc<BoundedStream>)
where
    R: tokio::io::AsyncRead + Unpin,
{
    drain_to_stream_teed(reader, stream, None).await
}

/// Drain an async reader into `stream`, copying every chunk into `tee` as well.
///
/// The tee is what makes `/v/jobs/{id}/stdout` live: a background job's stream
/// outlives the single command being drained here, so it receives each 8 KiB
/// chunk as the child emits it and is **not** closed at EOF — only `stream`,
/// which belongs to this one command, is. Closing the job's stream is the job's
/// own business, once every command in it has finished.
pub async fn drain_to_stream_teed<R>(
    mut reader: R,
    stream: Arc<BoundedStream>,
    tee: Option<Arc<BoundedStream>>,
) where
    R: tokio::io::AsyncRead + Unpin,
{
    use tokio::io::AsyncReadExt;

    let mut buf = [0u8; 8192];
    loop {
        match reader.read(&mut buf).await {
            Ok(0) => break, // EOF
            Ok(n) => {
                stream.write(&buf[..n]).await;
                if let Some(tee) = &tee {
                    tee.write(&buf[..n]).await;
                }
            }
            Err(e) => {
                tracing::warn!("drain_to_stream read error: {}", e);
                break;
            }
        }
    }
    stream.close().await;
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The wake-up must reach a waiter that registered before the write —
    /// the whole point of enabling the `Notified` future ahead of the read.
    #[tokio::test]
    async fn changed_since_wakes_on_a_later_write() {
        let stream = Arc::new(BoundedStream::new(1024));
        let writer = stream.clone();
        tokio::spawn(async move {
            tokio::time::sleep(std::time::Duration::from_millis(50)).await;
            writer.write(b"late").await;
        });

        let stats = tokio::time::timeout(
            std::time::Duration::from_secs(5),
            stream.changed_since(0),
        )
        .await
        .expect("changed_since parked instead of waking on the write");
        assert_eq!(stats.total_written, 4);
        assert!(!stats.closed);
    }

    /// Data already present must return immediately — a caller that polls
    /// once, then waits, must not miss what landed in between.
    #[tokio::test]
    async fn changed_since_returns_at_once_when_data_already_arrived() {
        let stream = BoundedStream::new(1024);
        stream.write(b"early").await;
        let stats = tokio::time::timeout(
            std::time::Duration::from_millis(500),
            stream.changed_since(0),
        )
        .await
        .expect("already-written data must not block");
        assert_eq!(stats.total_written, 5);
    }

    /// A closed stream can never change again, so waiting on one returns
    /// rather than parking forever.
    #[tokio::test]
    async fn changed_since_returns_on_close() {
        let stream = Arc::new(BoundedStream::new(1024));
        let closer = stream.clone();
        tokio::spawn(async move {
            tokio::time::sleep(std::time::Duration::from_millis(50)).await;
            closer.close().await;
        });

        let stats = tokio::time::timeout(
            std::time::Duration::from_secs(5),
            stream.changed_since(0),
        )
        .await
        .expect("close must wake a waiter");
        assert!(stats.closed, "the caller stops on this flag, not on a timeout");
        assert_eq!(stats.total_written, 0);
    }

    /// The tee gets every chunk the primary does, and is NOT closed at EOF —
    /// a job's stream outlives the one command being drained into it.
    #[tokio::test]
    async fn drain_to_stream_teed_copies_to_both_and_closes_only_the_primary() {
        let primary = Arc::new(BoundedStream::new(1024));
        let tee = Arc::new(BoundedStream::new(1024));

        let reader = std::io::Cursor::new(b"hello tee".to_vec());
        drain_to_stream_teed(reader, primary.clone(), Some(tee.clone())).await;

        assert_eq!(primary.read().await, b"hello tee");
        assert_eq!(tee.read().await, b"hello tee");
        assert!(primary.is_closed().await, "the drained command's own stream is done");
        assert!(
            !tee.is_closed().await,
            "the job's stream must stay open for the next command in the job"
        );
    }

    #[tokio::test]
    async fn test_basic_write_read() {
        let stream = BoundedStream::new(100);
        stream.write(b"hello").await;
        assert_eq!(stream.read().await, b"hello");
    }

    #[tokio::test]
    async fn test_multiple_writes() {
        let stream = BoundedStream::new(100);
        stream.write(b"hello ").await;
        stream.write(b"world").await;
        assert_eq!(stream.read().await, b"hello world");
    }

    #[tokio::test]
    async fn test_eviction_on_overflow() {
        let stream = BoundedStream::new(10);
        stream.write(b"12345").await;
        stream.write(b"67890").await;
        assert_eq!(stream.len().await, 10);

        // Write 5 more bytes - should evict first 5
        stream.write(b"ABCDE").await;
        assert_eq!(stream.read().await, b"67890ABCDE");

        let stats = stream.stats().await;
        assert_eq!(stats.bytes_evicted, 5);
        assert_eq!(stats.total_written, 15);
    }

    #[tokio::test]
    async fn test_large_write_exceeds_buffer() {
        let stream = BoundedStream::new(10);
        // Write more than max_size - should only keep last 10 bytes
        stream.write(b"0123456789ABCDEFGHIJ").await;
        assert_eq!(stream.read().await, b"ABCDEFGHIJ");
    }

    #[tokio::test]
    async fn test_close_prevents_writes() {
        let stream = BoundedStream::new(100);
        stream.write(b"before").await;
        stream.close().await;
        stream.write(b"after").await;
        assert_eq!(stream.read().await, b"before");
    }

    #[tokio::test]
    async fn test_read_string() {
        let stream = BoundedStream::new(100);
        stream.write(b"hello world").await;
        assert_eq!(stream.read_string().await, "hello world");
    }

    #[tokio::test]
    async fn test_concurrent_writes() {
        use std::sync::Arc;

        let stream = Arc::new(BoundedStream::new(1000));

        let handles: Vec<_> = (0..10)
            .map(|i| {
                let s = stream.clone();
                tokio::spawn(async move {
                    for j in 0..10 {
                        s.write(format!("[{}-{}]", i, j).as_bytes()).await;
                    }
                })
            })
            .collect();

        for h in handles {
            h.await.expect("task should not panic");
        }

        // All writes should complete without panic
        // Order is non-deterministic, but total length should be consistent
        let data = stream.read().await;
        assert!(!data.is_empty());
    }

    #[tokio::test]
    async fn test_stats() {
        let stream = BoundedStream::new(10);
        stream.write(b"1234567890").await;

        let stats = stream.stats().await;
        assert_eq!(stats.current_size, 10);
        assert_eq!(stats.max_size, 10);
        assert_eq!(stats.total_written, 10);
        assert_eq!(stats.bytes_evicted, 0);
        assert!(!stats.closed);
    }

    #[tokio::test]
    async fn test_empty_stream() {
        let stream = BoundedStream::new(100);
        assert!(stream.is_empty().await);
        assert_eq!(stream.len().await, 0);
        assert_eq!(stream.read().await, Vec::<u8>::new());
    }

    #[tokio::test]
    async fn test_drain_to_stream() {
        use std::io::Cursor;

        let data = b"test data from reader";
        let cursor = Cursor::new(data.to_vec());
        let stream = Arc::new(BoundedStream::new(100));

        drain_to_stream(cursor, stream.clone()).await;

        assert_eq!(stream.read().await, data);
        assert!(stream.is_closed().await);
    }

    #[tokio::test]
    async fn test_default_size() {
        let stream = BoundedStream::default_size();
        let stats = stream.stats().await;
        assert_eq!(stats.max_size, DEFAULT_STREAM_MAX_SIZE);
    }

    #[tokio::test]
    async fn test_has_overflowed() {
        let stream = BoundedStream::new(10);
        assert!(!stream.has_overflowed().await, "empty stream has not overflowed");

        stream.write(b"1234567890").await;
        assert!(
            !stream.has_overflowed().await,
            "exactly filling the buffer is not an overflow"
        );

        stream.write(b"more").await; // forces eviction of the oldest 4 bytes
        assert!(
            stream.has_overflowed().await,
            "writing past capacity must flip has_overflowed"
        );
    }

    #[test]
    fn stream_stats_round_trips_through_serde() {
        // GH #241 (folded in): StreamStats was flagged alongside the job types
        // as another kaish type family with no serde — bytes-evicted/
        // truncation warnings are useful for an embedder surfacing job output
        // health, same spirit as the rest of this PR.
        let stats = StreamStats {
            current_size: 42,
            max_size: 100,
            total_written: 142,
            bytes_evicted: 100,
            closed: true,
        };
        let json = serde_json::to_string(&stats).unwrap();
        let back: StreamStats = serde_json::from_str(&json).unwrap();
        assert_eq!(back.current_size, stats.current_size);
        assert_eq!(back.max_size, stats.max_size);
        assert_eq!(back.total_written, stats.total_written);
        assert_eq!(back.bytes_evicted, stats.bytes_evicted);
        assert_eq!(back.closed, stats.closed);
    }

    #[test]
    fn test_overflow_marker_wording() {
        let stats = StreamStats {
            current_size: 10 * 1024 * 1024,
            max_size: 10 * 1024 * 1024,
            total_written: 15 * 1024 * 1024,
            bytes_evicted: 5 * 1024 * 1024,
            closed: true,
        };
        let marker = stats.overflow_marker("stdout");
        assert!(marker.starts_with("[stdout truncated:"), "got: {marker}");
        assert!(marker.contains("10MB"), "got: {marker}");
        assert!(marker.contains(&(5 * 1024 * 1024).to_string()), "got: {marker}");
        assert!(marker.contains(&(15 * 1024 * 1024).to_string()), "got: {marker}");
        assert!(marker.contains("output-limit"), "got: {marker}");
    }
}
