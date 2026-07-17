//! Bounded streams for job output capture.
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
            return;
        }

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
        let mut inner = self.inner.write().await;
        inner.closed = true;
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
#[derive(Debug, Clone)]
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
pub async fn drain_to_stream<R>(mut reader: R, stream: Arc<BoundedStream>)
where
    R: tokio::io::AsyncRead + Unpin,
{
    use tokio::io::AsyncReadExt;

    let mut buf = [0u8; 8192];
    loop {
        match reader.read(&mut buf).await {
            Ok(0) => break, // EOF
            Ok(n) => stream.write(&buf[..n]).await,
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
