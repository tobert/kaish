//! Bounded byte pipe with backpressure for streaming pipelines.
//!
//! Unlike `BoundedStream` (which evicts oldest data on overflow for job capture),
//! `PipeStream` blocks the writer when the buffer is full — matching OS pipe
//! semantics with ~64KB kernel buffers.
//!
//! ```text
//!   PipeWriter ──▶ [VecDeque<u8> ring buffer] ──▶ PipeReader
//!                  ├── writer blocks when full (backpressure)
//!                  ├── reader blocks when empty
//!                  ├── drop writer → EOF (reader returns 0)
//!                  └── drop reader → broken pipe (writer returns error)
//! ```
//!
//! Implementation uses `std::sync::Mutex` (not tokio) since critical sections
//! are just VecDeque operations (microseconds). Closed flags are `AtomicBool`
//! so Drop is always synchronous — no `tokio::spawn` in destructors.
//! Wakers are stored under the lock to prevent lost wakeups.

use std::collections::VecDeque;
use std::io;
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::task::{Context, Poll, Waker};

use tokio::io::{AsyncRead, AsyncWrite, ReadBuf};

/// Default pipe buffer capacity (matches Linux kernel pipe default).
pub const PIPE_BUFFER_SIZE: usize = 64 * 1024;

/// Shared mutable state protected by std::sync::Mutex.
///
/// Lock is held only for VecDeque operations and waker registration —
/// always non-blocking and fast.
struct PipeBuffer {
    buffer: VecDeque<u8>,
    capacity: usize,
    /// Waker for the reader task (set when reader finds empty buffer).
    reader_waker: Option<Waker>,
    /// Waker for the writer task (set when writer finds full buffer).
    writer_waker: Option<Waker>,
}

/// Shared state between PipeWriter and PipeReader.
struct PipeShared {
    buf: Mutex<PipeBuffer>,
    /// Writer has been dropped (EOF). Atomic so Drop is lock-free.
    writer_closed: AtomicBool,
    /// Reader has been dropped (broken pipe). Atomic so Drop is lock-free.
    reader_closed: AtomicBool,
}

/// Writing end of a pipe stream.
pub struct PipeWriter {
    shared: Arc<PipeShared>,
}

/// Reading end of a pipe stream.
pub struct PipeReader {
    shared: Arc<PipeShared>,
}

/// Create a bounded pipe stream pair with the given capacity.
///
/// The writer blocks when the buffer is full (backpressure).
/// The reader blocks when the buffer is empty.
/// Dropping the writer signals EOF; dropping the reader signals broken pipe.
pub fn pipe_stream(capacity: usize) -> (PipeWriter, PipeReader) {
    let shared = Arc::new(PipeShared {
        buf: Mutex::new(PipeBuffer {
            buffer: VecDeque::with_capacity(capacity.min(8192)),
            capacity,
            reader_waker: None,
            writer_waker: None,
        }),
        writer_closed: AtomicBool::new(false),
        reader_closed: AtomicBool::new(false),
    });

    (
        PipeWriter { shared: shared.clone() },
        PipeReader { shared },
    )
}

/// Create a pipe stream pair with the default capacity (64KB).
pub fn pipe_stream_default() -> (PipeWriter, PipeReader) {
    pipe_stream(PIPE_BUFFER_SIZE)
}

impl PipeWriter {
    /// Write data to the pipe, blocking if the buffer is full.
    ///
    /// Returns the number of bytes written, or an error if the reader has been dropped.
    pub async fn write_bytes(&self, data: &[u8]) -> io::Result<usize> {
        use std::future::poll_fn;

        if data.is_empty() {
            return Ok(0);
        }

        poll_fn(|cx| Pin::new(&mut &*self).poll_write_impl(cx, data)).await
    }
}

impl PipeWriter {
    /// Shared poll implementation used by both AsyncWrite and write_bytes.
    fn poll_write_impl(
        &self,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<io::Result<usize>> {
        if buf.is_empty() {
            return Poll::Ready(Ok(0));
        }

        if self.shared.reader_closed.load(Ordering::Acquire) {
            return Poll::Ready(Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "pipe reader closed",
            )));
        }

        let mut inner = self.shared.buf.lock().unwrap_or_else(|e| e.into_inner());

        // Re-check reader_closed under lock (writer may have raced with reader drop)
        if self.shared.reader_closed.load(Ordering::Acquire) {
            return Poll::Ready(Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "pipe reader closed",
            )));
        }

        let available = inner.capacity.saturating_sub(inner.buffer.len());
        if available > 0 {
            let to_write = buf.len().min(available);
            inner.buffer.extend(&buf[..to_write]);
            // Wake reader if it was waiting for data
            if let Some(waker) = inner.reader_waker.take() {
                waker.wake();
            }
            Poll::Ready(Ok(to_write))
        } else {
            // Buffer full — register waker so reader can wake us
            inner.writer_waker = Some(cx.waker().clone());
            Poll::Pending
        }
    }
}

impl AsyncWrite for PipeWriter {
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<io::Result<usize>> {
        self.poll_write_impl(cx, buf)
    }

    fn poll_flush(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        Poll::Ready(Ok(()))
    }

    fn poll_shutdown(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        self.shared.writer_closed.store(true, Ordering::Release);
        let mut inner = self.shared.buf.lock().unwrap_or_else(|e| e.into_inner());
        if let Some(waker) = inner.reader_waker.take() {
            waker.wake();
        }
        Poll::Ready(Ok(()))
    }
}

impl Drop for PipeWriter {
    fn drop(&mut self) {
        self.shared.writer_closed.store(true, Ordering::Release);
        // Wake reader so it sees EOF. Lock is std::sync::Mutex — always available
        // synchronously (no tokio::spawn needed).
        if let Ok(mut inner) = self.shared.buf.lock() {
            if let Some(waker) = inner.reader_waker.take() {
                waker.wake();
            }
        }
        // If lock is poisoned, reader will see writer_closed on next poll.
    }
}

impl AsyncRead for PipeReader {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        let mut inner = self.shared.buf.lock().unwrap_or_else(|e| e.into_inner());

        if !inner.buffer.is_empty() {
            let to_read = buf.remaining().min(inner.buffer.len());
            let (front, back) = inner.buffer.as_slices();

            if to_read <= front.len() {
                buf.put_slice(&front[..to_read]);
            } else {
                buf.put_slice(front);
                let remaining = to_read - front.len();
                buf.put_slice(&back[..remaining]);
            }

            inner.buffer.drain(..to_read);
            // Wake writer if it was waiting for space
            if let Some(waker) = inner.writer_waker.take() {
                waker.wake();
            }
            Poll::Ready(Ok(()))
        } else if self.shared.writer_closed.load(Ordering::Acquire) {
            // EOF — writer is gone and buffer is drained
            Poll::Ready(Ok(()))
        } else {
            // No data, writer still alive — register waker and wait
            inner.reader_waker = Some(cx.waker().clone());
            Poll::Pending
        }
    }
}

impl Drop for PipeReader {
    fn drop(&mut self) {
        self.shared.reader_closed.store(true, Ordering::Release);
        if let Ok(mut inner) = self.shared.buf.lock() {
            if let Some(waker) = inner.writer_waker.take() {
                waker.wake();
            }
        }
    }
}

impl std::fmt::Debug for PipeWriter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PipeWriter").finish()
    }
}

impl std::fmt::Debug for PipeReader {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PipeReader").finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::io::{AsyncReadExt, AsyncWriteExt};

    #[tokio::test]
    async fn test_basic_write_read() {
        let (writer, mut reader) = pipe_stream(1024);

        writer.write_bytes(b"hello").await.unwrap();
        drop(writer); // Signal EOF

        let mut buf = Vec::new();
        reader.read_to_end(&mut buf).await.unwrap();
        assert_eq!(buf, b"hello");
    }

    #[tokio::test]
    async fn test_concurrent_write_read() {
        let (writer, mut reader) = pipe_stream(64);

        let write_task = tokio::spawn(async move {
            for i in 0..100 {
                let msg = format!("line {}\n", i);
                writer.write_bytes(msg.as_bytes()).await.unwrap();
            }
            // Writer dropped here → EOF
        });

        let mut output = Vec::new();
        reader.read_to_end(&mut output).await.unwrap();

        write_task.await.unwrap();

        let text = String::from_utf8(output).unwrap();
        assert!(text.contains("line 0"));
        assert!(text.contains("line 99"));
    }

    #[tokio::test]
    async fn test_backpressure() {
        let (writer, mut reader) = pipe_stream(16);

        // Write more than buffer capacity — should not lose data
        let write_task = tokio::spawn(async move {
            let data = b"0123456789ABCDEF_EXTRA_DATA";
            let mut written = 0;
            while written < data.len() {
                let n = writer.write_bytes(&data[written..]).await.unwrap();
                written += n;
            }
        });

        let mut output = Vec::new();
        reader.read_to_end(&mut output).await.unwrap();

        write_task.await.unwrap();
        assert_eq!(output, b"0123456789ABCDEF_EXTRA_DATA");
    }

    #[tokio::test]
    async fn test_eof_on_writer_drop() {
        let (writer, mut reader) = pipe_stream(1024);

        writer.write_bytes(b"data").await.unwrap();
        drop(writer);

        let mut buf = [0u8; 1024];
        let n = reader.read(&mut buf).await.unwrap();
        assert_eq!(&buf[..n], b"data");

        // Next read should return 0 (EOF)
        let n = reader.read(&mut buf).await.unwrap();
        assert_eq!(n, 0);
    }

    #[tokio::test]
    async fn test_broken_pipe_on_reader_drop() {
        let (writer, reader) = pipe_stream(1024);
        drop(reader);

        let result = writer.write_bytes(b"data").await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::BrokenPipe);
    }

    #[tokio::test]
    async fn test_empty_pipe() {
        let (writer, mut reader) = pipe_stream(1024);
        drop(writer);

        let mut buf = Vec::new();
        reader.read_to_end(&mut buf).await.unwrap();
        assert!(buf.is_empty());
    }

    #[tokio::test]
    async fn test_async_write_trait() {
        let (mut writer, mut reader) = pipe_stream(1024);

        writer.write_all(b"async write").await.unwrap();
        writer.shutdown().await.unwrap();

        let mut buf = Vec::new();
        reader.read_to_end(&mut buf).await.unwrap();
        assert_eq!(buf, b"async write");
    }

    #[tokio::test]
    async fn test_large_data_through_small_buffer() {
        let (writer, mut reader) = pipe_stream(32);

        let data: Vec<u8> = (0..10_000).map(|i| (i % 256) as u8).collect();
        let expected = data.clone();

        let write_task = tokio::spawn(async move {
            let mut pos = 0;
            while pos < data.len() {
                let n = writer.write_bytes(&data[pos..]).await.unwrap();
                pos += n;
            }
        });

        let mut output = Vec::new();
        reader.read_to_end(&mut output).await.unwrap();

        write_task.await.unwrap();
        assert_eq!(output, expected);
    }

    /// Stress test: many concurrent writers/readers shouldn't hang or lose data.
    /// This would hang with the lost-wakeup bug (writer misses notify after
    /// releasing the lock but before calling notified().await).
    #[tokio::test]
    async fn test_no_lost_wakeups_under_contention() {
        // Small buffer + many iterations maximizes lock contention
        let (writer, mut reader) = pipe_stream(16);

        let write_task = tokio::spawn(async move {
            for i in 0u32..1000 {
                let bytes = i.to_le_bytes();
                let mut pos = 0;
                while pos < bytes.len() {
                    let n = writer.write_bytes(&bytes[pos..]).await.unwrap();
                    pos += n;
                }
            }
            // Writer dropped → EOF
        });

        let mut output = Vec::new();
        reader.read_to_end(&mut output).await.unwrap();
        write_task.await.unwrap();

        // Should have received exactly 1000 * 4 bytes
        assert_eq!(output.len(), 4000);
    }

    /// Stress test with timeout: detects hangs from lost wakeups or deadlocks.
    #[tokio::test]
    async fn test_concurrent_stress_no_hang() {
        let result = tokio::time::timeout(std::time::Duration::from_secs(5), async {
            let (writer, mut reader) = pipe_stream(64);

            let write_task = tokio::spawn(async move {
                let chunk = vec![0xABu8; 37]; // odd size to stress partial writes
                for _ in 0..5000 {
                    let mut pos = 0;
                    while pos < chunk.len() {
                        match writer.write_bytes(&chunk[pos..]).await {
                            Ok(n) => pos += n,
                            Err(_) => return, // broken pipe
                        }
                    }
                }
            });

            let mut total = 0usize;
            let mut buf = [0u8; 128];
            loop {
                match reader.read(&mut buf).await {
                    Ok(0) => break,
                    Ok(n) => total += n,
                    Err(_) => break,
                }
            }

            write_task.await.unwrap();
            assert_eq!(total, 37 * 5000);
        }).await;

        assert!(result.is_ok(), "pipe stress test timed out — likely deadlock");
    }

    /// Drop writer while reader is actively reading — must not panic or hang.
    #[tokio::test]
    async fn test_writer_drop_during_active_read() {
        let (writer, mut reader) = pipe_stream(1024);

        // Spawn reader that's actively waiting for data
        let read_task = tokio::spawn(async move {
            let mut buf = Vec::new();
            reader.read_to_end(&mut buf).await.unwrap();
            buf
        });

        // Small delay then drop writer without writing
        tokio::time::sleep(std::time::Duration::from_millis(10)).await;
        drop(writer);

        let result = tokio::time::timeout(
            std::time::Duration::from_secs(2),
            read_task,
        ).await;
        assert!(result.is_ok(), "reader hung after writer dropped");
        assert!(result.unwrap().unwrap().is_empty());
    }

    /// Drop reader while writer is blocked on full buffer — must not hang.
    #[tokio::test]
    async fn test_reader_drop_while_writer_blocked() {
        let (writer, reader) = pipe_stream(8);

        let write_task = tokio::spawn(async move {
            let data = vec![0u8; 1024]; // much larger than buffer
            let mut pos = 0;
            while pos < data.len() {
                match writer.write_bytes(&data[pos..]).await {
                    Ok(n) => pos += n,
                    Err(e) => {
                        assert_eq!(e.kind(), io::ErrorKind::BrokenPipe);
                        return;
                    }
                }
            }
            panic!("writer should have gotten broken pipe");
        });

        // Let writer fill the buffer and block, then drop reader
        tokio::time::sleep(std::time::Duration::from_millis(10)).await;
        drop(reader);

        let result = tokio::time::timeout(
            std::time::Duration::from_secs(2),
            write_task,
        ).await;
        assert!(result.is_ok(), "writer hung after reader dropped");
    }
}
