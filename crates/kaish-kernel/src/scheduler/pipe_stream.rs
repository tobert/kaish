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

use std::collections::VecDeque;
use std::io;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll};

use tokio::io::{AsyncRead, AsyncWrite, ReadBuf};
use tokio::sync::{Mutex, Notify};

/// Default pipe buffer capacity (matches Linux kernel pipe default).
pub const PIPE_BUFFER_SIZE: usize = 64 * 1024;

/// Shared state between PipeWriter and PipeReader.
struct PipeInner {
    buffer: VecDeque<u8>,
    capacity: usize,
    /// Writer has been dropped (EOF).
    writer_closed: bool,
    /// Reader has been dropped (broken pipe).
    reader_closed: bool,
}

/// Shared handle wrapping the inner state with async synchronization.
struct PipeShared {
    inner: Mutex<PipeInner>,
    /// Notified when data is available to read or space is available to write.
    notify: Notify,
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
        inner: Mutex::new(PipeInner {
            buffer: VecDeque::with_capacity(capacity.min(8192)),
            capacity,
            writer_closed: false,
            reader_closed: false,
        }),
        notify: Notify::new(),
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
        if data.is_empty() {
            return Ok(0);
        }

        loop {
            {
                let mut inner = self.shared.inner.lock().await;

                if inner.reader_closed {
                    return Err(io::Error::new(io::ErrorKind::BrokenPipe, "pipe reader closed"));
                }

                let available = inner.capacity.saturating_sub(inner.buffer.len());
                if available > 0 {
                    let to_write = data.len().min(available);
                    inner.buffer.extend(&data[..to_write]);
                    // Notify reader that data is available
                    self.shared.notify.notify_waiters();
                    return Ok(to_write);
                }
            }

            // Buffer is full — wait for reader to consume
            self.shared.notify.notified().await;
        }
    }
}

impl AsyncWrite for PipeWriter {
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<io::Result<usize>> {
        if buf.is_empty() {
            return Poll::Ready(Ok(0));
        }

        let shared = self.shared.clone();

        // Try to acquire lock and write
        let mut inner = match shared.inner.try_lock() {
            Ok(guard) => guard,
            Err(_) => {
                // Lock contended — register waker and return pending
                cx.waker().wake_by_ref();
                return Poll::Pending;
            }
        };

        if inner.reader_closed {
            return Poll::Ready(Err(io::Error::new(
                io::ErrorKind::BrokenPipe,
                "pipe reader closed",
            )));
        }

        let available = inner.capacity.saturating_sub(inner.buffer.len());
        if available > 0 {
            let to_write = buf.len().min(available);
            inner.buffer.extend(&buf[..to_write]);
            shared.notify.notify_waiters();
            Poll::Ready(Ok(to_write))
        } else {
            // Buffer full — register waker via notify
            drop(inner);
            // We need to re-poll when space becomes available
            cx.waker().wake_by_ref();
            Poll::Pending
        }
    }

    fn poll_flush(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        Poll::Ready(Ok(()))
    }

    fn poll_shutdown(self: Pin<&mut Self>, _cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        // Signal EOF by marking writer as closed
        let shared = self.shared.clone();
        match shared.inner.try_lock() {
            Ok(mut inner) => {
                inner.writer_closed = true;
                shared.notify.notify_waiters();
                Poll::Ready(Ok(()))
            }
            Err(_) => {
                _cx.waker().wake_by_ref();
                Poll::Pending
            }
        }
    }
}

impl Drop for PipeWriter {
    fn drop(&mut self) {
        // Signal EOF to the reader
        if let Ok(mut inner) = self.shared.inner.try_lock() {
            inner.writer_closed = true;
            self.shared.notify.notify_waiters();
        } else {
            // Can't acquire lock in drop — spawn a task to signal
            let shared = self.shared.clone();
            tokio::spawn(async move {
                let mut inner = shared.inner.lock().await;
                inner.writer_closed = true;
                shared.notify.notify_waiters();
            });
        }
    }
}

impl AsyncRead for PipeReader {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        let shared = self.shared.clone();

        let mut inner = match shared.inner.try_lock() {
            Ok(guard) => guard,
            Err(_) => {
                cx.waker().wake_by_ref();
                return Poll::Pending;
            }
        };

        if !inner.buffer.is_empty() {
            // Read available data
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
            // Notify writer that space is available
            shared.notify.notify_waiters();
            Poll::Ready(Ok(()))
        } else if inner.writer_closed {
            // EOF
            Poll::Ready(Ok(()))
        } else {
            // No data available, writer still open — wait
            drop(inner);
            cx.waker().wake_by_ref();
            Poll::Pending
        }
    }
}

impl Drop for PipeReader {
    fn drop(&mut self) {
        if let Ok(mut inner) = self.shared.inner.try_lock() {
            inner.reader_closed = true;
            self.shared.notify.notify_waiters();
        } else {
            let shared = self.shared.clone();
            tokio::spawn(async move {
                let mut inner = shared.inner.lock().await;
                inner.reader_closed = true;
                shared.notify.notify_waiters();
            });
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
}
