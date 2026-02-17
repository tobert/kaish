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
    sender: mpsc::UnboundedSender<Vec<u8>>,
}

/// Receiving end of the stderr stream.
///
/// Owned by the kernel. Call `drain_lossy()` to collect all pending bytes
/// as a UTF-8 string (with lossy decode at this boundary).
pub struct StderrReceiver {
    receiver: mpsc::UnboundedReceiver<Vec<u8>>,
}

/// Create a new stderr stream pair.
pub fn stderr_stream() -> (StderrStream, StderrReceiver) {
    let (sender, receiver) = mpsc::unbounded_channel();
    (
        StderrStream { sender },
        StderrReceiver { receiver },
    )
}

impl StderrStream {
    /// Write raw bytes to the stderr stream.
    ///
    /// Non-blocking. If the receiver has been dropped, the data is silently
    /// discarded (same as writing to a closed pipe).
    pub fn write(&self, data: &[u8]) {
        if !data.is_empty() {
            // Ignore send errors — receiver dropped means nobody is listening
            let _ = self.sender.send(data.to_vec());
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
    pub fn drain_lossy(&mut self) -> String {
        let mut buf = Vec::new();
        while let Ok(chunk) = self.receiver.try_recv() {
            buf.extend_from_slice(&chunk);
        }
        if buf.is_empty() {
            String::new()
        } else {
            String::from_utf8_lossy(&buf).into_owned()
        }
    }
}
