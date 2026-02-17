//! Kernel-level stderr stream for real-time error output from pipeline stages.
//!
//! In bash, stderr from ALL pipeline stages streams to the terminal simultaneously.
//! kaish achieves this with `StderrStream` — a cloneable, Send+Sync handle backed
//! by an unbounded mpsc channel. Pipeline stages and external commands write to it
//! concurrently; the kernel drains it to the actual stderr sink.
//!
//! ```text
//!   Stage 1 ──┐
//!   Stage 2 ──┼──▶ StderrStream (mpsc) ──▶ drain task ──▶ eprintln! / MCP / log
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
    sender: mpsc::UnboundedSender<String>,
}

/// Receiving end of the stderr stream.
///
/// Owned by the kernel. Call `drain()` to collect all pending messages,
/// or `recv()` for incremental consumption.
pub struct StderrReceiver {
    receiver: mpsc::UnboundedReceiver<String>,
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
    /// Write a message to the stderr stream.
    ///
    /// Non-blocking. If the receiver has been dropped, the message is silently
    /// discarded (same as writing to a closed pipe).
    pub fn write(&self, msg: &str) {
        if !msg.is_empty() {
            // Ignore send errors — receiver dropped means nobody is listening
            let _ = self.sender.send(msg.to_string());
        }
    }
}

impl StderrReceiver {
    /// Drain all pending messages into a single string.
    ///
    /// Returns an empty string if no messages are pending.
    /// Non-blocking — returns immediately with whatever is available.
    pub fn drain(&mut self) -> String {
        let mut buf = String::new();
        while let Ok(msg) = self.receiver.try_recv() {
            buf.push_str(&msg);
        }
        buf
    }

    /// Drain all pending messages, calling `f` for each one.
    ///
    /// Useful for writing to process stderr without intermediate allocation.
    pub fn drain_each(&mut self, mut f: impl FnMut(&str)) {
        while let Ok(msg) = self.receiver.try_recv() {
            f(&msg);
        }
    }
}
