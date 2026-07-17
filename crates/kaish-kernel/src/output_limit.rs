//! Configurable output size limits for agent safety.
//!
//! When output exceeds the threshold the result is capped and `ExecResult.out`
//! is replaced with a head+tail preview. Two strategies, selected at runtime by
//! [`SpillMode`]:
//! - [`SpillMode::Disk`] (default): the full output is written to a spill file
//!   on the real filesystem and the preview points at it. The agent can then
//!   selectively read the file.
//! - [`SpillMode::Memory`]: the output is truncated in memory only — no disk
//!   I/O, no recoverable file. For runtime read-only kernels (e.g. kaibo) that
//!   must not touch the host filesystem even when `localfs` is compiled in.
//!   Memory stays bounded regardless of how much the command produces.
//!
//! Either way the exit code is remapped to 3 (`did_spill`) so callers can tell
//! the output was capped.
//!
//! Per-mode defaults: sandboxed-agent kernels get an 8KB limit, REPL/test
//! kernels are unlimited. Runtime-switchable via the `kaish-output-limit` builtin.

use std::path::PathBuf;

use crate::interpreter::ExecResult;
#[cfg(feature = "localfs")]
use crate::paths;

/// Default output limit for the sandboxed-agent preset (8KB).
const DEFAULT_AGENT_LIMIT: usize = 8 * 1024;

/// Default head preview size (bytes of output start to keep).
const DEFAULT_HEAD_BYTES: usize = 1024;

/// Default tail preview size (bytes of output end to keep).
const DEFAULT_TAIL_BYTES: usize = 512;

/// Where overflow output goes when it exceeds the limit.
///
/// This is a *runtime* choice, distinct from the compile-time `localfs`
/// feature: a `localfs`-built kernel can still be told to truncate in memory.
/// A build without `localfs` always behaves as [`SpillMode::Memory`] regardless
/// of this setting, since disk I/O is unavailable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SpillMode {
    /// Write overflow to a disk spill file under `paths::spill_dir()` and keep a
    /// head+tail preview in the result (the message carries the file path).
    /// Requires the `localfs` feature. This is the default.
    ///
    /// Auto-overridden to [`Memory`](Self::Memory) at kernel construction when
    /// the VFS mount is `NoLocal` (memory-only) — such a kernel has no host
    /// filesystem to spill to. See `Kernel::assemble`.
    #[default]
    Disk,
    /// Truncate in memory to head+tail only — no disk I/O, no recoverable file.
    /// For runtime read-only kernels (e.g. kaibo) that must never touch the host
    /// filesystem even when `localfs` is compiled in.
    Memory,
}

/// Configurable output size limit.
///
/// Threaded through `KernelConfig` → `ExecContext` → kernel pipeline execution.
/// Runtime-mutable via the `kaish-output-limit` builtin.
#[derive(Debug, Clone)]
pub struct OutputLimitConfig {
    max_bytes: Option<usize>,
    head_bytes: usize,
    tail_bytes: usize,
    spill_mode: SpillMode,
}

impl OutputLimitConfig {
    /// No limiting — REPL/embedded/test default.
    pub fn none() -> Self {
        Self {
            max_bytes: None,
            head_bytes: DEFAULT_HEAD_BYTES,
            tail_bytes: DEFAULT_TAIL_BYTES,
            spill_mode: SpillMode::Disk,
        }
    }

    /// Default limit used by `on` subcommand and `set -o output-limit`.
    pub fn default_limit() -> usize {
        DEFAULT_AGENT_LIMIT
    }

    /// Sandboxed-agent defaults: 8KB limit, 1KB head, 512B tail, disk spill.
    pub fn agent() -> Self {
        Self {
            max_bytes: Some(DEFAULT_AGENT_LIMIT),
            head_bytes: DEFAULT_HEAD_BYTES,
            tail_bytes: DEFAULT_TAIL_BYTES,
            spill_mode: SpillMode::Disk,
        }
    }

    /// Switch to in-memory truncation — no disk spill, no host filesystem
    /// writes. For runtime read-only kernels (e.g. kaibo). Builder form of
    /// [`set_spill_mode`](Self::set_spill_mode).
    ///
    /// Note: a `NoLocal` VFS mount forces this mode automatically at kernel
    /// construction, so an embedder only needs this for a `localfs`-mounted
    /// kernel it nonetheless wants to keep off the host disk.
    pub fn in_memory(mut self) -> Self {
        self.spill_mode = SpillMode::Memory;
        self
    }

    /// Whether output limiting is enabled.
    pub fn is_enabled(&self) -> bool {
        self.max_bytes.is_some()
    }

    /// The spill mode (disk vs in-memory truncation).
    pub fn spill_mode(&self) -> SpillMode {
        self.spill_mode
    }

    /// Set the spill mode.
    pub fn set_spill_mode(&mut self, mode: SpillMode) {
        self.spill_mode = mode;
    }

    /// The maximum output size in bytes, if set.
    pub fn max_bytes(&self) -> Option<usize> {
        self.max_bytes
    }

    /// Bytes of output head to preserve in truncated result.
    pub fn head_bytes(&self) -> usize {
        self.head_bytes
    }

    /// Bytes of output tail to preserve in truncated result.
    pub fn tail_bytes(&self) -> usize {
        self.tail_bytes
    }

    /// Set the output limit. `None` disables limiting.
    pub fn set_limit(&mut self, max: Option<usize>) {
        self.max_bytes = max;
    }

    /// Set the head preview size.
    pub fn set_head_bytes(&mut self, bytes: usize) {
        self.head_bytes = bytes;
    }

    /// Set the tail preview size.
    pub fn set_tail_bytes(&mut self, bytes: usize) {
        self.tail_bytes = bytes;
    }
}

/// Result of a spill operation.
pub struct SpillResult {
    pub path: PathBuf,
    pub total_bytes: usize,
}

/// Check if the result output exceeds the limit and spill to disk if so.
///
/// Mutates `result.out` in place: replaces with head+tail+pointer message.
/// Returns `Some(SpillResult)` if a spill file was written, `None` otherwise.
///
/// If the filesystem write fails, the result is replaced with an error.
/// Fail fast: truncating output silently could corrupt structured data
/// that an agent acts on. An explicit error is safer.
///
/// In [`SpillMode::Memory`], or in any build without the `localfs` feature,
/// performs in-memory head+tail truncation (no disk I/O) instead.
pub async fn spill_if_needed(
    result: &mut ExecResult,
    config: &OutputLimitConfig,
) -> Option<SpillResult> {
    let max = config.max_bytes?;

    // Binary payloads are measured and spilled by RAW bytes. text_out() would
    // lossy-decode a Bytes result — corrupting the spill file and mis-measuring
    // the size (U+FFFD is 3 bytes per invalid byte). Handle them up front.
    if let Some(total) = result.out_bytes().map(|b| b.len()) {
        if total <= max {
            return None;
        }
        #[cfg(feature = "localfs")]
        if config.spill_mode == SpillMode::Disk {
            let bytes = result.out_bytes().unwrap_or_default().to_vec();
            return match write_spill_file(&bytes).await {
                Ok((path, written)) => {
                    result.set_out(format!(
                        "[binary output: {total} bytes spilled to {} — read it with `cat {}`]",
                        path.display(),
                        path.display()
                    ));
                    result.did_spill = true;
                    Some(SpillResult { path, total_bytes: written })
                }
                Err(e) => {
                    tracing::error!("binary output spill failed: {}", e);
                    *result = ExecResult::failure(
                        1,
                        format!(
                            "binary output exceeded {max} byte limit ({total} bytes) and spill \
                             to disk failed: {e}"
                        ),
                    );
                    None
                }
            };
        }
        // Memory mode (or no localfs): bounded head+tail of the raw bytes. The
        // result stays binary, just truncated.
        let bytes = result.out_bytes().unwrap_or_default().to_vec();
        let head_n = config.head_bytes.min(bytes.len());
        let tail_n = config.tail_bytes.min(bytes.len().saturating_sub(head_n));
        let mut truncated = bytes[..head_n].to_vec();
        truncated.extend_from_slice(&bytes[bytes.len() - tail_n..]);
        result.set_out_bytes(truncated);
        result.did_spill = true;
        return None;
    }

    // Disk spill requires `localfs` AND the caller selecting it. Memory mode
    // (or a build without `localfs`) falls through to in-memory truncation.
    #[cfg(feature = "localfs")]
    if config.spill_mode == SpillMode::Disk {
        // If result.out is already populated (external commands), check it directly
        if !result.text_out().is_empty() && !result.has_output() {
            let total = result.text_out().len();
            if total <= max {
                return None;
            }
            return spill_string(result, config, max).await;
        }

        // If we have structured OutputData, estimate size before materializing
        if let Some(output) = result.output() {
            let estimate = output.estimated_byte_size();
            if estimate <= max {
                // Small enough — materialize normally
                result.materialize();
                // Re-check actual size (estimate is a lower bound)
                if result.text_out().len() <= max {
                    return None;
                }
                return spill_string(result, config, max).await;
            }

            // Large — stream directly to spill file, never holding full String
            return spill_output_data(result, config, max).await;
        }

        return None;
    }

    // In-memory head+tail truncation (Memory mode or no `localfs`): no disk I/O.
    truncate_in_memory(result, config, max)
}

/// Truncate output in memory to head+tail, with no disk I/O.
///
/// Sets `did_spill = true` so the kernel remaps the exit code to 3 — the same
/// "output was capped" signal as a disk spill — but the message carries no file
/// path because there is no recoverable file. Returns `None` (no `SpillResult`,
/// since nothing was written); the caller distinguishes truncation via
/// `result.did_spill`.
///
/// Memory is bounded: large structured `OutputData` is streamed through a byte
/// budget rather than materialized into a full `String`, so a builtin emitting
/// a huge tree (e.g. a recursive `ls` of a giant directory) cannot OOM a
/// read-only kernel.
fn truncate_in_memory(
    result: &mut ExecResult,
    config: &OutputLimitConfig,
    max: usize,
) -> Option<SpillResult> {
    // Structured OutputData: estimate first. If it would clearly overflow,
    // render only a bounded head prefix via `write_canonical` rather than
    // materializing the whole thing.
    if let Some(output) = result.output() {
        let estimate = output.estimated_byte_size();
        if estimate > max {
            // Render a bounded head prefix only — no full materialization.
            let mut buf = Vec::with_capacity(config.head_bytes + 64);
            // write_canonical stops shortly after the budget; ignore the count.
            let _ = output.write_canonical(&mut buf, Some(config.head_bytes));
            let s = String::from_utf8_lossy(&buf);
            let head = truncate_to_char_boundary(&s, config.head_bytes);
            let truncated = format!(
                "{}\n...\n[output truncated in memory: ~{} bytes (exceeds {} byte limit) — head only, no spill file]",
                head, estimate, max
            );
            result.set_out(truncated);
            result.did_spill = true;
            return None;
        }
        // Small enough to materialize safely.
        result.materialize();
    }

    let total = result.text_out().len();
    if total <= max {
        return None;
    }

    // Already-materialized text fits in memory (it was produced into RAM
    // regardless) — give a precise head+tail+total.
    let text = result.text_out().into_owned();
    let head = truncate_to_char_boundary(&text, config.head_bytes);
    let tail = tail_from_str(&text, config.tail_bytes);
    let truncated = format!(
        "{}\n...\n{}\n[output truncated in memory: {} bytes total — no spill file]",
        head, tail, total
    );
    result.set_out(truncated);
    result.did_spill = true;
    None
}

/// Spill an already-materialized string in result.out.
#[cfg(feature = "localfs")]
async fn spill_string(
    result: &mut ExecResult,
    config: &OutputLimitConfig,
    max: usize,
) -> Option<SpillResult> {
    let total = result.text_out().len();
    match write_spill_file(result.text_out().as_bytes()).await {
        Ok((path, written)) => {
            let truncated = build_truncated_output(&result.text_out(), config, &path, total);
            result.set_out(truncated);
            result.did_spill = true;
            Some(SpillResult {
                path,
                total_bytes: written,
            })
        }
        Err(e) => {
            tracing::error!("output spill failed: {}", e);
            *result = ExecResult::failure(1, format!(
                "output exceeded {} byte limit ({} bytes) and spill to disk failed: {}",
                max, total, e
            ));
            None
        }
    }
}

/// Stream OutputData directly to a spill file without materializing the full String.
#[cfg(feature = "localfs")]
async fn spill_output_data(
    result: &mut ExecResult,
    config: &OutputLimitConfig,
    max: usize,
) -> Option<SpillResult> {
    let output = result.output()?;

    let dir = paths::spill_dir();
    if let Err(e) = tokio::fs::create_dir_all(&dir).await {
        tracing::error!("output spill dir creation failed: {}", e);
        *result = ExecResult::failure(1, format!(
            "output exceeded {} byte limit and spill dir creation failed: {}", max, e
        ));
        return None;
    }

    let filename = generate_spill_filename();
    let path = dir.join(&filename);

    // Write OutputData directly to file via write_canonical
    let total = match std::fs::File::create(&path) {
        Ok(mut file) => {
            match output.write_canonical(&mut file, None) {
                Ok(n) => n,
                Err(e) => {
                    tracing::error!("output spill write failed: {}", e);
                    *result = ExecResult::failure(1, format!(
                        "output exceeded {} byte limit and spill to disk failed: {}", max, e
                    ));
                    return None;
                }
            }
        }
        Err(e) => {
            tracing::error!("output spill file creation failed: {}", e);
            *result = ExecResult::failure(1, format!(
                "output exceeded {} byte limit and spill to disk failed: {}", max, e
            ));
            return None;
        }
    };

    // Read head and tail from the spill file for the truncated preview
    let head = read_head_from_file(&path, config.head_bytes).await.unwrap_or_default();
    let tail = read_tail_from_file(&path, config.tail_bytes).await.unwrap_or_default();
    let path_str = path.to_string_lossy();

    result.set_out(format!(
        "{}\n...\n{}\n[output truncated: {} bytes total — full output at {}]",
        head, tail, total, path_str
    ));
    result.did_spill = true;

    Some(SpillResult {
        path,
        total_bytes: total,
    })
}

/// Write output bytes to a new spill file. Returns (path, bytes_written).
#[cfg(feature = "localfs")]
async fn write_spill_file(data: &[u8]) -> Result<(PathBuf, usize), std::io::Error> {
    let dir = paths::spill_dir();
    tokio::fs::create_dir_all(&dir).await?;

    let filename = generate_spill_filename();
    let path = dir.join(filename);
    tokio::fs::write(&path, data).await?;
    Ok((path, data.len()))
}

/// Build the truncated output string with head, tail, and pointer.
#[cfg(feature = "localfs")]
fn build_truncated_output(
    full: &str,
    config: &OutputLimitConfig,
    spill_path: &std::path::Path,
    total_bytes: usize,
) -> String {
    let head = truncate_to_char_boundary(full, config.head_bytes);
    let tail = tail_from_str(full, config.tail_bytes);
    let path_str = spill_path.to_string_lossy();
    format!(
        "{}\n...\n{}\n[output truncated: {} bytes total — full output at {}]",
        head, tail, total_bytes, path_str
    )
}

/// Truncate a string to at most `max_bytes`, respecting UTF-8 char boundaries.
fn truncate_to_char_boundary(s: &str, max_bytes: usize) -> &str {
    if s.len() <= max_bytes {
        return s;
    }
    // Find the last char boundary at or before max_bytes
    let mut end = max_bytes;
    while end > 0 && !s.is_char_boundary(end) {
        end -= 1;
    }
    &s[..end]
}

/// Get the last `max_bytes` of a string, respecting UTF-8 char boundaries.
fn tail_from_str(s: &str, max_bytes: usize) -> &str {
    if s.len() <= max_bytes {
        return s;
    }
    let start = s.len() - max_bytes;
    let mut adjusted = start;
    while adjusted < s.len() && !s.is_char_boundary(adjusted) {
        adjusted += 1;
    }
    &s[adjusted..]
}

/// Read the first N bytes from a file for head preview.
#[cfg(feature = "localfs")]
async fn read_head_from_file(path: &std::path::Path, max_bytes: usize) -> Result<String, std::io::Error> {
    use tokio::io::AsyncReadExt;

    let mut file = tokio::fs::File::open(path).await?;
    let mut buf = vec![0u8; max_bytes];
    let n = file.read(&mut buf).await?;
    buf.truncate(n);

    let s = String::from_utf8_lossy(&buf);
    // Truncate to char boundary
    let result = truncate_to_char_boundary(&s, max_bytes);
    Ok(result.to_string())
}

/// Read the last N bytes from a file for tail preview.
#[cfg(feature = "localfs")]
async fn read_tail_from_file(path: &std::path::Path, max_bytes: usize) -> Result<String, std::io::Error> {
    use tokio::io::{AsyncReadExt, AsyncSeekExt};

    let mut file = tokio::fs::File::open(path).await?;
    let metadata = file.metadata().await?;
    let len = metadata.len() as usize;

    if len <= max_bytes {
        let mut buf = Vec::new();
        file.read_to_end(&mut buf).await?;
        return Ok(String::from_utf8_lossy(&buf).into_owned());
    }

    let offset = len - max_bytes;
    file.seek(std::io::SeekFrom::Start(offset as u64)).await?;
    let mut buf = vec![0u8; max_bytes];
    let n = file.read(&mut buf).await?;
    buf.truncate(n);

    // Adjust to char boundary
    let s = String::from_utf8_lossy(&buf);
    Ok(s.into_owned())
}

/// Generate a unique spill filename using timestamp, PID, and monotonic counter.
#[cfg(feature = "localfs")]
fn generate_spill_filename() -> String {
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::time::SystemTime;

    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let seq = COUNTER.fetch_add(1, Ordering::Relaxed);
    let ts = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or_default();
    let pid = std::process::id();
    format!("spill-{}.{}-{}-{}.txt", ts.as_secs(), ts.subsec_nanos(), pid, seq)
}

/// Parse a size string with optional K/M suffix into bytes.
///
/// Accepts: "64K", "64k", "1M", "1m", "65536" (raw bytes).
pub fn parse_size(s: &str) -> Result<usize, String> {
    let s = s.trim();
    if s.is_empty() {
        return Err("empty size string".to_string());
    }

    let (num_str, multiplier) = if let Some(n) = s.strip_suffix('K').or_else(|| s.strip_suffix('k')) {
        (n, 1024)
    } else if let Some(n) = s.strip_suffix('M').or_else(|| s.strip_suffix('m')) {
        (n, 1024 * 1024)
    } else {
        (s, 1)
    };

    let num: usize = num_str
        .parse()
        .map_err(|_| format!("invalid size: {}", s))?;

    Ok(num * multiplier)
}

#[cfg(all(test, feature = "localfs"))]
mod tests {
    use super::*;

    #[test]
    fn test_none_is_disabled() {
        let config = OutputLimitConfig::none();
        assert!(!config.is_enabled());
        assert_eq!(config.max_bytes(), None);
    }

    #[test]
    fn test_agent_preset_is_enabled() {
        let config = OutputLimitConfig::agent();
        assert!(config.is_enabled());
        assert_eq!(config.max_bytes(), Some(8 * 1024));
        assert_eq!(config.head_bytes(), 1024);
        assert_eq!(config.tail_bytes(), 512);
    }

    #[test]
    fn test_set_limit() {
        let mut config = OutputLimitConfig::none();
        assert!(!config.is_enabled());

        config.set_limit(Some(1024));
        assert!(config.is_enabled());
        assert_eq!(config.max_bytes(), Some(1024));

        config.set_limit(None);
        assert!(!config.is_enabled());
    }

    #[test]
    fn test_set_head_tail() {
        let mut config = OutputLimitConfig::agent();
        config.set_head_bytes(2048);
        config.set_tail_bytes(1024);
        assert_eq!(config.head_bytes(), 2048);
        assert_eq!(config.tail_bytes(), 1024);
    }

    #[test]
    fn test_parse_size() {
        assert_eq!(parse_size("64K").unwrap(), 64 * 1024);
        assert_eq!(parse_size("64k").unwrap(), 64 * 1024);
        assert_eq!(parse_size("1M").unwrap(), 1024 * 1024);
        assert_eq!(parse_size("1m").unwrap(), 1024 * 1024);
        assert_eq!(parse_size("65536").unwrap(), 65536);
        assert!(parse_size("").is_err());
        assert!(parse_size("abc").is_err());
    }

    #[test]
    fn test_truncate_to_char_boundary() {
        assert_eq!(truncate_to_char_boundary("hello", 10), "hello");
        assert_eq!(truncate_to_char_boundary("hello", 3), "hel");
        // Multi-byte: "日" is 3 bytes
        assert_eq!(truncate_to_char_boundary("日本語", 3), "日");
        assert_eq!(truncate_to_char_boundary("日本語", 4), "日");
        assert_eq!(truncate_to_char_boundary("日本語", 6), "日本");
    }

    #[test]
    fn test_tail_from_str() {
        assert_eq!(tail_from_str("hello", 10), "hello");
        assert_eq!(tail_from_str("hello", 3), "llo");
        // Multi-byte
        assert_eq!(tail_from_str("日本語", 3), "語");
        assert_eq!(tail_from_str("日本語", 6), "本語");
    }

    #[test]
    fn test_generate_spill_filename() {
        let name = generate_spill_filename();
        assert!(name.starts_with("spill-"));
        assert!(name.ends_with(".txt"));
    }

    #[tokio::test]
    async fn test_spill_if_needed_under_limit() {
        let config = OutputLimitConfig::agent();
        let mut result = ExecResult::success("short output");
        let spill = spill_if_needed(&mut result, &config).await;
        assert!(spill.is_none());
        assert_eq!(&*result.text_out(), "short output");
        assert!(!result.did_spill);
    }

    #[tokio::test]
    async fn test_spill_if_needed_over_limit() {
        let config = OutputLimitConfig {
            max_bytes: Some(100),
            head_bytes: 20,
            tail_bytes: 10,
            spill_mode: SpillMode::Disk,
        };
        let big_output = "x".repeat(200);
        let mut result = ExecResult::success(big_output);
        let spill = spill_if_needed(&mut result, &config).await;
        assert!(spill.is_some());
        assert!(result.did_spill);

        let spill = spill.unwrap();
        assert_eq!(spill.total_bytes, 200);
        assert!(spill.path.exists());

        // Verify truncated output
        assert!(result.text_out().contains("..."));
        assert!(result.text_out().contains("[output truncated: 200 bytes total"));
        assert!(result.text_out().contains(&spill.path.to_string_lossy().to_string()));

        // Verify head (first 20 bytes)
        assert!(result.text_out().starts_with(&"x".repeat(20)));

        // Verify spill file has full content
        let spill_content = tokio::fs::read_to_string(&spill.path).await.unwrap();
        assert_eq!(spill_content.len(), 200);

        // Clean up
        let _ = tokio::fs::remove_file(&spill.path).await;
    }

    #[tokio::test]
    async fn test_spill_if_needed_disabled() {
        let config = OutputLimitConfig::none();
        let big_output = "x".repeat(200);
        let mut result = ExecResult::success(big_output.clone());
        let spill = spill_if_needed(&mut result, &config).await;
        assert!(spill.is_none());
        assert_eq!(&*result.text_out(), big_output);
        assert!(!result.did_spill);
    }

    #[test]
    fn test_build_truncated_output() {
        let config = OutputLimitConfig {
            max_bytes: Some(100),
            head_bytes: 5,
            tail_bytes: 3,
            spill_mode: SpillMode::Disk,
        };
        let full = "abcdefghijklmnop";
        let path = PathBuf::from("/tmp/test-spill.txt");
        let result = build_truncated_output(full, &config, &path, 16);
        assert!(result.starts_with("abcde"));
        assert!(result.contains("..."));
        assert!(result.contains("nop"));
        assert!(result.contains("[output truncated: 16 bytes total — full output at /tmp/test-spill.txt]"));
    }

    #[tokio::test]
    async fn test_kernel_agent_truncates_large_output() {
        use crate::kernel::{Kernel, KernelConfig};

        // agent preset has 8K limit by default — use a smaller limit for testing
        let config = KernelConfig::agent()
            .with_output_limit(OutputLimitConfig {
                max_bytes: Some(200),
                head_bytes: 50,
                tail_bytes: 30,
                spill_mode: SpillMode::Disk,
            });
        let kernel = Kernel::new(config).expect("kernel creation");

        // seq 1 10000 produces lots of output
        let result = kernel.execute("seq 1 10000").await.expect("execute");
        assert!(result.text_out().contains("[output truncated:"));
        assert!(result.text_out().contains("full output at"));
        // Head should contain the first numbers
        assert!(result.text_out().starts_with("1\n"));
    }

    /// GH #177: every other Disk-mode test in this module re-specifies
    /// `spill_mode: SpillMode::Disk` explicitly, so none of them actually pin
    /// that a host-backed (`Sandboxed`) kernel's *untouched* default — no
    /// `.with_output_limit()` override at all — is `Disk`. This is the literal
    /// "`Kernel::new` without `.in_memory()`" scenario the issue names. The
    /// forcing logic in `Kernel::assemble` (`no_host_side_channel`) must leave
    /// a `Sandboxed` kernel's config alone; only `NoLocal`/`with_backend`
    /// override it to `Memory`.
    #[tokio::test]
    async fn test_agent_kernel_unmodified_default_spills_to_disk() {
        use crate::kernel::{Kernel, KernelConfig};

        // Untouched: OutputLimitConfig::agent() — 8K limit, SpillMode::Disk.
        let config = KernelConfig::agent();
        assert_eq!(config.output_limit.spill_mode(), SpillMode::Disk);
        let kernel = Kernel::new(config).expect("kernel creation");

        let big = "x".repeat(8 * 1024 + 200);
        let result = kernel.execute(&format!("echo '{}'", big)).await.expect("execute");
        assert_eq!(result.code, 3, "default 8K agent limit should trip the spill");
        assert!(
            result.text_out().contains("full output at"),
            "an unmodified agent() default must spill to a real file, not truncate in \
             memory: {}",
            result.text_out()
        );
    }

    #[tokio::test]
    async fn test_spill_exits_3() {
        use crate::kernel::{Kernel, KernelConfig};

        let config = KernelConfig::agent()
            .with_output_limit(OutputLimitConfig {
                max_bytes: Some(100),
                head_bytes: 30,
                tail_bytes: 20,
                spill_mode: SpillMode::Disk,
            });
        let kernel = Kernel::new(config).expect("kernel creation");

        let big = "x".repeat(200);
        let result = kernel.execute(&format!("echo '{}'", big)).await.expect("execute");
        assert_eq!(result.code, 3, "spill should always exit 3");
        assert_eq!(result.original_code, Some(0), "original command exit code preserved");
        assert!(result.text_out().contains("[output truncated:"));
    }

    #[tokio::test]
    async fn test_kernel_repl_no_truncation() {
        use crate::kernel::{Kernel, KernelConfig};

        // REPL has no limit
        let config = KernelConfig::repl();
        let kernel = Kernel::new(config).expect("kernel creation");

        let result = kernel.execute("seq 1 100").await.expect("execute");
        assert!(!result.text_out().contains("[output truncated:"));
        assert!(result.text_out().contains("100"));
    }

    #[tokio::test]
    async fn test_kernel_builtin_truncation() {
        use crate::kernel::{Kernel, KernelConfig};

        // Builtins go through post-hoc spill check
        let config = KernelConfig::agent()
            .with_output_limit(OutputLimitConfig {
                max_bytes: Some(100),
                head_bytes: 30,
                tail_bytes: 20,
                spill_mode: SpillMode::Disk,
            });
        let kernel = Kernel::new(config).expect("kernel creation");

        // echo with a large string
        let big = "x".repeat(200);
        let result = kernel.execute(&format!("echo '{}'", big)).await.expect("execute");
        assert!(result.text_out().contains("[output truncated:"));
    }

    // ── OutputData estimation and streaming tests ──

    #[test]
    fn test_estimated_byte_size_text() {
        use crate::interpreter::OutputData;
        let data = OutputData::text("hello world");
        assert_eq!(data.estimated_byte_size(), 11);
    }

    #[test]
    fn test_estimated_byte_size_table() {
        use crate::interpreter::{OutputData, OutputNode};
        let data = OutputData::table(
            vec!["NAME".into(), "SIZE".into()],
            vec![
                OutputNode::new("foo").with_cells(vec!["123".into()]),
                OutputNode::new("bar").with_cells(vec!["456".into()]),
            ],
        );
        // "foo\t123\nbar\t456" = 3+1+3 + 1 + 3+1+3 = 15
        assert_eq!(data.estimated_byte_size(), 15);
    }

    #[test]
    fn test_estimated_byte_size_tree() {
        use crate::interpreter::{OutputData, OutputNode};
        let data = OutputData::nodes(vec![
            OutputNode::new("src").with_children(vec![
                OutputNode::new("main.rs"),
                OutputNode::new("lib.rs"),
            ]),
        ]);
        // "src/{main.rs,lib.rs}" = 3 + 2 + 7 + 1 + 6 + 1 = 20
        assert_eq!(data.estimated_byte_size(), 20);
    }

    #[test]
    fn test_write_canonical_matches_to_canonical_string() {
        use crate::interpreter::{OutputData, OutputNode};

        let cases: Vec<OutputData> = vec![
            OutputData::text("hello world"),
            OutputData::nodes(vec![
                OutputNode::new("file1"),
                OutputNode::new("file2"),
            ]),
            OutputData::table(
                vec!["NAME".into(), "SIZE".into()],
                vec![
                    OutputNode::new("foo").with_cells(vec!["123".into()]),
                    OutputNode::new("bar").with_cells(vec!["456".into()]),
                ],
            ),
            OutputData::nodes(vec![
                OutputNode::new("src").with_children(vec![
                    OutputNode::new("main.rs"),
                    OutputNode::new("lib.rs"),
                ]),
            ]),
        ];

        for data in cases {
            let expected = data.to_canonical_string();
            let mut buf = Vec::new();
            let written = data.write_canonical(&mut buf, None).unwrap();
            let got = String::from_utf8(buf).unwrap();
            assert_eq!(got, expected, "write_canonical mismatch for {:?}", data);
            assert_eq!(written, expected.len(), "byte count mismatch");
        }
    }

    #[test]
    fn test_write_canonical_budget_stops_early() {
        use crate::interpreter::{OutputData, OutputNode};

        let data = OutputData::nodes(
            (0..1000).map(|i| OutputNode::new(format!("file_{:04}", i))).collect()
        );
        let mut buf = Vec::new();
        let written = data.write_canonical(&mut buf, Some(100)).unwrap();
        // Should have stopped shortly after 100 bytes
        assert!(written > 100, "should exceed budget slightly");
        assert!(written < 500, "should stop soon after budget: got {}", written);
    }

    #[tokio::test]
    async fn test_spill_if_needed_large_output_data_no_oom() {
        use crate::interpreter::{OutputData, OutputNode};

        let config = OutputLimitConfig {
            max_bytes: Some(1024),
            head_bytes: 100,
            tail_bytes: 50,
            spill_mode: SpillMode::Disk,
        };

        // 100K nodes — large enough to detect OOM if materialized carelessly,
        // but small enough to not slow down the test
        let nodes: Vec<OutputNode> = (0..100_000)
            .map(|i| OutputNode::new(format!("node_{:06}", i)))
            .collect();
        let data = OutputData::nodes(nodes);
        let mut result = ExecResult::with_output(data);

        let spill = spill_if_needed(&mut result, &config).await;
        assert!(spill.is_some(), "should have spilled");
        assert!(result.did_spill);
        assert!(result.text_out().contains("[output truncated:"));

        // Clean up
        if let Some(s) = spill {
            let _ = tokio::fs::remove_file(&s.path).await;
        }
    }

    // ── In-memory spill mode (SpillMode::Memory) ──

    #[test]
    fn test_in_memory_builder_and_default() {
        assert_eq!(OutputLimitConfig::agent().spill_mode(), SpillMode::Disk);
        assert_eq!(OutputLimitConfig::agent().in_memory().spill_mode(), SpillMode::Memory);

        let mut config = OutputLimitConfig::none();
        config.set_spill_mode(SpillMode::Memory);
        assert_eq!(config.spill_mode(), SpillMode::Memory);
    }

    #[tokio::test]
    async fn test_memory_mode_truncates_string_without_disk() {
        let config = OutputLimitConfig {
            max_bytes: Some(100),
            head_bytes: 20,
            tail_bytes: 10,
            spill_mode: SpillMode::Memory,
        };
        let mut result = ExecResult::success("x".repeat(200));
        let spill = spill_if_needed(&mut result, &config).await;

        // No SpillResult (no file written) but did_spill flags the truncation.
        assert!(spill.is_none(), "memory mode must not write a spill file");
        assert!(result.did_spill, "memory truncation must set did_spill for the exit-3 remap");

        let out = result.text_out();
        assert!(out.contains("truncated in memory"), "got: {}", out);
        assert!(out.contains("200 bytes total"), "got: {}", out);
        assert!(!out.contains("full output at"), "memory mode must not point at a file: {}", out);
        assert!(out.starts_with(&"x".repeat(20)), "head preserved");
    }

    #[tokio::test]
    async fn test_memory_mode_under_limit_untouched() {
        let config = OutputLimitConfig {
            max_bytes: Some(100),
            head_bytes: 20,
            tail_bytes: 10,
            spill_mode: SpillMode::Memory,
        };
        let mut result = ExecResult::success("short");
        let spill = spill_if_needed(&mut result, &config).await;
        assert!(spill.is_none());
        assert!(!result.did_spill);
        assert_eq!(&*result.text_out(), "short");
    }

    #[tokio::test]
    async fn test_memory_mode_large_output_data_bounded() {
        use crate::interpreter::{OutputData, OutputNode};

        let config = OutputLimitConfig {
            max_bytes: Some(1024),
            head_bytes: 100,
            tail_bytes: 50,
            spill_mode: SpillMode::Memory,
        };

        // 100K nodes — would be a huge String if fully materialized.
        let nodes: Vec<OutputNode> = (0..100_000)
            .map(|i| OutputNode::new(format!("node_{:06}", i)))
            .collect();
        let mut result = ExecResult::with_output(OutputData::nodes(nodes));

        let spill = spill_if_needed(&mut result, &config).await;
        assert!(spill.is_none(), "memory mode writes no file");
        assert!(result.did_spill);
        let out = result.text_out();
        assert!(out.contains("truncated in memory"), "got: {}", out);
        assert!(out.starts_with("node_000000"), "head rendered: {}", out);
        // Head-only path for oversized structured data: no tail section echoed.
        assert!(out.contains("head only"), "got: {}", out);
    }

    #[tokio::test]
    async fn test_kernel_memory_mode_exits_3_preserves_original() {
        use crate::kernel::{Kernel, KernelConfig};

        let config = KernelConfig::agent().with_output_limit(OutputLimitConfig {
            max_bytes: Some(100),
            head_bytes: 30,
            tail_bytes: 20,
            spill_mode: SpillMode::Memory,
        });
        let kernel = Kernel::new(config).expect("kernel creation");

        let big = "x".repeat(200);
        let result = kernel.execute(&format!("echo '{}'", big)).await.expect("execute");
        assert_eq!(result.code, 3, "memory truncation still signals via exit 3");
        assert_eq!(result.original_code, Some(0), "original exit code preserved");
        assert!(result.text_out().contains("truncated in memory"));
        assert!(!result.text_out().contains("full output at"));
    }

    #[tokio::test]
    async fn test_nolocal_kernel_forces_memory_spill() {
        use crate::kernel::{Kernel, KernelConfig, VfsMountMode};

        // NoLocal mount + an explicit Disk spill mode: the kernel must override
        // to Memory so nothing is written to a host spill file, even though
        // `localfs` is compiled in.
        let config = KernelConfig::agent()
            .with_vfs_mode(VfsMountMode::NoLocal)
            .with_output_limit(OutputLimitConfig {
                max_bytes: Some(100),
                head_bytes: 30,
                tail_bytes: 20,
                spill_mode: SpillMode::Disk,
            });
        let kernel = Kernel::new(config).expect("kernel creation");

        let big = "x".repeat(200);
        let result = kernel.execute(&format!("echo '{}'", big)).await.expect("execute");
        assert_eq!(result.code, 3, "still signals truncation via exit 3");
        assert!(result.text_out().contains("truncated in memory"), "got: {}", result.text_out());
        assert!(
            !result.text_out().contains("full output at"),
            "NoLocal kernel must not write a host spill file: {}",
            result.text_out()
        );
    }

}
