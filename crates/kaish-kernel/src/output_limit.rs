//! Configurable output size limits for agent safety.
//!
//! When output exceeds the threshold, the full output is written to a
//! spill file on the real filesystem and `ExecResult.out` is truncated
//! with a pointer message. The agent can then selectively read the file.
//!
//! Per-mode defaults: MCP kernels get an 8KB limit, REPL/test kernels
//! are unlimited. Runtime-switchable via the `kaish-output-limit` builtin.

use std::path::PathBuf;

use crate::interpreter::ExecResult;
#[cfg(feature = "native")]
use crate::paths;

/// Default output limit for MCP mode (8KB).
const DEFAULT_MCP_LIMIT: usize = 8 * 1024;

/// Default head preview size (bytes of output start to keep).
const DEFAULT_HEAD_BYTES: usize = 1024;

/// Default tail preview size (bytes of output end to keep).
const DEFAULT_TAIL_BYTES: usize = 512;

/// Configurable output size limit.
///
/// Threaded through `KernelConfig` → `ExecContext` → kernel pipeline execution.
/// Runtime-mutable via the `kaish-output-limit` builtin.
#[derive(Debug, Clone)]
pub struct OutputLimitConfig {
    max_bytes: Option<usize>,
    head_bytes: usize,
    tail_bytes: usize,
}

impl OutputLimitConfig {
    /// No limiting — REPL/embedded/test default.
    pub fn none() -> Self {
        Self {
            max_bytes: None,
            head_bytes: DEFAULT_HEAD_BYTES,
            tail_bytes: DEFAULT_TAIL_BYTES,
        }
    }

    /// Default limit used by `on` subcommand and `set -o output-limit`.
    pub fn default_limit() -> usize {
        DEFAULT_MCP_LIMIT
    }

    /// MCP-safe defaults: 8KB limit, 1KB head, 512B tail.
    pub fn mcp() -> Self {
        Self {
            max_bytes: Some(DEFAULT_MCP_LIMIT),
            head_bytes: DEFAULT_HEAD_BYTES,
            tail_bytes: DEFAULT_TAIL_BYTES,
        }
    }

    /// Whether output limiting is enabled.
    pub fn is_enabled(&self) -> bool {
        self.max_bytes.is_some()
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
/// Without the `native` feature, performs in-memory head+tail truncation
/// (no disk I/O).
pub async fn spill_if_needed(
    result: &mut ExecResult,
    config: &OutputLimitConfig,
) -> Option<SpillResult> {
    let max = config.max_bytes?;

    #[cfg(feature = "native")]
    {
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

        None
    }

    // Without native: in-memory head+tail truncation (no disk I/O)
    #[cfg(not(feature = "native"))]
    {
        // Materialize structured OutputData if present
        if result.has_output() {
            result.materialize();
        }

        let total = result.text_out().len();
        if total <= max {
            return None;
        }

        let text = result.text_out().into_owned();
        let head = truncate_to_char_boundary(&text, config.head_bytes);
        let tail = tail_from_str(&text, config.tail_bytes);
        let truncated = format!(
            "{}\n...\n{}\n[output truncated: {} bytes total — spill not available in sandbox mode]",
            head, tail, total
        );
        result.set_out(truncated);
        None
    }
}

/// Spill an already-materialized string in result.out.
#[cfg(feature = "native")]
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
#[cfg(feature = "native")]
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

/// Collect stdout from a child process with spill-aware size limiting.
///
/// Two-phase approach:
/// 1. Detection window (up to 1s): accumulate in memory
/// 2. If still running after 1s and over limit: stream to spill file
///
/// Returns `(stdout_string, stderr_string, did_spill)`.
#[cfg(feature = "native")]
pub async fn spill_aware_collect(
    mut stdout: tokio::process::ChildStdout,
    mut stderr_reader: tokio::process::ChildStderr,
    stderr_stream: Option<crate::scheduler::StderrStream>,
    config: &OutputLimitConfig,
) -> (String, String, bool) {
    let max = config.max_bytes.unwrap_or(usize::MAX);

    // Spawn stderr collection
    let stderr_task = tokio::spawn(async move {
        collect_stderr(&mut stderr_reader, stderr_stream.as_ref()).await
    });

    let (stdout_result, did_spill) = collect_stdout_with_spill(&mut stdout, max, config).await;

    let stderr = stderr_task.await.unwrap_or_default();
    (stdout_result, stderr, did_spill)
}

/// Collect stderr (same pattern as existing dispatch code).
#[cfg(feature = "native")]
async fn collect_stderr(
    reader: &mut tokio::process::ChildStderr,
    stream: Option<&crate::scheduler::StderrStream>,
) -> String {
    use tokio::io::AsyncReadExt;

    let mut buf = Vec::new();
    let mut chunk = [0u8; 8192];
    loop {
        match reader.read(&mut chunk).await {
            Ok(0) => break,
            Ok(n) => {
                if let Some(s) = stream {
                    s.write(&chunk[..n]);
                } else {
                    buf.extend_from_slice(&chunk[..n]);
                }
            }
            Err(_) => break,
        }
    }
    if stream.is_some() {
        String::new()
    } else {
        String::from_utf8_lossy(&buf).into_owned()
    }
}

/// Collect stdout with two-phase spill detection.
///
/// Generic over `AsyncRead + Unpin` — works with `ChildStdout` in production
/// and `DuplexStream` in tests.
///
/// Returns `(stdout_string, did_spill)`.
#[cfg(feature = "native")]
async fn collect_stdout_with_spill<R: tokio::io::AsyncRead + Unpin>(
    stdout: &mut R,
    max_bytes: usize,
    config: &OutputLimitConfig,
) -> (String, bool) {
    use tokio::io::AsyncReadExt;
    use tokio::time::{sleep, Duration};

    let mut buffer = Vec::new();
    let mut chunk = [0u8; 8192];
    let deadline = sleep(Duration::from_secs(1));
    tokio::pin!(deadline);

    // Phase 1: Detection window (up to 1s)
    loop {
        tokio::select! {
            biased;
            result = stdout.read(&mut chunk) => {
                match result {
                    Ok(0) => {
                        // EOF — command finished within detection window.
                        // Post-hoc spill check happens in the caller.
                        return (String::from_utf8_lossy(&buffer).into_owned(), false);
                    }
                    Ok(n) => {
                        buffer.extend_from_slice(&chunk[..n]);
                        // Break early if already over limit — don't OOM during detection window
                        if buffer.len() > max_bytes {
                            break;
                        }
                    }
                    Err(_) => {
                        return (String::from_utf8_lossy(&buffer).into_owned(), false);
                    }
                }
            }
            () = &mut deadline => {
                // 1s elapsed, command still running
                break;
            }
        }
    }

    // Phase 2: Check if we should switch to spill mode
    if buffer.len() > max_bytes {
        // Already over limit — spill what we have and stream the rest to disk
        match stream_to_spill(&buffer, stdout, config).await {
            Ok(result) => return (result, true),
            Err(e) => {
                // Spill failed — return error. Don't continue accumulating.
                // Dropping stdout closes the pipe, which sends SIGPIPE to the child.
                tracing::error!("streaming spill failed: {}", e);
                let size = buffer.len();
                drop(buffer);
                return (format!(
                    "ERROR: output exceeded {} byte limit ({} bytes buffered) and spill to disk failed: {}",
                    max_bytes, size, e
                ), false);
            }
        }
    }

    // Continue collecting (under limit so far)
    // Check size after each chunk
    loop {
        match stdout.read(&mut chunk).await {
            Ok(0) => break,
            Ok(n) => {
                buffer.extend_from_slice(&chunk[..n]);
                // Check if we've exceeded limit mid-stream
                if buffer.len() > max_bytes {
                    match stream_to_spill(&buffer, stdout, config).await {
                        Ok(result) => return (result, true),
                        Err(e) => {
                            tracing::error!("streaming spill failed: {}", e);
                            let size = buffer.len();
                            drop(buffer);
                            return (format!(
                                "ERROR: output exceeded {} byte limit ({} bytes buffered) and spill to disk failed: {}",
                                max_bytes, size, e
                            ), false);
                        }
                    }
                }
            }
            Err(_) => break,
        }
    }

    (String::from_utf8_lossy(&buffer).into_owned(), false)
}

/// Write buffered data + remaining stdout to a spill file, return truncated result.
///
/// Generic over `AsyncRead + Unpin` for testability.
#[cfg(feature = "native")]
async fn stream_to_spill<R: tokio::io::AsyncRead + Unpin>(
    buffer: &[u8],
    stdout: &mut R,
    config: &OutputLimitConfig,
) -> Result<String, std::io::Error> {
    use tokio::io::AsyncReadExt;

    let spill_dir = paths::spill_dir();
    tokio::fs::create_dir_all(&spill_dir).await?;

    let filename = generate_spill_filename();
    let path = spill_dir.join(&filename);
    let mut file = tokio::fs::File::create(&path).await?;

    // Write buffered data
    use tokio::io::AsyncWriteExt;
    file.write_all(buffer).await?;
    let mut total = buffer.len();

    // Stream remaining chunks directly to file
    let mut chunk = [0u8; 8192];
    loop {
        match stdout.read(&mut chunk).await {
            Ok(0) => break,
            Ok(n) => {
                file.write_all(&chunk[..n]).await?;
                total += n;
            }
            Err(_) => break,
        }
    }
    file.flush().await?;

    // Read head + tail for the truncated message
    let full = String::from_utf8_lossy(buffer);
    let head = truncate_to_char_boundary(&full, config.head_bytes);

    // For tail, read from the spill file if buffer doesn't cover the end
    let tail: String = if total <= buffer.len() {
        let full_str = String::from_utf8_lossy(buffer);
        tail_from_str(&full_str, config.tail_bytes).to_string()
    } else {
        read_tail_from_file(&path, config.tail_bytes).await.unwrap_or_default()
    };

    let path_str = path.to_string_lossy();
    Ok(format!(
        "{}\n...\n{}\n[output truncated: {} bytes total — full output at {}]",
        head, tail, total, path_str
    ))
}

/// Write output bytes to a new spill file. Returns (path, bytes_written).
#[cfg(feature = "native")]
async fn write_spill_file(data: &[u8]) -> Result<(PathBuf, usize), std::io::Error> {
    let dir = paths::spill_dir();
    tokio::fs::create_dir_all(&dir).await?;

    let filename = generate_spill_filename();
    let path = dir.join(filename);
    tokio::fs::write(&path, data).await?;
    Ok((path, data.len()))
}

/// Build the truncated output string with head, tail, and pointer.
#[cfg(feature = "native")]
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
#[cfg(feature = "native")]
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
#[cfg(feature = "native")]
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
#[cfg(feature = "native")]
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

#[cfg(all(test, feature = "native"))]
mod tests {
    use super::*;

    #[test]
    fn test_none_is_disabled() {
        let config = OutputLimitConfig::none();
        assert!(!config.is_enabled());
        assert_eq!(config.max_bytes(), None);
    }

    #[test]
    fn test_mcp_is_enabled() {
        let config = OutputLimitConfig::mcp();
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
        let mut config = OutputLimitConfig::mcp();
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
        let config = OutputLimitConfig::mcp();
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
    async fn test_kernel_mcp_truncates_large_output() {
        use crate::kernel::{Kernel, KernelConfig};

        // MCP config has 8K limit by default — use a smaller limit for testing
        let config = KernelConfig::mcp()
            .with_output_limit(OutputLimitConfig {
                max_bytes: Some(200),
                head_bytes: 50,
                tail_bytes: 30,
            });
        let kernel = Kernel::new(config).expect("kernel creation");

        // seq 1 10000 produces lots of output
        let result = kernel.execute("seq 1 10000").await.expect("execute");
        assert!(result.text_out().contains("[output truncated:"));
        assert!(result.text_out().contains("full output at"));
        // Head should contain the first numbers
        assert!(result.text_out().starts_with("1\n"));
    }

    #[tokio::test]
    async fn test_spill_exits_3() {
        use crate::kernel::{Kernel, KernelConfig};

        let config = KernelConfig::mcp()
            .with_output_limit(OutputLimitConfig {
                max_bytes: Some(100),
                head_bytes: 30,
                tail_bytes: 20,
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
        let config = KernelConfig::mcp()
            .with_output_limit(OutputLimitConfig {
                max_bytes: Some(100),
                head_bytes: 30,
                tail_bytes: 20,
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

    // ── Streaming collector tests (using tokio::io::duplex) ──

    #[tokio::test]
    async fn test_collect_small_output_no_spill() {
        let (mut writer, reader) = tokio::io::duplex(1024);
        let config = OutputLimitConfig {
            max_bytes: Some(1024),
            head_bytes: 100,
            tail_bytes: 50,
        };

        // Write small data and close
        use tokio::io::AsyncWriteExt;
        writer.write_all(b"hello world").await.unwrap();
        drop(writer); // EOF

        let mut reader = reader;
        let (result, did_spill) = collect_stdout_with_spill(&mut reader, 1024, &config).await;
        assert_eq!(result, "hello world");
        assert!(!did_spill);
    }

    #[tokio::test]
    async fn test_collect_large_output_spills() {
        let (mut writer, reader) = tokio::io::duplex(64 * 1024);
        let config = OutputLimitConfig {
            max_bytes: Some(100),
            head_bytes: 20,
            tail_bytes: 10,
        };

        // Write data exceeding limit and close
        use tokio::io::AsyncWriteExt;
        let data = "x".repeat(500);
        writer.write_all(data.as_bytes()).await.unwrap();
        drop(writer); // EOF

        let mut reader = reader;
        let (result, did_spill) = collect_stdout_with_spill(&mut reader, 100, &config).await;
        assert!(did_spill, "should have spilled");
        assert!(result.contains("[output truncated:"));
        assert!(result.contains("full output at"));
    }

    #[tokio::test]
    async fn test_collect_exact_boundary_no_spill() {
        let (mut writer, reader) = tokio::io::duplex(1024);
        let config = OutputLimitConfig {
            max_bytes: Some(100),
            head_bytes: 20,
            tail_bytes: 10,
        };

        // Write exactly max_bytes
        use tokio::io::AsyncWriteExt;
        let data = "x".repeat(100);
        writer.write_all(data.as_bytes()).await.unwrap();
        drop(writer); // EOF

        let mut reader = reader;
        let (result, did_spill) = collect_stdout_with_spill(&mut reader, 100, &config).await;
        // Exactly at limit — should not spill (<=)
        assert!(!did_spill, "exact boundary should not spill");
        assert_eq!(result.len(), 100);
    }

    #[tokio::test]
    async fn test_collect_broken_pipe() {
        let (writer, reader) = tokio::io::duplex(1024);
        let config = OutputLimitConfig {
            max_bytes: Some(1024),
            head_bytes: 100,
            tail_bytes: 50,
        };

        // Write some data, then drop writer mid-stream
        use tokio::io::AsyncWriteExt;
        let mut writer = writer;
        writer.write_all(b"partial data").await.unwrap();
        drop(writer); // Simulate broken pipe

        let mut reader = reader;
        let (result, did_spill) = collect_stdout_with_spill(&mut reader, 1024, &config).await;
        assert_eq!(result, "partial data");
        assert!(!did_spill);
    }
}
