//! `grep -r` over a tree that holds special files, symlinks, unreadable
//! entries, and files with no useful end must finish, stay bounded, and
//! report trouble the way GNU grep does.
//!
//! GNU grep 3.12 is the reference, checked on a fixture:
//! - `-r` skips symlinks, FIFOs, sockets, and devices it finds while
//!   recursing. A command-line operand is still read.
//! - `-R` follows symlinks it finds while recursing.
//! - An unreadable file or directory is named on stderr, the walk goes on,
//!   and the exit status is 2.
//!
//! Every case runs under a time bound so a hang fails instead of hanging.
//! Endless files are served by an instrumented filesystem that counts the
//! bytes it hands out, so a test asserts a bound instead of waiting for OOM.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(all(feature = "localfs", unix))]

mod common;

use std::io;
use std::os::unix::fs::{symlink, PermissionsExt};
use std::path::Path;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;

use async_trait::async_trait;
use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::vfs::{DirEntry, Filesystem, MemoryFs, VfsRouter};
use kaish_kernel::{Kernel, KernelBackend, KernelConfig, LocalBackend};
use kaish_types::ReadRange;

use common::kernel_at;

/// Long enough for any of these fixtures on a loaded machine; short enough
/// that a hang fails the run quickly.
const BOUND: Duration = Duration::from_secs(20);

async fn run_bounded(kernel: &Kernel, script: &str) -> Option<ExecResult> {
    tokio::time::timeout(BOUND, kernel.execute(script))
        .await
        .ok()
        .map(|result| result.expect("kernel execute"))
}

/// True when mode 000 does not stop this process from reading (root, or a
/// filesystem that bypasses DAC). Permission cases skip there.
fn dac_is_bypassed(path: &Path) -> bool {
    std::fs::File::open(path).is_ok()
}

fn mkfifo(path: &Path) {
    let status = std::process::Command::new("mkfifo")
        .arg(path)
        .status()
        .expect("run mkfifo");
    assert!(status.success(), "mkfifo {path:?} failed");
}

/// Open `fifo` for writing and close it, which ends a read blocked on it.
/// Runs on a detached thread: with no reader waiting, the open would block.
fn release_fifo_reader(fifo: &Path) {
    let fifo = fifo.to_path_buf();
    std::thread::spawn(move || {
        // Ignored: this only frees a stuck reader so the runtime can shut
        // down; the test has already failed by the time it runs.
        let _ = std::fs::OpenOptions::new().write(true).open(&fifo);
    });
}

// ── Special files found while recursing ─────────────────────────────────

#[tokio::test]
async fn recursion_skips_a_fifo() {
    let tmp = tempfile::tempdir().unwrap();
    std::fs::write(tmp.path().join("plain.txt"), "needle\n").unwrap();
    let fifo = tmp.path().join("pipe");
    mkfifo(&fifo);
    let kernel = kernel_at(tmp.path());

    let Some(result) = run_bounded(&kernel, "grep -r needle .").await else {
        release_fifo_reader(&fifo);
        panic!("grep -r blocked opening a FIFO it found while recursing");
    };
    assert_eq!(result.text_out().trim(), "./plain.txt:needle");
    assert_eq!(result.err, "", "a skipped FIFO is not an error");
    assert_eq!(result.code, 0);
}

#[tokio::test]
async fn recursion_skips_a_socket_silently() {
    let tmp = tempfile::tempdir().unwrap();
    std::fs::write(tmp.path().join("plain.txt"), "needle\n").unwrap();
    let _listener = std::os::unix::net::UnixListener::bind(tmp.path().join("sock")).unwrap();
    let kernel = kernel_at(tmp.path());

    let result = run_bounded(&kernel, "grep -r needle .").await.expect("grep -r finished");
    assert_eq!(result.text_out().trim(), "./plain.txt:needle");
    assert_eq!(result.err, "", "a skipped socket is not an error");
    assert_eq!(result.code, 0);
}

#[tokio::test]
async fn recursion_skips_a_symlink_to_a_fifo_even_with_dereference() {
    let tmp = tempfile::tempdir().unwrap();
    std::fs::write(tmp.path().join("plain.txt"), "needle\n").unwrap();
    let fifo = tmp.path().join("pipe-target");
    mkfifo(&fifo);
    std::fs::create_dir(tmp.path().join("tree")).unwrap();
    std::fs::write(tmp.path().join("tree/plain.txt"), "needle\n").unwrap();
    symlink("../pipe-target", tmp.path().join("tree/pipe-link")).unwrap();
    let kernel = kernel_at(tmp.path());

    let Some(result) = run_bounded(&kernel, "grep -R needle tree").await else {
        release_fifo_reader(&fifo);
        panic!("grep -R blocked on a symlink to a FIFO");
    };
    assert_eq!(result.text_out().trim(), "tree/plain.txt:needle");
    assert_eq!(result.err, "");
    assert_eq!(result.code, 0);
}

/// kaish's `/dev` is synthetic (DevFs). Its endless devices must be skipped
/// like any other device, not read and not reported.
#[tokio::test]
async fn recursion_over_synthetic_dev_skips_devices() {
    let tmp = tempfile::tempdir().unwrap();
    let mut vfs = VfsRouter::new();
    vfs.mount("/", kaish_kernel::vfs::LocalFs::read_only(tmp.path()));
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    let kernel = Kernel::with_backend(backend, KernelConfig::isolated(), |_| {}, |_| {})
        .expect("with_backend kernel");

    let result = run_bounded(&kernel, "grep -r needle /dev").await.expect("grep -r finished");
    assert_eq!(result.text_out(), "");
    assert_eq!(result.err, "", "devices found while recursing are skipped, not reported");
    assert_eq!(result.code, 1);
}

// ── Symlinks found while recursing ───────────────────────────────────────

fn tree_with_links(root: &Path) {
    std::fs::create_dir_all(root.join("tree/sub")).unwrap();
    std::fs::write(root.join("tree/plain.txt"), "needle plain\n").unwrap();
    std::fs::write(root.join("tree/sub/deep.txt"), "needle deep\n").unwrap();
    std::fs::write(root.join("outside.txt"), "needle outside\n").unwrap();
    symlink("../outside.txt", root.join("tree/file-link")).unwrap();
    symlink("sub", root.join("tree/dir-link")).unwrap();
    symlink("..", root.join("tree/sub/loop")).unwrap();
    symlink("missing.txt", root.join("tree/dangling")).unwrap();
}

#[tokio::test]
async fn lowercase_r_skips_symlinks_found_while_recursing() {
    let tmp = tempfile::tempdir().unwrap();
    tree_with_links(tmp.path());
    let kernel = kernel_at(tmp.path());

    let result = run_bounded(&kernel, "grep -r needle tree").await.expect("grep -r finished");
    assert_eq!(
        result.text_out().trim(),
        "tree/plain.txt:needle plain\ntree/sub/deep.txt:needle deep",
        "GNU grep -r does not read a symlink it finds while recursing"
    );
    assert_eq!(result.err, "");
    assert_eq!(result.code, 0);
}

#[tokio::test]
async fn uppercase_r_reads_a_file_symlink_found_while_recursing() {
    let tmp = tempfile::tempdir().unwrap();
    tree_with_links(tmp.path());
    let kernel = kernel_at(tmp.path());

    let result = run_bounded(&kernel, "grep -R needle tree").await.expect("grep -R finished");
    let out = result.text_out();
    assert!(
        out.contains("tree/file-link:needle outside"),
        "grep -R reads a file symlink: {out:?}"
    );
    assert!(out.contains("tree/plain.txt:needle plain"), "{out:?}");
}

#[tokio::test]
async fn a_symlink_operand_is_followed_under_lowercase_r() {
    let tmp = tempfile::tempdir().unwrap();
    tree_with_links(tmp.path());
    let kernel = kernel_at(tmp.path());

    let result = run_bounded(&kernel, "grep -r needle tree/file-link")
        .await
        .expect("grep -r finished");
    assert_eq!(result.text_out().trim(), "needle outside");
    assert_eq!(result.code, 0);
}

// ── Unreadable entries ───────────────────────────────────────────────────

#[tokio::test]
async fn an_unreadable_file_is_reported_and_the_walk_goes_on() {
    let tmp = tempfile::tempdir().unwrap();
    std::fs::write(tmp.path().join("a.txt"), "needle a\n").unwrap();
    let locked = tmp.path().join("b.txt");
    std::fs::write(&locked, "needle b\n").unwrap();
    std::fs::write(tmp.path().join("c.txt"), "needle c\n").unwrap();
    std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o000)).unwrap();
    if dac_is_bypassed(&locked) {
        return;
    }
    let kernel = kernel_at(tmp.path());

    let result = run_bounded(&kernel, "grep -r needle .").await.expect("grep -r finished");
    assert_eq!(result.text_out().trim(), "./a.txt:needle a\n./c.txt:needle c");
    assert!(
        result.err.starts_with("grep: ./b.txt: ") && result.err.ends_with('\n'),
        "stderr names the unreadable file: {:?}",
        result.err
    );
    assert_eq!(result.code, 2, "an unreadable file makes the exit 2 even with matches");
}

#[tokio::test]
async fn an_unreadable_directory_is_reported_and_the_walk_goes_on() {
    let tmp = tempfile::tempdir().unwrap();
    std::fs::write(tmp.path().join("a.txt"), "needle a\n").unwrap();
    let locked = tmp.path().join("locked");
    std::fs::create_dir(&locked).unwrap();
    std::fs::write(locked.join("inner.txt"), "needle inner\n").unwrap();
    std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o000)).unwrap();
    if std::fs::read_dir(&locked).is_ok() {
        std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o755)).unwrap();
        return;
    }
    let kernel = kernel_at(tmp.path());

    let result = run_bounded(&kernel, "grep -r needle .").await;
    std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o755)).unwrap();
    let result = result.expect("grep -r finished");
    assert_eq!(result.text_out().trim(), "./a.txt:needle a");
    assert!(
        result.err.starts_with("grep: ./locked: ") && result.err.ends_with('\n'),
        "stderr names the unreadable directory: {:?}",
        result.err
    );
    assert_eq!(result.code, 2);
}

#[tokio::test]
async fn quiet_still_exits_zero_on_a_match_despite_an_unreadable_file() {
    let tmp = tempfile::tempdir().unwrap();
    let locked = tmp.path().join("a.txt");
    std::fs::write(&locked, "needle a\n").unwrap();
    std::fs::write(tmp.path().join("b.txt"), "needle b\n").unwrap();
    std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o000)).unwrap();
    if dac_is_bypassed(&locked) {
        return;
    }
    let kernel = kernel_at(tmp.path());

    let result = run_bounded(&kernel, "grep -rq needle .").await.expect("grep -rq finished");
    assert_eq!(result.code, 0, "GNU -q exits 0 on any match: {:?}", result.err);
}

// ── Files with no useful end (instrumented) ──────────────────────────────

/// What one [`GeneratedFs`] file holds. Every shape is a pure function of
/// the byte offset, so any range can be served without storing the file.
#[derive(Clone, Copy)]
enum Content {
    /// Zero bytes forever, like `/proc/<pid>/pagemap` or `/dev/zero`.
    Zeros,
    /// `needle 00000000\n` forever.
    EndlessNeedles,
    /// `lines` lines of 16 bytes each: `line 0000000000\n`, except the
    /// listed 0-based lines read `needle 00000000\n`.
    Lines { lines: u64, needles: &'static [u64] },
}

const LINE_WIDTH: u64 = 16;

impl Content {
    fn len(self) -> Option<u64> {
        match self {
            Content::Zeros | Content::EndlessNeedles => None,
            Content::Lines { lines, .. } => Some(lines * LINE_WIDTH),
        }
    }

    fn line(self, index: u64) -> String {
        match self {
            Content::Zeros => unreachable!("zeros have no lines"),
            Content::EndlessNeedles => format!("needle {index:08}\n"),
            Content::Lines { needles, .. } if needles.contains(&index) => {
                format!("needle {index:08}\n")
            }
            Content::Lines { .. } => format!("line {index:010}\n"),
        }
    }

    fn bytes(self, offset: u64, limit: u64) -> Vec<u8> {
        let end = match self.len() {
            Some(len) => (offset + limit).min(len),
            None => offset + limit,
        };
        if end <= offset {
            return Vec::new();
        }
        if let Content::Zeros = self {
            return vec![0; (end - offset) as usize];
        }
        let mut out = Vec::with_capacity((end - offset) as usize);
        let mut index = offset / LINE_WIDTH;
        while (index * LINE_WIDTH) < end {
            let line = self.line(index);
            let start = index * LINE_WIDTH;
            for (i, byte) in line.bytes().enumerate() {
                let at = start + i as u64;
                if at >= offset && at < end {
                    out.push(byte);
                }
            }
            index += 1;
        }
        out
    }
}

#[derive(Default)]
struct Counters {
    whole_reads: AtomicUsize,
    bytes_served: AtomicU64,
    largest_request: AtomicU64,
}

/// A read-only mount holding one generated file. A whole-file read is
/// refused and counted. Byte reads are served and counted, and stop with an
/// error past [`SERVE_CAP`] so a runaway reader ends instead of looping.
struct GeneratedFs {
    name: &'static str,
    content: Content,
    counters: Arc<Counters>,
}

const SERVE_CAP: u64 = 64 * 1024 * 1024;

impl GeneratedFs {
    fn file(&self, path: &Path) -> io::Result<()> {
        if path.to_string_lossy().trim_matches('/') == self.name {
            Ok(())
        } else {
            Err(io::Error::new(io::ErrorKind::NotFound, path.display().to_string()))
        }
    }

    fn is_root(path: &Path) -> bool {
        path.to_string_lossy().trim_matches('/').is_empty()
    }

    fn whole_read_refused(&self) -> io::Error {
        self.counters.whole_reads.fetch_add(1, Ordering::SeqCst);
        io::Error::other(format!("{} has no end; a whole read is unbounded", self.name))
    }
}

#[async_trait]
impl Filesystem for GeneratedFs {
    async fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        self.file(path)?;
        Err(self.whole_read_refused())
    }

    async fn read_range(&self, path: &Path, range: Option<ReadRange>) -> io::Result<Vec<u8>> {
        self.file(path)?;
        let Some(ReadRange { offset, limit: Some(limit), .. }) = range else {
            return Err(self.whole_read_refused());
        };
        let offset = offset.unwrap_or(0);
        self.counters.largest_request.fetch_max(limit, Ordering::SeqCst);
        let served = self.counters.bytes_served.load(Ordering::SeqCst);
        if served > SERVE_CAP {
            return Err(io::Error::other(format!("{} served past the test cap", self.name)));
        }
        let bytes = self.content.bytes(offset, limit);
        self.counters.bytes_served.fetch_add(bytes.len() as u64, Ordering::SeqCst);
        Ok(bytes)
    }

    async fn write(&self, _path: &Path, _data: &[u8]) -> io::Result<()> {
        Err(io::Error::new(io::ErrorKind::ReadOnlyFilesystem, "read-only"))
    }

    async fn list(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        if Self::is_root(path) {
            return Ok(vec![DirEntry::file(self.name, self.content.len().unwrap_or(0))]);
        }
        self.file(path)?;
        Err(io::Error::new(io::ErrorKind::NotADirectory, path.display().to_string()))
    }

    async fn stat(&self, path: &Path) -> io::Result<DirEntry> {
        if Self::is_root(path) {
            return Ok(DirEntry::directory("data"));
        }
        self.file(path)?;
        Ok(DirEntry::file(self.name, self.content.len().unwrap_or(0)))
    }

    async fn mkdir(&self, _path: &Path) -> io::Result<()> {
        Err(io::Error::new(io::ErrorKind::ReadOnlyFilesystem, "read-only"))
    }

    async fn remove(&self, _path: &Path) -> io::Result<()> {
        Err(io::Error::new(io::ErrorKind::ReadOnlyFilesystem, "read-only"))
    }

    fn read_only(&self) -> bool {
        true
    }
}

/// A kernel whose `/data` holds one generated file named `name`.
fn generated_kernel(name: &'static str, content: Content) -> (Kernel, Arc<Counters>) {
    let counters = Arc::new(Counters::default());
    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::new());
    vfs.mount(
        "/data",
        GeneratedFs { name, content, counters: Arc::clone(&counters) },
    );
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    let kernel = Kernel::with_backend(backend, KernelConfig::isolated(), |_| {}, |_| {})
        .expect("with_backend kernel");
    (kernel, counters)
}

/// One chunk of a forward scan: the most a bounded reader may ask for at once.
const CHUNK: u64 = 256 * 1024;

#[tokio::test]
async fn a_file_of_endless_zeros_is_read_one_chunk_and_dropped_as_binary() {
    let (kernel, counters) = generated_kernel("pagemap", Content::Zeros);

    let result = run_bounded(&kernel, "grep -r needle /data").await.expect("grep -r finished");
    assert_eq!(counters.whole_reads.load(Ordering::SeqCst), 0, "no whole-file read");
    assert!(
        counters.bytes_served.load(Ordering::SeqCst) <= CHUNK,
        "binary data at the start stops the read: served {} bytes",
        counters.bytes_served.load(Ordering::SeqCst)
    );
    assert_eq!(result.text_out(), "");
    assert_eq!(result.err, "");
    assert_eq!(result.code, 1);
}

#[tokio::test]
async fn files_with_matches_stops_reading_at_the_first_match() {
    let (kernel, counters) = generated_kernel("endless.txt", Content::EndlessNeedles);

    let result = run_bounded(&kernel, "grep -rl needle /data").await.expect("grep -rl finished");
    assert_eq!(result.text_out().trim(), "/data/endless.txt");
    assert_eq!(result.code, 0, "{:?}", result.err);
    assert_eq!(counters.whole_reads.load(Ordering::SeqCst), 0);
    assert!(counters.bytes_served.load(Ordering::SeqCst) <= 2 * CHUNK);
}

#[tokio::test]
async fn max_count_stops_reading_once_reached() {
    let (kernel, counters) = generated_kernel("endless.txt", Content::EndlessNeedles);

    let result = run_bounded(&kernel, "grep -r -m 2 needle /data").await.expect("grep -m finished");
    assert_eq!(
        result.text_out().trim(),
        "/data/endless.txt:needle 00000000\n/data/endless.txt:needle 00000001"
    );
    assert_eq!(result.code, 0, "{:?}", result.err);
    assert_eq!(counters.whole_reads.load(Ordering::SeqCst), 0);
    assert!(counters.bytes_served.load(Ordering::SeqCst) <= 2 * CHUNK);
}

#[tokio::test]
async fn quiet_stops_reading_at_the_first_match() {
    let (kernel, counters) = generated_kernel("endless.txt", Content::EndlessNeedles);

    let result = run_bounded(&kernel, "grep -rq needle /data").await.expect("grep -rq finished");
    assert_eq!(result.code, 0, "{:?}", result.err);
    assert_eq!(counters.whole_reads.load(Ordering::SeqCst), 0);
    assert!(counters.bytes_served.load(Ordering::SeqCst) <= 2 * CHUNK);
}

/// A large text file streams in chunks: no single request is larger than a
/// chunk, and line numbers count across chunk boundaries.
#[tokio::test]
async fn a_large_text_file_streams_with_correct_line_numbers() {
    // 1 MiB: four chunks. Needles straddle the first chunk boundary
    // (line 16383 ends at byte 262144) and sit near the end.
    let content = Content::Lines { lines: 65_536, needles: &[3, 16_383, 16_384, 65_535] };
    let (kernel, counters) = generated_kernel("big.txt", content);

    let result = run_bounded(&kernel, "grep -rn needle /data").await.expect("grep -rn finished");
    assert_eq!(
        result.text_out().trim(),
        "/data/big.txt:4:needle 00000003\n\
         /data/big.txt:16384:needle 00016383\n\
         /data/big.txt:16385:needle 00016384\n\
         /data/big.txt:65536:needle 00065535"
    );
    assert_eq!(result.code, 0, "{:?}", result.err);
    assert_eq!(counters.whole_reads.load(Ordering::SeqCst), 0, "no whole-file read");
    assert!(counters.largest_request.load(Ordering::SeqCst) <= CHUNK);
}

#[tokio::test]
async fn count_streams_a_large_text_file() {
    let content = Content::Lines { lines: 65_536, needles: &[0, 40_000, 65_535] };
    let (kernel, counters) = generated_kernel("big.txt", content);

    let result = run_bounded(&kernel, "grep -rc needle /data").await.expect("grep -rc finished");
    assert_eq!(result.text_out().trim(), "/data/big.txt:3");
    assert_eq!(counters.whole_reads.load(Ordering::SeqCst), 0);
    assert!(counters.largest_request.load(Ordering::SeqCst) <= CHUNK);
}
