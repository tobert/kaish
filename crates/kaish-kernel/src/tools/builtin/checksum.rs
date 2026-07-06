//! checksum — Compute file hashes (sha256, sha1, md5).

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use digest::Digest;

use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::builtin::get_path_string;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Checksum tool: compute or verify file hashes.
pub struct Checksum;

/// clap-derived argv layer for checksum.
#[derive(Parser, Debug)]
#[command(name = "checksum", about = "Compute or verify file hashes")]
struct ChecksumArgs {
    /// Hash algorithm: sha256, sha1, md5 (-a)
    #[arg(short = 'a', long = "algo")]
    algo: Option<String>,

    /// Verify checksums from file (-c)
    #[arg(short = 'c', long = "check")]
    check: Option<String>,

    /// Use SHA-256 (algorithm shortcut)
    #[arg(long = "sha256")]
    sha256: bool,

    /// Use SHA-1 (algorithm shortcut)
    #[arg(long = "sha1")]
    sha1: bool,

    /// Use MD5 (algorithm shortcut)
    #[arg(long = "md5")]
    md5: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Files to checksum; reads stdin when none are given.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Checksum {
    fn name(&self) -> &str {
        "checksum"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &ChecksumArgs::command(),
            "checksum",
            "Compute or verify file hashes",
            [
                ("SHA256 of a file", "checksum README.md"),
                ("MD5", "checksum -a md5 file.tar.gz"),
                ("Hash stdin", "echo hello | checksum"),
                ("Verify", "checksum -c checksums.txt"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let parsed = match ChecksumArgs::try_parse_from(
            std::iter::once("checksum".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("checksum: {e}")),
        };
        parsed.global.apply(ctx);

        let algo = parsed.algo.clone()
            .or_else(|| args.get_string("algo", usize::MAX))
            .or_else(|| {
                // --sha256/--sha1/--md5 shortcuts
                if parsed.sha256 {
                    Some("sha256".into())
                } else if parsed.sha1 {
                    Some("sha1".into())
                } else if parsed.md5 {
                    Some("md5".into())
                } else {
                    None
                }
            })
            .unwrap_or_else(|| "sha256".to_string());

        // Validate algorithm
        if !matches!(algo.as_str(), "sha256" | "sha1" | "md5") {
            return ExecResult::failure(
                1,
                format!("checksum: unknown algorithm '{}' (use sha256, sha1, or md5)", algo),
            );
        }

        // Check mode: verify checksums from a file. Read the untouched typed
        // value FIRST (not `parsed.check`) — clap's own field comes from
        // `to_argv()`'s re-serialization, which is exactly the lossy
        // stringify-to-`[binary: N bytes]` boundary this PR closes elsewhere,
        // so checking it first would silently defeat the guard below. A
        // binary `--check` value goes loud rather than silently falling
        // through to ordinary hash mode (which would hash the files named as
        // if `--check` was never passed at all — a silently wrong operation,
        // not just a wrong path).
        let check_path = match get_path_string(&args, "check", usize::MAX) {
            Ok(Some(p)) => Some(p),
            Ok(None) => parsed.check.clone(),
            Err(e) => return ExecResult::failure(1, format!("checksum: {e}")),
        };
        if let Some(check_path) = check_path {
            return self.verify_checksums(ctx, &check_path, &algo).await;
        }

        // Collect file paths, expanding globs
        let paths = match ctx.expand_paths(&args.positional).await {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(1, format!("checksum: {}", e)),
        };

        if paths.is_empty() {
            // Hash stdin as raw bytes — a lossy decode would hash the wrong
            // bytes for binary input (e.g. `dd if=/dev/urandom | checksum`).
            let input = ctx.read_stdin_to_bytes().await.unwrap_or_default();
            let hash = compute_hash(&input, &algo);
            let text = format!("{}  -", hash);
            // Table convention (OutputData::to_json): first header binds to
            // node.name, remaining headers to cells — so HASH is the name.
            let node = OutputNode::new(&hash).with_cells(vec!["-".to_string(), algo]);
            return ExecResult::with_output_and_text(
                OutputData::table(
                    vec!["HASH".to_string(), "FILE".to_string(), "ALGO".to_string()],
                    vec![node],
                ),
                text,
            );
        }

        // Hash each file
        let mut nodes = Vec::new();
        let mut text_lines = Vec::new();
        for path in &paths {
            let resolved = ctx.resolve_path(path);
            // Stream the file into the hasher rather than reading it whole — a
            // digest reduces any size of input to a fixed string, so this is a
            // pure win: bounded memory even on multi-GB files.
            let mut hasher = StreamHasher::new(&algo);
            match ctx
                .read_file_chunked(&resolved, ExecContext::STREAM_CHUNK_SIZE, |chunk| {
                    hasher.update(chunk);
                    std::ops::ControlFlow::Continue(())
                })
                .await
            {
                Ok(()) => {
                    let hash = hasher.finalize_hex();
                    let line = format!("{}  {}", hash, path);
                    // First header (HASH) binds to node.name; FILE/ALGO are
                    // cells. The old code put the rendered line in the name,
                    // which scrambled `--json` (HASH=line, FILE=hash,
                    // ALGO=path, algo dropped).
                    nodes.push(
                        OutputNode::new(&hash)
                            .with_cells(vec![path.clone(), algo.clone()]),
                    );
                    text_lines.push(line);
                }
                Err(e) => {
                    return ExecResult::failure(1, format!("checksum: {}: {}", path, e));
                }
            }
        }

        let text = text_lines.join("\n");
        let output = OutputData::table(
            vec!["HASH".to_string(), "FILE".to_string(), "ALGO".to_string()],
            nodes,
        );
        ExecResult::with_output_and_text(output, text)
    }
}

impl Checksum {
    /// Verify checksums from a file. Each line: "HASH  FILENAME"
    async fn verify_checksums(
        &self,
        ctx: &mut ExecContext,
        check_path: &str,
        algo: &str,
    ) -> ExecResult {
        let resolved = ctx.resolve_path(check_path);
        let data = match ctx.backend.read(Path::new(&resolved), None).await {
            Ok(d) => d,
            Err(e) => {
                return ExecResult::failure(1, format!("checksum: {}: {}", check_path, e))
            }
        };
        let content = match String::from_utf8(data) {
            Ok(s) => s,
            Err(_) => {
                return ExecResult::failure(
                    1,
                    format!("checksum: {}: invalid UTF-8", check_path),
                )
            }
        };

        let mut failures = 0;
        let mut output = String::new();

        for line in content.lines() {
            let line = line.trim();
            if line.is_empty() {
                continue;
            }

            // Parse "HASH  FILENAME", "HASH *FILENAME", or "HASH FILENAME"
            let (expected_hash, rest) = match line.split_once(' ') {
                Some(parts) => parts,
                None => {
                    output.push_str(&format!("{}: FAILED (malformed line)\n", line));
                    failures += 1;
                    continue;
                }
            };
            let filename = rest.trim_start_matches([' ', '*']);

            let file_resolved = ctx.resolve_path(filename);
            let mut hasher = StreamHasher::new(algo);
            match ctx
                .read_file_chunked(&file_resolved, ExecContext::STREAM_CHUNK_SIZE, |chunk| {
                    hasher.update(chunk);
                    std::ops::ControlFlow::Continue(())
                })
                .await
            {
                Ok(()) => {
                    let actual_hash = hasher.finalize_hex();
                    if actual_hash == expected_hash {
                        output.push_str(&format!("{}: OK\n", filename));
                    } else {
                        output.push_str(&format!("{}: FAILED\n", filename));
                        failures += 1;
                    }
                }
                Err(e) => {
                    output.push_str(&format!("{}: FAILED ({})\n", filename, e));
                    failures += 1;
                }
            }
        }

        // Remove trailing newline
        if output.ends_with('\n') {
            output.pop();
        }

        if failures > 0 {
            ExecResult::from_output(1, output, format!("checksum: {} computed checksum(s) did NOT match", failures))
        } else {
            ExecResult::with_output(OutputData::text(output))
        }
    }
}

/// Incremental hasher for streaming file digests.
///
/// One variant per supported algorithm — `digest::Digest` gives each a uniform
/// `update`/`finalize`, but the concrete types come from three crates, so the
/// enum dispatches. `new` panics on an unvalidated algorithm; callers validate
/// the name before constructing one (same contract as [`compute_hash`]).
enum StreamHasher {
    Sha256(sha2::Sha256),
    Sha1(sha1::Sha1),
    Md5(md5::Md5),
}

impl StreamHasher {
    fn new(algo: &str) -> Self {
        match algo {
            "sha256" => StreamHasher::Sha256(sha2::Sha256::new()),
            "sha1" => StreamHasher::Sha1(sha1::Sha1::new()),
            "md5" => StreamHasher::Md5(md5::Md5::new()),
            _ => unreachable!("algorithm validated before constructing StreamHasher"),
        }
    }

    fn update(&mut self, data: &[u8]) {
        match self {
            StreamHasher::Sha256(h) => h.update(data),
            StreamHasher::Sha1(h) => h.update(data),
            StreamHasher::Md5(h) => h.update(data),
        }
    }

    fn finalize_hex(self) -> String {
        match self {
            StreamHasher::Sha256(h) => hex_encode(h.finalize().as_slice()),
            StreamHasher::Sha1(h) => hex_encode(h.finalize().as_slice()),
            StreamHasher::Md5(h) => hex_encode(h.finalize().as_slice()),
        }
    }
}

/// Compute hash of bytes using the specified algorithm.
fn compute_hash(data: &[u8], algo: &str) -> String {
    match algo {
        "sha256" => hex_encode(sha2::Sha256::digest(data).as_slice()),
        "sha1" => hex_encode(sha1::Sha1::digest(data).as_slice()),
        "md5" => hex_encode(md5::Md5::digest(data).as_slice()),
        _ => unreachable!("algorithm validated before calling compute_hash"),
    }
}

/// Encode bytes as lowercase hex string.
fn hex_encode(bytes: &[u8]) -> String {
    let mut s = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        s.push_str(&format!("{:02x}", b));
    }
    s
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("hello.txt"), b"hello")
            .await
            .expect("write failed");
        mem.write(Path::new("world.txt"), b"world")
            .await
            .expect("write failed");
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_sha256_file() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("/hello.txt".into()));

        let result = Checksum.execute(args, &mut ctx).await;
        assert!(result.ok());
        // sha256 of "hello"
        let out = result.text_out();
        assert!(out.contains(
            "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
        ));
        assert!(out.contains("/hello.txt"));
    }

    #[tokio::test]
    async fn test_md5_file() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("/hello.txt".into()));
        args.named
            .insert("algo".to_string(), Value::String("md5".into()));

        let result = Checksum.execute(args, &mut ctx).await;
        assert!(result.ok());
        // md5 of "hello"
        assert!(result.text_out().contains("5d41402abc4b2a76b9719d911017c592"));
    }

    #[tokio::test]
    async fn test_sha1_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hello".to_string());

        let mut args = ToolArgs::new();
        args.named
            .insert("algo".to_string(), Value::String("sha1".into()));

        let result = Checksum.execute(args, &mut ctx).await;
        assert!(result.ok());
        // sha1 of "hello"
        let out = result.text_out();
        assert!(out.contains("aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d"));
        assert!(out.contains('-')); // stdin marker
    }

    #[tokio::test]
    async fn test_multiple_files() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("/hello.txt".into()));
        args.positional
            .push(Value::String("/world.txt".into()));

        let result = Checksum.execute(args, &mut ctx).await;
        assert!(result.ok());
        let out = result.text_out();
        let lines: Vec<&str> = out.lines().collect();
        assert_eq!(lines.len(), 2);
        assert!(lines[0].contains("/hello.txt"));
        assert!(lines[1].contains("/world.txt"));
    }

    #[tokio::test]
    async fn test_verify_ok() {
        // Create a checksum file
        let hash = "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824";
        let checksum_content = format!("{}  /hello.txt", hash);
        let mem = MemoryFs::new();
        mem.write(Path::new("hello.txt"), b"hello")
            .await
            .expect("write failed");
        mem.write(
            Path::new("sums.txt"),
            checksum_content.as_bytes(),
        )
        .await
        .expect("write failed");
        let mut vfs = VfsRouter::new();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.named
            .insert("check".to_string(), Value::String("/sums.txt".into()));

        let result = Checksum.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("OK"));
    }

    #[tokio::test]
    async fn test_verify_fail() {
        let checksum_content = "0000000000000000000000000000000000000000000000000000000000000000  /hello.txt";
        let mem = MemoryFs::new();
        mem.write(Path::new("hello.txt"), b"hello")
            .await
            .expect("write failed");
        mem.write(Path::new("sums.txt"), checksum_content.as_bytes())
            .await
            .expect("write failed");
        let mut vfs = VfsRouter::new();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.named
            .insert("check".to_string(), Value::String("/sums.txt".into()));

        let result = Checksum.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.text_out().contains("FAILED"));
    }

    #[tokio::test]
    async fn test_unknown_algo() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("data".to_string());

        let mut args = ToolArgs::new();
        args.named
            .insert("algo".to_string(), Value::String("blake3".into()));

        let result = Checksum.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("unknown algorithm"));
    }

    #[test]
    fn stream_hasher_matches_one_shot_across_every_split() {
        // Feeding the incremental hasher in two chunks split at every byte
        // boundary must yield the same digest as the one-shot `compute_hash`.
        let data = b"the quick brown fox\n\x00\xff binary too";
        for algo in ["sha256", "sha1", "md5"] {
            let want = compute_hash(data, algo);
            for split in 0..=data.len() {
                let mut h = StreamHasher::new(algo);
                h.update(&data[..split]);
                h.update(&data[split..]);
                assert_eq!(h.finalize_hex(), want, "algo={algo} split={split}");
            }
        }
    }

    #[tokio::test]
    async fn test_file_not_found() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("/nope.txt".into()));

        let result = Checksum.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }
}
