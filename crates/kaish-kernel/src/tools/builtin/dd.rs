//! dd — copy bytes between files/devices with explicit block sizing.
//!
//! A small, binary-aware subset of classic `dd`: `if=`/`of=`/`bs=`/`count=`/
//! `skip=` operands (parsed as `key=value` argv words — see the parser's
//! `keyword_word` for why `if=` lexes). Reads exactly `count*bs` bytes via the
//! VFS read-range plumbing, so it composes with endless devices
//! (`/dev/urandom`, `/dev/zero`) without hanging. With `of=` it writes to the
//! target; without, it emits a `Bytes` result. See `docs/binary-data.md`.

use async_trait::async_trait;
use std::path::Path;

use crate::ast::Value;
use crate::backend::{ReadRange, WriteMode};
use crate::interpreter::{value_to_string, ExecResult};
use crate::tools::{ExecContext, Tool, ToolArgs, ToolCtx, ToolSchema};

/// dd tool.
pub struct Dd;

/// Hard ceiling on a single `dd` transfer, independent of any device cap.
/// `count*bs` above this is refused rather than risking an OOM allocation.
const MAX_DD_BYTES: u64 = 256 * 1024 * 1024;

/// Parse a size operand: a non-negative integer with an optional binary
/// suffix (`k`/`K`=1024, `m`/`M`=1024², `g`/`G`=1024³).
fn parse_size(s: &str) -> Result<u64, String> {
    let (digits, mult) = match s.chars().last() {
        Some('k') | Some('K') => (&s[..s.len() - 1], 1024u64),
        Some('m') | Some('M') => (&s[..s.len() - 1], 1024 * 1024),
        Some('g') | Some('G') => (&s[..s.len() - 1], 1024 * 1024 * 1024),
        _ => (s, 1),
    };
    let base: u64 = digits
        .parse()
        .map_err(|_| format!("invalid number: {s:?}"))?;
    base.checked_mul(mult)
        .ok_or_else(|| format!("size overflow: {s:?}"))
}

#[async_trait]
impl Tool for Dd {
    fn name(&self) -> &str {
        "dd"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new(
            "dd",
            "Copy bytes between files/devices: if=, of=, bs=, count=, skip=",
        )
        .example("Random bytes to a file", "dd if=/dev/urandom of=key.bin bs=16 count=1")
        .example("Discard a measured stream", "dd if=/dev/zero of=/dev/null bs=1k count=10")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };

        let mut input: Option<String> = None;
        let mut output: Option<String> = None;
        let mut bs: u64 = 512;
        let mut count: Option<u64> = None;
        let mut skip: u64 = 0;

        for operand in &args.positional {
            let raw = match operand {
                Value::String(s) => s.clone(),
                other => value_to_string(other),
            };
            // `--json` etc. are stripped before tools see them; any bare flag
            // here is a usage error for dd's key=value grammar.
            let Some((key, val)) = raw.split_once('=') else {
                return ExecResult::failure(
                    2,
                    format!("dd: bad operand {raw:?} (expected key=value, e.g. if=/dev/urandom)"),
                );
            };
            match key {
                "if" => input = Some(val.to_string()),
                "of" => output = Some(val.to_string()),
                "bs" => match parse_size(val) {
                    Ok(n) => bs = n,
                    Err(e) => return ExecResult::failure(2, format!("dd: bs: {e}")),
                },
                "count" => match parse_size(val) {
                    Ok(n) => count = Some(n),
                    Err(e) => return ExecResult::failure(2, format!("dd: count: {e}")),
                },
                "skip" => match parse_size(val) {
                    Ok(n) => skip = n,
                    Err(e) => return ExecResult::failure(2, format!("dd: skip: {e}")),
                },
                other => {
                    return ExecResult::failure(2, format!("dd: unknown operand {other:?}"))
                }
            }
        }
        if bs == 0 {
            return ExecResult::failure(2, "dd: bs must be greater than 0");
        }

        // Build a ReadRange that covers the requested window:
        //   offset = skip * bs  (always applied when skip > 0)
        //   limit  = count * bs (only when count= is given)
        //
        // No count → read from the skip offset to end of input (a finite file;
        // an endless device errors loudly on an unbounded read).
        let range = match count {
            Some(c) => {
                let total = c.checked_mul(bs);
                match total {
                    Some(t) if t > MAX_DD_BYTES => {
                        return ExecResult::failure(
                            2,
                            format!(
                                "dd: requested {t} bytes (count*bs) exceeds the dd cap of \
                                 {MAX_DD_BYTES} bytes"
                            ),
                        )
                    }
                    Some(t) => match skip.checked_mul(bs) {
                        Some(offset) => Some(ReadRange::bytes(offset, t)),
                        None => return ExecResult::failure(2, "dd: skip*bs overflow"),
                    },
                    None => return ExecResult::failure(2, "dd: count*bs overflow"),
                }
            }
            // No count: skip to offset and read the rest.  When skip is zero
            // the range is a no-op (offset=0, no limit) which the backend
            // treats identically to a bare `None`.
            None => {
                match skip.checked_mul(bs) {
                    Some(0) => None, // no skip, no limit — pass None for efficiency
                    Some(offset) => Some(ReadRange {
                        offset: Some(offset),
                        limit: None,
                        start_line: None,
                        end_line: None,
                    }),
                    None => return ExecResult::failure(2, "dd: skip*bs overflow"),
                }
            }
        };

        let Some(input) = input else {
            return ExecResult::failure(2, "dd: if= is required (reading stdin is not yet supported)");
        };

        let in_resolved = ctx.resolve_path(&input);
        let data = match ctx.backend.read(Path::new(&in_resolved), range).await {
            Ok(d) => d,
            Err(e) => return ExecResult::failure(1, format!("dd: {input}: {e}")),
        };
        let copied = data.len();

        match output {
            Some(out) => {
                let out_resolved = ctx.resolve_path(&out);
                if let Err(e) = ctx
                    .backend
                    .write(Path::new(&out_resolved), &data, WriteMode::Overwrite)
                    .await
                {
                    return ExecResult::failure(1, format!("dd: {out}: {e}"));
                }
                // Status to stderr, like dd; stdout stays empty.
                let mut result = ExecResult::success("");
                result.err = format!("{copied} bytes copied\n");
                result
            }
            // No of=: the bytes are the result (hex dump in REPL, base64 under --json).
            None => {
                let mut result = ExecResult::success_bytes(data);
                result.err = format!("{copied} bytes copied\n");
                result
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{DevFs, Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("tmp/keep"), b"x").await.unwrap(); // ensure /tmp dir exists
        vfs.mount("/", mem);
        vfs.mount("/dev", DevFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    fn args(operands: &[&str]) -> ToolArgs {
        let mut a = ToolArgs::new();
        for op in operands {
            a.positional.push(Value::String((*op).to_string()));
        }
        a
    }

    #[tokio::test]
    async fn dd_zero_to_file_exact_count() {
        let mut ctx = make_ctx().await;
        let r = Dd
            .execute(args(&["if=/dev/zero", "of=/tmp/z.bin", "bs=8", "count=2"]), &mut ctx)
            .await;
        assert!(r.ok(), "stderr: {}", r.err);
        assert!(r.err.contains("16 bytes copied"), "status: {}", r.err);
        // The file holds exactly 16 zero bytes.
        let written = ctx.backend.read(Path::new("/tmp/z.bin"), None).await.unwrap();
        assert_eq!(written, vec![0u8; 16]);
    }

    #[tokio::test]
    async fn dd_urandom_to_bytes_result() {
        let mut ctx = make_ctx().await;
        let r = Dd.execute(args(&["if=/dev/urandom", "bs=16", "count=1"]), &mut ctx).await;
        assert!(r.ok(), "stderr: {}", r.err);
        assert!(r.is_bytes(), "no of= → Bytes result");
        assert_eq!(r.out_bytes().map(|b| b.len()), Some(16));
        assert!(r.err.contains("16 bytes copied"));
    }

    #[tokio::test]
    async fn dd_urandom_to_dev_null() {
        let mut ctx = make_ctx().await;
        let r = Dd
            .execute(args(&["if=/dev/urandom", "of=/dev/null", "bs=1k", "count=10"]), &mut ctx)
            .await;
        assert!(r.ok(), "stderr: {}", r.err);
        assert!(r.err.contains("10240 bytes copied"), "status: {}", r.err);
        assert!(!r.is_bytes(), "of= present → no Bytes result");
    }

    #[tokio::test]
    async fn dd_reads_back_file_size() {
        // Write 10240 bytes, then dd the file with no count → reports its size.
        let mut ctx = make_ctx().await;
        Dd.execute(args(&["if=/dev/urandom", "of=/tmp/r.bin", "bs=1k", "count=10"]), &mut ctx)
            .await;
        let r = Dd.execute(args(&["if=/tmp/r.bin", "of=/dev/null"]), &mut ctx).await;
        assert!(r.ok(), "stderr: {}", r.err);
        assert!(r.err.contains("10240 bytes copied"), "status: {}", r.err);
    }

    #[tokio::test]
    async fn dd_bad_operand_errors() {
        let mut ctx = make_ctx().await;
        let r = Dd.execute(args(&["if=/dev/zero", "bogus"]), &mut ctx).await;
        assert!(!r.ok());
        assert_eq!(r.code, 2);
    }

    #[tokio::test]
    async fn dd_cap_enforced() {
        let mut ctx = make_ctx().await;
        // 512 MiB > 256 MiB cap.
        let r = Dd.execute(args(&["if=/dev/zero", "bs=1m", "count=512"]), &mut ctx).await;
        assert!(!r.ok());
        assert!(r.err.contains("cap"), "status: {}", r.err);
    }

    #[test]
    fn parse_size_suffixes() {
        assert_eq!(parse_size("512").unwrap(), 512);
        assert_eq!(parse_size("1k").unwrap(), 1024);
        assert_eq!(parse_size("2M").unwrap(), 2 * 1024 * 1024);
        assert!(parse_size("nope").is_err());
    }
}
