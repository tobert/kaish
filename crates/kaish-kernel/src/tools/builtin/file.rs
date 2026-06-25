//! file — Identify file type by content (magic bytes).
//!
//! Content-only, like `file`'s magic test: the extension is never consulted, so
//! a `.txt` holding PNG bytes reports as an image. Detection is magic-first with
//! a UTF-8 text fallback, shared with the `kaish-glob` `filetype` module so
//! embedders (e.g. kaibo's image/TTS path) classify bytes the same way the shell
//! does. Zero bytes report `empty`; otherwise-opaque bytes report `data` — never
//! a guess.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use kaish_glob::{FileType, SNIFF_PREFIX_LEN};
use kaish_types::ReadRange;

use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema};

/// file tool: identify file type from its leading bytes.
pub struct File;

/// clap-derived argv layer for file.
#[derive(Parser, Debug)]
#[command(name = "file", about = "Identify file type by content (magic bytes)")]
struct FileArgs {
    /// Print only the MIME type (e.g. image/png) instead of the description.
    #[arg(short = 'i', long = "mime")]
    mime: bool,

    /// Do not prepend filenames to output (-b, --brief).
    #[arg(short = 'b', long = "brief")]
    brief: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Files to identify; reads stdin when none are given.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for File {
    fn name(&self) -> &str {
        "file"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &FileArgs::command(),
            "file",
            "Identify file type by content (magic bytes), not by extension",
            [
                ("Identify a file", "file photo"),
                ("MIME type only", "file -i photo"),
                ("No filename prefix", "file -b photo"),
                ("Identify stdin", "cat photo | file"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let parsed = match FileArgs::try_parse_from(
            std::iter::once("file".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("file: {e}")),
        };
        parsed.global.apply(ctx);

        // No paths: classify stdin. `-` is the conventional name in output.
        if args.positional.is_empty() {
            let bytes = ctx.read_stdin_to_bytes().await.unwrap_or_default();
            let id = Identity::of(&bytes);
            let text = render_line("-", &id.describe(parsed.mime), parsed.brief);
            return ExecResult::with_output_and_text(
                OutputData::table(headers(), vec![OutputNode::new("-").with_cells(id.cells())]),
                text,
            );
        }

        let paths = match ctx.expand_paths(&args.positional).await {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(1, format!("file: {}", e)),
        };

        let mut nodes = Vec::new();
        let mut lines = Vec::new();
        for path in &paths {
            let resolved = ctx.resolve_path(path);
            // Sniffing needs only a bounded prefix — never materialize the whole
            // file. Backends slice via read_range (LocalFs seeks); a short file
            // simply returns fewer bytes.
            let head = match ctx
                .backend
                .read(&resolved, Some(ReadRange::bytes(0, SNIFF_PREFIX_LEN as u64)))
                .await
            {
                Ok(b) => b,
                Err(e) => return ExecResult::failure(1, format!("file: {}: {}", path, e)),
            };
            let id = Identity::of(&head);
            nodes.push(OutputNode::new(path).with_cells(id.cells()));
            lines.push(render_line(path, &id.describe(parsed.mime), parsed.brief));
        }

        ExecResult::with_output_and_text(
            OutputData::table(headers(), nodes),
            lines.join("\n"),
        )
    }
}

/// Table headers: FILE binds to node.name; TYPE/MIME are cells (`--json`).
fn headers() -> Vec<String> {
    vec!["FILE".to_string(), "TYPE".to_string(), "MIME".to_string()]
}

/// One file's identity, resolved from its bytes a single time so the text and
/// `--json` renderings can't drift apart and we don't classify twice per file.
///
/// Detection is magic-first with a UTF-8 text fallback ([`kaish_glob::classify`]).
/// Two honest floors when nothing matches: `empty` for zero bytes (more useful
/// to an agent than `data`, matching `file`), and `data` for opaque bytes —
/// never a guessed type.
enum Identity {
    /// A recognized type (magic or text).
    Known(FileType),
    /// Zero bytes: `empty` / `inode/x-empty`.
    Empty,
    /// No signature and not text: `data` / `application/octet-stream`.
    Opaque,
}

impl Identity {
    fn of(bytes: &[u8]) -> Self {
        if bytes.is_empty() {
            Identity::Empty
        } else {
            match kaish_glob::classify(bytes) {
                Some(ft) => Identity::Known(ft),
                None => Identity::Opaque,
            }
        }
    }

    /// The type word and MIME used everywhere.
    fn parts(&self) -> (&str, &str) {
        match self {
            Identity::Known(ft) => (ft.category.as_str(), ft.mime),
            Identity::Empty => ("empty", "inode/x-empty"),
            Identity::Opaque => ("data", "application/octet-stream"),
        }
    }

    /// Human-facing description: `word (mime)` for a recognized type, or the
    /// bare floor word (`empty`/`data`) otherwise. `--mime` prints MIME alone.
    fn describe(&self, mime_only: bool) -> String {
        let (word, mime) = self.parts();
        if mime_only {
            mime.to_string()
        } else {
            match self {
                Identity::Known(_) => format!("{word} ({mime})"),
                _ => word.to_string(),
            }
        }
    }

    /// TYPE/MIME cells for the `--json` table — always the full pair regardless
    /// of `--mime`/`--brief`, so structured output stays uniform.
    fn cells(&self) -> Vec<String> {
        let (word, mime) = self.parts();
        vec![word.to_string(), mime.to_string()]
    }
}

/// `NAME: desc` unless `--brief`, which drops the prefix.
fn render_line(name: &str, desc: &str, brief: bool) -> String {
    if brief {
        desc.to_string()
    } else {
        format!("{}: {}", name, desc)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::path::Path;
    use std::sync::Arc;

    const PNG: &[u8] = b"\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR";
    const WAV: &[u8] = b"RIFF\x24\x00\x00\x00WAVEfmt ";

    async fn ctx_with(files: &[(&str, &[u8])]) -> ExecContext {
        let mem = MemoryFs::new();
        for (name, bytes) in files {
            mem.write(Path::new(name), bytes).await.expect("write failed");
        }
        let mut vfs = VfsRouter::new();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    fn arg(path: &str) -> ToolArgs {
        let mut a = ToolArgs::new();
        a.positional.push(Value::String(path.into()));
        a
    }

    #[tokio::test]
    async fn identifies_png_by_content() {
        let mut ctx = ctx_with(&[("photo.bin", PNG)]).await;
        let result = File.execute(arg("/photo.bin"), &mut ctx).await;
        assert!(result.ok());
        let out = result.text_out();
        assert!(out.contains("/photo.bin:"), "got: {out}");
        assert!(out.contains("image"), "got: {out}");
        assert!(out.contains("image/png"), "got: {out}");
    }

    #[tokio::test]
    async fn extension_does_not_lie() {
        // A PNG wearing a .txt name still reports as an image.
        let mut ctx = ctx_with(&[("notes.txt", PNG)]).await;
        let result = File.execute(arg("/notes.txt"), &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("image"));
    }

    #[tokio::test]
    async fn wav_reports_audio() {
        let mut ctx = ctx_with(&[("sound", WAV)]).await;
        let result = File.execute(arg("/sound"), &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("audio"), "got: {}", result.text_out());
    }

    #[tokio::test]
    async fn plain_text_reports_text() {
        // No magic, but valid UTF-8 — the text fallback fires.
        let mut ctx = ctx_with(&[("notes", b"just some plain prose, no magic\n")]).await;
        let result = File.execute(arg("/notes"), &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out(), "/notes: text (text/plain)");
    }

    #[tokio::test]
    async fn opaque_binary_reports_data_not_a_guess() {
        // No magic and not text (NUL + invalid UTF-8): the honest `data` floor.
        let mut ctx = ctx_with(&[("blob", b"\x00\x01\x02\xff\xfe not utf8 \x80")]).await;
        let result = File.execute(arg("/blob"), &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out(), "/blob: data");
    }

    #[tokio::test]
    async fn empty_file_reports_empty() {
        // Zero bytes: `empty`, not `data` — and `inode/x-empty` under --mime.
        let mut ctx = ctx_with(&[("nothing", b"")]).await;
        let result = File.execute(arg("/nothing"), &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out(), "/nothing: empty");

        let mut a = arg("/nothing");
        a.flags.insert("mime".into());
        let result = File.execute(a, &mut ctx).await;
        assert_eq!(result.text_out(), "/nothing: inode/x-empty");
    }

    #[tokio::test]
    async fn mime_flag_prints_mime_only() {
        let mut ctx = ctx_with(&[("photo", PNG)]).await;
        let mut a = arg("/photo");
        a.flags.insert("mime".into());
        let result = File.execute(a, &mut ctx).await;
        assert_eq!(result.text_out(), "/photo: image/png");
    }

    #[tokio::test]
    async fn brief_flag_drops_filename() {
        let mut ctx = ctx_with(&[("photo", PNG)]).await;
        let mut a = arg("/photo");
        a.flags.insert("brief".into());
        let result = File.execute(a, &mut ctx).await;
        assert_eq!(result.text_out(), "image (image/png)");
    }

    #[tokio::test]
    async fn reads_stdin_when_no_paths() {
        // GIF's magic ("GIF89a") is pure ASCII, so it survives the text-only
        // buffered-stdin test path; production binary stdin rides the byte-clean
        // pipe reader (read_stdin_to_bytes).
        let mut ctx = ctx_with(&[]).await;
        ctx.set_stdin("GIF89a".to_string());
        let result = File.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("image"), "got: {}", result.text_out());
        assert!(result.text_out().starts_with("-:"), "got: {}", result.text_out());
    }

    #[tokio::test]
    async fn missing_file_is_an_error() {
        let mut ctx = ctx_with(&[]).await;
        let result = File.execute(arg("/nope"), &mut ctx).await;
        assert!(!result.ok());
    }
}
