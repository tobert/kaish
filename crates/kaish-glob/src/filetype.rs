//! Content-based file-type detection (magic bytes).
//!
//! Wraps [`infer`] and maps its matcher groups onto a small, kaish-facing
//! taxonomy shared by the `file` builtin and embedders (e.g. kaibo's
//! image/TTS pipeline). Detection is content-only — extensions are never
//! consulted, so a mislabeled file is classified by what it actually is.
//!
//! **Signature-less data is not detectable.** Raw PCM samples, plain UTF-8
//! prose, and other bytes without a magic number return `None` from
//! [`detect`]. Callers that need a type for such data must carry it
//! explicitly rather than guess — there is nothing to sniff.

/// How many leading bytes are enough for every signature [`infer`] knows.
///
/// A few container formats (ISO base media: MP4, HEIF/HEIC, AVIF) carry their
/// brand a little way into the file, so we read generously. Callers sniffing a
/// stream or a large file should read at most this much and pass the prefix to
/// [`detect`] — no signature needs more.
pub const SNIFF_PREFIX_LEN: usize = 8 * 1024;

/// Broad, kaish-facing category for a detected file.
///
/// Deliberately coarser than [`infer`]'s matcher set: agents and embedders act
/// on "is this an image / some audio / an archive", not on the dozens of
/// concrete formats underneath. The concrete format still rides along in
/// [`FileType::mime`] and [`FileType::extension`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Category {
    Image,
    Audio,
    Video,
    Archive,
    Document,
    Font,
    Text,
    /// Recognized, but none of the above — executables, binary blobs, etc.
    Binary,
}

impl Category {
    /// The lowercase word used in `file`-style output and `--json`.
    pub fn as_str(self) -> &'static str {
        match self {
            Category::Image => "image",
            Category::Audio => "audio",
            Category::Video => "video",
            Category::Archive => "archive",
            Category::Document => "document",
            Category::Font => "font",
            Category::Text => "text",
            Category::Binary => "binary",
        }
    }

    fn from_matcher(matcher: infer::MatcherType) -> Self {
        use infer::MatcherType as M;
        match matcher {
            M::Image => Category::Image,
            M::Audio => Category::Audio,
            M::Video => Category::Video,
            M::Archive => Category::Archive,
            M::Book | M::Doc => Category::Document,
            M::Font => Category::Font,
            M::Text => Category::Text,
            M::App | M::Custom => Category::Binary,
        }
    }
}

impl std::fmt::Display for Category {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

/// A detected file type: a broad [`Category`] plus the canonical MIME type and
/// extension [`infer`] reports for the matched signature.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FileType {
    pub category: Category,
    /// e.g. `"image/png"`, `"audio/x-wav"`.
    pub mime: &'static str,
    /// Canonical extension without a dot, e.g. `"png"`, `"wav"`.
    pub extension: &'static str,
}

impl FileType {
    pub fn is_image(&self) -> bool {
        self.category == Category::Image
    }

    pub fn is_audio(&self) -> bool {
        self.category == Category::Audio
    }

    pub fn is_video(&self) -> bool {
        self.category == Category::Video
    }
}

/// MIME and extension reported for plain text recognized by the UTF-8
/// heuristic (when no magic signature matched).
const TEXT_MIME: &str = "text/plain";
const TEXT_EXT: &str = "txt";

/// Detect a file type from its leading bytes by magic number alone.
///
/// Returns `None` when the bytes carry no recognizable signature — including
/// plain text (which has none) and signature-less data such as headerless raw
/// PCM. `None` means "no known magic", never a silent default.
///
/// This is the strict, content-signature entry point: embedders asking "is this
/// an image / some audio?" want exactly this. For the broader "what is this
/// file, text included" question the `file` builtin asks, use [`classify`].
///
/// Only the first [`SNIFF_PREFIX_LEN`] bytes are ever consulted, so passing a
/// prefix of a large file is correct and cheap.
pub fn detect(bytes: &[u8]) -> Option<FileType> {
    let matched = infer::get(bytes)?;
    Some(FileType {
        category: Category::from_matcher(matched.matcher_type()),
        mime: matched.mime_type(),
        extension: matched.extension(),
    })
}

/// Identify a file type from its leading bytes: magic number first, then a
/// UTF-8 text heuristic.
///
/// [`detect`] plus a text layer. Structured formats are recognized by their
/// signature; failing that, bytes that read as text ([`looks_like_text`]) are
/// reported as `text/plain`. Only genuinely opaque bytes — binary without a
/// known signature, headerless raw PCM — return `None`.
///
/// This is the "what is this file" entry point the `file` builtin uses; prefer
/// [`detect`] when you specifically want magic-byte detection and nothing else.
pub fn classify(bytes: &[u8]) -> Option<FileType> {
    if let Some(found) = detect(bytes) {
        return Some(found);
    }
    looks_like_text(bytes).then_some(FileType {
        category: Category::Text,
        mime: TEXT_MIME,
        extension: TEXT_EXT,
    })
}

/// Heuristic: do these bytes look like human-readable text?
///
/// True when the bytes are valid UTF-8 — tolerating a single multibyte sequence
/// truncated by a prefix read — and contain no NUL or unusual control
/// characters (only the common text whitespace: tab, LF, VT, FF, CR). Empty
/// input is not text. This mirrors the class of test `file` applies before
/// falling back to `data`.
pub fn looks_like_text(bytes: &[u8]) -> bool {
    if bytes.is_empty() {
        return false;
    }
    // Length of the leading run of valid UTF-8 to inspect. A prefix read can
    // slice through a multibyte char: `error_len() == None` means "valid so far,
    // just an incomplete trailing sequence", so we keep the good prefix. A
    // mid-stream invalid byte (`Some(_)`) is real binary; `valid_up_to() == 0`
    // means there's no decodable text at all.
    let valid_len = match std::str::from_utf8(bytes) {
        Ok(_) => bytes.len(),
        Err(e) if e.error_len().is_none() && e.valid_up_to() > 0 => e.valid_up_to(),
        Err(_) => return false,
    };
    // `bytes[..valid_len]` is valid UTF-8 by construction — `valid_up_to()` is
    // defined as a valid boundary — so the decode cannot actually fail.
    let text = std::str::from_utf8(&bytes[..valid_len]).unwrap_or_default();
    !text.chars().any(is_binary_control)
}

/// A control character that marks bytes as binary rather than text. The common
/// text whitespace (tab, line feed, vertical tab, form feed, carriage return)
/// is allowed; everything else in the control range — NUL included — is not.
fn is_binary_control(c: char) -> bool {
    c.is_control() && !matches!(c, '\t' | '\n' | '\u{000b}' | '\u{000c}' | '\r')
}

#[cfg(test)]
mod tests {
    use super::*;

    // Minimal real magic-byte prefixes. We assert on the kaish category and
    // MIME, not infer's internals, so these stay valid across infer bumps.

    #[test]
    fn png_is_image() {
        let png = b"\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR";
        let ft = detect(png).expect("png should be detected");
        assert_eq!(ft.category, Category::Image);
        assert_eq!(ft.mime, "image/png");
        assert_eq!(ft.extension, "png");
        assert!(ft.is_image());
    }

    #[test]
    fn jpeg_is_image() {
        let jpeg = b"\xff\xd8\xff\xe0\x00\x10JFIF";
        let ft = detect(jpeg).expect("jpeg should be detected");
        assert_eq!(ft.category, Category::Image);
        assert_eq!(ft.mime, "image/jpeg");
    }

    #[test]
    fn gif_is_image() {
        let ft = detect(b"GIF89a").expect("gif should be detected");
        assert_eq!(ft.category, Category::Image);
    }

    #[test]
    fn webp_is_image_not_other_riff() {
        // RIFF container disambiguated by the WEBP form-type.
        let webp = b"RIFF\x00\x00\x00\x00WEBPVP8 ";
        let ft = detect(webp).expect("webp should be detected");
        assert_eq!(ft.category, Category::Image);
        assert_eq!(ft.mime, "image/webp");
    }

    #[test]
    fn wav_is_audio_not_other_riff() {
        // Same RIFF container as WebP — the WAVE form-type must steer it to audio.
        let wav = b"RIFF\x24\x00\x00\x00WAVEfmt ";
        let ft = detect(wav).expect("wav should be detected");
        assert_eq!(ft.category, Category::Audio);
        assert_eq!(ft.extension, "wav");
    }

    #[test]
    fn mp3_is_audio() {
        // ID3v2-tagged MP3.
        let mp3 = b"ID3\x03\x00\x00\x00\x00\x00\x00";
        let ft = detect(mp3).expect("mp3 should be detected");
        assert_eq!(ft.category, Category::Audio);
    }

    #[test]
    fn flac_is_audio() {
        let ft = detect(b"fLaC\x00\x00\x00\x22").expect("flac should be detected");
        assert_eq!(ft.category, Category::Audio);
    }

    #[test]
    fn ogg_is_audio() {
        let ogg = b"OggS\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00";
        let ft = detect(ogg).expect("ogg should be detected");
        assert_eq!(ft.category, Category::Audio);
    }

    #[test]
    fn raw_pcm_is_undetectable() {
        // Headerless 16-bit samples — no magic number, must NOT be guessed.
        let pcm = [0x00u8, 0x01, 0xff, 0x7f, 0x00, 0x80, 0x34, 0x12];
        assert!(detect(&pcm).is_none());
    }

    #[test]
    fn plain_text_has_no_magic_but_classifies_as_text() {
        // `detect` is magic-only — prose has no signature.
        assert!(detect(b"the quick brown fox\n").is_none());
        // `classify` layers the text heuristic on top.
        let ft = classify(b"the quick brown fox\n").expect("prose should classify as text");
        assert_eq!(ft.category, Category::Text);
        assert_eq!(ft.mime, "text/plain");
    }

    #[test]
    fn empty_is_undetectable() {
        assert!(detect(b"").is_none());
        assert!(classify(b"").is_none());
        assert!(!looks_like_text(b""));
    }

    #[test]
    fn raw_pcm_classifies_to_none_not_text() {
        // NUL bytes + invalid UTF-8 — not text, and no magic: stays unknown.
        let pcm = [0x00u8, 0x01, 0xff, 0x7f, 0x00, 0x80, 0x34, 0x12];
        assert!(classify(&pcm).is_none());
        assert!(!looks_like_text(&pcm));
    }

    #[test]
    fn utf8_prose_is_text() {
        assert!(looks_like_text("café — naïve façade\n".as_bytes()));
        assert!(looks_like_text(b"plain ascii\twith\ttabs\r\n"));
    }

    #[test]
    fn nul_byte_is_not_text() {
        assert!(!looks_like_text(b"looks texty\x00but has a nul"));
    }

    #[test]
    fn truncated_trailing_multibyte_is_still_text() {
        // A prefix read can cut a file mid-character. "é" is 0xC3 0xA9; drop the
        // continuation byte and the trailing lead byte must not flip us to data.
        let mut bytes = "long enough run of text é".as_bytes().to_vec();
        bytes.pop(); // remove 0xA9, leaving a dangling 0xC3 lead byte
        assert!(looks_like_text(&bytes));
    }

    #[test]
    fn invalid_midstream_utf8_is_not_text() {
        // A bad continuation byte in the middle (not a truncation) is binary.
        assert!(!looks_like_text(b"good text \xff\xfe more bytes here"));
    }

    #[test]
    fn solo_truncated_lead_byte_is_not_text() {
        // A buffer that is *only* an incomplete lead byte (`valid_up_to() == 0`)
        // has no decodable text — must not be classified as text.
        assert!(!looks_like_text(b"\xc3"));
    }

    #[test]
    fn category_words_are_stable() {
        assert_eq!(Category::Image.as_str(), "image");
        assert_eq!(Category::Audio.to_string(), "audio");
    }
}
