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

/// Detect a file type from its leading bytes.
///
/// Returns `None` when the bytes carry no recognizable magic number — including
/// signature-less data such as headerless raw PCM. `None` means "unknown, do
/// not assume", never a silent default.
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
    fn plain_text_is_undetectable() {
        // infer only fires for structured text; prose has no signature.
        assert!(detect(b"the quick brown fox\n").is_none());
    }

    #[test]
    fn empty_is_undetectable() {
        assert!(detect(b"").is_none());
    }

    #[test]
    fn category_words_are_stable() {
        assert_eq!(Category::Image.as_str(), "image");
        assert_eq!(Category::Audio.to_string(), "audio");
    }
}
