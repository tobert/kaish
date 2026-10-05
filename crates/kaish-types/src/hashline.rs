//! Line anchors: `42:202b` names line 42 of a file and the hash of its text.
//!
//! `cat --hashline`, `head`, `tail`, and `grep` print anchors; `edit` takes
//! them back and checks each hash against the file before it writes, so an
//! edit aimed at a line that has changed fails instead of landing on the
//! wrong text. The line number locates; the hash detects change.
//!
//! The kernel computes every hash with one [`LineHasher`], chosen by the
//! embedder. Builtins never hash on their own, so an anchor printed by one
//! builtin always matches the check another builtin makes.

use std::fmt;
use std::sync::Arc;

/// Computes the hash half of a line anchor from the line's bytes.
///
/// The bytes are the line without its terminator: `\n` and `\r\n` end a
/// line and are not hashed, and a lone `\r` at the end of the file stays in
/// the last line. [`lines`] splits text that way.
///
/// The output must be one or more ASCII letters or digits, so it sits
/// between the line number and the text in `42:202b:text` and parses back.
/// [`hash`](Self::hash) panics on anything else: a hasher that breaks the
/// anchor format is a configuration bug, and anchors nobody can parse are
/// worse than a crash.
#[derive(Clone)]
pub struct LineHasher(Arc<HashFn>);

/// The function a [`LineHasher`] wraps.
type HashFn = dyn Fn(&[u8]) -> String + Send + Sync;

impl LineHasher {
    /// Wrap a hash function.
    pub fn new(hash: impl Fn(&[u8]) -> String + Send + Sync + 'static) -> Self {
        Self(Arc::new(hash))
    }

    /// The default: [`fnv1a_line_hash`], 4 hex digits.
    pub fn fnv1a() -> Self {
        Self::new(fnv1a_line_hash)
    }

    /// Hash one line, given without its terminator.
    pub fn hash(&self, line: &[u8]) -> String {
        let hash = (self.0)(line);
        assert!(
            !hash.is_empty() && hash.bytes().all(|byte| byte.is_ascii_alphanumeric()),
            "LineHasher returned {hash:?}; a line hash must be ASCII letters and digits"
        );
        hash
    }
}

impl Default for LineHasher {
    fn default() -> Self {
        Self::fnv1a()
    }
}

impl fmt::Debug for LineHasher {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("LineHasher(..)")
    }
}

/// FNV-1a over the line's bytes, keeping the low 16 bits as 4 hex digits.
///
/// A changed line keeps its old hash about once in 65536 edits. It matches
/// kaijutsu's `line_hash`, so anchors carry over between the two.
///
/// ```
/// assert_eq!(kaish_types::hashline::fnv1a_line_hash(b"alpha"), "202b");
/// ```
pub fn fnv1a_line_hash(line: &[u8]) -> String {
    const OFFSET: u64 = 0xcbf2_9ce4_8422_2325;
    const PRIME: u64 = 0x0000_0100_0000_01b3;
    let mut hash = OFFSET;
    for byte in line {
        hash ^= u64::from(*byte);
        hash = hash.wrapping_mul(PRIME);
    }
    format!("{:04x}", hash & 0xffff)
}

/// Split text into lines the way anchors count them.
///
/// This is [`str::lines`]: `\n` and `\r\n` end a line, a final line without
/// a terminator still counts, and a lone `\r` at the end stays in the last
/// line. Every producer of anchors and `edit` use this one function.
pub fn lines(text: &str) -> std::str::Lines<'_> {
    text.lines()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fnv1a_matches_the_published_reference() {
        assert_eq!(fnv1a_line_hash(b"alpha"), "202b");
        assert_eq!(fnv1a_line_hash(b""), "2325");
    }

    #[test]
    fn whitespace_is_part_of_the_line() {
        assert_ne!(fnv1a_line_hash(b"    x = 1"), fnv1a_line_hash(b"\tx = 1"));
        assert_ne!(fnv1a_line_hash(b"x = 1"), fnv1a_line_hash(b"x = 1 "));
    }

    #[test]
    fn a_custom_hasher_replaces_the_default() {
        let hasher = LineHasher::new(|line| format!("{:02x}", line.len()));
        assert_eq!(hasher.hash(b"alpha"), "05");
        assert_eq!(LineHasher::default().hash(b"alpha"), "202b");
    }

    #[test]
    #[should_panic(expected = "a line hash must be ASCII letters and digits")]
    fn a_hash_that_breaks_the_anchor_format_panics() {
        LineHasher::new(|_| "a:b".to_string()).hash(b"alpha");
    }

    #[test]
    fn terminators_are_not_part_of_a_line() {
        let split: Vec<&str> = lines("a\r\nb\nc").collect();
        assert_eq!(split, ["a", "b", "c"]);
        let split: Vec<&str> = lines("a\nb\r").collect();
        assert_eq!(split, ["a", "b\r"], "a lone \\r at the end stays in the last line");
    }
}
