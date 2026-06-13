//! Binary-data helpers: the base64 JSON envelope and the hex dump.
//!
//! kaish is UTF-8 text end to end, so binary values must be encoded the moment
//! they cross a text boundary. Two encodings, two boundaries:
//!
//! - **base64 envelope** — how a `Value::Bytes` / a binary result serializes for
//!   `--json` and MCP. Self-describing so an agent can act on it:
//!   `{"_type":"bytes","encoding":"base64","data":"…","len":N}`.
//! - **hex dump** — how binary renders for a *human* (REPL): `xxd`-style offset
//!   / hex columns / ASCII gutter.
//!
//! See `docs/binary-data.md`.

use base64::Engine;
use base64::engine::general_purpose::STANDARD;

/// The `_type` discriminator marking a base64 byte envelope.
pub const BYTES_ENVELOPE_TYPE: &str = "bytes";

/// Encode raw bytes as the self-describing base64 JSON envelope.
pub fn bytes_to_envelope(data: &[u8]) -> serde_json::Value {
    let mut map = serde_json::Map::new();
    map.insert(
        "_type".to_string(),
        serde_json::Value::String(BYTES_ENVELOPE_TYPE.to_string()),
    );
    map.insert(
        "encoding".to_string(),
        serde_json::Value::String("base64".to_string()),
    );
    map.insert(
        "data".to_string(),
        serde_json::Value::String(STANDARD.encode(data)),
    );
    map.insert(
        "len".to_string(),
        serde_json::Value::Number((data.len() as u64).into()),
    );
    serde_json::Value::Object(map)
}

/// Recognize the base64 byte envelope and decode it back to raw bytes.
///
/// Returns `None` for anything that isn't a well-formed envelope (so a plain
/// JSON object stays a `Value::Json`). A malformed `data` field — an envelope
/// claiming base64 it can't decode — is a hard `None`, never a silent empty
/// vector.
pub fn envelope_to_bytes(value: &serde_json::Value) -> Option<Vec<u8>> {
    let obj = value.as_object()?;
    if obj.get("_type")?.as_str()? != BYTES_ENVELOPE_TYPE {
        return None;
    }
    // Only base64 is defined today; an unknown encoding is not our envelope.
    if obj.get("encoding").and_then(|e| e.as_str()) != Some("base64") {
        return None;
    }
    let data = obj.get("data")?.as_str()?;
    STANDARD.decode(data).ok()
}

/// Render bytes as an `xxd`-style hex dump for human display.
///
/// 16 bytes per line: an 8-digit hex offset, the bytes as space-separated hex
/// pairs (grouped in two columns of eight), then the printable-ASCII gutter
/// (non-printable bytes shown as `.`). No trailing newline.
pub fn hex_dump(data: &[u8]) -> String {
    const PER_LINE: usize = 16;
    let mut out = String::new();
    for (line_no, chunk) in data.chunks(PER_LINE).enumerate() {
        if line_no > 0 {
            out.push('\n');
        }
        out.push_str(&format!("{:08x}  ", line_no * PER_LINE));
        for i in 0..PER_LINE {
            if i == 8 {
                out.push(' ');
            }
            match chunk.get(i) {
                Some(b) => out.push_str(&format!("{:02x} ", b)),
                None => out.push_str("   "),
            }
        }
        out.push_str(" |");
        for b in chunk {
            out.push(if b.is_ascii_graphic() || *b == b' ' {
                *b as char
            } else {
                '.'
            });
        }
        out.push('|');
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn envelope_round_trips() {
        let data = vec![0u8, 1, 2, 255, 128, 64];
        let env = bytes_to_envelope(&data);
        assert_eq!(env["_type"], "bytes");
        assert_eq!(env["encoding"], "base64");
        assert_eq!(env["len"], 6);
        assert_eq!(envelope_to_bytes(&env), Some(data));
    }

    #[test]
    fn plain_object_is_not_an_envelope() {
        let plain = serde_json::json!({"name": "amy", "len": 3});
        assert_eq!(envelope_to_bytes(&plain), None);
        // A blob envelope is a different _type and must not be mistaken for bytes.
        let blob = serde_json::json!({"_type": "blob", "id": "x", "size": 1});
        assert_eq!(envelope_to_bytes(&blob), None);
    }

    #[test]
    fn malformed_base64_is_none_not_empty() {
        let bad = serde_json::json!({"_type":"bytes","encoding":"base64","data":"!!notb64!!","len":3});
        assert_eq!(envelope_to_bytes(&bad), None);
    }

    #[test]
    fn unknown_encoding_is_not_our_envelope() {
        let hexenc = serde_json::json!({"_type":"bytes","encoding":"hex","data":"00ff","len":2});
        assert_eq!(envelope_to_bytes(&hexenc), None);
    }

    #[test]
    fn hex_dump_layout() {
        // 0..18 so we get a full line plus a short second line.
        let data: Vec<u8> = (0u8..18).collect();
        let dump = hex_dump(&data);
        let lines: Vec<&str> = dump.lines().collect();
        assert_eq!(lines.len(), 2);
        assert!(lines[0].starts_with("00000000  00 01 02 03 04 05 06 07  08 09 0a 0b 0c 0d 0e 0f"));
        assert!(lines[1].starts_with("00000010  10 11 "));
        // ASCII gutter shows printable chars; control bytes become dots.
        assert!(lines[0].ends_with('|'));
    }

    #[test]
    fn hex_dump_ascii_gutter() {
        let dump = hex_dump(b"AB\x00C");
        // 'A' 'B' printable, 0x00 -> '.', 'C' printable
        assert!(dump.ends_with("|AB.C|"), "gutter: {dump}");
    }
}
