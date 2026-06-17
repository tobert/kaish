//! Process-level regressions for the non-interactive `kaish -c` stdin path —
//! the lazy bridge thread (`spawn_stdin_bridge`) and the binary-aware output
//! callback in `main.rs`. These spawn the real `kaish` binary because the bugs
//! they guard live in the frontend, below the kernel API the unit tests reach.
//!
//! Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::io::Write;
use std::process::{Command, Stdio};

/// Run `kaish <args...>` feeding `input` on stdin; return raw stdout bytes.
/// stdin is written from a side thread while `wait_with_output` drains stdout,
/// so large inputs (whose echoed output would fill the OS pipe) can't deadlock.
fn run_with_stdin(args: &[&str], input: Vec<u8>) -> Vec<u8> {
    let kaish = env!("CARGO_BIN_EXE_kaish");
    let mut child = Command::new(kaish)
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn kaish");
    let mut stdin = child.stdin.take().expect("child stdin");
    let writer = std::thread::spawn(move || {
        let _ = stdin.write_all(&input);
        // Dropping `stdin` here closes the pipe → EOF for the command.
    });
    let out = child.wait_with_output().expect("wait kaish");
    writer.join().expect("stdin writer thread");
    out.stdout
}

#[test]
fn large_piped_stdin_is_not_truncated() {
    // The bridge used to call `write_bytes` (a single poll that writes at most
    // one ~64 KiB pipe-buffer fill) and ignore the short count, silently
    // truncating any input larger than the buffer. 200 KiB must round-trip.
    let input = vec![b'x'; 200_000];
    let out = run_with_stdin(&["-c", "cat"], input.clone());
    assert_eq!(out.len(), 200_000, "piped stdin was truncated");
    assert_eq!(out, input, "piped stdin bytes were altered");
}

#[test]
fn large_piped_stdin_through_a_pipeline() {
    // Exercise a reader other than `cat` (the first stage drains the bridge).
    let input = vec![b'x'; 200_000];
    let out = run_with_stdin(&["-c", "wc -c"], input);
    assert_eq!(String::from_utf8_lossy(&out).trim(), "200000");
}

#[test]
fn binary_piped_stdin_round_trips_byte_clean() {
    // The non-interactive output callback used to print `text_out()`, which
    // lossy-decodes a binary (`Bytes`) result to U+FFFD. `cat` of binary stdin
    // must emit the exact bytes.
    let input = vec![0xffu8, 0xfe, 0x00, 0x41, 0x80];
    let out = run_with_stdin(&["-c", "cat"], input.clone());
    assert_eq!(out, input, "binary stdin was corrupted on the way out");
}

#[test]
fn binary_base64_decode_is_byte_clean() {
    // base64 -d produces a `Bytes` result; it must reach stdout verbatim.
    // base64("\xff\xfe\x00") == "//4A".
    let out = run_with_stdin(&["-c", "base64 -d"], b"//4A".to_vec());
    assert_eq!(out, vec![0xffu8, 0xfe, 0x00]);
}
