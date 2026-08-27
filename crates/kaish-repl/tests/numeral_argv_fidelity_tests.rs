//! An external process must receive the exact numeral word that was typed.
//!
//! `-0`, `0.10`, and `1.0` are valid JSON numbers whose own `Display` cannot
//! reproduce them, so the kernel carries their source text alongside the typed
//! value. `exec` builds its argv at its own edge rather than through
//! `build_args_flat`, so the two spellings of one command can drift apart.
//!
//! This spawns the real binary because `exec` replaces the calling process:
//! run in-process, it would execve over the test harness, and the remaining
//! tests in that binary would silently never run.
//!
//! Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::process::Command;

fn stdout_of(source: &str) -> String {
    let output = Command::new(env!("CARGO_BIN_EXE_kaish"))
        .arg("-c")
        .arg(source)
        .output()
        .expect("run kaish -c");
    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

const WORDS: &str = "-0 007 010 0.10 1.0";

#[test]
fn an_external_command_receives_the_source_words() {
    assert_eq!(stdout_of(&format!("/bin/echo {WORDS}")), WORDS);
}

#[test]
fn exec_hands_over_the_same_argv_as_the_direct_spelling() {
    assert_eq!(
        stdout_of(&format!("exec /bin/echo {WORDS}")),
        stdout_of(&format!("/bin/echo {WORDS}")),
        "exec must not disagree with the direct spelling"
    );
}
