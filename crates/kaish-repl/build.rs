//! Build script for kaish-repl.
//!
//! Captures git hash and build date for `--version` output.

use std::process::Command;

fn main() {
    // Only watch .git if it exists (absent in tarball/crate builds)
    if std::path::Path::new("../../.git").exists() {
        println!("cargo::rerun-if-changed=../../.git/HEAD");
        println!("cargo::rerun-if-changed=../../.git/refs/heads/");
    }

    let git_hash = Command::new("git")
        .args(["rev-parse", "--short", "HEAD"])
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
        .unwrap_or_else(|| "unknown".to_string());

    let build_date = chrono::Utc::now().format("%Y-%m-%d").to_string();

    println!("cargo:rustc-env=KAISH_GIT_HASH={git_hash}");
    println!("cargo:rustc-env=KAISH_BUILD_DATE={build_date}");
}
